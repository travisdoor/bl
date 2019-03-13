//************************************************************************************************
// bl
//
// File:   mir.c
// Author: Martin Dorazil
// Date:   3/15/18
//
// Copyright 2018 Martin Dorazil
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//************************************************************************************************

//#include <stdalign.h>
#include "mir.h"
#include "unit.h"
#include "common.h"
#include "builder.h"
#include "assembly.h"
#include "mir_printer.h"

// Constants
// clang-format off
#define ARENA_CHUNK_COUNT               512
#define TEST_CASE_FN_NAME               ".test"
#define RESOLVE_TYPE_FN_NAME            ".type"
#define INIT_VALUE_FN_NAME              ".init"
#define IMPL_FN_NAME                    ".impl_"
#define IMPL_VARGS_TMP_ARR              ".vargs_arr_"
#define IMPL_VARGS_TMP                  ".vargs_"
#define DEFAULT_EXEC_FRAME_STACK_SIZE   2097152 // 2MB
#define DEFAULT_EXEC_CALL_STACK_NESTING 10000
#define MAX_ALIGNMENT                   8
#define NO_REF_COUNTING                 -1
#define VERBOSE_EXEC                    false
#define VERBOSE_ANALYZE                 false
// clang-format on

// Debug helpers
#if BL_DEBUG && VERBOSE_EXEC
#define _log_push_ra                                                                               \
  {                                                                                                \
    if (instr) {                                                                                   \
      fprintf(stdout, "%6llu %20s  PUSH RA\n", (unsigned long long)cnt->exec_stack->pc->_serial,   \
              mir_instr_name(cnt->exec_stack->pc));                                                \
    } else {                                                                                       \
      fprintf(stdout, "     - %20s  PUSH RA\n", "Terminal");                                       \
    }                                                                                              \
  }

#define _log_pop_ra                                                                                \
  {                                                                                                \
    fprintf(stdout, "%6llu %20s  POP RA\n", (unsigned long long)cnt->exec_stack->pc->_serial,      \
            mir_instr_name(cnt->exec_stack->pc));                                                  \
  }

#define _log_push_stack                                                                            \
  {                                                                                                \
    char type_name[256];                                                                           \
    mir_type_to_str(type_name, 256, type, true);                                                   \
    if (cnt->exec_stack->pc) {                                                                     \
      fprintf(stdout, "%6llu %20s  PUSH    (%luB, %p) %s\n",                                       \
              (unsigned long long)cnt->exec_stack->pc->_serial,                                    \
              mir_instr_name(cnt->exec_stack->pc), size, tmp, type_name);                          \
    } else {                                                                                       \
      fprintf(stdout, "     -                       PUSH    (%luB, %p) %s\n", size, tmp,           \
              type_name);                                                                          \
    }                                                                                              \
  }

#define _log_pop_stack                                                                             \
  {                                                                                                \
    char type_name[256];                                                                           \
    mir_type_to_str(type_name, 256, type, true);                                                   \
    fprintf(stdout, "%6llu %20s  POP     (%luB, %p) %s\n",                                         \
            (unsigned long long)cnt->exec_stack->pc->_serial, mir_instr_name(cnt->exec_stack->pc), \
            size, cnt->exec_stack->top_ptr - size, type_name);                                     \
  }

#else
#define _log_push_ra
#define _log_pop_ra
#define _log_push_stack
#define _log_pop_stack
#endif

union _MirInstr
{
  MirInstrBlock       block;
  MirInstrDeclVar     var;
  MirInstrDeclMember  member;
  MirInstrConst       cnst;
  MirInstrLoad        load;
  MirInstrStore       store;
  MirInstrRet         ret;
  MirInstrBinop       binop;
  MirInstrFnProto     fn_proto;
  MirInstrDeclRef     decl_ref;
  MirInstrCall        call;
  MirInstrUnreachable unreachable;
  MirInstrCondBr      cond_br;
  MirInstrBr          br;
  MirInstrUnop        unop;
  MirInstrArg         arg;
  MirInstrElemPtr     elem_ptr;
  MirInstrMemberPtr   member_ptr;
  MirInstrAddrOf      addrof;
  MirInstrTypeArray   type_array;
  MirInstrTypeSlice   type_slice;
  MirInstrTypeVArgs   type_vargs;
  MirInstrTypePtr     type_ptr;
  MirInstrTypeStruct  type_struct;
  MirInstrTypeFn      type_fn;
  MirInstrCast        cast;
  MirInstrSizeof      szof;
  MirInstrAlignof     alof;
  MirInstrInit        init;
  MirInstrVArgs       vargs;
  MirInstrTypeInfo    type_info;
};

typedef struct MirFrame
{
  struct MirFrame *prev;
  MirInstr *       callee;
} MirFrame;

typedef struct MirStack
{
  MirStackPtr top_ptr;         /* pointer to top of the stack */
  size_t      used_bytes;      /* size of the used stack in bytes */
  size_t      allocated_bytes; /* total allocated size of the stack in bytes */
  MirFrame *  ra;              /* current frame beginning (return address)*/
  MirInstr *  pc;              /* currently executed instruction */
  bool        aborted;         /* true when execution was aborted */
} MirStack;

typedef struct
{
  Builder *   builder;
  Assembly *  assembly;
  MirModule * module;
  BArray *    test_cases;
  BString *   tmp_sh;
  BHashTable *type_table;
  MirFn *     entry_fn;
  bool        verbose_pre, verbose_post;

  /* DynCall/Lib data used for external method execution in compile time */
  struct
  {
    BArray *  libs;
    DCCallVM *vm;
  } dl;

  /* AST -> MIR generation */
  struct
  {
    MirInstrBlock *current_block;
    MirInstrBlock *break_block;
    MirInstrBlock *continue_block;
    ID *           current_entity_id; /* Sometimes used for named structures */
  } ast;

  /* Analyze MIR generated from AST */
  struct
  {
    /* Instructions waiting for analyze. */
    BList *queue;

    /* Hash table of arrays. Hash is ID of symbol and array contains queue of waiting instructions
     * (DeclRefs). */
    BHashTable *waiting;
  } analyze;

  /* MIR compile time execution. */
  struct
  {
    /* stack header is also allocated on the stack :) */
    MirStack *stack;
  } exec;

  /* Builtins */
  struct BuiltinTypes
  {
    MirType *entry_type;
    MirType *entry_s8;
    MirType *entry_s16;
    MirType *entry_s32;
    MirType *entry_s64;
    MirType *entry_u8;
    MirType *entry_u16;
    MirType *entry_u32;
    MirType *entry_u64;
    MirType *entry_usize;
    MirType *entry_bool;
    MirType *entry_f32;
    MirType *entry_f64;
    MirType *entry_void;
    MirType *entry_u8_ptr;
    MirType *entry_string;
    MirType *entry_resolve_type_fn;
    MirType *entry_test_case_fn;
  } builtin_types;
} Context;

typedef enum
{
  /* Analyze pass failed. */
  ANALYZE_FAILED = 0,

  /* Analyze pass passed. */
  ANALYZE_PASSED = 1,

  /* Analyze pass cannot be done because some of sub-parts has not been analyzed yet and probably
     needs to be executed during analyze pass. In such case we push analyzed instruction at the end
     of analyze queue. */
  ANALYZE_POSTPONE = 2
} AnalyzeResult;

/* Ids of builtin symbols, hash is calculated inside init_builtins function later. */
static ID builtin_ids[_MIR_BUILTIN_COUNT] = {
    {.str = "type", .hash = 0},   {.str = "s8", .hash = 0},      {.str = "s16", .hash = 0},
    {.str = "s32", .hash = 0},    {.str = "s64", .hash = 0},     {.str = "u8", .hash = 0},
    {.str = "u16", .hash = 0},    {.str = "u32", .hash = 0},     {.str = "u64", .hash = 0},
    {.str = "usize", .hash = 0},  {.str = "bool", .hash = 0},    {.str = "f32", .hash = 0},
    {.str = "f64", .hash = 0},    {.str = "void", .hash = 0},    {.str = "string", .hash = 0},
    {.str = "null_t", .hash = 0}, {.str = "main", .hash = 0},    {.str = "len", .hash = 0},
    {.str = "ptr", .hash = 0},    {.str = "TypeInfo", .hash = 0}};

static void
value_dtor(MirConstValue *value)
{
  if (!value->type) return;

  switch (value->type->kind) {
  case MIR_TYPE_ARRAY:
    bo_unref(value->data.v_array.elems);
    break;
  default:
    break;
  }
}

static void
array_dtor(BArray **arr)
{
  bo_unref(*arr);
}

/* FW decls */
static void
init_builtins(Context *cnt);

static int
init_dl(Context *cnt);

static void
terminate_dl(Context *cnt);

static void
execute_entry_fn(Context *cnt);

static void
execute_test_cases(Context *cnt);

static bool
type_cmp(MirType *first, MirType *second);

/* ctors */
static MirType *
create_type_type(Context *cnt);

static MirType *
create_type_null(Context *cnt, MirType *base_type);

static MirType *
create_type_void(Context *cnt);

static MirType *
create_type_bool(Context *cnt);

static MirType *
create_type_int(Context *cnt, ID *id, int32_t bitcount, bool is_signed);

static MirType *
create_type_real(Context *cnt, ID *id, int32_t bitcount);

static MirType *
create_type_ptr(Context *cnt, MirType *src_type);

static MirType *
create_type_fn(Context *cnt, MirType *ret_type, BArray *arg_types, bool is_vargs);

static MirType *
create_type_array(Context *cnt, MirType *elem_type, size_t len);

static MirType *
create_type_struct(Context *cnt, ID *id, Scope *scope, BArray *members, bool is_packed,
                   MirTypeStructKind kind);

static MirType *
create_type_slice(Context *cnt, ID *id, MirType *elem_ptr_type);

static MirType *
create_type_vargs(Context *cnt, MirType *elem_ptr_type);

static MirVar *
create_var(Context *cnt, Ast *decl_node, Scope *scope, ID *id, MirType *alloc_type,
           MirConstValue *value, bool is_mutable, bool is_in_gscope);

static MirVar *
create_var_impl(Context *cnt, const char *name, MirType *alloc_type, MirConstValue *value,
                bool is_mutable, bool is_in_gscope);

static BArray *
create_arr(Context *cnt, size_t size);

static MirFn *
create_fn(Context *cnt, Ast *node, ID *id, const char *llvm_name, Scope *scope, int32_t flags,
          MirInstrFnProto *prototype);

static MirMember *
create_member(Context *cnt, Ast *node, ID *id, Scope *scope, int64_t index, MirType *type);

static MirConstValue *
create_value(Context *cnt, MirType *type);

static MirInstrBlock *
append_block(Context *cnt, MirFn *fn, const char *name);

/* instructions */
static void
push_into_curr_block(Context *cnt, MirInstr *instr);

static MirInstr *
insert_instr_load_if_needed(Context *cnt, MirInstr *src);

static MirInstr *
try_impl_cast(Context *cnt, MirInstr *src, MirType *expected_type, bool *valid);

static MirCastOp
get_cast_op(MirType *from, MirType *to);

#define create_instr(_cnt, _kind, _node, _t) ((_t)_create_instr((_cnt), (_kind), (_node)))

static MirInstr *
_create_instr(Context *cnt, MirInstrKind kind, Ast *node);

static MirInstr *
create_instr_call_comptime(Context *cnt, Ast *node, MirInstr *fn);

static MirInstr *
append_instr_arg(Context *cnt, Ast *node, unsigned i);

static MirInstr *
append_instr_init(Context *cnt, Ast *node, MirInstr *type, BArray *values);

static MirInstr *
append_instr_cast(Context *cnt, Ast *node, MirInstr *type, MirInstr *next);

static MirInstr *
append_instr_sizeof(Context *cnt, Ast *node, MirInstr *expr);

static MirInstr *
append_instr_type_info(Context *cnt, Ast *node, MirInstr *expr);

static MirInstr *
append_instr_alignof(Context *cnt, Ast *node, MirInstr *expr);

static MirInstr *
create_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index,
                      bool target_is_slice);

static MirInstr *
append_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index,
                      bool target_is_slice);

static MirInstr *
create_instr_member_ptr(Context *cnt, Ast *node, MirInstr *target_ptr, Ast *member_ident,
                        ScopeEntry *scope_entry, MirBuiltinKind builtin_id);

static MirInstr *
append_instr_member_ptr(Context *cnt, Ast *node, MirInstr *target_ptr, Ast *member_ident,
                        ScopeEntry *scope_entry, MirBuiltinKind builtin_id);

static MirInstr *
append_instr_cond_br(Context *cnt, Ast *node, MirInstr *cond, MirInstrBlock *then_block,
                     MirInstrBlock *else_block);

static MirInstr *
append_instr_br(Context *cnt, Ast *node, MirInstrBlock *then_block);

static MirInstr *
append_instr_load(Context *cnt, Ast *node, MirInstr *src);

static MirInstr *
append_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, BArray *arg_types);

static MirInstr *
append_instr_type_struct(Context *cnt, Ast *node, ID *id, Scope *scope, BArray *members,
                         bool is_packed);

static MirInstr *
append_instr_type_ptr(Context *cnt, Ast *node, MirInstr *type);

static MirInstr *
append_instr_type_array(Context *cnt, Ast *node, MirInstr *elem_type, MirInstr *len);

static MirInstr *
append_instr_type_slice(Context *cnt, Ast *node, MirInstr *elem_type);

static MirInstr *
append_instr_type_vargs(Context *cnt, Ast *node, MirInstr *elem_type);

MirInstr *
append_instr_type_const(Context *cnt, Ast *node, MirInstr *type);

static MirInstr *
append_instr_fn_proto(Context *cnt, Ast *node, MirInstr *type, MirInstr *user_type);

static MirInstr *
append_instr_decl_ref(Context *cnt, Ast *node, ID *rid, Scope *scope, ScopeEntry *scope_entry);

static MirInstr *
append_instr_call(Context *cnt, Ast *node, MirInstr *callee, BArray *args);

static MirInstr *
append_instr_decl_var(Context *cnt, Ast *node, MirInstr *type, MirInstr *init, bool is_mutable,
                      bool is_in_gscope);

static MirInstr *
append_instr_decl_member(Context *cnt, Ast *node, MirInstr *type);

static MirInstr *
create_instr_const_usize(Context *cnt, Ast *node, uint64_t val);

static MirInstr *
append_instr_const_int(Context *cnt, Ast *node, uint64_t val);

static MirInstr *
append_instr_const_float(Context *cnt, Ast *node, float val);

static MirInstr *
append_instr_const_double(Context *cnt, Ast *node, double val);

static MirInstr *
append_instr_const_bool(Context *cnt, Ast *node, bool val);

static MirInstr *
append_instr_const_string(Context *cnt, Ast *node, const char *str);

static MirInstr *
append_instr_const_char(Context *cnt, Ast *node, char c);

static MirInstr *
append_instr_const_null(Context *cnt, Ast *node);

static MirInstr *
append_instr_ret(Context *cnt, Ast *node, MirInstr *value, bool allow_fn_ret_type_override);

static MirInstr *
append_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest);

static MirInstr *
append_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op);

static MirInstr *
append_instr_unop(Context *cnt, Ast *node, MirInstr *instr, UnopKind op);

static MirInstr *
append_instr_unrecheable(Context *cnt, Ast *node);

static MirInstr *
append_instr_addrof(Context *cnt, Ast *node, MirInstr *src);

static MirInstr *
create_instr_vargs_impl(Context *cnt, MirType *type, BArray *values);

/* ast */
static MirInstr *
ast_create_impl_fn_call(Context *cnt, Ast *node, const char *fn_name, MirType *fn_type);

static MirInstr *
ast(Context *cnt, Ast *node);

static void
ast_ublock(Context *cnt, Ast *ublock);

static void
ast_test_case(Context *cnt, Ast *test);

static void
ast_unrecheable(Context *cnt, Ast *unr);

static void
ast_block(Context *cnt, Ast *block);

static void
ast_stmt_if(Context *cnt, Ast *stmt_if);

static void
ast_stmt_return(Context *cnt, Ast *ret);

static void
ast_stmt_loop(Context *cnt, Ast *loop);

static void
ast_stmt_break(Context *cnt, Ast *br);

static void
ast_stmt_continue(Context *cnt, Ast *cont);

static MirInstr *
ast_decl_entity(Context *cnt, Ast *entity);

static MirInstr *
ast_decl_arg(Context *cnt, Ast *arg);

static MirInstr *
ast_decl_member(Context *cnt, Ast *arg);

static MirInstr *
ast_type_ref(Context *cnt, Ast *type_ref);

static MirInstr *
ast_type_struct(Context *cnt, Ast *type_struct);

static MirInstr *
ast_type_fn(Context *cnt, Ast *type_fn);

static MirInstr *
ast_type_arr(Context *cnt, Ast *type_arr);

static MirInstr *
ast_type_slice(Context *cnt, Ast *type_slice);

static MirInstr *
ast_type_ptr(Context *cnt, Ast *type_ptr);

static MirInstr *
ast_type_vargs(Context *cnt, Ast *type_vargs);

static MirInstr *
ast_expr_addrof(Context *cnt, Ast *addrof);

static MirInstr *
ast_expr_cast(Context *cnt, Ast *cast);

static MirInstr *
ast_expr_sizeof(Context *cnt, Ast *szof);

static MirInstr *
ast_expr_type_info(Context *cnt, Ast *type_info);

static MirInstr *
ast_expr_alignof(Context *cnt, Ast *szof);

static MirInstr *
ast_expr_type(Context *cnt, Ast *type);

static MirInstr *
ast_expr_deref(Context *cnt, Ast *deref);

static MirInstr *
ast_expr_ref(Context *cnt, Ast *ref);

static MirInstr *
ast_expr_call(Context *cnt, Ast *call);

static MirInstr *
ast_expr_elem(Context *cnt, Ast *elem);

static MirInstr *
ast_expr_member(Context *cnt, Ast *member);

static MirInstr *
ast_expr_null(Context *cnt, Ast *nl);

static MirInstr *
ast_expr_lit_int(Context *cnt, Ast *expr);

static MirInstr *
ast_expr_lit_float(Context *cnt, Ast *expr);

static MirInstr *
ast_expr_lit_double(Context *cnt, Ast *expr);

static MirInstr *
ast_expr_lit_bool(Context *cnt, Ast *expr);

static MirInstr *
ast_expr_lit_fn(Context *cnt, Ast *lit_fn);

static MirInstr *
ast_expr_lit_string(Context *cnt, Ast *lit_string);

static MirInstr *
ast_expr_lit_char(Context *cnt, Ast *lit_char);

static MirInstr *
ast_expr_binop(Context *cnt, Ast *binop);

static MirInstr *
ast_expr_unary(Context *cnt, Ast *unop);

static MirInstr *
ast_expr_compound(Context *cnt, Ast *cmp);

/* this will also set size and alignment of the type */
static void
init_type_llvm_ABI(Context *cnt, MirType *type);

/* analyze */
static void
reduce_instr(Context *cnt, MirInstr *instr);

static uint64_t
analyze_instr(Context *cnt, MirInstr *instr);

static uint64_t
analyze_instr_init(Context *cnt, MirInstrInit *init);

static uint64_t
analyze_instr_vargs(Context *cnt, MirInstrVArgs *vargs);

static uint64_t
analyze_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static uint64_t
analyze_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr);

static uint64_t
analyze_instr_addrof(Context *cnt, MirInstrAddrOf *addrof);

static uint64_t
analyze_instr_block(Context *cnt, MirInstrBlock *block);

static uint64_t
analyze_instr_ret(Context *cnt, MirInstrRet *ret);

static uint64_t
analyze_instr_arg(Context *cnt, MirInstrArg *arg);

static uint64_t
analyze_instr_unop(Context *cnt, MirInstrUnop *unop);

static uint64_t
analyze_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static uint64_t
analyze_instr_cond_br(Context *cnt, MirInstrCondBr *br);

static uint64_t
analyze_instr_br(Context *cnt, MirInstrBr *br);

static uint64_t
analyze_instr_load(Context *cnt, MirInstrLoad *load);

static uint64_t
analyze_instr_store(Context *cnt, MirInstrStore *store);

static uint64_t
analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);

static uint64_t
analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn);

static uint64_t
analyze_instr_type_struct(Context *cnt, MirInstrTypeStruct *type_struct);

static uint64_t
analyze_instr_type_slice(Context *cnt, MirInstrTypeSlice *type_slice);

static uint64_t
analyze_instr_type_vargs(Context *cnt, MirInstrTypeVArgs *type_vargs);

static uint64_t
analyze_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr);

static uint64_t
analyze_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr);

static uint64_t
analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *var);

static uint64_t
analyze_instr_decl_member(Context *cnt, MirInstrDeclMember *decl);

static uint64_t
analyze_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static uint64_t
analyze_instr_const(Context *cnt, MirInstrConst *cnst);

static uint64_t
analyze_instr_call(Context *cnt, MirInstrCall *call);

static uint64_t
analyze_instr_cast(Context *cnt, MirInstrCast *cast);

static uint64_t
analyze_instr_sizeof(Context *cnt, MirInstrSizeof *szof);

static uint64_t
analyze_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info);

static uint64_t
analyze_instr_alignof(Context *cnt, MirInstrAlignof *alof);

static uint64_t
analyze_instr_binop(Context *cnt, MirInstrBinop *binop);

static void
analyze(Context *cnt);

static void
analyze_report_unresolved(Context *cnt);

/* execute */
static void
exec_instr(Context *cnt, MirInstr *instr);

static void
exec_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static void
exec_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info);

static void
exec_instr_cast(Context *cnt, MirInstrCast *cast);

static void
exec_instr_addrof(Context *cnt, MirInstrAddrOf *addrof);

static void
exec_instr_br(Context *cnt, MirInstrBr *br);

static void
exec_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static void
exec_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr);

static void
exec_instr_arg(Context *cnt, MirInstrArg *arg);

static void
exec_instr_cond_br(Context *cnt, MirInstrCondBr *br);

static void
exec_instr_load(Context *cnt, MirInstrLoad *load);

static void
exec_instr_store(Context *cnt, MirInstrStore *store);

static void
exec_instr_binop(Context *cnt, MirInstrBinop *binop);

static void
exec_instr_unop(Context *cnt, MirInstrUnop *unop);

static void
exec_instr_call(Context *cnt, MirInstrCall *call);

static void
exec_instr_type_slice(Context *cnt, MirInstrTypeSlice *type_slice);

static void
exec_instr_ret(Context *cnt, MirInstrRet *ret);

static void
exec_instr_init(Context *cnt, MirStackPtr var_ptr, MirInstrInit *init);

static void
exec_instr_vargs(Context *cnt, MirInstrVArgs *vargs);

static void
exec_instr_decl_var(Context *cnt, MirInstrDeclVar *var);

static void
exec_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static bool
exec_fn(Context *cnt, MirFn *fn, BArray *args, MirConstValueData *out_value);

static MirConstValue *
exec_call_top_lvl(Context *cnt, MirInstrCall *call);

/* zero max nesting = unlimited nesting */
static void
exec_print_call_stack(Context *cnt, size_t max_nesting);

static MirStack *
exec_new_stack(size_t size);

static void
exec_delete_stack(MirStack *stack);

static void
exec_reset_stack(MirStack *stack);

static void
exec_copy_comptime_to_stack(Context *cnt, MirStackPtr dest_ptr, MirConstValue *src_value);

/* INLINES */
static inline MirInstr *
mutate_instr(MirInstr *instr, MirInstrKind kind)
{
  assert(instr);
  instr->kind = kind;
  return instr;
}

static inline void
erase_instr(MirInstr *instr)
{
  if (!instr) return;
  MirInstrBlock *block = instr->owner_block;
  if (!block) return;

  /* first in block */
  if (block->entry_instr == instr) block->entry_instr = instr->next;
  if (instr->prev) instr->prev->next = instr->next;
  if (instr->next) instr->next->prev = instr->prev;

  instr->prev = NULL;
  instr->next = NULL;
}

static inline void
insert_instr_after(MirInstr *after, MirInstr *instr)
{
  assert(after && instr);

  instr->next = after->next;
  instr->prev = after;
  if (after->next) after->next->prev = instr;
  after->next = instr;

  instr->owner_block = after->owner_block;
  instr->id          = instr->owner_block->owner_fn->instr_count++;
  if (instr->owner_block->last_instr == after) instr->owner_block->last_instr = instr;
}

static inline void
insert_instr_before(MirInstr *before, MirInstr *instr)
{
  assert(before && instr);

  instr->next = before;
  instr->prev = before->prev;
  if (before->prev) before->prev->next = instr;
  before->prev = instr;

  instr->owner_block = before->owner_block;
  instr->id          = instr->owner_block->owner_fn->instr_count++;
  if (instr->owner_block->entry_instr == before) instr->owner_block->entry_instr = instr;
}

static inline void
push_into_gscope(Context *cnt, MirInstr *instr)
{
  assert(instr);
  instr->id = bo_array_size(cnt->module->globals);
  bo_array_push_back(cnt->module->globals, instr);
};

static inline void
analyze_push_back(Context *cnt, MirInstr *instr)
{
  assert(instr);
  bo_list_push_back(cnt->analyze.queue, instr);
}

static inline void
analyze_push_front(Context *cnt, MirInstr *instr)
{
  assert(instr);
  bo_list_push_front(cnt->analyze.queue, instr);
}

static inline void
analyze_notify_provided(Context *cnt, uint64_t hash)
{
  bo_iterator_t iter = bo_htbl_find(cnt->analyze.waiting, hash);
  bo_iterator_t end  = bo_htbl_end(cnt->analyze.waiting);
  if (bo_iterator_equal(&iter, &end)) return; /* No one is waiting for this... */

#if BL_DEBUG && VERBOSE_ANALYZE
  printf("Analyze: Notify '%llu'.\n", (unsigned long long)hash);
#endif

  BArray *wq = bo_htbl_iter_peek_value(cnt->analyze.waiting, &iter, BArray *);
  assert(wq);

  MirInstr *instr;
  barray_foreach(wq, instr)
  {
    analyze_push_back(cnt, instr);
  }

  bo_htbl_erase(cnt->analyze.waiting, &iter);
}

static inline void
analyze_instr_rq(Context *cnt, MirInstr *instr)
{
  if (analyze_instr(cnt, instr) != ANALYZE_PASSED)
    bl_abort("invalid analyze of compiler-generated instruction");
}

static inline const char *
gen_uq_name(Context *cnt, const char *prefix)
{
  static int32_t ui = 0;
  BString *      s  = builder_create_cached_str(cnt->builder);

  bo_string_append(s, prefix);
  char ui_str[21];
  sprintf(ui_str, "%i", ui++);
  bo_string_append(s, ui_str);
  return bo_string_get(s);
}

/* Global variables are allocated in static data segment, so there is no need to use relative
 * pointer. When we set ignore to true original pointer is returned as absolute pointer to the
 * stack.  */
static inline MirStackPtr
exec_read_stack_ptr(Context *cnt, MirRelativeStackPtr rel_ptr, bool ignore)
{
  if (ignore) return (MirStackPtr)rel_ptr;
  assert(rel_ptr);

  MirStackPtr base = (MirStackPtr)cnt->exec.stack->ra;
  assert(base);
  return base + rel_ptr;
}

static inline void *
exec_read_value(MirConstValueData *dest, MirStackPtr src, MirType *type)
{
  assert(dest && src && type);
  const size_t size = type->store_size_bytes;
  return memcpy(dest, src, size);
}

static inline void
exec_abort(Context *cnt, int32_t report_stack_nesting)
{
  exec_print_call_stack(cnt, report_stack_nesting);
  cnt->exec.stack->aborted = true;
}

static inline size_t
exec_stack_alloc_size(const size_t size)
{
  assert(size != 0);
  return size + (MAX_ALIGNMENT - (size % MAX_ALIGNMENT));
}

/* allocate memory on frame stack, size is in bits!!! */
static inline MirStackPtr
exec_stack_alloc(Context *cnt, size_t size)
{
  assert(size && "trying to allocate 0 bits on stack");

  size = exec_stack_alloc_size(size);
  cnt->exec.stack->used_bytes += size;
  if (cnt->exec.stack->used_bytes > cnt->exec.stack->allocated_bytes) {
    msg_error("Stack overflow!!!");
    exec_abort(cnt, 10);
  }

  MirStackPtr mem          = (MirStackPtr)cnt->exec.stack->top_ptr;
  cnt->exec.stack->top_ptr = cnt->exec.stack->top_ptr + size;

  if (!is_aligned(mem, MAX_ALIGNMENT)) {
    bl_warning("BAD ALIGNMENT %p, %d bytes", mem, size);
  }

  return mem;
}

/* shift stack top by the size in bytes */
static inline MirStackPtr
exec_stack_free(Context *cnt, size_t size)
{
  size                = exec_stack_alloc_size(size);
  MirStackPtr new_top = cnt->exec.stack->top_ptr - size;
  if (new_top < (uint8_t *)(cnt->exec.stack->ra + 1)) bl_abort("Stack underflow!!!");
  cnt->exec.stack->top_ptr = new_top;
  cnt->exec.stack->used_bytes -= size;
  return new_top;
}

static inline void
exec_push_ra(Context *cnt, MirInstr *instr)
{
  MirFrame *prev      = cnt->exec.stack->ra;
  MirFrame *tmp       = (MirFrame *)exec_stack_alloc(cnt, sizeof(MirFrame));
  tmp->callee         = instr;
  tmp->prev           = prev;
  cnt->exec.stack->ra = tmp;
  _log_push_ra;
}

static inline MirInstr *
exec_pop_ra(Context *cnt)
{
  if (!cnt->exec.stack->ra) return NULL;
  MirInstr *callee = cnt->exec.stack->ra->callee;

  _log_pop_ra;

  /* rollback */
  MirStackPtr new_top_ptr     = (MirStackPtr)cnt->exec.stack->ra;
  cnt->exec.stack->used_bytes = cnt->exec.stack->top_ptr - new_top_ptr;
  cnt->exec.stack->top_ptr    = new_top_ptr;
  cnt->exec.stack->ra         = cnt->exec.stack->ra->prev;
  return callee;
}

static inline MirStackPtr
exec_push_stack_empty(Context *cnt, MirType *type)
{
  assert(type);
  const size_t size = type->store_size_bytes;
  assert(size && "pushing zero sized data on stack");
  MirStackPtr tmp = exec_stack_alloc(cnt, size);

  _log_push_stack;
  return tmp;
}

static inline MirStackPtr
exec_push_stack(Context *cnt, void *value, MirType *type)
{
  assert(value && "try to push NULL value");
  MirStackPtr  tmp  = exec_push_stack_empty(cnt, type);
  const size_t size = type->store_size_bytes;
  memcpy(tmp, value, size);
  /* pointer relative to frame top */
  return tmp;
}

static inline MirStackPtr
exec_pop_stack(Context *cnt, MirType *type)
{
  assert(type);
  const size_t size = type->store_size_bytes;
  assert(size && "popping zero sized data on stack");

  _log_pop_stack;

  return exec_stack_free(cnt, size);
}

#define exec_pop_stack_as(cnt, type, T) ((T)exec_pop_stack((cnt), (type)))

static inline void
exec_stack_alloc_var(Context *cnt, MirVar *var)
{
  assert(var);
  assert(!var->comptime && "cannot allocate compile time constant");
  /* allocate memory for variable on stack */

  MirStackPtr tmp    = exec_push_stack_empty(cnt, var->alloc_type);
  var->rel_stack_ptr = tmp - (MirStackPtr)cnt->exec.stack->ra;
}

static inline void
exec_stack_alloc_vars(Context *cnt, MirFn *fn)
{
  assert(fn);
  /* Init all stack variables. */
  BArray *vars = fn->variables;
  MirVar *var;
  barray_foreach(vars, var)
  {
    if (var->comptime) continue;
    exec_stack_alloc_var(cnt, var);
  }
}

/* Return pointer to value evaluated from src instruction. Source can be compile time constant or
 * allocated on the stack.*/
static inline MirStackPtr
exec_fetch_value(Context *cnt, MirInstr *src)
{
  if (src->comptime || src->kind == MIR_INSTR_DECL_REF) {
    return (MirStackPtr)&src->const_value.data;
  }

  return exec_pop_stack(cnt, src->const_value.type);
}

static inline MirInstr *
exec_get_pc(Context *cnt)
{
  return cnt->exec.stack->pc;
}

static inline MirFrame *
exec_get_ra(Context *cnt)
{
  return cnt->exec.stack->ra;
}

static inline void
exec_set_pc(Context *cnt, MirInstr *instr)
{
  cnt->exec.stack->pc = instr;
}

static void
exec_copy_comptime_to_stack(Context *cnt, MirStackPtr dest_ptr, MirConstValue *src_value)
{
  /* This may cause recursive calls for aggregate data types. */
  assert(dest_ptr && src_value);
  MirConstValueData *data     = &src_value->data;
  MirType *          src_type = src_value->type;
  assert(src_type);

  switch (src_type->kind) {
  case MIR_TYPE_STRUCT: {
    if (src_value->data.v_struct.is_zero_initializer) {
      memset(dest_ptr, 0, src_type->store_size_bytes);
    } else {
      BArray *       members = data->v_struct.members;
      MirConstValue *member;

      assert(members);
      const size_t memc = bo_array_size(members);
      for (size_t i = 0; i < memc; ++i) {
        member = bo_array_at(members, i, MirConstValue *);

        /* copy all members to variable allocated memory on the stack */
        MirStackPtr elem_dest_ptr =
            dest_ptr + LLVMOffsetOfElement(cnt->module->llvm_td, src_type->llvm_type, i);
        assert(elem_dest_ptr);

        exec_copy_comptime_to_stack(cnt, elem_dest_ptr, member);
      }
    }
    break;
  }

  case MIR_TYPE_ARRAY: {
    if (src_value->data.v_array.is_zero_initializer) {
      memset(dest_ptr, 0, src_type->store_size_bytes);
    } else {
      BArray *       elems = data->v_array.elems;
      MirConstValue *elem;

      assert(elems);
      const size_t memc = bo_array_size(elems);
      for (size_t i = 0; i < memc; ++i) {
        elem = bo_array_at(elems, i, MirConstValue *);

        /* copy all elems to variable allocated memory on the stack */
        MirStackPtr elem_dest_ptr = dest_ptr + (elem->type->store_size_bytes * i);
        assert(elem_dest_ptr);

        exec_copy_comptime_to_stack(cnt, elem_dest_ptr, elem);
      }
    }

    break;
  }

  default:
    memcpy(dest_ptr, (MirStackPtr)src_value, src_type->store_size_bytes);
  }
}
/* execute end */

static inline void
terminate_block(MirInstrBlock *block, MirInstr *terminator)
{
  assert(block);
  if (block->terminal) bl_abort("basic block already terminated!");
  block->terminal = terminator;
}

static inline bool
is_block_terminated(MirInstrBlock *block)
{
  return block->terminal;
}

static inline bool
is_builtin(Ast *ident, MirBuiltinKind kind)
{
  if (!ident) return false;
  assert(ident->kind == AST_IDENT);
  return ident->data.ident.id.hash == builtin_ids[kind].hash;
}

static inline bool
get_block_terminator(MirInstrBlock *block)
{
  return block->terminal;
}

static inline void
set_current_block(Context *cnt, MirInstrBlock *block)
{
  cnt->ast.current_block = block;
}

static inline MirInstrBlock *
get_current_block(Context *cnt)
{
  return cnt->ast.current_block;
}

static inline MirFn *
get_current_fn(Context *cnt)
{
  return cnt->ast.current_block ? cnt->ast.current_block->owner_fn : NULL;
}

static inline void
error_types(Context *cnt, MirType *from, MirType *to, Ast *loc, const char *msg)
{
  assert(from && to);
  if (!msg) msg = "no implicit cast for type '%s' and '%s'";

  char tmp_from[256];
  char tmp_to[256];
  mir_type_to_str(tmp_from, 256, from, true);
  mir_type_to_str(tmp_to, 256, to, true);

  builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, loc->src, BUILDER_CUR_WORD, msg,
              tmp_from, tmp_to);
}

static inline ScopeEntry *
provide_builtin_type(Context *cnt, MirType *type)
{
  /* all builtin types are inserted into the global scope */
  Scope *scope = cnt->assembly->gscope;
  assert(scope);
  assert(type->user_id && "missing identificator for builtin type!!!");

#if BL_DEBUG
  ScopeEntry *collision = scope_lookup(scope, type->user_id, false);
  assert(collision == NULL);
#endif

  ScopeEntry *entry =
      scope_create_entry(&cnt->builder->scope_arenas, SCOPE_ENTRY_TYPE, type->user_id, NULL, true);
  entry->data.type = type;
  scope_insert(scope, entry);
  return entry;
}

static inline ScopeEntry *
provide_var(Context *cnt, MirVar *var)
{
  assert(var && var->id && var->scope);

  ScopeEntry *collision = scope_lookup(var->scope, var->id, false);
  if (collision) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_DUPLICATE_SYMBOL, var->decl_node->src,
                BUILDER_CUR_WORD, "Duplicate symbol.");
    if (collision->node) {
      builder_msg(cnt->builder, BUILDER_MSG_NOTE, 0, collision->node->src, BUILDER_CUR_WORD,
                  "Previous declaration found here.");
    }
    return NULL;
  }

  ScopeEntry *entry = scope_create_entry(&cnt->builder->scope_arenas, SCOPE_ENTRY_VAR, var->id,
                                         var->decl_node, false);
  entry->data.var   = var;
  scope_insert(var->scope, entry);

  if (var->is_in_gscope) {
    analyze_notify_provided(cnt, var->id->hash);
  }

  return entry;
}

static inline ScopeEntry *
provide_member(Context *cnt, MirMember *member)
{
  assert(member && member->id && member->scope);

  ScopeEntry *collision = scope_lookup(member->scope, member->id, false);
  if (collision) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_DUPLICATE_SYMBOL, member->decl_node->src,
                BUILDER_CUR_WORD, "Duplicate symbol inside structure declaration.");
    return NULL;
  }

  ScopeEntry *entry  = scope_create_entry(&cnt->builder->scope_arenas, SCOPE_ENTRY_MEMBER,
                                         member->id, member->decl_node, false);
  entry->data.member = member;
  scope_insert(member->scope, entry);

  return entry;
}

static inline ScopeEntry *
provide_fn(Context *cnt, MirFn *fn)
{
  assert(fn && fn->id && fn->scope);

  ScopeEntry *collision = scope_lookup(fn->scope, fn->id, false);
  if (collision) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_DUPLICATE_SYMBOL, fn->decl_node->src,
                BUILDER_CUR_WORD, "Duplicate symbol.");
    return NULL;
  }

  ScopeEntry *entry =
      scope_create_entry(&cnt->builder->scope_arenas, SCOPE_ENTRY_FN, fn->id, fn->decl_node, false);
  entry->data.fn = fn;
  scope_insert(fn->scope, entry);

  analyze_notify_provided(cnt, fn->id->hash);

  return entry;
}

static inline void
unref_instr(MirInstr *instr)
{
  if (!instr || instr->ref_count == NO_REF_COUNTING) return;
  --instr->ref_count;
}

static inline void
ref_instr(MirInstr *instr)
{
  if (!instr || instr->ref_count == NO_REF_COUNTING) return;
  ++instr->ref_count;
}

static inline bool
is_load_needed(MirInstr *instr)
{
  assert(instr);
  if (!mir_is_pointer_type(instr->const_value.type)) return false;

  switch (instr->kind) {
  case MIR_INSTR_ARG:
  case MIR_INSTR_UNOP:
  case MIR_INSTR_CONST:
  case MIR_INSTR_BINOP:
  case MIR_INSTR_CALL:
  case MIR_INSTR_ADDROF:
  case MIR_INSTR_TYPE_ARRAY:
  case MIR_INSTR_TYPE_FN:
  case MIR_INSTR_TYPE_PTR:
  case MIR_INSTR_TYPE_STRUCT:
  case MIR_INSTR_CAST:
  case MIR_INSTR_DECL_MEMBER:
  case MIR_INSTR_TYPE_INFO:
    return false;
  case MIR_INSTR_DECL_REF: {
    MirInstrDeclRef *ref = (MirInstrDeclRef *)instr;
    if (ref->scope_entry->kind == SCOPE_ENTRY_FN) return true;
  }

  default:
    break;
  }

  return true;
}

static inline void
setup_null_type_if_needed(Context *cnt, MirConstValue *value, MirType *type)
{
  assert(value);
  /* use default null type */
  if (!type) type = cnt->builtin_types.entry_u8_ptr;
  if (type->kind == MIR_TYPE_NULL) type = type->data.null.base_type;
  if (value->type->kind == MIR_TYPE_NULL) {
    assert(mir_is_pointer_type(type) && "creating null for non-pointer value");
    value->type = create_type_null(cnt, type);
  }
}

/* impl */
/* Fetch type, when type with same sh has been already created and can be reused, this function
 * return false and set out_type to already created type from cache. When new type instance was
 * created function will return true and set out_type to new instance of type, new instance will be
 * stored in cache for later use also. */
static inline bool
create_type(Context *cnt, MirType **out_type, const char *sh)
{
  assert(out_type);
  if (sh) {
    uint64_t hash = bo_hash_from_str(sh);

    bo_iterator_t found = bo_htbl_find(cnt->type_table, hash);
    bo_iterator_t end   = bo_htbl_end(cnt->type_table);
    if (!bo_iterator_equal(&found, &end)) {
      *out_type = bo_htbl_iter_peek_value(cnt->type_table, &found, MirType *);
      assert(*out_type);
      return false;
    } else {
      MirType *tmp = arena_alloc(&cnt->module->arenas.type_arena);

      BString *copy = builder_create_cached_str(cnt->builder);
      bo_string_append(copy, sh);

      static uint64_t index = 0;
      tmp->id.str           = bo_string_get(copy);
      tmp->id.hash          = hash;
      tmp->type_table_index = index++;

      // bl_log("new type: '%s' (%llu)", tmp->id.str, tmp->id.hash);
      bo_htbl_insert(cnt->type_table, tmp->id.hash, tmp);
      *out_type = tmp;

      return true;
    }
  }

  bl_abort("should not happend");
}

MirType *
create_type_type(Context *cnt)
{
  MirType *tmp = NULL;
  if (create_type(cnt, &tmp, builtin_ids[MIR_BUILTIN_TYPE_TYPE].str)) {
    tmp->kind    = MIR_TYPE_TYPE;
    tmp->user_id = &builtin_ids[MIR_BUILTIN_TYPE_TYPE];
    init_type_llvm_ABI(cnt, tmp);
  }
  return tmp;
}

static inline const char *
sh_type_null(Context *cnt, MirType *base_type)
{
  assert(base_type->id.str);
  BString *tmp = cnt->tmp_sh;
  bo_string_clear(tmp);
  bo_string_append(tmp, "n.");
  bo_string_append(tmp, base_type->id.str);
  return bo_string_get(tmp);
}

MirType *
create_type_null(Context *cnt, MirType *base_type)
{
  assert(base_type && mir_is_pointer_type(base_type));
  MirType *tmp = NULL;
  if (create_type(cnt, &tmp, sh_type_null(cnt, base_type))) {
    tmp->kind                = MIR_TYPE_NULL;
    tmp->user_id             = &builtin_ids[MIR_BUILTIN_NULL];
    tmp->data.null.base_type = base_type;
    init_type_llvm_ABI(cnt, tmp);
  }
  return tmp;
}

MirType *
create_type_void(Context *cnt)
{
  MirType *tmp = NULL;
  if (create_type(cnt, &tmp, builtin_ids[MIR_BUILTIN_TYPE_VOID].str)) {
    tmp->kind    = MIR_TYPE_VOID;
    tmp->user_id = &builtin_ids[MIR_BUILTIN_TYPE_VOID];
    init_type_llvm_ABI(cnt, tmp);
  }
  return tmp;
}

MirType *
create_type_bool(Context *cnt)
{
  MirType *tmp = NULL;
  if (create_type(cnt, &tmp, builtin_ids[MIR_BUILTIN_TYPE_BOOL].str)) {
    tmp->kind    = MIR_TYPE_BOOL;
    tmp->user_id = &builtin_ids[MIR_BUILTIN_TYPE_BOOL];
    init_type_llvm_ABI(cnt, tmp);
  }
  return tmp;
}

MirType *
create_type_int(Context *cnt, ID *id, int32_t bitcount, bool is_signed)
{
  assert(id);
  assert(bitcount > 0);
  MirType *tmp = NULL;
  if (create_type(cnt, &tmp, id->str)) {
    tmp->kind                   = MIR_TYPE_INT;
    tmp->user_id                = id;
    tmp->data.integer.bitcount  = bitcount;
    tmp->data.integer.is_signed = is_signed;
    init_type_llvm_ABI(cnt, tmp);
  }
  return tmp;
}

MirType *
create_type_real(Context *cnt, ID *id, int32_t bitcount)
{
  assert(bitcount > 0);
  MirType *tmp = NULL;
  if (create_type(cnt, &tmp, id->str)) {
    tmp->kind               = MIR_TYPE_REAL;
    tmp->user_id            = id;
    tmp->data.real.bitcount = bitcount;
    init_type_llvm_ABI(cnt, tmp);
  }
  return tmp;
}

static inline const char *
sh_type_ptr(Context *cnt, MirType *src_type)
{
  assert(src_type->id.str);
  BString *tmp = cnt->tmp_sh;
  bo_string_clear(tmp);
  bo_string_append(tmp, "p.");
  bo_string_append(tmp, src_type->id.str);
  return bo_string_get(tmp);
}

MirType *
create_type_ptr(Context *cnt, MirType *src_type)
{
  MirType *tmp = NULL;
  if (create_type(cnt, &tmp, sh_type_ptr(cnt, src_type))) {
    tmp->kind          = MIR_TYPE_PTR;
    tmp->data.ptr.next = src_type;
    init_type_llvm_ABI(cnt, tmp);
  }

  return tmp;
}

static inline const char *
sh_type_fn(Context *cnt, MirType *ret_type, BArray *arg_types, bool is_vargs)
{
  // assert(src_type->id.str);
  BString *tmp = cnt->tmp_sh;
  bo_string_clear(tmp);

  bo_string_append(tmp, "f(");

  /* append all arg types isd */
  if (arg_types) {
    MirType *arg_type;
    barray_foreach(arg_types, arg_type)
    {
      assert(arg_type->id.str);
      bo_string_append(tmp, arg_type->id.str);

      if (i != bo_array_size(arg_types) - 1) bo_string_append(tmp, ",");
    }
  }

  bo_string_append(tmp, ")");

  if (ret_type) {
    assert(ret_type->id.str);
    bo_string_append(tmp, ret_type->id.str);
  } else {
    /* implicit return void */
    bo_string_append(tmp, cnt->builtin_types.entry_void->id.str);
  }

  return bo_string_get(tmp);
}

MirType *
create_type_fn(Context *cnt, MirType *ret_type, BArray *arg_types, bool is_vargs)
{
  MirType *tmp = NULL;
  if (create_type(cnt, &tmp, sh_type_fn(cnt, ret_type, arg_types, is_vargs))) {
    tmp->kind              = MIR_TYPE_FN;
    tmp->data.fn.arg_types = arg_types;
    tmp->data.fn.is_vargs  = is_vargs;
    tmp->data.fn.ret_type  = ret_type ? ret_type : cnt->builtin_types.entry_void;
    init_type_llvm_ABI(cnt, tmp);
  }

  return tmp;
}

static inline const char *
sh_type_arr(Context *cnt, MirType *elem_type, size_t len)
{
  assert(elem_type->id.str);
  BString *tmp = cnt->tmp_sh;
  bo_string_clear(tmp);

  char ui_str[21];
  sprintf(ui_str, "%llu", (unsigned long long)len);

  bo_string_append(tmp, ui_str);
  bo_string_append(tmp, ".");
  bo_string_append(tmp, elem_type->id.str);
  return bo_string_get(tmp);
}

MirType *
create_type_array(Context *cnt, MirType *elem_type, size_t len)
{
  MirType *tmp = NULL;
  if (create_type(cnt, &tmp, sh_type_arr(cnt, elem_type, len))) {
    tmp->kind                 = MIR_TYPE_ARRAY;
    tmp->data.array.elem_type = elem_type;
    tmp->data.array.len       = len;
    init_type_llvm_ABI(cnt, tmp);
  }

  return tmp;
}

static inline const char *
sh_type_struct(Context *cnt, ID *id, BArray *members, bool is_packed, MirTypeStructKind kind)
{
  assert(!is_packed);
  BString *tmp = cnt->tmp_sh;
  bo_string_clear(tmp);

  if (id) {
    bo_string_append(tmp, id->str);
    bo_string_append(tmp, "{");
  } else {
    switch (kind) {
    case MIR_TS_NONE:
      bo_string_append(tmp, "s{");
      break;
    case MIR_TS_SLICE:
      bo_string_append(tmp, "sl{");
      break;
    case MIR_TS_STRING:
      bl_unimplemented;
    case MIR_TS_VARGS:
      bl_unimplemented;
    }
  }

  if (members) {
    MirType *member_type;
    barray_foreach(members, member_type)
    {
      assert(member_type->id.str);
      bo_string_append(tmp, member_type->id.str);

      if (i != bo_array_size(members) - 1) bo_string_append(tmp, ",");
    }
  }

  bo_string_append(tmp, "}");
  return bo_string_get(tmp);
}

MirType *
create_type_struct(Context *cnt, ID *id, Scope *scope, BArray *members, bool is_packed,
                   MirTypeStructKind kind)
{
  MirType *tmp = NULL;

  if (create_type(cnt, &tmp, sh_type_struct(cnt, id, members, is_packed, kind))) {
    tmp->kind                 = MIR_TYPE_STRUCT;
    tmp->data.strct.members   = members;
    tmp->data.strct.scope     = scope;
    tmp->data.strct.is_packed = is_packed;
    tmp->data.strct.kind      = kind;
    tmp->user_id              = id;

    init_type_llvm_ABI(cnt, tmp);
  }

  return tmp;
}

MirType *
create_type_slice(Context *cnt, ID *id, MirType *elem_ptr_type)
{
  assert(mir_is_pointer_type(elem_ptr_type));
  BArray *members = create_arr(cnt, sizeof(MirType *));
  bo_array_reserve(members, 2);
  /* Slice layout struct { usize, *T } */
  bo_array_push_back(members, cnt->builtin_types.entry_usize);
  bo_array_push_back(members, elem_ptr_type);
  return create_type_struct(cnt, id, NULL, members, false, MIR_TS_SLICE);
}

MirType *
create_type_vargs(Context *cnt, MirType *elem_ptr_type)
{
  MirType *tmp         = create_type_slice(cnt, NULL, elem_ptr_type);
  tmp->data.strct.kind = MIR_TS_VARGS;
  return tmp;
}

MirType *
create_type_string(Context *cnt)
{
  MirType *tmp         = create_type_slice(cnt, &builtin_ids[MIR_BUILTIN_TYPE_STRING],
                                   cnt->builtin_types.entry_u8_ptr);
  tmp->data.strct.kind = MIR_TS_STRING;
  return tmp;
}

MirVar *
create_var(Context *cnt, Ast *decl_node, Scope *scope, ID *id, MirType *alloc_type,
           MirConstValue *value, bool is_mutable, bool is_in_gscope)
{
  assert(id);
  MirVar *tmp       = arena_alloc(&cnt->module->arenas.var_arena);
  tmp->id           = id;
  tmp->alloc_type   = alloc_type;
  tmp->value        = value;
  tmp->scope        = scope;
  tmp->decl_node    = decl_node;
  tmp->is_mutable   = is_mutable;
  tmp->is_in_gscope = is_in_gscope;
  tmp->llvm_name    = id->str;
  return tmp;
}

MirVar *
create_var_impl(Context *cnt, const char *name, MirType *alloc_type, MirConstValue *value,
                bool is_mutable, bool is_in_gscope)
{
  assert(name);
  MirVar *tmp       = arena_alloc(&cnt->module->arenas.var_arena);
  tmp->alloc_type   = alloc_type;
  tmp->value        = value;
  tmp->is_mutable   = is_mutable;
  tmp->is_in_gscope = is_in_gscope;
  tmp->llvm_name    = name;
  tmp->is_implicit  = true;
  return tmp;
}

BArray *
create_arr(Context *cnt, size_t size)
{
  BArray **tmp = arena_alloc(&cnt->module->arenas.array_arena);
  *tmp         = bo_array_new(size);
  return *tmp;
}

MirFn *
create_fn(Context *cnt, Ast *node, ID *id, const char *llvm_name, Scope *scope, int32_t flags,
          MirInstrFnProto *prototype)
{
  MirFn *tmp     = arena_alloc(&cnt->module->arenas.fn_arena);
  tmp->variables = create_arr(cnt, sizeof(MirVar *));
  tmp->llvm_name = llvm_name;
  tmp->id        = id;
  tmp->scope     = scope;
  tmp->flags     = flags;
  tmp->decl_node = node;
  tmp->prototype = &prototype->base;
  return tmp;
}

MirMember *
create_member(Context *cnt, Ast *node, ID *id, Scope *scope, int64_t index, MirType *type)
{
  MirMember *tmp = arena_alloc(&cnt->module->arenas.member_arena);
  tmp->decl_node = node;
  tmp->id        = id;
  tmp->index     = index;
  tmp->type      = type;
  tmp->scope     = scope;
  return tmp;
}

MirConstValue *
create_value(Context *cnt, MirType *type)
{
  MirConstValue *tmp = arena_alloc(&cnt->module->arenas.value_arena);
  tmp->type          = type;
  return tmp;
}

/* instructions */
void
push_into_curr_block(Context *cnt, MirInstr *instr)
{
  assert(instr);
  MirInstrBlock *block = get_current_block(cnt);
  MirFn *        fn    = get_current_fn(cnt);
  assert(block);

  instr->id          = fn->instr_count++;
  instr->owner_block = block;
  instr->prev        = block->last_instr;

  if (!block->entry_instr) block->entry_instr = instr;
  if (instr->prev) instr->prev->next = instr;
  block->last_instr = instr;
}

MirInstr *
insert_instr_load_if_needed(Context *cnt, MirInstr *src)
{
  if (!src) return src;
  if (!is_load_needed(src)) return src;

  MirInstrBlock *block = src->owner_block;
  assert(block);

  assert(src->const_value.type);
  assert(src->const_value.type->kind == MIR_TYPE_PTR);
  MirInstrLoad *tmp  = create_instr(cnt, MIR_INSTR_LOAD, src->node, MirInstrLoad *);
  tmp->src           = src;
  tmp->base.analyzed = true;
  tmp->base.id       = ++block->owner_fn->block_count;

  ref_instr(&tmp->base);
  insert_instr_after(src, &tmp->base);
  analyze_instr_load(cnt, tmp);
  return &tmp->base;
}

MirCastOp
get_cast_op(MirType *from, MirType *to)
{
  const size_t fsize = from->size_bits;
  const size_t tsize = to->size_bits;

  switch (from->kind) {
  case MIR_TYPE_INT: {
    /* from integer */
    switch (to->kind) {
    case MIR_TYPE_INT: {
      /* to integer */
      const bool is_to_signed = to->data.integer.is_signed;
      if (fsize < tsize) {
        return is_to_signed ? MIR_CAST_SEXT : MIR_CAST_ZEXT;
      } else {
        return MIR_CAST_TRUNC;
      }
    }

    case MIR_TYPE_REAL: {
      const bool is_from_signed = from->data.integer.is_signed;
      return is_from_signed ? MIR_CAST_SITOFP : MIR_CAST_UITOFP;
    }

    case MIR_TYPE_PTR: {
      /* to ptr */
      return MIR_CAST_INTTOPTR;
    }

    default:
      return MIR_CAST_INVALID;
    }
    break;
  }

  case MIR_TYPE_PTR: {
    /* from pointer */
    switch (to->kind) {
    case MIR_TYPE_PTR: {
      /* to pointer */
      return MIR_CAST_BITCAST;
    }

    case MIR_TYPE_INT: {
      /* to int */
      return MIR_CAST_PTRTOINT;
    }

    default:
      return MIR_CAST_INVALID;
    }
    break;
  }

  case MIR_TYPE_REAL: {
    /* from real */
    switch (to->kind) {
    case MIR_TYPE_INT: {
      /* to integer */
      const bool is_to_signed = to->data.integer.is_signed;
      return is_to_signed ? MIR_CAST_FPTOSI : MIR_CAST_FPTOUI;
    }

    default:
      return MIR_CAST_INVALID;
    }
    break;
  }

  default:
    return MIR_CAST_INVALID;
  }

  return MIR_CAST_INVALID;
}

static inline bool
can_impl_cast(MirType *from, MirType *to)
{
  assert(from && to);
  switch (from->kind) {
  case MIR_TYPE_INT:
    switch (to->kind) {
    case MIR_TYPE_INT:
      return true; // int to int
    default:
      break;
    }
    break;

  default:
    break;
  }
  return false;
}

MirInstr *
try_impl_cast(Context *cnt, MirInstr *src, MirType *expected_type, bool *valid)
{
  assert(src && expected_type && valid);
  *valid            = true;
  MirType *src_type = src->const_value.type;

  /* both types are same -> no cast is needed */
  if (type_cmp(src_type, expected_type)) {
    return src;
  }

  /* try create implicit cast */
  if (can_impl_cast(src_type, expected_type)) {
    if (src->kind == MIR_INSTR_CONST) {
      /* constant numeric literal */
      src->const_value.type = expected_type;
      /* TODO: check constant overflow */
      return src;
    }

    /* insert cast */
    MirInstrBlock *block = src->owner_block;
    assert(block);

    MirInstrCast *cast          = create_instr(cnt, MIR_INSTR_CAST, src->node, MirInstrCast *);
    cast->base.const_value.type = expected_type;
    cast->base.id               = ++block->owner_fn->block_count;
    cast->next                  = src;
    cast->op                    = get_cast_op(src_type, expected_type);
    ref_instr(&cast->base);

    insert_instr_after(src, &cast->base);
    analyze_instr_rq(cnt, &cast->base);

    return &cast->base;
  }

  error_types(cnt, src->const_value.type, expected_type, src->node, NULL);
  *valid = false;
  return src;
}

MirInstr *
_create_instr(Context *cnt, MirInstrKind kind, Ast *node)
{
  MirInstr *tmp = arena_alloc(&cnt->module->arenas.instr_arena);
  tmp->kind     = kind;
  tmp->node     = node;
  tmp->id       = 0;

#if BL_DEBUG
  static uint64_t counter = 0;
  tmp->_serial            = counter++;
#endif

  return tmp;
}

MirInstrBlock *
append_block(Context *cnt, MirFn *fn, const char *name)
{
  assert(fn && name);
  MirInstrBlock *tmp = create_instr(cnt, MIR_INSTR_BLOCK, NULL, MirInstrBlock *);
  tmp->name          = name;
  tmp->owner_fn      = fn;
  tmp->base.id       = fn->block_count++;

  if (!fn->first_block) {
    fn->first_block = tmp;

    /* first block is referenced everytime!!! */
    ref_instr(&tmp->base);
  }

  tmp->base.prev = &fn->last_block->base;
  if (fn->last_block) fn->last_block->base.next = &tmp->base;
  fn->last_block = tmp;

  return tmp;
}

MirInstr *
create_instr_call_comptime(Context *cnt, Ast *node, MirInstr *fn)
{
  assert(fn && fn->kind == MIR_INSTR_FN_PROTO);
  MirInstrCall *tmp  = create_instr(cnt, MIR_INSTR_CALL, node, MirInstrCall *);
  tmp->base.id       = 0;
  tmp->base.comptime = true;
  tmp->callee        = fn;
  ref_instr(fn);
  return &tmp->base;
}

MirInstr *
append_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, BArray *arg_types)
{
  MirInstrTypeFn *tmp        = create_instr(cnt, MIR_INSTR_TYPE_FN, node, MirInstrTypeFn *);
  tmp->base.const_value.type = cnt->builtin_types.entry_type;
  tmp->base.comptime         = true;
  tmp->ret_type              = ret_type;
  tmp->arg_types             = arg_types;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_type_struct(Context *cnt, Ast *node, ID *id, Scope *scope, BArray *members,
                         bool is_packed)
{
  MirInstrTypeStruct *tmp    = create_instr(cnt, MIR_INSTR_TYPE_STRUCT, node, MirInstrTypeStruct *);
  tmp->base.const_value.type = cnt->builtin_types.entry_type;
  tmp->base.comptime         = true;
  tmp->members               = members;
  tmp->scope                 = scope;
  tmp->is_packed             = is_packed;
  tmp->id                    = id;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_type_ptr(Context *cnt, Ast *node, MirInstr *type)
{
  MirInstrTypePtr *tmp       = create_instr(cnt, MIR_INSTR_TYPE_PTR, node, MirInstrTypePtr *);
  tmp->base.const_value.type = cnt->builtin_types.entry_type;
  tmp->base.comptime         = true;
  tmp->type                  = type;

  ref_instr(type);
  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_type_array(Context *cnt, Ast *node, MirInstr *elem_type, MirInstr *len)
{
  MirInstrTypeArray *tmp     = create_instr(cnt, MIR_INSTR_TYPE_ARRAY, node, MirInstrTypeArray *);
  tmp->base.const_value.type = cnt->builtin_types.entry_type;
  tmp->base.comptime         = true;
  tmp->elem_type             = elem_type;
  tmp->len                   = len;

  ref_instr(elem_type);
  ref_instr(len);
  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_type_slice(Context *cnt, Ast *node, MirInstr *elem_type)
{
  MirInstrTypeSlice *tmp     = create_instr(cnt, MIR_INSTR_TYPE_SLICE, node, MirInstrTypeSlice *);
  tmp->base.const_value.type = cnt->builtin_types.entry_type;
  tmp->base.comptime         = true;
  tmp->elem_type             = elem_type;

  ref_instr(elem_type);
  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_type_vargs(Context *cnt, Ast *node, MirInstr *elem_type)
{
  MirInstrTypeVArgs *tmp     = create_instr(cnt, MIR_INSTR_TYPE_VARGS, node, MirInstrTypeVArgs *);
  tmp->base.const_value.type = cnt->builtin_types.entry_type;
  tmp->base.comptime         = true;
  tmp->elem_type             = elem_type;

  ref_instr(elem_type);
  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_arg(Context *cnt, Ast *node, unsigned i)
{
  MirInstrArg *tmp = create_instr(cnt, MIR_INSTR_ARG, node, MirInstrArg *);
  tmp->i           = i;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_init(Context *cnt, Ast *node, MirInstr *type, BArray *values)
{
  if (values) {
    MirInstr *value;
    barray_foreach(values, value) ref_instr(value);
  }

  MirInstrInit *tmp = create_instr(cnt, MIR_INSTR_INIT, node, MirInstrInit *);
  tmp->type         = type;
  tmp->values       = values;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_cast(Context *cnt, Ast *node, MirInstr *type, MirInstr *next)
{
  ref_instr(type);
  ref_instr(next);
  MirInstrCast *tmp = create_instr(cnt, MIR_INSTR_CAST, node, MirInstrCast *);
  tmp->type         = type;
  tmp->next         = next;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_sizeof(Context *cnt, Ast *node, MirInstr *expr)
{
  ref_instr(expr);
  MirInstrSizeof *tmp        = create_instr(cnt, MIR_INSTR_SIZEOF, node, MirInstrSizeof *);
  tmp->base.const_value.type = cnt->builtin_types.entry_usize;
  tmp->base.comptime         = true;
  tmp->expr                  = expr;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_type_info(Context *cnt, Ast *node, MirInstr *expr)
{
  ref_instr(expr);
  MirInstrTypeInfo *tmp = create_instr(cnt, MIR_INSTR_TYPE_INFO, node, MirInstrTypeInfo *);
  tmp->expr             = expr;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_alignof(Context *cnt, Ast *node, MirInstr *expr)
{
  ref_instr(expr);
  MirInstrAlignof *tmp       = create_instr(cnt, MIR_INSTR_ALIGNOF, node, MirInstrAlignof *);
  tmp->base.const_value.type = cnt->builtin_types.entry_usize;
  tmp->base.comptime         = true;
  tmp->expr                  = expr;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_cond_br(Context *cnt, Ast *node, MirInstr *cond, MirInstrBlock *then_block,
                     MirInstrBlock *else_block)
{
  assert(cond && then_block && else_block);
  ref_instr(cond);
  ref_instr(&then_block->base);
  ref_instr(&else_block->base);
  MirInstrCondBr *tmp        = create_instr(cnt, MIR_INSTR_COND_BR, node, MirInstrCondBr *);
  tmp->base.ref_count        = NO_REF_COUNTING;
  tmp->base.const_value.type = cnt->builtin_types.entry_void;
  tmp->cond                  = cond;
  tmp->then_block            = then_block;
  tmp->else_block            = else_block;

  MirInstrBlock *block = get_current_block(cnt);
  terminate_block(block, &tmp->base);

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_br(Context *cnt, Ast *node, MirInstrBlock *then_block)
{
  assert(then_block);
  ref_instr(&then_block->base);
  MirInstrBr *tmp            = create_instr(cnt, MIR_INSTR_BR, node, MirInstrBr *);
  tmp->base.ref_count        = NO_REF_COUNTING;
  tmp->base.const_value.type = cnt->builtin_types.entry_void;
  tmp->then_block            = then_block;

  MirInstrBlock *block = get_current_block(cnt);
  terminate_block(block, &tmp->base);

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
create_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index,
                      bool target_is_slice)
{
  assert(arr_ptr && index);
  ref_instr(arr_ptr);
  ref_instr(index);
  MirInstrElemPtr *tmp = create_instr(cnt, MIR_INSTR_ELEM_PTR, node, MirInstrElemPtr *);
  tmp->arr_ptr         = arr_ptr;
  tmp->index           = index;
  tmp->target_is_slice = target_is_slice;

  return &tmp->base;
}

MirInstr *
append_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index,
                      bool target_is_slice)
{
  MirInstr *tmp = create_instr_elem_ptr(cnt, node, arr_ptr, index, target_is_slice);
  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
create_instr_member_ptr(Context *cnt, Ast *node, MirInstr *target_ptr, Ast *member_ident,
                        ScopeEntry *scope_entry, MirBuiltinKind builtin_id)
{
  ref_instr(target_ptr);
  MirInstrMemberPtr *tmp = create_instr(cnt, MIR_INSTR_MEMBER_PTR, node, MirInstrMemberPtr *);
  tmp->target_ptr        = target_ptr;
  tmp->member_ident      = member_ident;
  tmp->scope_entry       = scope_entry;
  tmp->builtin_id        = builtin_id;

  return &tmp->base;
}

MirInstr *
append_instr_member_ptr(Context *cnt, Ast *node, MirInstr *target_ptr, Ast *member_ident,
                        ScopeEntry *scope_entry, MirBuiltinKind builtin_id)
{
  MirInstr *tmp =
      create_instr_member_ptr(cnt, node, target_ptr, member_ident, scope_entry, builtin_id);
  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_load(Context *cnt, Ast *node, MirInstr *src)
{
  ref_instr(src);
  MirInstrLoad *tmp = create_instr(cnt, MIR_INSTR_LOAD, node, MirInstrLoad *);
  tmp->src          = src;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_addrof(Context *cnt, Ast *node, MirInstr *src)
{
  ref_instr(src);
  MirInstrAddrOf *tmp = create_instr(cnt, MIR_INSTR_ADDROF, node, MirInstrAddrOf *);
  tmp->src            = src;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_unrecheable(Context *cnt, Ast *node)
{
  MirInstrUnreachable *tmp = create_instr(cnt, MIR_INSTR_UNREACHABLE, node, MirInstrUnreachable *);
  tmp->base.const_value.type = cnt->builtin_types.entry_void;
  tmp->base.ref_count        = NO_REF_COUNTING;
  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_fn_proto(Context *cnt, Ast *node, MirInstr *type, MirInstr *user_type)
{
  MirInstrFnProto *tmp = create_instr(cnt, MIR_INSTR_FN_PROTO, node, MirInstrFnProto *);
  tmp->type            = type;
  tmp->user_type       = user_type;

  push_into_gscope(cnt, &tmp->base);
  analyze_push_back(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_decl_ref(Context *cnt, Ast *node, ID *rid, Scope *scope, ScopeEntry *scope_entry)
{
  assert(scope && rid);
  MirInstrDeclRef *tmp = create_instr(cnt, MIR_INSTR_DECL_REF, node, MirInstrDeclRef *);
  tmp->scope_entry     = scope_entry;
  tmp->scope           = scope;
  tmp->rid             = rid;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_call(Context *cnt, Ast *node, MirInstr *callee, BArray *args)
{
  assert(callee);
  MirInstrCall *tmp = create_instr(cnt, MIR_INSTR_CALL, node, MirInstrCall *);
  tmp->args         = args;
  tmp->callee       = callee;

  ref_instr(&tmp->base);

  /* reference all arguments */
  if (args) {
    MirInstr *instr;
    barray_foreach(args, instr) ref_instr(instr);
  }

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_decl_var(Context *cnt, Ast *node, MirInstr *type, MirInstr *init, bool is_mutable,
                      bool is_in_gscope)
{
  ref_instr(type);
  ref_instr(init);
  MirInstrDeclVar *tmp       = create_instr(cnt, MIR_INSTR_DECL_VAR, node, MirInstrDeclVar *);
  tmp->base.ref_count        = NO_REF_COUNTING;
  tmp->base.const_value.type = cnt->builtin_types.entry_void;
  tmp->type                  = type;
  tmp->init                  = init;
  tmp->var = create_var(cnt, node, node->data.ident.scope, &node->data.ident.id, NULL,
                        &tmp->base.const_value, is_mutable, is_in_gscope);
  if (is_in_gscope) {
    push_into_gscope(cnt, &tmp->base);
    analyze_push_back(cnt, &tmp->base);
  } else {
    push_into_curr_block(cnt, &tmp->base);
  }
  return &tmp->base;
}

MirInstr *
append_instr_decl_member(Context *cnt, Ast *node, MirInstr *type)
{
  ref_instr(type);
  MirInstrDeclMember *tmp    = create_instr(cnt, MIR_INSTR_DECL_MEMBER, node, MirInstrDeclMember *);
  tmp->base.ref_count        = NO_REF_COUNTING;
  tmp->base.comptime         = true;
  tmp->base.const_value.type = cnt->builtin_types.entry_void;
  tmp->type                  = type;

  ID *id      = node ? &node->data.ident.id : NULL;
  tmp->member = create_member(cnt, node, id, NULL, -1, NULL);

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

static MirInstr *
create_instr_const_usize(Context *cnt, Ast *node, uint64_t val)
{
  MirInstr *tmp               = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime               = true;
  tmp->const_value.type       = cnt->builtin_types.entry_usize;
  tmp->const_value.data.v_u64 = val;

  return tmp;
}

MirInstr *
append_instr_const_int(Context *cnt, Ast *node, uint64_t val)
{
  MirInstr *tmp               = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime               = true;
  tmp->const_value.type       = cnt->builtin_types.entry_s32;
  tmp->const_value.data.v_s64 = (int64_t)val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_float(Context *cnt, Ast *node, float val)
{
  MirInstr *tmp         = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime         = true;
  tmp->const_value.type = cnt->builtin_types.entry_f32;
  // memcpy(&tmp->const_value.data, &val, sizeof(float));
  tmp->const_value.data.v_f32 = val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_double(Context *cnt, Ast *node, double val)
{
  MirInstr *tmp               = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime               = true;
  tmp->const_value.type       = cnt->builtin_types.entry_f64;
  tmp->const_value.data.v_f64 = val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_bool(Context *cnt, Ast *node, bool val)
{
  MirInstr *tmp                = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime                = true;
  tmp->const_value.type        = cnt->builtin_types.entry_bool;
  tmp->const_value.data.v_bool = val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_string(Context *cnt, Ast *node, const char *str)
{
  MirInstr *tmp               = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime               = true;
  tmp->const_value.type       = cnt->builtin_types.entry_string;
  tmp->const_value.data.v_str = str;

  /* initialize constant slice */
  {
    BArray *       members      = create_arr(cnt, sizeof(MirConstValue *));
    BArray *       member_types = cnt->builtin_types.entry_string->data.strct.members;
    MirConstValue *value;

    /* string slice len */
    value             = create_value(cnt, bo_array_at(member_types, 0, MirType *));
    value->data.v_u64 = strlen(str);
    bo_array_push_back(members, value);

    /* string slice ptr */
    value             = create_value(cnt, bo_array_at(member_types, 1, MirType *));
    value->data.v_str = str;
    bo_array_push_back(members, value);

    tmp->const_value.data.v_struct.members = members;
  }

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_char(Context *cnt, Ast *node, char c)
{
  MirInstr *tmp                = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime                = true;
  tmp->const_value.type        = cnt->builtin_types.entry_u8;
  tmp->const_value.data.v_char = c;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_null(Context *cnt, Ast *node)
{
  MirInstr *tmp                    = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime                    = true;
  tmp->const_value.type            = create_type_null(cnt, cnt->builtin_types.entry_u8_ptr);
  tmp->const_value.data.v_void_ptr = NULL;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_ret(Context *cnt, Ast *node, MirInstr *value, bool allow_fn_ret_type_override)
{
  if (value) ref_instr(value);

  MirInstrRet *tmp                = create_instr(cnt, MIR_INSTR_RET, node, MirInstrRet *);
  tmp->base.const_value.type      = cnt->builtin_types.entry_void;
  tmp->base.ref_count             = NO_REF_COUNTING;
  tmp->value                      = value;
  tmp->allow_fn_ret_type_override = allow_fn_ret_type_override;

  MirInstrBlock *block = get_current_block(cnt);
  terminate_block(block, &tmp->base);

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest)
{
  assert(src && dest);
  ref_instr(src);
  ref_instr(dest);

  MirInstrStore *tmp         = create_instr(cnt, MIR_INSTR_STORE, node, MirInstrStore *);
  tmp->base.const_value.type = cnt->builtin_types.entry_void;
  tmp->base.ref_count        = NO_REF_COUNTING;
  tmp->src                   = src;
  tmp->dest                  = dest;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op)
{
  assert(lhs && rhs);
  ref_instr(lhs);
  ref_instr(rhs);
  MirInstrBinop *tmp = create_instr(cnt, MIR_INSTR_BINOP, node, MirInstrBinop *);
  tmp->lhs           = lhs;
  tmp->rhs           = rhs;
  tmp->op            = op;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_unop(Context *cnt, Ast *node, MirInstr *instr, UnopKind op)
{
  assert(instr);
  ref_instr(instr);
  MirInstrUnop *tmp = create_instr(cnt, MIR_INSTR_UNOP, node, MirInstrUnop *);
  tmp->instr        = instr;
  tmp->op           = op;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
create_instr_vargs_impl(Context *cnt, MirType *type, BArray *values)
{
  assert(type);
  MirInstrVArgs *tmp = create_instr(cnt, MIR_INSTR_VARGS, NULL, MirInstrVArgs *);
  tmp->type          = type;
  tmp->values        = values;

  return &tmp->base;
}

/* LLVM */
void
init_type_llvm_ABI(Context *cnt, MirType *type)
{
  if (!type) return;

  switch (type->kind) {
  case MIR_TYPE_TYPE: {
    type->alignment        = __alignof(MirType *);
    type->size_bits        = sizeof(MirType *) * 8;
    type->store_size_bytes = sizeof(MirType *);
    type->llvm_type        = LLVMVoidTypeInContext(cnt->module->llvm_cnt);
    break;
  }

  case MIR_TYPE_NULL: {
    MirType *tmp = type->data.null.base_type;
    assert(tmp);
    assert(tmp->llvm_type);
    type->llvm_type        = tmp->llvm_type;
    type->alignment        = tmp->alignment;
    type->size_bits        = tmp->size_bits;
    type->store_size_bytes = tmp->store_size_bytes;
    break;
  }

  case MIR_TYPE_VOID: {
    type->alignment        = 0;
    type->size_bits        = 0;
    type->store_size_bytes = 0;
    type->llvm_type        = LLVMVoidTypeInContext(cnt->module->llvm_cnt);
    break;
  }

  case MIR_TYPE_INT: {
    type->llvm_type =
        LLVMIntTypeInContext(cnt->module->llvm_cnt, (unsigned int)type->data.integer.bitcount);
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->module->llvm_td, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->module->llvm_td, type->llvm_type);
    type->alignment        = LLVMABIAlignmentOfType(cnt->module->llvm_td, type->llvm_type);
    break;
  }

  case MIR_TYPE_REAL: {
    if (type->data.real.bitcount == 32)
      type->llvm_type = LLVMFloatTypeInContext(cnt->module->llvm_cnt);
    else if (type->data.real.bitcount == 64)
      type->llvm_type = LLVMDoubleTypeInContext(cnt->module->llvm_cnt);
    else
      bl_abort("invalid floating point type");

    type->size_bits        = LLVMSizeOfTypeInBits(cnt->module->llvm_td, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->module->llvm_td, type->llvm_type);
    type->alignment        = LLVMABIAlignmentOfType(cnt->module->llvm_td, type->llvm_type);
    break;
  }

  case MIR_TYPE_BOOL: {
    type->llvm_type        = LLVMIntTypeInContext(cnt->module->llvm_cnt, 1);
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->module->llvm_td, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->module->llvm_td, type->llvm_type);
    type->alignment        = LLVMABIAlignmentOfType(cnt->module->llvm_td, type->llvm_type);
    break;
  }

  case MIR_TYPE_PTR: {
    MirType *tmp = mir_deref_type(type);
    assert(tmp);
    assert(tmp->llvm_type);
    type->llvm_type        = LLVMPointerType(tmp->llvm_type, 0);
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->module->llvm_td, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->module->llvm_td, type->llvm_type);
    type->alignment        = LLVMABIAlignmentOfType(cnt->module->llvm_td, type->llvm_type);
    break;
  }

  case MIR_TYPE_FN: {
    MirType *tmp_ret  = type->data.fn.ret_type;
    BArray * tmp_args = type->data.fn.arg_types;
    size_t   argc     = tmp_args ? bo_array_size(tmp_args) : 0;

    LLVMTypeRef *llvm_args = NULL;
    LLVMTypeRef  llvm_ret  = NULL;

    if (tmp_args) {
      llvm_args = bl_malloc(argc * sizeof(LLVMTypeRef));
      if (!llvm_args) bl_abort("bad alloc");

      MirType *tmp_arg;
      for (size_t i = 0; i < argc; ++i) {
        tmp_arg = bo_array_at(tmp_args, i, MirType *);
        assert(tmp_arg->llvm_type);
        llvm_args[i] = tmp_arg->llvm_type;
      }
    }

    llvm_ret = tmp_ret ? tmp_ret->llvm_type : LLVMVoidTypeInContext(cnt->module->llvm_cnt);
    assert(llvm_ret);

    type->llvm_type        = LLVMFunctionType(llvm_ret, llvm_args, (unsigned int)argc, false);
    type->size_bits        = 0;
    type->store_size_bytes = 0;
    type->alignment        = 0;
    bl_free(llvm_args);
    break;
  }

  case MIR_TYPE_ARRAY: {
    LLVMTypeRef llvm_elem_type = type->data.array.elem_type->llvm_type;
    assert(llvm_elem_type);
    const unsigned int len = (const unsigned int)type->data.array.len;

    type->llvm_type        = LLVMArrayType(llvm_elem_type, len);
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->module->llvm_td, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->module->llvm_td, type->llvm_type);
    type->alignment        = LLVMABIAlignmentOfType(cnt->module->llvm_td, type->llvm_type);
    break;
  }

  case MIR_TYPE_STRUCT: {
    BArray *members = type->data.strct.members;
    assert(members);
    const bool   is_packed = type->data.strct.is_packed;
    const size_t memc      = bo_array_size(members);
    assert(memc > 0);
    LLVMTypeRef *llvm_members = NULL;

    {
      llvm_members = bl_malloc(memc * sizeof(LLVMTypeRef));
      if (!llvm_members) bl_abort("bad alloc");

      MirType *tmp;
      barray_foreach(members, tmp)
      {
        assert(tmp->llvm_type);
        llvm_members[i] = tmp->llvm_type;
      }
    }

    /* named structure type */
    if (type->user_id) {
      type->llvm_type = LLVMStructCreateNamed(cnt->module->llvm_cnt, type->user_id->str);
      LLVMStructSetBody(type->llvm_type, llvm_members, memc, is_packed);
    } else {
      type->llvm_type =
          LLVMStructTypeInContext(cnt->module->llvm_cnt, llvm_members, memc, is_packed);
    }
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->module->llvm_td, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->module->llvm_td, type->llvm_type);
    type->alignment        = LLVMABIAlignmentOfType(cnt->module->llvm_td, type->llvm_type);
    free(llvm_members);
    break;
  }

  default:
    bl_unimplemented;
  }
}

bool
type_cmp(MirType *first, MirType *second)
{
  assert(first && second);
  /* null vs ptr / ptr vs null / null vs null*/
  if ((first->kind == MIR_TYPE_PTR && second->kind == MIR_TYPE_NULL) ||
      (first->kind == MIR_TYPE_NULL && second->kind == MIR_TYPE_PTR) ||
      (first->kind == MIR_TYPE_NULL && second->kind == MIR_TYPE_NULL))
    return true;

  if (first->kind != second->kind) return false;

  switch (first->kind) {
  case MIR_TYPE_INT: {
    return first->data.integer.bitcount == second->data.integer.bitcount &&
           first->data.integer.is_signed == second->data.integer.is_signed;
  }

  case MIR_TYPE_REAL: {
    return first->data.real.bitcount == second->data.real.bitcount;
  }

  case MIR_TYPE_PTR: {
    return type_cmp(mir_deref_type(first), mir_deref_type(second));
  }

  case MIR_TYPE_FN: {
    if (!type_cmp(first->data.fn.ret_type, second->data.fn.ret_type)) return false;
    BArray *     fargs = first->data.fn.arg_types;
    BArray *     sargs = second->data.fn.arg_types;
    const size_t fargc = fargs ? bo_array_size(fargs) : 0;
    const size_t sargc = sargs ? bo_array_size(sargs) : 0;

    if (fargc != sargc) return false;

    MirType *ftmp, *stmp;
    if (fargc) {
      barray_foreach(fargs, ftmp)
      {
        stmp = bo_array_at(sargs, i, MirType *);
        assert(stmp && ftmp);
        if (!type_cmp(ftmp, stmp)) return false;
      }
    }

    return true;
  }

  case MIR_TYPE_ARRAY: {
    if (first->data.array.len != second->data.array.len) return false;
    return type_cmp(first->data.array.elem_type, second->data.array.elem_type);
  }

  case MIR_TYPE_STRUCT: {
    /* slice is builtin so we can skip other comparations here... */
    if (mir_is_slice_type(first)) {
      /* second in not slice! */
      if (!mir_is_slice_type(second)) return false;
      /* validate slice kinds */
      if (first->data.strct.kind != second->data.strct.kind) return false;
    }

    /* HACK: here we compare named types if there is some name, later we prefer to create some kind
     * of type hashing. */
    if (first->user_id && second->user_id && first->user_id->hash == second->user_id->hash)
      return true;

    BArray *fmems = first->data.strct.members;
    BArray *smems = second->data.strct.members;
    assert(fmems && smems);

    /* Different count of members. */
    if (bo_array_size(fmems) != bo_array_size(smems)) return false;

    /* Compare members types. */
    MirType *ftmp;
    MirType *stmp;
    barray_foreach(fmems, ftmp)
    {
      stmp = bo_array_at(smems, i, MirType *);
      assert(fmems && smems);

      if (!type_cmp(ftmp, stmp)) return false;
    }

    return true;
  }

  case MIR_TYPE_VOID:
  case MIR_TYPE_TYPE:
  case MIR_TYPE_BOOL:
    return true;

  default:
    break;
  }

#if BL_DEBUG
  char tmp_first[256];
  char tmp_second[256];
  mir_type_to_str(tmp_first, 256, first, true);
  mir_type_to_str(tmp_second, 256, second, true);
  msg_warning("missing type comparation for types %s and %s!!!", tmp_first, tmp_second);
#endif

  return false;
}

/* analyze */
void
reduce_instr(Context *cnt, MirInstr *instr)
{
  if (!instr) return;
  /* instruction unknown in compile time cannot be reduced */
  if (!instr->comptime) return;

  switch (instr->kind) {
  case MIR_INSTR_CONST:
  case MIR_INSTR_DECL_MEMBER:
  case MIR_INSTR_TYPE_FN:
  case MIR_INSTR_TYPE_ARRAY:
  case MIR_INSTR_TYPE_PTR:
  case MIR_INSTR_TYPE_STRUCT:
  case MIR_INSTR_TYPE_SLICE:
  case MIR_INSTR_TYPE_VARGS:
  case MIR_INSTR_SIZEOF:
  case MIR_INSTR_ALIGNOF:
  case MIR_INSTR_INIT:
    erase_instr(instr);
    break;

  case MIR_INSTR_BINOP: {
    exec_instr_binop(cnt, (MirInstrBinop *)instr);
    erase_instr(instr);
    break;
  }

  case MIR_INSTR_UNOP: {
    exec_instr_unop(cnt, (MirInstrUnop *)instr);
    erase_instr(instr);
    break;
  }

  case MIR_INSTR_CAST: {
    exec_instr_cast(cnt, (MirInstrCast *)instr);
    erase_instr(instr);
    break;
  }

  case MIR_INSTR_LOAD: {
    exec_instr_load(cnt, (MirInstrLoad *)instr);
    erase_instr(instr);
    break;
  }

  case MIR_INSTR_DECL_REF: {
    exec_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
    erase_instr(instr);
    break;
  }

  default:
    break;
  }
}

uint64_t
analyze_instr_init(Context *cnt, MirInstrInit *init)
{
  BArray *values = init->values;

  init->type           = insert_instr_load_if_needed(cnt, init->type);
  MirInstr *instr_type = init->type;
  reduce_instr(cnt, instr_type);
  if (instr_type->const_value.type->kind != MIR_TYPE_TYPE) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, instr_type->node->src,
                BUILDER_CUR_WORD, "Expected type before compound expression.");
    return ANALYZE_FAILED;
  }

  MirType *type = instr_type->const_value.data.v_type;
  assert(type);
  MirInstr *   value;
  const size_t valc      = values ? bo_array_size(values) : 0;
  bool         comptime  = true;
  bool         zero_init = false;

  /* Check if array is supposed to be initilialized to {0} */
  if (valc == 1) {
    value = bo_array_at(values, 0, MirInstr *);
    if (value->kind == MIR_INSTR_CONST && value->const_value.type->kind == MIR_TYPE_INT &&
        value->const_value.data.v_u64 == 0) {
      reduce_instr(cnt, value);
      zero_init = true;
    }
  }

  switch (type->kind) {
  case MIR_TYPE_ARRAY: {
    if (zero_init) {
      init->base.const_value.data.v_array.is_zero_initializer = true;
      break;
    }

    if (valc != type->data.array.len) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_INITIALIZER, init->base.node->src,
                  BUILDER_CUR_WORD,
                  "Array initializer must explicitly set all array elements of the array or "
                  "initialize array to 0 by zero initializer {0}. Expected is %llu but given %llu.",
                  (unsigned long long)type->data.array.len, (unsigned long long)valc);
      return ANALYZE_FAILED;
    }

    /* Else iterate over values */
    MirInstr **value_ref;
    for (size_t i = 0; i < valc; ++i) {
      value_ref    = &bo_array_at(values, i, MirInstr *);
      (*value_ref) = insert_instr_load_if_needed(cnt, *value_ref);
      reduce_instr(cnt, *value_ref);

      /* validate value type */
      bool is_valid;
      *value_ref = try_impl_cast(cnt, *value_ref, type->data.array.elem_type, &is_valid);
      if (!is_valid) return ANALYZE_FAILED;

      comptime = (*value_ref)->comptime ? comptime : false;
    }

    // NOTE: Instructions can be used as values!!!
    init->base.const_value.data.v_array.elems = values;
    break;
  }

  case MIR_TYPE_STRUCT: {
    comptime = true;

    if (zero_init) {
      init->base.const_value.data.v_struct.is_zero_initializer = true;
      break;
    }

    const size_t memc = bo_array_size(type->data.strct.members);
    if (valc != memc) {
      builder_msg(
          cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_INITIALIZER, init->base.node->src,
          BUILDER_CUR_WORD,
          "Structure initializer must explicitly set all members of the structure or "
          "initialize structure to 0 by zero initializer {0}. Expected is %llu but given %llu.",
          (unsigned long long)memc, (unsigned long long)valc);
      return ANALYZE_FAILED;
    }

    /* Else iterate over values */
    MirInstr **value_ref;
    MirType *  member_type;
    for (size_t i = 0; i < valc; ++i) {
      value_ref    = &bo_array_at(values, i, MirInstr *);
      member_type  = bo_array_at(type->data.strct.members, i, MirType *);
      (*value_ref) = insert_instr_load_if_needed(cnt, *value_ref);
      reduce_instr(cnt, *value_ref);

      /* validate value type */
      bool is_valid;
      *value_ref = try_impl_cast(cnt, *value_ref, member_type, &is_valid);
      if (!is_valid) return ANALYZE_FAILED;

      comptime = (*value_ref)->comptime ? comptime : false;
    }

    // NOTE: Instructions can be used as values!!!
    init->base.const_value.data.v_struct.members = values;
    break;
  }

  default:
    bl_unimplemented;
  }

  init->base.comptime         = comptime;
  init->base.const_value.type = type;
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_vargs(Context *cnt, MirInstrVArgs *vargs)
{
  MirType *type   = vargs->type;
  BArray * values = vargs->values;
  assert(type && values);

  type = create_type_vargs(cnt, create_type_ptr(cnt, type));

  const size_t valc = bo_array_size(values);

  MirFn *fn = get_current_fn(cnt);
  if (valc > 0) {
    /* Prepare tmp array for values */
    const char *tmp_name = gen_uq_name(cnt, IMPL_VARGS_TMP_ARR);
    MirType *   tmp_type = create_type_array(cnt, vargs->type, valc);
    vargs->arr_tmp       = create_var_impl(cnt, tmp_name, tmp_type, NULL, true, false);
    bo_array_push_back(fn->variables, vargs->arr_tmp);
  }

  {
    /* Prepare tmp slice for vargs */
    const char *tmp_name = gen_uq_name(cnt, IMPL_VARGS_TMP);
    vargs->vargs_tmp     = create_var_impl(cnt, tmp_name, type, NULL, true, false);
    bo_array_push_back(fn->variables, vargs->vargs_tmp);
  }

  MirInstr **value;
  MirType *  value_type;
  bool       is_valid = true;

  for (size_t i = 0; i < valc && is_valid; ++i) {
    value = &bo_array_at(values, i, MirInstr *);

    *value     = insert_instr_load_if_needed(cnt, *value);
    value_type = (*value)->const_value.type;

    /* setup correct type of llvm null for */
    setup_null_type_if_needed(cnt, &(*value)->const_value, value_type);

    (*value) = try_impl_cast(cnt, (*value), vargs->type, &is_valid);
    reduce_instr(cnt, *value);
  }

  vargs->base.const_value.type = type;
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
  elem_ptr->index = insert_instr_load_if_needed(cnt, elem_ptr->index);
  assert(elem_ptr->index);

  bool valid;
  elem_ptr->index = try_impl_cast(cnt, elem_ptr->index, cnt->builtin_types.entry_usize, &valid);
  if (!valid) return ANALYZE_FAILED;

  MirInstr *arr_ptr = elem_ptr->arr_ptr;
  assert(arr_ptr);
  assert(arr_ptr->const_value.type);

  assert(mir_is_pointer_type(arr_ptr->const_value.type));
  MirType *arr_type = mir_deref_type(arr_ptr->const_value.type);
  assert(arr_type);

  if (arr_type->kind == MIR_TYPE_ARRAY) {
    /* array */
    if (elem_ptr->index->comptime) {
      const size_t len = arr_type->data.array.len;
      const size_t i   = elem_ptr->index->const_value.data.v_u64;
      if (i >= len) {
        builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_BOUND_CHECK_FAILED,
                    elem_ptr->index->node->src, BUILDER_CUR_WORD,
                    "Array index is out of the bounds (%llu)", i);
        return ANALYZE_FAILED;
      }
    }

    /* setup ElemPtr instruction const_value type */
    MirType *elem_type = arr_type->data.array.elem_type;
    assert(elem_type);
    elem_ptr->base.const_value.type = create_type_ptr(cnt, elem_type);
  } else if (mir_is_slice_type(arr_type)) {
    /* Support of direct slice access -> slice[N]
     * Since slice is special kind of structure data we need to handle access to pointer and lenght
     * later during execuion. We cannot create member poiner instruction here because we need check
     * boundaries on array later during runtime. This leads to special kind of elemptr
     * interpretation and IR generation also.
     */
    BArray *members = arr_type->data.strct.members;
    assert(members);

    /* setup type */
    MirType *elem_type = bo_array_at(members, 1, MirType *);
    assert(elem_type);
    elem_ptr->base.const_value.type = elem_type;

    /* this is important!!! */
    elem_ptr->target_is_slice = true;
  } else {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, arr_ptr->node->src,
                BUILDER_CUR_WORD, "Expected array or slice type.");
    return ANALYZE_FAILED;
  }

  reduce_instr(cnt, elem_ptr->arr_ptr);
  reduce_instr(cnt, elem_ptr->index);
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
  MirInstr *target_ptr = member_ptr->target_ptr;
  assert(target_ptr);
  MirType *target_type = target_ptr->const_value.type;
  assert(target_type->kind == MIR_TYPE_PTR && "this should be compiler error");
  Ast *ast_member_ident = member_ptr->member_ident;

  target_type = mir_deref_type(target_type);
  if (target_type->kind == MIR_TYPE_ARRAY) {
    /* check array builtin members */
    if (member_ptr->builtin_id == MIR_BUILTIN_ARR_LEN ||
        is_builtin(ast_member_ident, MIR_BUILTIN_ARR_LEN)) {
      /* .len */
      assert(member_ptr->target_ptr->kind == MIR_INSTR_DECL_REF);
      erase_instr(member_ptr->target_ptr);
      /* mutate instruction into constant */
      MirInstr *len               = mutate_instr(&member_ptr->base, MIR_INSTR_CONST);
      len->comptime               = true;
      len->const_value.type       = cnt->builtin_types.entry_usize;
      len->const_value.data.v_u64 = target_type->data.array.len;
    } else if (member_ptr->builtin_id == MIR_BUILTIN_ARR_PTR ||
               is_builtin(ast_member_ident, MIR_BUILTIN_ARR_PTR)) {
      /* .ptr -> This will be replaced by:
       *     elemptr
       *     addrof
       * to match syntax: &array[0]
       */

      MirInstr *index    = create_instr_const_usize(cnt, NULL, 0);
      MirInstr *elem_ptr = create_instr_elem_ptr(cnt, NULL, target_ptr, index, false);
      ref_instr(elem_ptr);

      insert_instr_before(&member_ptr->base, elem_ptr);

      analyze_instr_rq(cnt, index);
      analyze_instr_rq(cnt, elem_ptr);

      MirInstrAddrOf *addrof_elem =
          (MirInstrAddrOf *)mutate_instr(&member_ptr->base, MIR_INSTR_ADDROF);
      addrof_elem->src = elem_ptr;
      analyze_instr_rq(cnt, &addrof_elem->base);
    } else {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_MEMBER_ACCESS, ast_member_ident->src,
                  BUILDER_CUR_WORD, "Unknown member.");
    }
  } else {
    if (target_type->kind == MIR_TYPE_PTR) {
      /* we try to access structure member via pointer so we need one more load */
      member_ptr->target_ptr = insert_instr_load_if_needed(cnt, member_ptr->target_ptr);
      assert(member_ptr->target_ptr);
      target_type = mir_deref_type(target_type);
    }

    if (target_type->kind != MIR_TYPE_STRUCT) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_MEMBER_ACCESS, target_ptr->node->src,
                  BUILDER_CUR_WORD, "Expected structure type.");
      return ANALYZE_FAILED;
    }

    reduce_instr(cnt, member_ptr->target_ptr);

    if (target_type->data.strct.kind & MIR_TS_SLICE) {
      /* slice!!! */
      BArray *slice_members = target_type->data.strct.members;
      assert(slice_members);
      MirType *len_type = bo_array_at(slice_members, 0, MirType *);
      MirType *ptr_type = bo_array_at(slice_members, 1, MirType *);

      if (member_ptr->builtin_id == MIR_BUILTIN_ARR_LEN ||
          is_builtin(ast_member_ident, MIR_BUILTIN_ARR_LEN)) {
        /* .len builtin member of slices */
        member_ptr->builtin_id            = MIR_BUILTIN_ARR_LEN;
        member_ptr->base.const_value.type = create_type_ptr(cnt, len_type);
      } else if (member_ptr->builtin_id == MIR_BUILTIN_ARR_PTR ||
                 is_builtin(ast_member_ident, MIR_BUILTIN_ARR_PTR)) {
        /* .ptr builtin member of slices */
        member_ptr->builtin_id            = MIR_BUILTIN_ARR_PTR;
        member_ptr->base.const_value.type = create_type_ptr(cnt, ptr_type);
      } else {
        builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNKNOWN_SYMBOL,
                    member_ptr->member_ident->src, BUILDER_CUR_WORD, "Unknown slice member.");
        return ANALYZE_FAILED;
      }
    } else {
      /* lookup for member inside struct */
      Scope *     scope = target_type->data.strct.scope;
      ID *        rid   = &ast_member_ident->data.ident.id;
      ScopeEntry *found = scope_lookup(scope, rid, false);
      if (!found) {
        builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNKNOWN_SYMBOL,
                    member_ptr->member_ident->src, BUILDER_CUR_WORD, "Unknown structure member.");
        return ANALYZE_FAILED;
      }

      {
        assert(found->kind == SCOPE_ENTRY_MEMBER);
        MirMember *member = found->data.member;

        /* setup member_ptr type */
        MirType *type = create_type_ptr(cnt, member->type);
        assert(type);
        member_ptr->base.const_value.type = type;
      }

      member_ptr->scope_entry = found;
    }
  }

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_addrof(Context *cnt, MirInstrAddrOf *addrof)
{
  MirInstr *src = addrof->src;
  assert(src);
  if (src->kind != MIR_INSTR_DECL_REF && src->kind != MIR_INSTR_ELEM_PTR) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_DECL, addrof->base.node->src,
                BUILDER_CUR_WORD, "Cannot take the address of unallocated object.");
    return ANALYZE_FAILED;
  }

  if (src->const_value.addr_mode == MIR_VAM_LVALUE_CONST) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_ADDRES_MODE, addrof->base.node->src,
                BUILDER_CUR_WORD, "Cannot take address of constant.");
  }

  /* setup type */
  addrof->base.const_value.type = src->const_value.type;
  assert(addrof->base.const_value.type && "invalid type");

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_cast(Context *cnt, MirInstrCast *cast)
{
  MirType *dest_type = cast->base.const_value.type;
  cast->next         = insert_instr_load_if_needed(cnt, cast->next);
  MirInstr *src      = cast->next;
  assert(src);

  MirType *src_type = src->const_value.type;

  if (!dest_type) {
    assert(cast->type && cast->type->kind == MIR_INSTR_CALL);
    analyze_instr_rq(cnt, cast->type);
    MirConstValue *type_val = exec_call_top_lvl(cnt, (MirInstrCall *)cast->type);
    unref_instr(cast->type);
    assert(type_val->type && type_val->type->kind == MIR_TYPE_TYPE);
    dest_type = type_val->data.v_type;
  }

  assert(dest_type && "invalid cast destination type");
  assert(src_type && "invalid cast source type");

  cast->op = get_cast_op(src_type, dest_type);
  if (cast->op == MIR_CAST_INVALID) {
    error_types(cnt, src_type, dest_type, cast->base.node, "Invalid cast from '%s' to '%s'.");
    return ANALYZE_FAILED;
  }

  reduce_instr(cnt, cast->next);

  cast->base.const_value.type = dest_type;
  cast->base.comptime         = cast->next->comptime;
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_sizeof(Context *cnt, MirInstrSizeof *szof)
{
  assert(szof->expr);
  szof->expr = insert_instr_load_if_needed(cnt, szof->expr);
  reduce_instr(cnt, szof->expr);

  MirType *type = szof->expr->const_value.type;
  assert(type);

  if (type->kind == MIR_TYPE_TYPE) {
    type = szof->expr->const_value.data.v_type;
    assert(type);
  }

  szof->base.const_value.data.v_u64 = type->store_size_bytes;
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info)
{
  assert(type_info->expr);
  type_info->expr = insert_instr_load_if_needed(cnt, type_info->expr);
  reduce_instr(cnt, type_info->expr);

  MirType *type = type_info->expr->const_value.type;
  assert(type);

  if (type->kind == MIR_TYPE_TYPE) {
    type = type_info->expr->const_value.data.v_type;
    assert(type);
  }

  type_info->type_table_index = type->type_table_index;

  /* Resolve TypeInfo struct type */
  MirType *ret_type = NULL;

  {
    Scope *     gscope = cnt->assembly->gscope;
    ID *        id     = &builtin_ids[MIR_BUILTIN_TYPE_INFO];
    ScopeEntry *found  = scope_lookup(gscope, id, true);
    assert(found &&
           "TypeInfo base struct not found! This should be an error, we need to load 'core.bl'.");
    assert(found->kind == SCOPE_ENTRY_VAR);

    MirVar *var = found->data.var;
    assert(var);
    assert(var->comptime && var->alloc_type->kind == MIR_TYPE_TYPE);

    ret_type = var->value->data.v_type;
    assert(ret_type && ret_type->kind == MIR_TYPE_STRUCT);
    ret_type = create_type_ptr(cnt, ret_type);
  }

  assert(ret_type);
  type_info->base.const_value.type = ret_type;

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_alignof(Context *cnt, MirInstrAlignof *alof)
{
  assert(alof->expr);
  alof->expr = insert_instr_load_if_needed(cnt, alof->expr);
  reduce_instr(cnt, alof->expr);

  MirType *type = alof->expr->const_value.type;
  assert(type);

  if (type->kind == MIR_TYPE_TYPE) {
    type = alof->expr->const_value.data.v_type;
    assert(type);
  }

  alof->base.const_value.data.v_u64 = type->alignment;
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
  assert(ref->rid && ref->scope);

  ScopeEntry *found = scope_lookup(ref->scope, ref->rid, true);
  if (!found) {
    return ref->rid->hash;
  }

  switch (found->kind) {
  case SCOPE_ENTRY_FN: {
    MirFn *fn = found->data.fn;
    assert(fn);
    MirType *type = fn->type;
    assert(type);

    ref->base.const_value.type      = type;
    ref->base.const_value.data.v_fn = found->data.fn;
    ref->base.comptime              = true;
    ++fn->ref_count;
    break;
  }

  case SCOPE_ENTRY_TYPE: {
    ref->base.const_value.type        = cnt->builtin_types.entry_type;
    ref->base.const_value.data.v_type = found->data.type;
    ref->base.comptime                = true;
    break;
  }

  case SCOPE_ENTRY_VAR: {
    MirVar *var = found->data.var;
    assert(var);
    ++var->ref_count;
    MirType *type = var->alloc_type;
    assert(type);

    type                            = create_type_ptr(cnt, type);
    ref->base.const_value.type      = type;
    ref->base.comptime              = var->comptime;
    ref->base.const_value.addr_mode = var->is_mutable ? MIR_VAM_LVALUE : MIR_VAM_LVALUE_CONST;
    /* set pointer to variable const value directly when variable is compile time known */
    if (var->comptime) ref->base.const_value.data.v_void_ptr = found->data.var->value;
    break;
  }

  default:
    bl_abort("invalid scope entry kind");
  }

  ref->scope_entry = found;
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_arg(Context *cnt, MirInstrArg *arg)
{
  MirFn *fn = arg->base.owner_block->owner_fn;
  assert(fn);

  BArray *arg_types = fn->type->data.fn.arg_types;
  assert(arg_types && "trying to reference type of argument in function without arguments");

  assert(arg->i < bo_array_size(arg_types));

  MirType *type = bo_array_at(arg_types, arg->i, MirType *);
  assert(type);
  arg->base.const_value.type = type;

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
  /* nothing to do :( */
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
  /* resolve type */
  if (!fn_proto->base.const_value.type) {
    /* Analyze type of funcion literal. Here we expect call to type resolver function! */
    assert(fn_proto->type && fn_proto->type->kind == MIR_INSTR_CALL);
    if (analyze_instr(cnt, fn_proto->type) != ANALYZE_PASSED) return ANALYZE_POSTPONE;
    MirConstValue *type_val = exec_call_top_lvl(cnt, (MirInstrCall *)fn_proto->type);
    unref_instr(fn_proto->type);
    assert(type_val->type && type_val->type->kind == MIR_TYPE_TYPE);

    /* Analyze user defined type (this must be compared with infered type). */
    if (fn_proto->user_type) {
      assert(fn_proto->user_type->kind == MIR_INSTR_CALL);
      if (analyze_instr(cnt, fn_proto->user_type) != ANALYZE_PASSED) return ANALYZE_POSTPONE;
      MirConstValue *user_type_val = exec_call_top_lvl(cnt, (MirInstrCall *)fn_proto->user_type);
      unref_instr(fn_proto->user_type);
      assert(user_type_val->type && user_type_val->type->kind == MIR_TYPE_TYPE);

      if (!type_cmp(type_val->data.v_type, user_type_val->data.v_type)) {
        error_types(cnt, type_val->data.v_type, user_type_val->data.v_type,
                    fn_proto->user_type->node, NULL);
      }
    }

    if (!type_val->data.v_type) return ANALYZE_FAILED;
    assert(type_val->data.v_type->kind == MIR_TYPE_FN);
    fn_proto->base.const_value.type = type_val->data.v_type;
  }

  MirConstValue *value = &fn_proto->base.const_value;

  assert(value->type && "function has no valid type");
  assert(value->data.v_fn);
  value->data.v_fn->type = fn_proto->base.const_value.type;

  MirFn *fn = fn_proto->base.const_value.data.v_fn;
  assert(fn);

  /* implicit functions has no name -> generate one */
  if (!fn->llvm_name) {
    fn->llvm_name = gen_uq_name(cnt, IMPL_FN_NAME);
  }

  if (fn->flags & (FLAG_EXTERN)) {
    /* lookup external function exec handle */
    assert(fn->llvm_name);

    void * handle = NULL;
    DLLib *lib;
    barray_foreach(cnt->dl.libs, lib)
    {
      handle = dlFindSymbol(lib, fn->llvm_name);
      if (handle) break;
    }

    if (!handle) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNKNOWN_SYMBOL, fn_proto->base.node->src,
                  BUILDER_CUR_WORD, "External symbol '%s' not found.", fn->llvm_name);
    }

    fn->extern_entry = handle;
  } else {
    /* Add entry block of the function into analyze queue. */
    MirInstr *entry_block = (MirInstr *)fn->first_block;
    assert(entry_block);
    analyze_push_front(cnt, entry_block);
  }

  if (fn->id) provide_fn(cnt, fn);

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
  br->cond = insert_instr_load_if_needed(cnt, br->cond);
  assert(br->cond && br->then_block && br->else_block);
  assert(br->cond->analyzed);

  MirType *cond_type = br->cond->const_value.type;
  assert(cond_type);

  bool valid;
  br->cond = try_impl_cast(cnt, br->cond, cnt->builtin_types.entry_bool, &valid);
  if (!valid) return ANALYZE_FAILED;

  reduce_instr(cnt, br->cond);

  /* PERFORMANCE: When condition is known in compile time, we can discard whole else/then block
   * based on condition resutl. It is not possible because we don't have tracked down execution tree
   * for now. */

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_br(Context *cnt, MirInstrBr *br)
{
  assert(br->then_block);
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_load(Context *cnt, MirInstrLoad *load)
{
  MirInstr *src = load->src;
  assert(src);
  if (!mir_is_pointer_type(src->const_value.type)) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, src->node->src, BUILDER_CUR_WORD,
                "Expected pointer.");
    return ANALYZE_FAILED;
  }

  MirType *type = mir_deref_type(src->const_value.type);
  assert(type);
  load->base.const_value.type = type;

  reduce_instr(cnt, src);
  load->base.comptime              = src->comptime;
  load->base.const_value.addr_mode = MIR_VAM_RVALUE;

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn)
{
  assert(type_fn->base.const_value.type);
  assert(type_fn->ret_type ? type_fn->ret_type->analyzed : true);

  bool is_vargs = false;

  BArray *arg_types = NULL;
  if (type_fn->arg_types) {
    arg_types = create_arr(cnt, sizeof(MirType *));
    bo_array_reserve(arg_types, bo_array_size(type_fn->arg_types));

    MirInstr *arg_type;
    MirType * tmp;
    barray_foreach(type_fn->arg_types, arg_type)
    {
      assert(arg_type->comptime);
      tmp = arg_type->const_value.data.v_type;
      assert(tmp);

      if (mir_is_vargs_type(tmp)) {
        is_vargs = true;
        assert(i == bo_array_size(type_fn->arg_types) - 1 &&
               "VArgs must be last, this should be an error");
      }

      bo_array_push_back(arg_types, tmp);
      reduce_instr(cnt, arg_type);
    }
  }

  MirType *ret_type = NULL;
  if (type_fn->ret_type) {
    type_fn->ret_type = insert_instr_load_if_needed(cnt, type_fn->ret_type);
    assert(type_fn->ret_type->comptime);
    ret_type = type_fn->ret_type->const_value.data.v_type;
    assert(ret_type);
    reduce_instr(cnt, type_fn->ret_type);
  }

  type_fn->base.const_value.data.v_type = create_type_fn(cnt, ret_type, arg_types, is_vargs);

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_decl_member(Context *cnt, MirInstrDeclMember *decl)
{
  MirMember *member = decl->member;
  assert(member);

  decl->type = insert_instr_load_if_needed(cnt, decl->type);
  reduce_instr(cnt, decl->type);

  /* NOTE: Members will be provided by instr type struct because we need to know right ordering of
   * members inside structure layout. (index and llvm element offet need to be calculated)*/
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_type_struct(Context *cnt, MirInstrTypeStruct *type_struct)
{
  BArray *members = NULL;

  if (type_struct->members) {
    MirInstr **         member_instr;
    MirInstrDeclMember *decl_member;
    MirMember *         member;
    MirType *           member_type;
    Scope *             scope = type_struct->scope;
    const size_t        memc  = bo_array_size(type_struct->members);

    members = create_arr(cnt, sizeof(MirType *));
    bo_array_reserve(members, bo_array_size(type_struct->members));

    for (size_t i = 0; i < memc; ++i) {
      member_instr = &bo_array_at(type_struct->members, i, MirInstr *);

      *member_instr = insert_instr_load_if_needed(cnt, *member_instr);
      reduce_instr(cnt, *member_instr);

      decl_member = (MirInstrDeclMember *)*member_instr;
      assert(decl_member->base.kind == MIR_INSTR_DECL_MEMBER);
      assert(decl_member->base.comptime);

      /* solve member type */
      member_type = decl_member->type->const_value.data.v_type;

      assert(member_type);
      bo_array_push_back(members, member_type);

      /* setup and provide member */
      member = decl_member->member;
      assert(member);
      member->type  = member_type;
      member->scope = scope;
      member->index = i;

      provide_member(cnt, member);
    }
  }

  type_struct->base.const_value.data.v_type = create_type_struct(
      cnt, type_struct->id, type_struct->scope, members, type_struct->is_packed, MIR_TS_NONE);
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_type_slice(Context *cnt, MirInstrTypeSlice *type_slice)
{
  assert(type_slice->elem_type);
  type_slice->elem_type = insert_instr_load_if_needed(cnt, type_slice->elem_type);

  reduce_instr(cnt, type_slice->elem_type);

  ID *id = NULL;
  if (type_slice->base.node && type_slice->base.node->kind == AST_IDENT) {
    id = &type_slice->base.node->data.ident.id;
  }

  assert(type_slice->elem_type->comptime && "This should be an error");
  MirType *elem_type = type_slice->elem_type->const_value.data.v_type;
  assert(elem_type);
  elem_type                                = create_type_ptr(cnt, elem_type);
  type_slice->base.const_value.data.v_type = create_type_slice(cnt, id, elem_type);

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_type_vargs(Context *cnt, MirInstrTypeVArgs *type_vargs)
{
  assert(type_vargs->elem_type);
  type_vargs->elem_type = insert_instr_load_if_needed(cnt, type_vargs->elem_type);

  reduce_instr(cnt, type_vargs->elem_type);

  assert(type_vargs->elem_type->comptime && "This should be an error");
  MirType *elem_type = type_vargs->elem_type->const_value.data.v_type;
  assert(elem_type);
  elem_type                                = create_type_ptr(cnt, elem_type);
  type_vargs->base.const_value.data.v_type = create_type_vargs(cnt, elem_type);

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr)
{
  assert(type_arr->base.const_value.type);
  assert(type_arr->elem_type->analyzed);

  type_arr->len = insert_instr_load_if_needed(cnt, type_arr->len);

  bool valid;
  type_arr->len = try_impl_cast(cnt, type_arr->len, cnt->builtin_types.entry_usize, &valid);
  if (!valid) return ANALYZE_FAILED;

  /* len */
  if (!type_arr->len->comptime) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_CONST, type_arr->len->node->src,
                BUILDER_CUR_WORD, "Array size must be compile-time constant.");
    return ANALYZE_FAILED;
  }

  assert(type_arr->len->comptime && "this must be error");
  reduce_instr(cnt, type_arr->len);

  const size_t len = type_arr->len->const_value.data.v_u64;
  if (len == 0) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_ARR_SIZE, type_arr->len->node->src,
                BUILDER_CUR_WORD, "Array size cannot be 0.");
    return ANALYZE_FAILED;
  }

  /* elem type */
  assert(type_arr->elem_type->comptime);
  reduce_instr(cnt, type_arr->elem_type);

  MirType *elem_type = type_arr->elem_type->const_value.data.v_type;
  assert(elem_type);

  type_arr->base.const_value.data.v_type = create_type_array(cnt, elem_type, len);
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr)
{
  assert(type_ptr->type);

  type_ptr->type = insert_instr_load_if_needed(cnt, type_ptr->type);
  reduce_instr(cnt, type_ptr->type);
  assert(type_ptr->type->comptime);
  MirType *src_type = type_ptr->type->const_value.data.v_type;
  assert(src_type);

  if (src_type->kind == MIR_TYPE_TYPE) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, type_ptr->base.node->src,
                BUILDER_CUR_WORD, "Cannot create pointer to type.");
    return ANALYZE_FAILED;
  }

  type_ptr->base.const_value.data.v_type = create_type_ptr(cnt, src_type);
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_binop(Context *cnt, MirInstrBinop *binop)
{
#define is_valid(_type, _op)                                                                       \
  (((_type)->kind == MIR_TYPE_INT) || ((_type)->kind == MIR_TYPE_NULL) ||                          \
   ((_type)->kind == MIR_TYPE_REAL) || ((_type)->kind == MIR_TYPE_PTR) ||                          \
   ((_type)->kind == MIR_TYPE_BOOL && ast_binop_is_logic(_op)))

  /* insert load instructions is the are needed */
  binop->lhs = insert_instr_load_if_needed(cnt, binop->lhs);
  binop->rhs = insert_instr_load_if_needed(cnt, binop->rhs);

  /* setup llvm type for null type */
  if (binop->lhs->const_value.type->kind == MIR_TYPE_NULL)
    setup_null_type_if_needed(cnt, &binop->lhs->const_value, binop->rhs->const_value.type);
  else
    setup_null_type_if_needed(cnt, &binop->rhs->const_value, binop->lhs->const_value.type);

  bool valid;
  binop->rhs = try_impl_cast(cnt, binop->rhs, binop->lhs->const_value.type, &valid);
  if (!valid) return ANALYZE_FAILED;

  MirInstr *lhs = binop->lhs;
  MirInstr *rhs = binop->rhs;
  assert(lhs && rhs);
  assert(lhs->analyzed);
  assert(rhs->analyzed);

  const bool lhs_valid = is_valid(lhs->const_value.type, binop->op);
  const bool rhs_valid = is_valid(rhs->const_value.type, binop->op);

  if (!(lhs_valid && rhs_valid)) {
    error_types(cnt, lhs->const_value.type, rhs->const_value.type, binop->base.node,
                "invalid operation for %s type");
    return ANALYZE_FAILED;
  }

  MirType *type =
      ast_binop_is_logic(binop->op) ? cnt->builtin_types.entry_bool : lhs->const_value.type;
  assert(type);
  binop->base.const_value.type = type;

  reduce_instr(cnt, rhs);
  reduce_instr(cnt, lhs);

  /* when binary operation has lhs and rhs values known in compile it is known in compile time also
   */
  if (lhs->comptime && rhs->comptime) binop->base.comptime = true;

  return ANALYZE_PASSED;
#undef is_valid
}

uint64_t
analyze_instr_unop(Context *cnt, MirInstrUnop *unop)
{
  unop->instr = insert_instr_load_if_needed(cnt, unop->instr);
  assert(unop->instr && unop->instr->analyzed);
  MirType *type = unop->instr->const_value.type;
  assert(type);
  unop->base.const_value.type = type;

  unop->base.comptime = unop->instr->comptime;
  reduce_instr(cnt, unop->instr);

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_const(Context *cnt, MirInstrConst *cnst)
{
  assert(cnst->base.const_value.type);
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_ret(Context *cnt, MirInstrRet *ret)
{
  /* compare return value with current function type */
  MirInstrBlock *block = ret->base.owner_block;
  if (!block->terminal) block->terminal = &ret->base;

  ret->value      = insert_instr_load_if_needed(cnt, ret->value);
  MirInstr *value = ret->value;
  if (value) {
    assert(value->analyzed);
  }

  MirType *fn_type = get_current_fn(cnt)->type;
  assert(fn_type);
  assert(fn_type->kind == MIR_TYPE_FN);

  if (ret->allow_fn_ret_type_override) {
    /* return is supposed to override function return type */
    if (ret->value) {
      assert(ret->value->const_value.type);
      if (fn_type->data.fn.ret_type != ret->value->const_value.type) {
        MirFn *fn = get_current_fn(cnt);
        assert(fn);
        fn->type = create_type_fn(cnt, ret->value->const_value.type, fn_type->data.fn.arg_types,
                                  fn_type->data.fn.is_vargs);
        fn_type  = fn->type;
        /* HACK: Function type need to be set also for function prototype instruction, this is by
         * the way only reason why we need poinetr to prototype inside MirFn. Better solution should
         * be possible. */
        fn->prototype->const_value.type = fn_type;
      }
    }
  }

  const bool expected_ret_value =
      !type_cmp(fn_type->data.fn.ret_type, cnt->builtin_types.entry_void);

  /* return value is not expected, and it's not provided */
  if (!expected_ret_value && !value) {
    return ANALYZE_PASSED;
  }

  /* return value is expected, but it's not provided */
  if (expected_ret_value && !value) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_EXPR, ret->base.node->src,
                BUILDER_CUR_AFTER, "Expected return value.");
    return ANALYZE_FAILED;
  }

  /* return value is not expected, but it's provided */
  if (!expected_ret_value && value) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_EXPR, ret->value->node->src,
                BUILDER_CUR_WORD, "Unexpected return value.");
    return ANALYZE_FAILED;
  }

  /* setup correct type of llvm null for call(null) */
  setup_null_type_if_needed(cnt, &value->const_value, fn_type->data.fn.ret_type);

  bool valid;
  ret->value = try_impl_cast(cnt, ret->value, fn_type->data.fn.ret_type, &valid);
  if (!valid) return ANALYZE_FAILED;

  reduce_instr(cnt, ret->value);

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
  MirVar *var = decl->var;
  assert(var);

  if (decl->type && var->alloc_type == NULL) {
    assert(decl->type->kind == MIR_INSTR_CALL && "expected type resolver call");
    if (analyze_instr(cnt, decl->type) != ANALYZE_PASSED) return ANALYZE_POSTPONE;
    MirConstValue *resolved_type_value = exec_call_top_lvl(cnt, (MirInstrCall *)decl->type);
    unref_instr(decl->type);
    assert(resolved_type_value && resolved_type_value->type->kind == MIR_TYPE_TYPE);
    MirType *resolved_type = resolved_type_value->data.v_type;
    if (!resolved_type) return ANALYZE_FAILED;

    var->alloc_type = resolved_type;
  }

  if (decl->init) {
    if (decl->init->kind == MIR_INSTR_CALL && decl->init->comptime) {
      if (analyze_instr(cnt, decl->init) != ANALYZE_PASSED) return ANALYZE_POSTPONE;
      exec_call_top_lvl(cnt, (MirInstrCall *)decl->init);
    }
    setup_null_type_if_needed(cnt, &decl->init->const_value, var->alloc_type);
    decl->init = insert_instr_load_if_needed(cnt, decl->init);

    /* validate types or infer */
    if (var->alloc_type) {
      bool is_valid;
      decl->init = try_impl_cast(cnt, decl->init, var->alloc_type, &is_valid);
      if (!is_valid) return ANALYZE_FAILED;
    } else {
      /* infer type */
      MirType *type = decl->init->const_value.type;
      assert(type);
      assert(type->kind != MIR_TYPE_VOID);
      var->alloc_type = type;
    }

    decl->base.comptime = var->comptime = !var->is_mutable && decl->init->comptime;
  } else if (var->is_in_gscope) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNINITIALIZED, decl->base.node->src,
                BUILDER_CUR_WORD, "All globals must be initialized to compile time known value.");
    return ANALYZE_FAILED;
  }

  if (!var->alloc_type) {
    bl_abort("unknown declaration type");
  }

  if (var->alloc_type->kind == MIR_TYPE_TYPE && var->is_mutable) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_MUTABILITY, decl->base.node->src,
                BUILDER_CUR_WORD, "Type declaration must be immutable.");
    return ANALYZE_FAILED;
  }

  if (decl->base.ref_count == 0) {
    builder_msg(cnt->builder, BUILDER_MSG_WARNING, 0, decl->base.node->src, BUILDER_CUR_WORD,
                "Unused declaration.");
  }

  reduce_instr(cnt, decl->init);

  if (decl->base.comptime && decl->init) {
    /* initialize when known in compiletime */
    var->value = &decl->init->const_value;
  }

  /* insert variable into symbol lookup table */
  provide_var(cnt, decl->var);

  /* Type declaration should not be generated in LLVM. */
  const bool gen_llvm = var->alloc_type->kind != MIR_TYPE_TYPE;

  if (var->is_in_gscope) {
    /* Global varibales which are not compile time constants are allocated on the stack, one option
     * is to do allocation every time when we invoke comptime function execution, but we don't know
     * which globals will be used by function and we also don't known whatever function has some
     * side effect or not. So we produce allocation here. Variable will be stored in static data
     * segment. There is no need to use relative pointers here. */
    if (!var->comptime) {
      /* Allocate memory in static block. */
      exec_stack_alloc_var(cnt, var);
      /* Initialize data. */
      exec_instr_decl_var(cnt, decl);
    }
  } else if (gen_llvm) {
    /* store variable into current function (due alloca-first generation pass in LLVM) */
    MirFn *fn = decl->base.owner_block->owner_fn;
    assert(fn);
    bo_array_push_back(fn->variables, var);
  }

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_call(Context *cnt, MirInstrCall *call)
{
  assert(call->callee);
  /* callee has not been analyzed yet -> postpone call analyze */
  if (!call->callee->analyzed) return ANALYZE_POSTPONE;
  // analyze_instr(cnt, call->callee, call->base.comptime || comptime);

  MirType *type = call->callee->const_value.type;
  assert(type && "invalid type of called object");

  if (mir_is_pointer_type(type)) {
    /* we want to make calls also via pointer to functions so in such case we need to resolve
     * pointed function */
    type = mir_deref_type(type);
  }

  if (type->kind != MIR_TYPE_FN) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_FUNC, call->callee->node->src,
                BUILDER_CUR_WORD, "Expected a function name.");
    return ANALYZE_FAILED;
  }

  if (call->base.comptime && !call->callee->const_value.data.v_fn->analyzed_for_cmptime_exec) {
    return ANALYZE_POSTPONE;
  }

  MirType *result_type = type->data.fn.ret_type;
  assert(result_type && "invalid type of call result");
  call->base.const_value.type = result_type;

  /* validate arguments */
  const bool is_vargs = type->data.fn.is_vargs;

  size_t       callee_argc = type->data.fn.arg_types ? bo_array_size(type->data.fn.arg_types) : 0;
  const size_t call_argc   = call->args ? bo_array_size(call->args) : 0;

  if (is_vargs) {
    /* This will gonna be tricky... */
    --callee_argc;
    if ((call_argc < callee_argc)) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_ARG_COUNT, call->base.node->src,
                  BUILDER_CUR_WORD, "Expected at least %u %s, but called with %u.", callee_argc,
                  callee_argc == 1 ? "argument" : "arguments", call_argc);
      return ANALYZE_FAILED;
    }

    MirType *vargs_type = bo_array_at(type->data.fn.arg_types, callee_argc, MirType *);
    assert(mir_is_vargs_type(vargs_type) && "VArgs is expected to be last!!!");
    vargs_type = bo_array_at(vargs_type->data.strct.members, 1, MirType *);
    assert(vargs_type && mir_is_pointer_type(vargs_type));
    vargs_type = mir_deref_type(vargs_type);

    /* Prepare vargs values. */
    const size_t vargsc = call_argc - callee_argc;
    BArray *     values = create_arr(cnt, sizeof(MirInstr *));
    bo_array_reserve(values, vargsc);
    MirInstr *vargs = create_instr_vargs_impl(cnt, vargs_type, values);
    ref_instr(vargs);

    if (vargsc > 0) {
      /* One or more vargs passed. */
      bo_array_reserve(values, vargsc);
      MirInstr **insert_loc = &bo_array_at(call->args, callee_argc, MirInstr *);
      bo_array_insert(values, 0, insert_loc, vargsc);

      insert_instr_after(*insert_loc, vargs);
    } else if (callee_argc > 0) {
      /* No arguments passed into vargs but there are more regular arguments before vargs. */
      MirInstr *insert_loc = bo_array_at(call->args, 0, MirInstr *);
      insert_instr_before(insert_loc, vargs);
    } else {
      insert_instr_before(&call->base, vargs);
    }

    if (!analyze_instr_vargs(cnt, (MirInstrVArgs *)vargs)) return ANALYZE_FAILED;
    vargs->analyzed = true;

    /* Erase vargs from arguments. */
    bo_array_resize(call->args, callee_argc);

    /* Replace last with vargs. */
    bo_array_push_back(call->args, vargs);
  } else {
    if ((callee_argc != call_argc)) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_ARG_COUNT, call->base.node->src,
                  BUILDER_CUR_WORD, "Expected %u %s, but called with %u.", callee_argc,
                  callee_argc == 1 ? "argument" : "arguments", call_argc);
      return ANALYZE_FAILED;
    }
  }

  /* validate argument types */
  if (callee_argc) {
    MirInstr **call_arg;
    MirType *  callee_arg_type;
    bool       valid = true;

    for (size_t i = 0; i < callee_argc && valid; ++i) {
      call_arg        = &bo_array_at(call->args, i, MirInstr *);
      callee_arg_type = bo_array_at(type->data.fn.arg_types, i, MirType *);

      *call_arg = insert_instr_load_if_needed(cnt, *call_arg);

      /* setup correct type of llvm null for call(null) */
      setup_null_type_if_needed(cnt, &(*call_arg)->const_value, callee_arg_type);

      (*call_arg) = try_impl_cast(cnt, (*call_arg), callee_arg_type, &valid);
      reduce_instr(cnt, *call_arg);
    }
  }

  reduce_instr(cnt, call->callee);
  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_store(Context *cnt, MirInstrStore *store)
{
  MirInstr *dest = store->dest;
  assert(dest);
  assert(dest->analyzed);

  if (!mir_is_pointer_type(dest->const_value.type)) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_EXPR, store->base.node->src,
                BUILDER_CUR_WORD, "Left hand side of the expression cannot be assigned.");
    return ANALYZE_FAILED;
  }

  if (dest->const_value.addr_mode == MIR_VAM_LVALUE_CONST) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_EXPR, store->base.node->src,
                BUILDER_CUR_WORD, "Cannot assign to constant.");
  }

  MirType *dest_type = mir_deref_type(dest->const_value.type);
  assert(dest_type && "store destination has invalid base type");

  /* insert load if needed */
  store->src = insert_instr_load_if_needed(cnt, store->src);

  /* setup llvm type for null type */
  setup_null_type_if_needed(cnt, &store->src->const_value, dest_type);

  /* validate types and optionaly create cast */
  bool valid;
  store->src = try_impl_cast(cnt, store->src, dest_type, &valid);
  if (!valid) return ANALYZE_FAILED;

  MirInstr *src = store->src;
  assert(src);

  reduce_instr(cnt, store->src);
  reduce_instr(cnt, store->dest);

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr_block(Context *cnt, MirInstrBlock *block)
{
  assert(block);

  /* append implicit return for void functions or generate error when last block is not terminated
   */
  if (!is_block_terminated(block)) {
    MirFn *fn = block->owner_fn;
    assert(fn);

    if (fn->type->data.fn.ret_type->kind == MIR_TYPE_VOID) {
      set_current_block(cnt, block);
      append_instr_ret(cnt, NULL, NULL, false);
    } else {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_MISSING_RETURN, fn->decl_node->src,
                  BUILDER_CUR_WORD, "Not every path inside function return value.");
    }
  }

  return ANALYZE_PASSED;
}

uint64_t
analyze_instr(Context *cnt, MirInstr *instr)
{
  if (!instr) return ANALYZE_PASSED;

  /* skip already analyzed instructions */
  if (instr->analyzed) return ANALYZE_PASSED;
  uint64_t state;

  if (instr->owner_block) set_current_block(cnt, instr->owner_block);

  switch (instr->kind) {
  case MIR_INSTR_BLOCK:
    state = analyze_instr_block(cnt, (MirInstrBlock *)instr);
    break;
  case MIR_INSTR_FN_PROTO:
    state = analyze_instr_fn_proto(cnt, (MirInstrFnProto *)instr);
    break;
  case MIR_INSTR_DECL_VAR:
    state = analyze_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
    break;
  case MIR_INSTR_DECL_MEMBER:
    state = analyze_instr_decl_member(cnt, (MirInstrDeclMember *)instr);
    break;
  case MIR_INSTR_CALL:
    state = analyze_instr_call(cnt, (MirInstrCall *)instr);
    break;
  case MIR_INSTR_CONST:
    state = analyze_instr_const(cnt, (MirInstrConst *)instr);
    break;
  case MIR_INSTR_RET:
    state = analyze_instr_ret(cnt, (MirInstrRet *)instr);
    break;
  case MIR_INSTR_STORE:
    state = analyze_instr_store(cnt, (MirInstrStore *)instr);
    break;
  case MIR_INSTR_DECL_REF:
    state = analyze_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
    break;
  case MIR_INSTR_BINOP:
    state = analyze_instr_binop(cnt, (MirInstrBinop *)instr);
    break;
  case MIR_INSTR_TYPE_FN:
    state = analyze_instr_type_fn(cnt, (MirInstrTypeFn *)instr);
    break;
  case MIR_INSTR_TYPE_STRUCT:
    state = analyze_instr_type_struct(cnt, (MirInstrTypeStruct *)instr);
    break;
  case MIR_INSTR_TYPE_SLICE:
    state = analyze_instr_type_slice(cnt, (MirInstrTypeSlice *)instr);
    break;
  case MIR_INSTR_TYPE_VARGS:
    state = analyze_instr_type_vargs(cnt, (MirInstrTypeVArgs *)instr);
    break;
  case MIR_INSTR_TYPE_ARRAY:
    state = analyze_instr_type_array(cnt, (MirInstrTypeArray *)instr);
    break;
  case MIR_INSTR_TYPE_PTR:
    state = analyze_instr_type_ptr(cnt, (MirInstrTypePtr *)instr);
    break;
  case MIR_INSTR_LOAD:
    state = analyze_instr_load(cnt, (MirInstrLoad *)instr);
    break;
  case MIR_INSTR_INIT:
    state = analyze_instr_init(cnt, (MirInstrInit *)instr);
    break;
  case MIR_INSTR_BR:
    state = analyze_instr_br(cnt, (MirInstrBr *)instr);
    break;
  case MIR_INSTR_COND_BR:
    state = analyze_instr_cond_br(cnt, (MirInstrCondBr *)instr);
    break;
  case MIR_INSTR_UNOP:
    state = analyze_instr_unop(cnt, (MirInstrUnop *)instr);
    break;
  case MIR_INSTR_UNREACHABLE:
    state = analyze_instr_unreachable(cnt, (MirInstrUnreachable *)instr);
    break;
  case MIR_INSTR_ARG:
    state = analyze_instr_arg(cnt, (MirInstrArg *)instr);
    break;
  case MIR_INSTR_ELEM_PTR:
    state = analyze_instr_elem_ptr(cnt, (MirInstrElemPtr *)instr);
    break;
  case MIR_INSTR_MEMBER_PTR:
    state = analyze_instr_member_ptr(cnt, (MirInstrMemberPtr *)instr);
    break;
  case MIR_INSTR_ADDROF:
    state = analyze_instr_addrof(cnt, (MirInstrAddrOf *)instr);
    break;
  case MIR_INSTR_CAST:
    state = analyze_instr_cast(cnt, (MirInstrCast *)instr);
    break;
  case MIR_INSTR_SIZEOF:
    state = analyze_instr_sizeof(cnt, (MirInstrSizeof *)instr);
    break;
  case MIR_INSTR_ALIGNOF:
    state = analyze_instr_alignof(cnt, (MirInstrAlignof *)instr);
    break;
  case MIR_INSTR_TYPE_INFO:
    state = analyze_instr_type_info(cnt, (MirInstrTypeInfo *)instr);
    break;
  default:
    msg_warning("missing analyze for %s", mir_instr_name(instr));
    return ANALYZE_FAILED;
  }

  instr->analyzed = state == ANALYZE_PASSED;

  /* remove unused instructions */
  /* CLEANUP:
  if (instr->analyzed && instr->ref_count == 0) {
    erase_instr(instr);
  }
  */

  return state;
}

static inline MirInstr *
analyze_try_get_next(MirInstr *instr)
{
  if (!instr) return NULL;
  if (instr->kind == MIR_INSTR_BLOCK) {
    MirInstrBlock *block = (MirInstrBlock *)instr;
    return block->entry_instr;
  }

  /* Instruction can be the last instruction inside block, but block may not be the last block
   * inside function, we try to get following one. */
  MirInstrBlock *owner_block = instr->owner_block;
  if (owner_block && instr == owner_block->last_instr) {
    if (owner_block->base.next == NULL) {
      /* Instruction is last instruction of the function body, so the function can be executed
       * in compile time if needed, we need to set flag with this information here. */
      owner_block->owner_fn->analyzed_for_cmptime_exec = true;
#if BL_DEBUG && VERBOSE_ANALYZE
      printf("Analyze: " BLUE("Function '%s ~%llu' completely analyzed.\n"),
             owner_block->owner_fn->llvm_name,
             (unsigned long long)owner_block->owner_fn->prototype->_serial);
#endif
    }

    /* Return following block. */
    return owner_block->base.next;
  }

  return instr->next;
}

void
analyze(Context *cnt)
{
  if (cnt->verbose_pre) {
    MirInstr *instr;
    BArray *  globals = cnt->module->globals;
    barray_foreach(globals, instr)
    {
      mir_print_instr(instr, stdout);
    }
  }

  BList *   q = cnt->analyze.queue;
  uint64_t  state;
  int       postpone_loop_count = 0;
  MirInstr *ip                  = NULL;
  bool      skip                = false;

  if (bo_list_empty(q)) return;

  while (true) {
    ip = skip ? NULL : analyze_try_get_next(ip);
    if (!ip) {
      if (bo_list_empty(q)) break;

      ip = bo_list_front(q, MirInstr *);
      bo_list_pop_front(q);
      skip = false;
    }

    state = analyze_instr(cnt, ip);
    switch (state) {
    case ANALYZE_PASSED:
#if BL_DEBUG && VERBOSE_ANALYZE
      printf("Analyze: [ " GREEN("PASSED") " ] %16s (%llu)\n", mir_instr_name(ip),
             (unsigned long long)ip->_serial);
#endif
      postpone_loop_count = 0;
      break;

    case ANALYZE_FAILED:
#if BL_DEBUG && VERBOSE_ANALYZE
      printf("Analyze: [ " RED("FAILED") " ] %16s (%llu)\n", mir_instr_name(ip),
             (unsigned long long)ip->_serial);
#endif
      skip                = true;
      postpone_loop_count = 0;
      break;

    case ANALYZE_POSTPONE:
#if BL_DEBUG && VERBOSE_ANALYZE
      printf("Analyze: [" MAGENTA("POSTPONE") "] %16s (%llu)\n", mir_instr_name(ip),
             (unsigned long long)ip->_serial);
#endif

      skip = true;
      if (postpone_loop_count++ < bo_list_size(q)) bo_list_push_back(q, ip);
      break;

    default: {
#if BL_DEBUG && VERBOSE_ANALYZE
      printf("Analyze: [  " YELLOW("WAIT") "  ] %16s (%llu) is waiting for: '%llu'\n",
             mir_instr_name(ip), (unsigned long long)ip->_serial, (unsigned long long)state);
#endif

      BArray *      wq   = NULL;
      bo_iterator_t iter = bo_htbl_find(cnt->analyze.waiting, state);
      bo_iterator_t end  = bo_htbl_end(cnt->analyze.waiting);
      if (bo_iterator_equal(&iter, &end)) {
        wq = bo_array_new(sizeof(MirInstr *));
        bo_array_reserve(wq, 16);
        bo_htbl_insert(cnt->analyze.waiting, state, wq);
      } else {
        wq = bo_htbl_iter_peek_value(cnt->analyze.waiting, &iter, BArray *);
      }

      assert(wq);
      bo_array_push_back(wq, ip);
      skip                = true;
      postpone_loop_count = 0;
    }
    }
  }

  if (cnt->verbose_post) {
    MirInstr *instr;
    BArray *  globals = cnt->module->globals;
    barray_foreach(globals, instr)
    {
      mir_print_instr(instr, stdout);
    }
  }
}

void
analyze_report_unresolved(Context *cnt)
{
  MirInstr *    instr;
  BArray *      wq;
  bo_iterator_t iter;

  bhtbl_foreach(cnt->analyze.waiting, iter)
  {
    wq = bo_htbl_iter_peek_value(cnt->analyze.waiting, &iter, BArray *);
    assert(wq);
    barray_foreach(wq, instr)
    {
      assert(instr);

      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNKNOWN_SYMBOL, instr->node->src,
                  BUILDER_CUR_WORD, "Unknown symbol.");
    }
  }
}

/* executing */
MirStack *
exec_new_stack(size_t size)
{
  if (size == 0) bl_abort("invalid frame stack size");

  MirStack *stack = bl_malloc(sizeof(char) * size);
  if (!stack) bl_abort("bad alloc");
#if BL_DEBUG
  memset(stack, 0, size);
#endif

  stack->allocated_bytes = size;
  exec_reset_stack(stack);
  return stack;
}

void
exec_delete_stack(MirStack *stack)
{
  bl_free(stack);
}

void
exec_reset_stack(MirStack *stack)
{
  stack->pc         = NULL;
  stack->ra         = NULL;
  stack->aborted    = false;
  const size_t size = exec_stack_alloc_size(sizeof(MirStack));
  stack->used_bytes = size;
  stack->top_ptr    = (uint8_t *)stack + size;
}

void
exec_print_call_stack(Context *cnt, size_t max_nesting)
{
  MirInstr *instr = cnt->exec.stack->pc;
  MirFrame *fr    = cnt->exec.stack->ra;
  size_t    n     = 0;

  if (!instr) return;
  /* print last instruction */
  builder_msg(cnt->builder, BUILDER_MSG_LOG, 0, instr->node->src, BUILDER_CUR_WORD, "");

  while (fr) {
    instr = fr->callee;
    fr    = fr->prev;
    if (!instr) break;

    if (max_nesting && n == max_nesting) {
      msg_note("continue...");
      break;
    }

    builder_msg(cnt->builder, BUILDER_MSG_LOG, 0, instr->node->src, BUILDER_CUR_WORD, "");
    ++n;
  }
}

void
exec_instr(Context *cnt, MirInstr *instr)
{
  if (!instr) return;
  if (!instr->analyzed) {
#if BL_DEBUG
    bl_abort("instruction %s (%llu) has not been analyzed!", mir_instr_name(instr), instr->_serial);
#else
    bl_abort("instruction %s has not been analyzed!", mir_instr_name(instr));
#endif
  }
  // if (instr->ref_count == 0) return;

#if 0
  /* step-by-step execution */
  {
    mir_print_instr(instr);
    getchar();
  }
#endif

  switch (instr->kind) {
  case MIR_INSTR_CAST:
    exec_instr_cast(cnt, (MirInstrCast *)instr);
    break;
  case MIR_INSTR_ADDROF:
    exec_instr_addrof(cnt, (MirInstrAddrOf *)instr);
    break;
  case MIR_INSTR_BINOP:
    exec_instr_binop(cnt, (MirInstrBinop *)instr);
    break;
  case MIR_INSTR_UNOP:
    exec_instr_unop(cnt, (MirInstrUnop *)instr);
    break;
  case MIR_INSTR_CALL:
    exec_instr_call(cnt, (MirInstrCall *)instr);
    break;
  case MIR_INSTR_RET:
    exec_instr_ret(cnt, (MirInstrRet *)instr);
    break;
  case MIR_INSTR_TYPE_SLICE:
    exec_instr_type_slice(cnt, (MirInstrTypeSlice *)instr);
    break;
  case MIR_INSTR_DECL_VAR:
    exec_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
    break;
  case MIR_INSTR_DECL_REF:
    exec_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
    break;
  case MIR_INSTR_STORE:
    exec_instr_store(cnt, (MirInstrStore *)instr);
    break;
  case MIR_INSTR_LOAD:
    exec_instr_load(cnt, (MirInstrLoad *)instr);
    break;
  case MIR_INSTR_BR:
    exec_instr_br(cnt, (MirInstrBr *)instr);
    break;
  case MIR_INSTR_COND_BR:
    exec_instr_cond_br(cnt, (MirInstrCondBr *)instr);
    break;
  case MIR_INSTR_UNREACHABLE:
    exec_instr_unreachable(cnt, (MirInstrUnreachable *)instr);
    break;
  case MIR_INSTR_ARG:
    exec_instr_arg(cnt, (MirInstrArg *)instr);
    break;
  case MIR_INSTR_ELEM_PTR:
    exec_instr_elem_ptr(cnt, (MirInstrElemPtr *)instr);
    break;
  case MIR_INSTR_MEMBER_PTR:
    exec_instr_member_ptr(cnt, (MirInstrMemberPtr *)instr);
    break;
  case MIR_INSTR_VARGS:
    exec_instr_vargs(cnt, (MirInstrVArgs *)instr);
    break;
  case MIR_INSTR_TYPE_INFO:
    exec_instr_type_info(cnt, (MirInstrTypeInfo *)instr);
    break;
  case MIR_INSTR_INIT:
    /* noop */
    break;

  default:
    bl_abort("missing execution for instruction: %s", mir_instr_name(instr));
  }
}

void
exec_instr_addrof(Context *cnt, MirInstrAddrOf *addrof)
{
  MirInstr *src  = addrof->src;
  MirType * type = src->const_value.type;
  assert(type);

  if (src->kind == MIR_INSTR_ELEM_PTR) {
    /* address of the element is already on the stack */
    return;
  }

  if (src->kind == MIR_INSTR_DECL_REF) {
    MirStackPtr ptr = exec_fetch_value(cnt, src);
    ptr             = ((MirConstValueData *)ptr)->v_stack_ptr;
    exec_push_stack(cnt, (MirStackPtr)&ptr, type);
  }
}

void
exec_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info)
{
  bl_unimplemented;
}

void
exec_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
  /* pop index from stack */
  assert(mir_is_pointer_type(elem_ptr->arr_ptr->const_value.type));
  MirType *         arr_type   = mir_deref_type(elem_ptr->arr_ptr->const_value.type);
  MirType *         index_type = elem_ptr->index->const_value.type;
  MirStackPtr       index_ptr  = exec_fetch_value(cnt, elem_ptr->index);
  MirConstValueData result     = {0};

  MirStackPtr arr_ptr = exec_fetch_value(cnt, elem_ptr->arr_ptr);
  arr_ptr             = ((MirConstValueData *)arr_ptr)->v_stack_ptr;
  assert(arr_ptr && index_ptr);

  MirConstValueData index = {0};
  exec_read_value(&index, index_ptr, index_type);

  if (elem_ptr->target_is_slice) {
    assert(mir_is_slice_type(arr_type));
    BArray *members = arr_type->data.strct.members;
    assert(members);

    MirType *len_type = bo_array_at(members, 0, MirType *);
    MirType *ptr_type = bo_array_at(members, 1, MirType *);

    MirType *elem_type = mir_deref_type(ptr_type);
    assert(elem_type);

    MirConstValueData ptr_tmp = {0};
    MirConstValueData len_tmp = {0};
    const ptrdiff_t   len_member_offset =
        LLVMOffsetOfElement(cnt->module->llvm_td, arr_type->llvm_type, 0);
    const ptrdiff_t ptr_member_offset =
        LLVMOffsetOfElement(cnt->module->llvm_td, arr_type->llvm_type, 1);

    MirStackPtr ptr_ptr = arr_ptr + ptr_member_offset;
    MirStackPtr len_ptr = arr_ptr + len_member_offset;

    exec_read_value(&ptr_tmp, ptr_ptr, ptr_type);
    exec_read_value(&len_tmp, len_ptr, len_type);

    if (!ptr_tmp.v_stack_ptr) {
      msg_error("Dereferencing null pointer! Slice has not been set?");
      exec_abort(cnt, 0);
    }

    assert(len_tmp.v_u64 > 0);

    if (index.v_u64 >= len_tmp.v_u64) {
      msg_error("Array index is out of the bounds! Array index is: %llu, but array size is: %llu",
                (unsigned long long)index.v_u64, (unsigned long long)len_tmp.v_u64);
      exec_abort(cnt, 0);
    }

    result.v_stack_ptr =
        (MirStackPtr)((ptr_tmp.v_stack_ptr) + (index.v_u64 * elem_type->store_size_bytes));
  } else {
    MirType *elem_type = arr_type->data.array.elem_type;
    assert(elem_type);

    {
      const size_t len = arr_type->data.array.len;
      if (index.v_u64 >= len) {
        msg_error("Array index is out of the bounds! Array index is: %llu, but array size is: %llu",
                  (unsigned long long)index.v_u64, (unsigned long long)len);
        exec_abort(cnt, 0);
      }
    }
    result.v_stack_ptr = (MirStackPtr)((arr_ptr) + (index.v_u64 * elem_type->store_size_bytes));

#if BL_DEBUG
    {
      ptrdiff_t _diff = result.v_u64 - (uintptr_t)arr_ptr;
      assert(_diff / elem_type->store_size_bytes == index.v_u64);
    }
#endif
  }

  /* push result address on the stack */
  exec_push_stack(cnt, &result, elem_ptr->base.const_value.type);
}

void
exec_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
  assert(member_ptr->target_ptr);
  MirType *         target_type = member_ptr->target_ptr->const_value.type;
  MirConstValueData result      = {0};

  /* lookup for base structure declaration type
   * IDEA: maybe we can store parent type to the member type? But what about builtin types???
   */
  assert(target_type->kind == MIR_TYPE_PTR && "expected pointer");
  target_type = mir_deref_type(target_type);
  assert(target_type->kind == MIR_TYPE_STRUCT && "expected structure");

  /* fetch address of the struct begin */
  MirStackPtr ptr = exec_fetch_value(cnt, member_ptr->target_ptr);
  ptr             = ((MirConstValueData *)ptr)->v_stack_ptr;
  assert(ptr);

  LLVMTypeRef llvm_target_type = target_type->llvm_type;
  assert(llvm_target_type && "missing LLVM struct type ref");

  if (member_ptr->builtin_id == MIR_BUILTIN_NONE) {
    assert(member_ptr->scope_entry && member_ptr->scope_entry->kind == SCOPE_ENTRY_MEMBER);
    MirMember *member = member_ptr->scope_entry->data.member;
    assert(member);
    const int64_t index = member->index;

    /* let the llvm solve poiner offest */
    const ptrdiff_t ptr_offset = LLVMOffsetOfElement(cnt->module->llvm_td, llvm_target_type, index);

    result.v_stack_ptr = ptr + ptr_offset; // pointer shift
  } else {
    /* builtin member */
    assert(mir_is_slice_type(target_type));

    if (member_ptr->builtin_id == MIR_BUILTIN_ARR_PTR) {
      /* slice .ptr */
      const ptrdiff_t ptr_offset = LLVMOffsetOfElement(cnt->module->llvm_td, llvm_target_type, 1);
      result.v_stack_ptr         = ptr + ptr_offset; // pointer shift
    } else if (member_ptr->builtin_id == MIR_BUILTIN_ARR_LEN) {
      /* slice .len*/
      const ptrdiff_t len_offset = LLVMOffsetOfElement(cnt->module->llvm_td, llvm_target_type, 0);
      result.v_stack_ptr         = ptr + len_offset; // pointer shift
    } else {
      bl_abort("invalid slice member!");
    }
  }

  /* push result address on the stack */
  exec_push_stack(cnt, &result, member_ptr->base.const_value.type);
}

void
exec_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
  msg_error("execution reached unreachable code");
  exec_abort(cnt, 0);
}

void
exec_instr_br(Context *cnt, MirInstrBr *br)
{
  assert(br->then_block);
  exec_set_pc(cnt, br->then_block->entry_instr);
}

void
exec_instr_cast(Context *cnt, MirInstrCast *cast)
{
  MirType *         src_type  = cast->next->const_value.type;
  MirType *         dest_type = cast->base.const_value.type;
  MirConstValueData tmp       = {0};

  switch (cast->op) {
  case MIR_CAST_BITCAST:
    /* bitcast is always noop */
    break;

  case MIR_CAST_SEXT: {
    /* src is smaller than dest */
    MirStackPtr from_ptr = exec_fetch_value(cnt, cast->next);
    exec_read_value(&tmp, from_ptr, src_type);

#define sext_case(v, T)                                                                            \
  case sizeof(v.T):                                                                                \
    tmp.v_s64 = (int64_t)tmp.T;                                                                    \
    break;

    // clang-format off
    switch (src_type->store_size_bytes)
    {
      sext_case(tmp, v_s8)
      sext_case(tmp, v_s16)
      sext_case(tmp, v_s32)
    default:
      bl_abort("Invalid sext cast!");
    }
    // clang-format on

#undef sext_case

    if (cast->base.comptime)
      memcpy(&cast->base.const_value.data, &tmp, sizeof(tmp));
    else
      exec_push_stack(cnt, (MirStackPtr)&tmp, dest_type);

    break;
  }

  case MIR_CAST_FPTOSI: {
    /* real to signed integer */
    MirStackPtr from_ptr = exec_fetch_value(cnt, cast->next);
    exec_read_value(&tmp, from_ptr, src_type);

    if (src_type->store_size_bytes == sizeof(float))
      tmp.v_s32 = (int32_t)tmp.v_f32;
    else
      tmp.v_s64 = (int64_t)tmp.v_f64;

    if (cast->base.comptime)
      memcpy(&cast->base.const_value.data, &tmp, sizeof(tmp));
    else
      exec_push_stack(cnt, (MirStackPtr)&tmp, dest_type);
    break;
  }

  case MIR_CAST_FPTOUI: {
    /* real to signed integer */
    MirStackPtr from_ptr = exec_fetch_value(cnt, cast->next);
    exec_read_value(&tmp, from_ptr, src_type);

    if (src_type->store_size_bytes == sizeof(float))
      tmp.v_u64 = (uint64_t)tmp.v_f32;
    else
      tmp.v_u64 = (uint64_t)tmp.v_f64;

    if (cast->base.comptime)
      memcpy(&cast->base.const_value.data, &tmp, sizeof(tmp));
    else
      exec_push_stack(cnt, (MirStackPtr)&tmp, dest_type);
    break;
  }

  case MIR_CAST_SITOFP: {
    MirStackPtr from_ptr = exec_fetch_value(cnt, cast->next);
    exec_read_value(&tmp, from_ptr, src_type);

    if (dest_type->store_size_bytes == sizeof(float))
      tmp.v_f32 = (float)tmp.v_s64;
    else
      tmp.v_f64 = (double)tmp.v_s64;

    if (cast->base.comptime)
      memcpy(&cast->base.const_value.data, &tmp, sizeof(tmp));
    else
      exec_push_stack(cnt, (MirStackPtr)&tmp, dest_type);
    break;
  }

  case MIR_CAST_UITOFP: {
    MirStackPtr from_ptr = exec_fetch_value(cnt, cast->next);
    exec_read_value(&tmp, from_ptr, src_type);

    if (dest_type->store_size_bytes == sizeof(float))
      tmp.v_f32 = (float)tmp.v_u64;
    else
      tmp.v_f64 = (double)tmp.v_u64;

    if (cast->base.comptime)
      memcpy(&cast->base.const_value.data, &tmp, sizeof(tmp));
    else
      exec_push_stack(cnt, (MirStackPtr)&tmp, dest_type);
    break;
  }

  case MIR_CAST_INTTOPTR:
  case MIR_CAST_PTRTOINT: {
    /* noop for same sizes */
    const size_t src_size  = src_type->store_size_bytes;
    const size_t dest_size = dest_type->store_size_bytes;

    if (src_size != dest_size) {
      /* trunc or zero extend */
      MirStackPtr from_ptr = exec_fetch_value(cnt, cast->next);
      exec_read_value(&tmp, from_ptr, src_type);

      if (cast->base.comptime)
        memcpy(&cast->base.const_value.data, &tmp, sizeof(tmp));
      else
        exec_push_stack(cnt, (MirStackPtr)&tmp, dest_type);
    }

    break;
  }

  case MIR_CAST_ZEXT:
    /* src is smaller than dest and destination is unsigned, src value will be extended with zeros
     * to dest type size */
  case MIR_CAST_TRUNC: {
    /* src is bigger than dest */
    MirStackPtr from_ptr = exec_fetch_value(cnt, cast->next);
    exec_read_value(&tmp, from_ptr, src_type);

    if (cast->base.comptime)
      memcpy(&cast->base.const_value.data, &tmp, sizeof(tmp));
    else
      exec_push_stack(cnt, (MirStackPtr)&tmp, dest_type);
    break;
  }

  default:
    bl_abort("invalid cast operation");
  }
}

void
exec_instr_arg(Context *cnt, MirInstrArg *arg)
{
  /* arguments must be pushed before RA in reverse order */
  MirFn *fn = arg->base.owner_block->owner_fn;
  assert(fn);

  MirInstrCall *caller     = (MirInstrCall *)exec_get_ra(cnt)->callee;
  BArray *      arg_values = caller->args;
  assert(arg_values);
  MirInstr *curr_arg_value = bo_array_at(arg_values, arg->i, MirInstr *);

  if (curr_arg_value->comptime) {
    MirType *   type = curr_arg_value->const_value.type;
    MirStackPtr dest = exec_push_stack_empty(cnt, type);

    exec_copy_comptime_to_stack(cnt, dest, &curr_arg_value->const_value);
  } else {
    /* resolve argument pointer */
    MirInstr *arg_value = NULL;
    /* starting point */
    MirStackPtr arg_ptr = (MirStackPtr)cnt->exec.stack->ra;
    for (int32_t i = 0; i <= arg->i; ++i) {
      arg_value = bo_array_at(arg_values, i, MirInstr *);
      assert(arg_value);
      if (arg_value->comptime) continue;
      arg_ptr -= exec_stack_alloc_size(arg_value->const_value.type->store_size_bytes);
    }

    exec_push_stack(cnt, (MirStackPtr)arg_ptr, arg->base.const_value.type);
  }
}

void
exec_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
  assert(br->cond);
  MirType *type = br->cond->const_value.type;

  /* pop condition from stack */
  MirStackPtr cond = exec_fetch_value(cnt, br->cond);
  assert(cond);

  MirConstValueData tmp = {0};
  exec_read_value(&tmp, cond, type);

  if (tmp.v_s64) {
    exec_set_pc(cnt, br->then_block->entry_instr);
  } else {
    exec_set_pc(cnt, br->else_block->entry_instr);
  }
}

void
exec_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
  ScopeEntry *entry = ref->scope_entry;
  assert(entry);

  if (ref->base.comptime) return;

  switch (entry->kind) {
  case SCOPE_ENTRY_VAR: {
    MirVar *var = entry->data.var;
    assert(var);

    const bool  use_static_segment = var->is_in_gscope;
    MirStackPtr real_ptr           = NULL;
    if (var->comptime) {
      real_ptr = (MirStackPtr)&var->value->data;
    } else {
      real_ptr = exec_read_stack_ptr(cnt, var->rel_stack_ptr, use_static_segment);
    }

    ref->base.const_value.data.v_stack_ptr = real_ptr;
    break;
  }

  case SCOPE_ENTRY_FN:
  case SCOPE_ENTRY_TYPE:
    break;

  default:
    bl_abort("invalid declaration reference");
  }
}

void
exec_instr_init(Context *cnt, MirStackPtr var_ptr, MirInstrInit *init)
{
  BArray *values = init->values;
  assert(values);

  MirType *init_type = init->base.const_value.type;
  assert(init_type);
  LLVMTypeRef llvm_init_type = init_type->llvm_type;
  assert(llvm_init_type);
  MirStackPtr value_ptr;
  MirInstr *  value;

  switch (init_type->kind) {
  case MIR_TYPE_ARRAY: {
    const size_t elem_size = init_type->data.array.elem_type->store_size_bytes;

    barray_foreach(values, value)
    {
      // CLEANUP:
      if (value->comptime) {
        exec_copy_comptime_to_stack(cnt, var_ptr, &value->const_value);
      } else {
        if (value->kind == MIR_INSTR_INIT) {
          exec_instr_init(cnt, var_ptr, (MirInstrInit *)value);
        } else {
          value_ptr = exec_fetch_value(cnt, value);
          memcpy(var_ptr, value_ptr, elem_size);
        }
      }

      var_ptr += elem_size;
    }
    break;
  }

  case MIR_TYPE_STRUCT: {
    MirType *   member_type;
    MirStackPtr member_ptr;
    barray_foreach(values, value)
    {
      member_type = value->const_value.type;
      member_ptr  = var_ptr + LLVMOffsetOfElement(cnt->module->llvm_td, init_type->llvm_type, i);

      // CLEANUP:
      if (value->comptime) {
        exec_copy_comptime_to_stack(cnt, member_ptr, &value->const_value);
      } else {
        if (value->kind == MIR_INSTR_INIT) {
          exec_instr_init(cnt, member_ptr, (MirInstrInit *)value);
        } else {
          value_ptr = exec_fetch_value(cnt, value);
          memcpy(member_ptr, value_ptr, member_type->store_size_bytes);
        }
      }
    }
    break;
  }

  default:
    bl_unimplemented;
  }
}

void
exec_instr_vargs(Context *cnt, MirInstrVArgs *vargs)
{
  BArray *values    = vargs->values;
  MirVar *arr_tmp   = vargs->arr_tmp;
  MirVar *vargs_tmp = vargs->vargs_tmp;

  assert(mir_is_vargs_type(vargs_tmp->alloc_type));
  assert(vargs_tmp->rel_stack_ptr && "Unalocated vargs slice!!!");
  assert(values);

  MirStackPtr arr_tmp_ptr =
      arr_tmp ? exec_read_stack_ptr(cnt, arr_tmp->rel_stack_ptr, false) : NULL;

  /* Fill vargs tmp array with values from stack or constants. */
  {
    MirInstr *  value;
    MirStackPtr value_ptr;
    barray_foreach(values, value)
    {
      const size_t value_size = value->const_value.type->store_size_bytes;
      MirStackPtr  dest       = arr_tmp_ptr + i * value_size;

      if (value->comptime) {
        exec_copy_comptime_to_stack(cnt, dest, &value->const_value);
      } else {
        value_ptr = exec_fetch_value(cnt, value);
        memcpy(dest, value_ptr, value_size);
      }
    }
  }

  /* Push vargs slice on the stack. */
  {
    MirStackPtr vargs_tmp_ptr = exec_read_stack_ptr(cnt, vargs_tmp->rel_stack_ptr, false);
    // set len
    {
      MirConstValueData len_tmp = {0};
      MirStackPtr       len_ptr =
          vargs_tmp_ptr +
          LLVMOffsetOfElement(cnt->module->llvm_td, vargs_tmp->alloc_type->llvm_type, 0);

      MirType *len_type = bo_array_at(vargs_tmp->alloc_type->data.strct.members, 0, MirType *);
      len_tmp.v_u64     = bo_array_size(values);
      memcpy(len_ptr, &len_tmp, len_type->store_size_bytes);
    }

    // set ptr
    {
      MirConstValueData ptr_tmp = {0};
      MirStackPtr       ptr_ptr =
          vargs_tmp_ptr +
          LLVMOffsetOfElement(cnt->module->llvm_td, vargs_tmp->alloc_type->llvm_type, 1);

      MirType *ptr_type  = bo_array_at(vargs_tmp->alloc_type->data.strct.members, 1, MirType *);
      ptr_tmp.v_void_ptr = arr_tmp_ptr;
      memcpy(ptr_ptr, &ptr_tmp, ptr_type->store_size_bytes);
    }

    exec_push_stack(cnt, vargs_tmp_ptr, vargs_tmp->alloc_type);
  }
}

void
exec_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
  assert(decl->base.const_value.type);

  MirVar *var = decl->var;
  assert(var);

  /* compile time known variables cannot be modified and does not need stack allocated memory,
   * const_value is used instead
   *
   * already allocated variables will never be allocated again (in case declaration is inside loop
   * body!!!)
   */
  if (var->comptime) return;

  const bool use_static_segment = var->is_in_gscope;

  assert(var->rel_stack_ptr);

  /* initialize variable if there is some init value */
  if (decl->init) {
    MirStackPtr var_ptr = exec_read_stack_ptr(cnt, var->rel_stack_ptr, use_static_segment);
    assert(var_ptr);

    // CLEANUP:
    if (decl->init->comptime) {
      /* Compile time constants of agregate type are stored in different
         way, we need to produce decomposition of those data. */
      exec_copy_comptime_to_stack(cnt, var_ptr, &decl->init->const_value);
    } else {
      if (decl->init->kind == MIR_INSTR_INIT) {
        /* used compound initialization!!! */
        exec_instr_init(cnt, var_ptr, (MirInstrInit *)decl->init);
      } else {
        /* read initialization value if there is one */
        MirStackPtr init_ptr = NULL;
        if (decl->init) {
          init_ptr = exec_fetch_value(cnt, decl->init);
        }

        memcpy(var_ptr, init_ptr, var->alloc_type->store_size_bytes);
      }
    }
  }
}

void
exec_instr_load(Context *cnt, MirInstrLoad *load)
{
  /* pop source from stack or load directly when src is declaration, push on to stack dereferenced
   * value of source */
  MirType *src_type  = load->src->const_value.type;
  MirType *dest_type = load->base.const_value.type;
  assert(src_type && dest_type);
  assert(mir_is_pointer_type(src_type));

  MirStackPtr src_ptr = exec_fetch_value(cnt, load->src);
  src_ptr             = ((MirConstValueData *)src_ptr)->v_stack_ptr;

  if (!src_ptr) {
    msg_error("Dereferencing null pointer!");
    exec_abort(cnt, 0);
  }

  if (load->base.comptime) {
    memcpy(&load->base.const_value.data, src_ptr, dest_type->store_size_bytes);
  } else {
    exec_push_stack(cnt, src_ptr, dest_type);
  }
}

void
exec_instr_store(Context *cnt, MirInstrStore *store)
{
  /* loads destination (in case it is not direct reference to declaration) and source from stack
   */
  MirType *src_type  = store->src->const_value.type;
  MirType *dest_type = store->dest->const_value.type;
  assert(src_type && dest_type);
  assert(mir_is_pointer_type(dest_type));

  MirStackPtr dest_ptr = exec_fetch_value(cnt, store->dest);
  MirStackPtr src_ptr  = exec_fetch_value(cnt, store->src);

  dest_ptr = ((MirConstValueData *)dest_ptr)->v_stack_ptr;

  assert(dest_ptr && src_ptr);
  memcpy(dest_ptr, src_ptr, src_type->store_size_bytes);
}

void
exec_instr_type_slice(Context *cnt, MirInstrTypeSlice *type_slice)
{
  /* pop elm type */
  MirType *elem_type = *exec_pop_stack_as(cnt, type_slice->elem_type->const_value.type, MirType **);
  assert(elem_type);

  MirConstValueData tmp = {0};
  bl_unimplemented;
  exec_push_stack(cnt, &tmp, cnt->builtin_types.entry_type);
}

static MirConstValue *
exec_call_top_lvl(Context *cnt, MirInstrCall *call)
{
  assert(call && call->base.analyzed);

  assert(call->callee && call->base.const_value.type);
  MirConstValue *callee_val = &call->callee->const_value;
  assert(callee_val->type && callee_val->type->kind == MIR_TYPE_FN);

  MirFn *fn = callee_val->data.v_fn;
  exec_fn(cnt, fn, call->args, (MirConstValueData *)&call->base.const_value);
  return &call->base.const_value;
}

bool
exec_fn(Context *cnt, MirFn *fn, BArray *args, MirConstValueData *out_value)
{
  assert(fn);
  MirType *ret_type = fn->type->data.fn.ret_type;
  assert(ret_type);
  const bool does_return_value = ret_type->kind != MIR_TYPE_VOID;

  /* push terminal frame on stack */
  exec_push_ra(cnt, NULL);

  /* allocate local variables */
  exec_stack_alloc_vars(cnt, fn);

  /* store return frame pointer */
  fn->exec_ret_value = out_value;

  /* setup entry instruction */
  exec_set_pc(cnt, fn->first_block->entry_instr);

  /* iterate over entry block of executable */
  MirInstr *instr, *prev;
  while (true) {
    instr = exec_get_pc(cnt);
    prev  = instr;
    if (!instr || cnt->exec.stack->aborted) break;

    exec_instr(cnt, instr);

    /* stack head can be changed by br instructions */
    if (exec_get_pc(cnt) == prev) exec_set_pc(cnt, instr->next);
  }

  return does_return_value && !cnt->exec.stack->aborted;
}

static inline void
exec_push_dc_arg(Context *cnt, MirStackPtr val_ptr, MirType *type)
{
  assert(type);

  MirConstValueData tmp = {0};
  exec_read_value(&tmp, val_ptr, type);

  switch (type->kind) {
  case MIR_TYPE_INT: {
    switch (type->store_size_bytes) {
    case sizeof(int64_t):
      dcArgLongLong(cnt->dl.vm, tmp.v_s64);
      break;
    case sizeof(int32_t):
      dcArgInt(cnt->dl.vm, (DCint)tmp.v_s32);
      break;
    case sizeof(int16_t):
      dcArgShort(cnt->dl.vm, (DCshort)tmp.v_s16);
      break;
    case sizeof(int8_t):
      dcArgChar(cnt->dl.vm, (DCchar)tmp.v_s8);
      break;
    default:
      bl_abort("unsupported external call integer argument type");
    }
    break;
  }

  case MIR_TYPE_REAL: {
    switch (type->store_size_bytes) {
    case sizeof(float):
      dcArgFloat(cnt->dl.vm, tmp.v_f32);
      break;
    case sizeof(double):
      dcArgFloat(cnt->dl.vm, tmp.v_f64);
      break;
    default:
      bl_abort("unsupported external call integer argument type");
    }
    break;
  }

  case MIR_TYPE_NULL:
  case MIR_TYPE_PTR: {
    dcArgPointer(cnt->dl.vm, (DCpointer)tmp.v_void_ptr);
    break;
  }

  default:
    bl_abort("unsupported external call argument type");
  }
}

void
exec_instr_call(Context *cnt, MirInstrCall *call)
{
  assert(call->callee && call->base.const_value.type);
  MirConstValue *callee_val = &call->callee->const_value;
  assert(callee_val->type && callee_val->type->kind == MIR_TYPE_FN);

  MirFn *fn = callee_val->data.v_fn;
  assert(fn);

  MirType *ret_type = fn->type->data.fn.ret_type;
  assert(ret_type);

  if (fn->flags & (FLAG_EXTERN)) {
    /* call setup and clenup */
    assert(fn->extern_entry);
    dcMode(cnt->dl.vm, DC_CALL_C_DEFAULT);
    dcReset(cnt->dl.vm);

    /* pop all arguments from the stack */
    MirStackPtr arg_ptr;
    BArray *    arg_values = call->args;
    if (arg_values) {
      MirInstr *arg_value;
      barray_foreach(arg_values, arg_value)
      {
        arg_ptr = exec_fetch_value(cnt, arg_value);
        exec_push_dc_arg(cnt, arg_ptr, arg_value->const_value.type);
      }
    }

    bool does_return = true;

    MirConstValueData result = {0};
    switch (ret_type->kind) {
    case MIR_TYPE_INT:
      switch (ret_type->store_size_bytes) {
      case sizeof(char):
        result.v_s8 = dcCallChar(cnt->dl.vm, fn->extern_entry);
        break;
      case sizeof(short):
        result.v_s16 = dcCallShort(cnt->dl.vm, fn->extern_entry);
        break;
      case sizeof(int):
        result.v_s32 = dcCallInt(cnt->dl.vm, fn->extern_entry);
        break;
      case sizeof(long long):
        result.v_s64 = dcCallLongLong(cnt->dl.vm, fn->extern_entry);
        break;
      default:
        bl_abort("unsupported integer size for external call result");
      }
      break;

    case MIR_TYPE_PTR:
      result.v_void_ptr = dcCallPointer(cnt->dl.vm, fn->extern_entry);
      break;

    case MIR_TYPE_REAL: {
      switch (ret_type->store_size_bytes) {
      case sizeof(float):
        result.v_f32 = dcCallFloat(cnt->dl.vm, fn->extern_entry);
        break;
      case sizeof(double):
        result.v_f64 = dcCallDouble(cnt->dl.vm, fn->extern_entry);
        break;
      default:
        bl_abort("unsupported real number size for external call result");
      }
      break;
    }

    case MIR_TYPE_VOID:
      dcCallVoid(cnt->dl.vm, fn->extern_entry);
      does_return = false;
      break;

    default:
      bl_abort("unsupported external call return type");
    }

    /* PUSH result only if it is used */
    if (call->base.ref_count > 1 && does_return) {
      exec_push_stack(cnt, (MirStackPtr)&result, ret_type);
    }
  } else {
    /* Push current frame stack top. (Later poped by ret instruction)*/
    exec_push_ra(cnt, &call->base);
    assert(fn->first_block->entry_instr);

    exec_stack_alloc_vars(cnt, fn);

    /* setup entry instruction */
    exec_set_pc(cnt, fn->first_block->entry_instr);
  }
}

void
exec_instr_ret(Context *cnt, MirInstrRet *ret)
{
  MirFn *fn = ret->base.owner_block->owner_fn;
  assert(fn);

  /* read callee from frame stack */
  MirInstrCall *caller = (MirInstrCall *)exec_get_ra(cnt)->callee;

  MirType *   ret_type     = NULL;
  MirStackPtr ret_data_ptr = NULL;

  /* pop return value from stack */
  if (ret->value) {
    ret_type = ret->value->const_value.type;
    assert(ret_type);

    ret_data_ptr = exec_fetch_value(cnt, ret->value);
    assert(ret_data_ptr);

    /* TODO: remove */
    /* set fn execution resulting instruction */
    if (fn->exec_ret_value) {
      const size_t size = ret_type->store_size_bytes;
      memcpy(fn->exec_ret_value, ret_data_ptr, size);
    }

    /* discard return value pointer if result is not used on caller side, this solution is kinda
     * messy... */
    if (!(caller && caller->base.ref_count > 1)) ret_data_ptr = NULL;
  }

  /* do frame stack rollback */
  MirInstr *pc = exec_pop_ra(cnt);

  /* clean up all arguments from the stack */
  if (caller) {
    BArray *arg_values = caller->args;
    if (arg_values) {
      MirInstr *arg_value;
      barray_foreach(arg_values, arg_value)
      {
        if (arg_value->comptime) continue;
        exec_pop_stack(cnt, arg_value->const_value.type);
      }
    }
  }

  /* push return value on the stack if there is one */
  if (ret_data_ptr) {
    if (ret->value->comptime) {
      MirStackPtr dest = exec_push_stack_empty(cnt, ret_type);
      exec_copy_comptime_to_stack(cnt, dest, &ret->value->const_value);
    } else {
      exec_push_stack(cnt, ret_data_ptr, ret_type);
    }
  }

  /* set program counter to next instruction */
  pc = pc ? pc->next : NULL;
  exec_set_pc(cnt, pc);
}

void
exec_instr_binop(Context *cnt, MirInstrBinop *binop)
{
// clang-format off
#define _binop_int(_op, _lhs, _rhs, _result, _v_T)                                                 \
  case BINOP_ADD:                                                                                  \
    (_result)._v_T = _lhs._v_T + _rhs._v_T;                                                        \
    break;                                                                                         \
  case BINOP_SUB:                                                                                  \
    (_result)._v_T = _lhs._v_T - _rhs._v_T;                                                        \
    break;                                                                                         \
  case BINOP_MUL:                                                                                  \
    (_result)._v_T = _lhs._v_T * _rhs._v_T;                                                        \
    break;                                                                                         \
  case BINOP_DIV:                                                                                  \
    assert(_rhs._v_T != 0 && "divide by zero, this should be an error");                           \
    (_result)._v_T = _lhs._v_T / _rhs._v_T;                                                        \
    break;                                                                                         \
  case BINOP_EQ:                                                                                   \
    (_result).v_bool = _lhs._v_T == _rhs._v_T;                                                     \
    break;                                                                                         \
  case BINOP_NEQ:                                                                                  \
    (_result).v_bool = _lhs._v_T != _rhs._v_T;                                                     \
    break;                                                                                         \
  case BINOP_LESS:                                                                                 \
    (_result).v_bool = _lhs._v_T < _rhs._v_T;                                                      \
    break;                                                                                         \
  case BINOP_LESS_EQ:                                                                              \
    (_result).v_bool = _lhs._v_T == _rhs._v_T;                                                     \
    break;                                                                                         \
  case BINOP_GREATER:                                                                              \
    (_result).v_bool = _lhs._v_T > _rhs._v_T;                                                      \
    break;                                                                                         \
  case BINOP_GREATER_EQ:                                                                           \
    (_result).v_bool = _lhs._v_T >= _rhs._v_T;                                                     \
    break;                                                                                         \
  case BINOP_LOGIC_AND:                                                                            \
    (_result).v_bool = _lhs._v_T && _rhs._v_T;                                                     \
    break;                                                                                         \
  case BINOP_LOGIC_OR:                                                                             \
    (_result).v_bool = _lhs._v_T || _rhs._v_T;                                                     \
    break;

#define binop_case_int(_op, _lhs, _rhs, _result, _v_T)                                             \
  case sizeof(_lhs._v_T): {                                                                        \
    switch (_op) {                                                                                 \
      _binop_int(_op, _lhs, _rhs, _result, _v_T)                                 		   \
    case BINOP_MOD:                             						   \
      (_result)._v_T = _lhs._v_T % _rhs._v_T;                                                      \
      break;                                                                                       \
    case BINOP_AND:                                                                                \
      (_result).v_bool = _lhs._v_T & _rhs._v_T;                                                    \
      break;                                                                                       \
    case BINOP_OR:                                                                                 \
      (_result).v_bool = _lhs._v_T | _rhs._v_T;                                                    \
      break;                                                                                       \
    default:                                                                                       \
      bl_unimplemented;                                                                            \
    }                                                                                              \
  } break;

#define binop_case_real(_op, _lhs, _rhs, _result, _v_T)                                             \
  case sizeof(_lhs._v_T): {                                                                        \
    switch (_op) {                                                                                 \
      _binop_int(_op, _lhs, _rhs, _result, _v_T)                                 		   \
    default:                                                                                       \
      bl_unimplemented;                                                                            \
    }                                                                                              \
  } break;
  // clang-format on

  /* binop expects lhs and rhs on stack in exact order and push result again to the stack */
  MirType *type = binop->lhs->const_value.type;
  assert(type);
  MirStackPtr lhs_ptr = exec_fetch_value(cnt, binop->lhs);
  MirStackPtr rhs_ptr = exec_fetch_value(cnt, binop->rhs);
  assert(rhs_ptr && lhs_ptr);

  MirConstValueData result = {0};
  MirConstValueData lhs    = {0};
  MirConstValueData rhs    = {0};

  exec_read_value(&lhs, lhs_ptr, type);
  exec_read_value(&rhs, rhs_ptr, type);

  switch (type->kind) {

  case MIR_TYPE_PTR:
  case MIR_TYPE_NULL:
  case MIR_TYPE_BOOL:
  case MIR_TYPE_INT: {
    const size_t s = type->store_size_bytes;
    if (type->data.integer.is_signed) {
      switch (s) {
        binop_case_int(binop->op, lhs, rhs, result, v_s8);
        binop_case_int(binop->op, lhs, rhs, result, v_s16);
        binop_case_int(binop->op, lhs, rhs, result, v_s32);
        binop_case_int(binop->op, lhs, rhs, result, v_s64);
      default:
        bl_abort("invalid integer data type");
      }
    } else {
      switch (s) {
        binop_case_int(binop->op, lhs, rhs, result, v_u8);
        binop_case_int(binop->op, lhs, rhs, result, v_u16);
        binop_case_int(binop->op, lhs, rhs, result, v_u32);
        binop_case_int(binop->op, lhs, rhs, result, v_u64);
      default:
        bl_abort("invalid integer data type");
      }
    }
    break;
  }

  case MIR_TYPE_REAL: {
    const size_t s = type->store_size_bytes;
    switch (s) {
      binop_case_real(binop->op, lhs, rhs, result, v_f32);
      binop_case_real(binop->op, lhs, rhs, result, v_f64);
    default:
      bl_abort("invalid real data type");
    }
    break;
  }

  default:
    bl_abort("invalid binop type");
  }

  if (binop->base.comptime)
    memcpy(&binop->base.const_value.data, &result, sizeof(result));
  else
    exec_push_stack(cnt, &result, binop->base.const_value.type);
#undef binop_case_int
#undef binop_case_real
#undef _binop_int
}

void
exec_instr_unop(Context *cnt, MirInstrUnop *unop)
{
#define unop_case(_op, _value, _result, _v_T)                                                      \
  case sizeof(_value._v_T): {                                                                      \
    switch (_op) {                                                                                 \
    case UNOP_NOT:                                                                                 \
      (_result)._v_T = !_value._v_T;                                                               \
      break;                                                                                       \
    case UNOP_NEG:                                                                                 \
      (_result)._v_T = _value._v_T * -1;                                                           \
      break;                                                                                       \
    case UNOP_POS:                                                                                 \
      (_result)._v_T = _value._v_T;                                                                \
      break;                                                                                       \
    default:                                                                                       \
      bl_unimplemented;                                                                            \
    }                                                                                              \
  } break;

  assert(unop->base.const_value.type);
  MirType *   value_type = unop->instr->const_value.type;
  MirStackPtr value_ptr  = exec_fetch_value(cnt, unop->instr);
  assert(value_ptr);

  MirType *type = unop->instr->const_value.type;
  assert(type);

  MirConstValueData result = {0};
  MirConstValueData value  = {0};
  exec_read_value(&value, value_ptr, type);

  switch (type->kind) {
  case MIR_TYPE_BOOL:
  case MIR_TYPE_INT: {
    const size_t s = type->store_size_bytes;
    if (type->data.integer.is_signed) {
      switch (s) {
        unop_case(unop->op, value, result, v_s8);
        unop_case(unop->op, value, result, v_s16);
        unop_case(unop->op, value, result, v_s32);
        unop_case(unop->op, value, result, v_s64);
      default:
        bl_abort("invalid integer data type");
      }
    } else {
      switch (s) {
        unop_case(unop->op, value, result, v_u8);
        unop_case(unop->op, value, result, v_u16);
        unop_case(unop->op, value, result, v_u32);
        unop_case(unop->op, value, result, v_u64);
      default:
        bl_abort("invalid integer data type");
      }
    }
    break;
  }

  case MIR_TYPE_REAL: {
    const size_t s = type->store_size_bytes;

    switch (s) {
      unop_case(unop->op, value, result, v_f32);
      unop_case(unop->op, value, result, v_f64);
    default:
      bl_abort("invalid real data type");
    }
    break;
  }

  default:
    bl_abort("invalid unop type");
  }

  if (unop->instr->comptime) {
    assert(unop->base.comptime);
    memcpy(&unop->base.const_value.data, &result, sizeof(result));
  } else {
    exec_push_stack(cnt, &result, value_type);
  }
#undef unop
}

/* MIR builting */
void
ast_ublock(Context *cnt, Ast *ublock)
{
  Ast *tmp;
  barray_foreach(ublock->data.ublock.nodes, tmp) ast(cnt, tmp);
}

void
ast_block(Context *cnt, Ast *block)
{
  Ast *tmp;
  barray_foreach(block->data.block.nodes, tmp) ast(cnt, tmp);
}

void
ast_test_case(Context *cnt, Ast *test)
{
  /* build test function */
  Ast *ast_block = test->data.test_case.block;
  assert(ast_block);

  MirInstrFnProto *fn_proto = (MirInstrFnProto *)append_instr_fn_proto(cnt, test, NULL, NULL);

  fn_proto->base.const_value.type = cnt->builtin_types.entry_test_case_fn;

  const char *llvm_name = gen_uq_name(cnt, TEST_CASE_FN_NAME);
  MirFn *     fn        = create_fn(cnt, test, NULL, llvm_name, NULL, FLAG_TEST, fn_proto);

  if (cnt->builder->flags & BUILDER_FORCE_TEST_LLVM) ++fn->ref_count;
  assert(test->data.test_case.desc);
  fn->test_case_desc                   = test->data.test_case.desc;
  fn_proto->base.const_value.data.v_fn = fn;

  bo_array_push_back(cnt->test_cases, fn);

  MirInstrBlock *entry_block = append_block(cnt, fn_proto->base.const_value.data.v_fn, "entry");
  set_current_block(cnt, entry_block);

  /* generate body instructions */
  ast(cnt, ast_block);
}

void
ast_unrecheable(Context *cnt, Ast *unr)
{
  append_instr_unrecheable(cnt, unr);
}

void
ast_stmt_if(Context *cnt, Ast *stmt_if)
{
  Ast *ast_cond = stmt_if->data.stmt_if.test;
  Ast *ast_then = stmt_if->data.stmt_if.true_stmt;
  Ast *ast_else = stmt_if->data.stmt_if.false_stmt;
  assert(ast_cond && ast_then);

  MirFn *fn = get_current_fn(cnt);
  assert(fn);

  MirInstrBlock *tmp_block  = NULL;
  MirInstrBlock *then_block = append_block(cnt, fn, "if_then");
  MirInstrBlock *else_block = append_block(cnt, fn, "if_else");
  MirInstrBlock *cont_block = append_block(cnt, fn, "if_cont");

  MirInstr *cond = ast(cnt, ast_cond);
  append_instr_cond_br(cnt, stmt_if, cond, then_block, else_block);

  /* then block */
  set_current_block(cnt, then_block);
  ast(cnt, ast_then);

  tmp_block = get_current_block(cnt);
  if (!get_block_terminator(tmp_block)) {
    /* block has not been terminated -> add terminator */
    append_instr_br(cnt, NULL, cont_block);
  }

  /* else if */
  if (ast_else) {
    set_current_block(cnt, else_block);
    ast(cnt, ast_else);

    tmp_block = get_current_block(cnt);
    if (!is_block_terminated(tmp_block)) append_instr_br(cnt, NULL, cont_block);
  }

  if (!is_block_terminated(else_block)) {
    /* block has not been terminated -> add terminator */
    set_current_block(cnt, else_block);
    append_instr_br(cnt, NULL, cont_block);
  }

  set_current_block(cnt, cont_block);
}

void
ast_stmt_loop(Context *cnt, Ast *loop)
{
  Ast *ast_block     = loop->data.stmt_loop.block;
  Ast *ast_cond      = loop->data.stmt_loop.condition;
  Ast *ast_increment = loop->data.stmt_loop.increment;
  Ast *ast_init      = loop->data.stmt_loop.init;
  assert(ast_block);

  MirFn *fn = get_current_fn(cnt);
  assert(fn);

  /* prepare all blocks */
  MirInstrBlock *tmp_block       = NULL;
  MirInstrBlock *increment_block = ast_increment ? append_block(cnt, fn, "loop_increment") : NULL;
  MirInstrBlock *decide_block    = append_block(cnt, fn, "loop_decide");
  MirInstrBlock *body_block      = append_block(cnt, fn, "loop_body");
  MirInstrBlock *cont_block      = append_block(cnt, fn, "loop_continue");

  MirInstrBlock *prev_break_block    = cnt->ast.break_block;
  MirInstrBlock *prev_continue_block = cnt->ast.continue_block;
  cnt->ast.break_block               = cont_block;
  cnt->ast.continue_block            = ast_increment ? increment_block : decide_block;

  /* generate initialization if there is one */
  if (ast_init) {
    ast(cnt, ast_init);
  }

  /* decide block */
  append_instr_br(cnt, NULL, decide_block);
  set_current_block(cnt, decide_block);

  MirInstr *cond = ast_cond ? ast(cnt, ast_cond) : append_instr_const_bool(cnt, NULL, true);

  append_instr_cond_br(cnt, ast_cond, cond, body_block, cont_block);

  /* loop body */
  set_current_block(cnt, body_block);
  ast(cnt, ast_block);

  tmp_block = get_current_block(cnt);
  if (!is_block_terminated(tmp_block)) {
    append_instr_br(cnt, NULL, ast_increment ? increment_block : decide_block);
  }

  /* increment if there is one */
  if (ast_increment) {
    set_current_block(cnt, increment_block);
    ast(cnt, ast_increment);
    append_instr_br(cnt, NULL, decide_block);
  }

  cnt->ast.break_block    = prev_break_block;
  cnt->ast.continue_block = prev_continue_block;
  set_current_block(cnt, cont_block);
}

void
ast_stmt_break(Context *cnt, Ast *br)
{
  assert(cnt->ast.break_block && "break statement outside the loop");
  append_instr_br(cnt, br, cnt->ast.break_block);
}

void
ast_stmt_continue(Context *cnt, Ast *cont)
{
  assert(cnt->ast.continue_block && "break statement outside the loop");
  append_instr_br(cnt, cont, cnt->ast.continue_block);
}

void
ast_stmt_return(Context *cnt, Ast *ret)
{
  MirInstr *value = ast(cnt, ret->data.stmt_return.expr);
  append_instr_ret(cnt, ret, value, false);
}

MirInstr *
ast_expr_compound(Context *cnt, Ast *cmp)
{
  BArray *  ast_values = cmp->data.expr_compound.values;
  Ast *     ast_type   = cmp->data.expr_compound.type;
  MirInstr *type       = ast(cnt, ast_type);
  assert(type);

  if (!ast_values) {
    return append_instr_init(cnt, cmp, type, NULL);
  }

  const size_t valc = bo_array_size(ast_values);

  assert(ast_type);

  BArray *values = create_arr(cnt, sizeof(MirInstr *));
  bo_array_resize(values, bo_array_size(ast_values));

  Ast *     ast_value;
  MirInstr *value;

  /* Values must be appended in reverse order. */
  for (size_t i = valc; i-- > 0;) {
    ast_value = bo_array_at(ast_values, i, Ast *);
    value     = ast(cnt, ast_value);
    assert(value);
    bo_array_at(values, i, MirInstr *) = value;
  }

  return append_instr_init(cnt, cmp, type, values);
}

MirInstr *
ast_expr_addrof(Context *cnt, Ast *addrof)
{
  MirInstr *src = ast(cnt, addrof->data.expr_addrof.next);
  assert(src);

  return append_instr_addrof(cnt, addrof, src);
}

MirInstr *
ast_expr_cast(Context *cnt, Ast *cast)
{
  Ast *ast_type = cast->data.expr_cast.type;
  Ast *ast_next = cast->data.expr_cast.next;
  assert(ast_type && ast_next);

  // TODO: const type!!!
  MirInstr *type = ast_create_impl_fn_call(cnt, ast_type, RESOLVE_TYPE_FN_NAME,
                                           cnt->builtin_types.entry_resolve_type_fn);
  MirInstr *next = ast(cnt, ast_next);

  return append_instr_cast(cnt, cast, type, next);
}

MirInstr *
ast_expr_sizeof(Context *cnt, Ast *szof)
{
  Ast *ast_node = szof->data.expr_sizeof.node;
  assert(ast_node);

  MirInstr *expr = ast(cnt, ast_node);
  return append_instr_sizeof(cnt, szof, expr);
}

MirInstr *
ast_expr_type_info(Context *cnt, Ast *type_info)
{
  Ast *ast_node = type_info->data.expr_type_info.node;
  assert(ast_node);

  MirInstr *expr = ast(cnt, ast_node);
  return append_instr_type_info(cnt, type_info, expr);
}

MirInstr *
ast_expr_alignof(Context *cnt, Ast *szof)
{
  Ast *ast_node = szof->data.expr_alignof.node;
  assert(ast_node);

  MirInstr *expr = ast(cnt, ast_node);
  return append_instr_alignof(cnt, szof, expr);
}

MirInstr *
ast_expr_deref(Context *cnt, Ast *deref)
{
  MirInstr *next = ast(cnt, deref->data.expr_deref.next);
  assert(next);
  return append_instr_load(cnt, deref, next);
}

MirInstr *
ast_expr_lit_int(Context *cnt, Ast *expr)
{
  return append_instr_const_int(cnt, expr, expr->data.expr_integer.val);
}

MirInstr *
ast_expr_lit_float(Context *cnt, Ast *expr)
{
  return append_instr_const_float(cnt, expr, expr->data.expr_float.val);
}

MirInstr *
ast_expr_lit_double(Context *cnt, Ast *expr)
{
  return append_instr_const_double(cnt, expr, expr->data.expr_double.val);
}

MirInstr *
ast_expr_lit_bool(Context *cnt, Ast *expr)
{
  return append_instr_const_bool(cnt, expr, expr->data.expr_boolean.val);
}

MirInstr *
ast_expr_lit_char(Context *cnt, Ast *expr)
{
  return append_instr_const_char(cnt, expr, expr->data.expr_character.val);
}

MirInstr *
ast_expr_null(Context *cnt, Ast *nl)
{
  return append_instr_const_null(cnt, nl);
}

MirInstr *
ast_expr_call(Context *cnt, Ast *call)
{
  Ast *   ast_callee = call->data.expr_call.ref;
  BArray *ast_args   = call->data.expr_call.args;
  assert(ast_callee);

  MirInstr *callee = ast(cnt, ast_callee);
  BArray *  args   = create_arr(cnt, sizeof(MirInstr *));

  /* arguments need to be generated into reverse order due to bytecode call conventions */
  if (ast_args) {
    const size_t argc = bo_array_size(ast_args);
    bo_array_resize(args, argc);
    MirInstr *arg;
    Ast *     ast_arg;
    for (size_t i = argc; i-- > 0;) {
      ast_arg = bo_array_at(ast_args, i, Ast *);
      arg     = ast(cnt, ast_arg);

      bo_array_at(args, i, MirInstr *) = arg;
    }
  }

  return append_instr_call(cnt, call, callee, args);
}

MirInstr *
ast_expr_ref(Context *cnt, Ast *ref)
{
  Ast *ident = ref->data.expr_ref.ident;
  assert(ident);
  assert(ident->kind == AST_IDENT);

  return append_instr_decl_ref(cnt, ref, &ident->data.ident.id, ident->data.ident.scope, NULL);
}

MirInstr *
ast_expr_elem(Context *cnt, Ast *elem)
{
  Ast *ast_arr   = elem->data.expr_elem.next;
  Ast *ast_index = elem->data.expr_elem.index;
  assert(ast_arr && ast_index);

  MirInstr *arr_ptr = ast(cnt, ast_arr);
  MirInstr *index   = ast(cnt, ast_index);

  return append_instr_elem_ptr(cnt, elem, arr_ptr, index, false);
}

MirInstr *
ast_expr_member(Context *cnt, Ast *member)
{
  Ast *ast_next = member->data.expr_member.next;
  // assert(ast_next);

  MirInstr *target = ast(cnt, ast_next);
  // assert(target);

  return append_instr_member_ptr(cnt, member, target, member->data.expr_member.ident, NULL,
                                 MIR_BUILTIN_NONE);
}

MirInstr *
ast_expr_lit_fn(Context *cnt, Ast *lit_fn)
{
  /* creates function prototype */
  Ast *ast_block   = lit_fn->data.expr_fn.block;
  Ast *ast_fn_type = lit_fn->data.expr_fn.type;

  MirInstrFnProto *fn_proto = (MirInstrFnProto *)append_instr_fn_proto(cnt, lit_fn, NULL, NULL);

  fn_proto->type = ast_create_impl_fn_call(cnt, ast_fn_type, RESOLVE_TYPE_FN_NAME,
                                           cnt->builtin_types.entry_resolve_type_fn);
  assert(fn_proto->type);

  MirInstrBlock *prev_block = get_current_block(cnt);
  MirFn *        fn =
      create_fn(cnt, lit_fn, NULL, NULL, NULL, 0, fn_proto); /* TODO: based on user flag!!! */
  fn_proto->base.const_value.data.v_fn = fn;

  /* function body */
  /* external functions has no body */
  if (!ast_block) return &fn_proto->base;

  /* create block for initialization locals and arguments */
  MirInstrBlock *init_block = append_block(cnt, fn_proto->base.const_value.data.v_fn, "entry");
  set_current_block(cnt, init_block);

  /* build MIR for fn arguments */
  {
    BArray *ast_args = ast_fn_type->data.type_fn.args;
    if (ast_args) {
      Ast *ast_arg;
      Ast *ast_arg_name;

      const size_t argc = bo_array_size(ast_args);
      for (size_t i = argc; i-- > 0;) {
        ast_arg = bo_array_at(ast_args, i, Ast *);
        assert(ast_arg->kind == AST_DECL_ARG);
        ast_arg_name = ast_arg->data.decl.name;
        assert(ast_arg_name);

        /* create tmp declaration for arg variable */
        MirInstr *arg = append_instr_arg(cnt, NULL, i);
        append_instr_decl_var(cnt, ast_arg_name, NULL, arg, true, false);
      }
    }
  }

  /* generate body instructions */
  ast(cnt, ast_block);

  set_current_block(cnt, prev_block);
  return &fn_proto->base;
}

MirInstr *
ast_expr_lit_string(Context *cnt, Ast *lit_string)
{
  const char *cstr = lit_string->data.expr_string.val;
  assert(cstr);
  return append_instr_const_string(cnt, lit_string, cstr);
}

MirInstr *
ast_expr_binop(Context *cnt, Ast *binop)
{
  Ast *ast_lhs = binop->data.expr_binop.lhs;
  Ast *ast_rhs = binop->data.expr_binop.rhs;
  assert(ast_lhs && ast_rhs);

  const BinopKind op = binop->data.expr_binop.kind;

  if (ast_binop_is_assign(op)) {
    switch (op) {

    case BINOP_ASSIGN: {
      MirInstr *rhs = ast(cnt, ast_rhs);
      MirInstr *lhs = ast(cnt, ast_lhs);

      return append_instr_store(cnt, binop, rhs, lhs);
    }

    case BINOP_ADD_ASSIGN: {
      MirInstr *rhs = ast(cnt, ast_rhs);
      MirInstr *lhs = ast(cnt, ast_lhs);
      MirInstr *tmp = append_instr_binop(cnt, binop, lhs, rhs, BINOP_ADD);
      lhs           = ast(cnt, ast_lhs);

      return append_instr_store(cnt, binop, tmp, lhs);
    }

    case BINOP_SUB_ASSIGN: {
      MirInstr *rhs = ast(cnt, ast_rhs);
      MirInstr *lhs = ast(cnt, ast_lhs);
      MirInstr *tmp = append_instr_binop(cnt, binop, lhs, rhs, BINOP_SUB);
      lhs           = ast(cnt, ast_lhs);

      return append_instr_store(cnt, binop, tmp, lhs);
    }

    case BINOP_MUL_ASSIGN: {
      MirInstr *rhs = ast(cnt, ast_rhs);
      MirInstr *lhs = ast(cnt, ast_lhs);
      MirInstr *tmp = append_instr_binop(cnt, binop, lhs, rhs, BINOP_MUL);
      lhs           = ast(cnt, ast_lhs);

      return append_instr_store(cnt, binop, tmp, lhs);
    }

    case BINOP_DIV_ASSIGN: {
      MirInstr *rhs = ast(cnt, ast_rhs);
      MirInstr *lhs = ast(cnt, ast_lhs);
      MirInstr *tmp = append_instr_binop(cnt, binop, lhs, rhs, BINOP_DIV);
      lhs           = ast(cnt, ast_lhs);

      return append_instr_store(cnt, binop, tmp, lhs);
    }

    case BINOP_MOD_ASSIGN: {
      MirInstr *rhs = ast(cnt, ast_rhs);
      MirInstr *lhs = ast(cnt, ast_lhs);
      MirInstr *tmp = append_instr_binop(cnt, binop, lhs, rhs, BINOP_MOD);
      lhs           = ast(cnt, ast_lhs);

      return append_instr_store(cnt, binop, tmp, lhs);
    }

    default:
      bl_unimplemented;
    }
  } else {
    MirInstr *rhs = ast(cnt, ast_rhs);
    MirInstr *lhs = ast(cnt, ast_lhs);
    return append_instr_binop(cnt, binop, lhs, rhs, op);
  }
}

MirInstr *
ast_expr_unary(Context *cnt, Ast *unop)
{
  Ast *ast_next = unop->data.expr_unary.next;
  assert(ast_next);

  MirInstr *next = ast(cnt, ast_next);
  assert(next);

  return append_instr_unop(cnt, unop, next, unop->data.expr_unary.kind);
}

MirInstr *
ast_expr_type(Context *cnt, Ast *type)
{
  Ast *next_type = type->data.expr_type.type;
  assert(next_type);
  return ast(cnt, next_type);
}

MirInstr *
ast_decl_entity(Context *cnt, Ast *entity)
{
  MirInstr * result       = NULL;
  Ast *      ast_name     = entity->data.decl.name;
  Ast *      ast_type     = entity->data.decl.type;
  Ast *      ast_value    = entity->data.decl_entity.value;
  const bool is_mutable   = entity->data.decl_entity.mutable;
  const bool is_in_gscope = entity->data.decl_entity.in_gscope;

  if (ast_value && ast_value->kind == AST_EXPR_LIT_FN) {
    MirInstr *value = ast(cnt, ast_value);
    value->const_value.data.v_fn->llvm_name =
        is_in_gscope ? ast_name->data.ident.id.str : gen_uq_name(cnt, ast_name->data.ident.id.str);
    value->const_value.data.v_fn->scope     = ast_name->data.ident.scope;
    value->const_value.data.v_fn->id        = &ast_name->data.ident.id;
    value->const_value.data.v_fn->decl_node = ast_name;
    value->const_value.data.v_fn->flags     = entity->data.decl_entity.flags;

    if (ast_type) {
      ((MirInstrFnProto *)value)->user_type = ast_create_impl_fn_call(
          cnt, ast_type, RESOLVE_TYPE_FN_NAME, cnt->builtin_types.entry_resolve_type_fn);
    }

    /* check main */
    if (is_builtin(ast_name, MIR_BUILTIN_MAIN)) {
      assert(!cnt->entry_fn);
      cnt->entry_fn            = value->const_value.data.v_fn;
      cnt->entry_fn->ref_count = 1; /* main must be generated into LLVM */
    }
  } else {
    MirInstr *type = ast_type ? ast_create_impl_fn_call(cnt, ast_type, RESOLVE_TYPE_FN_NAME,
                                                        cnt->builtin_types.entry_resolve_type_fn)
                              : NULL;

    cnt->ast.current_entity_id = &ast_name->data.ident.id;
    /* initialize value */
    MirInstr *value = NULL;
    if (is_in_gscope) {
      /* Initialization of global variables must be done in implicit initializer function executed
       * in compile time. Every initialization function must be able to be executed in compile
       * time. */
      value = ast_value ? ast_create_impl_fn_call(cnt, ast_value, INIT_VALUE_FN_NAME, NULL) : NULL;
    } else {
      value = ast(cnt, ast_value);
    }

    append_instr_decl_var(cnt, ast_name, type, value, is_mutable, is_in_gscope);
    cnt->ast.current_entity_id = NULL;

    if (is_builtin(ast_name, MIR_BUILTIN_MAIN)) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_FUNC, ast_name->src,
                  BUILDER_CUR_WORD, "Main is expected to be a function.");
    }
  }

  return result;
}

MirInstr *
ast_decl_arg(Context *cnt, Ast *arg)
{
  Ast *ast_type = arg->data.decl.type;

  assert(ast_type);
  MirInstr *type = ast(cnt, ast_type);
  return type;
}

MirInstr *
ast_decl_member(Context *cnt, Ast *arg)
{
  Ast *ast_type = arg->data.decl.type;
  Ast *ast_name = arg->data.decl.name;

  assert(ast_type);
  MirInstr *result = ast(cnt, ast_type);

  /* named member? */
  if (ast_name) {
    assert(ast_name->kind == AST_IDENT);
    result = append_instr_decl_member(cnt, ast_name, result);
  }

  assert(result);
  return result;
}

MirInstr *
ast_type_ref(Context *cnt, Ast *type_ref)
{
  Ast *ident = type_ref->data.type_ref.ident;
  assert(ident);

  MirInstr *ref =
      append_instr_decl_ref(cnt, type_ref, &ident->data.ident.id, ident->data.ident.scope, NULL);
  return ref;
}

MirInstr *
ast_type_fn(Context *cnt, Ast *type_fn)
{
  Ast *   ast_ret_type  = type_fn->data.type_fn.ret_type;
  BArray *ast_arg_types = type_fn->data.type_fn.args;

  /* return type */
  MirInstr *ret_type = NULL;
  if (ast_ret_type) {
    ret_type = ast(cnt, ast_ret_type);
    ref_instr(ret_type);
  }

  BArray *arg_types = NULL;
  if (ast_arg_types && bo_array_size(ast_arg_types)) {
    const size_t c = bo_array_size(ast_arg_types);
    arg_types      = create_arr(cnt, sizeof(MirInstr *));
    bo_array_resize(arg_types, c);

    Ast *     ast_arg_type;
    MirInstr *arg_type;
    for (size_t i = c; i-- > 0;) {
      ast_arg_type = bo_array_at(ast_arg_types, i, Ast *);
      arg_type     = ast(cnt, ast_arg_type);
      ref_instr(arg_type);
      bo_array_at(arg_types, i, MirInstr *) = arg_type;
    }
  }

  return append_instr_type_fn(cnt, type_fn, ret_type, arg_types);
}

MirInstr *
ast_type_arr(Context *cnt, Ast *type_arr)
{
  Ast *ast_elem_type = type_arr->data.type_arr.elem_type;
  Ast *ast_len       = type_arr->data.type_arr.len;
  assert(ast_elem_type && ast_len);

  MirInstr *len       = ast(cnt, ast_len);
  MirInstr *elem_type = ast(cnt, ast_elem_type);
  return append_instr_type_array(cnt, type_arr, elem_type, len);
}

MirInstr *
ast_type_slice(Context *cnt, Ast *type_slice)
{
  Ast *ast_elem_type = type_slice->data.type_arr.elem_type;
  assert(ast_elem_type);

  MirInstr *elem_type = ast(cnt, ast_elem_type);
  return append_instr_type_slice(cnt, type_slice, elem_type);
}

MirInstr *
ast_type_ptr(Context *cnt, Ast *type_ptr)
{
  Ast *ast_type = type_ptr->data.type_ptr.type;
  assert(ast_type && "invalid pointee type");
  MirInstr *type = ast(cnt, ast_type);
  assert(type);
  return append_instr_type_ptr(cnt, type_ptr, type);
}

MirInstr *
ast_type_vargs(Context *cnt, Ast *type_vargs)
{
  Ast *ast_type = type_vargs->data.type_vargs.type;
  assert(ast_type);
  MirInstr *type = ast(cnt, ast_type);
  assert(type);
  return append_instr_type_vargs(cnt, type_vargs, type);
}

MirInstr *
ast_type_struct(Context *cnt, Ast *type_struct)
{
  BArray *   ast_members = type_struct->data.type_strct.members;
  const bool is_raw      = type_struct->data.type_strct.raw;
  assert(!is_raw && "TODO");
  assert(ast_members);

  const size_t memc = bo_array_size(ast_members);
  if (memc == 0) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EMPTY_STRUCT, type_struct->src,
                BUILDER_CUR_WORD, "Empty structure.");
    return NULL;
  }

  BArray *members = create_arr(cnt, sizeof(MirInstr *));
  bo_array_reserve(members, memc);

  MirInstr *tmp = NULL;
  Ast *     ast_member;
  barray_foreach(ast_members, ast_member)
  {
    tmp = ast(cnt, ast_member);
    assert(tmp);
    bo_array_push_back(members, tmp);
  }

  Scope *scope = type_struct->data.type_strct.scope;
  assert(scope);

  /* Consume declaration identificator. */
  ID *id                     = cnt->ast.current_entity_id;
  cnt->ast.current_entity_id = NULL;

  return append_instr_type_struct(cnt, type_struct, id, scope, members, false);
}

MirInstr *
ast_create_impl_fn_call(Context *cnt, Ast *node, const char *fn_name, MirType *fn_type)
{
  if (!node) return NULL;

  /* Sometimes we need to have implicit function return type based on resulting type of the AST
   * expression, in such case we must allow return instruction to change function return type and
   * create dummy type for the function. */
  MirType *final_fn_type  = fn_type;
  bool     infer_ret_type = false;
  if (!final_fn_type) {
    final_fn_type  = create_type_fn(cnt, NULL, NULL, false);
    infer_ret_type = true;
  }

  MirInstrBlock *prev_block  = get_current_block(cnt);
  MirInstr *     fn_proto    = append_instr_fn_proto(cnt, NULL, NULL, NULL);
  fn_proto->const_value.type = final_fn_type;
  fn_proto->const_value.data.v_fn =
      create_fn(cnt, NULL, NULL, fn_name, NULL, 0, (MirInstrFnProto *)fn_proto);
  fn_proto->const_value.data.v_fn->type = final_fn_type;

  MirInstrBlock *entry = append_block(cnt, fn_proto->const_value.data.v_fn, "entry");
  set_current_block(cnt, entry);

  MirInstr *result = ast(cnt, node);
  /* Guess return type here when it is based on expression result... */
  append_instr_ret(cnt, NULL, result, infer_ret_type);

  set_current_block(cnt, prev_block);
  return create_instr_call_comptime(cnt, node, fn_proto);
}

MirInstr *
ast(Context *cnt, Ast *node)
{
  if (!node) return NULL;
  switch (node->kind) {
  case AST_UBLOCK:
    ast_ublock(cnt, node);
    break;
  case AST_BLOCK:
    ast_block(cnt, node);
    break;
  case AST_TEST_CASE:
    ast_test_case(cnt, node);
    break;
  case AST_UNREACHABLE:
    ast_unrecheable(cnt, node);
    break;
  case AST_STMT_RETURN:
    ast_stmt_return(cnt, node);
    break;
  case AST_STMT_LOOP:
    ast_stmt_loop(cnt, node);
    break;
  case AST_STMT_BREAK:
    ast_stmt_break(cnt, node);
    break;
  case AST_STMT_CONTINUE:
    ast_stmt_continue(cnt, node);
    break;
  case AST_STMT_IF:
    ast_stmt_if(cnt, node);
    break;
  case AST_DECL_ENTITY:
    return ast_decl_entity(cnt, node);
  case AST_DECL_ARG:
    return ast_decl_arg(cnt, node);
  case AST_DECL_MEMBER:
    return ast_decl_member(cnt, node);
  case AST_TYPE_REF:
    return ast_type_ref(cnt, node);
  case AST_TYPE_STRUCT:
    return ast_type_struct(cnt, node);
  case AST_TYPE_FN:
    return ast_type_fn(cnt, node);
  case AST_TYPE_ARR:
    return ast_type_arr(cnt, node);
  case AST_TYPE_SLICE:
    return ast_type_slice(cnt, node);
  case AST_TYPE_PTR:
    return ast_type_ptr(cnt, node);
  case AST_TYPE_VARGS:
    return ast_type_vargs(cnt, node);
  case AST_EXPR_ADDROF:
    return ast_expr_addrof(cnt, node);
  case AST_EXPR_CAST:
    return ast_expr_cast(cnt, node);
  case AST_EXPR_SIZEOF:
    return ast_expr_sizeof(cnt, node);
  case AST_EXPR_ALIGNOF:
    return ast_expr_alignof(cnt, node);
  case AST_EXPR_DEREF:
    return ast_expr_deref(cnt, node);
  case AST_EXPR_LIT_INT:
    return ast_expr_lit_int(cnt, node);
  case AST_EXPR_LIT_FLOAT:
    return ast_expr_lit_float(cnt, node);
  case AST_EXPR_LIT_DOUBLE:
    return ast_expr_lit_double(cnt, node);
  case AST_EXPR_LIT_BOOL:
    return ast_expr_lit_bool(cnt, node);
  case AST_EXPR_LIT_FN:
    return ast_expr_lit_fn(cnt, node);
  case AST_EXPR_LIT_STRING:
    return ast_expr_lit_string(cnt, node);
  case AST_EXPR_LIT_CHAR:
    return ast_expr_lit_char(cnt, node);
  case AST_EXPR_BINOP:
    return ast_expr_binop(cnt, node);
  case AST_EXPR_UNARY:
    return ast_expr_unary(cnt, node);
  case AST_EXPR_REF:
    return ast_expr_ref(cnt, node);
  case AST_EXPR_CALL:
    return ast_expr_call(cnt, node);
  case AST_EXPR_ELEM:
    return ast_expr_elem(cnt, node);
  case AST_EXPR_NULL:
    return ast_expr_null(cnt, node);
  case AST_EXPR_MEMBER:
    return ast_expr_member(cnt, node);
  case AST_EXPR_TYPE:
    return ast_expr_type(cnt, node);
  case AST_EXPR_COMPOUND:
    return ast_expr_compound(cnt, node);
  case AST_EXPR_TYPE_INFO:
    return ast_expr_type_info(cnt, node);

  case AST_LOAD:
    break;
  default:
    bl_abort("invalid node %s", ast_get_name(node));
  }

  return NULL;
}

const char *
mir_instr_name(MirInstr *instr)
{
  if (!instr) return "unknown";
  switch (instr->kind) {
  case MIR_INSTR_INVALID:
    return "InstrInvalid";
  case MIR_INSTR_BLOCK:
    return "InstrBlock";
  case MIR_INSTR_DECL_VAR:
    return "InstrDeclVar";
  case MIR_INSTR_DECL_MEMBER:
    return "InstrDeclMember";
  case MIR_INSTR_CONST:
    return "InstrConst";
  case MIR_INSTR_LOAD:
    return "InstrLoad";
  case MIR_INSTR_STORE:
    return "InstrStore";
  case MIR_INSTR_BINOP:
    return "InstrBinop";
  case MIR_INSTR_RET:
    return "InstrRet";
  case MIR_INSTR_FN_PROTO:
    return "InstrFnProto";
  case MIR_INSTR_CALL:
    return "InstrCall";
  case MIR_INSTR_DECL_REF:
    return "InstrDeclRef";
  case MIR_INSTR_UNREACHABLE:
    return "InstrUnreachable";
  case MIR_INSTR_TYPE_FN:
    return "InstrTypeFn";
  case MIR_INSTR_TYPE_STRUCT:
    return "InstrTypeStruct";
  case MIR_INSTR_TYPE_ARRAY:
    return "InstrTypeArray";
  case MIR_INSTR_TYPE_SLICE:
    return "InstrTypeSlice";
  case MIR_INSTR_TYPE_VARGS:
    return "InstrTypeVArgs";
  case MIR_INSTR_COND_BR:
    return "InstrCondBr";
  case MIR_INSTR_BR:
    return "InstrBr";
  case MIR_INSTR_UNOP:
    return "InstrUnop";
  case MIR_INSTR_ARG:
    return "InstrArg";
  case MIR_INSTR_ELEM_PTR:
    return "InstrElemPtr";
  case MIR_INSTR_TYPE_PTR:
    return "InstrTypePtr";
  case MIR_INSTR_ADDROF:
    return "InstrAddrOf";
  case MIR_INSTR_MEMBER_PTR:
    return "InstrMemberPtr";
  case MIR_INSTR_CAST:
    return "InstrCast";
  case MIR_INSTR_SIZEOF:
    return "InstrSizeof";
  case MIR_INSTR_ALIGNOF:
    return "InstrAlignof";
  case MIR_INSTR_INIT:
    return "InstrInit";
  case MIR_INSTR_VARGS:
    return "InstrVArgs";
  case MIR_INSTR_TYPE_INFO:
    return "InstrTypeInfo";
  }

  return "UNKNOWN";
}

static void
arenas_init(struct MirArenas *arenas)
{
  arena_init(&arenas->instr_arena, sizeof(union _MirInstr), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->type_arena, sizeof(MirType), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->var_arena, sizeof(MirVar), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->fn_arena, sizeof(MirFn), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->member_arena, sizeof(MirMember), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->value_arena, sizeof(MirConstValue), ARENA_CHUNK_COUNT / 2,
             (ArenaElemDtor)value_dtor);
  arena_init(&arenas->array_arena, sizeof(BArray *), ARENA_CHUNK_COUNT / 2,
             (ArenaElemDtor)array_dtor);
}

static void
arenas_terminate(struct MirArenas *arenas)
{
  arena_terminate(&arenas->instr_arena);
  arena_terminate(&arenas->type_arena);
  arena_terminate(&arenas->var_arena);
  arena_terminate(&arenas->fn_arena);
  arena_terminate(&arenas->member_arena);
  arena_terminate(&arenas->value_arena);
  arena_terminate(&arenas->array_arena);
}

/* public */
static void
_type_to_str(char *buf, int32_t len, MirType *type, bool prefer_name)
{
#define append_buf(buf, len, str)                                                                  \
  {                                                                                                \
    const size_t filled = strlen(buf);                                                             \
    snprintf((buf) + filled, (len)-filled, "%s", str);                                             \
  }
  if (!buf) return;
  if (!type) {
    append_buf(buf, len, "<unknown>");
    return;
  }

  if (type->user_id && prefer_name) {
    append_buf(buf, len, type->user_id->str);
    return;
  }

  switch (type->kind) {
  case MIR_TYPE_TYPE:
    append_buf(buf, len, "type");
    break;

  case MIR_TYPE_STRUCT: {
    BArray * members = type->data.strct.members;
    MirType *tmp;

    if (mir_is_vargs_type(type)) {
      append_buf(buf, len, "...");

      if (members) {
        tmp = bo_array_at(members, 1, MirType *);
        tmp = mir_deref_type(tmp);
        _type_to_str(buf, len, tmp, true);
      }
    } else if (mir_is_slice_type(type)) {
      append_buf(buf, len, "[]");

      if (members) {
        tmp = bo_array_at(members, 1, MirType *);
        tmp = mir_deref_type(tmp);
        _type_to_str(buf, len, tmp, true);
      }
    } else {
      append_buf(buf, len, "struct{");
      if (members) {
        barray_foreach(members, tmp)
        {
          _type_to_str(buf, len, tmp, true);
          if (i < bo_array_size(members) - 1) append_buf(buf, len, ", ");
        }
      }
      append_buf(buf, len, "}");
    }

    break;
  }

  case MIR_TYPE_FN: {
    append_buf(buf, len, "fn(");

    MirType *tmp;
    BArray * args = type->data.fn.arg_types;
    if (args) {
      barray_foreach(args, tmp)
      {
        _type_to_str(buf, len, tmp, true);
        if (i < bo_array_size(args) - 1) append_buf(buf, len, ", ");
      }
    }

    append_buf(buf, len, ") ");

    _type_to_str(buf, len, type->data.fn.ret_type, true);
    break;
  }

  case MIR_TYPE_PTR: {
    append_buf(buf, len, "*");
    _type_to_str(buf, len, mir_deref_type(type), prefer_name);
    break;
  }

  case MIR_TYPE_ARRAY: {
    char str[35];
    sprintf(str, "[%lu]", type->data.array.len);
    append_buf(buf, len, str);

    _type_to_str(buf, len, type->data.array.elem_type, true);
    break;
  }

  default:
    if (type->user_id) {
      append_buf(buf, len, type->user_id->str);
    } else {
      append_buf(buf, len, "<invalid>");
    }
  }
}

void
mir_type_to_str(char *buf, int32_t len, MirType *type, bool prefer_name)
{
  if (!buf || !len) return;
  buf[0] = '\0';
  _type_to_str(buf, len, type, prefer_name);
}

void
execute_entry_fn(Context *cnt)
{
  msg_log("\nexecuting 'main' in compile time...");
  if (!cnt->entry_fn) {
    msg_error("Assembly '%s' has no entry function!", cnt->assembly->name);
    return;
  }

  MirType *fn_type = cnt->entry_fn->type;
  assert(fn_type && fn_type->kind == MIR_TYPE_FN);

  /* TODO: support passing of arguments. */
  if (fn_type->data.fn.arg_types) {
    msg_error("Main function expects arguments, this is not supported yet!");
    return;
  }

  /* tmp return value storage */
  MirConstValueData result = {0};
  if (exec_fn(cnt, cnt->entry_fn, NULL, &result)) {
    int64_t tmp = result.v_s64;
    msg_log("Execution finished with state: %lld\n", (long long)tmp);
  } else {
    msg_log("Execution finished %s\n", cnt->exec.stack->aborted ? "with errors" : "without errors");
  }
}

void
execute_test_cases(Context *cnt)
{
  msg_log("\nExecuting test cases...");

  const size_t c      = bo_array_size(cnt->test_cases);
  int32_t      failed = 0;
  MirFn *      test_fn;
  int32_t      line;
  const char * file;

  barray_foreach(cnt->test_cases, test_fn)
  {
    cnt->exec.stack->aborted = false;
    assert(test_fn->flags & FLAG_TEST);
    exec_fn(cnt, test_fn, NULL, NULL);

    line = test_fn->decl_node ? test_fn->decl_node->src->line : -1;
    file = test_fn->decl_node ? test_fn->decl_node->src->unit->filepath : "?";

    msg_log("[ %s ] (%lu/%lu) %s:%d '%s'",
            cnt->exec.stack->aborted ? RED("FAILED") : GREEN("PASSED"), i + 1, c, file, line,
            test_fn->test_case_desc);

    if (cnt->exec.stack->aborted) ++failed;
  }

  {
    int32_t perc = c > 0 ? (int32_t)((float)(c - failed) / (c * 0.01f)) : 100;

    msg_log("────────────────────────────────────────────────────────────────────────────────");
    if (perc == 100) {
      msg_log("Testing done, %d of %zu failed. Completed: " GREEN("%d%%"), failed, c, perc);
    } else {
      msg_log("Testing done, %d of %zu failed. Completed: " RED("%d%%"), failed, c, perc);
    }
    msg_log("────────────────────────────────────────────────────────────────────────────────");
  }
}

void
init_builtins(Context *cnt)
{
  {
    // initialize all hashes once
    for (int32_t i = 0; i < _MIR_BUILTIN_COUNT; ++i) {
      builtin_ids[i].hash = bo_hash_from_str(builtin_ids[i].str);
    }
  }

  { // TYPES
    struct BuiltinTypes *bt = &cnt->builtin_types;
    bt->entry_type          = create_type_type(cnt);
    bt->entry_void          = create_type_void(cnt);

    bt->entry_s8     = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_TYPE_S8], 8, true);
    bt->entry_s16    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_TYPE_S16], 16, true);
    bt->entry_s32    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_TYPE_S32], 32, true);
    bt->entry_s64    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_TYPE_S64], 64, true);
    bt->entry_u8     = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_TYPE_U8], 8, false);
    bt->entry_u16    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_TYPE_U16], 16, false);
    bt->entry_u32    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_TYPE_U32], 32, false);
    bt->entry_u64    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_TYPE_U64], 64, false);
    bt->entry_usize  = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_TYPE_USIZE], 64, false);
    bt->entry_bool   = create_type_bool(cnt);
    bt->entry_f32    = create_type_real(cnt, &builtin_ids[MIR_BUILTIN_TYPE_F32], 32);
    bt->entry_f64    = create_type_real(cnt, &builtin_ids[MIR_BUILTIN_TYPE_F64], 64);
    bt->entry_u8_ptr = create_type_ptr(cnt, bt->entry_u8);
    bt->entry_string = create_type_string(cnt);

    bt->entry_resolve_type_fn = create_type_fn(cnt, bt->entry_type, NULL, false);
    bt->entry_test_case_fn    = create_type_fn(cnt, bt->entry_void, NULL, false);

    provide_builtin_type(cnt, bt->entry_type);
    provide_builtin_type(cnt, bt->entry_s8);
    provide_builtin_type(cnt, bt->entry_s16);
    provide_builtin_type(cnt, bt->entry_s32);
    provide_builtin_type(cnt, bt->entry_s64);
    provide_builtin_type(cnt, bt->entry_u8);
    provide_builtin_type(cnt, bt->entry_u16);
    provide_builtin_type(cnt, bt->entry_u32);
    provide_builtin_type(cnt, bt->entry_u64);
    provide_builtin_type(cnt, bt->entry_usize);
    provide_builtin_type(cnt, bt->entry_bool);
    provide_builtin_type(cnt, bt->entry_f32);
    provide_builtin_type(cnt, bt->entry_f64);
    provide_builtin_type(cnt, bt->entry_string);
  }

  /* Init type info array */
  {}
}

int
init_dl(Context *cnt)
{
  cnt->dl.libs = bo_array_new(sizeof(DLLib *));

  /* load only current executed workspace */
#ifdef BL_PLATFORM_WIN
  const char *libc = "msvcrt";
#else
  const char *libc = NULL;
#endif
  DLLib *lib = dlLoadLibrary(libc);
  if (!lib) {
    msg_error("unable to load library");
    return ERR_LIB_NOT_FOUND;
  }

  bo_array_push_back(cnt->dl.libs, lib);

/* TEST: */
#ifdef BL_PLATFORM_MACOS
  const char *_sdl       = "libSDL2.dylib";
  const char *_sdl_image = "libSDL2_image.dylib";
#else
  const char *_sdl = "libSDL2.so";
  const char *_sdl_image = "libSDL2_image.so";
#endif

  lib = dlLoadLibrary(_sdl);
  assert(lib);
  bo_array_push_back(cnt->dl.libs, lib);

  lib = dlLoadLibrary(_sdl_image);
  assert(lib);
  bo_array_push_back(cnt->dl.libs, lib);

  DCCallVM *vm = dcNewCallVM(4096);
  dcMode(vm, DC_CALL_C_DEFAULT);
  cnt->dl.vm = vm;
  return NO_ERR;
}

void
terminate_dl(Context *cnt)
{
  dcFree(cnt->dl.vm);

  DLLib *lib;
  barray_foreach(cnt->dl.libs, lib) dlFreeLibrary(lib);

  bo_unref(cnt->dl.libs);
}

MirModule *
mir_new_module(const char *name)
{
  MirModule *tmp = bl_malloc(sizeof(MirModule));
  if (!tmp) bl_abort("bad alloc");

  arenas_init(&tmp->arenas);

  /* init LLVM */
  char *triple    = LLVMGetDefaultTargetTriple();
  char *cpu       = /*LLVMGetHostCPUName()*/ "";
  char *features  = /*LLVMGetHostCPUFeatures()*/ "";
  char *error_msg = NULL;

  msg_log("target: %s", triple);
  /*
  msg_log("cpu: %s", cpu);
  msg_log("features: %s", features);
  */

  LLVMTargetRef llvm_target = NULL;
  if (LLVMGetTargetFromTriple(triple, &llvm_target, &error_msg)) {
    msg_error("cannot get target with error: %s", error_msg);
    LLVMDisposeMessage(error_msg);
    abort();
  }

  /* TODO: set opt level */
#if BL_DEBUG
  LLVMCodeGenOptLevel opt_lvl = LLVMCodeGenLevelNone;
#else
  LLVMCodeGenOptLevel opt_lvl = LLVMCodeGenLevelAggressive;
#endif

  LLVMContextRef llvm_context = LLVMContextCreate();
  LLVMModuleRef  llvm_module  = LLVMModuleCreateWithNameInContext(name, llvm_context);

  LLVMTargetMachineRef llvm_tm = LLVMCreateTargetMachine(
      llvm_target, triple, cpu, features, opt_lvl, LLVMRelocDefault, LLVMCodeModelDefault);

  LLVMTargetDataRef llvm_td = LLVMCreateTargetDataLayout(llvm_tm);
  LLVMSetModuleDataLayout(llvm_module, llvm_td);
  LLVMSetTarget(llvm_module, triple);

  tmp->globals     = bo_array_new(sizeof(MirInstr *));
  tmp->llvm_cnt    = llvm_context;
  tmp->llvm_module = llvm_module;
  tmp->llvm_tm     = llvm_tm;
  tmp->llvm_td     = llvm_td;
  tmp->llvm_triple = triple;
  return tmp;
}

void
mir_delete_module(MirModule *module)
{
  if (!module) return;
  bo_unref(module->globals);

  arenas_terminate(&module->arenas);

  LLVMDisposeModule(module->llvm_module);
  LLVMDisposeTargetMachine(module->llvm_tm);
  LLVMDisposeMessage(module->llvm_triple);

  bl_free(module);
}

void
mir_run(Builder *builder, Assembly *assembly)
{
  Context cnt;
  memset(&cnt, 0, sizeof(Context));
  cnt.builder         = builder;
  cnt.assembly        = assembly;
  cnt.module          = assembly->mir_module;
  cnt.verbose_pre     = (bool)(builder->flags & BUILDER_VERBOSE_MIR_PRE);
  cnt.verbose_post    = (bool)(builder->flags & BUILDER_VERBOSE_MIR_POST);
  cnt.analyze.queue   = bo_list_new(sizeof(MirInstr *));
  cnt.test_cases      = bo_array_new(sizeof(MirFn *));
  cnt.exec.stack      = exec_new_stack(DEFAULT_EXEC_FRAME_STACK_SIZE);
  cnt.tmp_sh          = bo_string_new(1024);
  cnt.type_table      = bo_htbl_new(sizeof(MirType *), 8192);
  cnt.analyze.waiting = bo_htbl_new_bo(bo_typeof(BArray), true, 8192);

  init_builtins(&cnt);

  int32_t error = init_dl(&cnt);
  if (error != NO_ERR) return;

  /* Gen MIR from AST pass */
  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    ast(&cnt, unit->ast);
  }

  /* Analyze pass */
  if (!builder->errorc) {
    analyze(&cnt);
    analyze_report_unresolved(&cnt);
  }

  if (!builder->errorc && builder->flags & BUILDER_RUN_TESTS) execute_test_cases(&cnt);
  if (!builder->errorc && builder->flags & BUILDER_RUN) execute_entry_fn(&cnt);

  bo_unref(cnt.analyze.queue);
  bo_unref(cnt.analyze.waiting);
  bo_unref(cnt.test_cases);
  bo_unref(cnt.tmp_sh);
  bo_unref(cnt.type_table);

  terminate_dl(&cnt);
  exec_delete_stack(cnt.exec.stack);
}
