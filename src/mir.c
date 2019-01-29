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

#include <stdalign.h>
#include "mir.h"
#include "unit.h"
#include "common.h"
#include "builder.h"
#include "assembly.h"
#include "mir_printer.h"

// clang-format off
#define ARENA_CHUNK_COUNT               512
#define TEST_CASE_FN_NAME               "__test"
#define RESOLVE_TYPE_FN_NAME            "__type"
#define IMPL_FN_NAME                    "__impl_"
#define DEFAULT_EXEC_FRAME_STACK_SIZE   2097152 // 2MB
#define DEFAULT_EXEC_CALL_STACK_NESTING 10000
#define MAX_ALIGNMENT                   8

#define VERBOSE_EXEC false
// clang-format on

union _MirInstr
{
  MirInstrBlock        block;
  MirInstrDeclVar      var;
  MirInstrConst        cnst;
  MirInstrLoad         load;
  MirInstrStore        store;
  MirInstrRet          ret;
  MirInstrBinop        binop;
  MirInstrValidateType validate_type;
  MirInstrTypeFn       type_fn;
  MirInstrFnProto      fn_proto;
  MirInstrDeclRef      decl_ref;
  MirInstrCall         call;
  MirInstrUnreachable  unreachable;
  MirInstrTryInfer     try_infer;
  MirInstrCondBr       cond_br;
  MirInstrBr           br;
  MirInstrUnop         unop;
  MirInstrArg          arg;
  MirInstrElemPtr      elem_ptr;
  MirInstrMemberPtr    member_ptr;
  MirInstrAddrOf       addrof;
  MirInstrTypeArray    type_array;
  MirInstrTypePtr      type_ptr;
  MirInstrCast         cast;
};

typedef enum
{
  BUILDIN_TYPE_NONE = -1,

  BUILDIN_TYPE_S8,
  BUILDIN_TYPE_S16,
  BUILDIN_TYPE_S32,
  BUILDIN_TYPE_S64,
  BUILDIN_TYPE_U8,
  BUILDIN_TYPE_U16,
  BUILDIN_TYPE_U32,
  BUILDIN_TYPE_U64,
  BUILDIN_TYPE_USIZE,
  BUILDIN_TYPE_BOOL,
  BUILDIN_TYPE_F32,
  BUILDIN_TYPE_F64,

  _BUILDIN_TYPE_COUNT,
} BuildinType;

typedef enum
{
  BUILDIN_SPEC_MAIN,
  BUILDIN_SPEC_ARR_LEN,

  _BUILDIN_SPEC_COUNT,
} BuildinSpecial;

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
  Builder *  builder;
  Assembly * assembly;
  MirModule *module;
  BArray *   analyze_stack;
  BArray *   test_cases;

  /* stack header is also allocated on the stack :) */
  MirStack *exec_stack;
#if BL_DEBUG
  int64_t (*_debug_stack)[150];
#endif

  /* DynCall/Lib data used for external method execution in compile time */
  struct
  {
    DLLib *   lib;
    DCCallVM *vm;
  } dl;

  struct
  {
    BHashTable *table;

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
    MirType *entry_resolve_type_fn;
    MirType *entry_test_case_fn;
  } buildin_types;

  MirInstrBlock *current_block;
  MirInstrBlock *break_block;
  MirInstrBlock *continue_block;

  MirFn *entry_fn;
  bool   verbose_pre, verbose_post;
} Context;

static const char *buildin_type_names[_BUILDIN_TYPE_COUNT] = {
    "s8", "s16", "s32", "s64", "u8", "u16", "u32", "u64", "usize", "bool", "f32", "f64"};

static const char *buildin_spec_names[_BUILDIN_SPEC_COUNT]  = {"main", "len"};
static uint64_t    buildin_spec_hashes[_BUILDIN_SPEC_COUNT] = {0};

static void
instr_dtor(MirInstr *instr)
{
  switch (instr->kind) {
  case MIR_INSTR_TYPE_FN:
    bo_unref(((MirInstrTypeFn *)instr)->arg_types);
    break;
  case MIR_INSTR_CALL:
    bo_unref(((MirInstrCall *)instr)->args);
    break;
  default:
    break;
  }
}

static void
type_dtor(MirType *type)
{
  switch (type->kind) {
  case MIR_TYPE_FN:
    bo_unref(type->data.fn.arg_types);
    break;
  default:
    break;
  }
}

/* FW decls */
static void
init_buildins(Context *cnt);

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
create_type_void(Context *cnt);

static MirType *
create_type_bool(Context *cnt);

static MirType *
create_type_int(Context *cnt, const char *name, int32_t bitcount, bool is_signed);

static MirType *
create_type_real(Context *cnt, const char *name, int32_t bitcount);

static MirType *
create_type_ptr(Context *cnt, MirType *src_type);

static MirType *
create_type_fn(Context *cnt, MirType *ret_type, BArray *arg_types);

static MirType *
create_type_array(Context *cnt, MirType *elem_type, size_t len);

static MirVar *
create_var(Context *cnt, const char *name, MirType *type);

static MirFn *
create_fn(Context *cnt, Ast *node, const char *name, bool is_external, bool is_test_case);

static MirInstrBlock *
append_block(Context *cnt, MirFn *fn, const char *name);

/* instructions */
static void
push_into_curr_block(Context *cnt, MirInstr *instr);

static void
erase_unused_instr(Context *cnt, MirInstr *instr);

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
create_instr_call_type_resolve(Context *cnt, MirInstr *resolver_fn, Ast *type);

static MirInstr *
create_instr_fn_proto(Context *cnt, Ast *node, MirInstr *type, MirInstr *user_type);

static MirInstr *
append_instr_arg(Context *cnt, Ast *node, unsigned i);

static MirInstr *
append_instr_cast(Context *cnt, Ast *node, MirInstr *type, MirInstr *next);

static MirInstr *
append_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index);

static MirInstr *
append_instr_member_ptr(Context *cnt, Ast *node, MirInstr *target_ptr, Ast *member_ident,
                        int32_t order);

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
append_instr_type_ptr(Context *cnt, Ast *node, MirInstr *type);

static MirInstr *
append_instr_type_array(Context *cnt, Ast *node, MirInstr *elem_type, MirInstr *len);

static MirInstr *
append_instr_try_infer(Context *cnt, Ast *node, MirInstr *expr, MirInstr *dest);

static MirInstr *
append_instr_fn_proto(Context *cnt, Ast *node, MirInstr *type, MirInstr *user_type);

static MirInstr *
append_instr_decl_ref(Context *cnt, Ast *node, ScopeEntry *scope_entry);

static MirInstr *
append_instr_call(Context *cnt, Ast *node, MirInstr *callee, BArray *args);

static MirInstr *
create_instr_decl_var(Context *cnt, MirInstr *type, Ast *name);

static MirInstr *
append_instr_decl_var(Context *cnt, MirInstr *type, Ast *name);

static MirInstr *
append_instr_const_int(Context *cnt, Ast *node, uint64_t val);

static MirInstr *
append_instr_const_float(Context *cnt, Ast *node, float val);

static MirInstr *
append_instr_const_double(Context *cnt, Ast *node, double val);

static MirInstr *
append_instr_const_bool(Context *cnt, Ast *node, bool val);

static MirInstr *
append_instr_const_type(Context *cnt, Ast *node, MirType *type);

static MirInstr *
append_instr_const_string(Context *cnt, Ast *node, const char *str);

static MirInstr *
append_instr_const_null(Context *cnt, Ast *node);

static MirInstr *
append_instr_ret(Context *cnt, Ast *node, MirInstr *value);

static MirInstr *
append_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest);

static MirInstr *
append_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op);

static MirInstr *
append_instr_unop(Context *cnt, Ast *node, MirInstr *instr, UnopKind op);

static MirInstr *
append_instr_validate_type(Context *cnt, MirInstr *src);

static MirInstr *
append_instr_unrecheable(Context *cnt, Ast *node);

/* ast */
static MirInstr *
ast_create_type_resolver_call(Context *cnt, Ast *type);

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
ast_type_ref(Context *cnt, Ast *type_ref);

static MirInstr *
ast_type_fn(Context *cnt, Ast *type_fn);

static MirInstr *
ast_type_arr(Context *cnt, Ast *type_arr);

static MirInstr *
ast_type_ptr(Context *cnt, Ast *type_ptr);

static MirInstr *
ast_expr_addrof(Context *cnt, Ast *addrof);

static MirInstr *
ast_expr_cast(Context *cnt, Ast *cast);

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
ast_expr_binop(Context *cnt, Ast *binop);

static MirInstr *
ast_expr_unary(Context *cnt, Ast *unop);

/* this will also set size and alignment of the type */
static void
init_type_llvm_ABI(Context *cnt, MirType *type);

/* analyze */
static bool
analyze_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static bool
analyze_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr);

static bool
analyze_instr_addrof(Context *cnt, MirInstrAddrOf *addrof);

static bool
analyze_instr_block(Context *cnt, MirInstrBlock *block, bool comptime);

static bool
analyze_instr(Context *cnt, MirInstr *instr, bool comptime);

static bool
analyze_instr_ret(Context *cnt, MirInstrRet *ret);

static bool
analyze_instr_arg(Context *cnt, MirInstrArg *arg);

static bool
analyze_instr_unop(Context *cnt, MirInstrUnop *unop);

static bool
analyze_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static bool
analyze_instr_cond_br(Context *cnt, MirInstrCondBr *br);

static bool
analyze_instr_br(Context *cnt, MirInstrBr *br);

static bool
analyze_instr_load(Context *cnt, MirInstrLoad *load);

static bool
analyze_instr_store(Context *cnt, MirInstrStore *store);

static bool
analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto, bool comptime);

static bool
analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn);

static bool
analyze_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr);

static bool
analyze_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr);

static bool
analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *var);

static bool
analyze_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static bool
analyze_instr_const(Context *cnt, MirInstrConst *cnst);

static bool
analyze_instr_validate_type(Context *cnt, MirInstrValidateType *validate);

static bool
analyze_instr_try_infer(Context *cnt, MirInstrTryInfer *infer);

static bool
analyze_instr_call(Context *cnt, MirInstrCall *call, bool comptime);

static bool
analyze_instr_cast(Context *cnt, MirInstrCast *cast);

static bool
analyze_instr_binop(Context *cnt, MirInstrBinop *binop);

static void
analyze(Context *cnt);

/* execute */
static void
exec_instr(Context *cnt, MirInstr *instr);

static void
exec_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static void
exec_instr_cast(Context *cnt, MirInstrCast *cast);

static void
exec_instr_addrof(Context *cnt, MirInstrAddrOf *addrof);

static void
exec_instr_br(Context *cnt, MirInstrBr *br);

static void
exec_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static void
exec_instr_arg(Context *cnt, MirInstrArg *arg);

static void
exec_instr_cond_br(Context *cnt, MirInstrCondBr *br);

static void
exec_instr_const(Context *cnt, MirInstrConst *cnst);

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
exec_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn);

static void
exec_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr);

static void
exec_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr);

static void
exec_instr_ret(Context *cnt, MirInstrRet *ret);

static void
exec_instr_decl_var(Context *cnt, MirInstrDeclVar *var);

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

/* INLINES */
static inline MirInstr *
mutate_instr(MirInstr *instr, MirInstrKind kind)
{
  assert(instr);
  instr_dtor(instr);
  instr->kind = kind;
  return instr;
}

static inline void
erase_instr(MirInstr *instr)
{
  assert(instr);
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

static inline bool
is_pointer_type(MirType *type)
{
  assert(type);
  return type->kind == MIR_TYPE_PTR;
}

static inline MirStackPtr
exec_read_stack_ptr(Context *cnt, MirRelativeStackPtr rel_ptr)
{
  assert(rel_ptr);

  MirStackPtr base = (MirStackPtr)cnt->exec_stack->ra;
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
  // TODO
  exec_print_call_stack(cnt, report_stack_nesting);
  cnt->exec_stack->aborted = true;
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
  cnt->exec_stack->used_bytes += size;
  if (cnt->exec_stack->used_bytes > cnt->exec_stack->allocated_bytes) {
    msg_error("Stack overflow!!!");
    exec_abort(cnt, 10);
  }

  MirStackPtr mem          = (MirStackPtr)cnt->exec_stack->top_ptr;
  cnt->exec_stack->top_ptr = cnt->exec_stack->top_ptr + size;

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
  MirStackPtr new_top = cnt->exec_stack->top_ptr - size;
  if (new_top < (uint8_t *)(cnt->exec_stack->ra + 1)) bl_abort("Stack underflow!!!");
  cnt->exec_stack->top_ptr = new_top;
  cnt->exec_stack->used_bytes -= size;
  // align_ptr_up((void **)&new_top, MAX_ALIGNMENT, NULL);
  return new_top;
}

static inline void
exec_push_ra(Context *cnt, MirInstr *instr)
{
  MirFrame *prev      = cnt->exec_stack->ra;
  MirFrame *tmp       = (MirFrame *)exec_stack_alloc(cnt, sizeof(MirFrame));
  tmp->callee         = instr;
  tmp->prev           = prev;
  cnt->exec_stack->ra = tmp;

#if BL_DEBUG && VERBOSE_EXEC
  {
    if (instr) {
      fprintf(stdout, "%6llu %20s  PUSH RA\n", (unsigned long long)cnt->exec_stack->pc->_serial,
              mir_instr_name(cnt->exec_stack->pc));
    } else {
      fprintf(stdout, "     - %20s  PUSH RA\n", "Terminal");
    }
  }
#endif
}

static inline MirInstr *
exec_pop_ra(Context *cnt)
{
  if (!cnt->exec_stack->ra) return NULL;
  MirInstr *callee = cnt->exec_stack->ra->callee;

#if BL_DEBUG && VERBOSE_EXEC
  {
    fprintf(stdout, "%6llu %20s  POP RA\n", (unsigned long long)cnt->exec_stack->pc->_serial,
            mir_instr_name(cnt->exec_stack->pc));
  }
#endif

  /* rollback */
  MirStackPtr new_top_ptr     = (MirStackPtr)cnt->exec_stack->ra;
  cnt->exec_stack->used_bytes = cnt->exec_stack->top_ptr - new_top_ptr;
  cnt->exec_stack->top_ptr    = new_top_ptr;
  cnt->exec_stack->ra         = cnt->exec_stack->ra->prev;
  return callee;
}

static inline MirRelativeStackPtr
exec_push_stack(Context *cnt, void *value, MirType *type)
{
  assert(type);
  const size_t size = type->store_size_bytes;
  assert(size && "pushing zero sized data on stack");
  MirStackPtr tmp = exec_stack_alloc(cnt, size);
#if BL_DEBUG && VERBOSE_EXEC
  {
    char type_name[256];
    mir_type_to_str(type_name, 256, type);
    fprintf(stdout, "%6llu %20s  PUSH    (%luB, %p) %s\n",
            (unsigned long long)cnt->exec_stack->pc->_serial, mir_instr_name(cnt->exec_stack->pc),
            size, tmp, type_name);
  }
#endif

  /* copy data when there is some */
  if (value) memcpy(tmp, value, size);
  /* pointer relative to frame top */
  return tmp - (MirStackPtr)cnt->exec_stack->ra;
}

static inline MirStackPtr
exec_pop_stack(Context *cnt, MirType *type)
{
  assert(type);
  const size_t size = type->store_size_bytes;
  assert(size && "popping zero sized data on stack");
#if BL_DEBUG && VERBOSE_EXEC
  {
    char type_name[256];
    mir_type_to_str(type_name, 256, type);
    fprintf(stdout, "%6llu %20s  POP     (%luB, %p) %s\n",
            (unsigned long long)cnt->exec_stack->pc->_serial, mir_instr_name(cnt->exec_stack->pc),
            size, cnt->exec_stack->top_ptr - size, type_name);
  }
#endif

  return exec_stack_free(cnt, size);
}

#define exec_pop_stack_as(cnt, type, T) ((T)exec_pop_stack((cnt), (type)))

static inline MirInstr *
exec_get_pc(Context *cnt)
{
  return cnt->exec_stack->pc;
}

static inline MirFrame *
exec_get_ra(Context *cnt)
{
  return cnt->exec_stack->ra;
}

static inline void
exec_set_pc(Context *cnt, MirInstr *instr)
{
  cnt->exec_stack->pc = instr;
}

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
is_buildin_spec(Ast *ident, BuildinSpecial kind)
{
  if (!ident) return false;
  assert(ident->kind == AST_IDENT);
  return ident->data.ident.hash == buildin_spec_hashes[kind];
}

static inline BuildinType
is_buildin_type(Context *cnt, const uint64_t hash)
{
  if (!bo_htbl_has_key(cnt->buildin_types.table, hash)) return BUILDIN_TYPE_NONE;
  return bo_htbl_at(cnt->buildin_types.table, hash, BuildinType);
}

static inline bool
get_block_terminator(MirInstrBlock *block)
{
  return block->terminal;
}

static inline MirType *
get_buildin(Context *cnt, BuildinType id)
{
  switch (id) {
  case BUILDIN_TYPE_S8:
    return cnt->buildin_types.entry_s8;
  case BUILDIN_TYPE_S16:
    return cnt->buildin_types.entry_s16;
  case BUILDIN_TYPE_S32:
    return cnt->buildin_types.entry_s32;
  case BUILDIN_TYPE_S64:
    return cnt->buildin_types.entry_s64;
  case BUILDIN_TYPE_U8:
    return cnt->buildin_types.entry_u8;
  case BUILDIN_TYPE_U16:
    return cnt->buildin_types.entry_u16;
  case BUILDIN_TYPE_U32:
    return cnt->buildin_types.entry_u32;
  case BUILDIN_TYPE_U64:
    return cnt->buildin_types.entry_u64;
  case BUILDIN_TYPE_F32:
    return cnt->buildin_types.entry_f32;
  case BUILDIN_TYPE_F64:
    return cnt->buildin_types.entry_f64;
  case BUILDIN_TYPE_USIZE:
    return cnt->buildin_types.entry_usize;
  case BUILDIN_TYPE_BOOL:
    return cnt->buildin_types.entry_bool;
  default:
    bl_abort("invalid buildin type");
  }
}

static inline void
set_cursor_block(Context *cnt, MirInstrBlock *block)
{
  if (!block) return;
  cnt->current_block = block;
}

static inline MirInstrBlock *
get_current_block(Context *cnt)
{
  return cnt->current_block;
}

static inline MirFn *
get_current_fn(Context *cnt)
{
  return cnt->current_block ? cnt->current_block->owner_fn : NULL;
}

static inline void
error_types(Context *cnt, MirType *from, MirType *to, Ast *loc, const char *msg)
{
  assert(from && to);
  if (!msg) msg = "no implicit cast for type '%s' and '%s'";

  char tmp_from[256];
  char tmp_to[256];
  mir_type_to_str(tmp_from, 256, from);
  mir_type_to_str(tmp_to, 256, to);

  builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, loc->src, BUILDER_CUR_WORD, msg,
              tmp_from, tmp_to);
}

static inline ScopeEntry *
provide(Context *cnt, Ast *ident, MirInstr *instr, bool in_tree)
{
  assert(ident);
  assert(ident->kind == AST_IDENT);

  Scope *scope = ident->data.ident.scope;
  assert(scope);

  const uint64_t key       = ident->data.ident.hash;
  ScopeEntry *   collision = scope_lookup(scope, key, in_tree);
  if (collision) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_DUPLICATE_SYMBOL, ident->src, BUILDER_CUR_WORD,
                "symbol with same name is already declared");

    builder_msg(cnt->builder, BUILDER_MSG_NOTE, 0, collision->node->src, BUILDER_CUR_WORD,
                "previous declaration found here");
    return NULL;
  }

  ScopeEntry *entry = scope_create_entry(&cnt->builder->scope_arenas, ident, instr);
  scope_insert(scope, key, entry);
  return entry;
}

static inline ScopeEntry *
provide_into_existing_scope_entry(Context *cnt, Ast *ident, MirInstr *instr)
{
  /* Prepare scope entry created in parser */
  assert(ident->kind == AST_IDENT);
  Scope *scope = ident->data.ident.scope;
  assert(scope);
  ScopeEntry *scope_entry = scope_lookup(scope, ident->data.ident.hash, false);
  assert(scope_entry && "declaration has no scope entry");
  scope_entry->instr = instr;
  return scope_entry;
}

static inline void
unref_instr(MirInstr *instr)
{
  if (!instr) return;
  --instr->ref_count;
}

static inline void
ref_instr(MirInstr *instr)
{
  if (!instr) return;
  ++instr->ref_count;
}

static inline bool
is_ident_in_gscope(Ast *ident)
{
  assert(ident->kind == AST_IDENT);
  Scope *scope = ident->data.ident.scope;
  assert(scope);
  return scope->is_global;
}

static inline bool
is_load_needed(MirInstr *instr)
{
  assert(instr);
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
  case MIR_INSTR_CAST:
    return false;
  default:
    break;
  }

  return true;
}

static inline void
setup_null_type_if_needed(MirType *dest, MirType *src)
{
  if (dest->kind == MIR_TYPE_NULL) {
    dest->llvm_type = src->llvm_type;
    assert(dest->llvm_type);
  }
}

/* impl */
MirType *
create_type_type(Context *cnt)
{
  MirType *tmp = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind    = MIR_TYPE_TYPE;
  tmp->name    = "type";
  init_type_llvm_ABI(cnt, tmp);
  return tmp;
}

MirType *
create_type_null(Context *cnt)
{
  MirType *tmp = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind    = MIR_TYPE_NULL;
  tmp->name    = "null";
  /* default type of null in llvm (can be overriden later) */
  init_type_llvm_ABI(cnt, tmp);
  return tmp;
}

MirType *
create_type_void(Context *cnt)
{
  MirType *tmp = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind    = MIR_TYPE_VOID;
  tmp->name    = "void";
  init_type_llvm_ABI(cnt, tmp);
  return tmp;
}

MirType *
create_type_bool(Context *cnt)
{
  MirType *tmp = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind    = MIR_TYPE_BOOL;
  tmp->name    = "bool";
  init_type_llvm_ABI(cnt, tmp);
  return tmp;
}

MirType *
create_type_int(Context *cnt, const char *name, int32_t bitcount, bool is_signed)
{
  assert(bitcount > 0);
  MirType *tmp                = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind                   = MIR_TYPE_INT;
  tmp->name                   = name;
  tmp->data.integer.bitcount  = bitcount;
  tmp->data.integer.is_signed = is_signed;
  init_type_llvm_ABI(cnt, tmp);
  return tmp;
}

MirType *
create_type_real(Context *cnt, const char *name, int32_t bitcount)
{
  assert(bitcount > 0);
  MirType *tmp            = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind               = MIR_TYPE_REAL;
  tmp->name               = name;
  tmp->data.real.bitcount = bitcount;
  init_type_llvm_ABI(cnt, tmp);
  return tmp;
}

MirType *
create_type_ptr(Context *cnt, MirType *src_type)
{
  MirType *tmp       = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind          = MIR_TYPE_PTR;
  tmp->data.ptr.next = src_type;
  init_type_llvm_ABI(cnt, tmp);

  return tmp;
}

MirType *
create_type_fn(Context *cnt, MirType *ret_type, BArray *arg_types)
{
  MirType *tmp           = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind              = MIR_TYPE_FN;
  tmp->data.fn.arg_types = arg_types;
  tmp->data.fn.ret_type  = ret_type ? ret_type : cnt->buildin_types.entry_void;
  init_type_llvm_ABI(cnt, tmp);

  return tmp;
}

MirType *
create_type_array(Context *cnt, MirType *elem_type, size_t len)
{
  MirType *tmp              = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind                 = MIR_TYPE_ARRAY;
  tmp->data.array.elem_type = elem_type;
  tmp->data.array.len       = len;
  init_type_llvm_ABI(cnt, tmp);

  return tmp;
}

MirVar *
create_var(Context *cnt, const char *name, MirType *type)
{
  assert(name);
  MirVar *tmp     = arena_alloc(&cnt->module->arenas.var_arena);
  tmp->name       = name;
  tmp->alloc_type = type;
  return tmp;
}

MirFn *
create_fn(Context *cnt, Ast *node, const char *name, bool is_external, bool is_test_case)
{
  MirFn *tmp        = arena_alloc(&cnt->module->arenas.fn_arena);
  tmp->name         = name;
  tmp->is_external  = is_external;
  tmp->is_test_case = is_test_case;
  tmp->node         = node;
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

void
erase_unused_instr(Context *cnt, MirInstr *instr)
{
  if (!instr) return;
  if (instr->ref_count > 0) return;

  erase_instr(instr);

  switch (instr->kind) {
  case MIR_INSTR_VALIDATE_TYPE: {
    MirInstrValidateType *tmp = (MirInstrValidateType *)instr;
    unref_instr(tmp->src);
    erase_unused_instr(cnt, tmp->src);
    break;
  }

  case MIR_INSTR_ELEM_PTR: {
    MirInstrElemPtr *tmp = (MirInstrElemPtr *)instr;
    unref_instr(tmp->arr_ptr);
    unref_instr(tmp->index);
    erase_unused_instr(cnt, tmp->arr_ptr);
    erase_unused_instr(cnt, tmp->index);
    break;
  }

  case MIR_INSTR_CALL: {
    MirInstrCall *tmp = (MirInstrCall *)instr;
    unref_instr(tmp->callee);
    break;
  }

  case MIR_INSTR_TRY_INFER: {
    MirInstrTryInfer *tmp = (MirInstrTryInfer *)instr;
    unref_instr(tmp->dest);
    unref_instr(tmp->src);
    erase_unused_instr(cnt, tmp->dest);
    erase_unused_instr(cnt, tmp->src);
    break;
  }

  case MIR_INSTR_UNOP: {
    MirInstrUnop *tmp = (MirInstrUnop *)instr;
    unref_instr(tmp->instr);
    erase_unused_instr(cnt, tmp->instr);
    break;
  }

  case MIR_INSTR_BINOP: {
    MirInstrBinop *tmp = (MirInstrBinop *)instr;
    unref_instr(tmp->lhs);
    unref_instr(tmp->rhs);
    erase_unused_instr(cnt, tmp->lhs);
    erase_unused_instr(cnt, tmp->rhs);
    break;
  }

  case MIR_INSTR_LOAD: {
    MirInstrLoad *tmp = (MirInstrLoad *)instr;
    unref_instr(tmp->src);
    erase_unused_instr(cnt, tmp->src);
    break;
  }

  case MIR_INSTR_DECL_REF: {
    MirInstrDeclRef *tmp = (MirInstrDeclRef *)instr;
    unref_instr(tmp->scope_entry->instr);
    erase_unused_instr(cnt, tmp->scope_entry->instr);
    break;
  }

  case MIR_INSTR_DECL_VAR:
  case MIR_INSTR_CONST:
    break;
  default:
    bl_abort("cannot erase %s (%s:%d)", mir_instr_name(instr),
             instr->node ? instr->node->src->unit->filepath : "",
             instr->node ? instr->node->src->line : 0);
  }
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
  ref_instr(src);
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

    default:
      bl_abort("invalid to cast type");
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

    default:
      bl_abort("invalid to cast type");
    }
    break;
  }

  default:
    bl_abort("invalid from cast type");
  }

  return MIR_CAST_INVALID;
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
  if (src_type->kind == MIR_TYPE_INT && expected_type->kind == MIR_TYPE_INT) {
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
    cast->base.analyzed         = true;
    cast->next                  = src;
    cast->op                    = get_cast_op(src_type, expected_type);

    ref_instr(&cast->base);
    insert_instr_after(src, &cast->base);
    return &cast->base;
  }

  error_types(cnt, src->const_value.type, expected_type, src->node, NULL);
  *valid = false;
  return src;
}

MirInstr *
_create_instr(Context *cnt, MirInstrKind kind, Ast *node)
{
  MirInstr *tmp         = arena_alloc(&cnt->module->arenas.instr_arena);
  tmp->kind             = kind;
  tmp->node             = node;
  tmp->id               = 0;
  tmp->const_value.kind = MIR_CV_BASIC; /* can be overriden later */

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
create_instr_call_type_resolve(Context *cnt, MirInstr *resolver_fn, Ast *type)
{
  assert(resolver_fn && resolver_fn->kind == MIR_INSTR_FN_PROTO);
  MirInstrCall *tmp  = create_instr(cnt, MIR_INSTR_CALL, type, MirInstrCall *);
  tmp->base.id       = 0;
  tmp->base.comptime = true;
  tmp->callee        = resolver_fn;
  ref_instr(resolver_fn);
  return &tmp->base;
}

MirInstr *
append_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, BArray *arg_types)
{
  MirInstrTypeFn *tmp        = create_instr(cnt, MIR_INSTR_TYPE_FN, node, MirInstrTypeFn *);
  tmp->base.const_value.type = cnt->buildin_types.entry_type;
  tmp->base.comptime         = true;
  tmp->ret_type              = ret_type;
  tmp->arg_types             = arg_types;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_type_ptr(Context *cnt, Ast *node, MirInstr *type)
{
  MirInstrTypePtr *tmp       = create_instr(cnt, MIR_INSTR_TYPE_PTR, node, MirInstrTypePtr *);
  tmp->base.const_value.type = cnt->buildin_types.entry_type;
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
  tmp->base.const_value.type = cnt->buildin_types.entry_type;
  tmp->base.comptime         = true;
  tmp->elem_type             = elem_type;
  tmp->len                   = len;

  ref_instr(elem_type);
  ref_instr(len);
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
append_instr_cond_br(Context *cnt, Ast *node, MirInstr *cond, MirInstrBlock *then_block,
                     MirInstrBlock *else_block)
{
  assert(cond && then_block && else_block);
  ref_instr(cond);
  ref_instr(&then_block->base);
  ref_instr(&else_block->base);
  MirInstrCondBr *tmp        = create_instr(cnt, MIR_INSTR_COND_BR, node, MirInstrCondBr *);
  tmp->base.const_value.type = cnt->buildin_types.entry_void;
  tmp->cond                  = cond;
  tmp->then_block            = then_block;
  tmp->else_block            = else_block;

  MirInstrBlock *block = get_current_block(cnt);
  terminate_block(block, &tmp->base);

  push_into_curr_block(cnt, &tmp->base);
  ref_instr(&tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_br(Context *cnt, Ast *node, MirInstrBlock *then_block)
{
  assert(then_block);
  ref_instr(&then_block->base);
  MirInstrBr *tmp            = create_instr(cnt, MIR_INSTR_BR, node, MirInstrBr *);
  tmp->base.const_value.type = cnt->buildin_types.entry_void;
  tmp->then_block            = then_block;
  ref_instr(&tmp->base);

  MirInstrBlock *block = get_current_block(cnt);
  terminate_block(block, &tmp->base);

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index)
{
  assert(arr_ptr && index);
  ref_instr(arr_ptr);
  ref_instr(index);
  MirInstrElemPtr *tmp = create_instr(cnt, MIR_INSTR_ELEM_PTR, node, MirInstrElemPtr *);
  tmp->arr_ptr         = arr_ptr;
  tmp->index           = index;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_member_ptr(Context *cnt, Ast *node, MirInstr *target_ptr, Ast *member_ident,
                        int32_t order)
{
  assert(target_ptr && member_ident);
  ref_instr(target_ptr);
  MirInstrMemberPtr *tmp = create_instr(cnt, MIR_INSTR_MEMBER_PTR, node, MirInstrMemberPtr *);
  tmp->target_ptr        = target_ptr;
  tmp->member_ident      = member_ident;
  tmp->order             = order;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
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
  tmp->base.const_value.type = cnt->buildin_types.entry_void;
  push_into_curr_block(cnt, &tmp->base);
  ref_instr(&tmp->base);
  return &tmp->base;
}

MirInstr *
create_instr_fn_proto(Context *cnt, Ast *node, MirInstr *type, MirInstr *user_type)
{
  MirInstrFnProto *tmp = create_instr(cnt, MIR_INSTR_FN_PROTO, node, MirInstrFnProto *);
  tmp->base.id         = (int)bo_array_size(cnt->analyze_stack);
  tmp->type            = type;
  tmp->user_type       = user_type;
  return &tmp->base;
}

MirInstr *
append_instr_fn_proto(Context *cnt, Ast *node, MirInstr *type, MirInstr *user_type)
{
  MirInstr *tmp = create_instr_fn_proto(cnt, node, type, user_type);
  bo_array_push_back(cnt->analyze_stack, tmp);
  return tmp;
}

MirInstr *
append_instr_decl_ref(Context *cnt, Ast *node, ScopeEntry *scope_entry)
{
  MirInstrDeclRef *tmp = create_instr(cnt, MIR_INSTR_DECL_REF, node, MirInstrDeclRef *);
  tmp->scope_entry     = scope_entry;

  if (!scope_entry->parent_scope->is_global) {
    ref_instr(scope_entry->instr);
  }

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
  ref_instr(callee);

  /* every call must have ref_count = 1 at least */
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
create_instr_decl_var(Context *cnt, MirInstr *type, Ast *name)
{
  if (type) ref_instr(type);
  MirInstrDeclVar *tmp = create_instr(cnt, MIR_INSTR_DECL_VAR, name, MirInstrDeclVar *);
  tmp->type            = type;
  tmp->var             = create_var(cnt, name->data.ident.str, NULL); /* type is filled later */
  return &tmp->base;
}

MirInstr *
append_instr_decl_var(Context *cnt, MirInstr *type, Ast *name)
{
  MirInstr *tmp = create_instr_decl_var(cnt, type, name);
  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_int(Context *cnt, Ast *node, uint64_t val)
{
  MirInstr *tmp               = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime               = true;
  tmp->const_value.type       = cnt->buildin_types.entry_s32;
  tmp->const_value.data.v_int = (long long int)val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_float(Context *cnt, Ast *node, float val)
{
  MirInstr *tmp         = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime         = true;
  tmp->const_value.type = cnt->buildin_types.entry_f32;
  // memcpy(&tmp->const_value.data, &val, sizeof(float));
  tmp->const_value.data.v_float = val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_double(Context *cnt, Ast *node, double val)
{
  MirInstr *tmp                  = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime                  = true;
  tmp->const_value.type          = cnt->buildin_types.entry_f64;
  tmp->const_value.data.v_double = val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_bool(Context *cnt, Ast *node, bool val)
{
  MirInstr *tmp                = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime                = true;
  tmp->const_value.type        = cnt->buildin_types.entry_bool;
  tmp->const_value.data.v_bool = val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_type(Context *cnt, Ast *node, MirType *type)
{
  MirInstr *tmp                = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime                = true;
  tmp->const_value.type        = cnt->buildin_types.entry_type;
  tmp->const_value.data.v_type = type;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_string(Context *cnt, Ast *node, const char *str)
{
  MirInstr *tmp               = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime               = true;
  tmp->const_value.type       = create_type_array(cnt, cnt->buildin_types.entry_u8, strlen(str));
  tmp->const_value.data.v_str = str;
  tmp->const_value.kind       = MIR_CV_STRING;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_null(Context *cnt, Ast *node)
{
  MirInstr *tmp                    = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime                    = true;
  tmp->const_value.type            = create_type_null(cnt);
  tmp->const_value.data.v_void_ptr = NULL;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_ret(Context *cnt, Ast *node, MirInstr *value)
{
  if (value) ref_instr(value);

  MirInstrRet *tmp           = create_instr(cnt, MIR_INSTR_RET, node, MirInstrRet *);
  tmp->value                 = value;
  tmp->base.const_value.type = cnt->buildin_types.entry_void;
  ref_instr(&tmp->base);

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
  MirInstrStore *tmp = create_instr(cnt, MIR_INSTR_STORE, node, MirInstrStore *);
  tmp->src           = src;
  tmp->dest          = dest;
  ref_instr(&tmp->base);

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_try_infer(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest)
{
  assert(src && dest);
  ref_instr(src);
  ref_instr(dest);
  MirInstrTryInfer *tmp = create_instr(cnt, MIR_INSTR_TRY_INFER, node, MirInstrTryInfer *);
  tmp->src              = src;
  tmp->dest             = dest;

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
append_instr_validate_type(Context *cnt, MirInstr *src)
{
  assert(src);
  ref_instr(src);
  MirInstrValidateType *tmp =
      create_instr(cnt, MIR_INSTR_VALIDATE_TYPE, NULL, MirInstrValidateType *);
  tmp->src                   = src;
  tmp->base.const_value.type = cnt->buildin_types.entry_void;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

/* LLVM */
void
init_type_llvm_ABI(Context *cnt, MirType *type)
{
  if (!type) return;

  switch (type->kind) {
  case MIR_TYPE_TYPE: {
    type->alignment        = alignof(MirType *);
    type->size_bits        = sizeof(MirType *) * 8;
    type->store_size_bytes = sizeof(MirType *);
    type->llvm_type        = LLVMVoidTypeInContext(cnt->module->llvm_cnt);
    break;
  }

  case MIR_TYPE_NULL: {
    type->alignment        = cnt->buildin_types.entry_u8_ptr->alignment;
    type->size_bits        = cnt->buildin_types.entry_u8_ptr->size_bits;
    type->store_size_bytes = cnt->buildin_types.entry_u8_ptr->store_size_bytes;
    type->llvm_type        = cnt->buildin_types.entry_u8_ptr->llvm_type;
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
    MirType *tmp = type->data.ptr.next;
    assert(tmp);
    assert(tmp->llvm_type);
    type->llvm_type        = LLVMPointerType(tmp->llvm_type, 0);
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->module->llvm_td, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->module->llvm_td, type->llvm_type);
    type->alignment        = LLVMABIAlignmentOfType(cnt->module->llvm_td, type->llvm_type);
    break;
  }

  case MIR_TYPE_FN: {
    MirType *    tmp_ret   = type->data.fn.ret_type;
    BArray *     tmp_args  = type->data.fn.arg_types;
    const size_t cargs     = tmp_args ? bo_array_size(tmp_args) : 0;
    LLVMTypeRef *llvm_args = NULL;
    LLVMTypeRef  llvm_ret  = NULL;

    if (tmp_args) {
      llvm_args = bl_malloc(cargs * sizeof(LLVMTypeRef));
      if (!llvm_args) bl_abort("bad alloc");

      MirType *tmp_arg;
      barray_foreach(tmp_args, tmp_arg)
      {
        assert(tmp_arg->llvm_type);
        llvm_args[i] = tmp_arg->llvm_type;
      }
    }

    llvm_ret = tmp_ret ? tmp_ret->llvm_type : LLVMVoidTypeInContext(cnt->module->llvm_cnt);
    assert(llvm_ret);

    type->llvm_type        = LLVMFunctionType(llvm_ret, llvm_args, (unsigned int)cargs, false);
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
    return type_cmp(first->data.ptr.next, second->data.ptr.next);
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
  mir_type_to_str(tmp_first, 256, first);
  mir_type_to_str(tmp_second, 256, second);
  msg_warning("missing type comparation for types %s and %s!!!", tmp_first, tmp_second);
#endif

  return false;
}

/* analyze */
bool
analyze_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
  elem_ptr->index = insert_instr_load_if_needed(cnt, elem_ptr->index);
  assert(elem_ptr->index);

  bool valid;
  elem_ptr->index = try_impl_cast(cnt, elem_ptr->index, cnt->buildin_types.entry_usize, &valid);
  if (!valid) return false;

  MirInstr *arr_ptr = elem_ptr->arr_ptr;
  assert(arr_ptr);
  assert(arr_ptr->const_value.type);

  assert(is_pointer_type(arr_ptr->const_value.type));
  MirType *arr_type = arr_ptr->const_value.type->data.ptr.next;
  assert(arr_type);

  if (arr_type->kind != MIR_TYPE_ARRAY) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, arr_ptr->node->src,
                BUILDER_CUR_WORD, "expected array type");
    return false;
  }

  /* setup ElemPtr instruction const_value type */
  MirType *elem_type = arr_type->data.array.elem_type;
  assert(elem_type);
  elem_ptr->tmp_value.type        = elem_type;
  elem_ptr->base.const_value.type = create_type_ptr(cnt, elem_type);

  /* source is declref -> we replace reference to true declaration */
  if (arr_ptr->kind == MIR_INSTR_DECL_REF) {
    elem_ptr->arr_ptr = ((MirInstrDeclRef *)arr_ptr)->scope_entry->instr;
    assert(elem_ptr->arr_ptr && "invalid source for elemptr instruction");
    erase_instr(arr_ptr);
  }

  erase_unused_instr(cnt, &elem_ptr->base);
  return true;
}

bool
analyze_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
  MirType *target_type = member_ptr->target_ptr->const_value.type;
  assert(target_type->kind == MIR_TYPE_PTR && "this should be compiler error");
  Ast *ast_member_ident = member_ptr->member_ident;
  assert(ast_member_ident);

  target_type = target_type->data.ptr.next;
  if (target_type->kind == MIR_TYPE_ARRAY) {
    /* check array buildin members */
    if (is_buildin_spec(ast_member_ident, BUILDIN_SPEC_ARR_LEN)) {
      assert(member_ptr->target_ptr->kind == MIR_INSTR_DECL_REF);
      erase_instr(member_ptr->target_ptr);
      /* mutate instruction into constant */
      MirInstr *len                = mutate_instr(&member_ptr->base, MIR_INSTR_CONST);
      len->const_value.type        = cnt->buildin_types.entry_usize;
      len->const_value.data.v_uint = target_type->data.array.len;
    } else {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_MEMBER_ACCESS, ast_member_ident->src,
                  BUILDER_CUR_WORD, "unknown member");
    }
  } else {
    bl_unimplemented;
  }

  erase_unused_instr(cnt, &member_ptr->base);
  return true;
}

bool
analyze_instr_addrof(Context *cnt, MirInstrAddrOf *addrof)
{
  MirInstr *src = addrof->src;
  assert(src);
  if (src->kind != MIR_INSTR_DECL_REF && src->kind != MIR_INSTR_ELEM_PTR) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_DECL, addrof->base.node->src,
                BUILDER_CUR_WORD, "cannot take the address of unallocated object");
    return false;
  }

  /* setup type */
  addrof->base.const_value.type = src->const_value.type;
  assert(addrof->base.const_value.type && "invalid type");

  /* source is declref -> we replace reference to true declaration */
  if (src->kind == MIR_INSTR_DECL_REF) {
    addrof->src = ((MirInstrDeclRef *)src)->scope_entry->instr;
    assert(addrof->src && "invalid source for addrof instruction");
    erase_instr(src);
  }

  return true;
}

bool
analyze_instr_cast(Context *cnt, MirInstrCast *cast)
{
  cast->next    = insert_instr_load_if_needed(cnt, cast->next);
  MirInstr *src = cast->next;
  assert(src);

  MirType *dest_type = cast->base.const_value.type;
  MirType *src_type  = src->const_value.type;

  if (!dest_type) {
    assert(cast->type && cast->type->kind == MIR_INSTR_CALL);
    analyze_instr(cnt, cast->type, true);
    MirConstValue *type_val = exec_call_top_lvl(cnt, (MirInstrCall *)cast->type);
    unref_instr(cast->type);
    erase_unused_instr(cnt, cast->type);
    assert(type_val->type && type_val->type->kind == MIR_TYPE_TYPE);
    dest_type = type_val->data.v_type;
  }

  assert(dest_type && "invalid cast destination type");
  assert(src_type && "invalid cast source type");

  cast->op = get_cast_op(src_type, dest_type);
  if (cast->op == MIR_CAST_INVALID) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_CAST, cast->base.node->src,
                BUILDER_CUR_WORD, "invalid cast");
    return false;
  }

  cast->base.const_value.type = dest_type;
  return true;
}

bool
analyze_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
  ScopeEntry *scope_entry = ref->scope_entry;
  assert(scope_entry);

  assert(scope_entry->instr);
  assert(scope_entry->instr->analyzed);
  assert(scope_entry->instr->const_value.type);

  ref->base.const_value.type = scope_entry->instr->const_value.type;

  if (scope_entry->parent_scope->is_global) ref_instr(scope_entry->instr);

  erase_unused_instr(cnt, &ref->base);
  return true;
}

bool
analyze_instr_arg(Context *cnt, MirInstrArg *arg)
{
  MirFn *fn = arg->base.owner_block->owner_fn;
  assert(fn);

  BArray *arg_types = fn->type->data.fn.arg_types;
  assert(arg_types && "trying to reference type of argument in function without arguments");

  assert(arg->i < bo_array_size(arg_types));
  arg->base.const_value.type = bo_array_at(arg_types, arg->i, MirType *);
  assert(arg->base.const_value.type);

  return true;
}

bool
analyze_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
  /* nothing to do :( */
  return true;
}

bool
analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto, bool comptime)
{
  MirInstrBlock *prev_block = NULL;

  /* resolve type */
  if (!fn_proto->base.const_value.type) {
    assert(fn_proto->type && fn_proto->type->kind == MIR_INSTR_CALL);
    analyze_instr(cnt, fn_proto->type, true);
    MirConstValue *type_val = exec_call_top_lvl(cnt, (MirInstrCall *)fn_proto->type);
    unref_instr(fn_proto->type);
    erase_unused_instr(cnt, fn_proto->type);
    assert(type_val->type && type_val->type->kind == MIR_TYPE_TYPE);

    if (fn_proto->user_type) {
      assert(fn_proto->user_type->kind == MIR_INSTR_CALL);
      analyze_instr(cnt, fn_proto->user_type, true);
      MirConstValue *user_type_val = exec_call_top_lvl(cnt, (MirInstrCall *)fn_proto->user_type);
      unref_instr(fn_proto->user_type);
      erase_unused_instr(cnt, fn_proto->user_type);
      assert(user_type_val->type && user_type_val->type->kind == MIR_TYPE_TYPE);

      if (!type_cmp(type_val->data.v_type, user_type_val->data.v_type)) {
        error_types(cnt, type_val->data.v_type, user_type_val->data.v_type,
                    fn_proto->user_type->node, NULL);
      }
    }

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
  if (!fn->name) {
    fn->name = gen_uq_name(cnt, IMPL_FN_NAME);
  }

  if (fn->is_external) {
    /* lookup external function exec handle */
    assert(fn->name);
    void *handle = dlFindSymbol(cnt->dl.lib, fn->name);
    assert(handle);

    if (!handle) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNKNOWN_SYMBOL, fn_proto->base.node->src,
                  BUILDER_CUR_WORD, "external symbol '%s' not found", fn->name);
    }

    fn->extern_entry = handle;
  } else {
    prev_block         = get_current_block(cnt);
    MirInstrBlock *tmp = fn->first_block;

    while (tmp) {
      if (comptime)
        analyze_instr_block(cnt, tmp, comptime);
      else {
        /* TODO: first_block can be used as linked list, only first one must be pushed into
         * analyze_stack
         */
        bo_array_push_back(cnt->analyze_stack, tmp);
      }

      tmp = (MirInstrBlock *)tmp->base.next;
    }

    assert(prev_block);
    set_cursor_block(cnt, prev_block);
  }

  /* push function into globals of the mir module */
  bo_array_push_back(cnt->module->globals, fn_proto);
  return true;
}

bool
analyze_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
  br->cond = insert_instr_load_if_needed(cnt, br->cond);
  assert(br->cond && br->then_block && br->else_block);
  assert(br->cond->analyzed);

  MirType *cond_type = br->cond->const_value.type;
  assert(cond_type);

  bool valid;
  br->cond = try_impl_cast(cnt, br->cond, cnt->buildin_types.entry_bool, &valid);
  if (!valid) return false;

  return true;
}

bool
analyze_instr_br(Context *cnt, MirInstrBr *br)
{
  assert(br->then_block);
  return true;
}

bool
analyze_instr_load(Context *cnt, MirInstrLoad *load)
{
  MirInstr *src = load->src;
  assert(src);
  if (!is_pointer_type(src->const_value.type)) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, src->node->src, BUILDER_CUR_WORD,
                "expected pointer");
    return false;
  }

  MirType *type = src->const_value.type->data.ptr.next;
  assert(type);
  load->base.const_value.type = type;

  /* src is declref -> we replace reference to true declaration */
  if (src->kind == MIR_INSTR_DECL_REF) {
    load->src = ((MirInstrDeclRef *)src)->scope_entry->instr;
    assert(load->src && "invalid destination for load instruction");
    erase_instr(src);
  }

  return true;
}

bool
analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn)
{
  assert(type_fn->base.const_value.type);
  assert(type_fn->ret_type ? type_fn->ret_type->analyzed : true);

  return true;
}

bool
analyze_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr)
{
  assert(type_arr->base.const_value.type);
  assert(type_arr->elem_type->analyzed);

  bool valid;
  type_arr->len = try_impl_cast(cnt, type_arr->len, cnt->buildin_types.entry_usize, &valid);
  if (!valid) return false;

  return true;
}

bool
analyze_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr)
{
  assert(type_ptr->type);
  return true;
}

bool
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
    setup_null_type_if_needed(binop->lhs->const_value.type, binop->rhs->const_value.type);
  else
    setup_null_type_if_needed(binop->rhs->const_value.type, binop->lhs->const_value.type);

  bool valid;
  binop->rhs = try_impl_cast(cnt, binop->rhs, binop->lhs->const_value.type, &valid);
  if (!valid) return false;

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
    return false;
  }

  MirType *type =
      ast_binop_is_logic(binop->op) ? cnt->buildin_types.entry_bool : lhs->const_value.type;
  assert(type);
  binop->base.const_value.type = type;

  erase_unused_instr(cnt, &binop->base);
  return true;
#undef is_valid
}

bool
analyze_instr_unop(Context *cnt, MirInstrUnop *unop)
{
  unop->instr = insert_instr_load_if_needed(cnt, unop->instr);
  assert(unop->instr && unop->instr->analyzed);
  MirType *type = unop->instr->const_value.type;
  assert(type);
  unop->base.const_value.type = type;

  erase_unused_instr(cnt, &unop->base);
  return true;
}

bool
analyze_instr_const(Context *cnt, MirInstrConst *cnst)
{
  assert(cnst->base.const_value.type);

  erase_unused_instr(cnt, &cnst->base);
  return true;
}

bool
analyze_instr_validate_type(Context *cnt, MirInstrValidateType *validate)
{
  MirInstr *src = validate->src;
  assert(src);

  if (!type_cmp(src->const_value.type, cnt->buildin_types.entry_type)) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, src->node->src, BUILDER_CUR_WORD,
                "expected type");
  }

  assert(src->const_value.data.v_type);

  erase_unused_instr(cnt, &validate->base);
  return true;
}

bool
analyze_instr_try_infer(Context *cnt, MirInstrTryInfer *infer)
{
  MirInstr *src  = infer->src;
  MirInstr *dest = infer->dest;
  assert(src && dest);
  assert(src->analyzed && dest->analyzed);

  assert(dest->kind == MIR_INSTR_DECL_VAR);
  if (dest->const_value.type) return true;

  const bool will_be_loaded = is_load_needed(infer->src);

  /* set type to decl var and variable */
  dest->const_value.type =
      will_be_loaded ? src->const_value.type : create_type_ptr(cnt, src->const_value.type);
  MirVar *var = ((MirInstrDeclVar *)dest)->var;
  assert(var);
  var->alloc_type = will_be_loaded ? src->const_value.type->data.ptr.next : src->const_value.type;

  assert(dest->const_value.type);
  erase_unused_instr(cnt, &infer->base);
  return true;
}

bool
analyze_instr_ret(Context *cnt, MirInstrRet *ret)
{
  /* compare return value with current function type */
  MirInstrBlock *block = get_current_block(cnt);
  assert(block);
  if (!block->terminal) block->terminal = &ret->base;

  ret->value      = insert_instr_load_if_needed(cnt, ret->value);
  MirInstr *value = ret->value;
  if (value) {
    assert(value->analyzed);
  }

  MirType *fn_type = get_current_fn(cnt)->type;
  assert(fn_type);
  assert(fn_type->kind == MIR_TYPE_FN);

  const bool expected_ret_value =
      !type_cmp(fn_type->data.fn.ret_type, cnt->buildin_types.entry_void);

  /* return value is not expected, and it's not provided */
  if (!expected_ret_value && !value) {
    return true;
  }

  /* return value is expected, but it's not provided */
  if (expected_ret_value && !value) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_EXPR, ret->base.node->src,
                BUILDER_CUR_AFTER, "expected return value");
    return false;
  }

  /* return value is not expected, but it's provided */
  if (!expected_ret_value && value) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_EXPR, ret->value->node->src,
                BUILDER_CUR_WORD, "unexpected return value");
    return false;
  }

  /* setup correct type of llvm null for call(null) */
  setup_null_type_if_needed(value->const_value.type, fn_type->data.fn.ret_type);

  bool valid;
  ret->value = try_impl_cast(cnt, ret->value, fn_type->data.fn.ret_type, &valid);
  if (!valid) return false;
  return true;
}

bool
analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
  if (decl->type) {
    assert(decl->type->kind == MIR_INSTR_CALL && "expected type resolver call");
    analyze_instr(cnt, decl->type, true);
    MirConstValue *resolved_type_value = exec_call_top_lvl(cnt, (MirInstrCall *)decl->type);
    unref_instr(decl->type);
    erase_unused_instr(cnt, decl->type);
    assert(resolved_type_value && resolved_type_value->type->kind == MIR_TYPE_TYPE);
    MirType *resolved_type = resolved_type_value->data.v_type;
    assert(resolved_type);

    decl->base.const_value.type = create_type_ptr(cnt, resolved_type);
    MirVar *var                 = decl->var;
    assert(var);
    var->alloc_type = resolved_type;
  }

  /* TODO: reference can be created later during analyze pass */
  /* TODO: reference can be created later during analyze pass */
  /* TODO: reference can be created later during analyze pass */
  /* TODO: reference can be created later during analyze pass */
  if (decl->base.ref_count == 0) {
    builder_msg(cnt->builder, BUILDER_MSG_WARNING, 0, decl->base.node->src, BUILDER_CUR_WORD,
                "unused declaration");
  }

  erase_unused_instr(cnt, &decl->base);
  return true;
}

bool
analyze_instr_call(Context *cnt, MirInstrCall *call, bool comptime)
{
  assert(call->callee);
  analyze_instr(cnt, call->callee, call->base.comptime || comptime);

  MirType *type = call->callee->const_value.type;
  assert(type && "invalid type of called object");

  if (is_pointer_type(type)) {
    /* we want to make calls also via pointer to functions so in such case we need to resolve
     * pointed function */
    type = type->data.ptr.next;
  }

  if (type->kind != MIR_TYPE_FN) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_FUNC, call->callee->node->src,
                BUILDER_CUR_WORD, "expected a function name");
    return false;
  }

  MirType *result_type = type->data.fn.ret_type;
  assert(result_type && "invalid type of call result");
  call->base.const_value.type = result_type;

  /* validate arguments */
  const size_t callee_argc = type->data.fn.arg_types ? bo_array_size(type->data.fn.arg_types) : 0;
  const size_t call_argc   = call->args ? bo_array_size(call->args) : 0;
  if (callee_argc != call_argc) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_ARG_COUNT, call->base.node->src,
                BUILDER_CUR_WORD, "expected %u %s, but called with %u", callee_argc,
                callee_argc == 1 ? "argument" : "arguments", call_argc);
    return false;
  }

  /* validate argument types */
  if (call_argc) {
    MirInstr **call_arg;
    MirType *  callee_arg_type;
    bool       valid;

    for (int32_t i = 0; i < call_argc; ++i) {
      call_arg        = &bo_array_at(call->args, i, MirInstr *);
      callee_arg_type = bo_array_at(type->data.fn.arg_types, i, MirType *);

      *call_arg = insert_instr_load_if_needed(cnt, *call_arg);

      /* setup correct type of llvm null for call(null) */
      setup_null_type_if_needed((*call_arg)->const_value.type, callee_arg_type);

      (*call_arg) = try_impl_cast(cnt, (*call_arg), callee_arg_type, &valid);
    }
  }

  /* destination is declref -> we replace reference to true declaration */
  if (call->callee->kind == MIR_INSTR_DECL_REF) {
    MirInstr *callee = call->callee;
    call->callee     = ((MirInstrDeclRef *)callee)->scope_entry->instr;
    assert(call->callee && "invalid destination for call instruction");
    erase_instr(callee);
  }

  return true;
}

bool
analyze_instr_store(Context *cnt, MirInstrStore *store)
{
  MirInstr *dest = store->dest;
  assert(dest);
  assert(dest->analyzed);

  assert(is_pointer_type(dest->const_value.type) && "store expect destination to be a pointer");

  MirType *dest_type = dest->const_value.type->data.ptr.next;
  assert(dest_type && "store destination has invalid base type");

  /* setup llvm type for null type */
  store->src = insert_instr_load_if_needed(cnt, store->src);

  /* insert load if needed */
  setup_null_type_if_needed(store->src->const_value.type, dest_type);

  /* validate types and optionaly create cast */
  bool valid;
  store->src = try_impl_cast(cnt, store->src, dest_type, &valid);
  if (!valid) return false;

  MirInstr *src = store->src;
  assert(src);

  /* store implicitly yields void const_value */
  store->base.const_value.type = cnt->buildin_types.entry_void;

  /* destination is declref -> we replace reference to true declaration */
  if (dest->kind == MIR_INSTR_DECL_REF) {
    store->dest = ((MirInstrDeclRef *)dest)->scope_entry->instr;
    assert(store->dest && "invalid destination for store instruction");
    erase_instr(dest);
  }

  return true;
}

bool
analyze_instr_block(Context *cnt, MirInstrBlock *block, bool comptime)
{
  assert(block);
  set_cursor_block(cnt, block);

  /* append implicit return for void functions or generate error when last block is not terminated
   */
  if (!is_block_terminated(block)) {
    MirFn *fn = block->owner_fn;
    assert(fn);

    if (fn->type->data.fn.ret_type->kind == MIR_TYPE_VOID) {
      append_instr_ret(cnt, NULL, NULL);
    } else {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_MISSING_RETURN, fn->node->src,
                  BUILDER_CUR_WORD, "not every path inside function return value");
    }
  }

  /* iterate over entry block of executable */
  MirInstr *instr = block->entry_instr;
  MirInstr *next  = NULL;

  while (instr) {
    next = instr->next;
    if (!analyze_instr(cnt, instr, comptime)) return false;
    instr = next;
  }

  return true;
}

bool
analyze_instr(Context *cnt, MirInstr *instr, bool comptime)
{
  if (!instr) return NULL;

  /* skip already analyzed instructions */
  if (instr->analyzed) return instr;
  instr->analyzed = true;
  bool state;

  switch (instr->kind) {
  case MIR_INSTR_BLOCK:
    state = analyze_instr_block(cnt, (MirInstrBlock *)instr, comptime);
    break;
  case MIR_INSTR_FN_PROTO:
    state = analyze_instr_fn_proto(cnt, (MirInstrFnProto *)instr, comptime);
    break;
  case MIR_INSTR_DECL_VAR:
    state = analyze_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
    break;
  case MIR_INSTR_CALL:
    state = analyze_instr_call(cnt, (MirInstrCall *)instr, comptime);
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
  case MIR_INSTR_TYPE_ARRAY:
    state = analyze_instr_type_array(cnt, (MirInstrTypeArray *)instr);
    break;
  case MIR_INSTR_TYPE_PTR:
    state = analyze_instr_type_ptr(cnt, (MirInstrTypePtr *)instr);
    break;
  case MIR_INSTR_VALIDATE_TYPE:
    state = analyze_instr_validate_type(cnt, (MirInstrValidateType *)instr);
    break;
  case MIR_INSTR_TRY_INFER:
    state = analyze_instr_try_infer(cnt, (MirInstrTryInfer *)instr);
    break;
  case MIR_INSTR_LOAD:
    state = analyze_instr_load(cnt, (MirInstrLoad *)instr);
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
  default:
    msg_warning("missing analyze for %s", mir_instr_name(instr));
    return false;
  }

  return state;
}

void
analyze(Context *cnt)
{
  BArray *  stack = cnt->analyze_stack;
  MirInstr *instr;

  if (cnt->verbose_pre) {
    barray_foreach(stack, instr)
    {
      /* Print verbose information before analyze */
      if (instr->kind == MIR_INSTR_FN_PROTO) mir_print_instr(instr, stdout);
    }
  }

  barray_foreach(stack, instr)
  {
    assert(instr);
    analyze_instr(cnt, instr, false);
  }

  if (cnt->verbose_post) {
    barray_foreach(stack, instr)
    {
      /* Print verbose information before analyze */
      if (instr->kind == MIR_INSTR_FN_PROTO) mir_print_instr(instr, stdout);
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
  MirInstr *instr = cnt->exec_stack->pc;
  MirFrame *fr    = cnt->exec_stack->ra;
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
  if (instr->ref_count == 0) return;

#if 0
  /* step-by-step execution */
  {
    mir_print_instr(instr);
    getchar();
  }
#endif

  switch (instr->kind) {
  case MIR_INSTR_CONST:
    exec_instr_const(cnt, (MirInstrConst *)instr);
    break;
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
  case MIR_INSTR_TYPE_FN:
    exec_instr_type_fn(cnt, (MirInstrTypeFn *)instr);
    break;
  case MIR_INSTR_TYPE_PTR:
    exec_instr_type_ptr(cnt, (MirInstrTypePtr *)instr);
    break;
  case MIR_INSTR_TYPE_ARRAY:
    exec_instr_type_array(cnt, (MirInstrTypeArray *)instr);
    break;
  case MIR_INSTR_DECL_VAR:
    exec_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
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

  if (src->kind == MIR_INSTR_DECL_VAR) {
    MirStackPtr ptr = exec_read_stack_ptr(cnt, src->const_value.data.v_rel_stack_ptr);
    exec_push_stack(cnt, (MirStackPtr)&ptr, type);
  }
}

void
exec_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
  /* pop index from stack */
  assert(is_pointer_type(elem_ptr->arr_ptr->const_value.type));
  MirType *   arr_type   = elem_ptr->arr_ptr->const_value.type->data.ptr.next;
  MirType *   index_type = elem_ptr->index->const_value.type;
  MirStackPtr index_ptr  = exec_pop_stack(cnt, index_type);

  MirRelativeStackPtr arr_rel_ptr = elem_ptr->arr_ptr->const_value.data.v_rel_stack_ptr;
  MirStackPtr         arr_ptr     = exec_read_stack_ptr(cnt, arr_rel_ptr);
  assert(arr_ptr && index_ptr);

  MirType *elem_type = arr_type->data.array.elem_type;
  assert(elem_type);

  MirConstValueData index = {0};
  exec_read_value(&index, index_ptr, index_type);

  {
    const size_t len = arr_type->data.array.len;
    if (index.v_uint > len) {
      msg_error("Array index out of range! Index is %llu but array size is %llu",
                (unsigned long long)index.v_uint, (unsigned long long)len);
      exec_abort(cnt, 0);
    }
  }

  MirConstValueData result = {0};
  result.v_stack_ptr = (MirStackPtr)((arr_ptr) + (index.v_uint * elem_type->store_size_bytes));

#if BL_DEBUG
  {
    ptrdiff_t _diff = result.v_uint - (uintptr_t)arr_ptr;
    assert(_diff / elem_type->store_size_bytes == index.v_uint);
  }
#endif
  /* push result address on the stack */
  exec_push_stack(cnt, &result, elem_ptr->base.const_value.type);
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
    MirStackPtr from_ptr = exec_pop_stack(cnt, src_type);
    exec_read_value(&tmp, from_ptr, src_type);

    size_t        shift = src_type->size_bits - 1;
    const uint8_t neg   = (tmp.v_uint >> shift) & 1U;
    uint64_t      mask  = 0;
    while (shift < dest_type->size_bits) {
      mask = 1 << shift;
      tmp.v_uint ^= (mask & (neg << shift));
      ++shift;
    }

    exec_push_stack(cnt, (MirStackPtr)&tmp, dest_type);
    break;
  }

  case MIR_CAST_ZEXT:
    /* src is smaller than dest and destination is unsigned, src value will be extended with zeros
     * to dest type size */
  case MIR_CAST_TRUNC: {
    /* src is bigger than dest */
    MirStackPtr from_ptr = exec_pop_stack(cnt, src_type);
    exec_read_value(&tmp, from_ptr, src_type);
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

  /* resolve argument pointer */
  BArray * arg_types = fn->type->data.fn.arg_types;
  MirType *tmp_type  = NULL;
  /* starting point */
  uint8_t *arg_ptr = (uint8_t *)cnt->exec_stack->ra;
  for (int32_t i = 0; i <= arg->i; ++i) {
    tmp_type = bo_array_at(arg_types, i, MirType *);
    assert(tmp_type);
    arg_ptr -= exec_stack_alloc_size(tmp_type->store_size_bytes);
  }

  exec_push_stack(cnt, (MirStackPtr)arg_ptr, arg->base.const_value.type);
}

void
exec_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
  assert(br->cond);
  MirType *type = br->cond->const_value.type;

  /* pop condition from stack */
  MirStackPtr cond = exec_pop_stack(cnt, type);
  assert(cond);

  MirConstValueData tmp = {0};
  exec_read_value(&tmp, cond, type);

  if (tmp.v_int) {
    exec_set_pc(cnt, br->then_block->entry_instr);
  } else {
    exec_set_pc(cnt, br->else_block->entry_instr);
  }
}

void
exec_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
  assert(decl->base.const_value.type);

  MirVar *var = decl->var;
  assert(var);

  /* allocate memory for variable on stack */
  MirRelativeStackPtr ptr = exec_push_stack(cnt, NULL, var->alloc_type);

  decl->base.const_value.data.v_rel_stack_ptr = ptr;
}

void
exec_instr_load(Context *cnt, MirInstrLoad *load)
{
  /* pop source from stack or load directly when src is declaration, push on to stack dereferenced
   * value of source */
  MirType *src_type  = load->src->const_value.type;
  MirType *dest_type = load->base.const_value.type;
  assert(src_type && dest_type);
  assert(is_pointer_type(src_type));

  MirStackPtr src_ptr = NULL;
  if (load->src->kind == MIR_INSTR_DECL_VAR) {
    MirRelativeStackPtr src_rel_ptr = load->src->const_value.data.v_rel_stack_ptr;
    src_ptr                         = exec_read_stack_ptr(cnt, src_rel_ptr);
  } else {
    src_ptr = *exec_pop_stack_as(cnt, src_type, MirStackPtr *);
  }

  if (!src_ptr) {
    msg_error("Dereferencing null pointer!");
    exec_abort(cnt, 0);
  }

  exec_push_stack(cnt, src_ptr, dest_type);
}

void
exec_instr_store(Context *cnt, MirInstrStore *store)
{
  /* loads destination (in case it is not direct reference to declaration) and source from stack
   */
  MirType *src_type  = store->src->const_value.type;
  MirType *dest_type = store->dest->const_value.type;
  assert(src_type && dest_type);
  assert(is_pointer_type(dest_type));

  MirStackPtr dest_ptr = NULL;
  MirStackPtr src_ptr  = NULL;

  if (store->dest->kind == MIR_INSTR_DECL_VAR) {
    MirRelativeStackPtr dest_rel_ptr = store->dest->const_value.data.v_rel_stack_ptr;
    dest_ptr                         = exec_read_stack_ptr(cnt, dest_rel_ptr);
  } else {
    dest_ptr = *exec_pop_stack_as(cnt, dest_type, MirStackPtr *);
  }

  src_ptr = exec_pop_stack(cnt, src_type);

  assert(dest_ptr && src_ptr);
  memcpy(dest_ptr, src_ptr, src_type->store_size_bytes);
}

void
exec_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn)
{
  BArray *arg_types = NULL;
  if (type_fn->arg_types) {
    arg_types = bo_array_new(sizeof(MirType *));
    bo_array_reserve(arg_types, bo_array_size(type_fn->arg_types));

    MirInstr *arg_type;
    MirType * tmp;
    barray_foreach(type_fn->arg_types, arg_type)
    {
      tmp = *exec_pop_stack_as(cnt, arg_type->const_value.type, MirType **);
      assert(tmp);
      bo_array_push_back(arg_types, tmp);
    }
  }

  MirType *ret_type = NULL;
  if (type_fn->ret_type) {
    ret_type = *exec_pop_stack_as(cnt, type_fn->ret_type->const_value.type, MirType **);
    assert(ret_type);
  }

  MirType *tmp = create_type_fn(cnt, ret_type, arg_types);
  exec_push_stack(cnt, (MirStackPtr)&tmp, cnt->buildin_types.entry_type);
}

void
exec_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr)
{
  MirType *type = *exec_pop_stack_as(cnt, type_ptr->type->const_value.type, MirType **);
  assert(type);

  /* create pointer type */
  MirType *tmp = create_type_ptr(cnt, type);

  exec_push_stack(cnt, (MirStackPtr)&tmp, cnt->buildin_types.entry_type);
}

void
exec_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr)
{
  /* pop elm type */
  MirType *elem_type = *exec_pop_stack_as(cnt, type_arr->elem_type->const_value.type, MirType **);
  assert(elem_type);

  /* pop arr size */
  /* TODO: len set by immutable variable need to be set before use!!! */
  MirStackPtr       len_ptr = exec_pop_stack(cnt, type_arr->len->const_value.type);
  MirConstValueData len     = {0};
  exec_read_value(&len, len_ptr, type_arr->len->const_value.type);

  assert(elem_type);

  MirConstValueData tmp = {0};
  tmp.v_type            = create_type_array(cnt, elem_type, len.v_uint);

  exec_push_stack(cnt, &tmp, cnt->buildin_types.entry_type);
}

void
exec_instr_const(Context *cnt, MirInstrConst *cnst)
{
  assert(cnst->base.const_value.type);
  MirConstValue *value = &cnst->base.const_value;

  switch (value->kind) {
  case MIR_CV_BASIC:
    exec_push_stack(cnt, (MirStackPtr)&cnst->base.const_value.data, cnst->base.const_value.type);
    break;
  case MIR_CV_STRING: {
    assert(value->data.v_str && "invalid string value");
    exec_push_stack(cnt, (MirStackPtr)value->data.v_str, cnst->base.const_value.type);
    break;
  }
  default:
    bl_unimplemented;
  }
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

  exec_reset_stack(cnt->exec_stack);
  /* push terminal frame on stack */
  exec_push_ra(cnt, NULL);

  /* store return frame pointer */
  fn->exec_ret_value = out_value;

  /* setup entry instruction */
  exec_set_pc(cnt, fn->first_block->entry_instr);

  /* iterate over entry block of executable */
  MirInstr *instr, *prev;
  while (true) {
    instr = exec_get_pc(cnt);
    prev  = instr;
    if (!instr || cnt->exec_stack->aborted) break;

    exec_instr(cnt, instr);

    /* stack head can be changed by br instructions */
    if (exec_get_pc(cnt) == prev) exec_set_pc(cnt, instr->next);
  }

  return does_return_value && !cnt->exec_stack->aborted;
}

static inline void
exec_push_dc_arg(Context *cnt, MirStackPtr val_ptr, MirType *type)
{
  assert(type);

  MirConstValueData tmp = {0};
  exec_read_value(&tmp, val_ptr, type);

  switch (type->kind) {
  case MIR_TYPE_INT: {
    switch (type->data.integer.bitcount) {
    case 64:
      dcArgLongLong(cnt->dl.vm, tmp.v_int);
      break;
    case 32:
      dcArgInt(cnt->dl.vm, (DCint)tmp.v_int);
      break;
    case 16:
      dcArgShort(cnt->dl.vm, (DCshort)tmp.v_int);
      break;
    case 8:
      dcArgChar(cnt->dl.vm, (DCchar)tmp.v_int);
      break;
    default:
      bl_abort("unsupported external call integer argument type");
    }
    break;
  }

  default:
    bl_abort("unsupported external call argument type");
  }
}

/*
 * Stack operations of call instruction and argument ordering on stack
 *
 * pc   program counter (pointer to current instruction)
 * RA   return address (used later for rollback of the stack)
 *
 * call fn (1, 2, 3) 4
 *
 * | stack op | data         | instr |
 * |----------+--------------+-------|
 * | PUSH     | 3            | ?     |
 * | PUSH     | 2            | ?     |
 * | PUSH     | 1            | ?     |
 * | PUSH RA  | pc, prev RA  | call  |
 * | ...      | -            | -     |
 * | POP RA   | -            | ret   |
 * | POP      | -            | ret   |
 * | POP      | -            | ret   |
 * | POP      | -            | ret   |
 * | PUSH     | 4            | ret   |
 */
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

  if (fn->is_external) {
    /* call setup and clenup */
    assert(fn->extern_entry);
    dcMode(cnt->dl.vm, DC_CALL_C_DEFAULT);
    dcReset(cnt->dl.vm);

    /* pop all arguments from the stack */
    MirStackPtr arg_ptr;
    BArray *    arg_types = fn->type->data.fn.arg_types;
    if (arg_types) {
      MirType *arg_type;
      barray_foreach(arg_types, arg_type)
      {
        arg_ptr = exec_pop_stack(cnt, arg_type);
        exec_push_dc_arg(cnt, arg_ptr, arg_type);
      }
    }

    int64_t result = 0;
    switch (ret_type->kind) {
    case MIR_TYPE_INT:
      switch (ret_type->data.integer.bitcount) {
      case sizeof(char) * 8:
        result = dcCallChar(cnt->dl.vm, fn->extern_entry);
        break;
      case sizeof(short) * 8:
        result = dcCallShort(cnt->dl.vm, fn->extern_entry);
        break;
      case sizeof(int) * 8:
        result = dcCallInt(cnt->dl.vm, fn->extern_entry);
        break;
      case sizeof(long long) * 8:
        result = dcCallLongLong(cnt->dl.vm, fn->extern_entry);
        break;
      default:
        bl_abort("unsupported integer size for external call result");
      }
      break;

    default:
      bl_abort("unsupported external call return type");
    }

    exec_push_stack(cnt, (MirStackPtr)&result, ret_type);
  } else {
    /* Push current frame stack top. (Later poped by ret instruction)*/
    exec_push_ra(cnt, &call->base);
    assert(fn->first_block->entry_instr);

    /* setup entry instruction */
    exec_set_pc(cnt, fn->first_block->entry_instr);
  }
}

void
exec_instr_ret(Context *cnt, MirInstrRet *ret)
{
  MirFn *fn = ret->base.owner_block->owner_fn;
  assert(fn);

  MirType *   ret_type     = NULL;
  MirStackPtr ret_data_ptr = NULL;

  /* pop return value from stack */
  if (ret->value) {
    ret_type = ret->value->const_value.type;
    assert(ret_type);

    ret_data_ptr = exec_pop_stack(cnt, ret->value->const_value.type);
    assert(ret_data_ptr);

    /* TODO: remove */
    /* set fn execution resulting instruction */
    if (fn->exec_ret_value) {
      const size_t size = ret_type->store_size_bytes;
      memcpy(fn->exec_ret_value, ret_data_ptr, size);
    }

    /* read callee from frame stack */
    MirInstr *callee = exec_get_ra(cnt)->callee;
    /* discard return value pointer if result is not used on caller size, this solution is kinda
     * messy... */
    if (!(callee && callee->ref_count > 1)) ret_data_ptr = NULL;
  }

  /* do frame stack rollback */
  MirInstr *pc = exec_pop_ra(cnt);

  /* clean up all arguments from the stack */
  BArray *arg_types = fn->type->data.fn.arg_types;
  if (arg_types) {
    MirType *arg_type;
    barray_foreach(arg_types, arg_type)
    {
      exec_pop_stack(cnt, arg_type);
    }
  }

  /* push return value on the stack if there is one */
  if (ret_data_ptr) {
    exec_push_stack(cnt, ret_data_ptr, ret_type);
  }

  /* set program counter to next instruction */
  pc = pc ? pc->next : NULL;
  exec_set_pc(cnt, pc);
}

void
exec_instr_binop(Context *cnt, MirInstrBinop *binop)
{
#define binop(_op, _lhs, _rhs, _result, _v_T)                                                      \
  {                                                                                                \
    switch (_op) {                                                                                 \
    case BINOP_ADD:                                                                                \
      (_result)._v_T = _lhs._v_T + _rhs._v_T;                                                      \
      break;                                                                                       \
    case BINOP_SUB:                                                                                \
      (_result)._v_T = _lhs._v_T - _rhs._v_T;                                                      \
      break;                                                                                       \
    case BINOP_MUL:                                                                                \
      (_result)._v_T = _lhs._v_T * _rhs._v_T;                                                      \
      break;                                                                                       \
    case BINOP_DIV:                                                                                \
      assert(_rhs._v_T != 0 && "divide by zero, this should be error");                            \
      (_result)._v_T = _lhs._v_T / _rhs._v_T;                                                      \
      break;                                                                                       \
    case BINOP_EQ:                                                                                 \
      (_result).v_int = (int64_t)(_lhs._v_T == _rhs._v_T);                                         \
      break;                                                                                       \
    case BINOP_NEQ:                                                                                \
      (_result).v_int = (int64_t)(_lhs._v_T != _rhs._v_T);                                         \
      break;                                                                                       \
    case BINOP_LESS:                                                                               \
      (_result).v_int = (int64_t)(_lhs._v_T < _rhs._v_T);                                          \
      break;                                                                                       \
    case BINOP_LESS_EQ:                                                                            \
      (_result).v_int = (int64_t)(_lhs._v_T == _rhs._v_T);                                         \
      break;                                                                                       \
    case BINOP_GREATER:                                                                            \
      (_result).v_int = (int64_t)(_lhs._v_T > _rhs._v_T);                                          \
      break;                                                                                       \
    case BINOP_GREATER_EQ:                                                                         \
      (_result).v_int = (int64_t)(_lhs._v_T >= _rhs._v_T);                                         \
      break;                                                                                       \
    case BINOP_LOGIC_AND:                                                                          \
      (_result).v_int = (int64_t)(_lhs._v_T && _rhs._v_T);                                         \
      break;                                                                                       \
    case BINOP_LOGIC_OR:                                                                           \
      (_result).v_int = (int64_t)(_lhs._v_T || _rhs._v_T);                                         \
      break;                                                                                       \
    default:                                                                                       \
      bl_unimplemented;                                                                            \
    }                                                                                              \
  }

  /* binop expects lhs and rhs on stack in exact order and push result again to the stack */
  MirType *type = binop->lhs->const_value.type;
  assert(type);
  MirStackPtr lhs_ptr = exec_pop_stack(cnt, binop->lhs->const_value.type);
  MirStackPtr rhs_ptr = exec_pop_stack(cnt, binop->rhs->const_value.type);
  assert(rhs_ptr && lhs_ptr);

  MirConstValueData result = {0};
  MirConstValueData lhs    = {0};
  MirConstValueData rhs    = {0};

  exec_read_value(&lhs, lhs_ptr, type);
  exec_read_value(&rhs, rhs_ptr, type);

  switch (type->kind) {
  case MIR_TYPE_INT: {
    binop(binop->op, lhs, rhs, result, v_uint);
    break;
  }

  case MIR_TYPE_REAL: {
    const size_t size = type->store_size_bytes;

    if (size == sizeof(float)) { // float
      binop(binop->op, lhs, rhs, result, v_float);
    } else if (size == sizeof(double)) { // double
      binop(binop->op, lhs, rhs, result, v_double);
    } else {
      bl_abort("invalid floating point type");
    }
    break;
  }

  case MIR_TYPE_BOOL:
    binop(binop->op, lhs, rhs, result, v_int);
    break;

  case MIR_TYPE_PTR:
  case MIR_TYPE_NULL:
    binop(binop->op, lhs, rhs, result, v_uint);
    break;

  default:
    bl_abort("invalid binop type");
  }

  exec_push_stack(cnt, &result, binop->base.const_value.type);
#undef binop
}

void
exec_instr_unop(Context *cnt, MirInstrUnop *unop)
{
#define unop(_op, _value, _result, _v_T)                                                           \
  {                                                                                                \
    switch (_op) {                                                                                 \
    case UNOP_NEG:                                                                                 \
      (_result)._v_T = _value._v_T * -1;                                                           \
      break;                                                                                       \
    case UNOP_POS:                                                                                 \
      (_result)._v_T = _value._v_T;                                                                \
      break;                                                                                       \
    case UNOP_NOT:                                                                                 \
      (_result)._v_T = !_value._v_T;                                                               \
      break;                                                                                       \
    default:                                                                                       \
      bl_unimplemented;                                                                            \
    }                                                                                              \
  }

  assert(unop->base.const_value.type);
  MirType *   value_type = unop->instr->const_value.type;
  MirStackPtr value_ptr  = exec_pop_stack(cnt, value_type);
  assert(value_ptr);

  MirType *type = unop->instr->const_value.type;
  assert(type);

  MirConstValueData result = {0};
  MirConstValueData value  = {0};
  exec_read_value(&value, value_ptr, type);

  switch (type->kind) {
  case MIR_TYPE_INT: {
    unop(unop->op, value, result, v_uint);
    break;
  }

  case MIR_TYPE_REAL: {
    const int32_t size = type->store_size_bytes;

    if (size == sizeof(float)) { // float
      unop(unop->op, value, result, v_float);
    } else if (size == sizeof(double)) { // double
      unop(unop->op, value, result, v_double);
    } else {
      bl_abort("invalid floating point type");
    }
    break;
  }

  case MIR_TYPE_BOOL:
    unop(unop->op, value, result, v_int);
    break;

  default:
    bl_abort("invalid unop type");
  }

  exec_push_stack(cnt, &result, value_type);
#undef unop
}

/* MIR building */
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
  if (cnt->builder->flags & BUILDER_FORCE_TEST_LLVM) ref_instr(&fn_proto->base);

  fn_proto->base.const_value.type = cnt->buildin_types.entry_test_case_fn;

  MirInstrBlock *prev_block = get_current_block(cnt);
  MirFn *        fn =
      create_fn(cnt, test, TEST_CASE_FN_NAME, false, true); /* TODO: based on user flag!!! */
  fn->node = test;

  assert(test->data.test_case.desc);
  fn->test_case_desc                   = test->data.test_case.desc;
  fn_proto->base.const_value.data.v_fn = fn;

  bo_array_push_back(cnt->test_cases, fn);

  append_block(cnt, fn_proto->base.const_value.data.v_fn, "init");
  MirInstrBlock *entry_block = append_block(cnt, fn_proto->base.const_value.data.v_fn, "entry");
  set_cursor_block(cnt, entry_block);

  /* generate body instructions */
  ast(cnt, ast_block);

  /* terminate initialization block */
  set_cursor_block(cnt, fn->first_block);
  append_instr_br(cnt, NULL, entry_block);

  set_cursor_block(cnt, prev_block);
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
  set_cursor_block(cnt, then_block);
  ast(cnt, ast_then);

  tmp_block = get_current_block(cnt);
  if (!get_block_terminator(tmp_block)) {
    /* block has not been terminated -> add terminator */
    append_instr_br(cnt, NULL, cont_block);
  }

  /* else if */
  if (ast_else) {
    set_cursor_block(cnt, else_block);
    ast(cnt, ast_else);

    tmp_block = get_current_block(cnt);
    if (!is_block_terminated(tmp_block)) append_instr_br(cnt, NULL, cont_block);
  }

  if (!is_block_terminated(else_block)) {
    /* block has not been terminated -> add terminator */
    set_cursor_block(cnt, else_block);
    append_instr_br(cnt, NULL, cont_block);
  }

  set_cursor_block(cnt, cont_block);
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

  MirInstrBlock *prev_break_block    = cnt->break_block;
  MirInstrBlock *prev_continue_block = cnt->continue_block;
  cnt->break_block                   = cont_block;
  cnt->continue_block                = ast_increment ? increment_block : decide_block;

  /* generate initialization if there is one */
  if (ast_init) {
    ast(cnt, ast_init);
  }

  /* decide block */
  append_instr_br(cnt, NULL, decide_block);
  set_cursor_block(cnt, decide_block);

  MirInstr *cond = ast_cond ? ast(cnt, ast_cond) : append_instr_const_bool(cnt, NULL, true);

  append_instr_cond_br(cnt, ast_cond, cond, body_block, cont_block);

  /* loop body */
  set_cursor_block(cnt, body_block);
  ast(cnt, ast_block);

  tmp_block = get_current_block(cnt);
  if (!is_block_terminated(tmp_block)) {
    append_instr_br(cnt, NULL, ast_increment ? increment_block : decide_block);
  }

  /* increment if there is one */
  if (ast_increment) {
    set_cursor_block(cnt, increment_block);
    ast(cnt, ast_increment);
    append_instr_br(cnt, NULL, decide_block);
  }

  cnt->break_block    = prev_break_block;
  cnt->continue_block = prev_continue_block;
  set_cursor_block(cnt, cont_block);
}

void
ast_stmt_break(Context *cnt, Ast *br)
{
  assert(cnt->break_block && "break statement outside the loop");
  append_instr_br(cnt, br, cnt->break_block);
}

void
ast_stmt_continue(Context *cnt, Ast *cont)
{
  assert(cnt->continue_block && "break statement outside the loop");
  append_instr_br(cnt, cont, cnt->continue_block);
}

void
ast_stmt_return(Context *cnt, Ast *ret)
{
  MirInstr *value = ast(cnt, ret->data.stmt_return.expr);
  append_instr_ret(cnt, ret, value);
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

  MirInstr *type = ast_create_type_resolver_call(cnt, ast_type);
  MirInstr *next = ast(cnt, ast_next);

  return append_instr_cast(cnt, cast, type, next);
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
  BArray *  args   = NULL;

  /* arguments need to be generated into reverse order due to bytecode call conventions */
  if (ast_args) {
    const size_t argc = bo_array_size(ast_args);
    args              = bo_array_new(sizeof(MirInstr *));
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

  /* symbol lookup */
  Scope *scope = ident->data.ident.scope;
  assert(scope);
  ScopeEntry *scope_entry = scope_lookup(scope, ident->data.ident.hash, true);
  if (!scope_entry) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNKNOWN_SYMBOL, ident->src, BUILDER_CUR_WORD,
                "unknown symbol");
  }

  return append_instr_decl_ref(cnt, ident, scope_entry);
}

MirInstr *
ast_expr_elem(Context *cnt, Ast *elem)
{
  Ast *ast_arr   = elem->data.expr_elem.next;
  Ast *ast_index = elem->data.expr_elem.index;
  assert(ast_arr && ast_index);

  MirInstr *arr_ptr = ast(cnt, ast_arr);
  MirInstr *index   = ast(cnt, ast_index);

  return append_instr_elem_ptr(cnt, elem, arr_ptr, index);
}

MirInstr *
ast_expr_member(Context *cnt, Ast *member)
{
  Ast *ast_next = member->data.expr_member.next;
  assert(ast_next);

  MirInstr *target = ast(cnt, ast_next);
  assert(target);

  return append_instr_member_ptr(cnt, member, target, member->data.expr_member.ident, -1);
}

MirInstr *
ast_expr_lit_fn(Context *cnt, Ast *lit_fn)
{
  /* creates function prototype */
  Ast *ast_block   = lit_fn->data.expr_fn.block;
  Ast *ast_fn_type = lit_fn->data.expr_fn.type;

  MirInstrFnProto *fn_proto = (MirInstrFnProto *)append_instr_fn_proto(cnt, lit_fn, NULL, NULL);

  fn_proto->type = ast_create_type_resolver_call(cnt, ast_fn_type);
  assert(fn_proto->type);

  MirInstrBlock *prev_block = get_current_block(cnt);
  MirFn *fn = create_fn(cnt, lit_fn, NULL, !ast_block, false); /* TODO: based on user flag!!! */
  fn_proto->base.const_value.data.v_fn = fn;

  /* function body */
  /* external functions has no body */
  if (fn->is_external) return &fn_proto->base;

  /* create block for initialization locals and arguments */
  MirInstrBlock *init_block = append_block(cnt, fn_proto->base.const_value.data.v_fn, "init");
  set_cursor_block(cnt, init_block);

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
        MirInstr *var = append_instr_decl_var(cnt, NULL, ast_arg_name);
        MirInstr *arg = append_instr_arg(cnt, NULL, i);
        append_instr_try_infer(cnt, NULL, arg, var);
        append_instr_store(cnt, NULL, arg, var);

        /* registrate argument into local scope */
        provide(cnt, ast_arg_name, var, false);
      }
    }
  }

  MirInstrBlock *entry_block = append_block(cnt, fn_proto->base.const_value.data.v_fn, "entry");
  set_cursor_block(cnt, entry_block);

  /* generate body instructions */
  ast(cnt, ast_block);

  /* terminate initialization block */
  set_cursor_block(cnt, fn->first_block);
  append_instr_br(cnt, NULL, entry_block);

  set_cursor_block(cnt, prev_block);
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

      return append_instr_store(cnt, binop, tmp, lhs);
    }

    case BINOP_SUB_ASSIGN: {
      MirInstr *rhs = ast(cnt, ast_rhs);
      MirInstr *lhs = ast(cnt, ast_lhs);
      MirInstr *tmp = append_instr_binop(cnt, binop, lhs, rhs, BINOP_SUB);
      return append_instr_store(cnt, binop, tmp, lhs);
    }

    case BINOP_MUL_ASSIGN: {
      MirInstr *rhs = ast(cnt, ast_rhs);
      MirInstr *lhs = ast(cnt, ast_lhs);
      MirInstr *tmp = append_instr_binop(cnt, binop, lhs, rhs, BINOP_MUL);
      return append_instr_store(cnt, binop, tmp, lhs);
    }

    case BINOP_DIV_ASSIGN: {
      MirInstr *rhs = ast(cnt, ast_rhs);
      MirInstr *lhs = ast(cnt, ast_lhs);
      MirInstr *tmp = append_instr_binop(cnt, binop, lhs, rhs, BINOP_DIV);
      return append_instr_store(cnt, binop, tmp, lhs);
    }

    case BINOP_MOD_ASSIGN: {
      MirInstr *rhs = ast(cnt, ast_rhs);
      MirInstr *lhs = ast(cnt, ast_lhs);
      MirInstr *tmp = append_instr_binop(cnt, binop, lhs, rhs, BINOP_MOD);
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
ast_decl_entity(Context *cnt, Ast *entity)
{
  MirInstr *result    = NULL;
  Ast *     ast_name  = entity->data.decl.name;
  Ast *     ast_type  = entity->data.decl.type;
  Ast *     ast_value = entity->data.decl_entity.value;

  if (ast_value && ast_value->kind == AST_EXPR_LIT_FN) {
    MirInstr *value                    = ast(cnt, ast_value);
    value->const_value.data.v_fn->name = ast_name->data.ident.str;
    value->node                        = ast_name;

    if (!is_ident_in_gscope(ast_name))
      provide(cnt, ast_name, value, false);
    else
      provide_into_existing_scope_entry(cnt, ast_name, value);

    if (ast_type) {
      ((MirInstrFnProto *)value)->user_type = ast_create_type_resolver_call(cnt, ast_type);
    }

    /* check main */
    if (is_buildin_spec(ast_name, BUILDIN_SPEC_MAIN)) {
      assert(!cnt->entry_fn);
      cnt->entry_fn = value->const_value.data.v_fn;
      ref_instr(value);
    }
  } else {
    MirInstr *type = ast_type ? ast_create_type_resolver_call(cnt, ast_type) : NULL;

    MirInstrBlock *prev_block = get_current_block(cnt);
    MirFn *        fn         = get_current_fn(cnt);
    set_cursor_block(cnt, fn->first_block);
    MirInstr *decl = append_instr_decl_var(cnt, type, ast_name);
    set_cursor_block(cnt, prev_block);

    if (!is_ident_in_gscope(ast_name))
      provide(cnt, ast_name, decl, false);
    else
      provide_into_existing_scope_entry(cnt, ast_name, decl);

    /* initialize const_value */
    MirInstr *value = ast(cnt, ast_value);
    if (value) {
      if (!type) {
        append_instr_try_infer(cnt, NULL, value, decl);
      }
      result = append_instr_store(cnt, ast_value, value, decl);
    }

    if (is_buildin_spec(ast_name, BUILDIN_SPEC_MAIN)) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_FUNC, ast_name->src,
                  BUILDER_CUR_WORD, "'main' is expected to be a function");
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
ast_type_ref(Context *cnt, Ast *type_ref)
{
  MirInstr *result  = NULL;
  Ast *     ast_ref = type_ref->data.type_ref.ident;
  assert(ast_ref);

  BuildinType id = is_buildin_type(cnt, ast_ref->data.ident.hash);
  if (id != BUILDIN_TYPE_NONE) {
    /* buildin primitive !!! */
    result = append_instr_const_type(cnt, ast_ref, get_buildin(cnt, id));
  }

  assert(result && "unimplemented type ref resolving");
  append_instr_validate_type(cnt, result);
  return result;
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
    arg_types      = bo_array_new(sizeof(MirInstr *));
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
ast_type_ptr(Context *cnt, Ast *type_ptr)
{
  Ast *ast_type = type_ptr->data.type_ptr.type;
  assert(ast_type && "invalid pointee type");
  MirInstr *type = ast(cnt, ast_type);
  assert(type);
  return append_instr_type_ptr(cnt, type_ptr, type);
}

MirInstr *
ast_create_type_resolver_call(Context *cnt, Ast *type)
{
  if (!type) return NULL;
  MirInstrBlock *prev_block = get_current_block(cnt);
  MirInstr *     fn         = create_instr_fn_proto(cnt, NULL, NULL, NULL);
  fn->const_value.type      = cnt->buildin_types.entry_resolve_type_fn;
  fn->const_value.data.v_fn = create_fn(cnt, NULL, RESOLVE_TYPE_FN_NAME, false, false);

  MirInstrBlock *entry = append_block(cnt, fn->const_value.data.v_fn, "entry");
  set_cursor_block(cnt, entry);

  MirInstr *result = ast(cnt, type);
  append_instr_ret(cnt, NULL, result);

  set_cursor_block(cnt, prev_block);
  return create_instr_call_type_resolve(cnt, fn, type);
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
  case AST_TYPE_REF:
    return ast_type_ref(cnt, node);
  case AST_TYPE_FN:
    return ast_type_fn(cnt, node);
  case AST_TYPE_ARR:
    return ast_type_arr(cnt, node);
  case AST_TYPE_PTR:
    return ast_type_ptr(cnt, node);
  case AST_EXPR_ADDROF:
    return ast_expr_addrof(cnt, node);
  case AST_EXPR_CAST:
    return ast_expr_cast(cnt, node);
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
  assert(instr);
  switch (instr->kind) {
  case MIR_INSTR_INVALID:
    return "InstrInvalid";
  case MIR_INSTR_BLOCK:
    return "InstrBlock";
  case MIR_INSTR_DECL_VAR:
    return "InstrDeclVar";
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
  case MIR_INSTR_VALIDATE_TYPE:
    return "InstrValidateType";
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
  case MIR_INSTR_TYPE_ARRAY:
    return "InstrTypeArray";
  case MIR_INSTR_TRY_INFER:
    return "InstrTryInfer";
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
  }

  return "UNKNOWN";
}

static void
arenas_init(struct MirArenas *arenas)
{
  arena_init(&arenas->instr_arena, sizeof(union _MirInstr), ARENA_CHUNK_COUNT,
             (ArenaElemDtor)instr_dtor);
  arena_init(&arenas->type_arena, sizeof(MirType), ARENA_CHUNK_COUNT, (ArenaElemDtor)type_dtor);
  arena_init(&arenas->var_arena, sizeof(MirVar), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->fn_arena, sizeof(MirFn), ARENA_CHUNK_COUNT, NULL);
}

static void
arenas_terminate(struct MirArenas *arenas)
{
  arena_terminate(&arenas->instr_arena);
  arena_terminate(&arenas->type_arena);
  arena_terminate(&arenas->var_arena);
  arena_terminate(&arenas->fn_arena);
}

/* public */
static void
_type_to_str(char *buf, int32_t len, MirType *type)
{
#define append_buf(buf, len, str)                                                                  \
  {                                                                                                \
    const size_t filled = strlen(buf);                                                             \
    snprintf((buf) + filled, (len)-filled, "%s", str);                                             \
  }
  if (!buf) return;
  if (!type) {
    append_buf(buf, len, "?");
    return;
  }

  if (type->name) {
    append_buf(buf, len, type->name);
    return;
  }

  switch (type->kind) {
  case MIR_TYPE_INT:
    append_buf(buf, len, "integer");
    break;

  case MIR_TYPE_TYPE:
    append_buf(buf, len, "type");
    break;

  case MIR_TYPE_FN: {
    append_buf(buf, len, "fn(");

    MirType *tmp;
    BArray * args = type->data.fn.arg_types;
    if (args) {
      barray_foreach(args, tmp)
      {
        _type_to_str(buf, len, tmp);
        if (i < bo_array_size(args) - 1) append_buf(buf, len, ", ");
      }
    }

    append_buf(buf, len, ") ");

    _type_to_str(buf, len, type->data.fn.ret_type);
    break;
  }

  case MIR_TYPE_PTR: {
    append_buf(buf, len, "*");
    _type_to_str(buf, len, type->data.ptr.next);
    break;
  }

  case MIR_TYPE_ARRAY: {
    char str[35];
    sprintf(str, "[%lu]", type->data.array.len);
    append_buf(buf, len, str);

    _type_to_str(buf, len, type->data.array.elem_type);
    break;
  }

  default:
    bl_unimplemented;
  }
}

void
mir_type_to_str(char *buf, int32_t len, MirType *type)
{
  if (!buf || !len) return;
  buf[0] = '\0';
  _type_to_str(buf, len, type);
}

void
execute_entry_fn(Context *cnt)
{
  msg_log("\nexecuting 'main' in compile time...");
  if (!cnt->entry_fn) {
    msg_error("assembly '%s' has no entry function!", cnt->assembly->name);
    return;
  }

  /* tmp return value storage */
  MirConstValueData result = {0};
  if (exec_fn(cnt, cnt->entry_fn, NULL, &result)) {
    int64_t tmp = result.v_int;
    msg_log("execution finished with state: %lld\n", (long long)tmp);
  } else {
    msg_log("execution finished %s\n", cnt->exec_stack->aborted ? "with errors" : "without errors");
  }
}

void
execute_test_cases(Context *cnt)
{
  msg_log("\nexecuting test cases...");

  const size_t c      = bo_array_size(cnt->test_cases);
  int32_t      failed = 0;
  MirFn *      test_fn;
  int32_t      line;
  const char * file;

  barray_foreach(cnt->test_cases, test_fn)
  {
    cnt->exec_stack->aborted = false;
    assert(test_fn->is_test_case);
    exec_fn(cnt, test_fn, NULL, NULL);

    line = test_fn->node ? test_fn->node->src->line : -1;
    file = test_fn->node ? test_fn->node->src->unit->filepath : "?";

    msg_log("[ %s ] (%lu/%lu) %s:%d '%s'",
            cnt->exec_stack->aborted ? RED("FAILED") : GREEN("PASSED"), i + 1, c, file, line,
            test_fn->test_case_desc);

    if (cnt->exec_stack->aborted) ++failed;
  }

  msg_log("testing done, %d of %zu failed\n", failed, c);
}

void
init_buildins(Context *cnt)
{
  { // TYPES
    uint64_t tmp;
    cnt->buildin_types.table = bo_htbl_new(sizeof(BuildinType), _BUILDIN_TYPE_COUNT);
    for (int32_t i = 0; i < _BUILDIN_TYPE_COUNT; ++i) {
      tmp = bo_hash_from_str(buildin_type_names[i]);
      bo_htbl_insert(cnt->buildin_types.table, tmp, i);
    }

    cnt->buildin_types.entry_type = create_type_type(cnt);
    cnt->buildin_types.entry_void = create_type_void(cnt);

    cnt->buildin_types.entry_s8 =
        create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_S8], 8, true);
    cnt->buildin_types.entry_s16 =
        create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_S16], 16, true);
    cnt->buildin_types.entry_s32 =
        create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_S32], 32, true);
    cnt->buildin_types.entry_s64 =
        create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_S64], 64, true);

    cnt->buildin_types.entry_u8 =
        create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_U8], 8, false);
    cnt->buildin_types.entry_u16 =
        create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_U16], 16, false);
    cnt->buildin_types.entry_u32 =
        create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_U32], 32, false);
    cnt->buildin_types.entry_u64 =
        create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_U64], 64, false);
    cnt->buildin_types.entry_usize =
        create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_USIZE], 64, false);

    cnt->buildin_types.entry_bool = create_type_bool(cnt);

    cnt->buildin_types.entry_f32 = create_type_real(cnt, buildin_type_names[BUILDIN_TYPE_F32], 32);
    cnt->buildin_types.entry_f64 = create_type_real(cnt, buildin_type_names[BUILDIN_TYPE_F64], 64);

    cnt->buildin_types.entry_u8_ptr = create_type_ptr(cnt, cnt->buildin_types.entry_u8);

    cnt->buildin_types.entry_resolve_type_fn =
        create_type_fn(cnt, cnt->buildin_types.entry_type, NULL);

    cnt->buildin_types.entry_test_case_fn =
        create_type_fn(cnt, cnt->buildin_types.entry_void, NULL);
  }

  { // SPECIAL
    for (int32_t i = 0; i < _BUILDIN_SPEC_COUNT; ++i) {
      buildin_spec_hashes[i] = bo_hash_from_str(buildin_spec_names[i]);
    }
  }
}

int
init_dl(Context *cnt)
{
  /* load only current executed workspace */
  DLLib *lib = dlLoadLibrary(NULL);
  if (!lib) {
    msg_error("unable to load library");
    return ERR_LIB_NOT_FOUND;
  }

  DCCallVM *vm = dcNewCallVM(4096);
  dcMode(vm, DC_CALL_C_DEFAULT);
  cnt->dl.lib = lib;
  cnt->dl.vm  = vm;
  return NO_ERR;
}

void
terminate_dl(Context *cnt)
{
  dcFree(cnt->dl.vm);
  dlFreeLibrary(cnt->dl.lib);
}

MirModule *
mir_new_module(const char *name)
{
  MirModule *tmp = bl_malloc(sizeof(MirModule));
  if (!tmp) bl_abort("bad alloc");

  arenas_init(&tmp->arenas);

  /* init LLVM */
  char *triple    = LLVMGetDefaultTargetTriple();
  char *cpu       = "";
  char *features  = "";
  char *error_msg = NULL;

  msg_log("target: %s", triple);

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
  cnt.builder       = builder;
  cnt.assembly      = assembly;
  cnt.module        = assembly->mir_module;
  cnt.verbose_pre   = (bool)(builder->flags & BUILDER_VERBOSE_MIR_PRE);
  cnt.verbose_post  = (bool)(builder->flags & BUILDER_VERBOSE_MIR_POST);
  cnt.analyze_stack = bo_array_new(sizeof(MirInstr *));
  cnt.test_cases    = bo_array_new(sizeof(MirFn *));

  cnt.exec_stack = exec_new_stack(DEFAULT_EXEC_FRAME_STACK_SIZE);

#if BL_DEBUG
  cnt._debug_stack = (int64_t(*)[150])(cnt.exec_stack->top_ptr);
#endif

  init_buildins(&cnt);

  bo_array_reserve(cnt.analyze_stack, 1024);

  int32_t error = init_dl(&cnt);
  if (error != NO_ERR) return;

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    ast(&cnt, unit->ast);
  }

  if (!builder->errorc) {
    analyze(&cnt);
  }

  if (!builder->errorc && builder->flags & BUILDER_RUN_TESTS) execute_test_cases(&cnt);
  if (!builder->errorc && builder->flags & BUILDER_RUN) execute_entry_fn(&cnt);

  bo_unref(cnt.buildin_types.table);
  bo_unref(cnt.analyze_stack);
  bo_unref(cnt.test_cases);

  terminate_dl(&cnt);
  exec_delete_stack(cnt.exec_stack);
}
