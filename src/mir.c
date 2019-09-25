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

#include "mir.h"
#include "ast.h"
#include "builder.h"
#include "common.h"
#include "llvm_di.h"
#include "mir_printer.h"
#include "unit.h"

// Constants
// clang-format off
#define ARENA_CHUNK_COUNT               512
#define ANALYZE_TABLE_SIZE              8192 
#define TEST_CASE_FN_NAME               ".test"
#define RESOLVE_TYPE_FN_NAME            ".type"
#define INIT_VALUE_FN_NAME              ".init"
#define IMPL_FN_NAME                    ".impl"
#define IMPL_VARGS_TMP_ARR              ".vargs.arr"
#define IMPL_VARGS_TMP                  ".vargs"
#define IMPL_ANY_TMP                    ".any"
#define IMPL_ANY_EXPR_TMP               ".any.expr"
#define IMPL_COMPOUND_TMP               ".compound"
#define IMPL_RTTI_ENTRY                 ".rtti"
#define IMPL_RET_TMP                    ".ret"
#define NO_REF_COUNTING                 -1
#define VERBOSE_ANALYZE                 false
// clang-format on

#define ANALYZE_RESULT(_state, _waiting_for)                                                       \
	(AnalyzeResult)                                                                            \
	{                                                                                          \
		.state = ANALYZE_##_state, .waiting_for = (_waiting_for)                           \
	}

#define CREATE_INSTR(_cnt, _kind, _node, _t) ((_t)_create_instr((_cnt), (_kind), (_node)))

union _MirInstr {
	MirInstrBlock         block;
	MirInstrDeclVar       var;
	MirInstrDeclMember    member;
	MirInstrDeclVariant   variant;
	MirInstrDeclArg       decl_arg;
	MirInstrConst         cnst;
	MirInstrLoad          load;
	MirInstrStore         store;
	MirInstrRet           ret;
	MirInstrBinop         binop;
	MirInstrFnProto       fn_proto;
	MirInstrDeclRef       decl_ref;
	MirInstrDeclDirectRef decl_direct_ref;
	MirInstrCall          call;
	MirInstrUnreachable   unreachable;
	MirInstrCondBr        cond_br;
	MirInstrBr            br;
	MirInstrUnop          unop;
	MirInstrArg           arg;
	MirInstrElemPtr       elem_ptr;
	MirInstrMemberPtr     member_ptr;
	MirInstrAddrOf        addrof;
	MirInstrTypeArray     type_array;
	MirInstrTypeSlice     type_slice;
	MirInstrTypeVArgs     type_vargs;
	MirInstrTypePtr       type_ptr;
	MirInstrTypeStruct    type_struct;
	MirInstrTypeFn        type_fn;
	MirInstrTypeEnum      type_enum;
	MirInstrCast          cast;
	MirInstrSizeof        szof;
	MirInstrAlignof       alof;
	MirInstrCompound      init;
	MirInstrVArgs         vargs;
	MirInstrTypeInfo      type_info;
	MirInstrPhi           phi;
	MirInstrToAny         toany;
};

SmallArrayType(LLVMType, LLVMTypeRef, 8);
SmallArrayType(LLVMMetadata, LLVMMetadataRef, 16);
SmallArrayType(DeferStack, Ast *, 64);
SmallArrayType(InstrPtr64, MirInstr *, 64);
SmallArrayType(String, const char *, 64);

typedef struct {
	VM          vm;
	Assembly *  assembly;
	BArray *    test_cases;
	BString *   tmp_sh;
	BHashTable *type_table;
	MirFn *     entry_fn;
	bool        debug_mode;

	/* AST -> MIR generation */
	struct {
		SmallArray_DeferStack defer_stack;
		MirInstrBlock *       current_block;
		MirInstrBlock *       break_block;
		MirInstrBlock *       exit_block;
		MirInstrBlock *       continue_block;
		ID *                  current_entity_id; /* Sometimes used for named structures */
	} ast;

	/* Analyze MIR generated from AST */
	struct {
		/* Instructions waiting for analyze. */
		BList *queue;

		/* Hash table of arrays. Hash is ID of symbol and array contains queue
		 * of waiting instructions (DeclRefs). */
		BHashTable *waiting;
		bool        verbose_pre;
		bool        verbose_post;

		BHashTable *     RTTI_entry_types;
		LLVMDIBuilderRef llvm_di_builder;
	} analyze;

	/* Builtins */
	struct BuiltinTypes {
		/* PROVIDED BEGIN */
		MirType *t_type;
		MirType *t_s8;
		MirType *t_s16;
		MirType *t_s32;
		MirType *t_s64;
		MirType *t_u8;
		MirType *t_u16;
		MirType *t_u32;
		MirType *t_u64;
		MirType *t_usize;
		MirType *t_bool;
		MirType *t_f32;
		MirType *t_f64;
		MirType *t_string;
		/* PROVIDED END */

		/* OTHER BEGIN */
		MirType *t_void;
		MirType *t_u8_ptr;
		MirType *t_string_ptr;
		MirType *t_string_slice;
		MirType *t_resolve_type_fn;
		MirType *t_test_case_fn;
		MirType *t_TypeInfo_ptr;
		MirType *t_TypeInfo_slice;
		MirType *t_TypeInfoStructMembers_slice;
		MirType *t_TypeInfoEnumVariants_slice;
		/* OTHER END */

		/* Cache scope containing '#compiler' flagged symbols.  */
		Scope *cache;
	} builtin_types;
} Context;

typedef enum {
	/* Analyze pass failed. */
	ANALYZE_FAILED = 0,

	/* Analyze pass passed. */
	ANALYZE_PASSED = 1,

	/* Analyze pass cannot be done because some of sub-parts has not been
	   analyzed yet and probably needs to be executed during analyze pass. In
	   such case we push analyzed instruction at the end of analyze queue. */
	ANALYZE_POSTPONE = 2,

	/* In this case AnalyzeResult will contain hash of desired symbol which be satisfied later,
	   instruction is pushed into waiting table. */
	ANALYZE_WAITING = 3,

	/* Rarely used state, instruction was not analyzed but analyzer continue to the following
	   instruction. */
	ANALYZE_INCOMPLETE = 4,
} AnalyzeState;

typedef struct {
	AnalyzeState state;
	u64          waiting_for;
} AnalyzeResult;

typedef enum {
	ANALYZE_STAGE_BREAK,
	ANALYZE_STAGE_CONTINUE,
	ANALYZE_STAGE_FAILED,
} AnalyzeStageState;

typedef AnalyzeStageState (*AnalyzeStageFn)(Context *, MirInstr **, MirType *);

typedef struct {
	s32            count;
	AnalyzeStageFn stages[];
} AnalyzeSlotConfig;

/* Ids of builtin symbols, hash is calculated inside init_builtins function
 * later. */
// clang-format off
static ID builtin_ids[_MIR_BUILTIN_ID_COUNT] = {
    {.str = "type",                  .hash = 0},
    {.str = "s8",                    .hash = 0},
    {.str = "s16",                   .hash = 0},
    {.str = "s32",                   .hash = 0},
    {.str = "s64",                   .hash = 0},
    {.str = "u8",                    .hash = 0},
    {.str = "u16",                   .hash = 0},
    {.str = "u32",                   .hash = 0},
    {.str = "u64",                   .hash = 0},
    {.str = "usize",                 .hash = 0},
    {.str = "bool",                  .hash = 0},
    {.str = "f32",                   .hash = 0},
    {.str = "f64",                   .hash = 0},
    {.str = "void",                  .hash = 0},
    {.str = "string",                .hash = 0},
    {.str = "null_t",                .hash = 0},
    {.str = "main",                  .hash = 0},
    {.str = "len",                   .hash = 0},
    {.str = "ptr",                   .hash = 0},
    {.str = "Any",                   .hash = 0},
    {.str = "TypeKind",              .hash = 0},
    {.str = "TypeInfo",              .hash = 0},
    {.str = "TypeInfoType",          .hash = 0},
    {.str = "TypeInfoVoid",          .hash = 0},
    {.str = "TypeInfoInt",           .hash = 0},
    {.str = "TypeInfoReal",          .hash = 0},
    {.str = "TypeInfoFn",            .hash = 0},
    {.str = "TypeInfoPtr",           .hash = 0},
    {.str = "TypeInfoBool",          .hash = 0},
    {.str = "TypeInfoArray",         .hash = 0},
    {.str = "TypeInfoStruct",        .hash = 0},
    {.str = "TypeInfoEnum",          .hash = 0},
    {.str = "TypeInfoNull",          .hash = 0},
    {.str = "TypeInfoString",        .hash = 0},
    {.str = "TypeInfoSlice",         .hash = 0},
    {.str = "TypeInfoVArgs",         .hash = 0},
    {.str = "TypeInfoStructMember",  .hash = 0},
    {.str = "TypeInfoEnumVariant",   .hash = 0},
};
// clang-format on

static void
fn_dtor(MirFn **fn)
{
	dcbFreeCallback((*fn)->dyncall.extern_callback_handle);
}

/* FW decls */
static void
init_builtins(Context *cnt);

static void
execute_entry_fn(Context *cnt);

static void
execute_test_cases(Context *cnt);

static ScopeEntry *
register_symbol(Context *cnt, Ast *node, ID *id, Scope *scope, bool is_builtin, bool enable_groups);

static void
cache_builtin(Context *cnt, ScopeEntry *entry);

static MirType *
lookup_builtin(Context *cnt, MirBuiltinIdKind kind);

/* ctors */
static bool
create_type(Context *cnt, MirType **out_type, const char *sh);

static MirType *
create_type_type(Context *cnt);

static MirType *
create_type_null(Context *cnt, MirType *base_type);

static MirType *
create_type_void(Context *cnt);

static MirType *
create_type_bool(Context *cnt);

static MirType *
create_type_int(Context *cnt, ID *id, s32 bitcount, bool is_signed);

static MirType *
create_type_real(Context *cnt, ID *id, s32 bitcount);

static MirType *
create_type_ptr(Context *cnt, MirType *src_type);

static MirType *
create_type_fn(Context *cnt, ID *id, MirType *ret_type, SmallArray_ArgPtr *args, bool is_vargs);

static MirType *
create_type_array(Context *cnt, MirType *elem_type, s64 len);

static MirType *
create_type_struct(Context *             cnt,
                   MirTypeKind           kind,
                   ID *                  id,
                   Scope *               scope,
                   SmallArray_MemberPtr *members, /* MirMember */
                   bool                  is_packed);

static MirType *
create_type_enum(Context *              cnt,
                 ID *                   id,
                 Scope *                scope,
                 MirType *              base_type,
                 SmallArray_VariantPtr *variants);

MirType *
create_type_struct_special(Context *cnt, MirTypeKind kind, ID *id, MirType *elem_ptr_type);

static void
init_llvm_type_int(Context *cnt, MirType *type);

static void
init_llvm_type_real(Context *cnt, MirType *type);

static void
init_llvm_type_ptr(Context *cnt, MirType *type);

static void
init_llvm_type_null(Context *cnt, MirType *type);

static void
init_llvm_type_void(Context *cnt, MirType *type);

static void
init_llvm_type_bool(Context *cnt, MirType *type);

static void
init_llvm_type_fn(Context *cnt, MirType *type);

static void
init_llvm_type_array(Context *cnt, MirType *type);

static void
init_llvm_type_struct(Context *cnt, MirType *type);

static void
init_llvm_type_enum(Context *cnt, MirType *type);

static void
init_llvm_DI_scope(Context *cnt, Scope *scope);

static MirVar *
create_var(Context *cnt,
           Ast *    decl_node,
           Scope *  scope,
           ID *     id,
           MirType *alloc_type,
           bool     is_mutable,
           bool     is_in_gscope,
           u32      flags);

static MirVar *
create_var_impl(Context *   cnt,
                const char *name,
                MirType *   alloc_type,
                bool        is_mutable,
                bool        is_in_gscope,
                bool        comptime);

static MirFn *
create_fn(Context *        cnt,
          Ast *            node,
          ID *             id,
          const char *     linkage_name,
          s32              flags,
          MirInstrFnProto *prototype,
          bool             emit_llvm,
          bool             is_in_gscope);

static MirMember *
create_member(Context *cnt, Ast *node, ID *id, Scope *scope, s64 index, MirType *type);

static MirArg *
create_arg(Context *cnt, Ast *node, ID *id, Scope *scope, MirType *type);

static MirVariant *
create_variant(Context *cnt, ID *id, Scope *scope, MirConstValue *value);

static MirConstValue *
create_const_value(Context *cnt, MirType *type);

static MirConstValue *
init_or_create_const_integer(Context *cnt, MirConstValue *v, MirType *type, u64 i);

static MirConstValue *
init_or_create_const_bool(Context *cnt, MirConstValue *v, bool b);

static MirConstValue *
init_or_create_const_var_ptr(Context *cnt, MirConstValue *v, MirType *type, MirVar *var);

static MirConstValue *
init_or_create_const_array(Context *                 cnt,
                           MirConstValue *           v,
                           MirType *                 elem_type,
                           SmallArray_ConstValuePtr *elems);

static MirConstValue *
init_or_create_const_struct(Context *                 cnt,
                            MirConstValue *           v,
                            MirType *                 type,
                            SmallArray_ConstValuePtr *members);

static MirConstValue *
init_or_create_const_string(Context *cnt, MirConstValue *v, const char *str);

static MirInstrBlock *
append_block(Context *cnt, MirFn *fn, const char *name);

/* instructions */
static void
maybe_mark_as_unrechable(MirInstrBlock *block, MirInstr *instr);

static void
append_current_block(Context *cnt, MirInstr *instr);

static MirInstr *
insert_instr_load(Context *cnt, MirInstr *src);

static MirInstr *
insert_instr_cast(Context *cnt, MirInstr *src, MirType *to_type);

static MirInstr *
insert_instr_toany(Context *cnt, MirInstr *expr);

static MirCastOp
get_cast_op(MirType *from, MirType *to);

static MirInstr *
_create_instr(Context *cnt, MirInstrKind kind, Ast *node);

static MirInstr *
create_instr_call_comptime(Context *cnt, Ast *node, MirInstr *fn);

static MirInstr *
append_instr_arg(Context *cnt, Ast *node, unsigned i);

static MirInstr *
append_instr_phi(Context *cnt, Ast *node);

static MirInstr *
append_instr_compound(Context *cnt, Ast *node, MirInstr *type, SmallArray_InstrPtr *values);

static MirInstr *
append_instr_cast(Context *cnt, Ast *node, MirInstr *type, MirInstr *next);

static MirInstr *
append_instr_sizeof(Context *cnt, Ast *node, MirInstr *expr);

static MirInstr *
create_instr_type_info(Context *cnt, Ast *node, MirInstr *expr);

static MirInstr *
append_instr_type_info(Context *cnt, Ast *node, MirInstr *expr);

static MirInstr *
append_instr_alignof(Context *cnt, Ast *node, MirInstr *expr);

static MirInstr *
create_instr_elem_ptr(Context * cnt,
                      Ast *     node,
                      MirInstr *arr_ptr,
                      MirInstr *index,
                      bool      target_is_slice);

static MirInstr *
append_instr_elem_ptr(Context * cnt,
                      Ast *     node,
                      MirInstr *arr_ptr,
                      MirInstr *index,
                      bool      target_is_slice);

static MirInstr *
create_instr_member_ptr(Context *        cnt,
                        Ast *            node,
                        MirInstr *       target_ptr,
                        Ast *            member_ident,
                        ScopeEntry *     scope_entry,
                        MirBuiltinIdKind builtin_id);

static MirInstr *
append_instr_member_ptr(Context *        cnt,
                        Ast *            node,
                        MirInstr *       target_ptr,
                        Ast *            member_ident,
                        ScopeEntry *     scope_entry,
                        MirBuiltinIdKind builtin_id);

static MirInstr *
append_instr_cond_br(Context *      cnt,
                     Ast *          node,
                     MirInstr *     cond,
                     MirInstrBlock *then_block,
                     MirInstrBlock *else_block);

static MirInstr *
append_instr_br(Context *cnt, Ast *node, MirInstrBlock *then_block);

static MirInstr *
append_instr_load(Context *cnt, Ast *node, MirInstr *src);

static MirInstr *
append_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, SmallArray_InstrPtr *args);

static MirInstr *
append_instr_type_struct(Context *            cnt,
                         Ast *                node,
                         ID *                 id,
                         Scope *              scope,
                         SmallArray_InstrPtr *members,
                         bool                 is_packed);

static MirInstr *
append_instr_type_enum(Context *            cnt,
                       Ast *                node,
                       ID *                 id,
                       Scope *              scope,
                       SmallArray_InstrPtr *variants,
                       MirInstr *           base_type);

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
append_instr_fn_proto(Context * cnt,
                      Ast *     node,
                      MirInstr *type,
                      MirInstr *user_type,
                      bool      schedule_analyze);

static MirInstr *
append_instr_decl_ref(Context *   cnt,
                      Ast *       node,
                      Unit *      parent_unit,
                      ID *        rid,
                      Scope *     scope,
                      ScopeEntry *scope_entry);

static MirInstr *
append_instr_decl_direct_ref(Context *cnt, MirInstr *ref);

static MirInstr *
append_instr_call(Context *cnt, Ast *node, MirInstr *callee, SmallArray_InstrPtr *args);

static MirInstr *
append_instr_decl_var(Context * cnt,
                      Ast *     node,
                      MirInstr *type,
                      MirInstr *init,
                      bool      is_mutable,
                      bool      is_in_gscope,
                      s32       order, /* -1 of none */
                      u32       flags);

static MirInstr *
append_instr_decl_var_impl(Context *   cnt,
                           const char *name,
                           MirInstr *  type,
                           MirInstr *  init,
                           bool        is_mutable,
                           bool        is_in_gscope,
                           s32         order, /* -1 of none */
                           u32         flags);

static MirInstr *
append_instr_decl_member(Context *cnt, Ast *node, MirInstr *type);

static MirInstr *
append_instr_decl_arg(Context *cnt, Ast *node, MirInstr *type);

static MirInstr *
append_instr_decl_variant(Context *cnt, Ast *node, MirInstr *value);

static MirInstr *
create_instr_const_int(Context *cnt, Ast *node, MirType *type, u64 val);

static MirInstr *
append_instr_const_int(Context *cnt, Ast *node, MirType *type, u64 val);

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
append_instr_ret(Context *cnt, Ast *node, MirInstr *value, bool infer_type);

static MirInstr *
append_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest);

static MirInstr *
append_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op);

static MirInstr *
append_instr_unop(Context *cnt, Ast *node, MirInstr *instr, UnopKind op);

static MirInstr *
append_instr_unrecheable(Context *cnt, Ast *node);

static MirInstr *
create_instr_addrof(Context *cnt, Ast *node, MirInstr *src);

static MirInstr *
append_instr_addrof(Context *cnt, Ast *node, MirInstr *src);

/*
 * This will erase whole instruction tree of instruction with ref_count == 0. When force is set
 * ref_count is ignored.
 */
static void
erase_instr_tree(MirInstr *instr);

static MirInstr *
create_instr_vargs_impl(Context *cnt, MirType *type, SmallArray_InstrPtr *values);

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
ast_defer_block(Context *cnt, Ast *block, bool whole_tree);

static void
ast_block(Context *cnt, Ast *block);

static void
ast_stmt_if(Context *cnt, Ast *stmt_if);

static void
ast_stmt_return(Context *cnt, Ast *ret);

static void
ast_stmt_defer(Context *cnt, Ast *defer);

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
ast_decl_variant(Context *cnt, Ast *variant);

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
ast_type_enum(Context *cnt, Ast *type_enum);

static MirInstr *
ast_expr_line(Context *cnt, Ast *line);

static MirInstr *
ast_expr_file(Context *cnt, Ast *file);

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
ast_expr_lit_fn(Context *cnt, Ast *lit_fn, Ast *decl_node, bool is_in_gscope, u32 flags);

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

/* analyze */
static void
reduce_instr(Context *cnt, MirInstr *instr);

static AnalyzeResult
analyze_instr(Context *cnt, MirInstr *instr);

static AnalyzeState
analyze_slot(Context *cnt, const AnalyzeSlotConfig *conf, MirInstr **input, MirType *slot_type);

/* Insert load instruction if needed. */
static AnalyzeStageState
analyze_stage_load(Context *cnt, MirInstr **input, MirType *slot_type);

/* Set null type constant if needed. */
static AnalyzeStageState
analyze_stage_set_null(Context *cnt, MirInstr **input, MirType *slot_type);

/* Set auto cast desired type if needed. */
static AnalyzeStageState
analyze_stage_set_auto(Context *cnt, MirInstr **input, MirType *slot_type);

/* Implicit conversion of input value to any. */
static AnalyzeStageState
analyze_stage_toany(Context *cnt, MirInstr **input, MirType *slot_type);

static AnalyzeStageState
analyze_stage_set_volatile_expr(Context *cnt, MirInstr **input, MirType *slot_type);

/* Do implicit cast if possible. */
static AnalyzeStageState
analyze_stage_implicit_cast(Context *cnt, MirInstr **input, MirType *slot_type);

static AnalyzeStageState
analyze_stage_report_type_mismatch(Context *cnt, MirInstr **input, MirType *slot_type);

static const AnalyzeSlotConfig analyze_slot_conf_basic = {.count  = 1,
                                                          .stages = {analyze_stage_load}};

static const AnalyzeSlotConfig analyze_slot_conf_default = {.count  = 6,
                                                            .stages = {
                                                                analyze_stage_set_volatile_expr,
                                                                analyze_stage_set_null,
                                                                analyze_stage_set_auto,
                                                                analyze_stage_load,
                                                                analyze_stage_implicit_cast,
                                                                analyze_stage_report_type_mismatch,
                                                            }};

static const AnalyzeSlotConfig analyze_slot_conf_full = {.count  = 7,
                                                         .stages = {
                                                             analyze_stage_set_volatile_expr,
                                                             analyze_stage_set_null,
                                                             analyze_stage_set_auto,
                                                             analyze_stage_toany,
                                                             analyze_stage_load,
                                                             analyze_stage_implicit_cast,
                                                             analyze_stage_report_type_mismatch,
                                                         }};

/* This function produce analyze of implicit call to the type resolver function in MIR and set
 * out_type when analyze passed without problems. When analyze does not pass postpone is returned
 * and out_type stay unchanged.*/
static AnalyzeResult
analyze_resolve_type(Context *cnt, MirInstr *resolver_call, MirType **out_type);

static AnalyzeResult
analyze_instr_compound(Context *cnt, MirInstrCompound *cmp);

static AnalyzeResult
analyze_instr_phi(Context *cnt, MirInstrPhi *phi);

static AnalyzeResult
analyze_instr_toany(Context *cnt, MirInstrToAny *toany);

static AnalyzeResult
analyze_instr_vargs(Context *cnt, MirInstrVArgs *vargs);

static AnalyzeResult
analyze_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static AnalyzeResult
analyze_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr);

static AnalyzeResult
analyze_instr_addrof(Context *cnt, MirInstrAddrOf *addrof);

static AnalyzeResult
analyze_instr_block(Context *cnt, MirInstrBlock *block);

static AnalyzeResult
analyze_instr_ret(Context *cnt, MirInstrRet *ret);

static AnalyzeResult
analyze_instr_arg(Context *cnt, MirInstrArg *arg);

static AnalyzeResult
analyze_instr_unop(Context *cnt, MirInstrUnop *unop);

static AnalyzeResult
analyze_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static AnalyzeResult
analyze_instr_cond_br(Context *cnt, MirInstrCondBr *br);

static AnalyzeResult
analyze_instr_br(Context *cnt, MirInstrBr *br);

static AnalyzeResult
analyze_instr_load(Context *cnt, MirInstrLoad *load);

static AnalyzeResult
analyze_instr_store(Context *cnt, MirInstrStore *store);

static AnalyzeResult
analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);

static AnalyzeResult
analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn);

static AnalyzeResult
analyze_instr_type_struct(Context *cnt, MirInstrTypeStruct *type_struct);

static AnalyzeResult
analyze_instr_type_slice(Context *cnt, MirInstrTypeSlice *type_slice);

static AnalyzeResult
analyze_instr_type_vargs(Context *cnt, MirInstrTypeVArgs *type_vargs);

static AnalyzeResult
analyze_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr);

static AnalyzeResult
analyze_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr);

static AnalyzeResult
analyze_instr_type_enum(Context *cnt, MirInstrTypeEnum *type_enum);

static AnalyzeResult
analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *decl);

static AnalyzeResult
analyze_instr_decl_member(Context *cnt, MirInstrDeclMember *decl);

static AnalyzeResult
analyze_instr_decl_variant(Context *cnt, MirInstrDeclVariant *variant_instr);

static AnalyzeResult
analyze_instr_decl_arg(Context *cnt, MirInstrDeclArg *decl);

static AnalyzeResult
analyze_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static AnalyzeResult
analyze_instr_decl_direct_ref(Context *cnt, MirInstrDeclDirectRef *ref);

static AnalyzeResult
analyze_instr_const(Context *cnt, MirInstrConst *cnst);

static AnalyzeResult
analyze_instr_call(Context *cnt, MirInstrCall *call);

static AnalyzeResult
analyze_instr_cast(Context *cnt, MirInstrCast *cast, bool analyze_op_only);

static AnalyzeResult
analyze_instr_sizeof(Context *cnt, MirInstrSizeof *szof);

static AnalyzeResult
analyze_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info);

static AnalyzeResult
analyze_instr_alignof(Context *cnt, MirInstrAlignof *alof);

static AnalyzeResult
analyze_instr_binop(Context *cnt, MirInstrBinop *binop);

static void
analyze(Context *cnt);

static void
analyze_report_unresolved(Context *cnt);

static MirVar *
gen_RTTI(Context *cnt, MirType *type);

static MirConstValue *
gen_RTTI_base(Context *cnt, s32 kind, size_t size_bytes);

static MirVar *
gen_RTTI_empty(Context *cnt, MirType *type, MirType *rtti_type);

static MirVar *
gen_RTTI_int(Context *cnt, MirType *type);

static MirVar *
gen_RTTI_real(Context *cnt, MirType *type);

static MirVar *
gen_RTTI_ptr(Context *cnt, MirType *type);

static MirConstValue *
gen_RTTI_enum_variant(Context *cnt, MirVariant *variant);

static MirConstValue *
gen_RTTI_slice_of_enum_variants(Context *cnt, SmallArray_VariantPtr *variants);

static MirVar *
gen_RTTI_enum(Context *cnt, MirType *type);

static MirVar *
gen_RTTI_array(Context *cnt, MirType *type);

static MirConstValue *
gen_RTTI_slice_of_TypeInfo_ptr(Context *cnt, SmallArray_TypePtr *types);

static MirConstValue *
gen_RTTI_struct_member(Context *cnt, MirMember *member);

static MirConstValue *
gen_RTTI_slice_of_struct_members(Context *cnt, SmallArray_MemberPtr *members);

static MirVar *
gen_RTTI_struct(Context *cnt, MirType *type);

static MirVar *
gen_RTTI_fn(Context *cnt, MirType *type);

static void
gen_RTTI_types(Context *cnt);

/* INLINES */

/* Determinate if instruction has volatile type, that means we can change type of the value during
 * analyze pass as needed. This is used for constant integer literals. */
static inline bool
is_volatile_expr(MirInstr *instr)
{
	MirType *type = instr->value.type;

	if (!type) return false;
	if (type->kind != MIR_TYPE_INT) return false;

	switch (instr->kind) {
	case MIR_INSTR_CONST:
		/* Integer constant literals has always volatile type. */
		return true;
	case MIR_INSTR_UNOP:
		return ((MirInstrUnop *)instr)->volatile_type;
	case MIR_INSTR_BINOP:
		return ((MirInstrBinop *)instr)->volatile_type;
	default:
		return false;
	}
}

static inline bool
can_impl_cast(MirType *from, MirType *to)
{
	if (from->kind != to->kind) return false;
	if (from->kind != MIR_TYPE_INT) return false;
	// return true;
	// TODO: enable after correct type propagation of contants
	if (from->data.integer.is_signed != to->data.integer.is_signed) return false;

	const size_t fb = from->data.integer.bitcount;
	const size_t tb = to->data.integer.bitcount;

	if (fb > tb) return false;

	return true;
}

static inline MirFn *
get_callee(MirInstrCall *call)
{
	MirConstValue *callee_val = &call->callee->value;
	BL_ASSERT(callee_val->type && callee_val->type->kind == MIR_TYPE_FN);

	MirFn *fn = callee_val->data.v_ptr.data.fn;
	BL_ASSERT(fn);
	return fn;
}

static inline bool
type_cmp(MirType *first, MirType *second)
{
	BL_ASSERT(first && second);
	return first->id.hash == second->id.hash;
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
terminate_block(MirInstrBlock *block, MirInstr *terminator)
{
	BL_ASSERT(block);
	if (block->terminal) BL_ABORT("basic block '%s' already terminated!", block->name);
	block->terminal = terminator;
}

static inline bool
is_block_terminated(MirInstrBlock *block)
{
	return block->terminal;
}

static inline bool
is_current_block_terminated(Context *cnt)
{
	return get_current_block(cnt)->terminal;
}

static inline void
schedule_RTTI_generation(Context *cnt, MirType *type)
{
	if (!bo_htbl_has_key(cnt->analyze.RTTI_entry_types, (u64)type))
		bo_htbl_insert_empty(cnt->analyze.RTTI_entry_types, (u64)type);
}

static inline bool
is_allocated_object(MirInstr *instr)
{
	/* CLEANUP: is this good solution??? */
	/* CLEANUP: is this good solution??? */
	/* CLEANUP: is this good solution??? */
	if (instr->kind == MIR_INSTR_DECL_DIRECT_REF) return true;
	if (instr->kind == MIR_INSTR_DECL_REF) return true;
	if (instr->kind == MIR_INSTR_ELEM_PTR) return true;
	if (instr->kind == MIR_INSTR_MEMBER_PTR) return true;
	if (instr->kind == MIR_INSTR_FN_PROTO) return true;
	if (instr->kind == MIR_INSTR_COMPOUND) return true;
	if (instr->kind == MIR_INSTR_LOAD) return true;
	if (instr->kind == MIR_INSTR_TYPE_INFO) return true;

	return false;
}

static inline MirInstr *
mutate_instr(MirInstr *instr, MirInstrKind kind)
{
	BL_ASSERT(instr);
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
	BL_ASSERT(after && instr);

	MirInstrBlock *block = after->owner_block;
	instr->unrechable    = after->unrechable;

	instr->next = after->next;
	instr->prev = after;
	if (after->next) after->next->prev = instr;
	after->next = instr;

	instr->owner_block = block;
	if (block->last_instr == after) instr->owner_block->last_instr = instr;
}

static inline void
insert_instr_before(MirInstr *before, MirInstr *instr)
{
	BL_ASSERT(before && instr);

	MirInstrBlock *block = before->owner_block;
	instr->unrechable    = before->unrechable;

	instr->next = before;
	instr->prev = before->prev;
	if (before->prev) before->prev->next = instr;
	before->prev = instr;

	instr->owner_block = block;
	if (block->entry_instr == before) instr->owner_block->entry_instr = instr;
}

static inline void
push_into_gscope(Context *cnt, MirInstr *instr)
{
	BL_ASSERT(instr);
	instr->id = bo_array_size(cnt->assembly->MIR.global_instrs);
	bo_array_push_back(cnt->assembly->MIR.global_instrs, instr);
};

static inline void
analyze_push_back(Context *cnt, MirInstr *instr)
{
	BL_ASSERT(instr);
	bo_list_push_back(cnt->analyze.queue, instr);
}

static inline void
analyze_push_front(Context *cnt, MirInstr *instr)
{
	BL_ASSERT(instr);
	bo_list_push_front(cnt->analyze.queue, instr);
}

static inline void
analyze_notify_provided(Context *cnt, u64 hash)
{
	bo_iterator_t iter = bo_htbl_find(cnt->analyze.waiting, hash);
	bo_iterator_t end  = bo_htbl_end(cnt->analyze.waiting);
	if (bo_iterator_equal(&iter, &end)) return; /* No one is waiting for this... */

#if BL_DEBUG && VERBOSE_ANALYZE
	printf("Analyze: Notify '%llu'.\n", (unsigned long long)hash);
#endif

	BArray *wq = bo_htbl_iter_peek_value(cnt->analyze.waiting, &iter, BArray *);
	BL_ASSERT(wq);

	MirInstr *instr;
	BARRAY_FOREACH(wq, instr)
	{
		analyze_push_back(cnt, instr);
	}

	bo_htbl_erase(cnt->analyze.waiting, &iter);
}

static inline void
analyze_instr_rq(Context *cnt, MirInstr *instr)
{
	if (analyze_instr(cnt, instr).state != ANALYZE_PASSED)
		BL_WARNING("invalid analyze of compiler-generated instruction: %s",
		           mir_instr_name(instr));
}

static inline const char *
gen_uq_name(const char *prefix)
{
	static s32 ui = 0;
	/* RACECOND */
	/* RACECOND */
	/* RACECOND */
	BString *s = builder_create_cached_str();

	bo_string_append(s, prefix);
	char ui_str[22];
	sprintf(ui_str, ".%i", ui++);
	bo_string_append(s, ui_str);
	return bo_string_get(s);
}

static inline bool
is_builtin(Ast *ident, MirBuiltinIdKind kind)
{
	if (!ident) return false;
	BL_ASSERT(ident->kind == AST_IDENT);
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

static inline void
error_types(MirType *from, MirType *to, Ast *loc, const char *msg)
{
	BL_ASSERT(from && to);
	if (!msg) msg = "No implicit cast for type '%s' and '%s'.";

	char tmp_from[256];
	char tmp_to[256];
	mir_type_to_str(tmp_from, 256, from, true);
	mir_type_to_str(tmp_to, 256, to, true);

	builder_msg(BUILDER_MSG_ERROR,
	            ERR_INVALID_TYPE,
	            loc->location,
	            BUILDER_CUR_WORD,
	            msg,
	            tmp_from,
	            tmp_to);
}

static inline void
commit_fn(Context *cnt, MirFn *fn)
{
	ID *id = fn->id;
	BL_ASSERT(id);

	ScopeEntry *entry = scope_lookup(fn->decl_node->owner_scope, id, true, false);
	BL_ASSERT(entry && "cannot commit unregistred function");

	entry->kind    = SCOPE_ENTRY_FN;
	entry->data.fn = fn;

	analyze_notify_provided(cnt, id->hash);
}

static inline void
commit_variant(Context *cnt, MirVariant *v)
{
	ID *id = v->id;
	BL_ASSERT(id);

	ScopeEntry *entry = scope_lookup(v->decl_scope, id, false, true);
	BL_ASSERT(entry && "cannot commit unregistred variant");

	entry->kind         = SCOPE_ENTRY_VARIANT;
	entry->data.variant = v;
}

static inline void
commit_member(Context *cnt, MirMember *member)
{
	ID *id = member->id;
	BL_ASSERT(id);

	ScopeEntry *entry = scope_lookup(member->decl_scope, id, false, true);
	BL_ASSERT(entry && "cannot commit unregistred member");

	entry->kind        = SCOPE_ENTRY_MEMBER;
	entry->data.member = member;
}

static inline void
commit_var(Context *cnt, MirVar *var)
{
	ID *id = var->id;
	BL_ASSERT(id);

	ScopeEntry *entry = scope_lookup(var->decl_scope, id, true, false);
	BL_ASSERT(entry && "cannot commit unregistred var");

	entry->kind     = SCOPE_ENTRY_VAR;
	entry->data.var = var;

	if (var->is_in_gscope) analyze_notify_provided(cnt, id->hash);
}

/*
 * Provide builtin type. Register & commit.
 */
static inline void
provide_builtin_type(Context *cnt, MirType *type)
{
	ScopeEntry *entry =
	    register_symbol(cnt, NULL, type->user_id, cnt->assembly->gscope, true, false);
	if (!entry) return;

	entry->kind      = SCOPE_ENTRY_TYPE;
	entry->data.type = type;
}

static inline void
provide_builtin_member(Context *cnt, Scope *scope, MirMember *member)
{
	ScopeEntry *entry = register_symbol(cnt, NULL, member->id, scope, false, false);
	if (!entry) return;

	entry->kind        = SCOPE_ENTRY_MEMBER;
	entry->data.member = member;
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

static inline void
phi_add_income(MirInstrPhi *phi, MirInstr *value, MirInstrBlock *block)
{
	BL_ASSERT(phi && value && block);
	ref_instr(value);
	ref_instr(&block->base);

	sa_push_InstrPtr(phi->incoming_values, value);
	sa_push_InstrPtr(phi->incoming_blocks, &block->base);
}

static inline bool
is_load_needed(MirInstr *instr)
{
	if (!instr) return false;
	if (!mir_is_pointer_type(instr->value.type)) return false;

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

	default:
		break;
	}

	return true;
}

static inline bool
is_to_any_needed(Context *cnt, MirInstr *src, MirType *dest_type)
{
	if (!dest_type || !src) return false;
	MirType *any_type = lookup_builtin(cnt, MIR_BUILTIN_ID_ANY);

	if (dest_type != any_type) return false;

	if (is_load_needed(src)) {
		MirType *src_type = src->value.type;
		if (mir_deref_type(src_type) == any_type) return false;
	}

	return true;
}

/* string hash functions for types */
static inline const char *
sh_type_null(Context *cnt, MirType *base_type)
{
	BL_ASSERT(base_type->id.str);
	BString *tmp = cnt->tmp_sh;
	bo_string_clear(tmp);
	bo_string_append(tmp, "n.");
	bo_string_append(tmp, base_type->id.str);
	return bo_string_get(tmp);
}

static inline const char *
sh_type_ptr(Context *cnt, MirType *src_type)
{
	BL_ASSERT(src_type->id.str);
	BString *tmp = cnt->tmp_sh;
	bo_string_clear(tmp);
	bo_string_append(tmp, "p.");
	bo_string_append(tmp, src_type->id.str);
	return bo_string_get(tmp);
}

static inline const char *
sh_type_fn(Context *cnt, MirType *ret_type, SmallArray_ArgPtr *args, bool is_vargs)
{
	// BL_ASSERT(src_type->id.str);
	BString *tmp = cnt->tmp_sh;
	bo_string_clear(tmp);

	bo_string_append(tmp, "f.(");

	/* append all arg types isd */
	if (args) {
		MirArg *arg;
		SARRAY_FOREACH(args, arg)
		{
			BL_ASSERT(arg->type->id.str);
			bo_string_append(tmp, arg->type->id.str);

			if (i != args->size - 1) bo_string_append(tmp, ",");
		}
	}

	bo_string_append(tmp, ")");

	if (ret_type) {
		BL_ASSERT(ret_type->id.str);
		bo_string_append(tmp, ret_type->id.str);
	} else {
		/* implicit return void */
		bo_string_append(tmp, cnt->builtin_types.t_void->id.str);
	}

	return bo_string_get(tmp);
}

static inline const char *
sh_type_arr(Context *cnt, MirType *elem_type, size_t len)
{
	BL_ASSERT(elem_type->id.str);
	BString *tmp = cnt->tmp_sh;
	bo_string_clear(tmp);

	char ui_str[21];
	sprintf(ui_str, "%llu", (unsigned long long)len);

	bo_string_append(tmp, ui_str);
	bo_string_append(tmp, ".");
	bo_string_append(tmp, elem_type->id.str);
	return bo_string_get(tmp);
}

static inline const char *
sh_type_struct(Context *             cnt,
               MirTypeKind           kind,
               ID *                  id,
               SmallArray_MemberPtr *members,
               bool                  is_packed)
{
	BL_ASSERT(!is_packed);
	BString *tmp = cnt->tmp_sh;
	bo_string_clear(tmp);

	switch (kind) {
	case MIR_TYPE_STRUCT:
		bo_string_append(tmp, "s.");
		break;
	case MIR_TYPE_SLICE:
		bo_string_append(tmp, "sl.");
		break;
	case MIR_TYPE_STRING:
		bo_string_append(tmp, "ss.");
		break;
	case MIR_TYPE_VARGS:
		bo_string_append(tmp, "sv.");
		break;
	default:
		BL_ABORT("Expected struct base type.");
	}

	if (id) {
		bo_string_append(tmp, id->str);
	}

	bo_string_append(tmp, "{");
	if (members) {
		MirMember *member;
		SARRAY_FOREACH(members, member)
		{
			BL_ASSERT(member->type->id.str);
			bo_string_append(tmp, member->type->id.str);

			if (i != members->size - 1) bo_string_append(tmp, ",");
		}
	}

	bo_string_append(tmp, "}");
	return bo_string_get(tmp);
}

static inline const char *
sh_type_enum(Context *cnt, ID *id, MirType *base_type, SmallArray_VariantPtr *variants)
{
	BL_ASSERT(base_type->id.str);
	BString *tmp = cnt->tmp_sh;
	bo_string_clear(tmp);

	bo_string_append(tmp, "e.");

	if (id) bo_string_append(tmp, id->str);

	bo_string_append(tmp, "(");
	bo_string_append(tmp, base_type->id.str);
	bo_string_append(tmp, ")");

	bo_string_append(tmp, "{");
	if (variants) {
		MirVariant *variant;
		SARRAY_FOREACH(variants, variant)
		{
			BL_ASSERT(variant->value);

			char value_str[35];
			snprintf(value_str,
			         ARRAY_SIZE(value_str),
			         "%lld",
			         (long long)variant->value->data.v_s64);
			bo_string_append(tmp, value_str);

			if (i != variants->size - 1) bo_string_append(tmp, ",");
		}
	}
	bo_string_append(tmp, "}");
	return bo_string_get(tmp);
}

/* impl */
/* Fetch type, when type with same sh has been already created and can be
 * reused, this function return false and set out_type to already created type
 * from cache. When new type instance was created function will return true and
 * set out_type to new instance of type, new instance will be stored in cache
 * for later use also.
 *
 * Hashing rules:
 *
 * | Type       | Rules                           |
 * |------------+---------------------------------|
 * | Null       | n.<type>                        |
 * | Pointer    | p.<type>                        |
 * | Function   | f.(<arg1,...>)<return type>     |
 * | Array      | <len>.<type>                    |
 * | Structures | <s|sl|sv|ss>.<name>{<m1,...>}   |
 * | Enumerator | <e>.<name>(<type>){<1,2,...>}   |
 */
bool
create_type(Context *cnt, MirType **out_type, const char *sh)
{
	BL_ASSERT(out_type);
	BL_ASSERT(sh);
	u64 hash = bo_hash_from_str(sh);

	bo_iterator_t found = bo_htbl_find(cnt->type_table, hash);
	bo_iterator_t end   = bo_htbl_end(cnt->type_table);
	if (!bo_iterator_equal(&found, &end)) {
		*out_type = bo_htbl_iter_peek_value(cnt->type_table, &found, MirType *);
		BL_ASSERT(*out_type);
		return false;
	} else {
		MirType *tmp = arena_alloc(&cnt->assembly->arenas.mir.type);

		BString *copy = builder_create_cached_str();
		bo_string_append(copy, sh);

		tmp->id.str  = bo_string_get(copy);
		tmp->id.hash = hash;

		// BL_LOG("new type: '%s' (%llu)", tmp->id.str, tmp->id.hash);
		bo_htbl_insert(cnt->type_table, tmp->id.hash, tmp);
		*out_type = tmp;

		return true;
	}

	BL_ABORT("should not happend");
}

ScopeEntry *
register_symbol(Context *cnt, Ast *node, ID *id, Scope *scope, bool is_builtin, bool enable_groups)
{
	BL_ASSERT(id && "Missing symbol ID.");
	BL_ASSERT(scope && "Missing entry scope.");

	const bool  is_private = scope->kind == SCOPE_PRIVATE;
	ScopeEntry *collision  = scope_lookup(scope, id, is_private, false);

	if (collision) {
		if (!is_private) goto COLLIDE;

		const bool collision_in_same_unit =
		    (node ? node->location->unit : NULL) ==
		    (collision->node ? collision->node->location->unit : NULL);

		if (collision_in_same_unit) {
			goto COLLIDE;
		}
	}

	/* no collision */
	ScopeEntry *entry = scope_create_entry(
	    &cnt->assembly->arenas.scope, SCOPE_ENTRY_INCOMPLETE, id, node, is_builtin);

	scope_insert(scope, entry);
	if (is_builtin) cache_builtin(cnt, entry);
	return entry;

COLLIDE : {
	char *err_msg = collision->is_buildin || is_builtin
	                    ? "Symbol name colision with compiler builtin '%s'."
	                    : "Duplicate symbol";

	builder_msg(BUILDER_MSG_ERROR,
	            ERR_DUPLICATE_SYMBOL,
	            node ? node->location : NULL,
	            BUILDER_CUR_WORD,
	            err_msg,
	            id->str);

	if (collision->node) {
		builder_msg(BUILDER_MSG_NOTE,
		            0,
		            collision->node->location,
		            BUILDER_CUR_WORD,
		            "Previous declaration found here.");
	}

	return NULL;
}
}

void
cache_builtin(Context *cnt, ScopeEntry *entry)
{
	BL_ASSERT(entry);
	ScopeEntry *collision = scope_lookup(cnt->builtin_types.cache, entry->id, true, false);
	if (collision) {
		BL_ABORT("Duplicate compiler internal '%s'.", entry->id->str);
	}

	scope_insert(cnt->builtin_types.cache, entry);
}

MirType *
lookup_builtin(Context *cnt, MirBuiltinIdKind kind)
{
	ID *        id    = &builtin_ids[kind];
	Scope *     scope = cnt->builtin_types.cache;
	ScopeEntry *found = scope_lookup(scope, id, true, false);

	if (!found) BL_ABORT("Missing compiler internal symbol '%s'", id->str);
	if (found->kind == SCOPE_ENTRY_INCOMPLETE) return NULL;

	BL_ASSERT(found->kind == SCOPE_ENTRY_VAR);

	MirVar *var = found->data.var;

	if (!IS_FLAG(var->flags, FLAG_COMPILER))
		BL_ABORT("Internally used symbol '%s' declared without '#compiler' flag!",
		         var->llvm_name);

	BL_ASSERT(var);
	BL_ASSERT(var->comptime && var->value.type->kind == MIR_TYPE_TYPE);
	BL_ASSERT(var->value.data.v_ptr.data.type);

	return var->value.data.v_ptr.data.type;
}

MirType *
create_type_type(Context *cnt)
{
	MirType *tmp = NULL;
	if (create_type(cnt, &tmp, builtin_ids[MIR_BUILTIN_ID_TYPE_TYPE].str)) {
		/* NOTE: TypeType has no LLVM representation */
		tmp->kind             = MIR_TYPE_TYPE;
		tmp->user_id          = &builtin_ids[MIR_BUILTIN_ID_TYPE_TYPE];
		tmp->alignment        = __alignof(MirType *);
		tmp->size_bits        = sizeof(MirType *) * 8;
		tmp->store_size_bytes = sizeof(MirType *);
	}
	return tmp;
}

MirType *
create_type_null(Context *cnt, MirType *base_type)
{
	BL_ASSERT(base_type);
	MirType *tmp = NULL;
	if (create_type(cnt, &tmp, sh_type_null(cnt, base_type))) {
		tmp->kind                = MIR_TYPE_NULL;
		tmp->user_id             = &builtin_ids[MIR_BUILTIN_ID_NULL];
		tmp->data.null.base_type = base_type;

		init_llvm_type_null(cnt, tmp);
	}
	return tmp;
}

MirType *
create_type_void(Context *cnt)
{
	MirType *tmp = NULL;
	if (create_type(cnt, &tmp, builtin_ids[MIR_BUILTIN_ID_TYPE_VOID].str)) {
		tmp->kind    = MIR_TYPE_VOID;
		tmp->user_id = &builtin_ids[MIR_BUILTIN_ID_TYPE_VOID];

		init_llvm_type_void(cnt, tmp);
	}
	return tmp;
}

MirType *
create_type_bool(Context *cnt)
{
	MirType *tmp = NULL;
	if (create_type(cnt, &tmp, builtin_ids[MIR_BUILTIN_ID_TYPE_BOOL].str)) {
		tmp->kind    = MIR_TYPE_BOOL;
		tmp->user_id = &builtin_ids[MIR_BUILTIN_ID_TYPE_BOOL];

		init_llvm_type_bool(cnt, tmp);
	}
	return tmp;
}

MirType *
create_type_int(Context *cnt, ID *id, s32 bitcount, bool is_signed)
{
	BL_ASSERT(id);
	BL_ASSERT(bitcount > 0);
	MirType *tmp = NULL;
	if (create_type(cnt, &tmp, id->str)) {
		tmp->kind                   = MIR_TYPE_INT;
		tmp->user_id                = id;
		tmp->data.integer.bitcount  = bitcount;
		tmp->data.integer.is_signed = is_signed;

		init_llvm_type_int(cnt, tmp);
	}
	return tmp;
}

MirType *
create_type_real(Context *cnt, ID *id, s32 bitcount)
{
	BL_ASSERT(bitcount > 0);
	MirType *tmp = NULL;
	if (create_type(cnt, &tmp, id->str)) {
		tmp->kind               = MIR_TYPE_REAL;
		tmp->user_id            = id;
		tmp->data.real.bitcount = bitcount;

		init_llvm_type_real(cnt, tmp);
	}
	return tmp;
}

MirType *
create_type_ptr(Context *cnt, MirType *src_type)
{
	MirType *tmp = NULL;
	if (create_type(cnt, &tmp, sh_type_ptr(cnt, src_type))) {
		tmp->kind          = MIR_TYPE_PTR;
		tmp->data.ptr.expr = src_type;

		init_llvm_type_ptr(cnt, tmp);
	}

	return tmp;
}

MirType *
create_type_fn(Context *cnt, ID *id, MirType *ret_type, SmallArray_ArgPtr *args, bool is_vargs)
{
	MirType *tmp = NULL;
	if (create_type(cnt, &tmp, sh_type_fn(cnt, ret_type, args, is_vargs))) {
		tmp->kind             = MIR_TYPE_FN;
		tmp->data.fn.args     = args;
		tmp->data.fn.is_vargs = is_vargs;
		tmp->data.fn.ret_type = ret_type ? ret_type : cnt->builtin_types.t_void;
		tmp->user_id          = id;

		init_llvm_type_fn(cnt, tmp);
	}

	return tmp;
}

MirType *
create_type_array(Context *cnt, MirType *elem_type, s64 len)
{
	MirType *tmp = NULL;
	if (create_type(cnt, &tmp, sh_type_arr(cnt, elem_type, len))) {
		tmp->kind                 = MIR_TYPE_ARRAY;
		tmp->data.array.elem_type = elem_type;
		tmp->data.array.len       = len;

		init_llvm_type_array(cnt, tmp);
	}

	return tmp;
}

MirType *
create_type_struct(Context *             cnt,
                   MirTypeKind           kind,
                   ID *                  id,
                   Scope *               scope,
                   SmallArray_MemberPtr *members, /* MirMember */
                   bool                  is_packed)
{
	MirType *tmp = NULL;

	if (create_type(cnt, &tmp, sh_type_struct(cnt, kind, id, members, is_packed))) {
		tmp->kind                 = kind;
		tmp->data.strct.members   = members;
		tmp->data.strct.scope     = scope;
		tmp->data.strct.is_packed = is_packed;
		tmp->user_id              = id;

		init_llvm_type_struct(cnt, tmp);
	}

	return tmp;
}

MirType *
create_type_struct_special(Context *cnt, MirTypeKind kind, ID *id, MirType *elem_ptr_type)
{
	BL_ASSERT(mir_is_pointer_type(elem_ptr_type));
	BL_ASSERT(kind == MIR_TYPE_STRING || kind == MIR_TYPE_VARGS || kind == MIR_TYPE_SLICE);

	/* PERFORMANCE: due to reusing of the types we can create members and scope which will
	 * not be later used because same type already exists. */
	SmallArray_MemberPtr *members = create_sarr(SmallArray_MemberPtr, cnt->assembly);

	/* Slice layout struct { s64, *T } */
	Scope *body_scope = scope_create(
	    &cnt->assembly->arenas.scope, SCOPE_TYPE_STRUCT, cnt->assembly->gscope, 2, NULL);

	MirMember *tmp;
	tmp = create_member(cnt,
	                    NULL,
	                    &builtin_ids[MIR_BUILTIN_ID_ARR_LEN],
	                    body_scope,
	                    0,
	                    cnt->builtin_types.t_s64);

	sa_push_MemberPtr(members, tmp);
	provide_builtin_member(cnt, body_scope, tmp);

	tmp = create_member(
	    cnt, NULL, &builtin_ids[MIR_BUILTIN_ID_ARR_PTR], body_scope, 1, elem_ptr_type);

	sa_push_MemberPtr(members, tmp);
	provide_builtin_member(cnt, body_scope, tmp);

	return create_type_struct(cnt, kind, id, body_scope, members, false);
}

MirType *
create_type_enum(Context *              cnt,
                 ID *                   id,
                 Scope *                scope,
                 MirType *              base_type,
                 SmallArray_VariantPtr *variants)
{
	BL_ASSERT(base_type);
	MirType *tmp = NULL;
	if (create_type(cnt, &tmp, sh_type_enum(cnt, id, base_type, variants))) {
		tmp->kind               = MIR_TYPE_ENUM;
		tmp->data.enm.scope     = scope;
		tmp->data.enm.base_type = base_type;
		tmp->data.enm.variants  = variants;
		tmp->user_id            = id;

		init_llvm_type_enum(cnt, tmp);
	}

	return tmp;
}

void
init_llvm_type_int(Context *cnt, MirType *type)
{
	type->llvm_type        = LLVMIntTypeInContext(cnt->assembly->llvm.cnt,
                                               (unsigned int)type->data.integer.bitcount);
	type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
	type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
	type->alignment        = LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);

	/*** DI ***/
	if (!cnt->debug_mode) return;

	const char *    name = type->user_id ? type->user_id->str : type->id.str;
	DW_ATE_Encoding encoding;

	if (type->data.integer.is_signed) {
		if (type->size_bits == 8)
			encoding = DW_ATE_signed_char;
		else
			encoding = DW_ATE_signed;
	} else {
		if (type->size_bits == 8)
			encoding = DW_ATE_unsigned_char;
		else
			encoding = DW_ATE_unsigned;
	}

	type->llvm_meta = llvm_di_create_basic_type(
	    cnt->analyze.llvm_di_builder, name, type->size_bits, encoding);
}

void
init_llvm_type_real(Context *cnt, MirType *type)
{
	if (type->data.real.bitcount == 32)
		type->llvm_type = LLVMFloatTypeInContext(cnt->assembly->llvm.cnt);
	else if (type->data.real.bitcount == 64)
		type->llvm_type = LLVMDoubleTypeInContext(cnt->assembly->llvm.cnt);
	else
		BL_ABORT("invalid floating point type");

	type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
	type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
	type->alignment        = LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);

	/*** DI ***/
	if (!cnt->debug_mode) return;

	const char *name = type->user_id ? type->user_id->str : type->id.str;

	type->llvm_meta = llvm_di_create_basic_type(
	    cnt->analyze.llvm_di_builder, name, type->size_bits, DW_ATE_float);
}

void
init_llvm_type_ptr(Context *cnt, MirType *type)
{
	MirType *tmp = mir_deref_type(type);
	/* Pointer to Type has no LLVM representation and cannot not be generated into IR.*/
	if (tmp->kind == MIR_TYPE_TYPE) return;

	BL_ASSERT(tmp);
	BL_ASSERT(tmp->llvm_type);
	type->llvm_type        = LLVMPointerType(tmp->llvm_type, 0);
	type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
	type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
	type->alignment        = LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);

	/*** DI ***/
	if (!cnt->debug_mode) return;

	const char *name = type->user_id ? type->user_id->str : type->id.str;
	type->llvm_meta  = llvm_di_create_pointer_type(cnt->analyze.llvm_di_builder,
                                                      tmp->llvm_meta,
                                                      type->size_bits,
                                                      type->alignment * 8,
                                                      name);
}

void
init_llvm_type_void(Context *cnt, MirType *type)
{
	type->alignment        = 0;
	type->size_bits        = 0;
	type->store_size_bytes = 0;
	type->llvm_type        = LLVMVoidTypeInContext(cnt->assembly->llvm.cnt);

	/*** DI ***/
	if (!cnt->debug_mode) return;

	type->llvm_meta = llvm_di_create_basic_type(
	    cnt->analyze.llvm_di_builder, "void", 8, DW_ATE_unsigned_char);
}

void
init_llvm_type_null(Context *cnt, MirType *type)
{
	MirType *tmp = type->data.null.base_type;
	BL_ASSERT(tmp);
	BL_ASSERT(tmp->llvm_type);
	type->llvm_type        = tmp->llvm_type;
	type->alignment        = tmp->alignment;
	type->size_bits        = tmp->size_bits;
	type->store_size_bytes = tmp->store_size_bytes;

	/*** DI ***/
	if (!cnt->debug_mode) return;
	type->llvm_meta = llvm_di_create_null_type(cnt->analyze.llvm_di_builder);
}

void
init_llvm_type_bool(Context *cnt, MirType *type)
{
	type->llvm_type        = LLVMIntTypeInContext(cnt->assembly->llvm.cnt, 1);
	type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
	type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
	type->alignment        = LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);

	/*** DI ***/
	if (!cnt->debug_mode) return;

	const char *name = type->user_id ? type->user_id->str : type->id.str;
	type->llvm_meta =
	    llvm_di_create_basic_type(cnt->analyze.llvm_di_builder, name, 8, DW_ATE_boolean);
}

static inline size_t
struct_split_fit(Context *cnt, MirType *struct_type, size_t bound, size_t *start)
{
	s32    so     = mir_get_struct_elem_offest(cnt->assembly, struct_type, *start);
	size_t offset = 0;
	size_t size   = 0;
	size_t total  = 0;
	for (; *start < struct_type->data.strct.members->size; ++(*start)) {
		offset = mir_get_struct_elem_offest(cnt->assembly, struct_type, *start) - so;
		size   = mir_get_struct_elem_type(struct_type, *start)->store_size_bytes;
		if (offset + size > bound) return bound;
		total = offset + size;
	}

	return total > 1 ? next_pow_2(total) : total;
}

void
init_llvm_type_fn(Context *cnt, MirType *type)
{
	MirType *tmp_ret = type->data.fn.ret_type;
	if (tmp_ret->kind == MIR_TYPE_TYPE) {
		return;
	}

	SmallArray_ArgPtr *args      = type->data.fn.args;
	const bool         has_args  = args;
	bool               has_byval = false;

	SmallArray_LLVMType llvm_args;
	sa_init(&llvm_args);
	LLVMTypeRef llvm_ret = NULL;

	if (has_args) {
		MirArg *arg;
		SARRAY_FOREACH(args, arg)
		{
			arg->llvm_index = llvm_args.size;

			/* Composit types. */
			if (builder.options.promote_structs_into_registers &&
			    mir_is_composit_type(arg->type)) {
				LLVMContextRef llvm_cnt = cnt->assembly->llvm.cnt;
				size_t         start    = 0;
				s32            low      = 0;
				s32            high     = 0;

				if (!has_byval) has_byval = true;

				low = struct_split_fit(cnt, arg->type, sizeof(size_t), &start);

				if (start < arg->type->data.strct.members->size)
					high = struct_split_fit(
					    cnt, arg->type, sizeof(size_t), &start);

				if (start < arg->type->data.strct.members->size) {
					arg->llvm_easgm = LLVM_EASGM_BYVAL;

					BL_ASSERT(arg->type->llvm_type);
					sa_push_LLVMType(&llvm_args,
					                 LLVMPointerType(arg->type->llvm_type, 0));
				} else {
					switch (low) {
					case 1:
						arg->llvm_easgm = LLVM_EASGM_8;
						sa_push_LLVMType(&llvm_args,
						                 LLVMInt8TypeInContext(llvm_cnt));
						break;
					case 2:
						arg->llvm_easgm = LLVM_EASGM_16;
						sa_push_LLVMType(&llvm_args,
						                 LLVMInt16TypeInContext(llvm_cnt));
						break;
					case 4:
						arg->llvm_easgm = LLVM_EASGM_32;
						sa_push_LLVMType(&llvm_args,
						                 LLVMInt32TypeInContext(llvm_cnt));
						break;
					case 8: {
						switch (high) {
						case 0:
							arg->llvm_easgm = LLVM_EASGM_64;
							sa_push_LLVMType(
							    &llvm_args,
							    LLVMInt64TypeInContext(llvm_cnt));
							break;
						case 1:
							arg->llvm_easgm = LLVM_EASGM_64_8;
							sa_push_LLVMType(
							    &llvm_args,
							    LLVMInt64TypeInContext(llvm_cnt));
							sa_push_LLVMType(
							    &llvm_args,
							    LLVMInt8TypeInContext(llvm_cnt));
							break;
						case 2:
							arg->llvm_easgm = LLVM_EASGM_64_16;
							sa_push_LLVMType(
							    &llvm_args,
							    LLVMInt64TypeInContext(llvm_cnt));
							sa_push_LLVMType(
							    &llvm_args,
							    LLVMInt16TypeInContext(llvm_cnt));
							break;
						case 4:
							arg->llvm_easgm = LLVM_EASGM_64_32;
							sa_push_LLVMType(
							    &llvm_args,
							    LLVMInt64TypeInContext(llvm_cnt));
							sa_push_LLVMType(
							    &llvm_args,
							    LLVMInt32TypeInContext(llvm_cnt));
							break;
						case 8:
							arg->llvm_easgm = LLVM_EASGM_64_64;
							sa_push_LLVMType(
							    &llvm_args,
							    LLVMInt64TypeInContext(llvm_cnt));
							sa_push_LLVMType(
							    &llvm_args,
							    LLVMInt64TypeInContext(llvm_cnt));
							break;
						default:
							BL_ASSERT(false);
							break;
						}
						break;
					}
					default:
						BL_ASSERT(false);
						break;
					}
				}
			} else {
				BL_ASSERT(arg->type->llvm_type);
				sa_push_LLVMType(&llvm_args, arg->type->llvm_type);
			}
		}
	}

	llvm_ret = tmp_ret ? tmp_ret->llvm_type : LLVMVoidTypeInContext(cnt->assembly->llvm.cnt);
	BL_ASSERT(llvm_ret);

	type->llvm_type         = LLVMFunctionType(llvm_ret, llvm_args.data, llvm_args.size, false);
	type->alignment         = __alignof(MirFn *);
	type->size_bits         = sizeof(MirFn *) * 8;
	type->store_size_bytes  = sizeof(MirFn *);
	type->data.fn.has_byval = has_byval;

	sa_terminate(&llvm_args);

	/*** DI ***/
	if (!cnt->debug_mode) return;
	SmallArray_LLVMMetadata params;
	sa_init(&params);

	/* return type is first */
	sa_push_LLVMMetadata(&params, type->data.fn.ret_type->llvm_meta);

	if (type->data.fn.args) {
		MirArg *it;
		SARRAY_FOREACH(type->data.fn.args, it)
		{
			sa_push_LLVMMetadata(&params, it->type->llvm_meta);
		}
	}

	type->llvm_meta =
	    llvm_di_create_function_type(cnt->analyze.llvm_di_builder, params.data, params.size);

	sa_terminate(&params);
}

void
init_llvm_type_array(Context *cnt, MirType *type)
{
	LLVMTypeRef llvm_elem_type = type->data.array.elem_type->llvm_type;
	BL_ASSERT(llvm_elem_type);
	const unsigned int len = (const unsigned int)type->data.array.len;

	type->llvm_type        = LLVMArrayType(llvm_elem_type, len);
	type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
	type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
	type->alignment        = LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);

	/*** DI ***/
	if (!cnt->debug_mode) return;
	type->llvm_meta = llvm_di_create_array_type(cnt->analyze.llvm_di_builder,
	                                            type->size_bits,
	                                            type->alignment * 8,
	                                            type->data.array.elem_type->llvm_meta,
	                                            type->data.array.len);
}

void
init_llvm_type_struct(Context *cnt, MirType *type)
{
	SmallArray_MemberPtr *members = type->data.strct.members;
	BL_ASSERT(members);

	const bool   is_packed = type->data.strct.is_packed;
	const size_t memc      = members->size;
	BL_ASSERT(memc > 0);
	SmallArray_LLVMType llvm_members;
	sa_init(&llvm_members);

	MirMember *member;
	SARRAY_FOREACH(members, member)
	{
		BL_ASSERT(member->type->llvm_type);
		sa_push_LLVMType(&llvm_members, member->type->llvm_type);
	}

	/* named structure type */
	if (type->user_id) {
		type->llvm_type =
		    LLVMStructCreateNamed(cnt->assembly->llvm.cnt, type->user_id->str);
		LLVMStructSetBody(
		    type->llvm_type, llvm_members.data, (unsigned long)memc, is_packed);
	} else {
		type->llvm_type = LLVMStructTypeInContext(
		    cnt->assembly->llvm.cnt, llvm_members.data, (unsigned long)memc, is_packed);
	}

	type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
	type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
	type->alignment        = LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);

	sa_terminate(&llvm_members);

	/* set offsets for members */
	SARRAY_FOREACH(members, member)
	member->offset_bytes = mir_get_struct_elem_offest(cnt->assembly, type, i);

	/*** DI ***/
	if (!cnt->debug_mode) return;

	BL_ASSERT(type->data.strct.scope);
	if (!type->data.strct.scope->llvm_di_meta) init_llvm_DI_scope(cnt, type->data.strct.scope);

	const bool      is_implicit = !type->data.strct.scope->location;
	LLVMMetadataRef llvm_file;
	unsigned        struct_line;

	if (is_implicit) {
		struct_line = 0;
		llvm_file   = cnt->assembly->gscope->llvm_di_meta;
	} else {
		Location *location = type->data.strct.scope->location;
		llvm_file          = location->unit->llvm_file_meta;
		struct_line        = location->line;
	}

	LLVMMetadataRef llvm_scope = type->data.strct.scope->llvm_di_meta;
	const char *    struct_name;
	if (type->user_id) {
		struct_name = type->user_id->str;
	} else {
		/* NOTE: string has buildin ID */
		switch (type->kind) {
		case MIR_TYPE_STRUCT: {
			struct_name = "struct";
			break;
		}

		case MIR_TYPE_SLICE: {
			struct_name = "slice";
			break;
		}

		case MIR_TYPE_VARGS: {
			struct_name = "vargs";
			break;
		}

		default:
			BL_ABORT("cannot get struct name for DI");
		}
	}

	SmallArray_LLVMMetadata llvm_elems;
	sa_init(&llvm_elems);

	MirMember *elem;
	SARRAY_FOREACH(type->data.strct.members, elem)
	{
		unsigned        elem_line = elem->decl_node ? elem->decl_node->location->line : 0;
		LLVMMetadataRef llvm_elem = llvm_di_create_member_type(
		    cnt->analyze.llvm_di_builder,
		    llvm_scope,
		    elem->id->str,
		    llvm_file,
		    elem_line,
		    elem->type->size_bits,
		    elem->type->alignment * 8,
		    mir_get_struct_elem_offest(cnt->assembly, type, i) * 8,
		    elem->type->llvm_meta);

		sa_push_LLVMMetadata(&llvm_elems, llvm_elem);
	}

	LLVMMetadataRef llvm_struct =
	    llvm_di_create_struct_type(cnt->analyze.llvm_di_builder,
	                               type->data.strct.scope->parent->llvm_di_meta,
	                               struct_name,
	                               llvm_file,
	                               struct_line,
	                               type->size_bits,
	                               type->alignment * 8,
	                               llvm_elems.data,
	                               llvm_elems.size);

	type->llvm_meta = llvm_di_replace_temporary(
	    cnt->analyze.llvm_di_builder, type->data.strct.scope->llvm_di_meta, llvm_struct);

	sa_terminate(&llvm_elems);
	return;
}

void
init_llvm_type_enum(Context *cnt, MirType *type)
{
	MirType *base_type = type->data.enm.base_type;
	BL_ASSERT(base_type->kind == MIR_TYPE_INT);
	LLVMTypeRef llvm_base_type = base_type->llvm_type;
	BL_ASSERT(llvm_base_type);

	type->llvm_type        = llvm_base_type;
	type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
	type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
	type->alignment        = LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);

	/*** DI ***/
	if (!cnt->debug_mode) return;
	const char *name = type->user_id ? type->user_id->str : "enum";

	SmallArray_LLVMMetadata llvm_elems;
	sa_init(&llvm_elems);

	MirVariant *variant;
	SARRAY_FOREACH(type->data.enm.variants, variant)
	{
		LLVMMetadataRef llvm_variant =
		    llvm_di_create_enum_variant(cnt->analyze.llvm_di_builder,
		                                variant->id->str,
		                                variant->value->data.v_u64,
		                                !base_type->data.integer.is_signed);

		sa_push_LLVMMetadata(&llvm_elems, llvm_variant);
	}

	LLVMMetadataRef llvm_type =
	    llvm_di_create_enum_type(cnt->analyze.llvm_di_builder,
	                             type->data.enm.scope->parent->llvm_di_meta,
	                             name,
	                             type->data.enm.scope->location->unit->llvm_file_meta,
	                             type->data.enm.scope->location->line,
	                             type->size_bits,
	                             type->alignment * 8,
	                             llvm_elems.data,
	                             llvm_elems.size,
	                             base_type->llvm_meta);

	type->llvm_meta = llvm_di_replace_temporary(
	    cnt->analyze.llvm_di_builder, type->data.enm.scope->llvm_di_meta, llvm_type);
	sa_terminate(&llvm_elems);
}

void
init_llvm_DI_scope(Context *cnt, Scope *scope)
{
	switch (scope->kind) {
	case SCOPE_LEXICAL: {
		BL_ASSERT(scope->location);
		LLVMMetadataRef llvm_parent_scope = scope->parent->llvm_di_meta;
		LLVMMetadataRef llvm_unit         = scope->location->unit->llvm_file_meta;

		BL_ASSERT(llvm_parent_scope);
		BL_ASSERT(llvm_unit);

		scope->llvm_di_meta = llvm_di_create_lexical_scope(cnt->analyze.llvm_di_builder,
		                                                   llvm_parent_scope,
		                                                   llvm_unit,
		                                                   scope->location->line,
		                                                   scope->location->col);
		break;
	}

	case SCOPE_FN: {
		scope->llvm_di_meta = llvm_di_create_fn_fwd_decl(
		    cnt->analyze.llvm_di_builder, NULL, "", "", NULL, 0, NULL, 0);
		break;
	}

	case SCOPE_TYPE_STRUCT: {
		scope->llvm_di_meta = llvm_di_create_replecable_composite_type(
		    cnt->analyze.llvm_di_builder, DW_TAG_structure_type, "", NULL, NULL, 0);
		break;
	}

	case SCOPE_TYPE_ENUM: {
		scope->llvm_di_meta = llvm_di_create_replecable_composite_type(
		    cnt->analyze.llvm_di_builder, DW_TAG_enumeration_type, "", NULL, NULL, 0);
		break;
	}

	default:
		BL_ABORT("unsuported scope type for DI generation");
	}
}

static inline void
push_var(Context *cnt, MirVar *var)
{
	BL_ASSERT(var);

	if (var->is_in_gscope) return;

	MirFn *fn = get_current_fn(cnt);
	BL_ASSERT(fn);
	bo_array_push_back(fn->variables, var);
}

MirVar *
create_var(Context *cnt,
           Ast *    decl_node,
           Scope *  scope,
           ID *     id,
           MirType *alloc_type,
           bool     is_mutable,
           bool     is_in_gscope,
           u32      flags)
{
	BL_ASSERT(id);
	MirVar *tmp       = arena_alloc(&cnt->assembly->arenas.mir.var);
	tmp->id           = id;
	tmp->value.type   = alloc_type;
	tmp->decl_scope   = scope;
	tmp->decl_node    = decl_node;
	tmp->is_mutable   = is_mutable;
	tmp->is_in_gscope = is_in_gscope;
	tmp->llvm_name    = id->str;
	tmp->flags        = flags;
	tmp->gen_llvm     = true;

	push_var(cnt, tmp);
	return tmp;
}

MirVar *
create_var_impl(Context *   cnt,
                const char *name,
                MirType *   alloc_type,
                bool        is_mutable,
                bool        is_in_gscope,
                bool        comptime)
{
	BL_ASSERT(name);
	MirVar *tmp       = arena_alloc(&cnt->assembly->arenas.mir.var);
	tmp->value.type   = alloc_type;
	tmp->is_mutable   = is_mutable;
	tmp->is_in_gscope = is_in_gscope;
	tmp->llvm_name    = name;
	tmp->is_implicit  = true;
	tmp->gen_llvm     = true;
	tmp->comptime     = comptime;

	push_var(cnt, tmp);
	return tmp;
}

MirFn *
create_fn(Context *        cnt,
          Ast *            node,
          ID *             id,
          const char *     linkage_name,
          s32              flags,
          MirInstrFnProto *prototype,
          bool             emit_llvm,
          bool             is_in_gscope)
{
	MirFn *tmp        = arena_alloc(&cnt->assembly->arenas.mir.fn);
	tmp->variables    = create_arr(cnt->assembly, sizeof(MirVar *));
	tmp->linkage_name = linkage_name;
	tmp->id           = id;
	tmp->flags        = flags;
	tmp->decl_node    = node;
	tmp->prototype    = &prototype->base;
	tmp->emit_llvm    = emit_llvm;
	tmp->is_in_gscope = is_in_gscope;
	return tmp;
}

MirMember *
create_member(Context *cnt, Ast *node, ID *id, Scope *scope, s64 index, MirType *type)
{
	MirMember *tmp  = arena_alloc(&cnt->assembly->arenas.mir.member);
	tmp->decl_node  = node;
	tmp->id         = id;
	tmp->index      = index;
	tmp->type       = type;
	tmp->decl_scope = scope;
	return tmp;
}

MirArg *
create_arg(Context *cnt, Ast *node, ID *id, Scope *scope, MirType *type)
{
	MirArg *tmp     = arena_alloc(&cnt->assembly->arenas.mir.arg);
	tmp->decl_node  = node;
	tmp->id         = id;
	tmp->type       = type;
	tmp->decl_scope = scope;
	return tmp;
}

MirVariant *
create_variant(Context *cnt, ID *id, Scope *scope, MirConstValue *value)
{
	MirVariant *tmp = arena_alloc(&cnt->assembly->arenas.mir.variant);
	tmp->id         = id;
	tmp->decl_scope = scope;
	tmp->value      = value;
	return tmp;
}

MirConstValue *
create_const_value(Context *cnt, MirType *type)
{
	MirConstValue *tmp = arena_alloc(&cnt->assembly->arenas.mir.value);
	tmp->type          = type;
	tmp->addr_mode     = MIR_VAM_LVALUE_CONST;
	return tmp;
}

MirConstValue *
init_or_create_const_integer(Context *cnt, MirConstValue *v, MirType *type, u64 i)
{
	if (!v) v = arena_alloc(&cnt->assembly->arenas.mir.value);
	v->type       = type;
	v->addr_mode  = MIR_VAM_LVALUE_CONST;
	v->data.v_u64 = i;

	return v;
}

MirConstValue *
init_or_create_const_bool(Context *cnt, MirConstValue *v, bool b)
{
	if (!v) v = arena_alloc(&cnt->assembly->arenas.mir.value);
	v->type        = cnt->builtin_types.t_bool;
	v->addr_mode   = MIR_VAM_LVALUE_CONST;
	v->data.v_bool = b;

	return v;
}

MirConstValue *
init_or_create_const_var_ptr(Context *cnt, MirConstValue *v, MirType *type, MirVar *var)
{
	if (!v) v = arena_alloc(&cnt->assembly->arenas.mir.value);
	v->type      = type;
	v->addr_mode = MIR_VAM_LVALUE_CONST;

	mir_set_const_ptr(&v->data.v_ptr, var, MIR_CP_VAR);
	return v;
}

MirConstValue *
init_or_create_const_array(Context *                 cnt,
                           MirConstValue *           v,
                           MirType *                 elem_type,
                           SmallArray_ConstValuePtr *elems)
{
	if (!v) v = arena_alloc(&cnt->assembly->arenas.mir.value);
	v->type               = create_type_array(cnt, elem_type, elems->size);
	v->addr_mode          = MIR_VAM_LVALUE_CONST;
	v->data.v_array.elems = elems;

	return v;
}

MirConstValue *
init_or_create_const_struct(Context *                 cnt,
                            MirConstValue *           v,
                            MirType *                 type,
                            SmallArray_ConstValuePtr *members)
{
	if (!v) v = arena_alloc(&cnt->assembly->arenas.mir.value);
	v->type                  = type;
	v->addr_mode             = MIR_VAM_LVALUE_CONST;
	v->data.v_struct.members = members;

	return v;
}

MirConstValue *
init_or_create_const_string(Context *cnt, MirConstValue *v, const char *str)
{
	if (!v) v = arena_alloc(&cnt->assembly->arenas.mir.value);
	v->type      = cnt->builtin_types.t_string;
	v->addr_mode = MIR_VAM_LVALUE_CONST;

	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);

	/* .len */
	sa_push_ConstValuePtr(
	    m, init_or_create_const_integer(cnt, NULL, cnt->builtin_types.t_s64, strlen(str)));

	/* .ptr */
	MirConstValue *ptr = create_const_value(
	    cnt, mir_get_struct_elem_type(cnt->builtin_types.t_string, MIR_SLICE_PTR_INDEX));

	MirConstPtr *const_ptr = &ptr->data.v_ptr;
	mir_set_const_ptr(const_ptr, (void *)str, MIR_CP_STR);
	sa_push_ConstValuePtr(m, ptr);

	v->data.v_struct.members = m;
	return v;
}

/* instructions */
void
maybe_mark_as_unrechable(MirInstrBlock *block, MirInstr *instr)
{
	if (!is_block_terminated(block)) return;
	instr->unrechable         = true;
	MirFn *          fn       = block->owner_fn;
	MirInstrFnProto *fn_proto = (MirInstrFnProto *)fn->prototype;
	if (!fn_proto->first_unrechable_location && instr->node)
		fn_proto->first_unrechable_location = instr->node->location;
}

void
append_current_block(Context *cnt, MirInstr *instr)
{
	BL_ASSERT(instr);
	MirInstrBlock *block = get_current_block(cnt);
	BL_ASSERT(block);

	maybe_mark_as_unrechable(block, instr);

	instr->owner_block = block;
	instr->prev        = block->last_instr;

	if (!block->entry_instr) block->entry_instr = instr;
	if (instr->prev) instr->prev->next = instr;
	block->last_instr = instr;
}

MirInstr *
insert_instr_cast(Context *cnt, MirInstr *src, MirType *to_type)
{
	MirInstrCast *cast    = CREATE_INSTR(cnt, MIR_INSTR_CAST, src->node, MirInstrCast *);
	cast->base.value.type = to_type;
	cast->expr            = src;
	ref_instr(&cast->base);

	insert_instr_after(src, &cast->base);
	analyze_instr_rq(cnt, &cast->base);
	return &cast->base;
}

MirInstr *
insert_instr_toany(Context *cnt, MirInstr *expr)
{
	MirInstrToAny *toany   = CREATE_INSTR(cnt, MIR_INSTR_TOANY, expr->node, MirInstrToAny *);
	toany->base.value.type = create_type_ptr(cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_ANY));
	toany->expr            = expr;
	ref_instr(&toany->base);

	insert_instr_after(expr, &toany->base);
	analyze_instr_rq(cnt, &toany->base);
	return &toany->base;
}

MirInstr *
insert_instr_load(Context *cnt, MirInstr *src)
{
	BL_ASSERT(src);
	BL_ASSERT(src->value.type);
	BL_ASSERT(src->value.type->kind == MIR_TYPE_PTR);
	MirInstrLoad *tmp  = CREATE_INSTR(cnt, MIR_INSTR_LOAD, src->node, MirInstrLoad *);
	tmp->src           = src;
	tmp->base.analyzed = true;

	ref_instr(&tmp->base);
	insert_instr_after(src, &tmp->base);
	analyze_instr_load(cnt, tmp);

	return &tmp->base;
}

MirCastOp
get_cast_op(MirType *from, MirType *to)
{
	BL_ASSERT(from);
	BL_ASSERT(to);
	const size_t fsize = from->size_bits;
	const size_t tsize = to->size_bits;

	if (type_cmp(from, to)) return MIR_CAST_NONE;

	switch (from->kind) {
	case MIR_TYPE_ENUM:
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

		case MIR_TYPE_REAL: {
			/* to integer */
			if (fsize < tsize) {
				return MIR_CAST_FPEXT;
			} else {
				return MIR_CAST_FPTRUNC;
			}
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

static u64 _id_counter = 0;

MirInstr *
_create_instr(Context *cnt, MirInstrKind kind, Ast *node)
{
	MirInstr *tmp = arena_alloc(&cnt->assembly->arenas.mir.instr);
	tmp->kind     = kind;
	tmp->node     = node;
	tmp->id       = _id_counter++;

	return tmp;
}

MirInstrBlock *
append_block(Context *cnt, MirFn *fn, const char *name)
{
	BL_ASSERT(fn && name);
	MirInstrBlock *tmp = CREATE_INSTR(cnt, MIR_INSTR_BLOCK, NULL, MirInstrBlock *);
	tmp->name          = name;
	tmp->owner_fn      = fn;

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
	BL_ASSERT(fn && fn->kind == MIR_INSTR_FN_PROTO);
	MirInstrCall *tmp   = CREATE_INSTR(cnt, MIR_INSTR_CALL, node, MirInstrCall *);
	tmp->base.comptime  = true;
	tmp->base.ref_count = 2;
	tmp->callee         = fn;

	ref_instr(fn);
	return &tmp->base;
}

MirInstr *
append_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, SmallArray_InstrPtr *args)
{
	MirInstrTypeFn *tmp  = CREATE_INSTR(cnt, MIR_INSTR_TYPE_FN, node, MirInstrTypeFn *);
	tmp->base.value.type = cnt->builtin_types.t_type;
	tmp->base.comptime   = true;
	tmp->ret_type        = ret_type;
	tmp->args            = args;

	if (args) {
		MirInstr *it;
		SARRAY_FOREACH(args, it)
		{
			ref_instr(it);
		}
	}

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_type_struct(Context *            cnt,
                         Ast *                node,
                         ID *                 id,
                         Scope *              scope,
                         SmallArray_InstrPtr *members,
                         bool                 is_packed)
{
	MirInstrTypeStruct *tmp =
	    CREATE_INSTR(cnt, MIR_INSTR_TYPE_STRUCT, node, MirInstrTypeStruct *);
	tmp->base.value.type = cnt->builtin_types.t_type;
	tmp->base.comptime   = true;
	tmp->members         = members;
	tmp->scope           = scope;
	tmp->is_packed       = is_packed;
	tmp->id              = id;

	if (members) {
		MirInstr *it;
		SARRAY_FOREACH(members, it)
		{
			ref_instr(it);
		}
	}

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_type_enum(Context *            cnt,
                       Ast *                node,
                       ID *                 id,
                       Scope *              scope,
                       SmallArray_InstrPtr *variants,
                       MirInstr *           base_type)
{
	MirInstrTypeEnum *tmp = CREATE_INSTR(cnt, MIR_INSTR_TYPE_ENUM, node, MirInstrTypeEnum *);
	tmp->base.value.type  = cnt->builtin_types.t_type;
	tmp->base.comptime    = true;
	tmp->variants         = variants;
	tmp->scope            = scope;
	tmp->id               = id;
	tmp->base_type        = base_type;

	if (variants) {
		MirInstr *it;
		SARRAY_FOREACH(variants, it)
		{
			ref_instr(it);
		}
	}

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_type_ptr(Context *cnt, Ast *node, MirInstr *type)
{
	MirInstrTypePtr *tmp = CREATE_INSTR(cnt, MIR_INSTR_TYPE_PTR, node, MirInstrTypePtr *);
	tmp->base.value.type = cnt->builtin_types.t_type;
	tmp->base.comptime   = true;
	tmp->type            = type;

	ref_instr(type);
	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_type_array(Context *cnt, Ast *node, MirInstr *elem_type, MirInstr *len)
{
	MirInstrTypeArray *tmp = CREATE_INSTR(cnt, MIR_INSTR_TYPE_ARRAY, node, MirInstrTypeArray *);
	tmp->base.value.type   = cnt->builtin_types.t_type;
	tmp->base.comptime     = true;
	tmp->elem_type         = elem_type;
	tmp->len               = len;

	ref_instr(elem_type);
	ref_instr(len);
	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_type_slice(Context *cnt, Ast *node, MirInstr *elem_type)
{
	MirInstrTypeSlice *tmp = CREATE_INSTR(cnt, MIR_INSTR_TYPE_SLICE, node, MirInstrTypeSlice *);
	tmp->base.value.type   = cnt->builtin_types.t_type;
	tmp->base.comptime     = true;
	tmp->elem_type         = elem_type;

	ref_instr(elem_type);
	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_type_vargs(Context *cnt, Ast *node, MirInstr *elem_type)
{
	MirInstrTypeVArgs *tmp = CREATE_INSTR(cnt, MIR_INSTR_TYPE_VARGS, node, MirInstrTypeVArgs *);
	tmp->base.value.type   = cnt->builtin_types.t_type;
	tmp->base.comptime     = true;
	tmp->elem_type         = elem_type;

	ref_instr(elem_type);
	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_arg(Context *cnt, Ast *node, unsigned i)
{
	MirInstrArg *tmp = CREATE_INSTR(cnt, MIR_INSTR_ARG, node, MirInstrArg *);
	tmp->i           = i;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_phi(Context *cnt, Ast *node)
{
	MirInstrPhi *tmp     = CREATE_INSTR(cnt, MIR_INSTR_PHI, node, MirInstrPhi *);
	tmp->incoming_values = create_sarr(SmallArray_InstrPtr, cnt->assembly);
	tmp->incoming_blocks = create_sarr(SmallArray_InstrPtr, cnt->assembly);
	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_compound(Context *cnt, Ast *node, MirInstr *type, SmallArray_InstrPtr *values)
{
	if (values) {
		MirInstr *value;
		SARRAY_FOREACH(values, value) ref_instr(value);
	}
	ref_instr(type);

	MirInstrCompound *tmp = CREATE_INSTR(cnt, MIR_INSTR_COMPOUND, node, MirInstrCompound *);
	tmp->type             = type;
	tmp->values           = values;
	tmp->is_naked         = true;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_cast(Context *cnt, Ast *node, MirInstr *type, MirInstr *next)
{
	ref_instr(type);
	ref_instr(next);
	MirInstrCast *tmp = CREATE_INSTR(cnt, MIR_INSTR_CAST, node, MirInstrCast *);
	tmp->type         = type;
	tmp->expr         = next;
	tmp->auto_cast    = type == NULL;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_sizeof(Context *cnt, Ast *node, MirInstr *expr)
{
	ref_instr(expr);
	MirInstrSizeof *tmp  = CREATE_INSTR(cnt, MIR_INSTR_SIZEOF, node, MirInstrSizeof *);
	tmp->base.value.type = cnt->builtin_types.t_usize;
	tmp->base.comptime   = true;
	tmp->expr            = expr;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
create_instr_type_info(Context *cnt, Ast *node, MirInstr *expr)
{
	ref_instr(expr);
	MirInstrTypeInfo *tmp = CREATE_INSTR(cnt, MIR_INSTR_TYPE_INFO, node, MirInstrTypeInfo *);
	tmp->expr             = expr;
	return &tmp->base;
}

MirInstr *
append_instr_type_info(Context *cnt, Ast *node, MirInstr *expr)
{
	MirInstr *tmp = create_instr_type_info(cnt, node, expr);
	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
append_instr_alignof(Context *cnt, Ast *node, MirInstr *expr)
{
	ref_instr(expr);
	MirInstrAlignof *tmp = CREATE_INSTR(cnt, MIR_INSTR_ALIGNOF, node, MirInstrAlignof *);
	tmp->base.value.type = cnt->builtin_types.t_usize;
	tmp->base.comptime   = true;
	tmp->expr            = expr;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_cond_br(Context *      cnt,
                     Ast *          node,
                     MirInstr *     cond,
                     MirInstrBlock *then_block,
                     MirInstrBlock *else_block)
{
	BL_ASSERT(cond && then_block && else_block);
	ref_instr(cond);
	ref_instr(&then_block->base);
	ref_instr(&else_block->base);
	MirInstrCondBr *tmp  = CREATE_INSTR(cnt, MIR_INSTR_COND_BR, node, MirInstrCondBr *);
	tmp->base.ref_count  = NO_REF_COUNTING;
	tmp->base.value.type = cnt->builtin_types.t_void;
	tmp->cond            = cond;
	tmp->then_block      = then_block;
	tmp->else_block      = else_block;

	MirInstrBlock *block = get_current_block(cnt);

	append_current_block(cnt, &tmp->base);
	if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_br(Context *cnt, Ast *node, MirInstrBlock *then_block)
{
	BL_ASSERT(then_block);
	ref_instr(&then_block->base);
	MirInstrBr *tmp      = CREATE_INSTR(cnt, MIR_INSTR_BR, node, MirInstrBr *);
	tmp->base.ref_count  = NO_REF_COUNTING;
	tmp->base.value.type = cnt->builtin_types.t_void;
	tmp->then_block      = then_block;

	MirInstrBlock *block = get_current_block(cnt);

	append_current_block(cnt, &tmp->base);
	if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
	return &tmp->base;
}

MirInstr *
create_instr_elem_ptr(Context * cnt,
                      Ast *     node,
                      MirInstr *arr_ptr,
                      MirInstr *index,
                      bool      target_is_slice)
{
	BL_ASSERT(arr_ptr && index);
	ref_instr(arr_ptr);
	ref_instr(index);
	MirInstrElemPtr *tmp = CREATE_INSTR(cnt, MIR_INSTR_ELEM_PTR, node, MirInstrElemPtr *);
	tmp->arr_ptr         = arr_ptr;
	tmp->index           = index;
	tmp->target_is_slice = target_is_slice;

	return &tmp->base;
}

MirInstr *
append_instr_elem_ptr(Context * cnt,
                      Ast *     node,
                      MirInstr *arr_ptr,
                      MirInstr *index,
                      bool      target_is_slice)
{
	MirInstr *tmp = create_instr_elem_ptr(cnt, node, arr_ptr, index, target_is_slice);
	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
create_instr_member_ptr(Context *        cnt,
                        Ast *            node,
                        MirInstr *       target_ptr,
                        Ast *            member_ident,
                        ScopeEntry *     scope_entry,
                        MirBuiltinIdKind builtin_id)
{
	ref_instr(target_ptr);
	MirInstrMemberPtr *tmp = CREATE_INSTR(cnt, MIR_INSTR_MEMBER_PTR, node, MirInstrMemberPtr *);
	tmp->target_ptr        = target_ptr;
	tmp->member_ident      = member_ident;
	tmp->scope_entry       = scope_entry;
	tmp->builtin_id        = builtin_id;

	return &tmp->base;
}

MirInstr *
append_instr_member_ptr(Context *        cnt,
                        Ast *            node,
                        MirInstr *       target_ptr,
                        Ast *            member_ident,
                        ScopeEntry *     scope_entry,
                        MirBuiltinIdKind builtin_id)
{
	MirInstr *tmp =
	    create_instr_member_ptr(cnt, node, target_ptr, member_ident, scope_entry, builtin_id);
	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
append_instr_load(Context *cnt, Ast *node, MirInstr *src)
{
	ref_instr(src);
	MirInstrLoad *tmp = CREATE_INSTR(cnt, MIR_INSTR_LOAD, node, MirInstrLoad *);
	tmp->src          = src;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
create_instr_addrof(Context *cnt, Ast *node, MirInstr *src)
{
	ref_instr(src);
	MirInstrAddrOf *tmp = CREATE_INSTR(cnt, MIR_INSTR_ADDROF, node, MirInstrAddrOf *);
	tmp->src            = src;
	return &tmp->base;
}

MirInstr *
append_instr_addrof(Context *cnt, Ast *node, MirInstr *src)
{
	MirInstr *tmp = create_instr_addrof(cnt, node, src);
	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
append_instr_unrecheable(Context *cnt, Ast *node)
{
	MirInstrUnreachable *tmp =
	    CREATE_INSTR(cnt, MIR_INSTR_UNREACHABLE, node, MirInstrUnreachable *);
	tmp->base.value.type = cnt->builtin_types.t_void;
	tmp->base.ref_count  = NO_REF_COUNTING;
	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_fn_proto(Context * cnt,
                      Ast *     node,
                      MirInstr *type,
                      MirInstr *user_type,
                      bool      schedule_analyze)
{
	MirInstrFnProto *tmp    = CREATE_INSTR(cnt, MIR_INSTR_FN_PROTO, node, MirInstrFnProto *);
	tmp->type               = type;
	tmp->user_type          = user_type;
	tmp->base.comptime      = true;
	tmp->base.ref_count     = NO_REF_COUNTING;
	tmp->pushed_for_analyze = schedule_analyze;

	push_into_gscope(cnt, &tmp->base);

	if (schedule_analyze) analyze_push_back(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_decl_ref(Context *   cnt,
                      Ast *       node,
                      Unit *      parent_unit,
                      ID *        rid,
                      Scope *     scope,
                      ScopeEntry *scope_entry)
{
	BL_ASSERT(scope && rid);
	MirInstrDeclRef *tmp = CREATE_INSTR(cnt, MIR_INSTR_DECL_REF, node, MirInstrDeclRef *);
	tmp->scope_entry     = scope_entry;
	tmp->scope           = scope;
	tmp->rid             = rid;
	tmp->parent_unit     = parent_unit;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_decl_direct_ref(Context *cnt, MirInstr *ref)
{
	BL_ASSERT(ref);
	ref_instr(ref);
	MirInstrDeclDirectRef *tmp =
	    CREATE_INSTR(cnt, MIR_INSTR_DECL_DIRECT_REF, NULL, MirInstrDeclDirectRef *);
	tmp->ref = ref;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_call(Context *cnt, Ast *node, MirInstr *callee, SmallArray_InstrPtr *args)
{
	BL_ASSERT(callee);
	MirInstrCall *tmp = CREATE_INSTR(cnt, MIR_INSTR_CALL, node, MirInstrCall *);
	tmp->args         = args;
	tmp->callee       = callee;

	ref_instr(&tmp->base);

	/* Callee must be referenced even if we call no-ref counted fn_proto instructions, because
	 * sometimes callee is declaration reference poining to variable containing pointer to some
	 * function. */
	ref_instr(callee);

	/* reference all arguments */
	if (args) {
		MirInstr *instr;
		SARRAY_FOREACH(args, instr) ref_instr(instr);
	}

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_decl_var(Context * cnt,
                      Ast *     node,
                      MirInstr *type,
                      MirInstr *init,
                      bool      is_mutable,
                      bool      is_in_gscope,
                      s32       order,
                      u32       flags)
{
	ref_instr(type);
	ref_instr(init);
	MirInstrDeclVar *tmp = CREATE_INSTR(cnt, MIR_INSTR_DECL_VAR, node, MirInstrDeclVar *);
	tmp->base.ref_count  = NO_REF_COUNTING;
	tmp->base.value.type = cnt->builtin_types.t_void;
	tmp->type            = type;
	tmp->init            = init;

	tmp->var = create_var(cnt,
	                      node,
	                      node->owner_scope,
	                      &node->data.ident.id,
	                      NULL,
	                      is_mutable,
	                      is_in_gscope,
	                      flags);

	if (is_in_gscope) {
		push_into_gscope(cnt, &tmp->base);
		analyze_push_back(cnt, &tmp->base);
	} else {
		append_current_block(cnt, &tmp->base);
	}

	if (init && init->kind == MIR_INSTR_COMPOUND) {
		((MirInstrCompound *)init)->is_naked = false;
	}

	return &tmp->base;
}

MirInstr *
append_instr_decl_var_impl(Context *   cnt,
                           const char *name,
                           MirInstr *  type,
                           MirInstr *  init,
                           bool        is_mutable,
                           bool        is_in_gscope,
                           s32         order,
                           u32         flags)
{
	ref_instr(type);
	ref_instr(init);
	MirInstrDeclVar *tmp = CREATE_INSTR(cnt, MIR_INSTR_DECL_VAR, NULL, MirInstrDeclVar *);
	tmp->base.ref_count  = NO_REF_COUNTING;
	tmp->base.value.type = cnt->builtin_types.t_void;
	tmp->type            = type;
	tmp->init            = init;

	tmp->var = create_var_impl(cnt, name, NULL, is_mutable, is_in_gscope, false);

	if (is_in_gscope) {
		push_into_gscope(cnt, &tmp->base);
		analyze_push_back(cnt, &tmp->base);
	} else {
		append_current_block(cnt, &tmp->base);
	}

	if (init && init->kind == MIR_INSTR_COMPOUND) {
		((MirInstrCompound *)init)->is_naked = false;
	}

	return &tmp->base;
}

MirInstr *
append_instr_decl_member(Context *cnt, Ast *node, MirInstr *type)
{
	ref_instr(type);
	MirInstrDeclMember *tmp =
	    CREATE_INSTR(cnt, MIR_INSTR_DECL_MEMBER, node, MirInstrDeclMember *);
	tmp->base.ref_count  = NO_REF_COUNTING;
	tmp->base.comptime   = true;
	tmp->base.value.type = cnt->builtin_types.t_void;
	tmp->type            = type;

	ID *id      = node ? &node->data.ident.id : NULL;
	tmp->member = create_member(cnt, node, id, NULL, -1, NULL);

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_decl_arg(Context *cnt, Ast *node, MirInstr *type)
{
	ref_instr(type);
	MirInstrDeclArg *tmp = CREATE_INSTR(cnt, MIR_INSTR_DECL_ARG, node, MirInstrDeclArg *);

	tmp->base.ref_count  = NO_REF_COUNTING;
	tmp->base.comptime   = true;
	tmp->base.value.type = cnt->builtin_types.t_void;
	tmp->type            = type;

	ID *id   = node ? &node->data.ident.id : NULL;
	tmp->arg = create_arg(cnt, node, id, NULL, NULL);

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_decl_variant(Context *cnt, Ast *node, MirInstr *value)
{
	MirInstrDeclVariant *tmp =
	    CREATE_INSTR(cnt, MIR_INSTR_DECL_VARIANT, node, MirInstrDeclVariant *);

	tmp->base.ref_count  = NO_REF_COUNTING;
	tmp->base.comptime   = true;
	tmp->base.value.type = cnt->builtin_types.t_void;
	tmp->value           = value;

	BL_ASSERT(node && node->kind == AST_IDENT);
	ID *   id    = &node->data.ident.id;
	Scope *scope = node->owner_scope;
	tmp->variant = create_variant(cnt, id, scope, NULL);

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

static MirInstr *
create_instr_const_int(Context *cnt, Ast *node, MirType *type, u64 val)
{
	MirInstr *tmp         = CREATE_INSTR(cnt, MIR_INSTR_CONST, node, MirInstr *);
	tmp->comptime         = true;
	tmp->value.type       = type;
	tmp->value.data.v_u64 = val;

	return tmp;
}

MirInstr *
append_instr_const_int(Context *cnt, Ast *node, MirType *type, u64 val)
{
	MirInstr *tmp         = CREATE_INSTR(cnt, MIR_INSTR_CONST, node, MirInstr *);
	tmp->comptime         = true;
	tmp->value.type       = type;
	tmp->value.data.v_u64 = val;

	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
append_instr_const_float(Context *cnt, Ast *node, float val)
{
	MirInstr *tmp   = CREATE_INSTR(cnt, MIR_INSTR_CONST, node, MirInstr *);
	tmp->comptime   = true;
	tmp->value.type = cnt->builtin_types.t_f32;
	// memcpy(&tmp->const_value.data, &val, sizeof(f32));
	tmp->value.data.v_f32 = val;

	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
append_instr_const_double(Context *cnt, Ast *node, double val)
{
	MirInstr *tmp         = CREATE_INSTR(cnt, MIR_INSTR_CONST, node, MirInstr *);
	tmp->comptime         = true;
	tmp->value.type       = cnt->builtin_types.t_f64;
	tmp->value.data.v_f64 = val;

	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
append_instr_const_bool(Context *cnt, Ast *node, bool val)
{
	MirInstr *tmp          = CREATE_INSTR(cnt, MIR_INSTR_CONST, node, MirInstr *);
	tmp->comptime          = true;
	tmp->value.type        = cnt->builtin_types.t_bool;
	tmp->value.data.v_bool = val;

	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
append_instr_const_string(Context *cnt, Ast *node, const char *str)
{
	MirInstr *tmp = CREATE_INSTR(cnt, MIR_INSTR_CONST, node, MirInstr *);
	tmp->comptime = true;

	init_or_create_const_string(cnt, &tmp->value, str);

	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
append_instr_const_char(Context *cnt, Ast *node, char c)
{
	MirInstr *tmp          = CREATE_INSTR(cnt, MIR_INSTR_CONST, node, MirInstr *);
	tmp->comptime          = true;
	tmp->value.type        = cnt->builtin_types.t_u8;
	tmp->value.data.v_char = c;

	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
append_instr_const_null(Context *cnt, Ast *node)
{
	MirInstr *tmp   = CREATE_INSTR(cnt, MIR_INSTR_CONST, node, MirInstr *);
	tmp->comptime   = true;
	tmp->value.type = create_type_null(cnt, cnt->builtin_types.t_u8_ptr);

	mir_set_const_ptr(&tmp->value.data.v_ptr, NULL, MIR_CP_VALUE);

	append_current_block(cnt, tmp);
	return tmp;
}

MirInstr *
append_instr_ret(Context *cnt, Ast *node, MirInstr *value, bool infer_type)
{
	if (value) ref_instr(value);

	MirInstrRet *tmp     = CREATE_INSTR(cnt, MIR_INSTR_RET, node, MirInstrRet *);
	tmp->base.value.type = cnt->builtin_types.t_void;
	tmp->base.ref_count  = NO_REF_COUNTING;
	tmp->value           = value;
	tmp->infer_type      = infer_type;

	MirInstrBlock *block = get_current_block(cnt);

	append_current_block(cnt, &tmp->base);
	if (!is_block_terminated(block)) terminate_block(block, &tmp->base);

	MirFn *fn = block->owner_fn;
	BL_ASSERT(fn);

	fn->terminal_instr = tmp;

	return &tmp->base;
}

MirInstr *
append_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest)
{
	BL_ASSERT(src && dest);
	ref_instr(src);
	ref_instr(dest);

	MirInstrStore *tmp   = CREATE_INSTR(cnt, MIR_INSTR_STORE, node, MirInstrStore *);
	tmp->base.value.type = cnt->builtin_types.t_void;
	tmp->base.ref_count  = NO_REF_COUNTING;
	tmp->src             = src;
	tmp->dest            = dest;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op)
{
	BL_ASSERT(lhs && rhs);
	ref_instr(lhs);
	ref_instr(rhs);
	MirInstrBinop *tmp = CREATE_INSTR(cnt, MIR_INSTR_BINOP, node, MirInstrBinop *);
	tmp->lhs           = lhs;
	tmp->rhs           = rhs;
	tmp->op            = op;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
append_instr_unop(Context *cnt, Ast *node, MirInstr *instr, UnopKind op)
{
	BL_ASSERT(instr);
	ref_instr(instr);
	MirInstrUnop *tmp = CREATE_INSTR(cnt, MIR_INSTR_UNOP, node, MirInstrUnop *);
	tmp->expr         = instr;
	tmp->op           = op;

	append_current_block(cnt, &tmp->base);
	return &tmp->base;
}

MirInstr *
create_instr_vargs_impl(Context *cnt, MirType *type, SmallArray_InstrPtr *values)
{
	BL_ASSERT(type);
	MirInstrVArgs *tmp = CREATE_INSTR(cnt, MIR_INSTR_VARGS, NULL, MirInstrVArgs *);
	tmp->type          = type;
	tmp->values        = values;

	return &tmp->base;
}

/* analyze */
void
erase_instr_tree(MirInstr *instr)
{
	if (!instr) return;

	SmallArray_InstrPtr64 queue;
	sa_init(&queue);

	sa_push_InstrPtr64(&queue, instr);

	MirInstr *top;
	while (queue.size) {
		top = sa_pop_InstrPtr64(&queue);

		if (!top) continue;

		BL_ASSERT(top->analyzed && "Trying to erase not analyzed instruction.");
		if (top->ref_count == NO_REF_COUNTING) continue;
		if (top->ref_count > 0) continue;

		switch (top->kind) {
		case MIR_INSTR_BINOP: {
			MirInstrBinop *binop = (MirInstrBinop *)top;
			unref_instr(binop->lhs);
			unref_instr(binop->rhs);

			sa_push_InstrPtr64(&queue, binop->rhs);
			sa_push_InstrPtr64(&queue, binop->lhs);
			break;
		}

		case MIR_INSTR_LOAD: {
			MirInstrLoad *load = (MirInstrLoad *)top;
			unref_instr(load->src);

			sa_push_InstrPtr64(&queue, load->src);
			break;
		}

		case MIR_INSTR_SIZEOF: {
			MirInstrSizeof *szof = (MirInstrSizeof *)top;
			unref_instr(szof->expr);

			sa_push_InstrPtr64(&queue, szof->expr);
			break;
		}

		case MIR_INSTR_ELEM_PTR: {
			MirInstrElemPtr *ep = (MirInstrElemPtr *)top;
			unref_instr(ep->arr_ptr);
			unref_instr(ep->index);

			sa_push_InstrPtr64(&queue, ep->arr_ptr);
			sa_push_InstrPtr64(&queue, ep->index);
			break;
		}

		case MIR_INSTR_MEMBER_PTR: {
			MirInstrMemberPtr *mp = (MirInstrMemberPtr *)top;
			unref_instr(mp->target_ptr);

			sa_push_InstrPtr64(&queue, mp->target_ptr);
			break;
		}

		case MIR_INSTR_TYPE_INFO: {
			MirInstrTypeInfo *info = (MirInstrTypeInfo *)top;
			unref_instr(info->expr);

			sa_push_InstrPtr64(&queue, info->expr);
			break;
		}

		case MIR_INSTR_CAST: {
			MirInstrCast *cast = (MirInstrCast *)top;
			unref_instr(cast->expr);
			unref_instr(cast->type);

			sa_push_InstrPtr64(&queue, cast->expr);
			sa_push_InstrPtr64(&queue, cast->type);
			break;
		}

		case MIR_INSTR_CALL: {
			MirInstrCall *call = (MirInstrCall *)top;
			if (call->args) {
				MirInstr *it;
				SARRAY_FOREACH(call->args, it)
				{
					unref_instr(it);
					sa_push_InstrPtr64(&queue, it);
				}
			}
			break;
		}

		case MIR_INSTR_ADDROF: {
			MirInstrAddrOf *addrof = (MirInstrAddrOf *)top;
			unref_instr(addrof->src);
			sa_push_InstrPtr64(&queue, addrof->src);
			break;
		}

		case MIR_INSTR_UNOP: {
			MirInstrUnop *unop = (MirInstrUnop *)top;
			unref_instr(unop->expr);
			sa_push_InstrPtr64(&queue, unop->expr);
			break;
		}

		case MIR_INSTR_TYPE_PTR: {
			MirInstrTypePtr *tp = (MirInstrTypePtr *)top;
			unref_instr(tp->type);
			sa_push_InstrPtr64(&queue, tp->type);
			break;
		}

		case MIR_INSTR_TYPE_ENUM: {
			MirInstrTypeEnum *te = (MirInstrTypeEnum *)top;
			unref_instr(te->base_type);
			sa_push_InstrPtr64(&queue, te->base_type);

			MirInstr *it;
			SARRAY_FOREACH(te->variants, it)
			{
				unref_instr(it);
				sa_push_InstrPtr64(&queue, it);
			}
			break;
		}

		case MIR_INSTR_TYPE_FN: {
			MirInstrTypeFn *tf = (MirInstrTypeFn *)top;
			unref_instr(tf->ret_type);
			sa_push_InstrPtr64(&queue, tf->ret_type);

			if (tf->args) {
				MirInstr *it;
				SARRAY_FOREACH(tf->args, it)
				{
					unref_instr(it);
					sa_push_InstrPtr64(&queue, it);
				}
			}
			break;
		}

		case MIR_INSTR_TYPE_ARRAY: {
			MirInstrTypeArray *ta = (MirInstrTypeArray *)top;
			unref_instr(ta->elem_type);
			unref_instr(ta->len);
			sa_push_InstrPtr64(&queue, ta->elem_type);
			sa_push_InstrPtr64(&queue, ta->len);
			break;
		}

		case MIR_INSTR_TYPE_SLICE:
		case MIR_INSTR_TYPE_STRUCT: {
			MirInstrTypeStruct *ts = (MirInstrTypeStruct *)top;

			if (ts->members) {
				MirInstr *it;
				SARRAY_FOREACH(ts->members, it)
				{
					unref_instr(it);
					sa_push_InstrPtr64(&queue, it);
				}
			}
			break;
		}

		case MIR_INSTR_VARGS: {
			MirInstrVArgs *vargs = (MirInstrVArgs *)top;
			if (vargs->values) {
				MirInstr *it;
				SARRAY_FOREACH(vargs->values, it)
				{
					unref_instr(it);
					sa_push_InstrPtr64(&queue, it);
				}
			}
			break;
		}

		case MIR_INSTR_BLOCK:
			continue;

		case MIR_INSTR_DECL_REF:
		case MIR_INSTR_CONST:
			break;

		default:
			BL_ABORT("Missing erase for instruction '%s'", mir_instr_name(top));
		}

		erase_instr(top);
	}

	sa_terminate(&queue);
}

void
reduce_instr(Context *cnt, MirInstr *instr)
{
	if (!instr) return;
	/* instruction unknown in compile time cannot be reduced */
	if (!instr->comptime && instr->kind != MIR_INSTR_COMPOUND) return;

	switch (instr->kind) {
	case MIR_INSTR_CONST:
	case MIR_INSTR_DECL_MEMBER:
	case MIR_INSTR_DECL_VARIANT:
	case MIR_INSTR_DECL_ARG:
	case MIR_INSTR_TYPE_FN:
	case MIR_INSTR_TYPE_ARRAY:
	case MIR_INSTR_TYPE_PTR:
	case MIR_INSTR_TYPE_STRUCT:
	case MIR_INSTR_TYPE_SLICE:
	case MIR_INSTR_TYPE_VARGS:
	case MIR_INSTR_TYPE_ENUM:
	case MIR_INSTR_SIZEOF:
	case MIR_INSTR_ALIGNOF:
	case MIR_INSTR_MEMBER_PTR: {
		erase_instr(instr);
		break;
	}

	case MIR_INSTR_COMPOUND: {
		if (!((MirInstrCompound *)instr)->is_naked) erase_instr(instr);
		break;
	}

	case MIR_INSTR_BINOP: {
		vm_execute_instr(&cnt->vm, instr);
		erase_instr(instr);
		break;
	}

	case MIR_INSTR_UNOP: {
		vm_execute_instr(&cnt->vm, instr);
		erase_instr(instr);
		break;
	}

	case MIR_INSTR_CAST: {
		vm_execute_instr(&cnt->vm, instr);
		erase_instr(instr);
		break;
	}

	case MIR_INSTR_LOAD: {
		vm_execute_instr(&cnt->vm, instr);
		erase_instr(instr);
		break;
	}

	case MIR_INSTR_DECL_REF: {
		vm_execute_instr(&cnt->vm, instr);
		erase_instr(instr);
		break;
	}

	case MIR_INSTR_DECL_DIRECT_REF: {
		vm_execute_instr(&cnt->vm, instr);
		erase_instr(instr);
		break;
	}

	case MIR_INSTR_ADDROF: {
		vm_execute_instr(&cnt->vm, instr);
		erase_instr(instr);
		break;
	}

	default:
		break;
	}
}

AnalyzeResult
analyze_resolve_type(Context *cnt, MirInstr *resolver_call, MirType **out_type)
{
	BL_ASSERT(resolver_call && "Expected resolver call.");
	BL_ASSERT(resolver_call->kind == MIR_INSTR_CALL &&
	          "Type resolver is expected to be call to resolve function.");

	if (analyze_instr(cnt, resolver_call).state != ANALYZE_PASSED)
		return ANALYZE_RESULT(POSTPONE, 0);

	if (vm_execute_instr_top_level_call(&cnt->vm, (MirInstrCall *)resolver_call)) {
		*out_type = resolver_call->value.data.v_ptr.data.type;
		return ANALYZE_RESULT(PASSED, 0);
	} else {
		return ANALYZE_RESULT(FAILED, 0);
	}
}

AnalyzeResult
analyze_instr_toany(Context *cnt, MirInstrToAny *toany)
{
	MirType *toany_type = mir_deref_type(toany->base.value.type);
	BL_ASSERT(toany->expr && "Missing expression as toany input.");

	reduce_instr(cnt, toany->expr);

	MirInstr *expr      = toany->expr;
	MirType * rtti_type = expr->value.type;

	// HACK: Generate tmp rather only for comptime expresions???
	// HACK: Generate tmp rather only for comptime expresions???
	// HACK: Generate tmp rather only for comptime expresions???
	if (!is_allocated_object(expr)) {
		/* Target expression is not allocated object on the stack, so we need to crate
		 * temporary variable containing the value and fetch pointer to this variable. */
		const char *tmp_var_name = gen_uq_name(IMPL_ANY_EXPR_TMP);
		toany->expr_tmp =
		    create_var_impl(cnt, tmp_var_name, rtti_type, false, false, false);
	} else if (is_load_needed(expr)) {
		rtti_type = mir_deref_type(rtti_type);
	}

	BL_ASSERT(rtti_type);
	schedule_RTTI_generation(cnt, rtti_type);

	{ /* Tmp variable for Any */
		const char *tmp_var_name = gen_uq_name(IMPL_ANY_TMP);
		toany->tmp = create_var_impl(cnt, tmp_var_name, toany_type, false, false, false);
	}

	toany->has_data = rtti_type->kind != MIR_TYPE_VOID && rtti_type->kind != MIR_TYPE_NULL;

	/* When we pass type declaration reference as an expression into the toany instruction we
	 * need include pointer to type info of the real type passed. That means when we pass 's32'
	 * we get resulting any structure containing type info for Type and type info for s32 as
	 * data pointer. This is how we can later implement for example printing of type layout. */
	if (rtti_type->kind == MIR_TYPE_TYPE) {
		MirConstPtr *cp                 = &expr->value.data.v_ptr;
		MirType *    specification_type = NULL;

		/* HACK: There is probably better solution, here we handle situation when type is
		 * not fundamental type but custom type declaration where actual type is stored in
		 * constant variable. */
		if (cp->kind == MIR_CP_TYPE) {
			specification_type = cp->data.type;
		} else if (cp->kind == MIR_CP_VAR) {
			specification_type = cp->data.var->value.data.v_ptr.data.type;
		}

		BL_ASSERT(specification_type);
		schedule_RTTI_generation(cnt, specification_type);
		toany->rtti_type_specification = specification_type;
	}

	toany->rtti_type = rtti_type;

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_phi(Context *cnt, MirInstrPhi *phi)
{
	BL_ASSERT(phi->incoming_blocks && phi->incoming_values);
	BL_ASSERT(phi->incoming_values->size == phi->incoming_blocks->size);

	const size_t count = phi->incoming_values->size;

	bool       comptime = true;
	MirInstr **value_ref;
	MirInstr * block;
	MirType *  type = NULL;

	for (size_t i = 0; i < count; ++i) {
		value_ref = &phi->incoming_values->data[i];
		block     = phi->incoming_blocks->data[i];
		BL_ASSERT(block && block->kind == MIR_INSTR_BLOCK)

		const AnalyzeSlotConfig *conf =
		    type ? &analyze_slot_conf_default : &analyze_slot_conf_basic;

		if (analyze_slot(cnt, conf, value_ref, type) != ANALYZE_PASSED)
			return ANALYZE_RESULT(FAILED, 0);

		if (!type) type = (*value_ref)->value.type;

		comptime &= (*value_ref)->comptime;
	}

	BL_ASSERT(type && "Cannot resolve type of phi instruction!");
	phi->base.value.type      = type;
	phi->base.value.addr_mode = MIR_VAM_RVALUE;
	phi->base.comptime        = comptime;

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_compound(Context *cnt, MirInstrCompound *cmp)
{
	/* Setup compound type. */
	MirType *            type   = cmp->base.value.type;
	SmallArray_InstrPtr *values = cmp->values;
	if (!type) {
		/* generate load instruction if needed */
		BL_ASSERT(cmp->type->analyzed);
		if (analyze_slot(cnt, &analyze_slot_conf_basic, &cmp->type, NULL) != ANALYZE_PASSED)
			return ANALYZE_RESULT(FAILED, 0);

		MirInstr *instr_type = cmp->type;
		if (instr_type->value.type->kind != MIR_TYPE_TYPE) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_TYPE,
			            instr_type->node->location,
			            BUILDER_CUR_WORD,
			            "Expected type before compound expression.");
			return ANALYZE_RESULT(FAILED, 0);
		}
		type = instr_type->value.data.v_ptr.data.type;
	}

	BL_ASSERT(type);

	if (!values) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_INITIALIZER,
		            cmp->type->node->location,
		            BUILDER_CUR_AFTER,
		            "Expected value after ':'.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	cmp->base.value.type = type;
	cmp->base.comptime   = true; /* can be overriden later */

	/* Check if array is supposed to be initilialized to {0} */
	if (values->size == 1) {
		MirInstr *value = values->data[0];
		if (value->kind == MIR_INSTR_CONST && value->value.type->kind == MIR_TYPE_INT &&
		    value->value.data.v_u64 == 0) {
			reduce_instr(cnt, value);
			cmp->is_zero_initialized = true;
		}
	}

	switch (type->kind) {
	case MIR_TYPE_ARRAY: {
		if (cmp->is_zero_initialized) {
			cmp->base.value.data.v_array.is_zero_initializer = true;
			break;
		}

		if (values->size != (size_t)type->data.array.len) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_INITIALIZER,
			            cmp->base.node->location,
			            BUILDER_CUR_WORD,
			            "Array initializer must explicitly set all array elements of "
			            "the array or "
			            "initialize array to 0 by zero initializer {0}. Expected is "
			            "%llu but given %llu.",
			            (unsigned long long)type->data.array.len,
			            (unsigned long long)values->size);
			return ANALYZE_RESULT(FAILED, 0);
		}

		/* Else iterate over values */
		MirInstr **value_ref;
		for (size_t i = 0; i < values->size; ++i) {
			value_ref = &values->data[i];

			if (analyze_slot(cnt,
			                 &analyze_slot_conf_default,
			                 value_ref,
			                 type->data.array.elem_type) != ANALYZE_PASSED)
				return ANALYZE_RESULT(FAILED, 0);

			cmp->base.comptime = (*value_ref)->comptime ? cmp->base.comptime : false;
		}

		// NOTE: Instructions can be used as values!!!
		if (cmp->base.comptime)
			init_or_create_const_array(cnt,
			                           &cmp->base.value,
			                           type->data.array.elem_type,
			                           (SmallArray_ConstValuePtr *)values);
		break;
	}

	case MIR_TYPE_SLICE:
	case MIR_TYPE_STRING:
	case MIR_TYPE_VARGS:
	case MIR_TYPE_STRUCT: {
		if (cmp->is_zero_initialized) {
			cmp->base.value.data.v_struct.is_zero_initializer = true;
			break;
		}

		const size_t memc = type->data.strct.members->size;
		if (values->size != memc) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_INITIALIZER,
			            cmp->base.node->location,
			            BUILDER_CUR_WORD,
			            "Structure initializer must explicitly set all members of the "
			            "structure or initialize structure to 0 by zero initializer "
			            "{0}. Expected is %llu but given %llu.",
			            (unsigned long long)memc,
			            (unsigned long long)values->size);
			return ANALYZE_RESULT(FAILED, 0);
		}

		/* Else iterate over values */
		MirInstr **value_ref;
		MirType *  member_type;
		for (u32 i = 0; i < values->size; ++i) {
			value_ref   = &values->data[i];
			member_type = mir_get_struct_elem_type(type, i);

			if (analyze_slot(cnt, &analyze_slot_conf_default, value_ref, member_type) !=
			    ANALYZE_PASSED)
				return ANALYZE_RESULT(FAILED, 0);

			cmp->base.comptime = (*value_ref)->comptime ? cmp->base.comptime : false;
		}

		// NOTE: Instructions can be used as values!!!
		if (cmp->base.comptime)
			init_or_create_const_struct(
			    cnt, &cmp->base.value, type, (SmallArray_ConstValuePtr *)values);
		break;
	}

	default: {
		/* Non-agregate type. */
		if (values->size > 1) {
			MirInstr *value = values->data[1];
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_INITIALIZER,
			            value->node->location,
			            BUILDER_CUR_WORD,
			            "One value only is expected for non-agragate types.");
			return ANALYZE_RESULT(FAILED, 0);
		}

		MirInstr **value_ref = &values->data[0];

		const AnalyzeSlotConfig *conf =
		    type ? &analyze_slot_conf_default : &analyze_slot_conf_basic;

		if (analyze_slot(cnt, conf, value_ref, type) != ANALYZE_PASSED)
			return ANALYZE_RESULT(FAILED, 0);

		cmp->base.comptime = (*value_ref)->comptime;
		if (cmp->base.comptime) cmp->base.value = (*value_ref)->value;
	}
	}

	if (!cmp->base.comptime && cmp->is_naked) {
		/* For naked non-compile time compounds we need to generate implicit temp storage to
		 * keep all data. */

		const char *tmp_name = gen_uq_name(IMPL_COMPOUND_TMP);
		MirVar *    tmp_var  = create_var_impl(cnt, tmp_name, type, true, false, false);
		cmp->tmp_var         = tmp_var;
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_vargs(Context *cnt, MirInstrVArgs *vargs)
{
	MirType *            type   = vargs->type;
	SmallArray_InstrPtr *values = vargs->values;
	BL_ASSERT(type && values);

	type = create_type_struct_special(cnt, MIR_TYPE_VARGS, NULL, create_type_ptr(cnt, type));

	const size_t valc = values->size;

	if (valc > 0) {
		/* Prepare tmp array for values */
		const char *tmp_name = gen_uq_name(IMPL_VARGS_TMP_ARR);
		MirType *   tmp_type = create_type_array(cnt, vargs->type, valc);
		vargs->arr_tmp       = create_var_impl(cnt, tmp_name, tmp_type, true, false, false);
	}

	{
		/* Prepare tmp slice for vargs */
		const char *tmp_name = gen_uq_name(IMPL_VARGS_TMP);
		vargs->vargs_tmp     = create_var_impl(cnt, tmp_name, type, true, false, false);
	}

	MirInstr **value;
	bool       is_valid = true;

	for (size_t i = 0; i < valc && is_valid; ++i) {
		value = &values->data[i];

		if (analyze_slot(cnt, &analyze_slot_conf_full, value, vargs->type) !=
		    ANALYZE_PASSED)
			return ANALYZE_RESULT(FAILED, 0);
	}

	vargs->base.value.type = type;
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
	if (analyze_slot(
	        cnt, &analyze_slot_conf_default, &elem_ptr->index, cnt->builtin_types.t_s64) !=
	    ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	MirInstr *arr_ptr = elem_ptr->arr_ptr;
	BL_ASSERT(arr_ptr);
	BL_ASSERT(arr_ptr->value.type);

	if (!mir_is_pointer_type(arr_ptr->value.type)) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_TYPE,
		            elem_ptr->arr_ptr->node->location,
		            BUILDER_CUR_WORD,
		            "Expected array type or slice.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	MirType *arr_type = mir_deref_type(arr_ptr->value.type);
	BL_ASSERT(arr_type);

	if (arr_type->kind == MIR_TYPE_ARRAY) {
		/* array */
		if (elem_ptr->index->comptime) {
			const s64 len = arr_type->data.array.len;
			const s64 i   = elem_ptr->index->value.data.v_u64;
			if (i >= len || i < 0) {
				builder_msg(BUILDER_MSG_ERROR,
				            ERR_BOUND_CHECK_FAILED,
				            elem_ptr->index->node->location,
				            BUILDER_CUR_WORD,
				            "Array index is out of the bounds, array size is %lli "
				            "so index must fit in range from 0 to %lli.",
				            len,
				            len - 1);
				return ANALYZE_RESULT(FAILED, 0);
			}
		}

		/* setup ElemPtr instruction const_value type */
		MirType *elem_type = arr_type->data.array.elem_type;
		BL_ASSERT(elem_type);
		elem_ptr->base.value.type = create_type_ptr(cnt, elem_type);
	} else if (arr_type->kind == MIR_TYPE_SLICE || arr_type->kind == MIR_TYPE_STRING ||
	           arr_type->kind == MIR_TYPE_VARGS) {
		/* Support of direct slice access -> slice[N]
		 * Since slice is special kind of structure data we need to handle
		 * access to pointer and lenght later during execuion. We cannot create
		 * member poiner instruction here because we need check boundaries on
		 * array later during runtime. This leads to special kind of elemptr
		 * interpretation and IR generation also.
		 */

		/* setup type */
		MirType *elem_type = mir_get_struct_elem_type(arr_type, MIR_SLICE_PTR_INDEX);
		BL_ASSERT(elem_type);
		elem_ptr->base.value.type = elem_type;

		/* this is important!!! */
		elem_ptr->target_is_slice = true;
	} else {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_TYPE,
		            arr_ptr->node->location,
		            BUILDER_CUR_WORD,
		            "Expected array or slice type.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	reduce_instr(cnt, elem_ptr->arr_ptr);
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
	MirInstr *target_ptr = member_ptr->target_ptr;
	BL_ASSERT(target_ptr);
	MirType *target_type = target_ptr->value.type;

	if (target_type->kind != MIR_TYPE_PTR) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_TYPE,
		            target_ptr->node->location,
		            BUILDER_CUR_WORD,
		            "Expected structure type.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	Ast *ast_member_ident = member_ptr->member_ident;

	target_type = mir_deref_type(target_type);

	/* Array type */
	if (target_type->kind == MIR_TYPE_ARRAY) {
		/* check array builtin members */
		if (member_ptr->builtin_id == MIR_BUILTIN_ID_ARR_LEN ||
		    is_builtin(ast_member_ident, MIR_BUILTIN_ID_ARR_LEN)) {
			/* .len */
			/* mutate instruction into constant */
			unref_instr(member_ptr->target_ptr);
			erase_instr_tree(member_ptr->target_ptr);
			MirInstr *len         = mutate_instr(&member_ptr->base, MIR_INSTR_CONST);
			len->comptime         = true;
			len->value.type       = cnt->builtin_types.t_s64;
			len->value.data.v_u64 = target_type->data.array.len;
		} else if (member_ptr->builtin_id == MIR_BUILTIN_ID_ARR_PTR ||
		           is_builtin(ast_member_ident, MIR_BUILTIN_ID_ARR_PTR)) {
			/* .ptr -> This will be replaced by:
			 *     elemptr
			 *     addrof
			 * to match syntax: &array[0]
			 */

			MirInstr *index =
			    create_instr_const_int(cnt, NULL, cnt->builtin_types.t_s64, 0);
			MirInstr *elem_ptr =
			    create_instr_elem_ptr(cnt, NULL, target_ptr, index, false);
			ref_instr(elem_ptr);

			insert_instr_before(&member_ptr->base, elem_ptr);

			analyze_instr_rq(cnt, index);
			analyze_instr_rq(cnt, elem_ptr);

			MirInstrAddrOf *addrof_elem =
			    (MirInstrAddrOf *)mutate_instr(&member_ptr->base, MIR_INSTR_ADDROF);
			addrof_elem->src = elem_ptr;
			analyze_instr_rq(cnt, &addrof_elem->base);
		} else {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_MEMBER_ACCESS,
			            ast_member_ident->location,
			            BUILDER_CUR_WORD,
			            "Unknown member.");
			return ANALYZE_RESULT(FAILED, 0);
		}

		return ANALYZE_RESULT(PASSED, 0);
	}

	if (target_type->kind == MIR_TYPE_PTR) {
		/* We try to access structure member via pointer so we need one more load.
		 */

		member_ptr->target_ptr = insert_instr_load(cnt, member_ptr->target_ptr);
		BL_ASSERT(member_ptr->target_ptr);
		target_type = mir_deref_type(target_type);
	}

	/* struct type */
	if (target_type->kind == MIR_TYPE_STRUCT || target_type->kind == MIR_TYPE_STRING ||
	    target_type->kind == MIR_TYPE_SLICE || target_type->kind == MIR_TYPE_VARGS) {
		reduce_instr(cnt, member_ptr->target_ptr);
		/* lookup for member inside struct */
		Scope *     scope = target_type->data.strct.scope;
		ID *        rid   = &ast_member_ident->data.ident.id;
		ScopeEntry *found = scope_lookup(scope, rid, false, true);
		if (!found) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_UNKNOWN_SYMBOL,
			            member_ptr->member_ident->location,
			            BUILDER_CUR_WORD,
			            "Unknown structure member.");
			return ANALYZE_RESULT(FAILED, 0);
		}

		{
			BL_ASSERT(found->kind == SCOPE_ENTRY_MEMBER);
			MirMember *member = found->data.member;

			/* setup member_ptr type */
			MirType *type = create_type_ptr(cnt, member->type);
			BL_ASSERT(type);
			member_ptr->base.value.type = type;
		}

		member_ptr->scope_entry = found;

		return ANALYZE_RESULT(PASSED, 0);
	}

	/* Sub type member. */
	if (target_type->kind == MIR_TYPE_TYPE) {
		/* generate load instruction if needed */

		if (analyze_slot(cnt, &analyze_slot_conf_basic, &member_ptr->target_ptr, NULL) !=
		    ANALYZE_PASSED) {
			return ANALYZE_RESULT(FAILED, 0);
		}

		MirType *sub_type = member_ptr->target_ptr->value.data.v_ptr.data.type;
		BL_ASSERT(sub_type);

		if (sub_type->kind != MIR_TYPE_ENUM) {
			goto INVALID;
		}

		/* lookup for member inside struct */
		Scope *     scope = sub_type->data.enm.scope;
		ID *        rid   = &ast_member_ident->data.ident.id;
		ScopeEntry *found = scope_lookup(scope, rid, false, true);
		if (!found) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_UNKNOWN_SYMBOL,
			            member_ptr->member_ident->location,
			            BUILDER_CUR_WORD,
			            "Unknown enumerator variant.");
			return ANALYZE_RESULT(FAILED, 0);
		}

		{
			BL_ASSERT(found->kind == SCOPE_ENTRY_VARIANT);
			MirVariant *variant = found->data.variant;
			BL_ASSERT(variant);
			member_ptr->base.value.data      = variant->value->data;
			member_ptr->base.value.addr_mode = MIR_VAM_LVALUE_CONST;
		}

		member_ptr->scope_entry     = found;
		member_ptr->base.value.type = sub_type;
		member_ptr->base.comptime   = true;

		return ANALYZE_RESULT(PASSED, 0);
	}

	/* Invalid */
INVALID:
	builder_msg(BUILDER_MSG_ERROR,
	            ERR_INVALID_MEMBER_ACCESS,
	            target_ptr->node->location,
	            BUILDER_CUR_WORD,
	            "Expected structure or enumerator type.");
	return ANALYZE_RESULT(FAILED, 0);
}

AnalyzeResult
analyze_instr_addrof(Context *cnt, MirInstrAddrOf *addrof)
{
	MirInstr *src = addrof->src;
	BL_ASSERT(src);

	if (!is_allocated_object(src)) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_EXPECTED_DECL,
		            addrof->base.node->location,
		            BUILDER_CUR_WORD,
		            "Cannot take the address of unallocated object.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	if (src->value.addr_mode == MIR_VAM_LVALUE_CONST) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_ADDRES_MODE,
		            addrof->base.node->location,
		            BUILDER_CUR_WORD,
		            "Cannot take address of constant.");
	}

	/* setup type */
	MirType *type = NULL;
	BL_ASSERT(src->value.type);
	if (src->value.type->kind == MIR_TYPE_FN) {
		type = create_type_ptr(cnt, src->value.type);
	} else {
		type = src->value.type;
	}

	addrof->base.value.type = type;
	addrof->base.comptime   = addrof->src->comptime;
	BL_ASSERT(addrof->base.value.type && "invalid type");

	reduce_instr(cnt, addrof->src);

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_cast(Context *cnt, MirInstrCast *cast, bool analyze_op_only)
{
	MirType *dest_type = cast->base.value.type;

	if (!analyze_op_only) {
		if (!dest_type && !cast->auto_cast) {
			AnalyzeResult result = analyze_resolve_type(cnt, cast->type, &dest_type);
			if (result.state != ANALYZE_PASSED) return result;
		}

		if (analyze_slot(cnt, &analyze_slot_conf_basic, &cast->expr, NULL) !=
		    ANALYZE_PASSED) {
			return ANALYZE_RESULT(FAILED, 0);
		}

		BL_ASSERT(cast->expr->value.type && "invalid cast source type");

		if (!dest_type && cast->auto_cast) {
			dest_type = cast->expr->value.type;
		}
	}

	BL_ASSERT(dest_type && "invalid cast destination type");
	BL_ASSERT(cast->expr->value.type && "invalid cast source type");

	MirType *expr_type = cast->expr->value.type;

	/* Setup const int type. */
	if (analyze_stage_set_volatile_expr(cnt, &cast->expr, dest_type) == ANALYZE_STAGE_BREAK) {
		cast->op = MIR_CAST_NONE;
		goto DONE;
	}

	cast->op = get_cast_op(expr_type, dest_type);
	if (cast->op == MIR_CAST_INVALID) {
		error_types(
		    expr_type, dest_type, cast->base.node, "Invalid cast from '%s' to '%s'.");
		return ANALYZE_RESULT(FAILED, 0);
	}

DONE:
	cast->base.value.type = dest_type;
	cast->base.comptime   = cast->expr->comptime;

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_sizeof(Context *cnt, MirInstrSizeof *szof)
{
	BL_ASSERT(szof->expr);

	if (analyze_slot(cnt, &analyze_slot_conf_basic, &szof->expr, NULL) != ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	MirType *type = szof->expr->value.type;
	BL_ASSERT(type);

	if (type->kind == MIR_TYPE_TYPE) {
		type = szof->expr->value.data.v_ptr.data.type;
		BL_ASSERT(type);
	}

	/* sizeof operator needs only type of input expression so we can erase whole call
	 * tree generated to get this expression */
	unref_instr(szof->expr);
	erase_instr_tree(szof->expr);
	szof->base.value.data.v_u64 = type->store_size_bytes;
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info)
{
	BL_ASSERT(type_info->expr);

	/* Resolve TypeInfo struct type */
	MirType *ret_type = lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO);
	if (!ret_type) return ANALYZE_RESULT(POSTPONE, 0);

	if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_info->expr, NULL) != ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	MirType *type = type_info->expr->value.type;
	BL_ASSERT(type);

	if (type->kind == MIR_TYPE_TYPE) {
		type = type_info->expr->value.data.v_ptr.data.type;
		BL_ASSERT(type);
	}

	type_info->expr_type = type;

	schedule_RTTI_generation(cnt, type);

	ret_type                   = create_type_ptr(cnt, ret_type);
	type_info->base.value.type = ret_type;

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_alignof(Context *cnt, MirInstrAlignof *alof)
{
	BL_ASSERT(alof->expr);

	if (analyze_slot(cnt, &analyze_slot_conf_basic, &alof->expr, NULL) != ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	MirType *type = alof->expr->value.type;
	BL_ASSERT(type);

	if (type->kind == MIR_TYPE_TYPE) {
		type = alof->expr->value.data.v_ptr.data.type;
		BL_ASSERT(type);
	}

	alof->base.value.data.v_u64 = type->alignment;
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
	BL_ASSERT(ref->rid && ref->scope);

	ScopeEntry *found         = NULL;
	Scope *     private_scope = ref->parent_unit->private_scope;

	if (!private_scope) { /* reference in unit without private scope  */
		found = scope_lookup(ref->scope, ref->rid, true, false);
	} else { /* reference in unit with private scope */
		/* search in current tree and ignore global scope */
		found = scope_lookup(ref->scope, ref->rid, true, true);

		/* lookup in private scope and global scope also (private scope has global
		 * scope as parent every time) */
		if (!found) found = scope_lookup(private_scope, ref->rid, true, false);
	}

	if (found ? found->kind == SCOPE_ENTRY_INCOMPLETE : true) {
		return ANALYZE_RESULT(WAITING, ref->rid->hash);
	}

	switch (found->kind) {
	case SCOPE_ENTRY_FN: {
		MirFn *fn = found->data.fn;
		BL_ASSERT(fn);
		MirType *type = fn->type;
		BL_ASSERT(type);

		ref->base.value.type = type;
		ref->base.comptime   = true;
		ref_instr(fn->prototype);
		mir_set_const_ptr(&ref->base.value.data.v_ptr, fn, MIR_CP_FN);
		break;
	}

	case SCOPE_ENTRY_TYPE: {
		ref->base.value.type = cnt->builtin_types.t_type;
		ref->base.comptime   = true;
		mir_set_const_ptr(&ref->base.value.data.v_ptr, found->data.type, MIR_CP_TYPE);
		break;
	}

	case SCOPE_ENTRY_VARIANT: {
		MirVariant *variant = found->data.variant;
		BL_ASSERT(variant);

		MirType *type = variant->value->type;
		BL_ASSERT(type);

		type                      = create_type_ptr(cnt, type);
		ref->base.value.type      = type;
		ref->base.comptime        = true;
		ref->base.value.addr_mode = MIR_VAM_LVALUE_CONST;
		mir_set_const_ptr(&ref->base.value.data.v_ptr, variant->value, MIR_CP_VALUE);

		break;
	}

	case SCOPE_ENTRY_VAR: {
		MirVar *var = found->data.var;
		BL_ASSERT(var);
		++var->ref_count;
		MirType *type = var->value.type;
		BL_ASSERT(type);

		type                      = create_type_ptr(cnt, type);
		ref->base.value.type      = type;
		ref->base.comptime        = var->comptime;
		ref->base.value.addr_mode = var->is_mutable ? MIR_VAM_LVALUE : MIR_VAM_LVALUE_CONST;

		/* set pointer to variable const value directly when variable is compile
		 * time known
		 */
		if (var->comptime) mir_set_const_ptr(&ref->base.value.data.v_ptr, var, MIR_CP_VAR);
		break;
	}

	default:
		BL_ABORT("invalid scope entry kind");
	}

	ref->scope_entry = found;
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_decl_direct_ref(Context *cnt, MirInstrDeclDirectRef *ref)
{
	BL_ASSERT(ref->ref && "Missing declaration reference for direct ref.");
	BL_ASSERT(ref->ref->kind == MIR_INSTR_DECL_VAR && "Expected variable declaration.");
	BL_ASSERT(ref->ref->analyzed && "Reference not analyzed.");

	MirVar *var = ((MirInstrDeclVar *)ref->ref)->var;
	BL_ASSERT(var);
	++var->ref_count;
	MirType *type = var->value.type;
	BL_ASSERT(type);

	type                      = create_type_ptr(cnt, type);
	ref->base.value.type      = type;
	ref->base.comptime        = var->comptime;
	ref->base.value.addr_mode = var->is_mutable ? MIR_VAM_LVALUE : MIR_VAM_LVALUE_CONST;

	/* set pointer to variable const value directly when variable is compile
	 * time known
	 */
	if (var->comptime) mir_set_const_ptr(&ref->base.value.data.v_ptr, var, MIR_CP_VAR);

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_arg(Context *cnt, MirInstrArg *arg)
{
	MirFn *fn = arg->base.owner_block->owner_fn;
	BL_ASSERT(fn);

	MirType *type = mir_get_fn_arg_type(fn->type, arg->i);
	BL_ASSERT(type);
	arg->base.value.type = type;

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
	/* nothing to do :( */
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
	/* resolve type */
	if (!fn_proto->base.value.type) {
		MirType *     fn_type = NULL;
		AnalyzeResult result  = analyze_resolve_type(cnt, fn_proto->type, &fn_type);
		if (result.state != ANALYZE_PASSED) return result;

		/* Analyze user defined type (this must be compared with infered type).
		 */
		if (fn_proto->user_type) {
			MirType *user_fn_type = NULL;
			result = analyze_resolve_type(cnt, fn_proto->user_type, &user_fn_type);
			if (result.state != ANALYZE_PASSED) return result;

			if (!type_cmp(fn_type, user_fn_type)) {
				error_types(fn_type, user_fn_type, fn_proto->user_type->node, NULL);
			}
		}

		fn_proto->base.value.type = fn_type;
	}

	MirConstValue *value = &fn_proto->base.value;

	BL_ASSERT(value->type && "function has no valid type");
	BL_ASSERT(value->data.v_ptr.data.fn);
	value->data.v_ptr.data.fn->type = fn_proto->base.value.type;

	MirFn *fn = fn_proto->base.value.data.v_ptr.data.fn;
	BL_ASSERT(fn);

	if (fn->ret_tmp) {
		BL_ASSERT(fn->ret_tmp->kind == MIR_INSTR_DECL_VAR);
		((MirInstrDeclVar *)fn->ret_tmp)->var->value.type = value->type->data.fn.ret_type;
	}

	/* set type name */
	fn_proto->base.value.type->user_id = fn->id;

	/* Setup function linkage name, this will be later used by LLVM backend. */
	if (fn->id) {
		if (fn->is_in_gscope) {
			fn->linkage_name = fn->id->str;
		} else {
			if (IS_FLAG(fn->flags, FLAG_EXTERN))
				fn->linkage_name = fn->id->str;
			else
				fn->linkage_name = gen_uq_name(fn->id->str);
		}
	} else {
		/* Anonymous function use implicit unique name. */
		fn->linkage_name = gen_uq_name(IMPL_FN_NAME);
	}

	BL_ASSERT(fn->linkage_name && "Function without linkage name!");

	if (IS_FLAG(fn->flags, FLAG_EXTERN)) {
		/* lookup external function exec handle */
		BL_ASSERT(fn->linkage_name);
		fn->dyncall.extern_entry = assembly_find_extern(cnt->assembly, fn->linkage_name);

		if (!fn->dyncall.extern_entry) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_UNKNOWN_SYMBOL,
			            fn_proto->base.node->location,
			            BUILDER_CUR_WORD,
			            "External symbol '%s' not found.",
			            fn->linkage_name);
		} else {
			fn->fully_analyzed = true;
		}
	} else {
		/* Add entry block of the function into analyze queue. */
		MirInstr *entry_block = (MirInstr *)fn->first_block;
		if (!entry_block) {
			/* TODO: not the best place to do this check, move into ast
			 * generation later
			 */
			/* TODO: not the best place to do this check, move into ast
			 * generation later
			 */
			/* TODO: not the best place to do this check, move into ast
			 * generation later
			 */
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_EXPECTED_BODY,
			            fn_proto->base.node->location,
			            BUILDER_CUR_WORD,
			            "Missing function body.");
			return ANALYZE_RESULT(FAILED, 0);
		}

		analyze_push_front(cnt, entry_block);
	}

	if (fn->id) commit_fn(cnt, fn);

	if (fn_proto->first_unrechable_location) {
		builder_msg(BUILDER_MSG_WARNING,
		            0,
		            fn_proto->first_unrechable_location,
		            BUILDER_CUR_NONE,
		            "Unrechable code detected.");
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
	BL_ASSERT(br->cond && br->then_block && br->else_block);
	BL_ASSERT(br->cond->analyzed);

	if (analyze_slot(cnt, &analyze_slot_conf_default, &br->cond, cnt->builtin_types.t_bool) !=
	    ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	/* PERFORMANCE: When condition is known in compile time, we can discard
	 * whole else/then block based on condition resutl. It is not possible
	 * because we don't have tracked down execution tree for now. */

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_br(Context *cnt, MirInstrBr *br)
{
	BL_ASSERT(br->then_block);
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_load(Context *cnt, MirInstrLoad *load)
{
	MirInstr *src = load->src;
	BL_ASSERT(src);
	if (!mir_is_pointer_type(src->value.type)) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_TYPE,
		            src->node->location,
		            BUILDER_CUR_WORD,
		            "Expected pointer.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	MirType *type = mir_deref_type(src->value.type);
	BL_ASSERT(type);
	load->base.value.type = type;

	reduce_instr(cnt, src);
	load->base.comptime        = src->comptime;
	load->base.value.addr_mode = MIR_VAM_RVALUE;

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn)
{
	BL_ASSERT(type_fn->base.value.type);
	BL_ASSERT(type_fn->ret_type ? type_fn->ret_type->analyzed : true);

	bool is_vargs = false;

	SmallArray_ArgPtr *args = NULL;
	if (type_fn->args) {
		const size_t argc = type_fn->args->size;
		args              = create_sarr(SmallArray_ArgPtr, cnt->assembly);

		MirInstrDeclArg **arg_ref;
		MirArg *          arg;
		for (size_t i = 0; i < argc; ++i) {
			BL_ASSERT(type_fn->args->data[i]->kind == MIR_INSTR_DECL_ARG);
			arg_ref = (MirInstrDeclArg **)&type_fn->args->data[i];
			BL_ASSERT((*arg_ref)->base.comptime);

			if (analyze_slot(
			        cnt, &analyze_slot_conf_basic, (MirInstr **)arg_ref, NULL) !=
			    ANALYZE_PASSED) {
				return ANALYZE_RESULT(FAILED, 0);
			}

			arg = (*arg_ref)->arg;
			BL_ASSERT(arg);

			is_vargs = arg->type->kind == MIR_TYPE_VARGS;
			if (is_vargs && i != type_fn->args->size - 1) {
				builder_msg(
				    BUILDER_MSG_ERROR,
				    ERR_INVALID_TYPE,
				    arg->decl_node->location,
				    BUILDER_CUR_WORD,
				    "VArgs function argument must be last in argument list.");
			}

			sa_push_ArgPtr(args, arg);
		}
	}

	MirType *ret_type = NULL;
	if (type_fn->ret_type) {
		if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_fn->ret_type, NULL) !=
		    ANALYZE_PASSED) {
			return ANALYZE_RESULT(FAILED, 0);
		}

		BL_ASSERT(type_fn->ret_type->comptime);
		ret_type = type_fn->ret_type->value.data.v_ptr.data.type;
		BL_ASSERT(ret_type);
	}

	{
		MirConstPtr *const_ptr = &type_fn->base.value.data.v_ptr;
		mir_set_const_ptr(
		    const_ptr, create_type_fn(cnt, NULL, ret_type, args, is_vargs), MIR_CP_FN);
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_decl_member(Context *cnt, MirInstrDeclMember *decl)
{
	if (analyze_slot(cnt, &analyze_slot_conf_basic, &decl->type, NULL) != ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	/* NOTE: Members will be provided by instr type struct because we need to
	 * know right ordering of members inside structure layout. (index and llvm
	 * element offet need to be calculated)*/
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_decl_variant(Context *cnt, MirInstrDeclVariant *variant_instr)
{
	MirVariant *variant = variant_instr->variant;
	BL_ASSERT(variant && "Missing variant.");

	if (variant_instr->value) {
		/* User defined initialization value. */
		if (!variant_instr->value->comptime) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_EXPR,
			            variant_instr->value->node->location,
			            BUILDER_CUR_WORD,
			            "Enum variant value must be compile time known.");
			return ANALYZE_RESULT(FAILED, 0);
		}

		if (analyze_slot(cnt, &analyze_slot_conf_basic, &variant_instr->value, NULL) !=
		    ANALYZE_PASSED) {
			return ANALYZE_RESULT(FAILED, 0);
		}

		/* Setup value. */
		variant_instr->variant->value = &variant_instr->value->value;
	} else {
		/*
		 * CLENUP: Automatic initialization value is set in parser, mabye we will
		 * prefer to do automatic initialization here instead of doing so in parser
		 * pass.
		 */
		BL_UNIMPLEMENTED;
	}

	commit_variant(cnt, variant);

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_decl_arg(Context *cnt, MirInstrDeclArg *decl)
{
	if (analyze_slot(cnt, &analyze_slot_conf_basic, &decl->type, NULL) != ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	BL_ASSERT(decl->type->value.data.v_ptr.data.any);
	BL_ASSERT(decl->type->value.data.v_ptr.kind == MIR_CP_TYPE);
	MirType *type = decl->type->value.data.v_ptr.data.type;

	decl->arg->type = type;
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_type_struct(Context *cnt, MirInstrTypeStruct *type_struct)
{
	SmallArray_MemberPtr *members = NULL;

	if (type_struct->members) {
		MirInstr **         member_instr;
		MirInstrDeclMember *decl_member;
		MirType *           member_type;
		Scope *             scope = type_struct->scope;
		const size_t        memc  = type_struct->members->size;

		members = create_sarr(SmallArray_MemberPtr, cnt->assembly);

		for (size_t i = 0; i < memc; ++i) {
			member_instr = &type_struct->members->data[i];

			if (analyze_slot(cnt, &analyze_slot_conf_basic, member_instr, NULL) !=
			    ANALYZE_PASSED) {
				return ANALYZE_RESULT(FAILED, 0);
			}

			decl_member = (MirInstrDeclMember *)*member_instr;
			BL_ASSERT(decl_member->base.kind == MIR_INSTR_DECL_MEMBER);
			BL_ASSERT(decl_member->base.comptime);

			/* solve member type */
			member_type = decl_member->type->value.data.v_ptr.data.type;

			if (member_type->kind == MIR_TYPE_FN) {
				builder_msg(BUILDER_MSG_ERROR,
				            ERR_INVALID_TYPE,
				            (*member_instr)->node->location,
				            BUILDER_CUR_WORD,
				            "Invalid type of the structure member, functions can "
				            "be referenced only by pointers.");
				return ANALYZE_RESULT(FAILED, 0);
			}

			BL_ASSERT(member_type);

			/* setup and provide member */
			MirMember *member = decl_member->member;
			BL_ASSERT(member);
			member->type       = member_type;
			member->decl_scope = scope;
			member->index      = i;

			sa_push_MemberPtr(members, member);

			commit_member(cnt, member);
		}
	}

	{ /* Setup const pointer. */
		MirConstPtr *const_ptr = &type_struct->base.value.data.v_ptr;
		MirType *    tmp       = create_type_struct(cnt,
                                                  MIR_TYPE_STRUCT,
                                                  type_struct->id,
                                                  type_struct->scope,
                                                  members,
                                                  type_struct->is_packed);

		mir_set_const_ptr(const_ptr, tmp, MIR_CP_TYPE);
	}
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_type_slice(Context *cnt, MirInstrTypeSlice *type_slice)
{
	BL_ASSERT(type_slice->elem_type);

	if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_slice->elem_type, NULL) !=
	    ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	ID *id = NULL;
	if (type_slice->base.node && type_slice->base.node->kind == AST_IDENT) {
		id = &type_slice->base.node->data.ident.id;
	}

	if (type_slice->elem_type->value.type->kind != MIR_TYPE_TYPE) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_TYPE,
		            type_slice->elem_type->node->location,
		            BUILDER_CUR_WORD,
		            "Expected type.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	BL_ASSERT(type_slice->elem_type->comptime && "This should be an error");
	MirType *elem_type = type_slice->elem_type->value.data.v_ptr.data.type;
	BL_ASSERT(elem_type);

	elem_type = create_type_ptr(cnt, elem_type);
	elem_type = create_type_struct_special(cnt, MIR_TYPE_SLICE, id, elem_type);

	{ /* set const pointer value */
		MirConstPtr *const_ptr = &type_slice->base.value.data.v_ptr;
		mir_set_const_ptr(const_ptr, elem_type, MIR_CP_TYPE);
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_type_vargs(Context *cnt, MirInstrTypeVArgs *type_vargs)
{
	MirType *elem_type = NULL;
	if (type_vargs->elem_type) {
		if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_vargs->elem_type, NULL) !=
		    ANALYZE_PASSED) {
			return ANALYZE_RESULT(FAILED, 0);
		}

		if (type_vargs->elem_type->value.type->kind != MIR_TYPE_TYPE) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_TYPE,
			            type_vargs->elem_type->node->location,
			            BUILDER_CUR_WORD,
			            "Expected type.");
			return ANALYZE_RESULT(FAILED, 0);
		}

		BL_ASSERT(type_vargs->elem_type->comptime && "This should be an error");
		elem_type = type_vargs->elem_type->value.data.v_ptr.data.type;
	} else {
		/* use Any */
		elem_type = lookup_builtin(cnt, MIR_BUILTIN_ID_ANY);
		if (!elem_type)
			return ANALYZE_RESULT(WAITING, builtin_ids[MIR_BUILTIN_ID_ANY].hash);
	}

	BL_ASSERT(elem_type);

	elem_type = create_type_ptr(cnt, elem_type);
	elem_type = create_type_struct_special(cnt, MIR_TYPE_VARGS, NULL, elem_type);

	{ /* set const pointer value */
		MirConstPtr *const_ptr = &type_vargs->base.value.data.v_ptr;
		mir_set_const_ptr(const_ptr, elem_type, MIR_CP_TYPE);
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr)
{
	BL_ASSERT(type_arr->base.value.type);
	BL_ASSERT(type_arr->elem_type->analyzed);

	if (analyze_slot(
	        cnt, &analyze_slot_conf_default, &type_arr->len, cnt->builtin_types.t_s64) !=
	    ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_arr->elem_type, NULL) !=
	    ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	/* len */
	if (!type_arr->len->comptime) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_EXPECTED_CONST,
		            type_arr->len->node->location,
		            BUILDER_CUR_WORD,
		            "Array size must be compile-time constant.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	if (type_arr->elem_type->value.type->kind != MIR_TYPE_TYPE) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_TYPE,
		            type_arr->elem_type->node->location,
		            BUILDER_CUR_WORD,
		            "Expected type.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	BL_ASSERT(type_arr->len->comptime && "this must be error");
	reduce_instr(cnt, type_arr->len);

	const s64 len = type_arr->len->value.data.v_s64;
	if (len == 0) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_ARR_SIZE,
		            type_arr->len->node->location,
		            BUILDER_CUR_WORD,
		            "Array size cannot be 0.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	/* elem type */
	BL_ASSERT(type_arr->elem_type->comptime);
	reduce_instr(cnt, type_arr->elem_type);

	MirType *elem_type = type_arr->elem_type->value.data.v_ptr.data.type;
	BL_ASSERT(elem_type);

	elem_type = create_type_array(cnt, elem_type, len);

	{ /* set const pointer value */
		MirConstPtr *const_ptr = &type_arr->base.value.data.v_ptr;
		mir_set_const_ptr(const_ptr, elem_type, MIR_CP_TYPE);
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_type_enum(Context *cnt, MirInstrTypeEnum *type_enum)
{
	SmallArray_InstrPtr *variant_instrs = type_enum->variants;
	Scope *              scope          = type_enum->scope;
	BL_ASSERT(variant_instrs);
	BL_ASSERT(scope);
	const size_t varc = variant_instrs->size;
	BL_ASSERT(varc);

	/*
	 * Validate and settup enum base type.
	 */
	MirType *base_type;
	if (type_enum->base_type) {
		reduce_instr(cnt, type_enum->base_type);
		base_type = type_enum->base_type->value.data.v_ptr.data.type;

		/* Enum type must be integer! */
		if (base_type->kind != MIR_TYPE_INT) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_TYPE,
			            type_enum->base_type->node->location,
			            BUILDER_CUR_WORD,
			            "Base type of enumerator must be an integer type.");
			return ANALYZE_RESULT(FAILED, 0);
		}
	} else {
		/* Use s32 by default. */
		base_type = cnt->builtin_types.t_s32;
	}

	BL_ASSERT(base_type && "Invalid enum base type.");

	SmallArray_VariantPtr *variants = create_sarr(SmallArray_VariantPtr, cnt->assembly);

	/* Iterate over all enum variants and validate them. */
	MirInstr *  it;
	MirVariant *variant;

	SARRAY_FOREACH(variant_instrs, it)
	{
		MirInstrDeclVariant *variant_instr = (MirInstrDeclVariant *)it;
		variant                            = variant_instr->variant;
		BL_ASSERT(variant && "Missing variant.");

		if (analyze_slot(
		        cnt, &analyze_slot_conf_default, &variant_instr->value, base_type) !=
		    ANALYZE_PASSED) {
			return ANALYZE_RESULT(FAILED, 0);
		}

		reduce_instr(cnt, &variant_instr->base);

		sa_push_VariantPtr(variants, variant);
	}

	MirType *enum_type = create_type_enum(cnt, type_enum->id, scope, base_type, variants);

	{ /* set const pointer value */
		MirConstPtr *const_ptr = &type_enum->base.value.data.v_ptr;
		mir_set_const_ptr(const_ptr, enum_type, MIR_CP_TYPE);
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr)
{
	BL_ASSERT(type_ptr->type);

	if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_ptr->type, NULL) != ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	BL_ASSERT(type_ptr->type->comptime);

	{ /* Target value must be a type. */
		MirType *src_type = type_ptr->type->value.type;
		BL_ASSERT(src_type);

		if (src_type->kind != MIR_TYPE_TYPE) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_TYPE,
			            type_ptr->type->node->location,
			            BUILDER_CUR_WORD,
			            "Expected type name.");
			return ANALYZE_RESULT(FAILED, 0);
		}
	}

	MirType *src_type_value = type_ptr->type->value.data.v_ptr.data.type;
	BL_ASSERT(src_type_value);

	if (src_type_value->kind == MIR_TYPE_TYPE) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_TYPE,
		            type_ptr->base.node->location,
		            BUILDER_CUR_WORD,
		            "Cannot create pointer to type.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	MirType *tmp = create_type_ptr(cnt, src_type_value);

	{ /* set const pointer value */
		MirConstPtr *const_ptr = &type_ptr->base.value.data.v_ptr;
		mir_set_const_ptr(const_ptr, tmp, MIR_CP_TYPE);
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_binop(Context *cnt, MirInstrBinop *binop)
{
#define is_valid(_type, _op)                                                                       \
	(((_type)->kind == MIR_TYPE_INT) || ((_type)->kind == MIR_TYPE_NULL) ||                    \
	 ((_type)->kind == MIR_TYPE_REAL) || ((_type)->kind == MIR_TYPE_PTR) ||                    \
	 ((_type)->kind == MIR_TYPE_BOOL && ast_binop_is_logic(_op)) ||                            \
	 ((_type)->kind == MIR_TYPE_ENUM && (_op == BINOP_EQ || _op == BINOP_NEQ)))

	{ /* Handle type propagation. */
		MirType *lhs_type = binop->lhs->value.type;
		MirType *rhs_type = binop->rhs->value.type;

		if (is_load_needed(binop->lhs)) lhs_type = mir_deref_type(lhs_type);
		if (is_load_needed(binop->rhs)) rhs_type = mir_deref_type(rhs_type);

		const bool lhs_is_null = binop->lhs->value.type->kind == MIR_TYPE_NULL;
		const bool lhs_is_const_int =
		    binop->lhs->kind == MIR_INSTR_CONST && lhs_type->kind == MIR_TYPE_INT;
		const bool can_propagate_LtoR =
		    can_impl_cast(lhs_type, rhs_type) || lhs_is_const_int;

		char type_nameL[256];
		mir_type_to_str(type_nameL, 256, lhs_type, true);
		char type_nameR[256];
		mir_type_to_str(type_nameR, 256, rhs_type, true);

		if (can_propagate_LtoR) {
			if (analyze_slot(cnt, &analyze_slot_conf_default, &binop->lhs, rhs_type) !=
			    ANALYZE_PASSED)
				return ANALYZE_RESULT(FAILED, 0);

			if (analyze_slot(cnt, &analyze_slot_conf_basic, &binop->rhs, NULL) !=
			    ANALYZE_PASSED)
				return ANALYZE_RESULT(FAILED, 0);
		} else {
			if (analyze_slot(cnt, &analyze_slot_conf_basic, &binop->lhs, NULL) !=
			    ANALYZE_PASSED)
				return ANALYZE_RESULT(FAILED, 0);

			if (analyze_slot(
			        cnt,
			        lhs_is_null ? &analyze_slot_conf_basic : &analyze_slot_conf_default,
			        &binop->rhs,
			        lhs_is_null ? NULL : binop->lhs->value.type) != ANALYZE_PASSED)
				return ANALYZE_RESULT(FAILED, 0);

			if (lhs_is_null) {
				if (analyze_stage_set_null(
				        cnt, &binop->lhs, binop->rhs->value.type) !=
				    ANALYZE_STAGE_BREAK)
					return ANALYZE_RESULT(FAILED, 0);
			}
		}
	}

	MirInstr *lhs = binop->lhs;
	MirInstr *rhs = binop->rhs;
	BL_ASSERT(lhs && rhs);
	BL_ASSERT(lhs->analyzed);
	BL_ASSERT(rhs->analyzed);

	const bool lhs_valid = is_valid(lhs->value.type, binop->op);
	const bool rhs_valid = is_valid(rhs->value.type, binop->op);

	if (!(lhs_valid && rhs_valid)) {
		error_types(lhs->value.type,
		            rhs->value.type,
		            binop->base.node,
		            "invalid operation for %s type");
		return ANALYZE_RESULT(FAILED, 0);
	}

	MirType *type = ast_binop_is_logic(binop->op) ? cnt->builtin_types.t_bool : lhs->value.type;
	BL_ASSERT(type);
	binop->base.value.type = type;

	/* when binary operation has lhs and rhs values known in compile it is known
	 * in compile time also
	 */
	if (lhs->comptime && rhs->comptime) binop->base.comptime = true;

	binop->volatile_type = is_volatile_expr(lhs) && is_volatile_expr(rhs);

	return ANALYZE_RESULT(PASSED, 0);
#undef is_valid
}

AnalyzeResult
analyze_instr_unop(Context *cnt, MirInstrUnop *unop)
{
	MirType *type = unop->expr->value.type;
	if (analyze_slot(cnt, &analyze_slot_conf_basic, &unop->expr, NULL) != ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	BL_ASSERT(unop->expr && unop->expr->analyzed);
	type = unop->expr->value.type;
	BL_ASSERT(type);
	unop->base.value.type = type;

	unop->base.comptime = unop->expr->comptime;
	unop->volatile_type = is_volatile_expr(unop->expr);

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_const(Context *cnt, MirInstrConst *cnst)
{
	BL_ASSERT(cnst->base.value.type);
	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_ret(Context *cnt, MirInstrRet *ret)
{
	/* compare return value with current function type */
	MirInstrBlock *block = ret->base.owner_block;
	if (!block->terminal) block->terminal = &ret->base;

	MirType *fn_type = get_current_fn(cnt)->type;
	BL_ASSERT(fn_type);
	BL_ASSERT(fn_type->kind == MIR_TYPE_FN);

	if (ret->value) {
		const AnalyzeSlotConfig *conf =
		    ret->infer_type ? &analyze_slot_conf_basic : &analyze_slot_conf_default;
		if (analyze_slot(cnt,
		                 conf,
		                 &ret->value,
		                 ret->infer_type ? NULL : fn_type->data.fn.ret_type) !=
		    ANALYZE_PASSED) {
			return ANALYZE_RESULT(FAILED, 0);
		}
	}

	MirInstr *value = ret->value;
	if (value) {
		BL_ASSERT(value->analyzed);
	}

	if (ret->infer_type) {
		/* return is supposed to override function return type */
		if (ret->value) {
			BL_ASSERT(ret->value->value.type);
			if (fn_type->data.fn.ret_type != ret->value->value.type) {
				MirFn *fn = get_current_fn(cnt);
				BL_ASSERT(fn);
				fn->type = create_type_fn(cnt,
				                          NULL,
				                          ret->value->value.type,
				                          fn_type->data.fn.args,
				                          fn_type->data.fn.is_vargs);
				fn_type  = fn->type;
				/* HACK: Function type need to be set also for function
				 * prototype instruction, this is by the way only reason why
				 * we need poinetr to prototype inside MirFn. Better
				 * solution should be possible. */
				fn->prototype->value.type = fn_type;
			}
		}
	}

	const bool expected_ret_value =
	    !type_cmp(fn_type->data.fn.ret_type, cnt->builtin_types.t_void);

	/* return value is not expected, and it's not provided */
	if (!expected_ret_value && !value) {
		return ANALYZE_RESULT(PASSED, 0);
	}

	/* return value is expected, but it's not provided */
	if (expected_ret_value && !value) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_EXPR,
		            ret->base.node->location,
		            BUILDER_CUR_AFTER,
		            "Expected return value.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	/* return value is not expected, but it's provided */
	if (!expected_ret_value && value) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_EXPR,
		            ret->value->node->location,
		            BUILDER_CUR_WORD,
		            "Unexpected return value.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
	MirVar *var = decl->var;
	BL_ASSERT(var);

	/* CLEANUP: make also muttable variable to be compile time if they are set to comptime
	 * init value? */
	bool is_decl_comptime = !var->is_mutable;

	if (decl->type && var->value.type == NULL) {
		AnalyzeResult result = analyze_resolve_type(cnt, decl->type, &var->value.type);
		if (result.state != ANALYZE_PASSED) return result;
	}

	if (var->is_in_gscope) { // global variable
		/* All globals must be initialized. */
		if (!decl->init) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_UNINITIALIZED,
			            decl->base.node->location,
			            BUILDER_CUR_WORD,
			            "All globals must be initialized.");
			return ANALYZE_RESULT(FAILED, 0);
		}

		/* Global initializer must be compile time known. */
		if (!decl->init->comptime) {
			builder_msg(
			    BUILDER_MSG_ERROR,
			    ERR_EXPECTED_COMPTIME,
			    decl->init->node->location,
			    BUILDER_CUR_WORD,
			    "Global variables must be initialized with compile time known value.");
			return ANALYZE_RESULT(FAILED, 0);
		}

		/* Just to be sure we have call instruction. */
		BL_ASSERT(decl->init->kind == MIR_INSTR_CALL &&
		          "Global initializer is supposed to be comptime implicit call.");

		/* Since all globals are initialized by call to comptime function and no type infer
		 * is needed (user specified expected type directly), we must disable type infering
		 * of terminal instruction in initializer and set exact type we are expecting to be
		 * returned by the initializer function. */
		if (var->value.type) {
			MirInstrCall *initializer_call = (MirInstrCall *)decl->init;
			MirFn *       fn               = get_callee(initializer_call);
			MirInstrRet * terminal         = fn->terminal_instr;
			BL_ASSERT(terminal);

			if (terminal->infer_type) {
				terminal->infer_type = false;
				fn->type = create_type_fn(cnt, NULL, var->value.type, NULL, false);

				/* CLEANUP: why we need set type of the function also for fn
				 * prototype ??? */
				/* CLEANUP: why we need set type of the function also for fn
				 * prototype ??? */
				/* CLEANUP: why we need set type of the function also for fn
				 * prototype ??? */
				fn->prototype->value.type = fn->type;
			}
		}

		/* Analyze and execute initializer. This could lead to POSTPONE when initializer
		 * function is not ready yet. */
		AnalyzeResult result = analyze_instr(cnt, decl->init);
		if (result.state != ANALYZE_PASSED) return result;

		/* Execute only when analyze passed. */
		vm_execute_instr_top_level_call(&cnt->vm, (MirInstrCall *)decl->init);

		/* Infer type if needed */
		if (!var->value.type) {
			var->value.type = decl->init->value.type;
		}

		is_decl_comptime &= decl->init->comptime;
	} else { // local variable
		if (decl->init) {
			if (var->value.type) {
				if (analyze_slot(cnt,
				                 &analyze_slot_conf_default,
				                 &decl->init,
				                 var->value.type) != ANALYZE_PASSED) {
					return ANALYZE_RESULT(FAILED, 0);
				}
			} else {
				if (analyze_slot(
				        cnt, &analyze_slot_conf_basic, &decl->init, NULL) !=
				    ANALYZE_PASSED) {
					return ANALYZE_RESULT(FAILED, 0);
				}

				/* infer type */
				MirType *type = decl->init->value.type;
				BL_ASSERT(type);
				if (type->kind == MIR_TYPE_NULL) type = type->data.null.base_type;
				var->value.type = type;
			}

			is_decl_comptime &= decl->init->comptime;
		}
	}

	decl->base.comptime = var->comptime = is_decl_comptime;

	if (!var->value.type) {
		BL_ABORT("unknown declaration type");
	}

	if (var->value.type->kind == MIR_TYPE_TYPE && var->is_mutable) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_MUTABILITY,
		            decl->base.node->location,
		            BUILDER_CUR_WORD,
		            "Type declaration must be immutable.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	if (var->value.type->kind == MIR_TYPE_FN) {
		/* Allocated type is function. */
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_TYPE,
		            decl->base.node->location,
		            BUILDER_CUR_WORD,
		            "Invalid type of the variable, functions can be referenced "
		            "only by pointers.");
		return ANALYZE_RESULT(FAILED, 0);
	} else if (var->value.type->kind == MIR_TYPE_VOID) {
		/* Allocated type is void type. */
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_TYPE,
		            decl->base.node->location,
		            BUILDER_CUR_WORD,
		            "Cannot allocate unsized type.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	if (decl->base.ref_count == 0) {
		builder_msg(BUILDER_MSG_WARNING,
		            0,
		            decl->base.node->location,
		            BUILDER_CUR_WORD,
		            "Unused declaration.");
	}

	reduce_instr(cnt, decl->init);

	if (decl->base.comptime && decl->init) {
		/* initialize when known in compiletime */
		var->value = decl->init->value;
	}

	if (!decl->var->is_implicit) commit_var(cnt, decl->var);

	/* Type declaration should not be generated in LLVM. */
	var->gen_llvm = var->value.type->kind != MIR_TYPE_TYPE;

	if (var->is_in_gscope) {
		/* Global varibales which are not compile time constants are allocated
		 * on the stack, one option is to do allocation every time when we
		 * invoke comptime function execution, but we don't know which globals
		 * will be used by function and we also don't known whatever function
		 * has some side effect or not. So we produce allocation here. Variable
		 * will be stored in static data segment. There is no need to use
		 * relative pointers here. */
		if (!var->comptime) {
			vm_create_global(&cnt->vm, decl);
		}
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_call(Context *cnt, MirInstrCall *call)
{
	BL_ASSERT(call->callee);

	/*
	 * Direct call is call without any reference lookup, usually call to anonymous
	 * function, type resolver or variable initializer. Contant value of callee
	 * instruction must containt pointer to the MirFn object.
	 */
	const MirInstrKind callee_kind = call->callee->kind;
	const bool         is_direct_call =
	    callee_kind != MIR_INSTR_DECL_REF && callee_kind != MIR_INSTR_MEMBER_PTR;

	/* callee has not been analyzed yet -> postpone call analyze */
	if (!call->callee->analyzed) {
		BL_ASSERT(call->callee->kind == MIR_INSTR_FN_PROTO);
		MirInstrFnProto *fn_proto = (MirInstrFnProto *)call->callee;
		if (!fn_proto->pushed_for_analyze) {
			fn_proto->pushed_for_analyze = true;
			analyze_push_back(cnt, call->callee);
		}
		return ANALYZE_RESULT(POSTPONE, 0);
	}

	if (analyze_slot(cnt, &analyze_slot_conf_basic, &call->callee, NULL) != ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	MirType *type = call->callee->value.type;
	BL_ASSERT(type && "invalid type of called object");

	if (mir_is_pointer_type(type)) {
		/* we want to make calls also via pointer to functions so in such case
		 * we need to resolve pointed function */
		type = mir_deref_type(type);
	}

	if (type->kind != MIR_TYPE_FN) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_EXPECTED_FUNC,
		            call->callee->node->location,
		            BUILDER_CUR_WORD,
		            "Expected a function name.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	if (is_direct_call) {
		MirFn *fn = call->callee->value.data.v_ptr.data.fn;
		BL_ASSERT(fn && "Missing function reference for direct call!");
		if (call->base.comptime) {
			if (!fn->fully_analyzed) return ANALYZE_RESULT(POSTPONE, 0);
		} else if (call->callee->kind == MIR_INSTR_FN_PROTO) {
			/* Direct call of anonymous function. */

			/*
			 * CLENUP: Function reference counting is not clear, we can decide
			 * to count references direcly inside MirFn or in function prototype
			 * instruction.
			 */
			// ++fn->ref_count;
			fn->emit_llvm = true;
		}
	}

	MirType *result_type = type->data.fn.ret_type;
	BL_ASSERT(result_type && "invalid type of call result");
	call->base.value.type = result_type;

	/* validate arguments */
	const bool is_vargs = type->data.fn.is_vargs;

	size_t       callee_argc = type->data.fn.args ? type->data.fn.args->size : 0;
	const size_t call_argc   = call->args ? call->args->size : 0;

	if (is_vargs) {
		/* This is gonna be tricky... */
		--callee_argc;
		if ((call_argc < callee_argc)) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_ARG_COUNT,
			            call->base.node->location,
			            BUILDER_CUR_WORD,
			            "Expected at least %u %s, but called with %u.",
			            callee_argc,
			            callee_argc == 1 ? "argument" : "arguments",
			            call_argc);
			return ANALYZE_RESULT(FAILED, 0);
		}

		MirType *vargs_type = mir_get_fn_arg_type(type, (u32)callee_argc);
		BL_ASSERT(vargs_type->kind == MIR_TYPE_VARGS && "VArgs is expected to be last!!!");

		vargs_type = mir_get_struct_elem_type(vargs_type, 1);
		BL_ASSERT(vargs_type && mir_is_pointer_type(vargs_type));

		vargs_type = mir_deref_type(vargs_type);

		/* Prepare vargs values. */
		const size_t         vargsc = call_argc - callee_argc;
		SmallArray_InstrPtr *values = create_sarr(SmallArray_InstrPtr, cnt->assembly);
		MirInstr *           vargs  = create_instr_vargs_impl(cnt, vargs_type, values);
		ref_instr(vargs);

		if (vargsc > 0) {
			/* One or more vargs passed. */
			// TODO: check it this is ok!!!
			// TODO: check it this is ok!!!
			// TODO: check it this is ok!!!
			// TODO: check it this is ok!!!
			for (size_t i = 0; i < vargsc; ++i) {
				sa_push_InstrPtr(values, call->args->data[callee_argc + i]);
			}

			MirInstr *insert_loc = call->args->data[callee_argc];
			insert_instr_after(insert_loc, vargs);
		} else if (callee_argc > 0) {
			/* No arguments passed into vargs but there are more regular
			 * arguments before vargs. */
			MirInstr *insert_loc = call->args->data[0];
			insert_instr_before(insert_loc, vargs);
		} else {
			insert_instr_before(&call->base, vargs);
		}

		if (analyze_instr_vargs(cnt, (MirInstrVArgs *)vargs).state != ANALYZE_PASSED)
			return ANALYZE_RESULT(FAILED, 0);

		vargs->analyzed = true;

		/* Erase vargs from arguments. */
		sa_resize_InstrPtr(call->args, callee_argc);

		/* Replace last with vargs. */
		sa_push_InstrPtr(call->args, vargs);
	} else {
		if ((callee_argc != call_argc)) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_INVALID_ARG_COUNT,
			            call->base.node->location,
			            BUILDER_CUR_WORD,
			            "Expected %u %s, but called with %u.",
			            callee_argc,
			            callee_argc == 1 ? "argument" : "arguments",
			            call_argc);
			return ANALYZE_RESULT(FAILED, 0);
		}
	}

	/* validate argument types */
	if (callee_argc) {
		MirInstr **call_arg;
		MirArg *   callee_arg;
		bool       valid = true;

		for (u32 i = 0; i < callee_argc && valid; ++i) {
			call_arg   = &call->args->data[i];
			callee_arg = type->data.fn.args->data[i];
			BL_ASSERT(callee_arg);

			if (analyze_slot(
			        cnt, &analyze_slot_conf_full, call_arg, callee_arg->type) !=
			    ANALYZE_PASSED) {
				return ANALYZE_RESULT(FAILED, 0);
			}
		}
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_store(Context *cnt, MirInstrStore *store)
{
	MirInstr *dest = store->dest;
	BL_ASSERT(dest);
	BL_ASSERT(dest->analyzed);

	if (!mir_is_pointer_type(dest->value.type)) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_EXPR,
		            store->base.node->location,
		            BUILDER_CUR_WORD,
		            "Left hand side of the expression cannot be assigned.");
		return ANALYZE_RESULT(FAILED, 0);
	}

	if (dest->value.addr_mode == MIR_VAM_LVALUE_CONST) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_INVALID_EXPR,
		            store->base.node->location,
		            BUILDER_CUR_WORD,
		            "Cannot assign to constant.");
	}

	MirType *dest_type = mir_deref_type(dest->value.type);
	BL_ASSERT(dest_type && "store destination has invalid base type");

	if (analyze_slot(cnt, &analyze_slot_conf_default, &store->src, dest_type) !=
	    ANALYZE_PASSED) {
		return ANALYZE_RESULT(FAILED, 0);
	}

	reduce_instr(cnt, store->dest);

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult
analyze_instr_block(Context *cnt, MirInstrBlock *block)
{
	BL_ASSERT(block);

	MirFn *fn = block->owner_fn;
	BL_ASSERT(fn);
	MirInstrFnProto *fn_proto = (MirInstrFnProto *)fn->prototype;

	/* append implicit return for void functions or generate error when last
	 * block is not terminated
	 */
	if (!is_block_terminated(block)) {
		if (fn->type->data.fn.ret_type->kind == MIR_TYPE_VOID) {
			set_current_block(cnt, block);
			append_instr_ret(cnt, NULL, NULL, false);
		} else {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_MISSING_RETURN,
			            fn->decl_node->location,
			            BUILDER_CUR_WORD,
			            "Not every path inside function return value.");
		}
	}

	if (block->base.ref_count == 0 && !fn_proto->first_unrechable_location) {
		MirInstr *first_instr = block->entry_instr;
		if (first_instr && first_instr->node) {
			fn_proto->first_unrechable_location = first_instr->node->location;

			builder_msg(BUILDER_MSG_WARNING,
			            0,
			            fn_proto->first_unrechable_location,
			            BUILDER_CUR_NONE,
			            "Unrechable code detected.");
		}
	}

	return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeState
analyze_slot(Context *cnt, const AnalyzeSlotConfig *conf, MirInstr **input, MirType *slot_type)
{
	AnalyzeStageState state;
	for (s32 i = 0; i < conf->count; ++i) {
		state = conf->stages[i](cnt, input, slot_type);
		switch (state) {
		case ANALYZE_STAGE_BREAK:
			goto DONE;
		case ANALYZE_STAGE_FAILED:
			goto FAILED;
		case ANALYZE_STAGE_CONTINUE:
			break;
		}
	}

DONE:
	reduce_instr(cnt, *input);
	return ANALYZE_PASSED;
FAILED:
	return ANALYZE_FAILED;
}

AnalyzeStageState
analyze_stage_load(Context *cnt, MirInstr **input, MirType *slot_type)
{
	if (is_load_needed(*input)) {
		*input = insert_instr_load(cnt, *input);
	}

	return ANALYZE_STAGE_CONTINUE;
}

AnalyzeStageState
analyze_stage_set_null(Context *cnt, MirInstr **input, MirType *slot_type)
{
	BL_ASSERT(slot_type);
	MirInstr *_input = *input;

	if (_input->kind != MIR_INSTR_CONST) return ANALYZE_STAGE_CONTINUE;
	if (_input->value.type->kind != MIR_TYPE_NULL) return ANALYZE_STAGE_CONTINUE;

	if (slot_type->kind == MIR_TYPE_NULL) {
		_input->value.type = slot_type;
		return ANALYZE_STAGE_BREAK;
	}

	if (mir_is_pointer_type(slot_type)) {
		_input->value.type = create_type_null(cnt, slot_type);
		return ANALYZE_STAGE_BREAK;
	}

	builder_msg(BUILDER_MSG_ERROR,
	            ERR_INVALID_TYPE,
	            _input->node->location,
	            BUILDER_CUR_WORD,
	            "Invalid use of null constant.");

	return ANALYZE_STAGE_FAILED;
}

AnalyzeStageState
analyze_stage_set_auto(Context *cnt, MirInstr **input, MirType *slot_type)
{
	BL_ASSERT(slot_type);
	MirInstr *_input = *input;

	if (_input->kind != MIR_INSTR_CAST) return ANALYZE_STAGE_CONTINUE;
	if (!((MirInstrCast *)_input)->auto_cast) return ANALYZE_STAGE_CONTINUE;

	_input->value.type = slot_type;
	if (analyze_instr_cast(cnt, (MirInstrCast *)_input, true).state != ANALYZE_PASSED) {
		return ANALYZE_STAGE_FAILED;
	}

	return ANALYZE_STAGE_BREAK;
}

AnalyzeStageState
analyze_stage_toany(Context *cnt, MirInstr **input, MirType *slot_type)
{
	BL_ASSERT(slot_type);

	/* check any */
	if (!is_to_any_needed(cnt, *input, slot_type)) return ANALYZE_STAGE_CONTINUE;

	*input = insert_instr_toany(cnt, *input);
	*input = insert_instr_load(cnt, *input);

	return ANALYZE_STAGE_BREAK;
}

AnalyzeStageState
analyze_stage_set_volatile_expr(Context *cnt, MirInstr **input, MirType *slot_type)
{
	BL_ASSERT(slot_type);
	if (slot_type->kind != MIR_TYPE_INT) return ANALYZE_STAGE_CONTINUE;
	if (!is_volatile_expr(*input)) return ANALYZE_STAGE_CONTINUE;

	(*input)->value.type = slot_type;
	return ANALYZE_STAGE_BREAK;
}

AnalyzeStageState
analyze_stage_implicit_cast(Context *cnt, MirInstr **input, MirType *slot_type)
{
	if (type_cmp((*input)->value.type, slot_type)) return ANALYZE_STAGE_BREAK;
	if (!can_impl_cast((*input)->value.type, slot_type)) return ANALYZE_STAGE_CONTINUE;

	*input = insert_instr_cast(cnt, *input, slot_type);
	return ANALYZE_STAGE_BREAK;
}

AnalyzeStageState
analyze_stage_report_type_mismatch(Context *cnt, MirInstr **input, MirType *slot_type)
{
	error_types((*input)->value.type, slot_type, (*input)->node, NULL);
	return ANALYZE_STAGE_CONTINUE;
}

AnalyzeResult
analyze_instr(Context *cnt, MirInstr *instr)
{
	if (!instr) return ANALYZE_RESULT(PASSED, 0);

	/* skip already analyzed instructions */
	if (instr->analyzed) return ANALYZE_RESULT(PASSED, 0);
	AnalyzeResult state = ANALYZE_RESULT(PASSED, 0);

	if (instr->owner_block) set_current_block(cnt, instr->owner_block);

	switch (instr->kind) {
	case MIR_INSTR_VARGS:
	case MIR_INSTR_INVALID:
		break;

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
	case MIR_INSTR_DECL_VARIANT:
		state = analyze_instr_decl_variant(cnt, (MirInstrDeclVariant *)instr);
		break;
	case MIR_INSTR_DECL_ARG:
		state = analyze_instr_decl_arg(cnt, (MirInstrDeclArg *)instr);
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
	case MIR_INSTR_TYPE_ENUM:
		state = analyze_instr_type_enum(cnt, (MirInstrTypeEnum *)instr);
		break;
	case MIR_INSTR_LOAD:
		state = analyze_instr_load(cnt, (MirInstrLoad *)instr);
		break;
	case MIR_INSTR_COMPOUND:
		state = analyze_instr_compound(cnt, (MirInstrCompound *)instr);
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
		state = analyze_instr_cast(cnt, (MirInstrCast *)instr, false);
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
	case MIR_INSTR_PHI:
		state = analyze_instr_phi(cnt, (MirInstrPhi *)instr);
		break;
	case MIR_INSTR_TOANY:
		state = analyze_instr_toany(cnt, (MirInstrToAny *)instr);
		break;
	case MIR_INSTR_DECL_DIRECT_REF:
		state = analyze_instr_decl_direct_ref(cnt, (MirInstrDeclDirectRef *)instr);
		break;
	}

	instr->analyzed = state.state == ANALYZE_PASSED;

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

	/* Instruction can be the last instruction inside block, but block may not
	 * be the last block inside function, we try to get following one. */
	MirInstrBlock *owner_block = instr->owner_block;
	if (owner_block && instr == owner_block->last_instr) {
		if (owner_block->base.next == NULL) {
			/* Instruction is last instruction of the function body, so the
			 * function can be executed in compile time if needed, we need to
			 * set flag with this information here. */
			owner_block->owner_fn->fully_analyzed = true;
#if BL_DEBUG && VERBOSE_ANALYZE
			printf("Analyze: " BLUE("Function '%s' completely analyzed.\n"),
			       owner_block->owner_fn->linkage_name);
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
	if (cnt->analyze.verbose_pre) {
		MirInstr *instr;
		BArray *  globals = cnt->assembly->MIR.global_instrs;
		BARRAY_FOREACH(globals, instr)
		{
			mir_print_instr(instr, stdout);
		}
	}

	/* PERFORMANCE: use array??? */
	/* PERFORMANCE: use array??? */
	/* PERFORMANCE: use array??? */
	BList *       q = cnt->analyze.queue;
	AnalyzeResult result;
	size_t        postpone_loop_count = 0;
	MirInstr *    ip                  = NULL;
	MirInstr *    prev_ip             = NULL;
	bool          skip                = false;

	if (bo_list_empty(q)) return;

	while (true) {
		prev_ip = ip;
		ip      = skip ? NULL : analyze_try_get_next(ip);

		if (prev_ip && prev_ip->analyzed) {
			erase_instr_tree(prev_ip);
		}

		if (!ip) {
			if (bo_list_empty(q)) break;

			ip = bo_list_front(q, MirInstr *);
			bo_list_pop_front(q);
			skip = false;
		}

		result = analyze_instr(cnt, ip);

		switch (result.state) {
		case ANALYZE_INCOMPLETE:
		case ANALYZE_PASSED:
#if BL_DEBUG && VERBOSE_ANALYZE
			printf("Analyze: [ " GREEN("PASSED") " ] %16s\n", mir_instr_name(ip));
#endif
			postpone_loop_count = 0;
			break;

		case ANALYZE_FAILED:
#if BL_DEBUG && VERBOSE_ANALYZE
			printf("Analyze: [ " RED("FAILED") " ] %16s\n", mir_instr_name(ip));
#endif
			skip                = true;
			postpone_loop_count = 0;
			break;

		case ANALYZE_POSTPONE:
#if BL_DEBUG && VERBOSE_ANALYZE
			printf("Analyze: [" MAGENTA("POSTPONE") "] %16s\n", mir_instr_name(ip));
#endif

			skip = true;
			if (postpone_loop_count++ < bo_list_size(q)) bo_list_push_back(q, ip);
			break;

		case ANALYZE_WAITING: {
#if BL_DEBUG && VERBOSE_ANALYZE
			printf("Analyze: [  " YELLOW("WAIT") "  ] %16s is waiting for: '%llu'\n",
			       mir_instr_name(ip),
			       (unsigned long long)state);
#endif

			BArray *      wq   = NULL;
			bo_iterator_t iter = bo_htbl_find(cnt->analyze.waiting, result.waiting_for);
			bo_iterator_t end  = bo_htbl_end(cnt->analyze.waiting);
			if (bo_iterator_equal(&iter, &end)) {
				wq = bo_array_new(sizeof(MirInstr *));
				bo_array_reserve(wq, 16);
				bo_htbl_insert(cnt->analyze.waiting, result.waiting_for, wq);
			} else {
				wq = bo_htbl_iter_peek_value(cnt->analyze.waiting, &iter, BArray *);
			}

			BL_ASSERT(wq);
			bo_array_push_back(wq, ip);
			skip                = true;
			postpone_loop_count = 0;
		}
		}
	}

	if (cnt->analyze.verbose_post) {
		MirInstr *instr;
		BArray *  globals = cnt->assembly->MIR.global_instrs;
		BARRAY_FOREACH(globals, instr)
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

	BHTBL_FOREACH(cnt->analyze.waiting, iter)
	{
		wq = bo_htbl_iter_peek_value(cnt->analyze.waiting, &iter, BArray *);
		BL_ASSERT(wq);
		BARRAY_FOREACH(wq, instr)
		{
			BL_ASSERT(instr);

			builder_msg(BUILDER_MSG_ERROR,
			            ERR_UNKNOWN_SYMBOL,
			            instr->node->location,
			            BUILDER_CUR_WORD,
			            "Unknown symbol.");
		}
	}
}

/*
 * Push RTTI variable to the array of RTTIs, do stack allocation for execution and push
 * current value on the stack.
 */
static inline MirVar *
gen_RTTI_var(Context *cnt, MirType *type, MirConstValueData *value)
{
	const char *name = gen_uq_name(IMPL_RTTI_ENTRY);
	MirVar *    var  = create_var_impl(cnt, name, type, false, true, false);
	var->value.data  = *value;

	vm_create_implicit_global(&cnt->vm, var);

	/* Push into RTTI table */
	bo_array_push_back(cnt->assembly->MIR.RTTI_tmp_vars, var);
	return var;
}

MirConstValue *
gen_RTTI_base(Context *cnt, s32 kind, size_t size_bytes)
{
	MirType *struct_type = lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO);
	BL_ASSERT(struct_type);

	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);

	/* .kind */
	MirType *kind_type = lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_KIND);
	BL_ASSERT(kind_type);

	/* kind */
	sa_push_ConstValuePtr(m, init_or_create_const_integer(cnt, NULL, kind_type, kind));

	/* size_bytes */
	sa_push_ConstValuePtr(
	    m, init_or_create_const_integer(cnt, NULL, cnt->builtin_types.t_usize, size_bytes));

	return init_or_create_const_struct(cnt, NULL, struct_type, m);
}

MirVar *
gen_RTTI_empty(Context *cnt, MirType *type, MirType *rtti_type)
{
	MirConstValueData rtti_value = {0};

	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);
	/* .base */
	sa_push_ConstValuePtr(m, gen_RTTI_base(cnt, type->kind, type->store_size_bytes));

	/* set members */
	rtti_value.v_struct.members = m;

	/* setup type RTTI and push */
	return gen_RTTI_var(cnt, rtti_type, &rtti_value);
}

MirVar *
gen_RTTI_int(Context *cnt, MirType *type)
{
	const s32  bitcount  = type->data.integer.bitcount;
	const bool is_signed = type->data.integer.is_signed;

	MirConstValueData rtti_value = {0};

	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);
	/* .base */
	sa_push_ConstValuePtr(m, gen_RTTI_base(cnt, type->kind, type->store_size_bytes));

	/* .bitcount */
	sa_push_ConstValuePtr(
	    m, init_or_create_const_integer(cnt, NULL, cnt->builtin_types.t_s32, bitcount));

	/* .is_signed */
	sa_push_ConstValuePtr(m, init_or_create_const_bool(cnt, NULL, is_signed));

	/* set members */
	rtti_value.v_struct.members = m;

	/* setup type RTTI and push */
	return gen_RTTI_var(cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_INT), &rtti_value);
}

MirVar *
gen_RTTI_real(Context *cnt, MirType *type)
{
	const s32 bitcount = type->data.integer.bitcount;

	MirConstValueData rtti_value = {0};

	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);
	/* .base */
	sa_push_ConstValuePtr(m, gen_RTTI_base(cnt, type->kind, type->store_size_bytes));

	/* .bitcount */
	sa_push_ConstValuePtr(
	    m, init_or_create_const_integer(cnt, NULL, cnt->builtin_types.t_s32, bitcount));

	/* set members */
	rtti_value.v_struct.members = m;

	/* setup type RTTI and push */
	return gen_RTTI_var(cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_REAL), &rtti_value);
}

MirVar *
gen_RTTI_ptr(Context *cnt, MirType *type)
{
	MirVar *rtti_pointed = gen_RTTI(cnt, type->data.ptr.expr);

	MirConstValueData rtti_value = {0};

	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);
	/* .base */
	sa_push_ConstValuePtr(m, gen_RTTI_base(cnt, type->kind, type->store_size_bytes));

	sa_push_ConstValuePtr(m,
	                      init_or_create_const_var_ptr(
	                          cnt, NULL, cnt->builtin_types.t_TypeInfo_ptr, rtti_pointed));

	/* set members */
	rtti_value.v_struct.members = m;

	/* setup type RTTI and push */
	return gen_RTTI_var(cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_PTR), &rtti_value);
}

MirConstValue *
gen_RTTI_enum_variant(Context *cnt, MirVariant *variant)
{
	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);

	/* .name */
	sa_push_ConstValuePtr(m,
	                      init_or_create_const_string(
	                          cnt, NULL, variant->id ? variant->id->str : "<implicit_member>"));

	/* .value */
	sa_push_ConstValuePtr(m,
	                      init_or_create_const_integer(
	                          cnt, NULL, cnt->builtin_types.t_s64, variant->value->data.v_s64));

	return init_or_create_const_struct(
	    cnt, NULL, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_ENUM_VARIANT), m);
}

MirConstValue *
gen_RTTI_slice_of_enum_variants(Context *cnt, SmallArray_VariantPtr *variants)
{
	/* First build-up an array variable containing pointers to TypeInfo. */
	MirType *array_type = create_type_array(
	    cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_ENUM_VARIANT), variants->size);
	// MirVar *array_var = _create_and_alloc_RTTI_var(cnt, array_type);

	MirConstValueData         array_value = {0};
	SmallArray_ConstValuePtr *elems = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);

	MirVariant *variant;
	SARRAY_FOREACH(variants, variant)
	{
		sa_push_ConstValuePtr(elems, gen_RTTI_enum_variant(cnt, variant));
	}

	array_value.v_array.elems = elems;
	MirVar *array_var         = gen_RTTI_var(cnt, array_type, &array_value);

	/* Create slice. */
	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);

	/* len */
	sa_push_ConstValuePtr(
	    m, init_or_create_const_integer(cnt, NULL, cnt->builtin_types.t_s64, variants->size));

	/* ptr */
	sa_push_ConstValuePtr(
	    m,
	    init_or_create_const_var_ptr(cnt, NULL, create_type_ptr(cnt, array_type), array_var));

	return init_or_create_const_struct(
	    cnt, NULL, cnt->builtin_types.t_TypeInfoEnumVariants_slice, m);
}

MirVar *
gen_RTTI_enum(Context *cnt, MirType *type)
{
	MirVar *rtti_pointed = gen_RTTI(cnt, type->data.enm.base_type);

	MirConstValueData rtti_value = {0};

	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);
	/* .base */
	sa_push_ConstValuePtr(m, gen_RTTI_base(cnt, type->kind, type->store_size_bytes));

	/* .name */
	sa_push_ConstValuePtr(
	    m,
	    init_or_create_const_string(
	        cnt, NULL, type->user_id ? type->user_id->str : "<implicit_enum>"));

	/* .base_type */
	sa_push_ConstValuePtr(m,
	                      init_or_create_const_var_ptr(
	                          cnt, NULL, cnt->builtin_types.t_TypeInfo_ptr, rtti_pointed));

	/* .variants */
	sa_push_ConstValuePtr(m, gen_RTTI_slice_of_enum_variants(cnt, type->data.enm.variants));

	/* set members */
	rtti_value.v_struct.members = m;

	/* setup type RTTI and push */
	return gen_RTTI_var(cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_ENUM), &rtti_value);
}

MirVar *
gen_RTTI_array(Context *cnt, MirType *type)
{
	const s64 len          = type->data.array.len;
	MirVar *  rtti_pointed = gen_RTTI(cnt, type->data.array.elem_type);

	MirConstValueData rtti_value = {0};

	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);
	/* .base */
	sa_push_ConstValuePtr(m, gen_RTTI_base(cnt, type->kind, type->store_size_bytes));

	/* .name */
	sa_push_ConstValuePtr(
	    m,
	    init_or_create_const_string(
	        cnt, NULL, type->user_id ? type->user_id->str : "<implicit_array>"));

	/* .base_type */
	sa_push_ConstValuePtr(m,
	                      init_or_create_const_var_ptr(
	                          cnt, NULL, cnt->builtin_types.t_TypeInfo_ptr, rtti_pointed));

	sa_push_ConstValuePtr(
	    m, init_or_create_const_integer(cnt, NULL, cnt->builtin_types.t_s64, len));

	/* set members */
	rtti_value.v_struct.members = m;

	/* setup type RTTI and push */
	return gen_RTTI_var(cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_ARRAY), &rtti_value);
}

MirConstValue *
gen_RTTI_slice_of_TypeInfo_ptr(Context *cnt, SmallArray_TypePtr *types)
{
	/* First build-up an array variable containing pointers to TypeInfo. */
	MirType *array_type =
	    create_type_array(cnt, cnt->builtin_types.t_TypeInfo_ptr, types->size);

	MirConstValueData         array_value = {0};
	SmallArray_ConstValuePtr *elems = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);

	MirType *type;
	SARRAY_FOREACH(types, type)
	{
		sa_push_ConstValuePtr(
		    elems,
		    init_or_create_const_var_ptr(
		        cnt, NULL, cnt->builtin_types.t_TypeInfo_ptr, gen_RTTI(cnt, type)));
	}

	array_value.v_array.elems = elems;
	MirVar *array_var         = gen_RTTI_var(cnt, array_type, &array_value);

	/* Create slice. */
	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);

	/* len */
	sa_push_ConstValuePtr(
	    m, init_or_create_const_integer(cnt, NULL, cnt->builtin_types.t_s64, types->size));

	/* ptr */
	sa_push_ConstValuePtr(
	    m,
	    init_or_create_const_var_ptr(cnt, NULL, create_type_ptr(cnt, array_type), array_var));

	return init_or_create_const_struct(cnt, NULL, cnt->builtin_types.t_TypeInfo_slice, m);
}

MirConstValue *
gen_RTTI_struct_member(Context *cnt, MirMember *member)
{
	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);

	/* .name */
	sa_push_ConstValuePtr(m,
	                      init_or_create_const_string(
	                          cnt, NULL, member->id ? member->id->str : "<implicit_member>"));

	/* .base_type */
	sa_push_ConstValuePtr(
	    m,
	    init_or_create_const_var_ptr(
	        cnt, NULL, cnt->builtin_types.t_TypeInfo_ptr, gen_RTTI(cnt, member->type)));

	/* .offset_bytes */
	sa_push_ConstValuePtr(m,
	                      init_or_create_const_integer(
	                          cnt, NULL, cnt->builtin_types.t_s32, member->offset_bytes));

	/* .index */
	sa_push_ConstValuePtr(
	    m, init_or_create_const_integer(cnt, NULL, cnt->builtin_types.t_s32, member->index));

	return init_or_create_const_struct(
	    cnt, NULL, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_STRUCT_MEMBER), m);
}

MirConstValue *
gen_RTTI_slice_of_struct_members(Context *cnt, SmallArray_MemberPtr *members)
{
	/* First build-up an array variable containing pointers to TypeInfo. */
	MirType *array_type = create_type_array(
	    cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_STRUCT_MEMBER), members->size);

	MirConstValueData         array_value = {0};
	SmallArray_ConstValuePtr *elems = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);

	MirMember *member;
	SARRAY_FOREACH(members, member)
	{
		sa_push_ConstValuePtr(elems, gen_RTTI_struct_member(cnt, member));
	}

	array_value.v_array.elems = elems;
	MirVar *array_var         = gen_RTTI_var(cnt, array_type, &array_value);

	/* Create slice. */
	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);

	/* len */
	sa_push_ConstValuePtr(
	    m, init_or_create_const_integer(cnt, NULL, cnt->builtin_types.t_s64, members->size));

	/* ptr */
	sa_push_ConstValuePtr(
	    m,
	    init_or_create_const_var_ptr(cnt, NULL, create_type_ptr(cnt, array_type), array_var));

	return init_or_create_const_struct(
	    cnt, NULL, cnt->builtin_types.t_TypeInfoStructMembers_slice, m);
}

MirVar *
gen_RTTI_struct(Context *cnt, MirType *type)
{
	MirConstValueData rtti_value = {0};

	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);
	/* .base */
	sa_push_ConstValuePtr(
	    m,
	    gen_RTTI_base(
	        cnt, MIR_TYPE_STRUCT, type->store_size_bytes)); /* use same for MIR_TYPE_SLICE!!! */

	/* name */
	sa_push_ConstValuePtr(
	    m,
	    init_or_create_const_string(
	        cnt, NULL, type->user_id ? type->user_id->str : "<implicit_struct>"));

	/* .members */
	sa_push_ConstValuePtr(m, gen_RTTI_slice_of_struct_members(cnt, type->data.strct.members));

	/* .is_slice */
	sa_push_ConstValuePtr(
	    m,
	    init_or_create_const_bool(
	        cnt, NULL, type->kind == MIR_TYPE_SLICE || type->kind == MIR_TYPE_VARGS));

	/* set members */
	rtti_value.v_struct.members = m;

	/* setup type RTTI and push */
	return gen_RTTI_var(cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_STRUCT), &rtti_value);
}

MirVar *
gen_RTTI_fn(Context *cnt, MirType *type)
{
	MirConstValueData rtti_value = {0};

	SmallArray_ConstValuePtr *m = create_sarr(SmallArray_ConstValuePtr, cnt->assembly);
	/* .base */
	sa_push_ConstValuePtr(m, gen_RTTI_base(cnt, type->kind, type->store_size_bytes));

	/* .args */
	SmallArray_TypePtr types;
	sa_init(&types);

	/* TODO: make RTTI for function arguments more descriptive and include whole Arg
	 * informations instead of type only. */
	MirArg *it;
	if (type->data.fn.args) {
		SARRAY_FOREACH(type->data.fn.args, it)
		{
			BL_ASSERT(it->type)
			sa_push_TypePtr(&types, it->type);
		}
	}

	sa_push_ConstValuePtr(m, gen_RTTI_slice_of_TypeInfo_ptr(cnt, &types));

	/* .ret */
	sa_push_ConstValuePtr(m,
	                      init_or_create_const_var_ptr(cnt,
	                                                   NULL,
	                                                   cnt->builtin_types.t_TypeInfo_ptr,
	                                                   gen_RTTI(cnt, type->data.fn.ret_type)));

	/* .is_vargs  */
	sa_push_ConstValuePtr(m, init_or_create_const_bool(cnt, NULL, type->data.fn.is_vargs));

	/* set members */
	rtti_value.v_struct.members = m;

	/* setup type RTTI and push */
	MirVar *rtti_var =
	    gen_RTTI_var(cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_FN), &rtti_value);
	sa_terminate(&types);
	return rtti_var;
}

MirVar *
gen_RTTI(Context *cnt, MirType *type)
{
	BL_ASSERT(type);
	if (type->rtti.var) return type->rtti.var;

	/* NEW */
	switch (type->kind) {
	case MIR_TYPE_TYPE:
		type->rtti.var =
		    gen_RTTI_empty(cnt, type, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_TYPE));
		return type->rtti.var;

	case MIR_TYPE_VOID:
		type->rtti.var =
		    gen_RTTI_empty(cnt, type, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_VOID));
		return type->rtti.var;

	case MIR_TYPE_BOOL:
		type->rtti.var =
		    gen_RTTI_empty(cnt, type, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_BOOL));
		return type->rtti.var;

	case MIR_TYPE_NULL:
		type->rtti.var =
		    gen_RTTI_empty(cnt, type, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_NULL));
		return type->rtti.var;

	case MIR_TYPE_STRING:
		type->rtti.var =
		    gen_RTTI_empty(cnt, type, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_STRING));
		return type->rtti.var;

	case MIR_TYPE_INT:
		type->rtti.var = gen_RTTI_int(cnt, type);
		return type->rtti.var;

	case MIR_TYPE_REAL:
		type->rtti.var = gen_RTTI_real(cnt, type);
		return type->rtti.var;

	case MIR_TYPE_PTR:
		type->rtti.var = gen_RTTI_ptr(cnt, type);
		return type->rtti.var;

	case MIR_TYPE_ENUM:
		type->rtti.var = gen_RTTI_enum(cnt, type);
		return type->rtti.var;

	case MIR_TYPE_ARRAY:
		type->rtti.var = gen_RTTI_array(cnt, type);
		return type->rtti.var;

	case MIR_TYPE_SLICE:
	case MIR_TYPE_VARGS:
	case MIR_TYPE_STRUCT:
		type->rtti.var = gen_RTTI_struct(cnt, type);
		return type->rtti.var;

	case MIR_TYPE_FN:
		type->rtti.var = gen_RTTI_fn(cnt, type);
		return type->rtti.var;

	case MIR_TYPE_INVALID:
		break;
	}

	char type_name[256];
	mir_type_to_str(type_name, 256, type, true);
	BL_ABORT("missing RTTI generation for type '%s'", type_name);
}

/*
 * Generate global type table in data segment of an assembly.
 */
void
gen_RTTI_types(Context *cnt)
{
	BHashTable *table = cnt->analyze.RTTI_entry_types;
	if (bo_htbl_size(table) == 0) return;

	{ /* Preload RTTI provided types */
		cnt->builtin_types.t_TypeInfo_ptr =
		    create_type_ptr(cnt, lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO));

		cnt->builtin_types.t_TypeInfo_slice = create_type_struct_special(
		    cnt, MIR_TYPE_SLICE, NULL, cnt->builtin_types.t_TypeInfo_ptr);

		cnt->builtin_types.t_TypeInfoStructMembers_slice = create_type_struct_special(
		    cnt,
		    MIR_TYPE_SLICE,
		    NULL,
		    create_type_ptr(cnt,
		                    lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_STRUCT_MEMBER)));

		cnt->builtin_types.t_TypeInfoEnumVariants_slice = create_type_struct_special(
		    cnt,
		    MIR_TYPE_SLICE,
		    NULL,
		    create_type_ptr(cnt,
		                    lookup_builtin(cnt, MIR_BUILTIN_ID_TYPE_INFO_ENUM_VARIANT)));
	}

	bo_iterator_t it;
	MirType *     type;
	BHTBL_FOREACH(table, it)
	{
		type = (MirType *)bo_htbl_iter_peek_key(table, &it);
		gen_RTTI(cnt, type);
	}
}

/* MIR builting */
void
ast_defer_block(Context *cnt, Ast *block, bool whole_tree)
{
	SmallArray_DeferStack *stack = &cnt->ast.defer_stack;
	Ast *                  defer;

	for (size_t i = stack->size; i-- > 0;) {
		defer = stack->data[i];

		if (defer->owner_scope == block->owner_scope) {
			sa_pop_DeferStack(stack);
		} else if (!whole_tree) {
			break;
		}

		ast(cnt, defer->data.stmt_defer.expr);
	}
}

void
ast_ublock(Context *cnt, Ast *ublock)
{
	Ast *tmp;
	BARRAY_FOREACH(ublock->data.ublock.nodes, tmp) ast(cnt, tmp);
}

void
ast_block(Context *cnt, Ast *block)
{
	if (cnt->debug_mode) init_llvm_DI_scope(cnt, block->owner_scope);

	Ast *tmp;
	BARRAY_FOREACH(block->data.block.nodes, tmp) ast(cnt, tmp);

	if (!block->data.block.has_return) ast_defer_block(cnt, block, false);
}

void
ast_test_case(Context *cnt, Ast *test)
{
	/* build test function */
	Ast *ast_block = test->data.test_case.block;
	BL_ASSERT(ast_block);

	MirInstrFnProto *fn_proto =
	    (MirInstrFnProto *)append_instr_fn_proto(cnt, test, NULL, NULL, true);

	fn_proto->base.value.type = cnt->builtin_types.t_test_case_fn;

	const bool  emit_llvm    = builder.options.force_test_llvm;
	const char *linkage_name = gen_uq_name(TEST_CASE_FN_NAME);
	const bool  is_in_gscope =
	    test->owner_scope->kind == SCOPE_GLOBAL || test->owner_scope->kind == SCOPE_PRIVATE;
	MirFn *fn =
	    create_fn(cnt, test, NULL, linkage_name, FLAG_TEST, fn_proto, emit_llvm, is_in_gscope);

	BL_ASSERT(test->data.test_case.desc);
	fn->test_case_desc = test->data.test_case.desc;
	mir_set_const_ptr(&fn_proto->base.value.data.v_ptr, fn, MIR_CP_FN);

	bo_array_push_back(cnt->test_cases, fn);

	MirInstrBlock *entry_block = append_block(cnt, fn, "entry");

	cnt->ast.exit_block = append_block(cnt, fn, "exit");

	set_current_block(cnt, cnt->ast.exit_block);
	append_instr_ret(cnt, NULL, NULL, false);

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
	BL_ASSERT(ast_cond && ast_then);

	MirFn *fn = get_current_fn(cnt);
	BL_ASSERT(fn);

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
	BL_ASSERT(ast_block);

	MirFn *fn = get_current_fn(cnt);
	BL_ASSERT(fn);

	/* prepare all blocks */
	MirInstrBlock *tmp_block = NULL;
	MirInstrBlock *increment_block =
	    ast_increment ? append_block(cnt, fn, "loop_increment") : NULL;
	MirInstrBlock *decide_block = append_block(cnt, fn, "loop_decide");
	MirInstrBlock *body_block   = append_block(cnt, fn, "loop_body");
	MirInstrBlock *cont_block   = append_block(cnt, fn, "loop_continue");

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
	BL_ASSERT(cnt->ast.break_block && "break statement outside the loop");
	append_instr_br(cnt, br, cnt->ast.break_block);
}

void
ast_stmt_continue(Context *cnt, Ast *cont)
{
	BL_ASSERT(cnt->ast.continue_block && "break statement outside the loop");
	append_instr_br(cnt, cont, cnt->ast.continue_block);
}

void
ast_stmt_return(Context *cnt, Ast *ret)
{
	/* Return statement produce only setup of .ret temporary and break into the exit
	 * block of the function. */
	MirInstr *value = ast(cnt, ret->data.stmt_return.expr);

	if (!is_current_block_terminated(cnt)) {
		MirFn *fn = get_current_fn(cnt);
		BL_ASSERT(fn);

		if (fn->ret_tmp) {
			if (!value) {
				builder_msg(BUILDER_MSG_ERROR,
				            ERR_EXPECTED_EXPR,
				            ret->location,
				            BUILDER_CUR_AFTER,
				            "Expected return value.");
			}

			MirInstr *ref = append_instr_decl_direct_ref(cnt, fn->ret_tmp);
			append_instr_store(cnt, ret, value, ref);
		} else if (value) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_UNEXPECTED_EXPR,
			            value->node->location,
			            BUILDER_CUR_WORD,
			            "Unexpected return value.");
		}

		ast_defer_block(cnt, ret->data.stmt_return.owner_block, true);
	}

	BL_ASSERT(cnt->ast.exit_block);
	append_instr_br(cnt, ret, cnt->ast.exit_block);
}

void
ast_stmt_defer(Context *cnt, Ast *defer)
{
	/* push new defer record */
	sa_push_DeferStack(&cnt->ast.defer_stack, defer);
}

MirInstr *
ast_expr_compound(Context *cnt, Ast *cmp)
{
	SmallArray_AstPtr *ast_values = cmp->data.expr_compound.values;
	Ast *              ast_type   = cmp->data.expr_compound.type;
	MirInstr *         type       = ast(cnt, ast_type);
	BL_ASSERT(type);

	if (!ast_values) {
		return append_instr_compound(cnt, cmp, type, NULL);
	}

	const size_t valc = ast_values->size;

	BL_ASSERT(ast_type);

	SmallArray_InstrPtr *values = create_sarr(SmallArray_InstrPtr, cnt->assembly);
	sa_resize_InstrPtr(values, valc);

	Ast *     ast_value;
	MirInstr *value;

	/* Values must be appended in reverse order. */
	for (size_t i = valc; i-- > 0;) {
		ast_value = ast_values->data[i];
		value     = ast(cnt, ast_value);
		BL_ASSERT(value);
		values->data[i] = value;
	}

	return append_instr_compound(cnt, cmp, type, values);
}

MirInstr *
ast_expr_line(Context *cnt, Ast *line)
{
	const s32 l = line->data.expr_line.line;
	return append_instr_const_int(cnt, line, cnt->builtin_types.t_s32, l);
};

MirInstr *
ast_expr_file(Context *cnt, Ast *file)
{
	const char *f = file->data.expr_file.filename;
	return append_instr_const_string(cnt, file, f);
}

MirInstr *
ast_expr_addrof(Context *cnt, Ast *addrof)
{
	MirInstr *src = ast(cnt, addrof->data.expr_addrof.next);
	BL_ASSERT(src);

	return append_instr_addrof(cnt, addrof, src);
}

MirInstr *
ast_expr_cast(Context *cnt, Ast *cast)
{
	const bool auto_cast = cast->data.expr_cast.auto_cast;
	Ast *      ast_type  = cast->data.expr_cast.type;
	Ast *      ast_next  = cast->data.expr_cast.next;
	BL_ASSERT(ast_next);

	// TODO: const type!!!
	MirInstr *type = NULL;

	if (!auto_cast) {
		BL_ASSERT(ast_type);
		type = ast_create_impl_fn_call(
		    cnt, ast_type, RESOLVE_TYPE_FN_NAME, cnt->builtin_types.t_resolve_type_fn);
	}

	MirInstr *next = ast(cnt, ast_next);

	return append_instr_cast(cnt, cast, type, next);
}

MirInstr *
ast_expr_sizeof(Context *cnt, Ast *szof)
{
	Ast *ast_node = szof->data.expr_sizeof.node;
	BL_ASSERT(ast_node);

	MirInstr *expr = ast(cnt, ast_node);
	return append_instr_sizeof(cnt, szof, expr);
}

MirInstr *
ast_expr_type_info(Context *cnt, Ast *type_info)
{
	Ast *ast_node = type_info->data.expr_type_info.node;
	BL_ASSERT(ast_node);

	MirInstr *expr = ast(cnt, ast_node);
	return append_instr_type_info(cnt, type_info, expr);
}

MirInstr *
ast_expr_alignof(Context *cnt, Ast *szof)
{
	Ast *ast_node = szof->data.expr_alignof.node;
	BL_ASSERT(ast_node);

	MirInstr *expr = ast(cnt, ast_node);
	return append_instr_alignof(cnt, szof, expr);
}

MirInstr *
ast_expr_deref(Context *cnt, Ast *deref)
{
	MirInstr *next = ast(cnt, deref->data.expr_deref.next);
	BL_ASSERT(next);
	return append_instr_load(cnt, deref, next);
}

MirInstr *
ast_expr_lit_int(Context *cnt, Ast *expr)
{
	u64 val = expr->data.expr_integer.val;

	if (expr->data.expr_integer.overflow) {
		builder_msg(
		    BUILDER_MSG_ERROR,
		    ERR_NUM_LIT_OVERFLOW,
		    expr->location,
		    BUILDER_CUR_WORD,
		    "Integer literal is too big and cannot be represented as any integer type.");
	}

	MirType * type         = NULL;
	const int desired_bits = count_bits(val);

	/* Here we choose best type for const integer literal: s32, s64 or u64. When u64 is
	 * selected, this number cannot be negative. */
	if (desired_bits < 32) {
		type = cnt->builtin_types.t_s32;
	} else if (desired_bits < 64) {
		type = cnt->builtin_types.t_s64;
	} else {
		type = cnt->builtin_types.t_u64;
	}

	return append_instr_const_int(cnt, expr, type, val);
}

MirInstr *
ast_expr_lit_float(Context *cnt, Ast *expr)
{
	if (expr->data.expr_float.overflow) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_NUM_LIT_OVERFLOW,
		            expr->location,
		            BUILDER_CUR_WORD,
		            "Float literal is too big and cannot be represented as f32.");
	}

	return append_instr_const_float(cnt, expr, expr->data.expr_float.val);
}

MirInstr *
ast_expr_lit_double(Context *cnt, Ast *expr)
{
	if (expr->data.expr_double.overflow) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_NUM_LIT_OVERFLOW,
		            expr->location,
		            BUILDER_CUR_WORD,
		            "Double literal is too big and cannot be represented as f64.");
	}

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
	Ast *              ast_callee = call->data.expr_call.ref;
	SmallArray_AstPtr *ast_args   = call->data.expr_call.args;
	BL_ASSERT(ast_callee);

	SmallArray_InstrPtr *args = create_sarr(SmallArray_InstrPtr, cnt->assembly);

	/* arguments need to be generated into reverse order due to bytecode call
	 * conventions */
	if (ast_args) {
		const size_t argc = ast_args->size;
		sa_resize_InstrPtr(args, argc);
		MirInstr *arg;
		Ast *     ast_arg;
		for (size_t i = argc; i-- > 0;) {
			ast_arg = ast_args->data[i];
			arg     = ast(cnt, ast_arg);

			args->data[i] = arg;
		}
	}

	MirInstr *callee = ast(cnt, ast_callee);

	return append_instr_call(cnt, call, callee, args);
}

MirInstr *
ast_expr_ref(Context *cnt, Ast *ref)
{
	Ast *ident = ref->data.expr_ref.ident;
	BL_ASSERT(ident);
	BL_ASSERT(ident->kind == AST_IDENT);

	Scope *scope = ident->owner_scope;
	Unit * unit  = ident->location->unit;
	BL_ASSERT(unit);
	BL_ASSERT(scope);

	return append_instr_decl_ref(cnt, ref, unit, &ident->data.ident.id, scope, NULL);
}

MirInstr *
ast_expr_elem(Context *cnt, Ast *elem)
{
	Ast *ast_arr   = elem->data.expr_elem.next;
	Ast *ast_index = elem->data.expr_elem.index;
	BL_ASSERT(ast_arr && ast_index);

	MirInstr *arr_ptr = ast(cnt, ast_arr);
	MirInstr *index   = ast(cnt, ast_index);

	return append_instr_elem_ptr(cnt, elem, arr_ptr, index, false);
}

MirInstr *
ast_expr_member(Context *cnt, Ast *member)
{
	Ast *ast_next = member->data.expr_member.next;
	// BL_ASSERT(ast_next);

	MirInstr *target = ast(cnt, ast_next);
	// BL_ASSERT(target);

	return append_instr_member_ptr(
	    cnt, member, target, member->data.expr_member.ident, NULL, MIR_BUILTIN_ID_NONE);
}

MirInstr *
ast_expr_lit_fn(Context *cnt, Ast *lit_fn, Ast *decl_node, bool is_in_gscope, u32 flags)
{
	/* creates function prototype */
	Ast *ast_block   = lit_fn->data.expr_fn.block;
	Ast *ast_fn_type = lit_fn->data.expr_fn.type;

	MirInstrFnProto *fn_proto =
	    (MirInstrFnProto *)append_instr_fn_proto(cnt, lit_fn, NULL, NULL, true);

	/* Generate type resolver for function type. */
	fn_proto->type = ast_create_impl_fn_call(
	    cnt, ast_fn_type, RESOLVE_TYPE_FN_NAME, cnt->builtin_types.t_resolve_type_fn);
	BL_ASSERT(fn_proto->type);

	MirInstrBlock *prev_block      = get_current_block(cnt);
	MirInstrBlock *prev_exit_block = cnt->ast.exit_block;

	MirFn *fn = create_fn(cnt,
	                      decl_node ? decl_node : lit_fn,
	                      decl_node ? &decl_node->data.ident.id : NULL,
	                      NULL,
	                      flags,
	                      fn_proto,
	                      true,
	                      is_in_gscope);

	mir_set_const_ptr(&fn_proto->base.value.data.v_ptr, fn, MIR_CP_FN);

	/* function body */
	/* external functions has no body */
	if (IS_FLAG(flags, FLAG_EXTERN)) {
		return &fn_proto->base;
	}

	BL_ASSERT(ast_block && "Non-external function literal without block!");

	/* Set body scope for DI. */
	fn->body_scope = ast_block->owner_scope;

	/* create block for initialization locals and arguments */
	MirInstrBlock *init_block = append_block(cnt, fn, "entry");

	/* Every user generated function must contain exit block; this block is invoked last
	 * in every function a eventually can return .ret value stored in temporary storage.
	 * When ast parser hit user defined 'return' statement it sets up .ret temporary if
	 * there is one and produce break into exit block. This approach is needed due to
	 * defer statement, because we need to call defer blocks after return value
	 * evaluation and before terminal instruction of the function. Last defer block
	 * always breaks into the exit block. */
	cnt->ast.exit_block = append_block(cnt, fn, "exit");

	if (ast_fn_type->data.type_fn.ret_type) {
		set_current_block(cnt, init_block);
		fn->ret_tmp = append_instr_decl_var_impl(
		    cnt, gen_uq_name(IMPL_RET_TMP), NULL, NULL, true, false, -1, 0);

		set_current_block(cnt, cnt->ast.exit_block);
		MirInstr *ret_init = append_instr_decl_direct_ref(cnt, fn->ret_tmp);

		append_instr_ret(cnt, NULL, ret_init, false);
	} else {
		set_current_block(cnt, cnt->ast.exit_block);
		append_instr_ret(cnt, NULL, NULL, false);
	}

	set_current_block(cnt, init_block);

	/* build MIR for fn arguments */
	SmallArray_AstPtr *ast_args = ast_fn_type->data.type_fn.args;
	if (ast_args) {
		Ast *ast_arg;
		Ast *ast_arg_name;

		const size_t argc = ast_args->size;
		for (size_t i = argc; i-- > 0;) {
			ast_arg = ast_args->data[i];
			BL_ASSERT(ast_arg->kind == AST_DECL_ARG);
			ast_arg_name = ast_arg->data.decl.name;
			BL_ASSERT(ast_arg_name);

			/* create tmp declaration for arg variable */
			MirInstr *arg = append_instr_arg(cnt, NULL, (unsigned long)i);
			append_instr_decl_var(cnt, ast_arg_name, NULL, arg, true, false, i, 0);

			register_symbol(cnt,
			                ast_arg_name,
			                &ast_arg_name->data.ident.id,
			                ast_arg_name->owner_scope,
			                false,
			                false);
		}
	}

	/* generate body instructions */
	ast(cnt, ast_block);

	if (!is_block_terminated(get_current_block(cnt)))
		append_instr_br(cnt, NULL, cnt->ast.exit_block);

	cnt->ast.exit_block = prev_exit_block;
	set_current_block(cnt, prev_block);
	return &fn_proto->base;
}

MirInstr *
ast_expr_lit_string(Context *cnt, Ast *lit_string)
{
	const char *cstr = lit_string->data.expr_string.val;
	BL_ASSERT(cstr);
	return append_instr_const_string(cnt, lit_string, cstr);
}

MirInstr *
ast_expr_binop(Context *cnt, Ast *binop)
{
	Ast *ast_lhs = binop->data.expr_binop.lhs;
	Ast *ast_rhs = binop->data.expr_binop.rhs;
	BL_ASSERT(ast_lhs && ast_rhs);

	const BinopKind op = binop->data.expr_binop.kind;

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

	case BINOP_LOGIC_AND: {
		MirFn *        fn        = get_current_fn(cnt);
		MirInstrBlock *rhs_block = append_block(cnt, fn, "rhs_block");
		MirInstrBlock *end_block = append_block(cnt, fn, "end_block");

		MirInstr *lhs = ast(cnt, ast_lhs);
		append_instr_cond_br(cnt, NULL, lhs, rhs_block, end_block);

		set_current_block(cnt, rhs_block);
		MirInstr *rhs = ast(cnt, ast_rhs);
		append_instr_br(cnt, NULL, end_block);

		set_current_block(cnt, end_block);
		MirInstr *   const_false = append_instr_const_bool(cnt, NULL, false);
		MirInstrPhi *phi         = (MirInstrPhi *)append_instr_phi(cnt, binop);
		phi_add_income(phi, const_false, lhs->owner_block);
		phi_add_income(phi, rhs, rhs_block);

		return &phi->base;
	}

	case BINOP_LOGIC_OR: {
		MirFn *        fn        = get_current_fn(cnt);
		MirInstrBlock *rhs_block = append_block(cnt, fn, "rhs_block");
		MirInstrBlock *end_block = append_block(cnt, fn, "end_block");

		MirInstr *lhs = ast(cnt, ast_lhs);
		append_instr_cond_br(cnt, NULL, lhs, end_block, rhs_block);

		set_current_block(cnt, rhs_block);
		MirInstr *rhs = ast(cnt, ast_rhs);
		append_instr_br(cnt, NULL, end_block);

		set_current_block(cnt, end_block);
		MirInstr *   const_true = append_instr_const_bool(cnt, NULL, true);
		MirInstrPhi *phi        = (MirInstrPhi *)append_instr_phi(cnt, binop);
		phi_add_income(phi, const_true, lhs->owner_block);
		phi_add_income(phi, rhs, rhs_block);

		return &phi->base;
	}

	default: {
		MirInstr *rhs = ast(cnt, ast_rhs);
		MirInstr *lhs = ast(cnt, ast_lhs);
		return append_instr_binop(cnt, binop, lhs, rhs, op);
	}
	}
}

MirInstr *
ast_expr_unary(Context *cnt, Ast *unop)
{
	Ast *ast_next = unop->data.expr_unary.next;
	BL_ASSERT(ast_next);

	MirInstr *next = ast(cnt, ast_next);
	BL_ASSERT(next);

	return append_instr_unop(cnt, unop, next, unop->data.expr_unary.kind);
}

MirInstr *
ast_expr_type(Context *cnt, Ast *type)
{
	Ast *next_type = type->data.expr_type.type;
	BL_ASSERT(next_type);
	return ast(cnt, next_type);
}

MirInstr *
ast_decl_entity(Context *cnt, Ast *entity)
{
	MirInstr * result        = NULL;
	Ast *      ast_name      = entity->data.decl.name;
	Ast *      ast_type      = entity->data.decl.type;
	Ast *      ast_value     = entity->data.decl_entity.value;
	const bool is_mutable    = entity->data.decl_entity.mut;
	const bool is_in_gscope  = entity->data.decl_entity.in_gscope;
	const bool is_compiler   = IS_FLAG(entity->data.decl_entity.flags, FLAG_COMPILER);
	bool       enable_groups = false;

	BL_ASSERT(ast_name && "Missing entity name.");
	BL_ASSERT(ast_name->kind == AST_IDENT && "Expected identificator.");

	Scope *scope = ast_name->owner_scope;
	ID *   id    = &ast_name->data.ident.id;

	if (ast_value && ast_value->kind == AST_EXPR_LIT_FN) {
		/* recognised named function declaraton */
		const s32 flags = entity->data.decl_entity.flags;
		MirInstr *value = ast_expr_lit_fn(cnt, ast_value, ast_name, is_in_gscope, flags);
		enable_groups   = true;

		if (ast_type) {
			((MirInstrFnProto *)value)->user_type =
			    ast_create_impl_fn_call(cnt,
			                            ast_type,
			                            RESOLVE_TYPE_FN_NAME,
			                            cnt->builtin_types.t_resolve_type_fn);
		}

		/* check main */
		if (is_builtin(ast_name, MIR_BUILTIN_ID_MAIN)) {
			BL_ASSERT(!cnt->entry_fn);
			cnt->entry_fn = value->value.data.v_ptr.data.fn;
			ref_instr(cnt->entry_fn->prototype); /* main must be generated into LLVM */
		}
	} else {
		/* other declaration types */
		MirInstr *type = ast_type
		                     ? ast_create_impl_fn_call(cnt,
		                                               ast_type,
		                                               RESOLVE_TYPE_FN_NAME,
		                                               cnt->builtin_types.t_resolve_type_fn)
		                     : NULL;

		cnt->ast.current_entity_id = &ast_name->data.ident.id;
		/* initialize value */
		MirInstr *value = NULL;
		if (is_in_gscope) {
			/* Initialization of global variables must be done in implicit
			 * initializer function executed in compile time. Every
			 * initialization function must be able to be executed in compile
			 * time. */
			value =
			    ast_value
			        ? ast_create_impl_fn_call(cnt, ast_value, INIT_VALUE_FN_NAME, NULL)
			        : NULL;
		} else {
			value = ast(cnt, ast_value);
		}

		append_instr_decl_var(cnt,
		                      ast_name,
		                      type,
		                      value,
		                      is_mutable,
		                      is_in_gscope,
		                      -1,
		                      entity->data.decl_entity.flags);

		cnt->ast.current_entity_id = NULL;

		if (is_builtin(ast_name, MIR_BUILTIN_ID_MAIN)) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_EXPECTED_FUNC,
			            ast_name->location,
			            BUILDER_CUR_WORD,
			            "Main is expected to be a function.");
		}
	}

	register_symbol(cnt, ast_name, id, scope, is_compiler, enable_groups);
	return result;
}

MirInstr *
ast_decl_arg(Context *cnt, Ast *arg)
{
	Ast *ast_name = arg->data.decl.name;
	Ast *ast_type = arg->data.decl.type;

	BL_ASSERT(ast_type);
	MirInstr *type = ast(cnt, ast_type);

	return append_instr_decl_arg(cnt, ast_name, type);
}

MirInstr *
ast_decl_member(Context *cnt, Ast *arg)
{
	Ast *ast_type = arg->data.decl.type;
	Ast *ast_name = arg->data.decl.name;

	BL_ASSERT(ast_type);
	MirInstr *result = ast(cnt, ast_type);

	/* named member? */
	if (ast_name) {
		BL_ASSERT(ast_name->kind == AST_IDENT);
		result = append_instr_decl_member(cnt, ast_name, result);

		register_symbol(
		    cnt, ast_name, &ast_name->data.ident.id, ast_name->owner_scope, false, false);
	}

	BL_ASSERT(result);
	return result;
}

MirInstr *
ast_decl_variant(Context *cnt, Ast *variant)
{
	Ast *ast_name  = variant->data.decl.name;
	Ast *ast_value = variant->data.decl_variant.value;
	BL_ASSERT(ast_name && "Missing enum variant name!");

	MirInstr *value = ast(cnt, ast_value);

	register_symbol(
	    cnt, ast_name, &ast_name->data.ident.id, ast_name->owner_scope, false, false);

	return append_instr_decl_variant(cnt, ast_name, value);
}

MirInstr *
ast_type_ref(Context *cnt, Ast *type_ref)
{
	Ast *ident = type_ref->data.type_ref.ident;
	BL_ASSERT(ident);

	Scope *scope = ident->owner_scope;
	Unit * unit  = ident->location->unit;
	BL_ASSERT(unit);
	BL_ASSERT(scope);

	MirInstr *ref =
	    append_instr_decl_ref(cnt, type_ref, unit, &ident->data.ident.id, scope, NULL);
	return ref;
}

MirInstr *
ast_type_fn(Context *cnt, Ast *type_fn)
{
	Ast *              ast_ret_type  = type_fn->data.type_fn.ret_type;
	SmallArray_AstPtr *ast_arg_types = type_fn->data.type_fn.args;

	/* return type */
	MirInstr *ret_type = NULL;
	if (ast_ret_type) {
		ret_type = ast(cnt, ast_ret_type);
		ref_instr(ret_type);
	}

	SmallArray_InstrPtr *args = NULL;
	if (ast_arg_types && ast_arg_types->size) {
		const size_t c = ast_arg_types->size;
		args           = create_sarr(SmallArray_InstrPtr, cnt->assembly);
		sa_resize_InstrPtr(args, c);

		Ast *     ast_arg_type;
		MirInstr *arg;
		for (size_t i = c; i-- > 0;) {
			ast_arg_type = ast_arg_types->data[i];
			arg          = ast(cnt, ast_arg_type);
			ref_instr(arg);
			args->data[i] = arg;
		}
	}

	return append_instr_type_fn(cnt, type_fn, ret_type, args);
}

MirInstr *
ast_type_arr(Context *cnt, Ast *type_arr)
{
	Ast *ast_elem_type = type_arr->data.type_arr.elem_type;
	Ast *ast_len       = type_arr->data.type_arr.len;
	BL_ASSERT(ast_elem_type && ast_len);

	MirInstr *len       = ast(cnt, ast_len);
	MirInstr *elem_type = ast(cnt, ast_elem_type);
	return append_instr_type_array(cnt, type_arr, elem_type, len);
}

MirInstr *
ast_type_slice(Context *cnt, Ast *type_slice)
{
	Ast *ast_elem_type = type_slice->data.type_arr.elem_type;
	BL_ASSERT(ast_elem_type);

	MirInstr *elem_type = ast(cnt, ast_elem_type);
	return append_instr_type_slice(cnt, type_slice, elem_type);
}

MirInstr *
ast_type_ptr(Context *cnt, Ast *type_ptr)
{
	Ast *ast_type = type_ptr->data.type_ptr.type;
	BL_ASSERT(ast_type && "invalid pointee type");
	MirInstr *type = ast(cnt, ast_type);
	BL_ASSERT(type);
	return append_instr_type_ptr(cnt, type_ptr, type);
}

MirInstr *
ast_type_vargs(Context *cnt, Ast *type_vargs)
{
	/* type is optional (Any will be used when no type was specified) */
	Ast *     ast_type = type_vargs->data.type_vargs.type;
	MirInstr *type     = ast(cnt, ast_type);
	return append_instr_type_vargs(cnt, type_vargs, type);
}

MirInstr *
ast_type_enum(Context *cnt, Ast *type_enum)
{
	SmallArray_AstPtr *ast_variants  = type_enum->data.type_enm.variants;
	Ast *              ast_base_type = type_enum->data.type_enm.type;
	BL_ASSERT(ast_variants);

	const size_t varc = ast_variants->size;
	if (varc == 0) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_EMPTY_ENUM,
		            type_enum->location,
		            BUILDER_CUR_WORD,
		            "Empty enumerator.");
		return NULL;
	}

	MirInstr *base_type = ast(cnt, ast_base_type);

	Scope *scope = type_enum->data.type_enm.scope;
	BL_ASSERT(scope);
	if (cnt->debug_mode) init_llvm_DI_scope(cnt, scope);

	SmallArray_InstrPtr *variants = create_sarr(SmallArray_InstrPtr, cnt->assembly);

	/* Build variant instructions */
	MirInstr *variant;
	Ast *     ast_variant;
	SARRAY_FOREACH(ast_variants, ast_variant)
	{
		variant = ast(cnt, ast_variant);
		BL_ASSERT(variant);
		sa_push_InstrPtr(variants, variant);
	}

	/* Consume declaration identificator. */
	ID *id                     = cnt->ast.current_entity_id;
	cnt->ast.current_entity_id = NULL;

	return append_instr_type_enum(cnt, type_enum, id, scope, variants, base_type);
}

MirInstr *
ast_type_struct(Context *cnt, Ast *type_struct)
{
	SmallArray_AstPtr *ast_members = type_struct->data.type_strct.members;
	const bool         is_raw      = type_struct->data.type_strct.raw;
	if (is_raw) {
		BL_ABORT_ISSUE(31);
	}

	BL_ASSERT(ast_members);

	const size_t memc = ast_members->size;
	if (memc == 0) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_EMPTY_STRUCT,
		            type_struct->location,
		            BUILDER_CUR_WORD,
		            "Empty structure.");
		return NULL;
	}

	SmallArray_InstrPtr *members = create_sarr(SmallArray_InstrPtr, cnt->assembly);

	MirInstr *tmp = NULL;
	Ast *     ast_member;
	SARRAY_FOREACH(ast_members, ast_member)
	{
		tmp = ast(cnt, ast_member);
		BL_ASSERT(tmp);
		sa_push_InstrPtr(members, tmp);
	}

	Scope *scope = type_struct->data.type_strct.scope;
	BL_ASSERT(scope);

	if (cnt->debug_mode) init_llvm_DI_scope(cnt, scope);

	/* Consume declaration identificator. */
	ID *id                     = cnt->ast.current_entity_id;
	cnt->ast.current_entity_id = NULL;

	return append_instr_type_struct(cnt, type_struct, id, scope, members, false);
}

MirInstr *
ast_create_impl_fn_call(Context *cnt, Ast *node, const char *fn_name, MirType *fn_type)
{
	if (!node) return NULL;

	/* Sometimes we need to have implicit function return type based on
	 * resulting type of the AST expression, in such case we must allow return
	 * instruction to change function return
	 * type and create dummy type for the function. */
	MirType *final_fn_type  = fn_type;
	bool     infer_ret_type = false;
	if (!final_fn_type) {
		final_fn_type  = create_type_fn(cnt, NULL, NULL, NULL, false);
		infer_ret_type = true;
	}

	MirInstrBlock *prev_block = get_current_block(cnt);
	MirInstr *     fn_proto   = append_instr_fn_proto(cnt, NULL, NULL, NULL, false);
	fn_proto->value.type      = final_fn_type;

	MirFn *fn =
	    create_fn(cnt, NULL, NULL, fn_name, 0, (MirInstrFnProto *)fn_proto, false, true);
	mir_set_const_ptr(&fn_proto->value.data.v_ptr, fn, MIR_CP_FN);

	fn->type = final_fn_type;

	MirInstrBlock *entry = append_block(cnt, fn, "entry");
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
	case AST_STMT_DEFER:
		ast_stmt_defer(cnt, node);
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
	case AST_DECL_VARIANT:
		return ast_decl_variant(cnt, node);
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
	case AST_TYPE_ENUM:
		return ast_type_enum(cnt, node);
	case AST_EXPR_FILE:
		return ast_expr_file(cnt, node);
	case AST_EXPR_LINE:
		return ast_expr_line(cnt, node);
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
		return ast_expr_lit_fn(cnt, node, NULL, false, 0);
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
	case AST_LINK:
	case AST_PRIVATE:
		break;
	default:
		BL_ABORT("invalid node %s", ast_get_name(node));
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
	case MIR_INSTR_DECL_ARG:
		return "InstrDeclArg";
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
	case MIR_INSTR_DECL_DIRECT_REF:
		return "InstrDeclDirectRef";
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
	case MIR_INSTR_COMPOUND:
		return "InstrCompound";
	case MIR_INSTR_VARGS:
		return "InstrVArgs";
	case MIR_INSTR_TYPE_INFO:
		return "InstrTypeInfo";
	case MIR_INSTR_PHI:
		return "InstrPhi";
	case MIR_INSTR_TYPE_ENUM:
		return "InstrTypeEnum";
	case MIR_INSTR_DECL_VARIANT:
		return "InstrDeclVariant";
	case MIR_INSTR_TOANY:
		return "InstrToAny";
	}

	return "UNKNOWN";
}

/* public */
static void
_type_to_str(char *buf, s32 len, MirType *type, bool prefer_name)
{
#define append_buf(buf, len, str)                                                                  \
	{                                                                                          \
		const size_t filled = strlen(buf);                                                 \
		snprintf((buf) + filled, (len)-filled, "%s", str);                                 \
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

	case MIR_TYPE_SLICE: {
		const bool has_members = (bool)type->data.strct.members;
		append_buf(buf, len, "[]");

		if (has_members) {
			MirType *tmp = mir_get_struct_elem_type(type, MIR_SLICE_PTR_INDEX);
			tmp          = mir_deref_type(tmp);
			_type_to_str(buf, len, tmp, true);
		}
		break;
	}

	case MIR_TYPE_VARGS: {
		const bool has_members = (bool)type->data.strct.members;
		append_buf(buf, len, "...");

		if (has_members) {
			MirType *tmp = mir_get_struct_elem_type(type, MIR_SLICE_PTR_INDEX);
			tmp          = mir_deref_type(tmp);
			_type_to_str(buf, len, tmp, true);
		}
		break;
	}

	case MIR_TYPE_STRUCT: {
		SmallArray_MemberPtr *members = type->data.strct.members;
		MirMember *           tmp;

		append_buf(buf, len, "struct{");
		if (members) {
			SARRAY_FOREACH(members, tmp)
			{
				_type_to_str(buf, len, tmp->type, true);
				if (i < members->size - 1) append_buf(buf, len, ", ");
			}
		}
		append_buf(buf, len, "}");

		break;
	}

	case MIR_TYPE_ENUM: {
		SmallArray_VariantPtr *variants = type->data.enm.variants;
		append_buf(buf, len, "enum{");

		if (variants) {
			MirVariant *variant;
			SARRAY_FOREACH(variants, variant)
			{
				append_buf(buf, len, variant->id->str);
				append_buf(buf, len, " :: ");

				if (variant->value) {
					char value_str[35];
					snprintf(value_str,
					         ARRAY_SIZE(value_str),
					         "%lld",
					         (long long)variant->value->data.v_s64);
					append_buf(buf, len, value_str);
				} else {
					append_buf(buf, len, "<invalid>");
				}

				if (i < variants->size - 1) append_buf(buf, len, ", ");
			}
		}
		append_buf(buf, len, "}");
		break;
	}

	case MIR_TYPE_FN: {
		append_buf(buf, len, "fn(");

		MirArg *           it;
		SmallArray_ArgPtr *args = type->data.fn.args;
		if (args) {
			SARRAY_FOREACH(args, it)
			{
				_type_to_str(buf, len, it->type, true);
				if (i < args->size - 1) append_buf(buf, len, ", ");
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
		sprintf(str, "[%llu]", (unsigned long long)type->data.array.len);
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
#undef append_buf
}

void
mir_type_to_str(char *buf, s32 len, MirType *type, bool prefer_name)
{
	if (!buf || !len) return;
	buf[0] = '\0';
	_type_to_str(buf, len, type, prefer_name);
}

void
execute_entry_fn(Context *cnt)
{
	msg_log("\nExecuting 'main' in compile time...");
	if (!cnt->entry_fn) {
		msg_error("Assembly '%s' has no entry function!", cnt->assembly->name);
		return;
	}

	MirType *fn_type = cnt->entry_fn->type;
	BL_ASSERT(fn_type && fn_type->kind == MIR_TYPE_FN);

	/* TODO: support passing of arguments. */
	if (fn_type->data.fn.args) {
		msg_error("Main function expects arguments, this is not supported yet!");
		return;
	}

	/* tmp return value storage */
	VMStackPtr ret_ptr = NULL;
	if (vm_execute_fn(&cnt->vm, cnt->entry_fn, &ret_ptr)) {
		if (ret_ptr) {
			MirConstValue tmp = {.type = fn_type->data.fn.ret_type};
			vm_read_stack_value(&tmp, ret_ptr);
			msg_log("Execution finished with state: %lld\n", (long long)tmp.data.v_s64);
		} else {
			msg_log("Execution finished without errors");
		}
	} else {
		msg_log("Execution finished with errors");
	}
}

void
execute_test_cases(Context *cnt)
{
	msg_log("\nExecuting test cases...");

	const size_t c      = bo_array_size(cnt->test_cases);
	s32          failed = 0;
	MirFn *      test_fn;
	s32          line;
	const char * file;

	BARRAY_FOREACH(cnt->test_cases, test_fn)
	{
		BL_ASSERT(IS_FLAG(test_fn->flags, FLAG_TEST));
		const bool passed = vm_execute_fn(&cnt->vm, test_fn, NULL);

		line = test_fn->decl_node ? test_fn->decl_node->location->line : -1;
		file = test_fn->decl_node ? test_fn->decl_node->location->unit->filepath : "?";

		msg_log("[ %s ] (%llu/%llu) %s:%d '%s'",
		        passed ? GREEN("PASSED") : RED("FAILED"),
		        (unsigned long long)i + 1,
		        (unsigned long long)c,
		        file,
		        line,
		        test_fn->test_case_desc);

		if (!passed) ++failed;
	}

	{
		s32 perc = c > 0 ? (s32)((f32)(c - failed) / (c * 0.01f)) : 100;

		msg_log("------------------------------------------------------------------"
		        "--------"
		        "------");
		if (perc == 100) {
			msg_log("Testing done, %d of %zu failed. Completed: " GREEN("%d%%"),
			        failed,
			        c,
			        perc);
		} else {
			msg_log("Testing done, %d of %zu failed. Completed: " RED("%d%%"),
			        failed,
			        c,
			        perc);
		}
		msg_log("------------------------------------------------------------------"
		        "--------"
		        "------");
	}
}

void
init_builtins(Context *cnt)
{
	{
		// initialize all hashes once
		for (s32 i = 0; i < _MIR_BUILTIN_ID_COUNT; ++i) {
			builtin_ids[i].hash = bo_hash_from_str(builtin_ids[i].str);
		}
	}

	{ // TYPES
		struct BuiltinTypes *bt = &cnt->builtin_types;
		bt->t_type              = create_type_type(cnt);
		bt->t_void              = create_type_void(cnt);

		bt->t_s8  = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_S8], 8, true);
		bt->t_s16 = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_S16], 16, true);
		bt->t_s32 = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_S32], 32, true);
		bt->t_s64 = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_S64], 64, true);
		bt->t_u8  = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_U8], 8, false);
		bt->t_u16 = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_U16], 16, false);
		bt->t_u32 = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_U32], 32, false);
		bt->t_u64 = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_U64], 64, false);
		bt->t_usize =
		    create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_USIZE], 64, false);
		bt->t_bool   = create_type_bool(cnt);
		bt->t_f32    = create_type_real(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_F32], 32);
		bt->t_f64    = create_type_real(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_F64], 64);
		bt->t_u8_ptr = create_type_ptr(cnt, bt->t_u8);
		bt->t_string = create_type_struct_special(
		    cnt, MIR_TYPE_STRING, &builtin_ids[MIR_BUILTIN_ID_TYPE_STRING], bt->t_u8_ptr);

		bt->t_string_ptr = create_type_ptr(cnt, bt->t_string);

		bt->t_string_slice =
		    create_type_struct_special(cnt, MIR_TYPE_SLICE, NULL, bt->t_string_ptr);

		bt->t_resolve_type_fn = create_type_fn(cnt, NULL, bt->t_type, NULL, false);
		bt->t_test_case_fn    = create_type_fn(cnt, NULL, bt->t_void, NULL, false);

		/* Provide types into global scope */
		provide_builtin_type(cnt, bt->t_type);
		provide_builtin_type(cnt, bt->t_s8);
		provide_builtin_type(cnt, bt->t_s16);
		provide_builtin_type(cnt, bt->t_s32);
		provide_builtin_type(cnt, bt->t_s64);
		provide_builtin_type(cnt, bt->t_u8);
		provide_builtin_type(cnt, bt->t_u16);
		provide_builtin_type(cnt, bt->t_u32);
		provide_builtin_type(cnt, bt->t_u64);
		provide_builtin_type(cnt, bt->t_usize);
		provide_builtin_type(cnt, bt->t_bool);
		provide_builtin_type(cnt, bt->t_f32);
		provide_builtin_type(cnt, bt->t_f64);
		provide_builtin_type(cnt, bt->t_string);
	}
}

ptrdiff_t
mir_get_struct_elem_offest(Assembly *assembly, MirType *type, u32 i)
{
	BL_ASSERT(mir_is_composit_type(type) && "Expected structure type");
	return LLVMOffsetOfElement(assembly->llvm.TD, type->llvm_type, (unsigned long)i);
}

ptrdiff_t
mir_get_array_elem_offset(MirType *type, u32 i)
{
	BL_ASSERT(type->kind == MIR_TYPE_ARRAY && "Expected array type");
	MirType *elem_type = type->data.array.elem_type;
	BL_ASSERT(elem_type);
	return elem_type->store_size_bytes * i;
}

void
mir_arenas_init(MirArenas *arenas)
{
	arena_init(&arenas->instr, sizeof(union _MirInstr), ARENA_CHUNK_COUNT, NULL);
	arena_init(&arenas->type, sizeof(MirType), ARENA_CHUNK_COUNT, NULL);
	arena_init(&arenas->var, sizeof(MirVar), ARENA_CHUNK_COUNT, NULL);
	arena_init(&arenas->fn, sizeof(MirFn), ARENA_CHUNK_COUNT, (ArenaElemDtor)&fn_dtor);
	arena_init(&arenas->member, sizeof(MirMember), ARENA_CHUNK_COUNT, NULL);
	arena_init(&arenas->variant, sizeof(MirVariant), ARENA_CHUNK_COUNT, NULL);
	arena_init(&arenas->value, sizeof(MirConstValue), ARENA_CHUNK_COUNT / 2, NULL);
	arena_init(&arenas->arg, sizeof(MirArg), ARENA_CHUNK_COUNT / 2, NULL);
}

void
mir_arenas_terminate(MirArenas *arenas)
{
	arena_terminate(&arenas->fn);
	arena_terminate(&arenas->instr);
	arena_terminate(&arenas->member);
	arena_terminate(&arenas->type);
	arena_terminate(&arenas->value);
	arena_terminate(&arenas->var);
	arena_terminate(&arenas->variant);
	arena_terminate(&arenas->arg);
}

void
mir_run(Assembly *assembly)
{
	Context cnt;
	memset(&cnt, 0, sizeof(Context));
	cnt.assembly                 = assembly;
	cnt.debug_mode               = builder.options.debug_build;
	cnt.analyze.verbose_pre      = false;
	cnt.analyze.verbose_post     = false;
	cnt.analyze.queue            = bo_list_new(sizeof(MirInstr *));
	cnt.analyze.RTTI_entry_types = bo_htbl_new(0, 1024);
	cnt.analyze.waiting          = bo_htbl_new_bo(bo_typeof(BArray), true, ANALYZE_TABLE_SIZE);
	cnt.analyze.llvm_di_builder  = assembly->llvm.di_builder;
	cnt.test_cases               = bo_array_new(sizeof(MirFn *));
	cnt.tmp_sh                   = bo_string_new(1024);
	cnt.type_table               = assembly->type_table;
	cnt.builtin_types.cache =
	    scope_create(&assembly->arenas.scope, SCOPE_GLOBAL, NULL, 64, NULL);

	vm_init(&cnt.vm, assembly, VM_STACK_SIZE);

	sa_init(&cnt.ast.defer_stack);

	/* initialize all builtin types */
	init_builtins(&cnt);

	/* Gen MIR from AST pass */
	Unit *unit;
	BARRAY_FOREACH(assembly->units, unit) ast(&cnt, unit->ast);

	if (builder.errorc) goto SKIP;

	/* Analyze pass */
	analyze(&cnt);
	analyze_report_unresolved(&cnt);

	if (builder.errorc) goto SKIP;

	gen_RTTI_types(&cnt);

	if (builder.options.run_tests) execute_test_cases(&cnt);
	if (builder.options.run) execute_entry_fn(&cnt);

SKIP:
	bo_unref(cnt.analyze.queue);
	bo_unref(cnt.analyze.waiting);
	bo_unref(cnt.analyze.RTTI_entry_types);
	bo_unref(cnt.test_cases);
	bo_unref(cnt.tmp_sh);

	sa_terminate(&cnt.ast.defer_stack);

	vm_terminate(&cnt.vm);
}
