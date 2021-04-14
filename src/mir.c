// =================================================================================================
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
// =================================================================================================

#include "mir.h"
#include "ast.h"
#include "builder.h"
#include "common.h"
#include "mir_printer.h"
#include "unit.h"

#ifdef _MSC_VER
#pragma warning(disable : 6001)
#endif

#define ARENA_CHUNK_COUNT 512
#define ARENA_INSTR_CHUNK_COUNT 2048
#define ANALYZE_TABLE_SIZE 8192
#define RESOLVE_TYPE_FN_NAME ".type"
#define INIT_VALUE_FN_NAME ".init"
#define IMPL_FN_NAME ".impl"
#define IMPL_VARGS_TMP_ARR ".vargs.arr"
#define IMPL_VARGS_TMP ".vargs"
#define IMPL_ANY_TMP ".any"
#define IMPL_ANY_EXPR_TMP ".any.expr"
#define IMPL_COMPOUND_TMP ".compound"
#define IMPL_RTTI_ENTRY ".rtti"
#define IMPL_TESTCASES_TMP ".testcases"
#define IMPL_ARG_DEFAULT ".arg.default"
#define IMPL_CALL_LOC ".call.loc"
#define IMPL_RET_TMP ".ret"
#define IMPL_UNROLL_TMP ".unroll"
#define NO_REF_COUNTING -1
#define VERBOSE_ANALYZE false

#define ANALYZE_INSTR_RQ(i)                                                                        \
    {                                                                                              \
        const AnalyzeResult r = analyze_instr(cnt, (i));                                           \
        BL_ASSERT(r.state == ANALYZE_PASSED);                                                      \
        (void)r;                                                                                   \
    }

#define ANALYZE_RESULT(_state, _waiting_for)                                                       \
    (AnalyzeResult)                                                                                \
    {                                                                                              \
        .state = ANALYZE_##_state, .waiting_for = (_waiting_for)                                   \
    }

#define CREATE_TYPE_RESOLVER_CALL(_ast)                                                            \
    ast_create_impl_fn_call(                                                                       \
        cnt, (_ast), RESOLVE_TYPE_FN_NAME, cnt->builtin_types->t_resolve_type_fn, false)

#define TEXT_LINE "--------------------------------------------------------------------------------"

#define GEN_INSTR_SIZEOF
#include "mir.inc"
#undef GEN_INSTR_SIZEOF

#define CREATE_TYPE_STRUCT_SLICE(cnt, id, elem_ptr_type)                                           \
    _create_type_struct_slice(cnt, MIR_TYPE_SLICE, id, elem_ptr_type)

#define CREATE_TYPE_STRUCT_VARGS(cnt, id, elem_ptr_type)                                           \
    _create_type_struct_slice(cnt, MIR_TYPE_VARGS, id, elem_ptr_type)

#define CREATE_TYPE_STRUCT_STRING(cnt, id, elem_ptr_type)                                          \
    _create_type_struct_slice(cnt, MIR_TYPE_STRING, id, elem_ptr_type)

// Sets is_naked flag to 'v' if '_instr' is valid compound expression.
#define SET_IS_NAKED_IF_COMPOUND(_instr, v)                                                        \
    if ((_instr) && (_instr)->kind == MIR_INSTR_COMPOUND) {                                        \
        ((MirInstrCompound *)(_instr))->is_naked = v;                                              \
    }

typedef struct {
    MirVar * var;
    MirType *type;
} RTTIIncomplete;

TSMALL_ARRAY_TYPE(LLVMType, LLVMTypeRef, 8);
TSMALL_ARRAY_TYPE(LLVMMetadata, LLVMMetadataRef, 16);
TSMALL_ARRAY_TYPE(DeferStack, Ast *, 64);
TSMALL_ARRAY_TYPE(InstrPtr64, MirInstr *, 64);
TSMALL_ARRAY_TYPE(String, char *, 64);
TSMALL_ARRAY_TYPE(RTTIIncomplete, RTTIIncomplete, 64);

typedef struct {
    TSmallArray_DeferStack defer_stack;
    MirInstrBlock *        exit_block;
} AstFnContext;

// Instance in run method is zero initialized, no need to set default values explicitly.
typedef struct {
    VM *      vm;
    Assembly *assembly;
    TString   tmp_sh;
    bool      debug_mode;

    // AST -> MIR generation
    struct {
        TArray _fnctx_stack;

        MirInstrBlock *current_block;
        MirInstrBlock *current_phi_end_block;
        MirInstrPhi *  current_phi;
        MirInstrBlock *break_block;
        MirInstrBlock *continue_block;
        AstFnContext * fnctx;
        ID *           current_entity_id;
        MirInstr *     current_fwd_struct_decl;
    } ast;

    // Analyze MIR generated from AST
    struct {
        // Instructions waiting for analyze.
        TList queue;

        // Hash table of arrays. Hash is ID of symbol and array contains queue
        // of waiting instructions (DeclRefs).
        THashTable waiting;

        // Structure members can sometimes point to self, in such case we end up with
        // endless looping RTTI generation, to solve this problem we create dummy RTTI
        // variable for all pointer types and store them in this array. When structure RTTI
        // is complete we can fill missing pointer RTTIs in second generation pass.
        TSmallArray_RTTIIncomplete incomplete_rtti;

        // Incomplete type check stack.
        TSmallArray_TypePtr complete_check_type_stack;
        THashTable          complete_check_visited;

        // Switch cases duplicity check
        THashTable  presented_switch_cases;
        TArray      usage_check_queue;
        ScopeEntry *void_entry;
    } analyze;

    struct {
        // Same as assembly->testing.cases.
        TArray *cases;

        // Expected unit test count is evaluated before analyze pass. We need this
        // information before we analyze all test functions because metadata runtime
        // variable must be preallocated (testcases builtin operator cannot wait for all
        // test case functions to be analyzed). This count must match cases->size.
        s32 expected_test_count;
    } testing;

    // Builtins
    struct BuiltinTypes *builtin_types;
} Context;

typedef enum {
    // Analyze pass failed.
    ANALYZE_FAILED = 0,

    // Analyze pass passed.
    ANALYZE_PASSED = 1,

    // Analyze pass cannot be done because some of sub-parts has not been
    // analyzed yet and probably needs to be executed during analyze pass. In
    // such case we push analyzed instruction at the end of analyze queue.
    ANALYZE_POSTPONE = 2,

    // In this case AnalyzeResult will contain hash of desired symbol which be satisfied later,
    // instruction is pushed into waiting table.
    ANALYZE_WAITING = 3,
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

// Argument list used in slot analyze functions
// clang-format off
#define ANALYZE_STAGE_ARGS                                                                            \
    	Context   UNUSED(*cnt),          /* Stage context. */                                             \
	    MirInstr  UNUSED(**input),       /* Slot input, this represents slot itself and can be changed. */\
	    MirType   UNUSED(*slot_type),    /* Optional expected result type. */                             \
	    bool      UNUSED(is_initializer) /* True when slot is variable initializer. */
// clang-format on

#define ANALYZE_STAGE_FN(N) AnalyzeStageState analyze_stage_##N(ANALYZE_STAGE_ARGS)
typedef AnalyzeStageState (*AnalyzeStageFn)(ANALYZE_STAGE_ARGS);

typedef struct {
    s32            count;
    AnalyzeStageFn stages[];
} AnalyzeSlotConfig;

// Ids of builtin symbols, hash is calculated inside builtin_inits function later.
static ID builtin_ids[_MIR_BUILTIN_ID_COUNT] = {
#define GEN_BUILTIN_IDS
#include "mir.inc"
#undef GEN_BUILTIN_IDS
};

// Arena destructor for functions.
static void fn_dtor(MirFn **_fn)
{
    MirFn *fn = *_fn;
    if (fn->dyncall.extern_callback_handle) dcbFreeCallback(fn->dyncall.extern_callback_handle);
}

// FW decls
// Initialize all builtin types.
static void    builtin_inits(Context *cnt);
static void    testing_add_test_case(Context *cnt, MirFn *fn);
static MirVar *testing_gen_meta(Context *cnt);

// Execute all registered test cases in current assembly.
static const char *get_intrinsic(const char *name);
static MirFn *     group_select_overload(Context *                  cnt,
                                         const MirFnGroup *         group,
                                         const TSmallArray_TypePtr *expected_args);

// Register incomplete scope entry for symbol.
static ScopeEntry *register_symbol(Context *cnt, Ast *node, ID *id, Scope *scope, bool is_builtin);

// Lookup builtin by builtin kind in global scope. Return NULL even if builtin is valid symbol in
// case when it's not been analyzed yet or is incomplete struct type. In such case caller must
// postpone analyze process. This is an error in any post-analyze processing (every type must be
// complete when analyze pass id completed!).
static MirType *lookup_builtin_type(Context *cnt, MirBuiltinIdKind kind);
static MirFn *  lookup_builtin_fn(Context *cnt, MirBuiltinIdKind kind);

// @HACK: Better way to do this will be enable compiler to have default preload file; we need to
// make lexing, parsing, MIR generation and analyze of this file first and then process rest of the
// source base. Then it will be guaranteed that all desired builtins are ready to use.

// Try to complete cached RTTI related types, return NULL if all types are resolved or return ID for
// first missing type.
static ID *lookup_builtins_rtti(Context *cnt);
static ID *lookup_builtins_any(Context *cnt);
static ID *lookup_builtins_test_cases(Context *cnt);
static ID *lookup_builtins_code_loc(Context *cnt);

// Lookup member in composit structure type. Searching also in base types. When 'out_base_type' is
// set to base member type if entry was found in parent.
static ScopeEntry *lookup_composit_member(MirType *type, ID *rid, MirType **out_base_type);

// Provide global bool variable into the assembly.
static MirVar *add_global_bool(Context *cnt, ID *id, bool is_mutable, bool v);

// Initialize type ID. This function creates and set ID string and calculates integer hash from this
// string. The type.id.str could be also used as name for unnamed types.
static void type_init_id(Context *cnt, MirType *type);

// Create new type. The 'user_id' is optional.
static MirType *create_type(Context *cnt, MirTypeKind kind, ID *user_id);
static MirType *create_type_type(Context *cnt);
static MirType *create_type_named_scope(Context *cnt);
static MirType *create_type_null(Context *cnt, MirType *base_type);
static MirType *create_type_void(Context *cnt);
static MirType *create_type_bool(Context *cnt);
static MirType *create_type_int(Context *cnt, ID *id, s32 bitcount, bool is_signed);
static MirType *create_type_real(Context *cnt, ID *id, s32 bitcount);
static MirType *create_type_ptr(Context *cnt, MirType *src_type);
static MirType *create_type_fn(Context *           cnt,
                               ID *                id,
                               MirType *           ret_type,
                               TSmallArray_ArgPtr *args,
                               bool                is_vargs,
                               bool                has_default_args);
static MirType *create_type_fn_group(Context *cnt, ID *id, TSmallArray_TypePtr *variants);
static MirType *create_type_array(Context *cnt, ID *id, MirType *elem_type, s64 len);
static MirType *create_type_struct(Context *              cnt,
                                   MirTypeKind            kind,
                                   ID *                   id,
                                   Scope *                scope,
                                   TSmallArray_MemberPtr *members,   // MirMember
                                   MirType *              base_type, // optional
                                   bool                   is_union,
                                   bool                   is_packed,
                                   bool                   is_multiple_return_type);

// Make incomplete type struct declaration complete. This function sets all desired information
// about struct to the forward declaration type.
static MirType *complete_type_struct(Context *              cnt,
                                     MirInstr *             fwd_decl,
                                     Scope *                scope,
                                     TSmallArray_MemberPtr *members,
                                     MirType *              base_type, // optional
                                     bool                   is_packed,
                                     bool                   is_union,
                                     bool                   is_multiple_return_type);

// Create incomplete struct type placeholder to be filled later.
static MirType *create_type_struct_incomplete(Context *cnt, ID *user_id, bool is_union);
static MirType *create_type_enum(Context *               cnt,
                                 ID *                    id,
                                 Scope *                 scope,
                                 MirType *               base_type,
                                 TSmallArray_VariantPtr *variants);

MirType *_create_type_struct_slice(Context *cnt, MirTypeKind kind, ID *id, MirType *elem_ptr_type);
MirType *create_type_struct_dyarr(Context *cnt, ID *id, MirType *elem_ptr_type);
static void type_init_llvm_int(Context *cnt, MirType *type);
static void type_init_llvm_real(Context *cnt, MirType *type);
static void type_init_llvm_ptr(Context *cnt, MirType *type);
static void type_init_llvm_null(Context *cnt, MirType *type);
static void type_init_llvm_void(Context *cnt, MirType *type);
static void type_init_llvm_bool(Context *cnt, MirType *type);
static void type_init_llvm_fn(Context *cnt, MirType *type);
static void type_init_llvm_array(Context *cnt, MirType *type);
static void type_init_llvm_struct(Context *cnt, MirType *type);
static void type_init_llvm_enum(Context *cnt, MirType *type);

static MirVar *create_var(Context *        cnt,
                          Ast *            decl_node,
                          Scope *          scope,
                          ID *             id,
                          MirType *        alloc_type,
                          bool             is_mutable,
                          bool             is_global,
                          bool             is_comptime,
                          u32              flags,
                          MirBuiltinIdKind builtin_id);

static MirVar *create_var_impl(Context *   cnt,
                               const char *name,
                               MirType *   alloc_type,
                               bool        is_mutable,
                               bool        is_global,
                               bool        is_comptime);

static MirFn *create_fn(Context *        cnt,
                        Ast *            node,
                        ID *             id,
                        const char *     linkage_name,
                        u32              flags,
                        MirInstrFnProto *prototype,
                        bool             emit_llvm,
                        bool             is_global,
                        MirBuiltinIdKind builtin_id);

static MirFnGroup *create_fn_group(Context *cnt, Ast *decl_node, TSmallArray_FnPtr *variants);
static MirMember * create_member(Context *cnt, Ast *node, ID *id, s64 index, MirType *type);
static MirArg *
create_arg(Context *cnt, Ast *node, ID *id, Scope *scope, MirType *type, MirInstr *value);

static MirVariant *create_variant(Context *cnt, ID *id, MirConstExprValue *value);
// Create block without owner function.
static MirInstrBlock *create_block(Context *cnt, const char *name);
// Create and append block into function specified.
static MirInstrBlock *append_block(Context *cnt, MirFn *fn, const char *name);
// Append already created block into function. Block cannot be already member of other function.
static MirInstrBlock *append_block2(Context *cnt, MirFn *fn, MirInstrBlock *block);
static MirInstrBlock *append_global_block(Context *cnt, const char *name);

// instructions
static void *    create_instr(Context *cnt, MirInstrKind kind, Ast *node);
static MirInstr *create_instr_const_type(Context *cnt, Ast *node, MirType *type);
static MirInstr *create_instr_const_int(Context *cnt, Ast *node, MirType *type, u64 val);
static MirInstr *create_instr_const_ptr(Context *cnt, Ast *node, MirType *type, VMStackPtr ptr);
static MirInstr *create_instr_const_float(Context *cnt, Ast *node, float val);
static MirInstr *create_instr_const_double(Context *cnt, Ast *node, double val);
static MirInstr *create_instr_const_bool(Context *cnt, Ast *node, bool val);
static MirInstr *create_instr_addrof(Context *cnt, Ast *node, MirInstr *src);
static MirInstr *create_instr_vargs_impl(Context *cnt, MirType *type, TSmallArray_InstrPtr *values);
static MirInstr *create_instr_decl_direct_ref(Context *cnt, MirInstr *ref);
static MirInstr *create_instr_call_comptime(Context *cnt, Ast *node, MirInstr *fn);
static MirInstr *create_instr_call_loc(Context *cnt, Ast *node, Location *call_location);
static MirInstr *create_instr_compound(Context *             cnt,
                                       Ast *                 node,
                                       MirInstr *            type,
                                       TSmallArray_InstrPtr *values,
                                       bool                  is_multiple_return_value);

static MirInstr *
create_instr_compound_impl(Context *cnt, Ast *node, MirType *type, TSmallArray_InstrPtr *values);

static MirInstr *create_default_value_for_type(Context *cnt, MirType *type);
static MirInstr *create_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index);
static MirInstr *create_instr_type_info(Context *cnt, Ast *node, MirInstr *expr);
static MirInstr *create_instr_member_ptr(Context *        cnt,
                                         Ast *            node,
                                         MirInstr *       target_ptr,
                                         Ast *            member_ident,
                                         ScopeEntry *     scope_entry,
                                         MirBuiltinIdKind builtin_id);
static MirInstr *create_instr_phi(Context *cnt, Ast *node);

static MirInstr *insert_instr_load(Context *cnt, MirInstr *src);
static MirInstr *insert_instr_cast(Context *cnt, MirInstr *src, MirType *to_type);
static MirInstr *insert_instr_addrof(Context *cnt, MirInstr *src);
static MirInstr *insert_instr_toany(Context *cnt, MirInstr *expr);
static MirCastOp get_cast_op(MirType *from, MirType *to);
static void      append_current_block(Context *cnt, MirInstr *instr);
static MirInstr *append_instr_arg(Context *cnt, Ast *node, unsigned i);
static MirInstr *
append_instr_unroll(Context *cnt, Ast *node, MirInstr *src, MirInstr *remove_src, s32 index);
static MirInstr *
append_instr_set_initializer(Context *cnt, Ast *node, TSmallArray_InstrPtr *dests, MirInstr *src);

static MirInstr *
append_instr_set_initializer_impl(Context *cnt, TSmallArray_InstrPtr *dests, MirInstr *src);

// @CLEANUP Is this even used?
static MirInstr *append_instr_phi(Context *cnt, Ast *node);
static MirInstr *append_instr_compound(Context *             cnt,
                                       Ast *                 node,
                                       MirInstr *            type,
                                       TSmallArray_InstrPtr *values,
                                       bool                  is_multiple_return_value);

static MirInstr *
append_instr_compound_impl(Context *cnt, Ast *node, MirType *type, TSmallArray_InstrPtr *values);

static MirInstr *append_instr_cast(Context *cnt, Ast *node, MirInstr *type, MirInstr *next);
static MirInstr *append_instr_sizeof(Context *cnt, Ast *node, MirInstr *expr);
static MirInstr *append_instr_type_info(Context *cnt, Ast *node, MirInstr *expr);
static MirInstr *append_instr_test_cases(Context *cnt, Ast *node);
static MirInstr *append_instr_alignof(Context *cnt, Ast *node, MirInstr *expr);
static MirInstr *append_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index);

static MirInstr *append_instr_member_ptr(Context *        cnt,
                                         Ast *            node,
                                         MirInstr *       target_ptr,
                                         Ast *            member_ident,
                                         ScopeEntry *     scope_entry,
                                         MirBuiltinIdKind builtin_id);

static MirInstr *append_instr_cond_br(Context *      cnt,
                                      Ast *          node,
                                      MirInstr *     cond,
                                      MirInstrBlock *then_block,
                                      MirInstrBlock *else_block);

static MirInstr *append_instr_br(Context *cnt, Ast *node, MirInstrBlock *then_block);
static MirInstr *append_instr_switch(Context *               cnt,
                                     Ast *                   node,
                                     MirInstr *              value,
                                     MirInstrBlock *         default_block,
                                     bool                    user_defined_default,
                                     TSmallArray_SwitchCase *cases);

static MirInstr *append_instr_load(Context *cnt, Ast *node, MirInstr *src);
static MirInstr *
append_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, TSmallArray_InstrPtr *args);
static MirInstr *
append_instr_type_fn_group(Context *cnt, Ast *node, ID *id, TSmallArray_InstrPtr *variants);

static MirInstr *append_instr_type_struct(Context *             cnt,
                                          Ast *                 node,
                                          ID *                  id,
                                          MirInstr *            fwd_decl, // Optional
                                          Scope *               scope,
                                          TSmallArray_InstrPtr *members,
                                          bool                  is_packed,
                                          bool                  is_union,
                                          bool                  is_multiple_return_type);

static MirInstr *append_instr_type_enum(Context *             cnt,
                                        Ast *                 node,
                                        ID *                  id,
                                        Scope *               scope,
                                        TSmallArray_InstrPtr *variants,
                                        MirInstr *            base_type);

static MirInstr *append_instr_type_ptr(Context *cnt, Ast *node, MirInstr *type);
static MirInstr *
append_instr_type_array(Context *cnt, Ast *node, ID *id, MirInstr *elem_type, MirInstr *len);

static MirInstr *append_instr_type_slice(Context *cnt, Ast *node, MirInstr *elem_type);
static MirInstr *append_instr_type_dynarr(Context *cnt, Ast *node, MirInstr *elem_type);
static MirInstr *append_instr_type_vargs(Context *cnt, Ast *node, MirInstr *elem_type);
static MirInstr *append_instr_fn_proto(Context * cnt,
                                       Ast *     node,
                                       MirInstr *type,
                                       MirInstr *user_type,
                                       bool      schedule_analyze);
static MirInstr *append_instr_fn_group(Context *cnt, Ast *node, TSmallArray_InstrPtr *variants);
static MirInstr *append_instr_decl_ref(Context *   cnt,
                                       Ast *       node,
                                       Unit *      parent_unit,
                                       ID *        rid,
                                       Scope *     scope,
                                       ScopeEntry *scope_entry);

static MirInstr *append_instr_decl_direct_ref(Context *cnt, MirInstr *ref);

static MirInstr *
append_instr_call(Context *cnt, Ast *node, MirInstr *callee, TSmallArray_InstrPtr *args);

static MirInstr *append_instr_decl_var(Context *        cnt,
                                       Ast *            node, // Optional
                                       ID *             id,
                                       Scope *          scope,
                                       MirInstr *       type,
                                       MirInstr *       init,
                                       bool             is_mutable,
                                       u32              flags,
                                       MirBuiltinIdKind builtin_id);

static MirInstr *create_instr_decl_var_impl(Context *   cnt,
                                            const char *name,
                                            MirInstr *  type,
                                            MirInstr *  init,
                                            bool        is_mutable,
                                            bool        is_global);

static MirInstr *append_instr_decl_var_impl(Context *   cnt,
                                            const char *name,
                                            MirInstr *  type,
                                            MirInstr *  init,
                                            bool        is_mutable,
                                            bool        is_global);

static MirInstr *
append_instr_decl_member(Context *cnt, Ast *node, MirInstr *type, TSmallArray_InstrPtr *tags);

static MirInstr *append_instr_decl_member_impl(Context *             cnt,
                                               Ast *                 node,
                                               ID *                  id,
                                               MirInstr *            type,
                                               TSmallArray_InstrPtr *tags);

static MirInstr *append_instr_decl_arg(Context *cnt, Ast *node, MirInstr *type, MirInstr *value);
static MirInstr *append_instr_decl_variant(Context *cnt, Ast *node, MirInstr *value);
static MirInstr *append_instr_const_int(Context *cnt, Ast *node, MirType *type, u64 val);
static MirInstr *append_instr_const_float(Context *cnt, Ast *node, float val);
static MirInstr *append_instr_const_double(Context *cnt, Ast *node, double val);
static MirInstr *append_instr_const_bool(Context *cnt, Ast *node, bool val);
static MirInstr *append_instr_const_string(Context *cnt, Ast *node, const char *str);
static MirInstr *append_instr_const_char(Context *cnt, Ast *node, char c);
static MirInstr *append_instr_const_null(Context *cnt, Ast *node);
static MirInstr *append_instr_ret(Context *cnt, Ast *node, MirInstr *value);
static MirInstr *append_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest);
static MirInstr *
append_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op);

static MirInstr *append_instr_unop(Context *cnt, Ast *node, MirInstr *instr, UnopKind op);
static MirInstr *append_instr_unrecheable(Context *cnt, Ast *node);
static MirInstr *append_instr_addrof(Context *cnt, Ast *node, MirInstr *src);

// This will erase whole instruction tree of instruction with ref_count == 0. When force is set
// ref_count is ignored.
static void      erase_instr_tree(MirInstr *instr, bool keep_root, bool force);
static MirInstr *append_instr_call_loc(Context *cnt, Ast *node);

// ast
static MirInstr *
ast_create_global_initializer2(Context *cnt, Ast *ast_value, TSmallArray_InstrPtr *decls);
static MirInstr *ast_create_global_initializer(Context *cnt, Ast *node, MirInstr *decls);
static MirInstr *ast_create_impl_fn_call(Context *   cnt,
                                         Ast *       node,
                                         const char *fn_name,
                                         MirType *   fn_type,
                                         bool        schedule_analyze);

static void      ast_push_fn_context(Context *cnt);
static void      ast_pop_fn_context(Context *cnt);
static MirInstr *ast(Context *cnt, Ast *node);
static void      ast_ublock(Context *cnt, Ast *ublock);
static void      ast_unrecheable(Context *cnt, Ast *unr);
static void      ast_defer_block(Context *cnt, Ast *block, bool whole_tree);
static void      ast_block(Context *cnt, Ast *block);
static void      ast_stmt_if(Context *cnt, Ast *stmt_if);
static void      ast_stmt_return(Context *cnt, Ast *ret);
static void      ast_stmt_defer(Context *cnt, Ast *defer);
static void      ast_stmt_loop(Context *cnt, Ast *loop);
static void      ast_stmt_break(Context *cnt, Ast *br);
static void      ast_stmt_continue(Context *cnt, Ast *cont);
static void      ast_stmt_switch(Context *cnt, Ast *stmt_switch);
static MirInstr *ast_decl_entity(Context *cnt, Ast *entity);
static MirInstr *ast_decl_arg(Context *cnt, Ast *arg);
static MirInstr *ast_decl_member(Context *cnt, Ast *arg);
static MirInstr *ast_decl_variant(Context *cnt, Ast *variant);
static MirInstr *ast_ref(Context *cnt, Ast *ref);
static MirInstr *ast_type_struct(Context *cnt, Ast *type_struct);
static MirInstr *ast_type_fn(Context *cnt, Ast *type_fn);
static MirInstr *ast_type_fn_group(Context *cnt, Ast *group);
static MirInstr *ast_type_arr(Context *cnt, Ast *type_arr);
static MirInstr *ast_type_slice(Context *cnt, Ast *type_slice);
static MirInstr *ast_type_dynarr(Context *cnt, Ast *type_dynarr);
static MirInstr *ast_type_ptr(Context *cnt, Ast *type_ptr);
static MirInstr *ast_type_vargs(Context *cnt, Ast *type_vargs);
static MirInstr *ast_type_enum(Context *cnt, Ast *type_enum);
static MirInstr *ast_expr_addrof(Context *cnt, Ast *addrof);
static MirInstr *ast_expr_cast(Context *cnt, Ast *cast);
static MirInstr *ast_expr_sizeof(Context *cnt, Ast *szof);
static MirInstr *ast_expr_type_info(Context *cnt, Ast *type_info);
static MirInstr *ast_expr_test_cases(Context *cnt, Ast *tc);
static MirInstr *ast_expr_alignof(Context *cnt, Ast *szof);
static MirInstr *ast_expr_type(Context *cnt, Ast *type);
static MirInstr *ast_expr_deref(Context *cnt, Ast *deref);
static MirInstr *ast_expr_call(Context *cnt, Ast *call);
static MirInstr *ast_expr_elem(Context *cnt, Ast *elem);
static MirInstr *ast_expr_null(Context *cnt, Ast *nl);
static MirInstr *ast_expr_lit_int(Context *cnt, Ast *expr);
static MirInstr *ast_expr_lit_float(Context *cnt, Ast *expr);
static MirInstr *ast_expr_lit_double(Context *cnt, Ast *expr);
static MirInstr *ast_expr_lit_bool(Context *cnt, Ast *expr);
static MirInstr *ast_expr_lit_fn(Context *        cnt,
                                 Ast *            lit_fn,
                                 Ast *            decl_node,
                                 Ast *            explicit_linkage_name,
                                 bool             is_global,
                                 u32              flags,
                                 MirBuiltinIdKind builtin_id);
static MirInstr *ast_expr_lit_fn_group(Context *cnt, Ast *group);
static MirInstr *ast_expr_lit_string(Context *cnt, Ast *lit_string);
static MirInstr *ast_expr_lit_char(Context *cnt, Ast *lit_char);
static MirInstr *ast_expr_binop(Context *cnt, Ast *binop);
static MirInstr *ast_expr_unary(Context *cnt, Ast *unop);
static MirInstr *ast_expr_compound(Context *cnt, Ast *cmp);
static MirInstr *ast_call_loc(Context *cnt, Ast *loc);

// analyze
static bool          evaluate(Context *cnt, MirInstr *instr);
static AnalyzeResult analyze_var(Context *cnt, MirVar *var);
static AnalyzeResult analyze_instr(Context *cnt, MirInstr *instr);

#define analyze_slot(cnt, conf, input, slot_type)                                                  \
    _analyze_slot((cnt), (conf), (input), (slot_type), false)

#define analyze_slot_initializer(cnt, conf, input, slot_type)                                      \
    _analyze_slot((cnt), (conf), (input), (slot_type), true)

static AnalyzeState _analyze_slot(Context *                cnt,
                                  const AnalyzeSlotConfig *conf,
                                  MirInstr **              input,
                                  MirType *                slot_type,
                                  bool                     is_initilizer);

static ANALYZE_STAGE_FN(load);
static ANALYZE_STAGE_FN(toany);
static ANALYZE_STAGE_FN(arrtoslice);
static ANALYZE_STAGE_FN(dynarrtoslice);
static ANALYZE_STAGE_FN(implicit_cast);
static ANALYZE_STAGE_FN(report_type_mismatch);
static ANALYZE_STAGE_FN(unroll);
static ANALYZE_STAGE_FN(set_volatile_expr);
static ANALYZE_STAGE_FN(set_null);
static ANALYZE_STAGE_FN(set_auto);

static const AnalyzeSlotConfig analyze_slot_conf_dummy = {.count = 0};

static const AnalyzeSlotConfig analyze_slot_conf_basic = {
    .count  = 2,
    .stages = {analyze_stage_unroll, analyze_stage_load}};

static const AnalyzeSlotConfig analyze_slot_conf_default = {.count  = 8,
                                                            .stages = {
                                                                analyze_stage_set_volatile_expr,
                                                                analyze_stage_set_null,
                                                                analyze_stage_set_auto,
                                                                analyze_stage_arrtoslice,
                                                                analyze_stage_dynarrtoslice,
                                                                analyze_stage_load,
                                                                analyze_stage_implicit_cast,
                                                                analyze_stage_report_type_mismatch,
                                                            }};

static const AnalyzeSlotConfig analyze_slot_conf_full = {.count  = 9,
                                                         .stages = {
                                                             analyze_stage_set_volatile_expr,
                                                             analyze_stage_set_null,
                                                             analyze_stage_set_auto,
                                                             analyze_stage_toany,
                                                             analyze_stage_arrtoslice,
                                                             analyze_stage_dynarrtoslice,
                                                             analyze_stage_load,
                                                             analyze_stage_implicit_cast,
                                                             analyze_stage_report_type_mismatch,
                                                         }};

// This function produce analyze of implicit call to the type resolver function in MIR and set
// out_type when analyze passed without problems. When analyze does not pass postpone is returned
// and out_type stay unchanged.
static AnalyzeResult
analyze_resolve_type(Context *cnt, MirInstr *resolver_call, MirType **out_type);
static AnalyzeResult analyze_instr_unroll(Context *cnt, MirInstrUnroll *unroll);
static AnalyzeResult analyze_instr_compound(Context *cnt, MirInstrCompound *cmp);
static AnalyzeResult analyze_instr_set_initializer(Context *cnt, MirInstrSetInitializer *si);
static AnalyzeResult analyze_instr_phi(Context *cnt, MirInstrPhi *phi);
static AnalyzeResult analyze_instr_toany(Context *cnt, MirInstrToAny *toany);
static AnalyzeResult analyze_instr_vargs(Context *cnt, MirInstrVArgs *vargs);
static AnalyzeResult analyze_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);
static AnalyzeResult analyze_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr);
static AnalyzeResult analyze_instr_addrof(Context *cnt, MirInstrAddrOf *addrof);
static AnalyzeResult analyze_instr_block(Context *cnt, MirInstrBlock *block);
static AnalyzeResult analyze_instr_ret(Context *cnt, MirInstrRet *ret);
static AnalyzeResult analyze_instr_arg(Context *cnt, MirInstrArg *arg);
static AnalyzeResult analyze_instr_unop(Context *cnt, MirInstrUnop *unop);
static AnalyzeResult analyze_instr_test_cases(Context *cnt, MirInstrTestCases *tc);
static AnalyzeResult analyze_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);
static AnalyzeResult analyze_instr_cond_br(Context *cnt, MirInstrCondBr *br);
static AnalyzeResult analyze_instr_br(Context *cnt, MirInstrBr *br);
static AnalyzeResult analyze_instr_switch(Context *cnt, MirInstrSwitch *sw);
static AnalyzeResult analyze_instr_load(Context *cnt, MirInstrLoad *load);
static AnalyzeResult analyze_instr_store(Context *cnt, MirInstrStore *store);
static AnalyzeResult analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);
static AnalyzeResult analyze_instr_fn_group(Context *cnt, MirInstrFnGroup *group);
static AnalyzeResult analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn);
static AnalyzeResult analyze_instr_type_fn_group(Context *cnt, MirInstrTypeFnGroup *group);
static AnalyzeResult analyze_instr_type_struct(Context *cnt, MirInstrTypeStruct *type_struct);
static AnalyzeResult analyze_instr_type_slice(Context *cnt, MirInstrTypeSlice *type_slice);
static AnalyzeResult analyze_instr_type_dynarr(Context *cnt, MirInstrTypeDynArr *type_dynarr);
static AnalyzeResult analyze_instr_type_vargs(Context *cnt, MirInstrTypeVArgs *type_vargs);
static AnalyzeResult analyze_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr);
static AnalyzeResult analyze_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr);
static AnalyzeResult analyze_instr_type_enum(Context *cnt, MirInstrTypeEnum *type_enum);
static AnalyzeResult analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *decl);
static AnalyzeResult analyze_instr_decl_member(Context *cnt, MirInstrDeclMember *decl);
static AnalyzeResult analyze_instr_decl_variant(Context *cnt, MirInstrDeclVariant *variant_instr);
static AnalyzeResult analyze_instr_decl_arg(Context *cnt, MirInstrDeclArg *decl);
static AnalyzeResult analyze_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);
static AnalyzeResult analyze_instr_decl_direct_ref(Context *cnt, MirInstrDeclDirectRef *ref);
static AnalyzeResult analyze_instr_const(Context *cnt, MirInstrConst *cnst);
static AnalyzeResult analyze_builtin_call(Context *cnt, MirInstrCall *call);
static AnalyzeResult analyze_instr_call(Context *cnt, MirInstrCall *call);
static AnalyzeResult analyze_instr_cast(Context *cnt, MirInstrCast *cast, bool analyze_op_only);
static AnalyzeResult analyze_instr_sizeof(Context *cnt, MirInstrSizeof *szof);
static AnalyzeResult analyze_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info);
static AnalyzeResult analyze_instr_alignof(Context *cnt, MirInstrAlignof *alof);
static AnalyzeResult analyze_instr_binop(Context *cnt, MirInstrBinop *binop);
static AnalyzeResult analyze_instr_call_loc(Context *cnt, MirInstrCallLoc *loc);
static void          analyze_report_unresolved(Context *cnt);
static void          analyze_report_unused(Context *cnt);

//***********/
//*  RTTI   */
//***********/
static MirVar *   _rtti_gen(Context *cnt, MirType *type);
static MirVar *   rtti_gen(Context *cnt, MirType *type);
static MirVar *   rtti_create_and_alloc_var(Context *cnt, MirType *type);
static void       rtti_satisfy_incomplete(Context *cnt, RTTIIncomplete *incomplete);
static MirVar *   rtti_gen_integer(Context *cnt, MirType *type);
static MirVar *   rtti_gen_real(Context *cnt, MirType *type);
static MirVar *   rtti_gen_ptr(Context *cnt, MirType *type, MirVar *incomplete);
static MirVar *   rtti_gen_array(Context *cnt, MirType *type);
static MirVar *   rtti_gen_empty(Context *cnt, MirType *type, MirType *rtti_type);
static MirVar *   rtti_gen_enum(Context *cnt, MirType *type);
static void       rtti_gen_enum_variant(Context *cnt, VMStackPtr dest, MirVariant *variant);
static VMStackPtr rtti_gen_enum_variants_array(Context *cnt, TSmallArray_VariantPtr *variants);
static void
rtti_gen_enum_variants_slice(Context *cnt, VMStackPtr dest, TSmallArray_VariantPtr *variants);

static void       rtti_gen_struct_member(Context *cnt, VMStackPtr dest, MirMember *member);
static VMStackPtr rtti_gen_struct_members_array(Context *cnt, TSmallArray_MemberPtr *members);
static void
rtti_gen_struct_members_slice(Context *cnt, VMStackPtr dest, TSmallArray_MemberPtr *members);

static MirVar *   rtti_gen_struct(Context *cnt, MirType *type);
static void       rtti_gen_fn_arg(Context *cnt, VMStackPtr dest, MirArg *arg);
static VMStackPtr rtti_gen_fn_args_array(Context *cnt, TSmallArray_ArgPtr *args);
static VMStackPtr rtti_gen_fns_array(Context *cnt, TSmallArray_TypePtr *fns);
static void       rtti_gen_fn_args_slice(Context *cnt, VMStackPtr dest, TSmallArray_ArgPtr *args);
static MirVar *   rtti_gen_fn(Context *cnt, MirType *type);
static void       rtti_gen_fn_slice(Context *cnt, VMStackPtr dest, TSmallArray_TypePtr *fns);
static MirVar *   rtti_gen_fn_group(Context *cnt, MirType *type);

// INLINES
static INLINE void usage_check_push(Context *cnt, ScopeEntry *entry)
{
    BL_ASSERT(entry);
    if (!entry->id) return;
    if (!entry->node) return;
    tarray_push(&cnt->analyze.usage_check_queue, entry);
}

static INLINE bool can_mutate_comptime_to_const(MirInstr *instr)
{
    BL_ASSERT(instr->analyzed && "Non-analyzed instruction.");
    BL_ASSERT(mir_is_comptime(instr));

    switch (instr->kind) {
    case MIR_INSTR_CONST:
    case MIR_INSTR_BLOCK:
    case MIR_INSTR_FN_PROTO:
    case MIR_INSTR_CALL:
        return false;
    default:
        break;
    }

    switch (instr->value.type->kind) {
    case MIR_TYPE_TYPE:
    case MIR_TYPE_INT:
    case MIR_TYPE_REAL:
    case MIR_TYPE_BOOL:
    case MIR_TYPE_ENUM:
    case MIR_TYPE_STRING:
        return true;
        break;

    default:
        return false;
    }
}

// @INCOMPLETE
// @INCOMPLETE
// @INCOMPLETE

// Get struct base type if there is one.
static INLINE MirType *get_base_type(const MirType *struct_type)
{
    if (struct_type->kind != MIR_TYPE_STRUCT) return NULL;
    MirType *base_type = struct_type->data.strct.base_type;
    return base_type;
}

// Get base type scope if there is one.
static INLINE Scope *get_base_type_scope(MirType *struct_type)
{
    MirType *base_type = get_base_type(struct_type);
    if (!base_type) return NULL;
    if (!mir_is_composit_type(base_type)) return NULL;

    return base_type->data.strct.scope;
}

// Determinate if type is incomplete struct type.
static INLINE bool is_incomplete_struct_type(MirType *type)
{
    return mir_is_composit_type(type) && type->data.strct.is_incomplete;
}

// Checks whether type is complete type, checks also dependencies. In practice only composit types
// can be incomplete, but in some cases (RTTI generation) we need to check whole dependency type
// tree for completeness.
static bool is_complete_type(Context *cnt, MirType *type)
{
    TracyCZone(_tctx, true);
    TSmallArray_TypePtr *stack   = &cnt->analyze.complete_check_type_stack;
    THashTable *         visited = &cnt->analyze.complete_check_visited;
    tsa_push_TypePtr(stack, type);
    bool result = true;
    while (stack->size > 0) {
        MirType *top = tsa_pop_TypePtr(stack);
        BL_ASSERT(top);
        if (top->checked_and_complete) continue;
        if (is_incomplete_struct_type(top)) {
            result = false;
            goto DONE;
        }
        switch (top->kind) {
        case MIR_TYPE_PTR: {
            tsa_push_TypePtr(stack, top->data.ptr.expr);
            break;
        }
        case MIR_TYPE_ARRAY: {
            tsa_push_TypePtr(stack, top->data.array.elem_type);
            break;
        }
        case MIR_TYPE_FN: {
            if (top->data.fn.ret_type) tsa_push_TypePtr(stack, top->data.fn.ret_type);
            if (top->data.fn.args) {
                MirArg *arg;
                TSA_FOREACH(top->data.fn.args, arg)
                {
                    tsa_push_TypePtr(stack, arg->type);
                }
            }
            break;
        }
        case MIR_TYPE_DYNARR:
        case MIR_TYPE_SLICE:
        case MIR_TYPE_STRING:
        case MIR_TYPE_VARGS:
        case MIR_TYPE_STRUCT: {
            if (thtbl_has_key(visited, (u64)top)) break;
            thtbl_insert_empty(visited, (u64)top);
            MirMember *member;
            TSA_FOREACH(top->data.strct.members, member)
            {
                tsa_push_TypePtr(stack, member->type);
            }
            break;
        }
        default:
            continue;
        }
    }
DONE:
    stack->size = 0;
    thtbl_clear(visited);
    type->checked_and_complete = result;
    TracyCZoneEnd(_tctx);
    return result;
}

// Determinate if instruction has volatile type, that means we can change type of the value during
// analyze pass as needed. This is used for constant integer literals.
static INLINE bool is_instr_type_volatile(MirInstr *instr)
{
    switch (instr->kind) {
    case MIR_INSTR_CONST:
        return ((MirInstrConst *)instr)->volatile_type;

    case MIR_INSTR_UNOP:
        return ((MirInstrUnop *)instr)->volatile_type;

    case MIR_INSTR_BINOP:
        return ((MirInstrBinop *)instr)->volatile_type;

    default:
        return false;
    }
}

static INLINE bool is_pointer_to_type_type(MirType *type)
{
    while (mir_is_pointer_type(type)) {
        type = mir_deref_type(type);
    }

    return type->kind == MIR_TYPE_TYPE;
}

static INLINE bool type_cmp(const MirType *first, const MirType *second)
{
    BL_ASSERT(first && second);
    return first->id.hash == second->id.hash;
}

static INLINE bool can_impl_cast(const MirType *from, const MirType *to)
{
    // Here we allow implicit cast from any pointer type to bool, this breaks quite strict
    // implicit casting rules in this language, but this kind of cast can be handy because we
    // check pointer values for null very often. Until this was enabled, programmer was forced to
    // explicitly type down comparison to null.
    //
    // We should consider later if this implicit casting should not be enabled only in
    // expressions used in if statement because globally enabled implicit cast from pointer to
    // bool could be misleading sometimes. Consider calling of a function, taking as parameter
    // bool, in such case we can pass pointer to such function instead of bool without any
    // explicit information about casting.
    if (from->kind == MIR_TYPE_PTR && to->kind == MIR_TYPE_BOOL) return true;
    // Implicit cast of vargs to slice.
    if (from->kind == MIR_TYPE_VARGS && to->kind == MIR_TYPE_SLICE) {
        from = mir_get_struct_elem_type(from, MIR_SLICE_PTR_INDEX);
        to   = mir_get_struct_elem_type(to, MIR_SLICE_PTR_INDEX);
        return type_cmp(from, to);
    }
    if (from->kind != to->kind) return false;

    // Check base types for structs.
    if (from->kind == MIR_TYPE_PTR) {
        from = mir_deref_type(from);
        to   = mir_deref_type(to);

        while (true) {
            if (!from) return false;
            if (type_cmp(from, to)) {
                return true;
            } else {
                from = get_base_type(from);
            }
        }

        return false;
    }
    if (from->kind != MIR_TYPE_INT) return false;
    if (from->data.integer.is_signed != to->data.integer.is_signed) return false;
    const s32 fb = from->data.integer.bitcount;
    const s32 tb = to->data.integer.bitcount;
    if (fb > tb) return false;
    return true;
}

static INLINE MirFn *get_callee(MirInstrCall *call)
{
    MirFn *fn = MIR_CEV_READ_AS(MirFn *, &call->callee->value);
    BL_MAGIC_ASSERT(fn);
    return fn;
}

static INLINE MirFn *instr_owner_fn(MirInstr *instr)
{
    if (!instr->owner_block) return NULL;
    return instr->owner_block->owner_fn;
}

static INLINE MirInstrBlock *ast_current_block(Context *cnt)
{
    return cnt->ast.current_block;
}

static INLINE MirFn *ast_current_fn(Context *cnt)
{
    return cnt->ast.current_block ? cnt->ast.current_block->owner_fn : NULL;
}

static INLINE void terminate_block(MirInstrBlock *block, MirInstr *terminator)
{
    BL_ASSERT(block);
    if (block->terminal) BL_ABORT("basic block '%s' already terminated!", block->name);
    block->terminal = terminator;
}

static INLINE bool is_block_terminated(MirInstrBlock *block)
{
    return block->terminal;
}

static INLINE bool is_current_block_terminated(Context *cnt)
{
    return ast_current_block(cnt)->terminal;
}

static INLINE MirInstr *mutate_instr(MirInstr *instr, MirInstrKind kind)
{
    BL_ASSERT(instr);
    instr->kind = kind;
    return instr;
}

static INLINE void erase_instr(MirInstr *instr)
{
    if (!instr) return;
    MirInstrBlock *block = instr->owner_block;
    if (!block) return;

    // first in block
    if (block->entry_instr == instr) block->entry_instr = instr->next;
    if (instr->prev) instr->prev->next = instr->next;
    if (instr->next) instr->next->prev = instr->prev;

    instr->prev = NULL;
    instr->next = NULL;
}

static INLINE void insert_instr_after(MirInstr *after, MirInstr *instr)
{
    BL_ASSERT(after && instr);

    MirInstrBlock *block = after->owner_block;

    instr->next = after->next;
    instr->prev = after;
    if (after->next) after->next->prev = instr;
    after->next = instr;

    instr->owner_block = block;
    if (block->last_instr == after) instr->owner_block->last_instr = instr;
}

static INLINE void insert_instr_before(MirInstr *before, MirInstr *instr)
{
    BL_ASSERT(before && instr);

    MirInstrBlock *block = before->owner_block;

    instr->next = before;
    instr->prev = before->prev;
    if (before->prev) before->prev->next = instr;
    before->prev = instr;

    instr->owner_block = block;
    if (block->entry_instr == before) instr->owner_block->entry_instr = instr;
}

static INLINE void push_into_gscope(Context *cnt, MirInstr *instr)
{
    BL_ASSERT(instr);
    instr->id = cnt->assembly->MIR.global_instrs.size;
    tarray_push(&cnt->assembly->MIR.global_instrs, instr);
};

static int         push_count = 0;
static INLINE void analyze_push_back(Context *cnt, MirInstr *instr)
{
    BL_ASSERT(instr);
    ++push_count;
    tlist_push_back(&cnt->analyze.queue, instr);
}

static INLINE void analyze_push_front(Context *cnt, MirInstr *instr)
{
    BL_ASSERT(instr);
    tlist_push_front(&cnt->analyze.queue, instr);
}

static INLINE void analyze_notify_provided(Context *cnt, u64 hash)
{
    TIterator iter = thtbl_find(&cnt->analyze.waiting, hash);
    TIterator end  = thtbl_end(&cnt->analyze.waiting);
    if (TITERATOR_EQUAL(iter, end)) return; // No one is waiting for this...

#if BL_DEBUG && VERBOSE_ANALYZE
    printf("Analyze: Notify '%llu'.\n", (unsigned long long)hash);
#endif

    TArray *wq = &thtbl_iter_peek_value(TArray, iter);
    BL_ASSERT(wq);

    MirInstr *instr;
    TARRAY_FOREACH(MirInstr *, wq, instr)
    {
        analyze_push_back(cnt, instr);
    }

    // Also clear element content!
    tarray_terminate(wq);
    thtbl_erase(&cnt->analyze.waiting, iter);
}

static INLINE const char *gen_uq_name(const char *prefix)
{
    static s32 ui = 0;
    TString *  s  = builder_create_cached_str();

    tstring_append(s, prefix);
    char ui_str[22];
    sprintf(ui_str, ".%d", ui++);
    tstring_append(s, ui_str);
    return s->data;
}

static INLINE bool is_builtin(Ast *ident, MirBuiltinIdKind kind)
{
    if (!ident) return false;
    BL_ASSERT(ident->kind == AST_IDENT);
    return ident->data.ident.id.hash == builtin_ids[kind].hash;
}

static MirBuiltinIdKind get_builtin_kind(Ast *ident)
{
    if (!ident) return false;
    BL_ASSERT(ident->kind == AST_IDENT);

    // PERFORMANCE: Eventually use hash table.
    for (u32 i = 0; i < TARRAY_SIZE(builtin_ids); ++i) {
        if (builtin_ids[i].hash == ident->data.ident.id.hash) {
            return i;
        }
    }

    return MIR_BUILTIN_ID_NONE;
}

static INLINE bool get_block_terminator(MirInstrBlock *block)
{
    return block->terminal;
}

static INLINE void set_current_block(Context *cnt, MirInstrBlock *block)
{
    cnt->ast.current_block = block;
}

static INLINE void error_types(MirType *from, MirType *to, Ast *loc, const char *msg)
{
    BL_ASSERT(from && to);
    if (!msg) msg = "No implicit cast for type '%s' and '%s'.";

    char tmp_from[256];
    char tmp_to[256];
    mir_type_to_str(tmp_from, 256, from, true);
    mir_type_to_str(tmp_to, 256, to, true);

    builder_msg(BUILDER_MSG_ERROR,
                ERR_INVALID_TYPE,
                loc ? loc->location : NULL,
                BUILDER_CUR_WORD,
                msg,
                tmp_from,
                tmp_to);
}

static INLINE void commit_fn(Context *cnt, MirFn *fn)
{
    ID *id = fn->id;
    BL_ASSERT(id);
    ScopeEntry *entry = fn->entry;
    BL_MAGIC_ASSERT(entry);
    BL_ASSERT(entry->kind != SCOPE_ENTRY_VOID);
    entry->kind    = SCOPE_ENTRY_FN;
    entry->data.fn = fn;
    analyze_notify_provided(cnt, id->hash);
}

static INLINE void commit_variant(Context UNUSED(*cnt), MirVariant *variant)
{
    ScopeEntry *entry = variant->entry;
    BL_MAGIC_ASSERT(entry);
    BL_ASSERT(entry->kind != SCOPE_ENTRY_VOID);
    entry->kind         = SCOPE_ENTRY_VARIANT;
    entry->data.variant = variant;
}

static INLINE void commit_member(Context UNUSED(*cnt), MirMember *member)
{
    ScopeEntry *entry = member->entry;
    BL_MAGIC_ASSERT(entry);
    // Do not commit void entries
    if (entry->kind == SCOPE_ENTRY_VOID) return;
    entry->kind        = SCOPE_ENTRY_MEMBER;
    entry->data.member = member;
}

static INLINE void commit_var(Context *cnt, MirVar *var)
{
    ID *id = var->id;
    BL_ASSERT(id);
    ScopeEntry *entry = var->entry;
    BL_MAGIC_ASSERT(entry);
    // Do not commit void entries
    if (entry->kind == SCOPE_ENTRY_VOID) return;
    entry->kind     = SCOPE_ENTRY_VAR;
    entry->data.var = var;
    BL_TRACY_MESSAGE("COMMIT_VAR", "%s", id->str);
    if (var->is_global || var->is_struct_typedef) analyze_notify_provided(cnt, id->hash);
}

// Provide builtin type. Register & commit.
static INLINE void provide_builtin_type(Context *cnt, MirType *type)
{
    ScopeEntry *entry = register_symbol(cnt, NULL, type->user_id, cnt->assembly->gscope, true);
    if (!entry) return;
    BL_ASSERT(entry->kind != SCOPE_ENTRY_VOID);
    entry->kind      = SCOPE_ENTRY_TYPE;
    entry->data.type = type;
}

static INLINE void provide_builtin_member(Context *cnt, Scope *scope, MirMember *member)
{
    ScopeEntry *entry = register_symbol(cnt, NULL, member->id, scope, true);
    if (!entry) return;
    BL_ASSERT(entry->kind != SCOPE_ENTRY_VOID);
    entry->kind        = SCOPE_ENTRY_MEMBER;
    entry->data.member = member;
    member->entry      = entry;
}

static INLINE MirInstr *unref_instr(MirInstr *instr)
{
    if (!instr) return NULL;
    if (instr->ref_count == NO_REF_COUNTING) return instr;
    --instr->ref_count;
    return instr;
}

static INLINE MirInstr *ref_instr(MirInstr *instr)
{
    if (!instr) return NULL;
    if (instr->ref_count == NO_REF_COUNTING) return instr;
    ++instr->ref_count;
    return instr;
}

static INLINE void phi_add_income(MirInstrPhi *phi, MirInstr *value, MirInstrBlock *block)
{
    BL_ASSERT(phi && value && block);
    tsa_push_InstrPtr(phi->incoming_values, ref_instr(value));
    tsa_push_InstrPtr(phi->incoming_blocks, ref_instr(&block->base));
    if (value->kind == MIR_INSTR_COND_BR) {
        ((MirInstrCondBr *)value)->keep_stack_value = true;
    }
}

static INLINE bool is_load_needed(MirInstr *instr)
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
    case MIR_INSTR_TYPE_FN_GROUP:
    case MIR_INSTR_TYPE_PTR:
    case MIR_INSTR_TYPE_STRUCT:
    case MIR_INSTR_TYPE_DYNARR:
    case MIR_INSTR_CAST:
    case MIR_INSTR_DECL_MEMBER:
    case MIR_INSTR_TYPE_INFO:
    case MIR_INSTR_CALL_LOC:
    case MIR_INSTR_COMPOUND:
    case MIR_INSTR_SIZEOF:
        return false;

    case MIR_INSTR_LOAD: {
        // @HACK: this solves problem with user-level dereference of pointer to pointer
        // values. We get s32 vs *s32 type mismatch without this.
        //
        // Ex.: j : *s32 = ^ (cast(**s32) i_ptr_ptr);
        //
        // But I'm not 100% sure that this didn't broke something else...
        //
        MirInstrLoad *load = (MirInstrLoad *)instr;
        return load->is_deref && is_load_needed(load->src);
    }

    default:
        break;
    }

    return true;
}

static INLINE bool is_to_any_needed(Context *cnt, MirInstr *src, MirType *dest_type)
{
    if (!dest_type || !src) return false;
    MirType *any_type = lookup_builtin_type(cnt, MIR_BUILTIN_ID_ANY);
    BL_ASSERT(any_type);

    if (dest_type != any_type) return false;

    if (is_load_needed(src)) {
        MirType *src_type = src->value.type;
        if (mir_deref_type(src_type) == any_type) return false;
    }

    return true;
}

void ast_push_fn_context(Context *cnt)
{
    AstFnContext *fnctx;
    {
        TArray *     stack = &cnt->ast._fnctx_stack;
        AstFnContext _fnctx;
        tarray_push(stack, _fnctx);
        fnctx = &tarray_at(AstFnContext, stack, stack->size - 1);
    }
    memset(fnctx, 0, sizeof(AstFnContext));
    tsa_init(&fnctx->defer_stack);
    cnt->ast.fnctx = fnctx;
}

void ast_pop_fn_context(Context *cnt)
{
    TArray *stack = &cnt->ast._fnctx_stack;
    BL_ASSERT(stack->size);
    AstFnContext *fnctx = &tarray_at(AstFnContext, stack, stack->size - 1);
    BL_ASSERT(fnctx == cnt->ast.fnctx &&
              "AST function generation context malformed, push and pop out of sycn?");
    tsa_terminate(&fnctx->defer_stack);
    tarray_pop(stack);
    cnt->ast.fnctx = stack->size ? &tarray_at(AstFnContext, stack, stack->size - 1) : NULL;
}

void type_init_id(Context *cnt, MirType *type)
{
    // =============================================================================================
#define GEN_ID_STRUCT                                                                              \
    if (type->user_id) {                                                                           \
        tstring_append(tmp, type->user_id->str);                                                   \
    }                                                                                              \
                                                                                                   \
    tstring_append(tmp, "{");                                                                      \
    if (type->data.strct.members) {                                                                \
        MirMember *member;                                                                         \
        TSA_FOREACH(type->data.strct.members, member)                                              \
        {                                                                                          \
            BL_ASSERT(member->type->id.str);                                                       \
            tstring_append(tmp, member->type->id.str);                                             \
                                                                                                   \
            if (i != type->data.strct.members->size - 1) tstring_append(tmp, ",");                 \
        }                                                                                          \
    }                                                                                              \
                                                                                                   \
    tstring_append(tmp, "}");
    // =============================================================================================
    BL_ASSERT(type && "Invalid type pointer!");
    TString *tmp = &cnt->tmp_sh;
    tstring_clear(tmp);

    switch (type->kind) {
    case MIR_TYPE_BOOL:
    case MIR_TYPE_VOID:
    case MIR_TYPE_TYPE:
    case MIR_TYPE_REAL:
    case MIR_TYPE_NAMED_SCOPE:
    case MIR_TYPE_INT: {
        BL_ASSERT(type->user_id);
        tstring_append(tmp, type->user_id->str);
        break;
    }

    case MIR_TYPE_NULL: {
        // n.<name>
        tstring_clear(tmp);
        tstring_append(tmp, "n.");
        tstring_append(tmp, type->data.null.base_type->id.str);
        break;
    }

    case MIR_TYPE_PTR: {
        // p.<name>
        tstring_clear(tmp);
        tstring_append(tmp, "p.");
        tstring_append(tmp, type->data.ptr.expr->id.str);

        break;
    }

    case MIR_TYPE_FN: {
        tstring_append(tmp, "f.(");

        // append all arg types isd
        if (type->data.fn.args) {
            MirArg *arg;
            TSA_FOREACH(type->data.fn.args, arg)
            {
                BL_ASSERT(arg->type->id.str);
                tstring_append(tmp, arg->type->id.str);

                if (i != type->data.fn.args->size - 1) tstring_append(tmp, ",");
            }
        }

        tstring_append(tmp, ")");
        type->data.fn.argument_hash = thash_from_str(tmp->data);

        if (type->data.fn.ret_type) {
            BL_ASSERT(type->data.fn.ret_type->id.str);
            tstring_append(tmp, type->data.fn.ret_type->id.str);
        } else {
            // implicit return void
            tstring_append(tmp, cnt->builtin_types->t_void->id.str);
        }
        break;
    }

    case MIR_TYPE_FN_GROUP: {
        tstring_append(tmp, "f.{");
        // append all arg types isd
        if (type->data.fn_group.variants) {
            MirType *variant;
            TSA_FOREACH(type->data.fn_group.variants, variant)
            {
                BL_ASSERT(variant->id.str);
                tstring_append(tmp, variant->id.str);

                if (i != type->data.fn_group.variants->size - 1) tstring_append(tmp, ",");
            }
        }
        tstring_append(tmp, "}");
        break;
    }

    case MIR_TYPE_ARRAY: {
        char ui_str[21];
        sprintf(ui_str, "%llu", (unsigned long long)type->data.array.len);

        tstring_append(tmp, ui_str);
        tstring_append(tmp, ".");
        tstring_append(tmp, type->data.array.elem_type->id.str);
        break;
    }

    case MIR_TYPE_STRING: {
        tstring_append(tmp, "ss.");
        GEN_ID_STRUCT;
        break;
    }

    case MIR_TYPE_SLICE: {
        tstring_append(tmp, "sl.");
        GEN_ID_STRUCT;
        break;
    }

    case MIR_TYPE_DYNARR: {
        tstring_append(tmp, "da.");
        GEN_ID_STRUCT;
        break;
    }

    case MIR_TYPE_VARGS: {
        tstring_append(tmp, "sv.");
        GEN_ID_STRUCT;
        break;
    }

    case MIR_TYPE_STRUCT: {
        static u64 serial   = 0;
        const bool is_union = type->data.strct.is_union;
        if (type->user_id) {
            char prefix[37];
            snprintf(prefix, TARRAY_SIZE(prefix), "%s%llu.", is_union ? "u" : "s", ++serial);

            tstring_append(tmp, prefix);
            if (type->user_id) tstring_append(tmp, type->user_id->str);
            break;
        }
        tstring_append(tmp, is_union ? "u." : "s.");
        GEN_ID_STRUCT;
        break;
    }

    case MIR_TYPE_ENUM: {
        tstring_append(tmp, "e.");

        if (type->user_id) tstring_append(tmp, type->user_id->str);
        tstring_append(tmp, "(");
        tstring_append(tmp, type->data.enm.base_type->id.str);
        tstring_append(tmp, ")");
        tstring_append(tmp, "{");
        if (type->data.enm.variants) {
            MirVariant *variant;
            TSA_FOREACH(type->data.enm.variants, variant)
            {
                BL_ASSERT(variant->value);

                char value_str[35];
                snprintf(value_str,
                         TARRAY_SIZE(value_str),
                         "%lld",
                         MIR_CEV_READ_AS(long long, variant->value));

                tstring_append(tmp, value_str);
                if (i != type->data.enm.variants->size - 1) tstring_append(tmp, ",");
            }
        }
        tstring_append(tmp, "}");
        break;
    }

    default:
        BL_UNIMPLEMENTED;
    }

    TString *copy = builder_create_cached_str();
    tstring_append(copy, tmp->data);
    type->id.str  = copy->data;
    type->id.hash = thash_from_str(copy->data);

#if TRACY_ENABLE
    static int tc = 0;
    TracyCPlot("Type count", ++tc);
    BL_TRACY_MESSAGE("CREATE_TYPE",
                     "%s %s (%lluB)",
                     type->id.str,
                     !is_incomplete_struct_type(type) ? "<INCOMPLETE>" : "",
                     (unsigned long long)sizeof(MirType));
#endif

#undef GEN_ID_STRUCT
}

MirType *create_type(Context *cnt, MirTypeKind kind, ID *user_id)
{
    MirType *type = arena_alloc(&cnt->assembly->arenas.mir.type);
    BL_MAGIC_SET(type);
    type->kind    = kind;
    type->user_id = user_id;
    return type;
}

ScopeEntry *register_symbol(Context *cnt, Ast *node, ID *id, Scope *scope, bool is_builtin)
{
    BL_ASSERT(id && "Missing symbol ID.");
    BL_ASSERT(scope && "Missing entry scope.");
    // Do not register explicitly unused symbol!!!
    if (is_ignored_id(id)) {
        BL_ASSERT(!is_builtin);
        return cnt->analyze.void_entry;
    }
    const bool  is_private = scope->kind == SCOPE_PRIVATE;
    ScopeEntry *collision  = scope_lookup(scope, id, is_private, false, NULL);
    if (collision) {
        if (!is_private) goto COLLIDE;
        const bool collision_in_same_unit =
            (node ? node->location->unit : NULL) ==
            (collision->node ? collision->node->location->unit : NULL);

        if (collision_in_same_unit) {
            goto COLLIDE;
        }
    }

    // no collision
    ScopeEntry *entry = scope_create_entry(
        &cnt->assembly->arenas.scope, SCOPE_ENTRY_INCOMPLETE, id, node, is_builtin);
    scope_insert(scope, entry);
    // Check usage only for function locals not starting with underscore.
    if (!builder.options->no_usage_check && (scope_is_subtree_of_kind(scope, SCOPE_FN) ||
                                             scope_is_subtree_of_kind(scope, SCOPE_PRIVATE))) {
        usage_check_push(cnt, entry);
    }
    BL_TRACY_MESSAGE("REGISTER_SYMBOL", "%s", id->str);
    return entry;

COLLIDE : {
    char *err_msg = (collision->is_builtin || is_builtin)
                        ? "Symbol name collision with compiler builtin '%s'."
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

MirType *lookup_builtin_type(Context *cnt, MirBuiltinIdKind kind)
{
    ID *        id    = &builtin_ids[kind];
    Scope *     scope = cnt->assembly->gscope;
    ScopeEntry *found = scope_lookup(scope, id, true, false, NULL);

    if (!found) BL_ABORT("Missing compiler internal symbol '%s'", id->str);
    if (found->kind == SCOPE_ENTRY_INCOMPLETE) return NULL;

    if (!found->is_builtin) {
        builder_msg(BUILDER_MSG_WARNING,
                    0,
                    found->node ? found->node->location : NULL,
                    BUILDER_CUR_WORD,
                    "Builtins used by compiler must have '#compiler' flag!");
    }

    BL_ASSERT(found->kind == SCOPE_ENTRY_VAR);
    MirVar *var = found->data.var;
    BL_ASSERT(var && var->value.is_comptime && var->value.type->kind == MIR_TYPE_TYPE);
    MirType *var_type = MIR_CEV_READ_AS(MirType *, &var->value);
    BL_MAGIC_ASSERT(var_type);

    // Wait when internal is not complete!
    if (is_incomplete_struct_type(var_type)) {
        return NULL;
    }

    return var_type;
}

MirFn *lookup_builtin_fn(Context *cnt, MirBuiltinIdKind kind)
{
    ID *        id    = &builtin_ids[kind];
    Scope *     scope = cnt->assembly->gscope;
    ScopeEntry *found = scope_lookup(scope, id, true, false, NULL);

    if (!found) BL_ABORT("Missing compiler internal symbol '%s'", id->str);
    if (found->kind == SCOPE_ENTRY_INCOMPLETE) return NULL;

    if (!found->is_builtin) {
        builder_msg(BUILDER_MSG_WARNING,
                    0,
                    found->node ? found->node->location : NULL,
                    BUILDER_CUR_WORD,
                    "Builtins used by compiler must have '#compiler' flag!");
    }

    BL_ASSERT(found->kind == SCOPE_ENTRY_FN);
    ref_instr(found->data.fn->prototype);
    return found->data.fn;
}

ID *lookup_builtins_rtti(Context *cnt)
{
//*********************************************************************************************/
#define LOOKUP_TYPE(N, K)                                                                          \
    if (!cnt->builtin_types->t_Type##N) {                                                          \
        cnt->builtin_types->t_Type##N = lookup_builtin_type(cnt, MIR_BUILTIN_ID_TYPE_##K);         \
        if (!cnt->builtin_types->t_Type##N) {                                                      \
            return &builtin_ids[MIR_BUILTIN_ID_TYPE_##K];                                          \
        }                                                                                          \
    }                                                                                              \
    //*********************************************************************************************/
    if (cnt->builtin_types->is_rtti_ready) return NULL;
    LOOKUP_TYPE(Kind, KIND);
    LOOKUP_TYPE(Info, INFO);
    LOOKUP_TYPE(InfoInt, INFO_INT);
    LOOKUP_TYPE(InfoReal, INFO_REAL);
    LOOKUP_TYPE(InfoPtr, INFO_PTR);
    LOOKUP_TYPE(InfoEnum, INFO_ENUM);
    LOOKUP_TYPE(InfoEnumVariant, INFO_ENUM_VARIANT);
    LOOKUP_TYPE(InfoArray, INFO_ARRAY);
    LOOKUP_TYPE(InfoStruct, INFO_STRUCT);
    LOOKUP_TYPE(InfoStructMember, INFO_STRUCT_MEMBER);
    LOOKUP_TYPE(InfoFn, INFO_FN);
    LOOKUP_TYPE(InfoFnArg, INFO_FN_ARG);
    LOOKUP_TYPE(InfoType, INFO_TYPE);
    LOOKUP_TYPE(InfoVoid, INFO_VOID);
    LOOKUP_TYPE(InfoBool, INFO_BOOL);
    LOOKUP_TYPE(InfoNull, INFO_NULL);
    LOOKUP_TYPE(InfoString, INFO_STRING);
    LOOKUP_TYPE(InfoStructMember, INFO_STRUCT_MEMBER);
    LOOKUP_TYPE(InfoEnumVariant, INFO_ENUM_VARIANT);
    LOOKUP_TYPE(InfoFnArg, INFO_FN_ARG);
    LOOKUP_TYPE(InfoFnGroup, INFO_FN_GROUP);

    cnt->builtin_types->t_TypeInfo_ptr   = create_type_ptr(cnt, cnt->builtin_types->t_TypeInfo);
    cnt->builtin_types->t_TypeInfoFn_ptr = create_type_ptr(cnt, cnt->builtin_types->t_TypeInfoFn);
    cnt->builtin_types->t_TypeInfo_slice =
        CREATE_TYPE_STRUCT_SLICE(cnt, NULL, cnt->builtin_types->t_TypeInfo_ptr);
    cnt->builtin_types->t_TypeInfoStructMembers_slice = CREATE_TYPE_STRUCT_SLICE(
        cnt, NULL, create_type_ptr(cnt, cnt->builtin_types->t_TypeInfoStructMember));
    cnt->builtin_types->t_TypeInfoEnumVariants_slice = CREATE_TYPE_STRUCT_SLICE(
        cnt, NULL, create_type_ptr(cnt, cnt->builtin_types->t_TypeInfoEnumVariant));
    cnt->builtin_types->t_TypeInfoFnArgs_slice = CREATE_TYPE_STRUCT_SLICE(
        cnt, NULL, create_type_ptr(cnt, cnt->builtin_types->t_TypeInfoFnArg));
    cnt->builtin_types->t_TypeInfoFn_ptr_slice = CREATE_TYPE_STRUCT_SLICE(
        cnt, NULL, create_type_ptr(cnt, cnt->builtin_types->t_TypeInfoFn_ptr));
    cnt->builtin_types->is_rtti_ready = true;
    return NULL;
#undef LOOKUP_TYPE
}

ID *lookup_builtins_any(Context *cnt)
{
    if (cnt->builtin_types->is_any_ready) return NULL;
    cnt->builtin_types->t_Any = lookup_builtin_type(cnt, MIR_BUILTIN_ID_ANY);
    if (!cnt->builtin_types->t_Any) {
        return &builtin_ids[MIR_BUILTIN_ID_ANY];
    }
    cnt->builtin_types->t_Any_ptr    = create_type_ptr(cnt, cnt->builtin_types->t_Any);
    cnt->builtin_types->is_any_ready = true;
    return NULL;
}

ID *lookup_builtins_test_cases(Context *cnt)
{
    if (cnt->builtin_types->is_test_cases_ready) return NULL;
    cnt->builtin_types->t_TestCase = lookup_builtin_type(cnt, MIR_BUILTIN_ID_TYPE_TEST_CASES);
    if (!cnt->builtin_types->t_TestCase) {
        return &builtin_ids[MIR_BUILTIN_ID_TYPE_TEST_CASES];
    }
    cnt->builtin_types->t_TestCases_slice =
        CREATE_TYPE_STRUCT_SLICE(cnt, NULL, create_type_ptr(cnt, cnt->builtin_types->t_TestCase));
    return NULL;
}

ID *lookup_builtins_code_loc(Context *cnt)
{
    if (cnt->builtin_types->t_CodeLocation) return NULL;
    cnt->builtin_types->t_CodeLocation =
        lookup_builtin_type(cnt, MIR_BUILTIN_ID_TYPE_CALL_LOCATION);
    if (!cnt->builtin_types->t_CodeLocation) {
        return &builtin_ids[MIR_BUILTIN_ID_TYPE_CALL_LOCATION];
    }
    cnt->builtin_types->t_CodeLocation_ptr =
        create_type_ptr(cnt, cnt->builtin_types->t_CodeLocation);
    return NULL;
}

ScopeEntry *lookup_composit_member(MirType *type, ID *rid, MirType **out_base_type)
{
    BL_ASSERT(type);
    BL_ASSERT(mir_is_composit_type(type) && "Expected composit type!");

    Scope *     scope = type->data.strct.scope;
    ScopeEntry *found = NULL;
    while (true) {
        found = scope_lookup(scope, rid, false, true, NULL);
        if (found) break;
        scope = get_base_type_scope(type);
        type  = get_base_type(type);
        if (!scope) break;
    }
    if (out_base_type) *out_base_type = type;
    return found;
}

MirVar *add_global_bool(Context *cnt, ID *id, bool is_mutable, bool v)
{
    Scope *scope = cnt->assembly->gscope;

    // 1) Create global variable.
    // 2) Create initializer block.
    // 3) Register new variable into scope.
    MirInstr *decl_var =
        append_instr_decl_var(cnt, NULL, id, scope, NULL, NULL, is_mutable, 0, MIR_BUILTIN_ID_NONE);

    MirInstrBlock *prev_block = ast_current_block(cnt);
    MirInstrBlock *block      = append_global_block(cnt, INIT_VALUE_FN_NAME);
    set_current_block(cnt, block);
    MirInstr *            init  = append_instr_const_bool(cnt, NULL, v);
    TSmallArray_InstrPtr *decls = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
    tsa_push_InstrPtr(decls, decl_var);
    append_instr_set_initializer(cnt, NULL, decls, init);
    set_current_block(cnt, prev_block);
    MirVar *var = ((MirInstrDeclVar *)decl_var)->var;
    var->entry  = register_symbol(cnt, NULL, id, scope, true);
    return var;
}

MirType *create_type_type(Context *cnt)
{
    MirType *tmp = create_type(cnt, MIR_TYPE_TYPE, &builtin_ids[MIR_BUILTIN_ID_TYPE_TYPE]);
    // NOTE: TypeType has no LLVM representation
    tmp->alignment        = __alignof(MirType *);
    tmp->size_bits        = sizeof(MirType *) * 8;
    tmp->store_size_bytes = sizeof(MirType *);
    type_init_id(cnt, tmp);
    return tmp;
}

MirType *create_type_named_scope(Context *cnt)
{
    MirType *tmp =
        create_type(cnt, MIR_TYPE_NAMED_SCOPE, &builtin_ids[MIR_BUILTIN_ID_TYPE_NAMED_SCOPE]);
    tmp->alignment        = __alignof(ScopeEntry *);
    tmp->size_bits        = sizeof(ScopeEntry *) * 8;
    tmp->store_size_bytes = sizeof(ScopeEntry *);
    type_init_id(cnt, tmp);
    return tmp;
}

MirType *create_type_null(Context *cnt, MirType *base_type)
{
    BL_ASSERT(base_type);
    MirType *tmp             = create_type(cnt, MIR_TYPE_NULL, &builtin_ids[MIR_BUILTIN_ID_NULL]);
    tmp->data.null.base_type = base_type;
    type_init_id(cnt, tmp);
    type_init_llvm_null(cnt, tmp);
    return tmp;
}

MirType *create_type_void(Context *cnt)
{
    MirType *tmp = create_type(cnt, MIR_TYPE_VOID, &builtin_ids[MIR_BUILTIN_ID_TYPE_VOID]);
    type_init_id(cnt, tmp);
    type_init_llvm_void(cnt, tmp);
    return tmp;
}

MirType *create_type_bool(Context *cnt)
{
    MirType *tmp = create_type(cnt, MIR_TYPE_BOOL, &builtin_ids[MIR_BUILTIN_ID_TYPE_BOOL]);

    type_init_id(cnt, tmp);
    type_init_llvm_bool(cnt, tmp);

    return tmp;
}

MirType *create_type_int(Context *cnt, ID *id, s32 bitcount, bool is_signed)
{
    BL_ASSERT(id);
    BL_ASSERT(bitcount > 0);
    MirType *tmp                = create_type(cnt, MIR_TYPE_INT, id);
    tmp->data.integer.bitcount  = bitcount;
    tmp->data.integer.is_signed = is_signed;

    type_init_id(cnt, tmp);
    type_init_llvm_int(cnt, tmp);

    return tmp;
}

MirType *create_type_real(Context *cnt, ID *id, s32 bitcount)
{
    BL_ASSERT(bitcount > 0);
    MirType *tmp            = create_type(cnt, MIR_TYPE_REAL, id);
    tmp->data.real.bitcount = bitcount;

    type_init_id(cnt, tmp);
    type_init_llvm_real(cnt, tmp);

    return tmp;
}

MirType *create_type_ptr(Context *cnt, MirType *src_type)
{
    BL_ASSERT(src_type && "Invalid src type for pointer type.");
    MirType *tmp       = create_type(cnt, MIR_TYPE_PTR, NULL);
    tmp->data.ptr.expr = src_type;

    type_init_id(cnt, tmp);
    type_init_llvm_ptr(cnt, tmp);

    return tmp;
}

MirType *create_type_fn(Context *           cnt,
                        ID *                id,
                        MirType *           ret_type,
                        TSmallArray_ArgPtr *args,
                        bool                is_vargs,
                        bool                has_default_args)
{
    MirType *tmp                  = create_type(cnt, MIR_TYPE_FN, id);
    tmp->data.fn.args             = args;
    tmp->data.fn.is_vargs         = is_vargs;
    tmp->data.fn.has_default_args = has_default_args;
    tmp->data.fn.ret_type         = ret_type ? ret_type : cnt->builtin_types->t_void;
    tmp->data.fn.builtin_id       = MIR_BUILTIN_ID_NONE;
    type_init_id(cnt, tmp);
    type_init_llvm_fn(cnt, tmp);
    return tmp;
}

MirType *create_type_fn_group(Context *cnt, ID *id, TSmallArray_TypePtr *variants)
{
    BL_ASSERT(variants);
    MirType *tmp                = create_type(cnt, MIR_TYPE_FN_GROUP, id);
    tmp->data.fn_group.variants = variants;
    tmp->alignment              = __alignof(MirFnGroup *);
    tmp->size_bits              = sizeof(MirFnGroup *) * 8;
    tmp->store_size_bytes       = sizeof(MirFnGroup *);
    type_init_id(cnt, tmp);
    return tmp;
}

MirType *create_type_array(Context *cnt, ID *id, MirType *elem_type, s64 len)
{
    BL_ASSERT(elem_type);
    MirType *tmp              = create_type(cnt, MIR_TYPE_ARRAY, id);
    tmp->data.array.elem_type = elem_type;
    tmp->data.array.len       = len;
    type_init_id(cnt, tmp);
    type_init_llvm_array(cnt, tmp);
    return tmp;
}

MirType *create_type_struct(Context *              cnt,
                            MirTypeKind            kind,
                            ID *                   id, // optional
                            Scope *                scope,
                            TSmallArray_MemberPtr *members,   // MirMember
                            MirType *              base_type, // optional
                            bool                   is_union,
                            bool                   is_packed,
                            bool                   is_multiple_return_type)
{
    MirType *tmp                            = create_type(cnt, kind, id);
    tmp->data.strct.members                 = members;
    tmp->data.strct.scope                   = scope;
    tmp->data.strct.is_packed               = is_packed;
    tmp->data.strct.is_union                = is_union;
    tmp->data.strct.is_multiple_return_type = is_multiple_return_type;
    tmp->data.strct.base_type               = base_type;
    type_init_id(cnt, tmp);
    type_init_llvm_struct(cnt, tmp);
    return tmp;
}

MirType *complete_type_struct(Context *              cnt,
                              MirInstr *             fwd_decl,
                              Scope *                scope,
                              TSmallArray_MemberPtr *members,
                              MirType *              base_type,
                              bool                   is_packed,
                              bool                   is_union,
                              bool                   is_multiple_return_type)
{
    BL_ASSERT(fwd_decl && "Invalid fwd_decl pointer!");

    BL_ASSERT(fwd_decl->value.type->kind == MIR_TYPE_TYPE &&
              "Forward struct declaration does not point to type definition!");

    MirType *incomplete_type = MIR_CEV_READ_AS(MirType *, &fwd_decl->value);
    BL_MAGIC_ASSERT(incomplete_type);
    BL_ASSERT(incomplete_type->kind == MIR_TYPE_STRUCT && "Incomplete type is not struct type!");
    BL_ASSERT(incomplete_type->data.strct.is_incomplete &&
              "Incomplete struct type is not marked as incomplete!");

    incomplete_type->data.strct.members                 = members;
    incomplete_type->data.strct.scope                   = scope;
    incomplete_type->data.strct.is_incomplete           = false;
    incomplete_type->data.strct.base_type               = base_type;
    incomplete_type->data.strct.is_packed               = is_packed;
    incomplete_type->data.strct.is_union                = is_union;
    incomplete_type->data.strct.is_multiple_return_type = is_multiple_return_type;

#if TRACY_ENABLE
    {
        char type_name[256];
        mir_type_to_str(type_name, 256, incomplete_type, true);
        BL_TRACY_MESSAGE("COMPLETE_TYPE", "%s", type_name);
    }
#endif
    type_init_llvm_struct(cnt, incomplete_type);
    return incomplete_type;
}

MirType *create_type_struct_incomplete(Context *cnt, ID *user_id, bool is_union)
{
    MirType *type                  = create_type(cnt, MIR_TYPE_STRUCT, user_id);
    type->data.strct.is_incomplete = true;
    type->data.strct.is_union      = is_union;

    type_init_id(cnt, type);
    type_init_llvm_struct(cnt, type);
    return type;
}

MirType *_create_type_struct_slice(Context *cnt, MirTypeKind kind, ID *id, MirType *elem_ptr_type)
{
    BL_ASSERT(mir_is_pointer_type(elem_ptr_type));
    BL_ASSERT(kind == MIR_TYPE_STRING || kind == MIR_TYPE_VARGS || kind == MIR_TYPE_SLICE);

    TSmallArray_MemberPtr *members = create_sarr(TSmallArray_MemberPtr, cnt->assembly);

    // Slice layout struct { s64, *T }
    Scope *body_scope = scope_create(
        &cnt->assembly->arenas.scope, SCOPE_TYPE_STRUCT, cnt->assembly->gscope, 2, NULL);

    MirMember *tmp;
    tmp = create_member(
        cnt, NULL, &builtin_ids[MIR_BUILTIN_ID_ARR_LEN], 0, cnt->builtin_types->t_s64);

    tsa_push_MemberPtr(members, tmp);
    provide_builtin_member(cnt, body_scope, tmp);

    tmp = create_member(cnt, NULL, &builtin_ids[MIR_BUILTIN_ID_ARR_PTR], 1, elem_ptr_type);

    tsa_push_MemberPtr(members, tmp);
    provide_builtin_member(cnt, body_scope, tmp);

    return create_type_struct(cnt, kind, id, body_scope, members, NULL, false, false, false);
}

MirType *create_type_struct_dynarr(Context *cnt, ID *id, MirType *elem_ptr_type)
{
    BL_ASSERT(mir_is_pointer_type(elem_ptr_type));

    TSmallArray_MemberPtr *members = create_sarr(TSmallArray_MemberPtr, cnt->assembly);

    // Dynamic array layout struct { s64, *T, usize, allocator }
    Scope *body_scope = scope_create(
        &cnt->assembly->arenas.scope, SCOPE_TYPE_STRUCT, cnt->assembly->gscope, 2, NULL);

    MirMember *tmp;
    { // .len
        tmp = create_member(
            cnt, NULL, &builtin_ids[MIR_BUILTIN_ID_ARR_LEN], 0, cnt->builtin_types->t_s64);

        tsa_push_MemberPtr(members, tmp);
        provide_builtin_member(cnt, body_scope, tmp);
    }

    { // .ptr
        tmp = create_member(cnt, NULL, &builtin_ids[MIR_BUILTIN_ID_ARR_PTR], 1, elem_ptr_type);

        tsa_push_MemberPtr(members, tmp);
        provide_builtin_member(cnt, body_scope, tmp);
    }

    { // .allocated
        tmp = create_member(
            cnt, NULL, &builtin_ids[MIR_BUILTIN_ID_ARR_ALLOCATED], 2, cnt->builtin_types->t_usize);

        tsa_push_MemberPtr(members, tmp);
        provide_builtin_member(cnt, body_scope, tmp);
    }

    { // .allocator
        tmp =
            create_member(cnt, NULL, &builtin_ids[MIR_BUILTIN_ID_ARR_ALLOCATOR], 3, elem_ptr_type);

        tsa_push_MemberPtr(members, tmp);
        provide_builtin_member(cnt, body_scope, tmp);
    }

    return create_type_struct(
        cnt, MIR_TYPE_DYNARR, id, body_scope, members, NULL, false, false, false);
}

MirType *create_type_enum(Context *               cnt,
                          ID *                    id,
                          Scope *                 scope,
                          MirType *               base_type,
                          TSmallArray_VariantPtr *variants)
{
    BL_ASSERT(base_type);
    MirType *tmp            = create_type(cnt, MIR_TYPE_ENUM, id);
    tmp->data.enm.scope     = scope;
    tmp->data.enm.base_type = base_type;
    tmp->data.enm.variants  = variants;

    type_init_id(cnt, tmp);
    type_init_llvm_enum(cnt, tmp);

    return tmp;
}

void type_init_llvm_int(Context *cnt, MirType *type)
{
    type->llvm_type =
        LLVMIntTypeInContext(cnt->assembly->llvm.cnt, (unsigned int)type->data.integer.bitcount);
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
    type->alignment        = LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);
}

void type_init_llvm_real(Context *cnt, MirType *type)
{
    if (type->data.real.bitcount == 32)
        type->llvm_type = LLVMFloatTypeInContext(cnt->assembly->llvm.cnt);
    else if (type->data.real.bitcount == 64)
        type->llvm_type = LLVMDoubleTypeInContext(cnt->assembly->llvm.cnt);
    else
        BL_ABORT("invalid floating point type");

    type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s32)LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);
}

void type_init_llvm_ptr(Context *cnt, MirType *type)
{
    MirType *tmp = mir_deref_type(type);
    // Pointer to Type has no LLVM representation and cannot not be generated into IR.
    if (!mir_type_has_llvm_representation(tmp)) return;
    BL_ASSERT(tmp);
    BL_ASSERT(tmp->llvm_type);
    type->llvm_type        = LLVMPointerType(tmp->llvm_type, 0);
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s32)LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);
}

void type_init_llvm_void(Context *cnt, MirType *type)
{
    type->alignment        = 0;
    type->size_bits        = 0;
    type->store_size_bytes = 0;
    type->llvm_type        = LLVMVoidTypeInContext(cnt->assembly->llvm.cnt);
}

void type_init_llvm_null(Context UNUSED(*cnt), MirType *type)
{
    MirType *tmp = type->data.null.base_type;
    BL_ASSERT(tmp);
    BL_ASSERT(tmp->llvm_type);
    type->llvm_type        = tmp->llvm_type;
    type->alignment        = tmp->alignment;
    type->size_bits        = tmp->size_bits;
    type->store_size_bytes = tmp->store_size_bytes;
}

void type_init_llvm_bool(Context *cnt, MirType *type)
{
    type->llvm_type        = LLVMIntTypeInContext(cnt->assembly->llvm.cnt, 1);
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s32)LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);
}

static INLINE usize struct_split_fit(Context *cnt, MirType *struct_type, u32 bound, u32 *start)
{
    s64 so     = vm_get_struct_elem_offset(cnt->assembly, struct_type, *start);
    u32 offset = 0;
    u32 size   = 0;
    u32 total  = 0;
    for (; *start < struct_type->data.strct.members->size; ++(*start)) {
        offset = (u32)vm_get_struct_elem_offset(cnt->assembly, struct_type, *start) - (u32)so;
        size   = (u32)mir_get_struct_elem_type(struct_type, *start)->store_size_bytes;
        if (offset + size > bound) return bound;
        total = offset + size;
    }

    return total > 1 ? next_pow_2((u32)total) : total;
}

void type_init_llvm_fn(Context *cnt, MirType *type)
{
    MirType *ret_type = type->data.fn.ret_type;

    LLVMTypeRef         llvm_ret  = NULL;
    TSmallArray_ArgPtr *args      = type->data.fn.args;
    const bool          has_args  = args;
    const bool          has_ret   = ret_type;
    bool                has_byval = false;

    BL_ASSERT(ret_type);
    if (has_ret && ret_type->kind == MIR_TYPE_TYPE) {
        return;
    }

    TSmallArray_LLVMType llvm_args;
    tsa_init(&llvm_args);

    if (has_ret) {
        if (cnt->assembly->target->reg_split && mir_is_composit_type(ret_type) &&
            ret_type->store_size_bytes >= 16) {
            type->data.fn.has_sret = true;
            tsa_push_LLVMType(&llvm_args, LLVMPointerType(ret_type->llvm_type, 0));
            llvm_ret = LLVMVoidTypeInContext(cnt->assembly->llvm.cnt);
        } else {
            llvm_ret = ret_type->llvm_type;
        }
    } else {
        llvm_ret = LLVMVoidTypeInContext(cnt->assembly->llvm.cnt);
    }

    BL_ASSERT(llvm_ret);

    if (has_args) {
        MirArg *arg;
        TSA_FOREACH(args, arg)
        {
            arg->llvm_index = (u32)llvm_args.size;

            // Composit types.
            if (cnt->assembly->target->reg_split && mir_is_composit_type(arg->type)) {
                LLVMContextRef llvm_cnt = cnt->assembly->llvm.cnt;

                u32   start = 0;
                usize low   = 0;
                usize high  = 0;

                if (!has_byval) has_byval = true;

                low = struct_split_fit(cnt, arg->type, sizeof(usize), &start);

                if (start < arg->type->data.strct.members->size)
                    high = struct_split_fit(cnt, arg->type, sizeof(usize), &start);

                if (start < arg->type->data.strct.members->size) {
                    arg->llvm_easgm = LLVM_EASGM_BYVAL;

                    BL_ASSERT(arg->type->llvm_type);
                    tsa_push_LLVMType(&llvm_args, LLVMPointerType(arg->type->llvm_type, 0));
                } else {
                    switch (low) {
                    case 1:
                        arg->llvm_easgm = LLVM_EASGM_8;
                        tsa_push_LLVMType(&llvm_args, LLVMInt8TypeInContext(llvm_cnt));
                        break;
                    case 2:
                        arg->llvm_easgm = LLVM_EASGM_16;
                        tsa_push_LLVMType(&llvm_args, LLVMInt16TypeInContext(llvm_cnt));
                        break;
                    case 4:
                        arg->llvm_easgm = LLVM_EASGM_32;
                        tsa_push_LLVMType(&llvm_args, LLVMInt32TypeInContext(llvm_cnt));
                        break;
                    case 8: {
                        switch (high) {
                        case 0:
                            arg->llvm_easgm = LLVM_EASGM_64;
                            tsa_push_LLVMType(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                            break;
                        case 1:
                            arg->llvm_easgm = LLVM_EASGM_64_8;
                            tsa_push_LLVMType(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                            tsa_push_LLVMType(&llvm_args, LLVMInt8TypeInContext(llvm_cnt));
                            break;
                        case 2:
                            arg->llvm_easgm = LLVM_EASGM_64_16;
                            tsa_push_LLVMType(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                            tsa_push_LLVMType(&llvm_args, LLVMInt16TypeInContext(llvm_cnt));
                            break;
                        case 4:
                            arg->llvm_easgm = LLVM_EASGM_64_32;
                            tsa_push_LLVMType(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                            tsa_push_LLVMType(&llvm_args, LLVMInt32TypeInContext(llvm_cnt));
                            break;
                        case 8:
                            arg->llvm_easgm = LLVM_EASGM_64_64;
                            tsa_push_LLVMType(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                            tsa_push_LLVMType(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
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
                tsa_push_LLVMType(&llvm_args, arg->type->llvm_type);
            }
        }
    }

    type->llvm_type = LLVMFunctionType(llvm_ret, llvm_args.data, (unsigned)llvm_args.size, false);
    type->alignment = __alignof(MirFn *);
    type->size_bits = sizeof(MirFn *) * 8;
    type->store_size_bytes  = sizeof(MirFn *);
    type->data.fn.has_byval = has_byval;

    tsa_terminate(&llvm_args);
}

void type_init_llvm_array(Context *cnt, MirType *type)
{
    LLVMTypeRef llvm_elem_type = type->data.array.elem_type->llvm_type;
    BL_ASSERT(llvm_elem_type);
    const unsigned int len = (const unsigned int)type->data.array.len;

    type->llvm_type        = LLVMArrayType(llvm_elem_type, len);
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s32)LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);
}

void type_init_llvm_struct(Context *cnt, MirType *type)
{
    if (type->data.strct.is_incomplete) {
        BL_ASSERT(type->user_id && "Missing user id for incomplete struct type.");
        type->llvm_type = LLVMStructCreateNamed(cnt->assembly->llvm.cnt, type->user_id->str);
        return;
    }

    TSmallArray_MemberPtr *members = type->data.strct.members;
    BL_ASSERT(members);

    const bool  is_packed = type->data.strct.is_packed;
    const bool  is_union  = type->data.strct.is_union;
    const usize memc      = members->size;
    BL_ASSERT(memc > 0);

    TSmallArray_LLVMType llvm_members;
    tsa_init(&llvm_members);

    // When structure is union we have to find biggest member and use only found one since LLVM
    // has no explicit union representation. We select biggest one. We have to consider better
    // selection later due to correct union alignment.
    MirMember *union_member = NULL;

    MirMember *member;
    TSA_FOREACH(members, member)
    {
        BL_ASSERT(member->type->llvm_type);

        if (is_union) {
            if (!union_member) {
                union_member = member;
                continue;
            }

            if (member->type->store_size_bytes > union_member->type->store_size_bytes)
                union_member = member;
        } else {
            tsa_push_LLVMType(&llvm_members, member->type->llvm_type);
        }
    }

    if (union_member) tsa_push_LLVMType(&llvm_members, union_member->type->llvm_type);

    // named structure type
    if (type->user_id) {
        if (type->llvm_type == NULL) {
            // Create new named type only if it's not already created (by incomplete
            // type declaration).
            type->llvm_type = LLVMStructCreateNamed(cnt->assembly->llvm.cnt, type->user_id->str);
        }

        LLVMStructSetBody(
            type->llvm_type, llvm_members.data, (unsigned)llvm_members.size, is_packed);
    } else {
        type->llvm_type = LLVMStructTypeInContext(
            cnt->assembly->llvm.cnt, llvm_members.data, (unsigned)llvm_members.size, is_packed);
    }

    type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s32)LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);

    tsa_terminate(&llvm_members);

    // set offsets for members
    TSA_FOREACH(members, member)
    {
        // Note: Union members has 0 offset.
        member->offset_bytes = (s32)vm_get_struct_elem_offset(cnt->assembly, type, (u32)i);
    }
}

void type_init_llvm_enum(Context *cnt, MirType *type)
{
    MirType *base_type = type->data.enm.base_type;
    BL_ASSERT(base_type->kind == MIR_TYPE_INT);
    LLVMTypeRef llvm_base_type = base_type->llvm_type;
    BL_ASSERT(llvm_base_type);

    type->llvm_type        = llvm_base_type;
    type->size_bits        = LLVMSizeOfTypeInBits(cnt->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(cnt->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s32)LLVMABIAlignmentOfType(cnt->assembly->llvm.TD, type->llvm_type);
}

static INLINE void push_var(Context *cnt, MirVar *var)
{
    BL_ASSERT(var);
    if (var->is_global) return;
    MirFn *fn = ast_current_fn(cnt);
    BL_ASSERT(fn);
    tarray_push(fn->variables, var);
}

MirVar *create_var(Context *        cnt,
                   Ast *            decl_node,
                   Scope *          scope,
                   ID *             id,
                   MirType *        alloc_type,
                   bool             is_mutable,
                   bool             is_global,
                   bool             is_comptime,
                   u32              flags,
                   MirBuiltinIdKind builtin_id)
{
    BL_ASSERT(id);
    MirVar *tmp            = arena_alloc(&cnt->assembly->arenas.mir.var);
    tmp->value.type        = alloc_type;
    tmp->value.is_comptime = is_comptime;
    tmp->id                = id;
    tmp->decl_scope        = scope;
    tmp->decl_node         = decl_node;
    tmp->is_mutable        = is_mutable;
    tmp->is_global         = is_global;
    tmp->linkage_name      = id->str;
    tmp->flags             = flags;
    tmp->emit_llvm         = true;
    tmp->builtin_id        = builtin_id;
    push_var(cnt, tmp);
    return tmp;
}

MirVar *create_var_impl(Context *   cnt,
                        const char *name,
                        MirType *   alloc_type,
                        bool        is_mutable,
                        bool        is_global,
                        bool        is_comptime)
{
    BL_ASSERT(name);
    MirVar *tmp            = arena_alloc(&cnt->assembly->arenas.mir.var);
    tmp->value.type        = alloc_type;
    tmp->value.is_comptime = is_comptime;
    tmp->is_mutable        = is_mutable;
    tmp->is_global         = is_global;
    tmp->ref_count         = 1;
    tmp->linkage_name      = name;
    tmp->is_implicit       = true;
    tmp->emit_llvm         = true;
    push_var(cnt, tmp);
    return tmp;
}

MirFn *create_fn(Context *        cnt,
                 Ast *            node,
                 ID *             id,
                 const char *     linkage_name,
                 u32              flags,
                 MirInstrFnProto *prototype,
                 bool             emit_llvm,
                 bool             is_global,
                 MirBuiltinIdKind builtin_id)
{
    MirFn *tmp = arena_alloc(&cnt->assembly->arenas.mir.fn);
    BL_MAGIC_SET(tmp);
    tmp->variables    = create_arr(cnt->assembly, sizeof(MirVar *));
    tmp->linkage_name = linkage_name;
    tmp->id           = id;
    tmp->flags        = flags;
    tmp->decl_node    = node;
    tmp->prototype    = &prototype->base;
    tmp->emit_llvm    = emit_llvm;
    tmp->is_global    = is_global;
    tmp->builtin_id   = builtin_id;
    return tmp;
}

MirFnGroup *create_fn_group(Context *cnt, Ast *decl_node, TSmallArray_FnPtr *variants)
{
    BL_ASSERT(decl_node);
    BL_ASSERT(variants);
    MirFnGroup *tmp = arena_alloc(&cnt->assembly->arenas.mir.fn_group);
    BL_MAGIC_SET(tmp);
    tmp->decl_node = decl_node;
    tmp->variants  = variants;
    return tmp;
}

MirMember *create_member(Context *cnt, Ast *node, ID *id, s64 index, MirType *type)
{
    MirMember *tmp = arena_alloc(&cnt->assembly->arenas.mir.member);
    BL_MAGIC_SET(tmp);
    tmp->decl_node = node;
    tmp->id        = id;
    tmp->index     = index;
    tmp->type      = type;
    return tmp;
}

MirArg *create_arg(Context *cnt, Ast *node, ID *id, Scope *scope, MirType *type, MirInstr *value)
{
    MirArg *tmp     = arena_alloc(&cnt->assembly->arenas.mir.arg);
    tmp->decl_node  = node;
    tmp->id         = id;
    tmp->type       = type;
    tmp->decl_scope = scope;
    tmp->value      = value;
    return tmp;
}

MirVariant *create_variant(Context *cnt, ID *id, MirConstExprValue *value)
{
    MirVariant *tmp = arena_alloc(&cnt->assembly->arenas.mir.variant);
    tmp->id         = id;
    tmp->value      = value;
    return tmp;
}

// instructions
void append_current_block(Context *cnt, MirInstr *instr)
{
    BL_ASSERT(instr);
    MirInstrBlock *block = ast_current_block(cnt);
    BL_ASSERT(block);

    if (is_block_terminated(block)) {
        // Append this instruction into unreachable block if current block was terminated
        // already. Unreachable block will never be generated into LLVM and compiler can
        // complain later about this and give hit to the user.
        block = append_block(cnt, block->owner_fn, ".unreachable");
        set_current_block(cnt, block);
    }

    instr->owner_block = block;
    instr->prev        = block->last_instr;

    if (!block->entry_instr) block->entry_instr = instr;
    if (instr->prev) instr->prev->next = instr;
    block->last_instr = instr;
}

MirInstr *insert_instr_cast(Context *cnt, MirInstr *src, MirType *to_type)
{
    MirInstrCast *tmp     = create_instr(cnt, MIR_INSTR_CAST, src->node);
    tmp->base.value.type  = to_type;
    tmp->base.is_implicit = true;
    tmp->expr             = src;
    tmp->op               = MIR_CAST_INVALID;
    ref_instr(&tmp->base);

    insert_instr_after(src, &tmp->base);
    return &tmp->base;
}

MirInstr *insert_instr_addrof(Context *cnt, MirInstr *src)
{
    MirInstr *tmp    = create_instr_addrof(cnt, src->node, src);
    tmp->is_implicit = true;

    insert_instr_after(src, tmp);
    return tmp;
}

MirInstr *insert_instr_toany(Context *cnt, MirInstr *expr)
{
    BL_ASSERT(cnt->builtin_types->is_any_ready &&
              "All 'Any' related types must be ready before this!");

    MirInstrToAny *tmp    = create_instr(cnt, MIR_INSTR_TOANY, expr->node);
    tmp->base.value.type  = cnt->builtin_types->t_Any_ptr;
    tmp->base.is_implicit = true;
    tmp->expr             = expr;
    ref_instr(&tmp->base);

    insert_instr_after(expr, &tmp->base);
    return &tmp->base;
}

MirInstr *insert_instr_load(Context *cnt, MirInstr *src)
{
    BL_ASSERT(src);
    BL_ASSERT(src->value.type);
    BL_ASSERT(src->value.type->kind == MIR_TYPE_PTR);
    MirInstrLoad *tmp     = create_instr(cnt, MIR_INSTR_LOAD, src->node);
    tmp->base.is_implicit = true;
    tmp->src              = src;

    ref_instr(&tmp->base);
    insert_instr_after(src, &tmp->base);

    return &tmp->base;
}

MirCastOp get_cast_op(MirType *from, MirType *to)
{
    BL_ASSERT(from);
    BL_ASSERT(to);
    const usize fsize = from->store_size_bytes;
    const usize tsize = to->store_size_bytes;

    if (type_cmp(from, to)) return MIR_CAST_NONE;

#ifndef _MSC_VER
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
#endif

    switch (from->kind) {
    case MIR_TYPE_ENUM:
        // from enum
        from = from->data.enm.base_type;
    case MIR_TYPE_INT: {
        // from integer
        switch (to->kind) {
        case MIR_TYPE_ENUM:
            to = to->data.enm.base_type;
        case MIR_TYPE_INT: {
            // to integer
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
            // to ptr
            return MIR_CAST_INTTOPTR;
        }

        default:
            return MIR_CAST_INVALID;
        }
    }

    case MIR_TYPE_PTR: {
        // from pointer
        switch (to->kind) {
        case MIR_TYPE_PTR: {
            // to pointer
            return MIR_CAST_BITCAST;
        }

        case MIR_TYPE_INT: {
            // to int
            return MIR_CAST_PTRTOINT;
        }

        case MIR_TYPE_BOOL: {
            // to bool
            return MIR_CAST_PTRTOBOOL;
        }

        default:
            return MIR_CAST_INVALID;
        }
    }

    case MIR_TYPE_REAL: {
        // from real
        switch (to->kind) {
        case MIR_TYPE_INT: {
            // to integer
            const bool is_to_signed = to->data.integer.is_signed;
            return is_to_signed ? MIR_CAST_FPTOSI : MIR_CAST_FPTOUI;
        }

        case MIR_TYPE_REAL: {
            // to integer
            if (fsize < tsize) {
                return MIR_CAST_FPEXT;
            } else {
                return MIR_CAST_FPTRUNC;
            }
        }

        default:
            return MIR_CAST_INVALID;
        }
    }

    case MIR_TYPE_VARGS: {
        return to->kind == MIR_TYPE_SLICE ? MIR_CAST_NONE : MIR_CAST_INVALID;
    }

    default:
        return MIR_CAST_INVALID;
    }

#ifndef _MSC_VER
#pragma GCC diagnostic pop
#endif
}

static u64 _id_counter = 1;

void *create_instr(Context *cnt, MirInstrKind kind, Ast *node)
{
    MirInstr *tmp   = arena_alloc(&cnt->assembly->arenas.mir.instr);
    tmp->value.data = (VMStackPtr)&tmp->value._tmp;
    tmp->kind       = kind;
    tmp->node       = node;
    tmp->id         = _id_counter++;
#if BL_DEBUG && defined(TRACY_ENABLE)
    static int ic = 0;
    TracyCPlot("INSTR", ++ic);
    BL_TRACY_MESSAGE("INSTR_CREATE", "size: %lluB", (unsigned long long)SIZEOF_MIR_INSTR);
#endif
    return tmp;
}

MirInstr *create_instr_call_comptime(Context *cnt, Ast *node, MirInstr *fn)
{
    BL_ASSERT(fn && fn->kind == MIR_INSTR_FN_PROTO);
    MirInstrCall *tmp           = create_instr(cnt, MIR_INSTR_CALL, node);
    tmp->base.value.addr_mode   = MIR_VAM_LVALUE_CONST;
    tmp->base.value.is_comptime = true;
    tmp->base.ref_count         = 2;
    tmp->callee                 = ref_instr(fn);
    return &tmp->base;
}

MirInstr *create_instr_compound(Context *             cnt,
                                Ast *                 node,
                                MirInstr *            type,
                                TSmallArray_InstrPtr *values,
                                bool                  is_multiple_return_value)
{
    if (values) {
        MirInstr *value;
        TSA_FOREACH(values, value) ref_instr(value);
    }
    MirInstrCompound *tmp         = create_instr(cnt, MIR_INSTR_COMPOUND, node);
    tmp->base.value.addr_mode     = MIR_VAM_RVALUE;
    tmp->type                     = ref_instr(type);
    tmp->values                   = values;
    tmp->is_naked                 = true;
    tmp->is_multiple_return_value = is_multiple_return_value;
    return &tmp->base;
}

MirInstr *
create_instr_compound_impl(Context *cnt, Ast *node, MirType *type, TSmallArray_InstrPtr *values)
{
    MirInstr *tmp    = create_instr_compound(cnt, node, NULL, values, false);
    tmp->value.type  = type;
    tmp->is_implicit = true;
    return tmp;
}

MirInstr *create_instr_type_info(Context *cnt, Ast *node, MirInstr *expr)
{
    MirInstrTypeInfo *tmp = create_instr(cnt, MIR_INSTR_TYPE_INFO, node);
    tmp->expr             = ref_instr(expr);
    return &tmp->base;
}

MirInstr *create_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index)
{
    BL_ASSERT(arr_ptr && index);
    MirInstrElemPtr *tmp = create_instr(cnt, MIR_INSTR_ELEM_PTR, node);
    tmp->arr_ptr         = ref_instr(arr_ptr);
    tmp->index           = ref_instr(index);
    return &tmp->base;
}

MirInstr *create_instr_member_ptr(Context *        cnt,
                                  Ast *            node,
                                  MirInstr *       target_ptr,
                                  Ast *            member_ident,
                                  ScopeEntry *     scope_entry,
                                  MirBuiltinIdKind builtin_id)
{
    MirInstrMemberPtr *tmp = create_instr(cnt, MIR_INSTR_MEMBER_PTR, node);
    tmp->target_ptr        = ref_instr(target_ptr);
    tmp->member_ident      = member_ident;
    tmp->scope_entry       = scope_entry;
    tmp->builtin_id        = builtin_id;
    return &tmp->base;
}

MirInstr *create_instr_addrof(Context *cnt, Ast *node, MirInstr *src)
{
    MirInstrAddrOf *tmp = create_instr(cnt, MIR_INSTR_ADDROF, node);
    tmp->src            = ref_instr(src);
    return &tmp->base;
}

MirInstr *create_instr_decl_direct_ref(Context *cnt, MirInstr *ref)
{
    BL_ASSERT(ref);
    MirInstrDeclDirectRef *tmp = create_instr(cnt, MIR_INSTR_DECL_DIRECT_REF, NULL);
    tmp->ref                   = ref_instr(ref);
    return &tmp->base;
}

MirInstr *create_instr_const_int(Context *cnt, Ast *node, MirType *type, u64 val)
{
    MirInstrConst *tmp          = create_instr(cnt, MIR_INSTR_CONST, node);
    tmp->base.value.type        = type;
    tmp->base.value.addr_mode   = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime = true;
    tmp->volatile_type          = true;
    MIR_CEV_WRITE_AS(u64, &tmp->base.value, val);
    return &tmp->base;
}

MirInstr *create_instr_const_type(Context *cnt, Ast *node, MirType *type)
{
    MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node);
    tmp->value.type        = cnt->builtin_types->t_type;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    tmp->value.is_comptime = true;
    MIR_CEV_WRITE_AS(MirType *, &tmp->value, type);
    return tmp;
}

MirInstr *create_instr_const_float(Context *cnt, Ast *node, float val)
{
    MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = cnt->builtin_types->t_f32;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    MIR_CEV_WRITE_AS(float, &tmp->value, val);
    return tmp;
}

MirInstr *create_instr_const_double(Context *cnt, Ast *node, double val)
{
    MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = cnt->builtin_types->t_f64;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    MIR_CEV_WRITE_AS(double, &tmp->value, val);
    return tmp;
}

MirInstr *create_instr_const_bool(Context *cnt, Ast *node, bool val)
{
    MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node);
    tmp->value.type        = cnt->builtin_types->t_bool;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    tmp->value.is_comptime = true;
    MIR_CEV_WRITE_AS(bool, &tmp->value, val);
    return tmp;
}

MirInstr *create_instr_const_ptr(Context *cnt, Ast *node, MirType *type, VMStackPtr ptr)
{
    BL_ASSERT(mir_is_pointer_type(type) && "Expected pointer type!");
    MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = type;
    tmp->value.addr_mode   = MIR_VAM_LVALUE_CONST;
    MIR_CEV_WRITE_AS(VMStackPtr, &tmp->value, ptr);
    return tmp;
}

MirInstr *create_instr_call_loc(Context *cnt, Ast *node, Location *call_location)
{
    MirInstrCallLoc *tmp        = create_instr(cnt, MIR_INSTR_CALL_LOC, node);
    tmp->base.value.addr_mode   = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime = true;
    tmp->call_location          = call_location;
    return &tmp->base;
}

MirInstrBlock *create_block(Context *cnt, const char *name)
{
    BL_ASSERT(name);
    MirInstrBlock *tmp   = create_instr(cnt, MIR_INSTR_BLOCK, NULL);
    tmp->base.value.type = cnt->builtin_types->t_void;
    tmp->name            = name;
    tmp->emit_llvm       = true;
    tmp->owner_fn        = NULL;
    return tmp;
}

MirInstrBlock *append_block2(Context UNUSED(*cnt), MirFn *fn, MirInstrBlock *block)
{
    BL_ASSERT(block && fn);
    BL_ASSERT(!block->owner_fn && "Block is already appended to function!");
    block->owner_fn = fn;
    if (!fn->first_block) {
        fn->first_block = block;
        // first block is referenced every time!!!
        ref_instr(&block->base);
    }
    block->base.prev = &fn->last_block->base;
    if (fn->last_block) fn->last_block->base.next = &block->base;
    fn->last_block = block;
    return block;
}

MirInstrBlock *append_block(Context *cnt, MirFn *fn, const char *name)
{
    BL_ASSERT(fn && name);
    MirInstrBlock *tmp = create_block(cnt, name);
    append_block2(cnt, fn, tmp);
    return tmp;
}

MirInstrBlock *append_global_block(Context *cnt, const char *name)
{
    MirInstrBlock *tmp          = create_instr(cnt, MIR_INSTR_BLOCK, NULL);
    tmp->base.value.type        = cnt->builtin_types->t_void;
    tmp->base.value.is_comptime = true;
    tmp->name                   = name;
    tmp->emit_llvm              = true;
    ref_instr(&tmp->base);
    push_into_gscope(cnt, &tmp->base);
    analyze_push_back(cnt, &tmp->base);
    return tmp;
}

MirInstr *
append_instr_set_initializer(Context *cnt, Ast *node, TSmallArray_InstrPtr *dests, MirInstr *src)
{
    MirInstrSetInitializer *tmp = create_instr(cnt, MIR_INSTR_SET_INITIALIZER, node);
    tmp->base.value.type        = cnt->builtin_types->t_void;
    tmp->base.value.is_comptime = true;
    tmp->base.ref_count         = NO_REF_COUNTING;
    tmp->dests                  = dests;
    tmp->src                    = ref_instr(src);
    MirInstr *dest;
    TSA_FOREACH(tmp->dests, dest)
    {
        ref_instr(dest);
    }
    append_current_block(cnt, &tmp->base);
    MirInstrBlock *block = ast_current_block(cnt);
    if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
    return &tmp->base;
}

MirInstr *
append_instr_set_initializer_impl(Context *cnt, TSmallArray_InstrPtr *dests, MirInstr *src)
{
    MirInstr *tmp    = append_instr_set_initializer(cnt, NULL, dests, src);
    tmp->is_implicit = true;
    return tmp;
}

MirInstr *
append_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, TSmallArray_InstrPtr *args)
{
    MirInstrTypeFn *tmp         = create_instr(cnt, MIR_INSTR_TYPE_FN, node);
    tmp->base.value.type        = cnt->builtin_types->t_type;
    tmp->base.value.addr_mode   = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime = true;
    tmp->ret_type               = ret_type;
    tmp->args                   = args;
    if (args) {
        MirInstr *it;
        TSA_FOREACH(args, it)
        {
            ref_instr(it);
        }
    }
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *
append_instr_type_fn_group(Context *cnt, Ast *node, ID *id, TSmallArray_InstrPtr *variants)
{
    BL_ASSERT(variants);
    MirInstrTypeFnGroup *tmp    = create_instr(cnt, MIR_INSTR_TYPE_FN_GROUP, node);
    tmp->base.value.type        = cnt->builtin_types->t_type;
    tmp->base.value.addr_mode   = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime = true;
    tmp->variants               = variants;
    tmp->id                     = id;

    MirInstr *it;
    TSA_FOREACH(variants, it)
    {
        ref_instr(it);
    }

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_type_struct(Context *             cnt,
                                   Ast *                 node,
                                   ID *                  id,
                                   MirInstr *            fwd_decl,
                                   Scope *               scope,
                                   TSmallArray_InstrPtr *members,
                                   bool                  is_packed,
                                   bool                  is_union,
                                   bool                  is_multiple_return_type)
{
    MirInstrTypeStruct *tmp      = create_instr(cnt, MIR_INSTR_TYPE_STRUCT, node);
    tmp->base.value.type         = cnt->builtin_types->t_type;
    tmp->base.value.is_comptime  = true;
    tmp->base.value.addr_mode    = MIR_VAM_RVALUE;
    tmp->members                 = members;
    tmp->scope                   = scope;
    tmp->is_packed               = is_packed;
    tmp->is_union                = is_union;
    tmp->is_multiple_return_type = is_multiple_return_type;

    tmp->id       = id;
    tmp->fwd_decl = fwd_decl;

    if (members) {
        MirInstr *it;
        TSA_FOREACH(members, it)
        {
            ref_instr(it);
        }
    }

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_type_enum(Context *             cnt,
                                 Ast *                 node,
                                 ID *                  id,
                                 Scope *               scope,
                                 TSmallArray_InstrPtr *variants,
                                 MirInstr *            base_type)
{
    MirInstrTypeEnum *tmp       = create_instr(cnt, MIR_INSTR_TYPE_ENUM, node);
    tmp->base.value.type        = cnt->builtin_types->t_type;
    tmp->base.value.is_comptime = true;
    tmp->base.value.addr_mode   = MIR_VAM_RVALUE;
    tmp->variants               = variants;
    tmp->scope                  = scope;
    tmp->id                     = id;
    tmp->base_type              = base_type;

    if (variants) {
        MirInstr *it;
        TSA_FOREACH(variants, it)
        {
            ref_instr(it);
        }
    }

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_type_ptr(Context *cnt, Ast *node, MirInstr *type)
{
    MirInstrTypePtr *tmp        = create_instr(cnt, MIR_INSTR_TYPE_PTR, node);
    tmp->base.value.type        = cnt->builtin_types->t_type;
    tmp->base.value.addr_mode   = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime = true;
    tmp->type                   = ref_instr(type);
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *
append_instr_type_array(Context *cnt, Ast *node, ID *id, MirInstr *elem_type, MirInstr *len)
{
    MirInstrTypeArray *tmp      = create_instr(cnt, MIR_INSTR_TYPE_ARRAY, node);
    tmp->base.value.type        = cnt->builtin_types->t_type;
    tmp->base.value.addr_mode   = MIR_VAM_LVALUE_CONST;
    tmp->base.value.is_comptime = true;
    tmp->elem_type              = ref_instr(elem_type);
    tmp->len                    = ref_instr(len);
    tmp->id                     = id;
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_type_slice(Context *cnt, Ast *node, MirInstr *elem_type)
{
    MirInstrTypeSlice *tmp      = create_instr(cnt, MIR_INSTR_TYPE_SLICE, node);
    tmp->base.value.type        = cnt->builtin_types->t_type;
    tmp->base.value.is_comptime = true;
    tmp->elem_type              = ref_instr(elem_type);
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_type_dynarr(Context *cnt, Ast *node, MirInstr *elem_type)
{
    MirInstrTypeSlice *tmp      = create_instr(cnt, MIR_INSTR_TYPE_DYNARR, node);
    tmp->base.value.type        = cnt->builtin_types->t_type;
    tmp->base.value.is_comptime = true;
    tmp->elem_type              = ref_instr(elem_type);
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_type_vargs(Context *cnt, Ast *node, MirInstr *elem_type)
{
    MirInstrTypeVArgs *tmp      = create_instr(cnt, MIR_INSTR_TYPE_VARGS, node);
    tmp->base.value.type        = cnt->builtin_types->t_type;
    tmp->base.value.is_comptime = true;
    tmp->elem_type              = ref_instr(elem_type);
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_arg(Context *cnt, Ast *node, unsigned i)
{
    MirInstrArg *tmp = create_instr(cnt, MIR_INSTR_ARG, node);
    tmp->i           = i;
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *
append_instr_unroll(Context *cnt, Ast *node, MirInstr *src, MirInstr *remove_src, s32 index)
{
    BL_ASSERT(index >= 0);
    BL_ASSERT(src);
    MirInstrUnroll *tmp = create_instr(cnt, MIR_INSTR_UNROLL, node);
    tmp->src            = ref_instr(src);
    tmp->remove_src     = ref_instr(remove_src);
    tmp->index          = index;
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *create_instr_phi(Context *cnt, Ast *node)
{
    MirInstrPhi *tmp     = create_instr(cnt, MIR_INSTR_PHI, node);
    tmp->incoming_values = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
    tmp->incoming_blocks = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
    return &tmp->base;
}

MirInstr *append_instr_phi(Context *cnt, Ast *node)
{
    MirInstr *tmp = create_instr_phi(cnt, node);
    append_current_block(cnt, tmp);
    return tmp;
}

MirInstr *append_instr_compound(Context *             cnt,
                                Ast *                 node,
                                MirInstr *            type,
                                TSmallArray_InstrPtr *values,
                                bool                  is_multiple_return_value)
{
    MirInstr *tmp = create_instr_compound(cnt, node, type, values, is_multiple_return_value);
    append_current_block(cnt, tmp);
    return tmp;
}

MirInstr *
append_instr_compound_impl(Context *cnt, Ast *node, MirType *type, TSmallArray_InstrPtr *values)
{
    MirInstr *tmp    = append_instr_compound(cnt, node, NULL, values, false);
    tmp->value.type  = type;
    tmp->is_implicit = true;

    return tmp;
}

MirInstr *create_default_value_for_type(Context *cnt, MirType *type)
{
    // Default initializer is only zero initialized compound expression known in compile time,
    // this is universal for every type.
    BL_ASSERT(type && "Missing type for default zero initializer!");

    MirInstr *default_value = NULL;

    switch (type->kind) {
    case MIR_TYPE_ENUM: {
        // Use first enum variant as default.
        MirType *   base_type = type->data.enm.base_type;
        MirVariant *variant   = type->data.enm.variants->data[0];
        const u64   v         = MIR_CEV_READ_AS(u64, variant->value);
        default_value         = create_instr_const_int(cnt, NULL, base_type, v);
        break;
    }

    case MIR_TYPE_INT: {
        default_value = create_instr_const_int(cnt, NULL, type, 0);
        break;
    }

    case MIR_TYPE_REAL: {
        if (type->data.real.bitcount == 32) {
            default_value = create_instr_const_float(cnt, NULL, 0);
        } else {
            default_value = create_instr_const_double(cnt, NULL, 0);
        }
        break;
    }

    case MIR_TYPE_BOOL: {
        default_value = create_instr_const_bool(cnt, NULL, false);
        break;
    }

    default: {
        // Use zero initialized compound.
        MirInstrCompound *compound =
            (MirInstrCompound *)create_instr_compound_impl(cnt, NULL, type, NULL);
        compound->is_naked               = false;
        compound->is_zero_initialized    = true;
        compound->base.value.is_comptime = true;
        default_value                    = &compound->base;
        break;
    }
    }

    BL_ASSERT(default_value && "Invalid default value!");
    return ref_instr(default_value);
}

MirInstr *append_instr_cast(Context *cnt, Ast *node, MirInstr *type, MirInstr *next)
{
    MirInstrCast *tmp         = create_instr(cnt, MIR_INSTR_CAST, node);
    tmp->base.value.addr_mode = MIR_VAM_RVALUE;
    tmp->op                   = MIR_CAST_INVALID;
    tmp->type                 = ref_instr(type);
    tmp->expr                 = ref_instr(next);
    tmp->auto_cast            = type == NULL;

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_sizeof(Context *cnt, Ast *node, MirInstr *expr)
{
    MirInstrSizeof *tmp         = create_instr(cnt, MIR_INSTR_SIZEOF, node);
    tmp->base.value.type        = cnt->builtin_types->t_usize;
    tmp->base.value.is_comptime = true;
    tmp->base.value.addr_mode   = MIR_VAM_RVALUE;
    tmp->expr                   = ref_instr(expr);

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_type_info(Context *cnt, Ast *node, MirInstr *expr)
{
    MirInstr *tmp          = create_instr_type_info(cnt, node, expr);
    tmp->value.is_comptime = true;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    append_current_block(cnt, tmp);
    return tmp;
}

MirInstr *append_instr_test_cases(Context *cnt, Ast *node)
{
    MirInstr *tmp          = create_instr(cnt, MIR_INSTR_TEST_CASES, node);
    tmp->value.is_comptime = true;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    append_current_block(cnt, tmp);
    return tmp;
}

MirInstr *append_instr_alignof(Context *cnt, Ast *node, MirInstr *expr)
{
    MirInstrAlignof *tmp        = create_instr(cnt, MIR_INSTR_ALIGNOF, node);
    tmp->base.value.type        = cnt->builtin_types->t_usize;
    tmp->base.value.is_comptime = true;
    tmp->expr                   = ref_instr(expr);

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_cond_br(Context *      cnt,
                               Ast *          node,
                               MirInstr *     cond,
                               MirInstrBlock *then_block,
                               MirInstrBlock *else_block)
{
    BL_ASSERT(cond && then_block && else_block);
    ref_instr(&then_block->base);
    ref_instr(&else_block->base);

    MirInstrCondBr *tmp  = create_instr(cnt, MIR_INSTR_COND_BR, node);
    tmp->base.value.type = cnt->builtin_types->t_void;
    tmp->base.ref_count  = NO_REF_COUNTING;
    tmp->cond            = ref_instr(cond);
    tmp->then_block      = then_block;
    tmp->else_block      = else_block;

    append_current_block(cnt, &tmp->base);

    MirInstrBlock *block = ast_current_block(cnt);
    if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_br(Context *cnt, Ast *node, MirInstrBlock *then_block)
{
    BL_ASSERT(then_block);
    ref_instr(&then_block->base);
    MirInstrBr *tmp      = create_instr(cnt, MIR_INSTR_BR, node);
    tmp->base.value.type = cnt->builtin_types->t_void;
    tmp->base.ref_count  = NO_REF_COUNTING;
    tmp->then_block      = then_block;

    MirInstrBlock *block = ast_current_block(cnt);

    append_current_block(cnt, &tmp->base);
    if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_switch(Context *               cnt,
                              Ast *                   node,
                              MirInstr *              value,
                              MirInstrBlock *         default_block,
                              bool                    user_defined_default,
                              TSmallArray_SwitchCase *cases)
{
    BL_ASSERT(default_block);
    BL_ASSERT(cases);
    BL_ASSERT(value);

    ref_instr(&default_block->base);
    ref_instr(value);

    for (usize i = 0; i < cases->size; ++i) {
        MirSwitchCase *c = &cases->data[i];
        ref_instr(&c->block->base);
        ref_instr(c->on_value);
    }

    MirInstrSwitch *tmp           = create_instr(cnt, MIR_INSTR_SWITCH, node);
    tmp->base.value.type          = cnt->builtin_types->t_void;
    tmp->base.ref_count           = NO_REF_COUNTING;
    tmp->value                    = value;
    tmp->default_block            = default_block;
    tmp->cases                    = cases;
    tmp->has_user_defined_default = user_defined_default;

    MirInstrBlock *block = ast_current_block(cnt);

    append_current_block(cnt, &tmp->base);
    if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index)
{
    MirInstr *tmp = create_instr_elem_ptr(cnt, node, arr_ptr, index);
    append_current_block(cnt, tmp);
    return tmp;
}

MirInstr *append_instr_member_ptr(Context *        cnt,
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

MirInstr *append_instr_load(Context *cnt, Ast *node, MirInstr *src)
{
    MirInstrLoad *tmp = create_instr(cnt, MIR_INSTR_LOAD, node);
    tmp->src          = ref_instr(src);
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_addrof(Context *cnt, Ast *node, MirInstr *src)
{
    MirInstr *tmp = create_instr_addrof(cnt, node, src);
    append_current_block(cnt, tmp);
    return tmp;
}

MirInstr *append_instr_unrecheable(Context *cnt, Ast *node)
{
    MirInstrUnreachable *tmp = create_instr(cnt, MIR_INSTR_UNREACHABLE, node);
    tmp->base.value.type     = cnt->builtin_types->t_void;
    tmp->base.ref_count      = NO_REF_COUNTING;
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_fn_proto(Context * cnt,
                                Ast *     node,
                                MirInstr *type,
                                MirInstr *user_type,
                                bool      schedule_analyze)
{
    MirInstrFnProto *tmp        = create_instr(cnt, MIR_INSTR_FN_PROTO, node);
    tmp->base.value.addr_mode   = MIR_VAM_LVALUE_CONST;
    tmp->base.value.is_comptime = true;
    tmp->type                   = type;
    tmp->user_type              = user_type;
    tmp->base.ref_count         = NO_REF_COUNTING;
    tmp->pushed_for_analyze     = schedule_analyze;

    push_into_gscope(cnt, &tmp->base);

    if (schedule_analyze) analyze_push_back(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_fn_group(Context *cnt, Ast *node, TSmallArray_InstrPtr *variants)
{
    BL_ASSERT(variants);
    MirInstrFnGroup *tmp        = create_instr(cnt, MIR_INSTR_FN_GROUP, node);
    tmp->base.value.addr_mode   = MIR_VAM_LVALUE_CONST;
    tmp->base.value.is_comptime = true;
    tmp->base.ref_count         = NO_REF_COUNTING;
    tmp->variants               = variants;
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_decl_ref(Context *   cnt,
                                Ast *       node,
                                Unit *      parent_unit,
                                ID *        rid,
                                Scope *     scope,
                                ScopeEntry *scope_entry)
{
    BL_ASSERT(scope && rid);
    MirInstrDeclRef *tmp = create_instr(cnt, MIR_INSTR_DECL_REF, node);
    tmp->scope_entry     = scope_entry;
    tmp->scope           = scope;
    tmp->rid             = rid;
    tmp->parent_unit     = parent_unit;

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_decl_direct_ref(Context *cnt, MirInstr *ref)
{
    MirInstr *tmp                       = create_instr_decl_direct_ref(cnt, ref);
    ((MirInstrDeclDirectRef *)tmp)->ref = ref;
    append_current_block(cnt, tmp);
    return tmp;
}

MirInstr *append_instr_call(Context *cnt, Ast *node, MirInstr *callee, TSmallArray_InstrPtr *args)
{
    BL_ASSERT(callee);
    MirInstrCall *tmp         = create_instr(cnt, MIR_INSTR_CALL, node);
    tmp->base.value.addr_mode = MIR_VAM_RVALUE;
    tmp->args                 = args;
    tmp->callee               = callee;

    ref_instr(&tmp->base);

    // Callee must be referenced even if we call no-ref counted fn_proto instructions, because
    // sometimes callee is declaration reference pointing to variable containing pointer to some
    // function.
    ref_instr(callee);

    // reference all arguments
    if (args) {
        MirInstr *instr;
        TSA_FOREACH(args, instr) ref_instr(instr);
    }

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_decl_var(Context *        cnt,
                                Ast *            node,
                                ID *             id,
                                Scope *          scope,
                                MirInstr *       type,
                                MirInstr *       init,
                                bool             is_mutable,
                                u32              flags,
                                MirBuiltinIdKind builtin_id)
{
    BL_ASSERT(id && "Missing id.");
    BL_ASSERT(scope && "Missing scope.");
    MirInstrDeclVar *tmp = create_instr(cnt, MIR_INSTR_DECL_VAR, node);
    tmp->base.value.type = cnt->builtin_types->t_void;
    tmp->base.ref_count  = NO_REF_COUNTING;
    tmp->type            = ref_instr(type);
    tmp->init            = ref_instr(init);
    const bool is_global = !scope_is_local(scope);
    tmp->var =
        create_var(cnt, node, scope, id, NULL, is_mutable, is_global, false, flags, builtin_id);
    if (is_global) {
        push_into_gscope(cnt, &tmp->base);
        analyze_push_back(cnt, &tmp->base);
    } else {
        append_current_block(cnt, &tmp->base);
    }
    SET_IS_NAKED_IF_COMPOUND(init, false);
    return &tmp->base;
}

MirInstr *create_instr_decl_var_impl(Context *   cnt,
                                     const char *name,
                                     MirInstr *  type,
                                     MirInstr *  init,
                                     bool        is_mutable,
                                     bool        is_global)
{
    MirInstrDeclVar *tmp = create_instr(cnt, MIR_INSTR_DECL_VAR, NULL);
    tmp->base.value.type = cnt->builtin_types->t_void;
    tmp->base.ref_count  = NO_REF_COUNTING;
    tmp->type            = ref_instr(type);
    tmp->init            = ref_instr(init);
    tmp->var             = create_var_impl(cnt, name, NULL, is_mutable, is_global, false);
    SET_IS_NAKED_IF_COMPOUND(init, false);
    return &tmp->base;
}

MirInstr *append_instr_decl_var_impl(Context *   cnt,
                                     const char *name,
                                     MirInstr *  type,
                                     MirInstr *  init,
                                     bool        is_mutable,
                                     bool        is_global)
{
    MirInstr *tmp = create_instr_decl_var_impl(cnt, name, type, init, is_mutable, is_global);
    if (is_global) {
        push_into_gscope(cnt, tmp);
        analyze_push_back(cnt, tmp);
    } else {
        append_current_block(cnt, tmp);
    }
    return tmp;
}

MirInstr *
append_instr_decl_member(Context *cnt, Ast *node, MirInstr *type, TSmallArray_InstrPtr *tags)
{
    ID *id = node ? &node->data.ident.id : NULL;
    return append_instr_decl_member_impl(cnt, node, id, type, tags);
}

MirInstr *append_instr_decl_member_impl(Context *             cnt,
                                        Ast *                 node,
                                        ID *                  id,
                                        MirInstr *            type,
                                        TSmallArray_InstrPtr *tags)
{
    MirInstrDeclMember *tmp     = create_instr(cnt, MIR_INSTR_DECL_MEMBER, node);
    tmp->base.value.is_comptime = true;
    tmp->base.value.type        = cnt->builtin_types->t_void;
    tmp->base.ref_count         = NO_REF_COUNTING;
    tmp->type                   = ref_instr(type);
    tmp->tags                   = tags;

    tmp->member = create_member(cnt, node, id, -1, NULL);

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_decl_arg(Context *cnt, Ast *node, MirInstr *type, MirInstr *value)
{
    ref_instr(value);
    MirInstrDeclArg *tmp        = create_instr(cnt, MIR_INSTR_DECL_ARG, node);
    tmp->base.value.is_comptime = true;
    tmp->base.value.type        = cnt->builtin_types->t_void;

    tmp->base.ref_count = NO_REF_COUNTING;
    tmp->type           = ref_instr(type);

    ID *id   = node ? &node->data.ident.id : NULL;
    tmp->arg = create_arg(cnt, node, id, NULL, NULL, value);

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_decl_variant(Context *cnt, Ast *node, MirInstr *value)
{
    MirInstrDeclVariant *tmp    = create_instr(cnt, MIR_INSTR_DECL_VARIANT, node);
    tmp->base.value.is_comptime = true;
    tmp->base.value.type        = cnt->builtin_types->t_void;
    tmp->base.value.addr_mode   = MIR_VAM_LVALUE_CONST;
    tmp->base.ref_count         = NO_REF_COUNTING;
    tmp->value                  = value;

    BL_ASSERT(node && node->kind == AST_IDENT);
    ID *id       = &node->data.ident.id;
    tmp->variant = create_variant(cnt, id, NULL);

    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_const_int(Context *cnt, Ast *node, MirType *type, u64 val)
{
    MirInstr *tmp = create_instr_const_int(cnt, node, type, val);
    append_current_block(cnt, tmp);
    return tmp;
}

INLINE MirInstr *append_instr_const_float(Context *cnt, Ast *node, float val)
{
    MirInstr *tmp = create_instr_const_float(cnt, node, val);
    append_current_block(cnt, tmp);
    return tmp;
}

INLINE MirInstr *append_instr_const_double(Context *cnt, Ast *node, double val)
{
    MirInstr *tmp = create_instr_const_double(cnt, node, val);
    append_current_block(cnt, tmp);
    return tmp;
}

INLINE MirInstr *append_instr_const_bool(Context *cnt, Ast *node, bool val)
{
    MirInstr *tmp = create_instr_const_bool(cnt, node, val);
    append_current_block(cnt, tmp);
    return tmp;
}

MirInstr *append_instr_const_string(Context *cnt, Ast *node, const char *str)
{
    // Build up string as compound expression of lenght and pointer to data.
    TSmallArray_InstrPtr *values = create_sarr(TSmallArray_InstrPtr, cnt->assembly);

    MirInstr *len = create_instr_const_int(cnt, node, cnt->builtin_types->t_s64, strlen(str));
    MirInstr *ptr =
        create_instr_const_ptr(cnt, node, cnt->builtin_types->t_u8_ptr, (VMStackPtr)str);

    ANALYZE_INSTR_RQ(len);
    ANALYZE_INSTR_RQ(ptr);

    tsa_push_InstrPtr(values, len);
    tsa_push_InstrPtr(values, ptr);

    MirInstrCompound *compound = (MirInstrCompound *)append_instr_compound_impl(
        cnt, node, cnt->builtin_types->t_string, values);

    compound->is_naked               = false;
    compound->base.value.is_comptime = true;
    compound->base.value.addr_mode   = MIR_VAM_RVALUE;

    return &compound->base;
}

INLINE MirInstr *append_instr_const_char(Context *cnt, Ast *node, char c)
{
    MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = cnt->builtin_types->t_u8;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;

    MIR_CEV_WRITE_AS(char, &tmp->value, c);

    append_current_block(cnt, tmp);
    return tmp;
}

INLINE MirInstr *append_instr_const_null(Context *cnt, Ast *node)
{
    MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = create_type_null(cnt, cnt->builtin_types->t_u8_ptr);
    tmp->value.addr_mode   = MIR_VAM_RVALUE;

    MIR_CEV_WRITE_AS(void *, &tmp->value, NULL);

    append_current_block(cnt, tmp);
    return tmp;
}

MirInstr *append_instr_ret(Context *cnt, Ast *node, MirInstr *value)
{
    if (value) ref_instr(value);

    MirInstrRet *tmp          = create_instr(cnt, MIR_INSTR_RET, node);
    tmp->base.value.type      = cnt->builtin_types->t_void;
    tmp->base.value.addr_mode = MIR_VAM_RVALUE;
    tmp->base.ref_count       = NO_REF_COUNTING;
    tmp->value                = value;

    append_current_block(cnt, &tmp->base);

    MirInstrBlock *block = ast_current_block(cnt);
    if (!is_block_terminated(block)) terminate_block(block, &tmp->base);

    MirFn *fn = block->owner_fn;
    BL_ASSERT(fn);

    fn->terminal_instr = tmp;

    return &tmp->base;
}

MirInstr *append_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest)
{
    BL_ASSERT(src && dest);
    MirInstrStore *tmp   = create_instr(cnt, MIR_INSTR_STORE, node);
    tmp->base.value.type = cnt->builtin_types->t_void;
    tmp->base.ref_count  = NO_REF_COUNTING;
    tmp->src             = ref_instr(src);
    tmp->dest            = ref_instr(dest);
    SET_IS_NAKED_IF_COMPOUND(src, false);
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op)
{
    BL_ASSERT(lhs && rhs);
    MirInstrBinop *tmp = create_instr(cnt, MIR_INSTR_BINOP, node);
    tmp->lhs           = ref_instr(lhs);
    tmp->rhs           = ref_instr(rhs);
    tmp->op            = op;
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *append_instr_unop(Context *cnt, Ast *node, MirInstr *instr, UnopKind op)
{
    BL_ASSERT(instr);
    MirInstrUnop *tmp = create_instr(cnt, MIR_INSTR_UNOP, node);
    tmp->expr         = ref_instr(instr);
    tmp->op           = op;
    append_current_block(cnt, &tmp->base);
    return &tmp->base;
}

MirInstr *create_instr_vargs_impl(Context *cnt, MirType *type, TSmallArray_InstrPtr *values)
{
    BL_ASSERT(type);
    MirInstrVArgs *tmp = create_instr(cnt, MIR_INSTR_VARGS, NULL);
    tmp->type          = type;
    tmp->values        = values;
    return &tmp->base;
}

MirInstr *append_instr_call_loc(Context *cnt, Ast *node)
{
    MirInstr *tmp = create_instr_call_loc(cnt, node, NULL);
    append_current_block(cnt, tmp);
    return tmp;
}

// analyze
void erase_instr_tree(MirInstr *instr, bool keep_root, bool force)
{
    if (!instr) return;
    TSmallArray_InstrPtr64 queue;
    tsa_init(&queue);
    tsa_push_InstrPtr64(&queue, instr);
    MirInstr *top;
    while (queue.size) {
        top = tsa_pop_InstrPtr64(&queue);
        if (!top) continue;

        BL_ASSERT(top->analyzed && "Trying to erase not analyzed instruction.");
        if (!force) {
            if (top->ref_count == NO_REF_COUNTING) continue;
            if (top->ref_count > 0) continue;
        }

        switch (top->kind) {
        case MIR_INSTR_COMPOUND: {
            MirInstrCompound *compound = (MirInstrCompound *)top;
            if (compound->is_zero_initialized) break;

            MirInstr *it;
            TSA_FOREACH(compound->values, it)
            {
                unref_instr(it);
                tsa_push_InstrPtr64(&queue, it);
            }
            break;
        }

        case MIR_INSTR_BINOP: {
            MirInstrBinop *binop = (MirInstrBinop *)top;
            unref_instr(binop->lhs);
            unref_instr(binop->rhs);
            tsa_push_InstrPtr64(&queue, binop->rhs);
            tsa_push_InstrPtr64(&queue, binop->lhs);
            break;
        }

        case MIR_INSTR_LOAD: {
            MirInstrLoad *load = (MirInstrLoad *)top;
            unref_instr(load->src);
            tsa_push_InstrPtr64(&queue, load->src);
            break;
        }

        case MIR_INSTR_ALIGNOF: {
            MirInstrAlignof *alof = (MirInstrAlignof *)top;
            unref_instr(alof->expr);
            tsa_push_InstrPtr64(&queue, alof->expr);
            break;
        }

        case MIR_INSTR_SIZEOF: {
            MirInstrSizeof *szof = (MirInstrSizeof *)top;
            unref_instr(szof->expr);
            tsa_push_InstrPtr64(&queue, szof->expr);
            break;
        }

        case MIR_INSTR_ELEM_PTR: {
            MirInstrElemPtr *ep = (MirInstrElemPtr *)top;
            unref_instr(ep->arr_ptr);
            unref_instr(ep->index);
            tsa_push_InstrPtr64(&queue, ep->arr_ptr);
            tsa_push_InstrPtr64(&queue, ep->index);
            break;
        }

        case MIR_INSTR_MEMBER_PTR: {
            MirInstrMemberPtr *mp = (MirInstrMemberPtr *)top;
            unref_instr(mp->target_ptr);
            tsa_push_InstrPtr64(&queue, mp->target_ptr);
            break;
        }

        case MIR_INSTR_TYPE_INFO: {
            MirInstrTypeInfo *info = (MirInstrTypeInfo *)top;
            unref_instr(info->expr);
            tsa_push_InstrPtr64(&queue, info->expr);
            break;
        }

        case MIR_INSTR_CAST: {
            MirInstrCast *cast = (MirInstrCast *)top;
            unref_instr(cast->expr);
            unref_instr(cast->type);
            tsa_push_InstrPtr64(&queue, cast->expr);
            tsa_push_InstrPtr64(&queue, cast->type);
            break;
        }

        case MIR_INSTR_CALL: {
            MirInstrCall *call = (MirInstrCall *)top;
            if (call->args) {
                MirInstr *it;
                TSA_FOREACH(call->args, it)
                {
                    unref_instr(it);
                    tsa_push_InstrPtr64(&queue, it);
                }
            }
            break;
        }

        case MIR_INSTR_ADDROF: {
            MirInstrAddrOf *addrof = (MirInstrAddrOf *)top;
            unref_instr(addrof->src);
            tsa_push_InstrPtr64(&queue, addrof->src);
            break;
        }

        case MIR_INSTR_UNOP: {
            MirInstrUnop *unop = (MirInstrUnop *)top;
            unref_instr(unop->expr);
            tsa_push_InstrPtr64(&queue, unop->expr);
            break;
        }

        case MIR_INSTR_TYPE_PTR: {
            MirInstrTypePtr *tp = (MirInstrTypePtr *)top;
            unref_instr(tp->type);
            tsa_push_InstrPtr64(&queue, tp->type);
            break;
        }

        case MIR_INSTR_TYPE_ENUM: {
            MirInstrTypeEnum *te = (MirInstrTypeEnum *)top;
            unref_instr(te->base_type);
            tsa_push_InstrPtr64(&queue, te->base_type);

            MirInstr *it;
            TSA_FOREACH(te->variants, it)
            {
                unref_instr(it);
                tsa_push_InstrPtr64(&queue, it);
            }
            break;
        }

        case MIR_INSTR_TYPE_FN: {
            MirInstrTypeFn *tf = (MirInstrTypeFn *)top;
            unref_instr(tf->ret_type);
            tsa_push_InstrPtr64(&queue, tf->ret_type);

            if (tf->args) {
                MirInstr *it;
                TSA_FOREACH(tf->args, it)
                {
                    unref_instr(it);
                    tsa_push_InstrPtr64(&queue, it);
                }
            }
            break;
        }

        case MIR_INSTR_TYPE_FN_GROUP: {
            MirInstrTypeFnGroup *group = (MirInstrTypeFnGroup *)top;
            BL_ASSERT(group->variants);
            MirInstr *it;
            TSA_FOREACH(group->variants, it)
            {
                unref_instr(it);
                tsa_push_InstrPtr64(&queue, it);
            }
            break;
        }

        case MIR_INSTR_TYPE_VARGS: {
            MirInstrTypeVArgs *vargs = (MirInstrTypeVArgs *)top;
            unref_instr(vargs->elem_type);
            tsa_push_InstrPtr64(&queue, vargs->elem_type);
            break;
        }

        case MIR_INSTR_TYPE_ARRAY: {
            MirInstrTypeArray *ta = (MirInstrTypeArray *)top;
            unref_instr(ta->elem_type);
            unref_instr(ta->len);
            tsa_push_InstrPtr64(&queue, ta->elem_type);
            tsa_push_InstrPtr64(&queue, ta->len);
            break;
        }

        case MIR_INSTR_TYPE_DYNARR:
        case MIR_INSTR_TYPE_SLICE:
        case MIR_INSTR_TYPE_STRUCT: {
            MirInstrTypeStruct *ts = (MirInstrTypeStruct *)top;

            if (ts->members) {
                MirInstr *it;
                TSA_FOREACH(ts->members, it)
                {
                    unref_instr(it);
                    tsa_push_InstrPtr64(&queue, it);
                }
            }
            break;
        }

        case MIR_INSTR_VARGS: {
            MirInstrVArgs *vargs = (MirInstrVArgs *)top;
            if (vargs->values) {
                MirInstr *it;
                TSA_FOREACH(vargs->values, it)
                {
                    unref_instr(it);
                    tsa_push_InstrPtr64(&queue, it);
                }
            }
            break;
        }

        case MIR_INSTR_BLOCK:
            continue;

        case MIR_INSTR_DECL_REF:
        case MIR_INSTR_DECL_MEMBER:
        case MIR_INSTR_DECL_ARG:
        case MIR_INSTR_DECL_VARIANT:
        case MIR_INSTR_CONST:
        case MIR_INSTR_DECL_DIRECT_REF:
        case MIR_INSTR_CALL_LOC:
        case MIR_INSTR_UNROLL:
            break;

        default:
            BL_ABORT("Missing erase for instruction '%s'", mir_instr_name(top));
        }

        if (keep_root && top == instr) continue;
        erase_instr(top);
    }
    tsa_terminate(&queue);
}

bool evaluate(Context *cnt, MirInstr *instr)
{
    if (!instr) return true;
    BL_ASSERT(instr->analyzed && "Non-analyzed instruction cannot be evaluated!");
    // We can evaluate compile time know instructions only.
    if (!instr->value.is_comptime) return true;
    if (!vm_eval_instr(cnt->vm, cnt->assembly, instr)) {
        // Evaluation was aborted due to error.
        return false;
    }
    if (can_mutate_comptime_to_const(instr)) {
        const bool is_volatile = is_instr_type_volatile(instr);
        erase_instr_tree(instr, true, true);
        mutate_instr(instr, MIR_INSTR_CONST);
        ((MirInstrConst *)instr)->volatile_type = is_volatile;
    }

    return true;
}

AnalyzeResult analyze_resolve_type(Context *cnt, MirInstr *resolver_call, MirType **out_type)
{
    BL_ASSERT(resolver_call && "Expected resolver call.");
    BL_ASSERT(resolver_call->kind == MIR_INSTR_CALL &&
              "Type resolver is expected to be call to resolve function.");

    if (analyze_instr(cnt, resolver_call).state != ANALYZE_PASSED)
        return ANALYZE_RESULT(POSTPONE, 0);

    if (vm_execute_instr_top_level_call(cnt->vm, cnt->assembly, (MirInstrCall *)resolver_call)) {
        *out_type = MIR_CEV_READ_AS(MirType *, &resolver_call->value);
        BL_MAGIC_ASSERT(*out_type);
        return ANALYZE_RESULT(PASSED, 0);
    } else {
        return ANALYZE_RESULT(FAILED, 0);
    }
}

AnalyzeResult analyze_instr_toany(Context *cnt, MirInstrToAny *toany)
{
    MirInstr *expr      = toany->expr;
    MirType * any_type  = cnt->builtin_types->t_Any;
    MirType * expr_type = expr->value.type;

    BL_ASSERT(any_type && expr && expr_type);

    ID *missing_rtti_type_id = lookup_builtins_rtti(cnt);
    if (missing_rtti_type_id) {
        return ANALYZE_RESULT(WAITING, missing_rtti_type_id->hash);
    }

    MirType *  rtti_type       = expr_type;
    const bool is_deref_needed = is_load_needed(expr);
    if (is_deref_needed) rtti_type = mir_deref_type(rtti_type);

    const bool is_type       = rtti_type->kind == MIR_TYPE_TYPE || rtti_type->kind == MIR_TYPE_FN;
    const bool is_tmp_needed = expr->value.addr_mode == MIR_VAM_RVALUE && !is_type;

    if (rtti_type->kind == MIR_TYPE_VOID) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    expr->node->location,
                    BUILDER_CUR_AFTER,
                    "Expression yields 'void' value.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    if (is_tmp_needed && expr_type->store_size_bytes > 0) {
        // Target expression is not allocated object on the stack, so we need to crate
        // temporary variable containing the value and fetch pointer to this variable.
        const char *tmp_var_name = gen_uq_name(IMPL_ANY_EXPR_TMP);
        toany->expr_tmp = create_var_impl(cnt, tmp_var_name, rtti_type, false, false, false);
    }

    // Generate RTTI for expression's type.
    toany->rtti_type = rtti_type;
    rtti_gen(cnt, rtti_type);

    if (is_type) {
        BL_ASSERT(mir_is_comptime(expr));
        BL_ASSERT(!is_tmp_needed);

        VMStackPtr expr_data = NULL;
        if (is_deref_needed) {
            expr_data = *MIR_CEV_READ_AS(VMStackPtr *, &expr->value);
        } else {
            expr_data = MIR_CEV_READ_AS(VMStackPtr, &expr->value);
        }

        MirType *rtti_data = NULL;

        switch (rtti_type->kind) {
        case MIR_TYPE_FN: {
            MirFn *fn = (MirFn *)expr_data;
            rtti_data = fn->type;
            break;
        }

        case MIR_TYPE_TYPE: {
            rtti_data = (MirType *)expr_data;
            break;
        }

        default:
            BL_ABORT("Invalid expression type!");
        }

        BL_ASSERT(rtti_data && "Missing speficifation type for RTTI generation!");
        toany->rtti_data = rtti_data;

        rtti_gen(cnt, rtti_data);
        erase_instr_tree(expr, false, true);
    }

    // This is temporary vaiable used for Any data.
    const char *tmp_var_name = gen_uq_name(IMPL_ANY_TMP);
    toany->tmp               = create_var_impl(cnt, tmp_var_name, any_type, false, false, false);

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_phi(Context UNUSED(*cnt), MirInstrPhi *phi)
{
    // PHI instruction accept income values of two kinds, one is direct value and second reference
    // to last conditional break instruction in the income's block. Second case is supported due to
    // nature of instruction analyze; when instruction is referenced in conditionali break, it's
    // going to be analyzed as an input slot and could be eventually replaced by proper constant or
    // load instruction. In such case PHI would do same slot analyze pass again; this behaviour is
    // undefined, PHI income value can refer to already deleted instruction and ends up with invalid
    // value.
    BL_ASSERT(phi->incoming_blocks && phi->incoming_values);
    BL_ASSERT(phi->incoming_values->size == phi->incoming_blocks->size);

    const usize count = phi->incoming_values->size;

    MirInstr **value_ref;
    MirInstr * block;
    MirType *  type = NULL;

    for (usize i = 0; i < count; ++i) {
        value_ref = &phi->incoming_values->data[i];
        block     = phi->incoming_blocks->data[i];
        BL_ASSERT(block && block->kind == MIR_INSTR_BLOCK);
        BL_ASSERT((*value_ref)->analyzed && "Phi incomming value is not analyzed!");
        if ((*value_ref)->kind == MIR_INSTR_COND_BR) {
            *value_ref = ((MirInstrCondBr *)(*value_ref))->cond;
            BL_ASSERT(value_ref && *value_ref);
            BL_ASSERT((*value_ref)->analyzed && "Phi incomming value is not analyzed!");
        } else {
            const AnalyzeSlotConfig *conf =
                type ? &analyze_slot_conf_default : &analyze_slot_conf_basic;
            if (analyze_slot(cnt, conf, value_ref, type) != ANALYZE_PASSED)
                return ANALYZE_RESULT(FAILED, 0);
        }
        if (!type) type = (*value_ref)->value.type;
    }

    BL_ASSERT(type && "Cannot resolve type of phi instruction!");
    phi->base.value.type        = type;
    phi->base.value.addr_mode   = MIR_VAM_RVALUE;
    phi->base.value.is_comptime = false;

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_unroll(Context *cnt, MirInstrUnroll *unroll)
{
    MirInstr *src   = unroll->src;
    const s32 index = unroll->index;
    BL_ASSERT(src && "Missing unroll input!");
    BL_ASSERT(src->kind == MIR_INSTR_CALL && "Unroll expects source to be CALL instruction!");
    BL_ASSERT(index >= 0);
    BL_ASSERT(src->value.type);
    MirType *src_type = src->value.type;
    MirType *type     = src_type;
    if (mir_is_composit_type(src_type) && src_type->data.strct.is_multiple_return_type) {
        if (index >= (s32)src_type->data.strct.members->size) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_MEMBER_ACCESS,
                        unroll->base.node->location,
                        BUILDER_CUR_AFTER,
                        "Expected more return values than function returns.");
            return ANALYZE_RESULT(FAILED, 0);
        }
        MirInstrCall *src_call = (MirInstrCall *)src;
        MirInstr *    tmp_var  = src_call->unroll_tmp_var;
        if (!tmp_var) {
            // no tmp var to unroll from; create one and insert it after call
            tmp_var = create_instr_decl_var_impl(cnt,
                                                 gen_uq_name(IMPL_UNROLL_TMP),
                                                 NULL,
                                                 src,
                                                 false, // is_mutable
                                                 false  // is_global
            );
            insert_instr_after(src, tmp_var);
            ANALYZE_INSTR_RQ(tmp_var);
            src_call->unroll_tmp_var = tmp_var;
        }
        tmp_var = create_instr_decl_direct_ref(cnt, tmp_var);
        insert_instr_before(&unroll->base, tmp_var);
        ANALYZE_INSTR_RQ(tmp_var);
        unroll->src = ref_instr(tmp_var);
        type        = create_type_ptr(cnt, mir_get_struct_elem_type(src_type, index));
    } else {
        unroll->remove = true;
    }
    BL_ASSERT(type);
    unroll->base.value.type        = type;
    unroll->base.value.is_comptime = src->value.is_comptime;
    unroll->base.value.addr_mode   = src->value.addr_mode;
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_compound(Context *cnt, MirInstrCompound *cmp)
{
    TSmallArray_InstrPtr *values = cmp->values;

    if (cmp->is_multiple_return_value) {
        // Compound expression used as multiple return value has no type specified; function return
        // type must by used.
        MirFn *fn = instr_owner_fn(&cmp->base);
        BL_ASSERT(fn && fn->type);
        cmp->base.value.type = fn->type->data.fn.ret_type;
        BL_ASSERT(cmp->base.value.type);
    }

    MirType *type = cmp->base.value.type;
    if (cmp->base.is_implicit) {
        BL_ASSERT(type && "Missig type for implicit compound!");
        BL_ASSERT((cmp->is_zero_initialized || values->size) &&
                  "Missnig type for implicit compound!");

        goto SKIP_IMPLICIT;
    }
    // Setup compound type.
    if (!type) {
        // generate load instruction if needed
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
        type = MIR_CEV_READ_AS(MirType *, &instr_type->value);
        BL_MAGIC_ASSERT(type);
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

    cmp->base.value.type        = type;
    cmp->base.value.is_comptime = true; // can be overriden later

    // Check if array is supposed to be initilialized to {0}
    if (values->size == 1) {
        MirInstr *value = values->data[0];
        if (value->kind == MIR_INSTR_CONST && value->value.type->kind == MIR_TYPE_INT &&
            MIR_CEV_READ_AS(u64, &value->value) == 0) {
            cmp->is_zero_initialized = true;
        }
    }

    switch (type->kind) {
    case MIR_TYPE_ARRAY: {
        if (cmp->is_zero_initialized) break;

        if (values->size != (usize)type->data.array.len) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_INITIALIZER,
                        cmp->base.node->location,
                        BUILDER_CUR_WORD,
                        "Array initializer must explicitly set all array elements or "
                        "initialize array to 0 by zero initializer {0}. Expected is "
                        "%llu but given %llu.",
                        (unsigned long long)type->data.array.len,
                        (unsigned long long)values->size);
            return ANALYZE_RESULT(FAILED, 0);
        }

        // Else iterate over values
        MirInstr **value_ref;
        for (usize i = 0; i < values->size; ++i) {
            value_ref = &values->data[i];

            if (analyze_slot(
                    cnt, &analyze_slot_conf_default, value_ref, type->data.array.elem_type) !=
                ANALYZE_PASSED)
                return ANALYZE_RESULT(FAILED, 0);

            cmp->base.value.is_comptime =
                (*value_ref)->value.is_comptime ? cmp->base.value.is_comptime : false;
        }

        break;
    }

    case MIR_TYPE_DYNARR:
    case MIR_TYPE_SLICE:
    case MIR_TYPE_STRING:
    case MIR_TYPE_VARGS:
    case MIR_TYPE_STRUCT: {
        if (cmp->is_zero_initialized) break;
        const bool  is_union                = type->data.strct.is_union;
        const bool  is_multiple_return_type = type->data.strct.is_multiple_return_type;
        const usize memc                    = type->data.strct.members->size;

        if (is_union) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_INITIALIZER,
                        cmp->base.node->location,
                        BUILDER_CUR_WORD,
                        "Union can be zero initialized only, this is related to Issue: "
                        "https://github.com/travisdoor/bl/issues/105");
            return ANALYZE_RESULT(FAILED, 0);
        } else {
            if (values->size != memc) {
                const char *msg =
                    is_multiple_return_type
                        ? "Expected %llu return values but given %llu."
                        : "Structure initializer must explicitly set all members of the "
                          "structure or initialize structure to 0 by zero initializer "
                          "{0}. Expected is %llu but given %llu.";
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_INVALID_INITIALIZER,
                            cmp->base.node->location,
                            BUILDER_CUR_WORD,
                            msg,
                            (unsigned long long)memc,
                            (unsigned long long)values->size);
                return ANALYZE_RESULT(FAILED, 0);
            }

            // Else iterate over values
            MirInstr **value_ref;
            MirType *  member_type;
            for (u32 i = 0; i < values->size; ++i) {
                value_ref   = &values->data[i];
                member_type = mir_get_struct_elem_type(type, i);

                if (analyze_slot(cnt, &analyze_slot_conf_default, value_ref, member_type) !=
                    ANALYZE_PASSED)
                    return ANALYZE_RESULT(FAILED, 0);

                cmp->base.value.is_comptime =
                    (*value_ref)->value.is_comptime ? cmp->base.value.is_comptime : false;
            }
        }

        break;
    }

    default: {
        // Non-aggregate type.
        if (values->size > 1) {
            MirInstr *value = values->data[1];
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_INITIALIZER,
                        value->node->location,
                        BUILDER_CUR_WORD,
                        "One value only is expected for non-aggregate types.");
            return ANALYZE_RESULT(FAILED, 0);
        }
        MirInstr **              value_ref = &values->data[0];
        const AnalyzeSlotConfig *conf =
            type ? &analyze_slot_conf_default : &analyze_slot_conf_basic;
        if (analyze_slot(cnt, conf, value_ref, type) != ANALYZE_PASSED)
            return ANALYZE_RESULT(FAILED, 0);
        cmp->base.value.is_comptime = (*value_ref)->value.is_comptime;
    }
    }

SKIP_IMPLICIT:
    if (!mir_is_global(&cmp->base) && cmp->is_naked) {
        // For naked non-compile time compounds we need to generate implicit temp storage to
        // keep all data.
        MirVar *tmp_var =
            create_var_impl(cnt, gen_uq_name(IMPL_COMPOUND_TMP), type, true, false, false);
        cmp->tmp_var = tmp_var;
    }

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_var(Context *cnt, MirVar *var)
{
    BL_TRACY_MESSAGE("ANALYZE_VAR", "%s", var->linkage_name);
    if (!var->value.type) {
        BL_ABORT("unknown declaration type");
    }
    switch (var->value.type->kind) {
    case MIR_TYPE_TYPE:
        // Disable LLVM generation of typedefs.
        if (!var->is_mutable) break;
        // Typedef must be immutable!
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_MUTABILITY,
                    var->decl_node ? var->decl_node->location : NULL,
                    BUILDER_CUR_WORD,
                    "Type declaration must be immutable.");
        return ANALYZE_RESULT(FAILED, 0);
    case MIR_TYPE_FN:
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    var->decl_node ? var->decl_node->location : NULL,
                    BUILDER_CUR_WORD,
                    "Invalid type of the variable, functions can be referenced "
                    "only by pointers.");
        return ANALYZE_RESULT(FAILED, 0);
    case MIR_TYPE_FN_GROUP:
        if (!var->is_mutable) break;
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    var->decl_node ? var->decl_node->location : NULL,
                    BUILDER_CUR_WORD,
                    "Function group must be immutable.");
        return ANALYZE_RESULT(FAILED, 0);
    case MIR_TYPE_VOID:
        // Allocated type is void type.
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    var->decl_node ? var->decl_node->location : NULL,
                    BUILDER_CUR_WORD,
                    "Cannot allocate unsized type.");
        return ANALYZE_RESULT(FAILED, 0);
    default:
        break;
    }

    if (!var->is_implicit) {
        if (var->is_global && var->entry->kind == SCOPE_ENTRY_VOID) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_NAME,
                        var->decl_node->location,
                        BUILDER_CUR_WORD,
                        "Global variable cannot be explicitly unnamed.");
            return ANALYZE_RESULT(FAILED, 0);
        }
        commit_var(cnt, var);
    }
    // Type declaration should not be generated in LLVM.
    const MirTypeKind var_type_kind = var->value.type->kind;
    var->emit_llvm = var_type_kind != MIR_TYPE_TYPE && var_type_kind != MIR_TYPE_FN_GROUP;
    // Just take note whether variable was fully analyzed.
    var->analyzed = true;
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_set_initializer(Context *cnt, MirInstrSetInitializer *si)
{
    TSmallArray_InstrPtr *dests = si->dests;
    BL_ASSERT(dests && "Missing variables to initialize.");
    BL_ASSERT(dests->size && "Expected at least one variable.");
    MirInstr *dest;
    TSA_FOREACH(dests, dest)
    {
        // Just pre-scan to check if all destination variables are analyzed.
        BL_ASSERT(dest && dest->kind == MIR_INSTR_DECL_VAR);
        if (!dest->analyzed) return ANALYZE_RESULT(POSTPONE, 0); // PERFORMANCE: use wait???
    }

    MirType *type = ((MirInstrDeclVar *)dests->data[0])->var->value.type;
    // When there is no source initialization value to set global we can omit type inferring and
    // initialization value slot analyze pass.
    const bool is_default = !si->src;
    if (!is_default) {
        const AnalyzeSlotConfig *config =
            type ? &analyze_slot_conf_default : &analyze_slot_conf_basic;

        if (analyze_slot(cnt, config, &si->src, type) != ANALYZE_PASSED) {
            return ANALYZE_RESULT(FAILED, 0);
        }

        // Infer variable type if needed.
        if (!type) type = si->src->value.type;
    } else {
        // Generate default value based on type!
        BL_ASSERT(type && "Missing variable initializer type for default global initializer!");
        MirInstr *default_init = create_default_value_for_type(cnt, type);
        insert_instr_before(&si->base, default_init);
        ANALYZE_INSTR_RQ(default_init);
        si->src = default_init;
    }

    BL_ASSERT(type && "Missing variable initializer type for default global initializer!");
    BL_ASSERT(type->kind != MIR_TYPE_VOID && "Global value cannot be void!");
    BL_ASSERT(si->src && "Invalid global initializer source value.");
    // Global initializer must be compile time known.
    if (!si->src->value.is_comptime) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_EXPECTED_COMPTIME,
                    si->src->node->location,
                    BUILDER_CUR_WORD,
                    "Global variables must be initialized with compile time known value.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    SET_IS_NAKED_IF_COMPOUND(si->src, false);

    TSA_FOREACH(dests, dest)
    {
        MirVar *var = ((MirInstrDeclVar *)dest)->var;
        BL_ASSERT(var && "Missing MirVar for variable declaration!");
        BL_ASSERT((var->is_global || var->is_struct_typedef) &&
                  "Only global variables can be initialized by initializer!");
        var->value.type = type;
        // Initializer value is guaranteed to be comptime so we just check variable mutability.
        // (mutable variables cannot be comptime)
        var->value.is_comptime = !var->is_mutable;

        if (IS_FLAG(var->flags, FLAG_THREAD_LOCAL)) {
            switch (var->value.type->kind) {
            case MIR_TYPE_FN:
            case MIR_TYPE_FN_GROUP:
            case MIR_TYPE_TYPE:
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_INVALID_TYPE,
                            var->decl_node->location,
                            BUILDER_CUR_WORD,
                            "Invalid type of thread local variable.");
                return ANALYZE_RESULT(FAILED, 0);
            default:
                break;
            }
        }

        AnalyzeResult state = analyze_var(cnt, var);
        if (state.state != ANALYZE_PASSED) return state;

        // Typedef resolver cannot be generated into LLVM IR because TypeType has no LLVM
        // representation and should live only during compile time of MIR.
        si->base.owner_block->emit_llvm = var->emit_llvm;

        if (!var->value.is_comptime) {
            // Global variables which are not compile time constants are allocated
            // on the stack, one option is to do allocation every time when we
            // invoke comptime function execution, but we don't know which globals
            // will be used by function and we also don't known whatever function
            // has some side effect or not. So we produce allocation here. Variable
            // will be stored in static data segment. There is no need to use
            // relative pointers here.
            vm_alloc_global(cnt->vm, cnt->assembly, var);
        }

        // Check whether variable is command_line_arguments, we store pointer to this variable for
        // later use (it's going to be set to user input arguments in case of compile-time
        // execution).
        if (var->builtin_id == MIR_BUILTIN_ID_COMMAND_LINE_ARGUMENTS) {
            BL_ASSERT(!cnt->assembly->vm_run.command_line_arguments);
            cnt->assembly->vm_run.command_line_arguments = var;
        }
    }
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_vargs(Context *cnt, MirInstrVArgs *vargs)
{
    MirType *             type   = vargs->type;
    TSmallArray_InstrPtr *values = vargs->values;
    BL_ASSERT(type && values);
    type             = CREATE_TYPE_STRUCT_VARGS(cnt, NULL, create_type_ptr(cnt, type));
    const usize valc = values->size;
    if (valc > 0) {
        // Prepare tmp array for values
        const char *tmp_name = gen_uq_name(IMPL_VARGS_TMP_ARR);
        MirType *   tmp_type = create_type_array(cnt, NULL, vargs->type, (u32)valc);
        vargs->arr_tmp       = create_var_impl(cnt, tmp_name, tmp_type, true, false, false);
    }

    {
        // Prepare tmp slice for vargs
        const char *tmp_name = gen_uq_name(IMPL_VARGS_TMP);
        vargs->vargs_tmp     = create_var_impl(cnt, tmp_name, type, true, false, false);
    }

    MirInstr **value;
    bool       is_valid = true;

    for (usize i = 0; i < valc && is_valid; ++i) {
        value = &values->data[i];

        if (analyze_slot(cnt, &analyze_slot_conf_full, value, vargs->type) != ANALYZE_PASSED)
            return ANALYZE_RESULT(FAILED, 0);
    }

    vargs->base.value.type = type;
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
    if (analyze_slot(
            cnt, &analyze_slot_conf_default, &elem_ptr->index, cnt->builtin_types->t_s64) !=
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

    switch (arr_type->kind) {
    case MIR_TYPE_ARRAY: {
        if (mir_is_comptime(elem_ptr->index)) {
            const s64 len = arr_type->data.array.len;
            const s64 i   = MIR_CEV_READ_AS(s64, &elem_ptr->index->value);
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

        // setup ElemPtr instruction const_value type
        MirType *elem_type = arr_type->data.array.elem_type;
        BL_ASSERT(elem_type);
        elem_ptr->base.value.type = create_type_ptr(cnt, elem_type);
        break;
    }
    case MIR_TYPE_SLICE:
    case MIR_TYPE_STRING:
    case MIR_TYPE_VARGS:
    case MIR_TYPE_DYNARR: {
        // Support of direct slice access -> slice[N]
        // Since slice is special kind of structure data we need to handle
        // access to pointer and lenght later during execuion. We cannot create
        // member poiner instruction here because we need check boundaries on
        // array later during runtime. This leads to special kind of elemptr
        // interpretation and IR generation also.

        // setup type
        MirType *elem_type = mir_get_struct_elem_type(arr_type, MIR_SLICE_PTR_INDEX);
        BL_ASSERT(elem_type);
        elem_ptr->base.value.type = elem_type;
        break;
    }
    default: {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    arr_ptr->node->location,
                    BUILDER_CUR_WORD,
                    "Expected array or slice type.");
        return ANALYZE_RESULT(FAILED, 0);
    }
    }

    elem_ptr->base.value.addr_mode   = arr_ptr->value.addr_mode;
    elem_ptr->base.value.is_comptime = mir_is_comptime(arr_ptr) && mir_is_comptime(elem_ptr->index);
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
    MirInstr *target_ptr = member_ptr->target_ptr;
    BL_ASSERT(target_ptr);
    MirType *target_type = target_ptr->value.type;

    MirValueAddressMode target_addr_mode = target_ptr->value.addr_mode;
    Ast *               ast_member_ident = member_ptr->member_ident;

    if (target_type->kind == MIR_TYPE_NAMED_SCOPE) {
        ScopeEntry *scope_entry = MIR_CEV_READ_AS(ScopeEntry *, &target_ptr->value);
        BL_ASSERT(scope_entry);
        BL_MAGIC_ASSERT(scope_entry);
        BL_ASSERT(scope_entry->kind == SCOPE_ENTRY_NAMED_SCOPE && "Expected named scope.");
        Scope *scope = scope_entry->data.scope;
        BL_ASSERT(scope);
        BL_MAGIC_ASSERT(scope);
        ID *  rid         = &ast_member_ident->data.ident.id;
        Unit *parent_unit = ast_member_ident->location->unit;
        BL_ASSERT(rid);
        BL_ASSERT(parent_unit);
        MirInstrDeclRef *decl_ref =
            (MirInstrDeclRef *)mutate_instr(&member_ptr->base, MIR_INSTR_DECL_REF);
        decl_ref->scope                  = scope;
        decl_ref->scope_entry            = NULL;
        decl_ref->accept_incomplete_type = false;
        decl_ref->parent_unit            = parent_unit;
        decl_ref->rid                    = rid;
        unref_instr(target_ptr);
        erase_instr_tree(target_ptr, false, false);
        return ANALYZE_RESULT(POSTPONE, 0);
    }

    if (target_type->kind != MIR_TYPE_PTR) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    target_ptr->node->location,
                    BUILDER_CUR_WORD,
                    "Expected structure type.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    target_type = mir_deref_type(target_type);

    // Array type
    if (target_type->kind == MIR_TYPE_ARRAY) {
        // check array builtin members
        if (member_ptr->builtin_id == MIR_BUILTIN_ID_ARR_LEN ||
            is_builtin(ast_member_ident, MIR_BUILTIN_ID_ARR_LEN)) {
            // .len
            // mutate instruction into constant
            unref_instr(member_ptr->target_ptr);
            erase_instr_tree(member_ptr->target_ptr, false, false);
            MirInstrConst *len = (MirInstrConst *)mutate_instr(&member_ptr->base, MIR_INSTR_CONST);
            len->volatile_type = false;
            len->base.value.is_comptime = true;
            len->base.value.type        = cnt->builtin_types->t_s64;
            MIR_CEV_WRITE_AS(s64, &len->base.value, target_type->data.array.len);
        } else if (member_ptr->builtin_id == MIR_BUILTIN_ID_ARR_PTR ||
                   is_builtin(ast_member_ident, MIR_BUILTIN_ID_ARR_PTR)) {
            // .ptr -> This will be replaced by:
            //     elemptr
            //     addrof
            // to match syntax: &array[0]
            MirInstr *index = create_instr_const_int(cnt, NULL, cnt->builtin_types->t_s64, 0);

            insert_instr_before(&member_ptr->base, index);

            MirInstr *elem_ptr = create_instr_elem_ptr(cnt, NULL, target_ptr, index);
            ref_instr(elem_ptr);

            insert_instr_before(&member_ptr->base, elem_ptr);

            ANALYZE_INSTR_RQ(index);
            ANALYZE_INSTR_RQ(elem_ptr);

            MirInstrAddrOf *addrof_elem =
                (MirInstrAddrOf *)mutate_instr(&member_ptr->base, MIR_INSTR_ADDROF);
            addrof_elem->src = elem_ptr;
            ANALYZE_INSTR_RQ(&addrof_elem->base);
        } else {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_MEMBER_ACCESS,
                        ast_member_ident->location,
                        BUILDER_CUR_WORD,
                        "Unknown member.");
            return ANALYZE_RESULT(FAILED, 0);
        }

        member_ptr->base.value.addr_mode = target_addr_mode;
        return ANALYZE_RESULT(PASSED, 0);
    }

    bool additional_load_needed = false;
    if (target_type->kind == MIR_TYPE_PTR) {
        // We try to access structure member via pointer so we need one more load.
        additional_load_needed = true;
        target_type            = mir_deref_type(target_type);
    }

    // struct type
    if (mir_is_composit_type(target_type)) {
        // Check if structure type is complete, if not analyzer must wait for it!
        if (is_incomplete_struct_type(target_type))
            return ANALYZE_RESULT(WAITING, target_type->user_id->hash);

        if (additional_load_needed) {
            member_ptr->target_ptr = insert_instr_load(cnt, member_ptr->target_ptr);
            ANALYZE_INSTR_RQ(member_ptr->target_ptr);
        }

        ID *        rid   = &ast_member_ident->data.ident.id;
        MirType *   type  = target_type;
        ScopeEntry *found = lookup_composit_member(target_type, rid, &type);

        // Check if member was found in base type's scope.
        if (found && found->parent_scope != target_type->data.strct.scope) {
            // @HACK: It seems to be the best way for now just create implicit
            // cast to desired base type and use this as target, that also
            // should solve problems with deeper nesting (bitcast of pointer is
            // better then multiple GEPs?)
            if (is_load_needed(member_ptr->target_ptr)) {
                member_ptr->target_ptr = insert_instr_addrof(cnt, member_ptr->target_ptr);

                ANALYZE_INSTR_RQ(member_ptr->target_ptr);
            }
            member_ptr->target_ptr =
                insert_instr_cast(cnt, member_ptr->target_ptr, create_type_ptr(cnt, type));
            ANALYZE_INSTR_RQ(member_ptr->target_ptr);
        }

        if (!found) {
            // Member not found!
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_UNKNOWN_SYMBOL,
                        member_ptr->member_ident->location,
                        BUILDER_CUR_WORD,
                        "Unknown structure member.");
            return ANALYZE_RESULT(FAILED, 0);
        }

        BL_ASSERT(found->kind == SCOPE_ENTRY_MEMBER);
        MirMember *member = found->data.member;

        // setup member_ptr type
        MirType *member_ptr_type           = create_type_ptr(cnt, member->type);
        member_ptr->base.value.type        = member_ptr_type;
        member_ptr->base.value.addr_mode   = target_addr_mode;
        member_ptr->base.value.is_comptime = target_ptr->value.is_comptime;
        member_ptr->scope_entry            = found;

        return ANALYZE_RESULT(PASSED, 0);
    }

    // Sub type member.
    if (target_type->kind == MIR_TYPE_TYPE) {
        // generate load instruction if needed
        if (analyze_slot(cnt, &analyze_slot_conf_basic, &member_ptr->target_ptr, NULL) !=
            ANALYZE_PASSED) {
            return ANALYZE_RESULT(FAILED, 0);
        }

        MirType *sub_type = MIR_CEV_READ_AS(MirType *, &member_ptr->target_ptr->value);
        BL_MAGIC_ASSERT(sub_type);

        if (sub_type->kind != MIR_TYPE_ENUM) {
            goto INVALID;
        }

        // lookup for member inside struct
        Scope *     scope = sub_type->data.enm.scope;
        ID *        rid   = &ast_member_ident->data.ident.id;
        ScopeEntry *found = scope_lookup(scope, rid, false, true, NULL);
        if (!found) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_UNKNOWN_SYMBOL,
                        member_ptr->member_ident->location,
                        BUILDER_CUR_WORD,
                        "Unknown enumerator variant.");
            return ANALYZE_RESULT(FAILED, 0);
        }

        BL_ASSERT(found->kind == SCOPE_ENTRY_VARIANT);

        member_ptr->scope_entry            = found;
        member_ptr->base.value.type        = sub_type;
        member_ptr->base.value.addr_mode   = target_addr_mode;
        member_ptr->base.value.is_comptime = true;

        return ANALYZE_RESULT(PASSED, 0);
    }

    // Invalid
INVALID:
    builder_msg(BUILDER_MSG_ERROR,
                ERR_INVALID_MEMBER_ACCESS,
                target_ptr->node->location,
                BUILDER_CUR_WORD,
                "Expected structure or enumerator type.");
    return ANALYZE_RESULT(FAILED, 0);
}

AnalyzeResult analyze_instr_addrof(Context *cnt, MirInstrAddrOf *addrof)
{
    MirInstr *src = addrof->src;
    BL_ASSERT(src);
    if (!src->analyzed) return ANALYZE_RESULT(POSTPONE, 0);
    const MirValueAddressMode src_addr_mode = src->value.addr_mode;
    const bool                can_grab_address =
        (src_addr_mode == MIR_VAM_LVALUE || src_addr_mode == MIR_VAM_LVALUE_CONST ||
         src->value.type->kind == MIR_TYPE_FN);

    if (!can_grab_address) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_EXPECTED_DECL,
                    addrof->base.node->location,
                    BUILDER_CUR_WORD,
                    "Cannot take the address of unallocated object.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    // setup type
    MirType *type = NULL;
    BL_ASSERT(src->value.type);
    if (src->value.type->kind == MIR_TYPE_FN) {
        MirFn *fn = MIR_CEV_READ_AS(MirFn *, &src->value);
        BL_MAGIC_ASSERT(fn);
        // NOTE: Here we increase function ref count.
        ++fn->ref_count;
        type = create_type_ptr(cnt, src->value.type);
    } else {
        type = src->value.type;
    }
    addrof->base.value.type        = type;
    addrof->base.value.is_comptime = addrof->src->value.is_comptime && mir_is_global(addrof->src);
    addrof->base.value.addr_mode   = MIR_VAM_RVALUE;
    BL_ASSERT(addrof->base.value.type && "invalid type");
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_cast(Context *cnt, MirInstrCast *cast, bool analyze_op_only)
{
    MirType *dest_type = cast->base.value.type;

    if (!analyze_op_only) {
        if (!dest_type && !cast->auto_cast) {
            AnalyzeResult result = analyze_resolve_type(cnt, cast->type, &dest_type);
            if (result.state != ANALYZE_PASSED) return result;
        }

        const AnalyzeSlotConfig *config =
            cast->base.is_implicit ? &analyze_slot_conf_dummy : &analyze_slot_conf_basic;

        if (analyze_slot(cnt, config, &cast->expr, dest_type) != ANALYZE_PASSED) {
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

    // Setup const int type.
    if (analyze_stage_set_volatile_expr(cnt, &cast->expr, dest_type, false) ==
        ANALYZE_STAGE_BREAK) {
        cast->op = MIR_CAST_NONE;
        goto DONE;
    }

    cast->op = get_cast_op(expr_type, dest_type);
    if (cast->op == MIR_CAST_INVALID) {
        error_types(expr_type, dest_type, cast->base.node, "Invalid cast from '%s' to '%s'.");
        return ANALYZE_RESULT(FAILED, 0);
    }

DONE:
    cast->base.value.type        = dest_type;
    cast->base.value.is_comptime = cast->expr->value.is_comptime;

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_sizeof(Context *cnt, MirInstrSizeof *szof)
{
    BL_ASSERT(szof->expr);

    if (analyze_slot(cnt, &analyze_slot_conf_basic, &szof->expr, NULL) != ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    MirType *type = szof->expr->value.type;
    BL_ASSERT(type);

    if (type->kind == MIR_TYPE_TYPE) {
        type = MIR_CEV_READ_AS(MirType *, &szof->expr->value);
        BL_MAGIC_ASSERT(type);
    }

    // sizeof operator needs only type of input expression so we can erase whole call
    // tree generated to get this expression
    unref_instr(szof->expr);
    erase_instr_tree(szof->expr, false, false);

    MIR_CEV_WRITE_AS(u64, &szof->base.value, type->store_size_bytes);
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info)
{
    if (!type_info->rtti_type) {
        BL_ASSERT(type_info->expr);
        ID *missing_rtti_type_id = lookup_builtins_rtti(cnt);
        if (missing_rtti_type_id) {
            return ANALYZE_RESULT(WAITING, missing_rtti_type_id->hash);
        }
        if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_info->expr, NULL) != ANALYZE_PASSED) {
            return ANALYZE_RESULT(FAILED, 0);
        }
        MirType *type = type_info->expr->value.type;
        BL_MAGIC_ASSERT(type);
        if (type->kind == MIR_TYPE_TYPE) {
            type = MIR_CEV_READ_AS(MirType *, &type_info->expr->value);
            BL_MAGIC_ASSERT(type);
        }
        type_info->rtti_type = type;
    }
    if (!is_complete_type(cnt, type_info->rtti_type)) {
        return ANALYZE_RESULT(POSTPONE, 0);
    }
    if (type_info->rtti_type->kind == MIR_TYPE_NAMED_SCOPE) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    type_info->expr->node->location,
                    BUILDER_CUR_WORD,
                    "No type info available for scope type.");
        return ANALYZE_RESULT(FAILED, 0);
    }
    rtti_gen(cnt, type_info->rtti_type);
    type_info->base.value.type = cnt->builtin_types->t_TypeInfo_ptr;

    erase_instr_tree(type_info->expr, false, true);
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_alignof(Context *cnt, MirInstrAlignof *alof)
{
    BL_ASSERT(alof->expr);

    if (analyze_slot(cnt, &analyze_slot_conf_basic, &alof->expr, NULL) != ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    MirType *type = alof->expr->value.type;
    BL_ASSERT(type);

    if (type->kind == MIR_TYPE_TYPE) {
        type = MIR_CEV_READ_AS(MirType *, &alof->expr->value);
        BL_MAGIC_ASSERT(type);
    }

    MIR_CEV_WRITE_AS(s32, &alof->base.value, type->alignment);
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
    BL_ASSERT(ref->rid && ref->scope);

    ScopeEntry *found                        = NULL;
    Scope *     private_scope                = ref->parent_unit->private_scope;
    bool        is_ref_out_of_fn_local_scope = false;

    // Disable access to the symbols declared in private named scope from the outside of the named
    // scope.
    //     foo();
    //     #scope Foo
    //     #private
    //     foo :: fn () {}
    if (private_scope && scope_is_subtree_of_kind(private_scope, SCOPE_NAMED) &&
        !scope_is_subtree_of_kind(ref->scope, SCOPE_NAMED)) {
        private_scope = NULL;
    }

    if (!private_scope) { // reference in unit without private scope
        found = scope_lookup(ref->scope, ref->rid, true, false, &is_ref_out_of_fn_local_scope);
    } else { // reference in unit with private scope
        // search in current tree and ignore global scope
        found = scope_lookup(ref->scope, ref->rid, true, false, &is_ref_out_of_fn_local_scope);

        // lookup in private scope and global scope also (private scope has global
        // scope as parent every time)
        if (!found) {
            found =
                scope_lookup(private_scope, ref->rid, true, false, &is_ref_out_of_fn_local_scope);
        }
    }

    if (!found) return ANALYZE_RESULT(WAITING, ref->rid->hash);
    if (found->kind == SCOPE_ENTRY_INCOMPLETE) {
        BL_TRACY_MESSAGE("INCOMPLETE_DECL_REF", "%s", ref->rid->str);
        return ANALYZE_RESULT(WAITING, ref->rid->hash);
    }
    scope_entry_ref(found);
    switch (found->kind) {
    case SCOPE_ENTRY_FN: {
        MirFn *fn = found->data.fn;
        BL_ASSERT(fn);
        MirType *type = fn->type;
        BL_ASSERT(type);

        ref->base.value.type        = type;
        ref->base.value.is_comptime = true;
        ref->base.value.addr_mode   = MIR_VAM_RVALUE;

        // CLEANUP: We should be able to delete this line but, it's not working in
        // all test cases.
        //
        // We increase ref_count when function is called and also if we take address
        // of it, but it's probably not handle all possible cases. So I leave this
        // problem open, basically it's not an issue to have invalid function
        // reference count, main goal is not to have zero ref count for function
        // which are used.
        ++fn->ref_count;
        break;
    }

    case SCOPE_ENTRY_TYPE: {
        ref->base.value.type        = cnt->builtin_types->t_type;
        ref->base.value.is_comptime = true;
        ref->base.value.addr_mode   = MIR_VAM_RVALUE;
        break;
    }

    case SCOPE_ENTRY_NAMED_SCOPE: {
        ref->base.value.type        = cnt->builtin_types->t_scope;
        ref->base.value.is_comptime = true;
        ref->base.value.addr_mode   = MIR_VAM_RVALUE;
        break;
    }

    case SCOPE_ENTRY_VARIANT: {
        MirVariant *variant = found->data.variant;
        BL_ASSERT(variant);
        MirType *type = variant->value->type;
        BL_ASSERT(type);
        type                        = create_type_ptr(cnt, type);
        ref->base.value.type        = type;
        ref->base.value.is_comptime = true;
        ref->base.value.addr_mode   = MIR_VAM_RVALUE;
        break;
    }

    case SCOPE_ENTRY_VAR: {
        MirVar *var = found->data.var;
        BL_ASSERT(var);
        MirType *type = var->value.type;
        BL_ASSERT(type);
        ++var->ref_count;
        // Check if we try get reference to incomplete structure type.
        if (type->kind == MIR_TYPE_TYPE) {
            MirType *t = MIR_CEV_READ_AS(MirType *, &var->value);
            BL_MAGIC_ASSERT(t);
            if (!ref->accept_incomplete_type && is_incomplete_struct_type(t)) {
                BL_ASSERT(t->user_id);
                return ANALYZE_RESULT(WAITING, t->user_id->hash);
            }
        } else if (!var->is_global && is_ref_out_of_fn_local_scope) {
            // Here we must handle situation when we try to reference variables
            // declared in parent functions of local functions. (We try to
            // implicitly capture those variables and this leads to invalid LLVM
            // IR.)
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_REFERENCE,
                        ref->base.node->location,
                        BUILDER_CUR_WORD,
                        "Attempt to reference variable from parent "
                        "function. This is not allowed.");

            builder_msg(BUILDER_MSG_NOTE,
                        0,
                        var->decl_node->location,
                        BUILDER_CUR_NONE,
                        "Variable declared here.");

            return ANALYZE_RESULT(FAILED, 0);
        }
        ref->base.value.type        = create_type_ptr(cnt, type);
        ref->base.value.is_comptime = var->value.is_comptime;
        if (type->kind == MIR_TYPE_FN_GROUP) {
            ref->base.value.addr_mode = MIR_VAM_RVALUE;
        } else {
            ref->base.value.addr_mode = var->is_mutable ? MIR_VAM_LVALUE : MIR_VAM_LVALUE_CONST;
        }
        break;
    }

    default:
        BL_ABORT("invalid scope entry kind");
    }

    ref->scope_entry = found;
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_decl_direct_ref(Context *cnt, MirInstrDeclDirectRef *ref)
{
    BL_ASSERT(ref->ref && "Missing declaration reference for direct ref.");
    BL_ASSERT(ref->ref->kind == MIR_INSTR_DECL_VAR && "Expected variable declaration.");
    if (!ref->ref->analyzed) return ANALYZE_RESULT(POSTPONE, 0);

    MirVar *var = ((MirInstrDeclVar *)ref->ref)->var;
    BL_ASSERT(var);
    ++var->ref_count;
    MirType *type = var->value.type;
    BL_ASSERT(type);

    type                        = create_type_ptr(cnt, type);
    ref->base.value.type        = type;
    ref->base.value.is_comptime = var->value.is_comptime;
    ref->base.value.addr_mode   = var->is_mutable ? MIR_VAM_LVALUE : MIR_VAM_LVALUE_CONST;
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_arg(Context UNUSED(*cnt), MirInstrArg *arg)
{
    MirFn *fn = arg->base.owner_block->owner_fn;
    BL_ASSERT(fn);

    MirType *type = mir_get_fn_arg_type(fn->type, arg->i);
    BL_ASSERT(type);
    arg->base.value.type = type;

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
    MirFn *abort_fn = lookup_builtin_fn(cnt, MIR_BUILTIN_ID_ABORT_FN);
    if (!abort_fn) return ANALYZE_RESULT(POSTPONE, 0);
    ++abort_fn->ref_count;
    unr->abort_fn = abort_fn;

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_test_cases(Context *cnt, MirInstrTestCases UNUSED(*tc))
{
    ID *missing = lookup_builtins_test_cases(cnt);
    if (missing) return ANALYZE_RESULT(WAITING, missing->hash);

    tc->base.value.type = cnt->builtin_types->t_TestCases_slice;

    if (cnt->testing.expected_test_count == 0) return ANALYZE_RESULT(PASSED, 0);
    testing_gen_meta(cnt);

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
    // resolve type
    if (!fn_proto->base.value.type) {
        MirType *     fn_type = NULL;
        AnalyzeResult result  = analyze_resolve_type(cnt, fn_proto->type, &fn_type);
        if (result.state != ANALYZE_PASSED) return result;

        // Analyze user defined type (this must be compared with infered type).
        if (fn_proto->user_type) {
            MirType *user_fn_type = NULL;
            result                = analyze_resolve_type(cnt, fn_proto->user_type, &user_fn_type);
            if (result.state != ANALYZE_PASSED) return result;

            if (!type_cmp(fn_type, user_fn_type)) {
                error_types(fn_type, user_fn_type, fn_proto->user_type->node, NULL);
            }
        }

        fn_proto->base.value.type = fn_type;
    }

    MirConstExprValue *value = &fn_proto->base.value;

    BL_ASSERT(value->type && "function has no valid type");
    BL_ASSERT(value->data);

    MirFn *fn = MIR_CEV_READ_AS(MirFn *, value);
    BL_MAGIC_ASSERT(fn);

    if (IS_FLAG(fn->flags, FLAG_TEST_FN)) {
        // We must wait for builtin types for test cases.
        ID *missing = lookup_builtins_test_cases(cnt);
        if (missing) return ANALYZE_RESULT(WAITING, missing->hash);
    }
    fn->type          = value->type;
    fn->type->user_id = fn->id;

    BL_ASSERT(fn->type);

    // Set builtin ID to type if there is one. We must store this information in function type
    // because we need it during call analyze pass, in this case function can be called via
    // pointer or directly, so we don't have access to MirFn instance.
    fn->type->data.fn.builtin_id = fn->builtin_id;

    if (fn->ret_tmp) {
        BL_ASSERT(fn->ret_tmp->kind == MIR_INSTR_DECL_VAR);
        ((MirInstrDeclVar *)fn->ret_tmp)->var->value.type = value->type->data.fn.ret_type;
    }

    TString *name_prefix = get_tmpstr();
    if (fn->decl_node) {
        Scope *owner_scope = fn->decl_node->owner_scope;
        BL_ASSERT(owner_scope);
        scope_get_full_name(name_prefix, owner_scope);
    }

    // Setup function linkage name, this will be later used by LLVM backend.
    if (fn->id && !fn->linkage_name) { // Has ID and has no linkage name specified.
        TString *full_name = builder_create_cached_str();
        tstring_appendf(full_name, "%s.%s", name_prefix->data, fn->id->str);
        if (IS_FLAG(fn->flags, FLAG_EXTERN) || IS_FLAG(fn->flags, FLAG_ENTRY) ||
            IS_FLAG(fn->flags, FLAG_EXPORT) || IS_FLAG(fn->flags, FLAG_INTRINSIC)) {
            fn->linkage_name = fn->id->str;
            fn->full_name    = full_name->data;
        } else {
            // Here we generate unique linkage name
            fn->linkage_name = gen_uq_name(full_name->data);
            fn->full_name    = full_name->data;
        }
    } else if (!fn->linkage_name) {
        // Anonymous function use implicit unique name.
        TString *full_name = get_tmpstr();
        tstring_appendf(full_name, "%s.%s", name_prefix->data, IMPL_FN_NAME);
        fn->linkage_name = gen_uq_name(full_name->data);
        fn->full_name    = fn->linkage_name;
        put_tmpstr(full_name);
    }
    put_tmpstr(name_prefix);
    BL_ASSERT(fn->linkage_name);
    if (!fn->full_name) fn->full_name = fn->linkage_name;

    if (IS_FLAG(fn->flags, FLAG_ENTRY)) {
        fn->ref_count = NO_REF_COUNTING;
    }

    // Check build entry function.
    if (IS_FLAG(fn->flags, FLAG_BUILD_ENTRY)) {
        if (fn->type->data.fn.args) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_ARG_COUNT,
                        fn_proto->base.node->location,
                        BUILDER_CUR_WORD,
                        "Build entry function cannot take arguments.");
            return ANALYZE_RESULT(FAILED, 0);
        }

        if (fn->type->data.fn.ret_type->kind != MIR_TYPE_VOID) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_TYPE,
                        fn_proto->base.node->location,
                        BUILDER_CUR_WORD,
                        "Build entry function cannot return value.");
            return ANALYZE_RESULT(FAILED, 0);
        }

        if (cnt->assembly->target->kind == ASSEMBLY_BUILD_PIPELINE) {
            cnt->assembly->vm_run.build_entry = fn;
        } else {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_DUPLICATE_ENTRY,
                        fn_proto->base.node->location,
                        BUILDER_CUR_WORD,
                        "More than one build entry per assembly is not allowed.");
            return ANALYZE_RESULT(FAILED, 0);
        }
    }

    BL_ASSERT(fn->linkage_name && "Function without linkage name!");

    if (IS_FLAG(fn->flags, FLAG_EXTERN)) {
        // lookup external function exec handle
        fn->dyncall.extern_entry = assembly_find_extern(cnt->assembly, fn->linkage_name);
        fn->fully_analyzed       = true;
    } else if (IS_FLAG(fn->flags, FLAG_INTRINSIC)) {
        // For intrinsics we use shorter names defined in user code, so we need username ->
        // internal name mapping in this case.
        const char *intrinsic_name = get_intrinsic(fn->linkage_name);
        if (!intrinsic_name) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_UNKNOWN_SYMBOL,
                        fn_proto->base.node->location,
                        BUILDER_CUR_WORD,
                        "Unknown compiler intrinsic '%s'.",
                        fn->linkage_name);

            return ANALYZE_RESULT(FAILED, 0);
        }

        fn->dyncall.extern_entry = assembly_find_extern(cnt->assembly, intrinsic_name);
        fn->fully_analyzed       = true;
    } else if (cnt->assembly->target->kind == ASSEMBLY_BUILD_PIPELINE &&
               IS_FLAG(fn->flags, FLAG_ENTRY)) {
        // IGNORE BODY OF ENTRY FUNCTION WHEN WE BUILD 'BUILD ENTRY' ASSEMBLY.
    } else {
        // Add entry block of the function into analyze queue.
        MirInstr *entry_block = (MirInstr *)fn->first_block;
        if (!entry_block) {
            // INCOMPLETE: not the best place to do this check, move into ast
            // generation later
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_EXPECTED_BODY,
                        fn_proto->base.node->location,
                        BUILDER_CUR_WORD,
                        "Missing function body.");
            return ANALYZE_RESULT(FAILED, 0);
        }

        analyze_push_front(cnt, entry_block);
    }

    // Store test case for later use if it's going to be tested.
    if (IS_FLAG(fn->flags, FLAG_TEST_FN)) {
        BL_ASSERT(fn->id && "Test case without name!");

        if (fn->type->data.fn.args != NULL) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_ARG_COUNT,
                        fn_proto->base.node->location,
                        BUILDER_CUR_WORD,
                        "Test case function cannot have arguments.");

            return ANALYZE_RESULT(FAILED, 0);
        }

        if (fn->type->data.fn.ret_type->kind != MIR_TYPE_VOID) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_UNEXPECTED_RETURN,
                        fn_proto->base.node->location,
                        BUILDER_CUR_WORD,
                        "Test case function cannot return value.");

            return ANALYZE_RESULT(FAILED, 0);
        }

        testing_add_test_case(cnt, fn);

        // To be sure that test case will be generated in LLVM.
        fn->emit_llvm = true;
        ++fn->ref_count;
    }

    if (IS_FLAG(fn->flags, FLAG_EXPORT)) {
        fn->emit_llvm = true;
        ++fn->ref_count;
    }

    if (fn->id) {
        if (fn->entry->kind == SCOPE_ENTRY_VOID) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_NAME,
                        fn->decl_node->location,
                        BUILDER_CUR_WORD,
                        "Functions cannot be explicitly unnamed.");
            return ANALYZE_RESULT(FAILED, 0);
        }
        commit_fn(cnt, fn);
    }
    return ANALYZE_RESULT(PASSED, 0);
}

int _group_compare(const void *_first, const void *_second)
{
    MirFn *first  = *(MirFn **)_first;
    MirFn *second = *(MirFn **)_second;
    BL_MAGIC_ASSERT(first);
    BL_MAGIC_ASSERT(second);
    BL_ASSERT(first->type && second->type);
    return first->type->data.fn.argument_hash - second->type->data.fn.argument_hash;
}

AnalyzeResult analyze_instr_fn_group(Context *cnt, MirInstrFnGroup *group)
{
    TSmallArray_InstrPtr *variants = group->variants;
    BL_ASSERT(variants);
    const usize vc = variants->size;
    BL_ASSERT(vc);
    for (usize i = 0; i < vc; ++i) {
        MirInstr *variant_ref = variants->data[i];
        if (!variant_ref->analyzed) return ANALYZE_RESULT(POSTPONE, 0);
    }
    AnalyzeResult        result        = ANALYZE_RESULT(PASSED, 0);
    TSmallArray_TypePtr *variant_types = create_sarr(TSmallArray_TypePtr, cnt->assembly);
    TSmallArray_FnPtr *  variant_fns   = create_sarr(TSmallArray_FnPtr, cnt->assembly);
    TSmallArray_FnPtr    validation_queue;
    tsa_init(&validation_queue);
    tsa_resize_TypePtr(variant_types, vc);
    tsa_resize_FnPtr(variant_fns, vc);
    tsa_resize_FnPtr(&validation_queue, vc);
    for (usize i = 0; i < vc; ++i) {
        MirInstr **variant_ref = &variants->data[i];
        if (analyze_slot(cnt, &analyze_slot_conf_basic, variant_ref, NULL) != ANALYZE_PASSED) {
            result = ANALYZE_RESULT(FAILED, 0);
            goto FINALLY;
        }
        MirInstr *variant = *variant_ref;
        if (variant->value.type->kind != MIR_TYPE_FN) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_EXPECTED_FUNC,
                        variant->node->location,
                        BUILDER_CUR_WORD,
                        "Expected a function name.");
            result = ANALYZE_RESULT(FAILED, 0);
            goto FINALLY;
        }

        BL_ASSERT(mir_is_comptime(variant));
        MirFn *fn = MIR_CEV_READ_AS(MirFn *, &variant->value);
        BL_MAGIC_ASSERT(fn);
        BL_ASSERT(fn->type && "Missing function type!");
        variant_fns->data[i]     = fn;
        variant_types->data[i]   = fn->type;
        validation_queue.data[i] = fn;
        if (variant->kind == MIR_INSTR_FN_PROTO) {
            ++fn->ref_count;
        }
    }
    // Validate group.
    qsort(&validation_queue.data[0], validation_queue.size, sizeof(MirFn *), &_group_compare);
    MirFn *prev_fn = NULL;
    for (usize i = validation_queue.size; i-- > 0;) {
        MirFn *   it = validation_queue.data[i];
        const u64 h1 = it->type->data.fn.argument_hash;
        const u64 h2 = prev_fn ? prev_fn->type->data.fn.argument_hash : 0;
        if (h1 == h2) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_AMBIGUOUS,
                        it->decl_node->location,
                        BUILDER_CUR_WORD,
                        "Function overload is ambiguous in group.");
            builder_msg(BUILDER_MSG_NOTE,
                        0,
                        group->base.node->location,
                        BUILDER_CUR_WORD,
                        "Group defined here:");
        }
        prev_fn = it;
    }
    group->base.value.type = create_type_fn_group(cnt, NULL, variant_types);
    MIR_CEV_WRITE_AS(
        MirFnGroup *, &group->base.value, create_fn_group(cnt, group->base.node, variant_fns));
FINALLY:
    tsa_terminate(&validation_queue);
    return result;
}

AnalyzeResult analyze_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
    BL_ASSERT(br->cond && br->then_block && br->else_block);
    BL_ASSERT(br->cond->analyzed);

    if (analyze_slot(cnt, &analyze_slot_conf_default, &br->cond, cnt->builtin_types->t_bool) !=
        ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    // PERFORMANCE: When condition is known in compile time, we can discard
    // whole else/then block based on condition resutl. It is not possible
    // because we don't have tracked down execution tree for now.
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_br(Context UNUSED(*cnt), MirInstrBr *br)
{
    BL_ASSERT(br->then_block);
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_switch(Context *cnt, MirInstrSwitch *sw)
{
    if (analyze_slot(cnt, &analyze_slot_conf_basic, &sw->value, NULL) != ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    MirType *expected_case_type = sw->value->value.type;
    BL_ASSERT(expected_case_type);

    if (expected_case_type->kind != MIR_TYPE_INT && expected_case_type->kind != MIR_TYPE_ENUM) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    sw->value->node->location,
                    BUILDER_CUR_WORD,
                    "Invalid type of switch expression. Only integer types and "
                    "enums can be used.");

        return ANALYZE_RESULT(FAILED, 0);
    }

    if (!sw->cases->size) {
        builder_msg(BUILDER_MSG_WARNING,
                    0,
                    sw->base.node->location,
                    BUILDER_CUR_WORD,
                    "Empty switch statement.");

        return ANALYZE_RESULT(PASSED, 0);
    }

    MirSwitchCase *c;
    THashTable *   presented = &cnt->analyze.presented_switch_cases;
    thtbl_clear(presented);
    for (usize i = sw->cases->size; i-- > 0;) {
        c = &sw->cases->data[i];

        if (!mir_is_comptime(c->on_value)) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_EXPECTED_COMPTIME,
                        c->on_value->node->location,
                        BUILDER_CUR_WORD,
                        "Switch case value must be compile-time known.");
            return ANALYZE_RESULT(FAILED, 0);
        }

        if (analyze_slot(cnt, &analyze_slot_conf_basic, &c->on_value, expected_case_type) !=
            ANALYZE_PASSED) {
            return ANALYZE_RESULT(FAILED, 0);
        }

        { // validate value
            const s64 v    = MIR_CEV_READ_AS(s64, &c->on_value->value);
            TIterator iter = thtbl_find(presented, v);
            TIterator end  = thtbl_end(presented);
            if (!TITERATOR_EQUAL(iter, end)) {
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_DUPLICIT_SWITCH_CASE,
                            c->on_value->node->location,
                            BUILDER_CUR_WORD,
                            "Switch already contains case for this value!");

                MirSwitchCase *ce = thtbl_iter_peek_value(MirSwitchCase *, iter);
                builder_msg(BUILDER_MSG_NOTE,
                            0,
                            ce->on_value->node->location,
                            BUILDER_CUR_WORD,
                            "Same value found here.");
                return ANALYZE_RESULT(FAILED, 0);
            }
            thtbl_insert(presented, (u64)v, c);
        }
    }

    s64 expected_case_count = expected_case_type->kind == MIR_TYPE_ENUM
                                  ? (s64)expected_case_type->data.enm.variants->size
                                  : -1;

    if ((expected_case_count > (s64)sw->cases->size) && !sw->has_user_defined_default) {
        builder_msg(BUILDER_MSG_WARNING,
                    0,
                    sw->base.node->location,
                    BUILDER_CUR_WORD,
                    "Switch does not handle all possible enumerator values.");

        BL_ASSERT(expected_case_type->kind == MIR_TYPE_ENUM);
        MirVariant *variant;
        TSA_FOREACH(expected_case_type->data.enm.variants, variant)
        {
            bool hit = false;
            for (usize i2 = 0; i2 < sw->cases->size; ++i2) {
                c                       = &sw->cases->data[i2];
                const s64 on_value      = MIR_CEV_READ_AS(s64, &c->on_value->value);
                const s64 variant_value = MIR_CEV_READ_AS(s64, variant->value);
                if (on_value == variant_value) {
                    hit = true;
                    break;
                }
            }

            if (!hit) {
                builder_msg(BUILDER_MSG_NOTE,
                            0,
                            NULL,
                            BUILDER_CUR_NONE,
                            "Missing case for: %s",
                            variant->id->str);
            }
        }
    }
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_load(Context UNUSED(*cnt), MirInstrLoad *load)
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
    load->base.value.type        = type;
    load->base.value.is_comptime = src->value.is_comptime;
    load->base.value.addr_mode   = src->value.addr_mode;
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn)
{
    BL_ASSERT(type_fn->ret_type ? type_fn->ret_type->analyzed : true);

    bool is_vargs         = false;
    bool has_default_args = false;

    TSmallArray_ArgPtr *args = NULL;
    if (type_fn->args) {
        const usize argc = type_fn->args->size;

        for (usize i = 0; i < argc; ++i) {
            BL_ASSERT(type_fn->args->data[i]->kind == MIR_INSTR_DECL_ARG);
            MirInstrDeclArg *decl_arg = (MirInstrDeclArg *)type_fn->args->data[i];
            MirArg *         arg      = decl_arg->arg;
            if (arg->value && !arg->value->analyzed) {
                return ANALYZE_RESULT(POSTPONE, 0);
            }
        }

        args = create_sarr(TSmallArray_ArgPtr, cnt->assembly);
        for (usize i = 0; i < argc; ++i) {
            BL_ASSERT(type_fn->args->data[i]->kind == MIR_INSTR_DECL_ARG);
            MirInstrDeclArg **arg_ref = (MirInstrDeclArg **)&type_fn->args->data[i];
            BL_ASSERT((*arg_ref)->base.value.is_comptime);

            if (analyze_slot(cnt, &analyze_slot_conf_basic, (MirInstr **)arg_ref, NULL) !=
                ANALYZE_PASSED) {
                return ANALYZE_RESULT(FAILED, 0);
            }

            MirArg *arg = (*arg_ref)->arg;
            BL_ASSERT(arg);
            is_vargs         = arg->type->kind == MIR_TYPE_VARGS ? true : is_vargs;
            has_default_args = arg->value ? true : has_default_args;
            if (is_vargs && i != type_fn->args->size - 1) {
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_INVALID_TYPE,
                            arg->decl_node->location,
                            BUILDER_CUR_WORD,
                            "VArgs function argument must be last in "
                            "the argument list.");
                return ANALYZE_RESULT(FAILED, 0);
            }
            if (is_vargs && has_default_args) {
                MirInstr *arg_prev = type_fn->args->data[i - 1];
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_INVALID_TYPE,
                            arg_prev->node->location,
                            BUILDER_CUR_WORD,
                            "Argument with default value cannot be used with VArgs "
                            "presented in the function argument list.");
                return ANALYZE_RESULT(FAILED, 0);
            }
            if (!arg->value && has_default_args) {
                MirInstr *arg_prev = type_fn->args->data[i - 1];
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_INVALID_TYPE,
                            arg_prev->node->location,
                            BUILDER_CUR_WORD,
                            "All arguments with default value must be listed last "
                            "in the function argument list. Before arguments "
                            "without default value.");
                return ANALYZE_RESULT(FAILED, 0);
            }
            tsa_push_ArgPtr(args, arg);
        }
    }

    MirType *ret_type = NULL;
    if (type_fn->ret_type) {
        if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_fn->ret_type, NULL) !=
            ANALYZE_PASSED) {
            return ANALYZE_RESULT(FAILED, 0);
        }

        BL_ASSERT(type_fn->ret_type->value.is_comptime);
        ret_type = MIR_CEV_READ_AS(MirType *, &type_fn->ret_type->value);
        BL_MAGIC_ASSERT(ret_type);
    }

    MIR_CEV_WRITE_AS(MirType *,
                     &type_fn->base.value,
                     create_type_fn(cnt, NULL, ret_type, args, is_vargs, has_default_args));
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_type_fn_group(Context *cnt, MirInstrTypeFnGroup *group)
{
    TSmallArray_InstrPtr *variants = group->variants;
    BL_ASSERT(variants);
    const usize varc = variants->size;
    if (varc == 0) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    group->base.node->location,
                    BUILDER_CUR_WORD,
                    "Function group type must contain one function type at least.");
        return ANALYZE_RESULT(FAILED, 0);
    }
    TSmallArray_TypePtr *variant_types = create_sarr(TSmallArray_TypePtr, cnt->assembly);
    tsa_resize_TypePtr(variant_types, varc);
    for (usize i = 0; i < varc; ++i) {
        MirInstr **variant_ref = &variants->data[i];
        if (analyze_slot(cnt, &analyze_slot_conf_basic, variant_ref, NULL) != ANALYZE_PASSED) {
            return ANALYZE_RESULT(FAILED, 0);
        }
        MirInstr *variant = *variant_ref;
        BL_ASSERT(variant->value.type);
        if (variant->value.type->kind != MIR_TYPE_TYPE) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_TYPE,
                        variant->node->location,
                        BUILDER_CUR_WORD,
                        "Function group type variant is supposed to be function type.");
            return ANALYZE_RESULT(FAILED, 0);
        }
        MirType *variant_type = MIR_CEV_READ_AS(MirType *, &variant->value);
        BL_MAGIC_ASSERT(variant_type);
        if (variant_type->kind != MIR_TYPE_FN) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_TYPE,
                        variant->node->location,
                        BUILDER_CUR_WORD,
                        "Function group type variant is supposed to be function type.");
            return ANALYZE_RESULT(FAILED, 0);
        }
        variant_types->data[i] = variant_type;
    }
    MIR_CEV_WRITE_AS(MirType *, &group->base.value, create_type_fn_group(cnt, NULL, variant_types));
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_decl_member(Context *cnt, MirInstrDeclMember *decl)
{
    MirMember *           member    = decl->member;
    TSmallArray_InstrPtr *tags      = decl->tags;
    s32                   tag_group = 0;

    // Analyze struct member tags.
    if (tags) {
        for (usize i = 0; i < tags->size; ++i) {
            MirInstr **tag = &tags->data[i];
            if (analyze_slot(cnt, &analyze_slot_conf_default, tag, cnt->builtin_types->t_s32) !=
                ANALYZE_PASSED) {
                return ANALYZE_RESULT(FAILED, 0);
            }

            if (!mir_is_comptime(*tag)) {
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_EXPECTED_CONST,
                            (*tag)->node->location,
                            BUILDER_CUR_WORD,
                            "Struct member tag must be compile-time constant.");
                return ANALYZE_RESULT(FAILED, 0);
            }

            // NOTE:
            // Tag values are used in the same way as flags, here we produce
            // mergin of all into one integer value.
            tag_group |= MIR_CEV_READ_AS(s32, &(*tag)->value);
        }
    }

    member->tags = tag_group;

    if (analyze_slot(cnt, &analyze_slot_conf_basic, &decl->type, NULL) != ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    // NOTE: Members will be provided by instr type struct because we need to
    // know right ordering of members inside structure layout. (index and llvm
    // element offet need to be calculated)
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_decl_variant(Context *cnt, MirInstrDeclVariant *variant_instr)
{
    MirVariant *variant = variant_instr->variant;
    BL_ASSERT(variant && "Missing variant.");

    if (variant_instr->value) {
        // User defined initialization value.
        if (!mir_is_comptime(variant_instr->value)) {
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

        // Setup value.
        variant_instr->variant->value = &variant_instr->value->value;
    } else {
        // CLENUP: Automatic initialization value is set in parser, maybe we will
        // prefer to do automatic initialization here instead of doing so in parser
        // pass.
        BL_UNIMPLEMENTED;
    }
    if (variant->entry->kind == SCOPE_ENTRY_VOID) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_NAME,
                    variant_instr->base.node->location,
                    BUILDER_CUR_WORD,
                    "Enum variant cannot be explicitly unnamed.");
        return ANALYZE_RESULT(FAILED, 0);
    }
    commit_variant(cnt, variant);
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_decl_arg(Context *cnt, MirInstrDeclArg *decl)
{
    MirArg *arg = decl->arg;
    BL_ASSERT(arg);
    if (decl->type) {
        // Variable type is explicitly defined.
        if (decl->type->kind == MIR_INSTR_CALL) {
            AnalyzeResult result = analyze_resolve_type(cnt, decl->type, &arg->type);
            if (result.state != ANALYZE_PASSED) return result;
        } else {
            if (analyze_slot(cnt, &analyze_slot_conf_basic, &decl->type, NULL) != ANALYZE_PASSED) {
                return ANALYZE_RESULT(FAILED, 0);
            }
            arg->type = MIR_CEV_READ_AS(MirType *, &decl->type->value);
            BL_MAGIC_ASSERT(arg->type);
        }
        // @NOTE: Argument default value is generated as an implicit global constant
        // variable with proper expected type defined, so there is no need to do any type
        // validation with user type, variable analyze pass will do it for us.*/
    } else {
        // There is no explicitly defined argument type, but we have default argument value
        // to infer type from.
        BL_ASSERT(arg->value);
        if (!arg->value->analyzed) {
            // @PERFORMANCE: WAITING is preferred here, but we don't have ID to wait
            // for.
            return ANALYZE_RESULT(POSTPONE, 0);
        }
        if (arg->value->kind == MIR_INSTR_DECL_VAR) {
            MirVar *var = ((MirInstrDeclVar *)arg->value)->var;
            BL_ASSERT(var);
            if (!var->analyzed) return ANALYZE_RESULT(POSTPONE, 0);
            arg->type = var->value.type;
        } else {
            arg->type = arg->value->value.type;
        }
    }
    BL_ASSERT(arg->type && "Invalid argument type!");
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_type_struct(Context *cnt, MirInstrTypeStruct *type_struct)
{
    TSmallArray_MemberPtr *members   = NULL;
    MirType *              base_type = NULL;
    const bool             is_union  = type_struct->is_union;

    if (type_struct->members) {
        MirInstr **         member_instr;
        MirInstrDeclMember *decl_member;
        MirType *           member_type;
        const usize         memc = type_struct->members->size;
        members                  = create_sarr(TSmallArray_MemberPtr, cnt->assembly);
        for (usize i = 0; i < memc; ++i) {
            member_instr = &type_struct->members->data[i];
            if (analyze_slot(cnt, &analyze_slot_conf_basic, member_instr, NULL) != ANALYZE_PASSED) {
                return ANALYZE_RESULT(FAILED, 0);
            }
            decl_member = (MirInstrDeclMember *)*member_instr;
            BL_ASSERT(decl_member->base.kind == MIR_INSTR_DECL_MEMBER);
            BL_ASSERT(mir_is_comptime(&decl_member->base));

            // solve member type
            member_type = MIR_CEV_READ_AS(MirType *, &decl_member->type->value);
            BL_MAGIC_ASSERT(member_type);
            if (member_type->kind == MIR_TYPE_FN) {
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_INVALID_TYPE,
                            (*member_instr)->node->location,
                            BUILDER_CUR_WORD,
                            "Invalid type of the structure member, functions can "
                            "be referenced only by pointers.");
                return ANALYZE_RESULT(FAILED, 0);
            }
            if (member_type->kind == MIR_TYPE_TYPE) {
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_INVALID_TYPE,
                            decl_member->type->node->location,
                            BUILDER_CUR_WORD,
                            "Generic 'type' cannot be used as struct member type.");
                return ANALYZE_RESULT(FAILED, 0);
            }

            // setup and provide member
            MirMember *member = decl_member->member;
            BL_ASSERT(member);
            member->type            = member_type;
            member->index           = (s64)i;
            member->is_parent_union = is_union;

            if (member->is_base) {
                BL_ASSERT(!base_type && "Structure cannot have more than one base type!");
                base_type = member_type;
            }

            tsa_push_MemberPtr(members, member);
            commit_member(cnt, member);
        }
    }

    MirType *result_type = NULL;

    if (type_struct->fwd_decl) {
        // Type has fwd declaration. In this case we set all desired information
        // about struct type into previously created forward declaration.
        result_type = complete_type_struct(cnt,
                                           type_struct->fwd_decl,
                                           type_struct->scope,
                                           members,
                                           base_type,
                                           type_struct->is_packed,
                                           type_struct->is_union,
                                           type_struct->is_multiple_return_type);

        analyze_notify_provided(cnt, result_type->user_id->hash);
    } else {
        result_type = create_type_struct(cnt,
                                         MIR_TYPE_STRUCT,
                                         type_struct->id,
                                         type_struct->scope,
                                         members,
                                         base_type,
                                         is_union,
                                         type_struct->is_packed,
                                         type_struct->is_multiple_return_type);
    }

    BL_ASSERT(result_type);
    MIR_CEV_WRITE_AS(MirType *, &type_struct->base.value, result_type);
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_type_slice(Context *cnt, MirInstrTypeSlice *type_slice)
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

    BL_ASSERT(mir_is_comptime(type_slice->elem_type) && "This should be an error");
    MirType *elem_type = MIR_CEV_READ_AS(MirType *, &type_slice->elem_type->value);
    BL_MAGIC_ASSERT(elem_type);

    if (elem_type->kind == MIR_TYPE_TYPE) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    type_slice->elem_type->node->location,
                    BUILDER_CUR_WORD,
                    "Generic 'type' cannot be used as slice base type.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    elem_type = create_type_ptr(cnt, elem_type);

    MIR_CEV_WRITE_AS(
        MirType *, &type_slice->base.value, CREATE_TYPE_STRUCT_SLICE(cnt, id, elem_type));

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_type_dynarr(Context *cnt, MirInstrTypeDynArr *type_dynarr)
{
    // We need TypeInfo initialized because it's needed as member of dynamic array type!
    ID *missing_rtti_type_id = lookup_builtins_rtti(cnt);
    if (missing_rtti_type_id) {
        return ANALYZE_RESULT(WAITING, missing_rtti_type_id->hash);
    }

    BL_ASSERT(type_dynarr->elem_type);

    if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_dynarr->elem_type, NULL) !=
        ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    ID *id = NULL;
    if (type_dynarr->base.node && type_dynarr->base.node->kind == AST_IDENT) {
        id = &type_dynarr->base.node->data.ident.id;
    }

    if (type_dynarr->elem_type->value.type->kind != MIR_TYPE_TYPE) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    type_dynarr->elem_type->node->location,
                    BUILDER_CUR_WORD,
                    "Expected type.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    BL_ASSERT(mir_is_comptime(type_dynarr->elem_type) && "This should be an error");
    MirType *elem_type = MIR_CEV_READ_AS(MirType *, &type_dynarr->elem_type->value);
    BL_MAGIC_ASSERT(elem_type);

    if (elem_type->kind == MIR_TYPE_TYPE) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    type_dynarr->elem_type->node->location,
                    BUILDER_CUR_WORD,
                    "Generic 'type' cannot be used as dynamic array base type.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    elem_type = create_type_ptr(cnt, elem_type);

    MIR_CEV_WRITE_AS(
        MirType *, &type_dynarr->base.value, create_type_struct_dynarr(cnt, id, elem_type));

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_type_vargs(Context *cnt, MirInstrTypeVArgs *type_vargs)
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

        BL_ASSERT(mir_is_comptime(type_vargs->elem_type) && "This should be an error");
        elem_type = MIR_CEV_READ_AS(MirType *, &type_vargs->elem_type->value);
        BL_MAGIC_ASSERT(elem_type);
    } else {
        // use Any
        elem_type = lookup_builtin_type(cnt, MIR_BUILTIN_ID_ANY);
        if (!elem_type) return ANALYZE_RESULT(WAITING, builtin_ids[MIR_BUILTIN_ID_ANY].hash);
    }
    BL_ASSERT(elem_type);
    elem_type = create_type_ptr(cnt, elem_type);
    MIR_CEV_WRITE_AS(
        MirType *, &type_vargs->base.value, CREATE_TYPE_STRUCT_VARGS(cnt, NULL, elem_type));

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr)
{
    BL_ASSERT(type_arr->base.value.type);
    BL_ASSERT(type_arr->elem_type->analyzed);

    if (analyze_slot(cnt, &analyze_slot_conf_default, &type_arr->len, cnt->builtin_types->t_s64) !=
        ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_arr->elem_type, NULL) != ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    // len
    if (!mir_is_comptime(type_arr->len)) {
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

    const s64 len = MIR_CEV_READ_AS(s64, &type_arr->len->value);
    if (len == 0) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_ARR_SIZE,
                    type_arr->len->node->location,
                    BUILDER_CUR_WORD,
                    "Array size cannot be 0.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    // elem type
    BL_ASSERT(mir_is_comptime(type_arr->elem_type));

    MirType *elem_type = MIR_CEV_READ_AS(MirType *, &type_arr->elem_type->value);
    BL_MAGIC_ASSERT(elem_type);

    if (elem_type->kind == MIR_TYPE_TYPE) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    type_arr->elem_type->node->location,
                    BUILDER_CUR_WORD,
                    "Generic 'type' cannot be used as array element type.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    MIR_CEV_WRITE_AS(
        MirType *, &type_arr->base.value, create_type_array(cnt, type_arr->id, elem_type, len));
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_type_enum(Context *cnt, MirInstrTypeEnum *type_enum)
{
    TSmallArray_InstrPtr *variant_instrs = type_enum->variants;
    Scope *               scope          = type_enum->scope;
    BL_ASSERT(variant_instrs);
    BL_ASSERT(scope);
    BL_ASSERT(variant_instrs->size);
    // Validate and setup enum base type.
    MirType *base_type;
    if (type_enum->base_type) {
        base_type = MIR_CEV_READ_AS(MirType *, &type_enum->base_type->value);
        BL_MAGIC_ASSERT(base_type);

        // Enum type must be integer!
        if (base_type->kind != MIR_TYPE_INT) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_TYPE,
                        type_enum->base_type->node->location,
                        BUILDER_CUR_WORD,
                        "Base type of enumerator must be an integer type.");
            return ANALYZE_RESULT(FAILED, 0);
        }
    } else {
        // Use s32 by default.
        base_type = cnt->builtin_types->t_s32;
    }
    BL_ASSERT(base_type && "Invalid enum base type.");
    TSmallArray_VariantPtr *variants = create_sarr(TSmallArray_VariantPtr, cnt->assembly);
    // Iterate over all enum variants and validate them.
    MirInstr *  it;
    MirVariant *variant;
    TSA_FOREACH(variant_instrs, it)
    {
        MirInstrDeclVariant *variant_instr = (MirInstrDeclVariant *)it;
        variant                            = variant_instr->variant;
        BL_ASSERT(variant && "Missing variant.");
        if (analyze_slot(cnt, &analyze_slot_conf_default, &variant_instr->value, base_type) !=
            ANALYZE_PASSED) {
            return ANALYZE_RESULT(FAILED, 0);
        }
        tsa_push_VariantPtr(variants, variant);
    }

    MIR_CEV_WRITE_AS(MirType *,
                     &type_enum->base.value,
                     create_type_enum(cnt, type_enum->id, scope, base_type, variants));
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr)
{
    BL_ASSERT(type_ptr->type);

    if (analyze_slot(cnt, &analyze_slot_conf_basic, &type_ptr->type, NULL) != ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    if (!mir_is_comptime(type_ptr->type)) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    type_ptr->type->node->location,
                    BUILDER_CUR_WORD,
                    "Expected compile time known type after '*' pointer type declaration.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    if (type_ptr->type->value.type->kind != MIR_TYPE_TYPE) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    type_ptr->type->node->location,
                    BUILDER_CUR_WORD,
                    "Expected type name.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    MirType *src_type_value = MIR_CEV_READ_AS(MirType *, &type_ptr->type->value);
    BL_MAGIC_ASSERT(src_type_value);

    if (src_type_value->kind == MIR_TYPE_TYPE) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    type_ptr->base.node->location,
                    BUILDER_CUR_WORD,
                    "Cannot create pointer to type.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    MIR_CEV_WRITE_AS(MirType *, &type_ptr->base.value, create_type_ptr(cnt, src_type_value));
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_binop(Context *cnt, MirInstrBinop *binop)
{
//*********************************************************************************************/
#define is_valid(_type, _op)                                                                       \
    (((_type)->kind == MIR_TYPE_INT) || ((_type)->kind == MIR_TYPE_NULL) ||                        \
     ((_type)->kind == MIR_TYPE_REAL) || ((_type)->kind == MIR_TYPE_PTR) ||                        \
     ((_type)->kind == MIR_TYPE_BOOL && ast_binop_is_logic(_op)) ||                                \
     ((_type)->kind == MIR_TYPE_ENUM && (_op == BINOP_EQ || _op == BINOP_NEQ)))
    //*********************************************************************************************/

    { // Handle type propagation.
        MirType *lhs_type = binop->lhs->value.type;
        MirType *rhs_type = binop->rhs->value.type;

        if (is_load_needed(binop->lhs)) lhs_type = mir_deref_type(lhs_type);
        if (is_load_needed(binop->rhs)) rhs_type = mir_deref_type(rhs_type);

        const bool lhs_is_null = binop->lhs->value.type->kind == MIR_TYPE_NULL;
        const bool can_propagate_RtoL =
            can_impl_cast(lhs_type, rhs_type) || is_instr_type_volatile(binop->lhs);

        if (can_propagate_RtoL) {
            // Propagate right hand side expression type to the left.
            if (analyze_slot(cnt, &analyze_slot_conf_default, &binop->lhs, rhs_type) !=
                ANALYZE_PASSED)
                return ANALYZE_RESULT(FAILED, 0);

            if (analyze_slot(cnt, &analyze_slot_conf_basic, &binop->rhs, NULL) != ANALYZE_PASSED)
                return ANALYZE_RESULT(FAILED, 0);
        } else {
            // Propagate left hand side expression type to the right.
            if (analyze_slot(cnt, &analyze_slot_conf_basic, &binop->lhs, NULL) != ANALYZE_PASSED)
                return ANALYZE_RESULT(FAILED, 0);

            if (analyze_slot(cnt,
                             lhs_is_null ? &analyze_slot_conf_basic : &analyze_slot_conf_default,
                             &binop->rhs,
                             lhs_is_null ? NULL : binop->lhs->value.type) != ANALYZE_PASSED)
                return ANALYZE_RESULT(FAILED, 0);

            if (lhs_is_null) {
                if (analyze_stage_set_null(cnt, &binop->lhs, binop->rhs->value.type, false) !=
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
        error_types(
            lhs->value.type, rhs->value.type, binop->base.node, "Invalid operation for %s type.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    MirType *type = ast_binop_is_logic(binop->op) ? cnt->builtin_types->t_bool : lhs->value.type;
    BL_ASSERT(type);
    binop->base.value.type = type;

    // when binary operation has lhs and rhs values known in compile it is known
    // in compile time also
    binop->base.value.is_comptime = lhs->value.is_comptime && rhs->value.is_comptime;
    binop->base.value.addr_mode   = MIR_VAM_RVALUE;
    binop->volatile_type          = is_instr_type_volatile(lhs) && is_instr_type_volatile(rhs);

    return ANALYZE_RESULT(PASSED, 0);
#undef is_valid
}

AnalyzeResult analyze_instr_call_loc(Context *cnt, MirInstrCallLoc *loc)
{
    ID *missing = lookup_builtins_code_loc(cnt);
    if (missing) return ANALYZE_RESULT(WAITING, missing->hash);
    loc->base.value.type = cnt->builtin_types->t_CodeLocation_ptr;
    if (!loc->call_location) return ANALYZE_RESULT(PASSED, 0);

    MirType *type = cnt->builtin_types->t_CodeLocation;
    MirVar * var  = create_var_impl(cnt, IMPL_CALL_LOC, type, false, true, true);
    vm_alloc_global(cnt->vm, cnt->assembly, var);

    VMStackPtr dest           = vm_read_var(cnt->vm, var);
    MirType *  dest_file_type = mir_get_struct_elem_type(type, 0);
    VMStackPtr dest_file      = vm_get_struct_elem_ptr(cnt->assembly, type, dest, 0);
    MirType *  dest_line_type = mir_get_struct_elem_type(type, 1);
    VMStackPtr dest_line      = vm_get_struct_elem_ptr(cnt->assembly, type, dest, 1);
    MirType *  dest_hash_type = mir_get_struct_elem_type(type, 2);
    VMStackPtr dest_hash      = vm_get_struct_elem_ptr(cnt->assembly, type, dest, 2);

    const char *filepath = loc->call_location->unit->filepath;
    BL_ASSERT(filepath);

    TString *str_hash = get_tmpstr();
    tstring_append(str_hash, filepath);
    char str_line[10];
    snprintf(str_line, TARRAY_SIZE(str_line), "%d", loc->call_location->line);
    tstring_append(str_hash, str_line);
    const u32 hash = thash_from_str(str_hash->data);
    put_tmpstr(str_hash);

    vm_write_string(cnt->vm, dest_file_type, dest_file, filepath, strlen(filepath));
    vm_write_int(dest_line_type, dest_line, (u64)loc->call_location->line);
    vm_write_int(dest_hash_type, dest_hash, (u64)hash);

    loc->meta_var = var;
    loc->hash     = hash;
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_unop(Context *cnt, MirInstrUnop *unop)
{
    MirType *expected_type = unop->op == UNOP_NOT ? cnt->builtin_types->t_bool : NULL;
    const AnalyzeSlotConfig *conf =
        unop->op == UNOP_NOT ? &analyze_slot_conf_default : &analyze_slot_conf_basic;

    if (analyze_slot(cnt, conf, &unop->expr, expected_type) != ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    BL_ASSERT(unop->expr && unop->expr->analyzed);

    MirType *expr_type = unop->expr->value.type;
    BL_ASSERT(expr_type);

    switch (unop->op) {
    case UNOP_NOT: {
        if (expr_type->kind != MIR_TYPE_BOOL) return ANALYZE_RESULT(FAILED, 0);
        break;
    }

    case UNOP_BIT_NOT: {
        if (expr_type->kind != MIR_TYPE_INT) {
            char tmp[256];
            mir_type_to_str(tmp, 256, expr_type, true);

            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_TYPE,
                        unop->base.node->location,
                        BUILDER_CUR_AFTER,
                        "Invalid operation for type '%s'. This operation "
                        "is valid for integer types only.",
                        tmp);

            return ANALYZE_RESULT(FAILED, 0);
        }
        break;
    }

    case UNOP_POS:
    case UNOP_NEG: {
        if (expr_type->kind != MIR_TYPE_INT && expr_type->kind != MIR_TYPE_REAL) {
            char tmp[256];
            mir_type_to_str(tmp, 256, expr_type, true);

            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_TYPE,
                        unop->base.node->location,
                        BUILDER_CUR_AFTER,
                        "Invalid operation for type '%s'. This operation "
                        "is valid for integer or real types only.",
                        tmp);

            return ANALYZE_RESULT(FAILED, 0);
        }
        break;
    }

    default:
        break;
    }

    unop->base.value.type        = expr_type;
    unop->base.value.is_comptime = unop->expr->value.is_comptime;
    unop->base.value.addr_mode   = unop->expr->value.addr_mode;

    unop->volatile_type = is_instr_type_volatile(unop->expr);

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_const(Context UNUSED(*cnt), MirInstrConst *cnst)
{
    BL_ASSERT(cnst->base.value.type);
    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_ret(Context *cnt, MirInstrRet *ret)
{
    // compare return value with current function type
    MirInstrBlock *block = ret->base.owner_block;
    if (!block->terminal) block->terminal = &ret->base;

    MirType *fn_type = ast_current_fn(cnt)->type;
    BL_ASSERT(fn_type);
    BL_ASSERT(fn_type->kind == MIR_TYPE_FN);

    if (ret->value) {
        if (analyze_slot(cnt, &analyze_slot_conf_default, &ret->value, fn_type->data.fn.ret_type) !=
            ANALYZE_PASSED) {
            return ANALYZE_RESULT(FAILED, 0);
        }
    }

    MirInstr *value = ret->value;
    if (value) {
        BL_ASSERT(value->analyzed);
    }

    const bool expected_ret_value =
        !type_cmp(fn_type->data.fn.ret_type, cnt->builtin_types->t_void);

    // return value is not expected, and it's not provided
    if (!expected_ret_value && !value) {
        return ANALYZE_RESULT(PASSED, 0);
    }

    // return value is expected, but it's not provided
    if (expected_ret_value && !value) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_EXPR,
                    ret->base.node->location,
                    BUILDER_CUR_AFTER,
                    "Expected return value.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    // return value is not expected, but it's provided
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

AnalyzeResult analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
    MirVar *var = decl->var;
    BL_ASSERT(var);

    // Immutable declaration can be comptime, but only if it's initializer value is also
    // comptime! Value of this variable can be adjusted later during analyze pass when
    // we know actual initialization value.
    bool is_decl_comptime = !var->is_mutable;

    // Resolve declaration type if not set implicitly to the target variable by
    // compiler.
    if (decl->type && !var->value.type) {
        AnalyzeResult result = analyze_resolve_type(cnt, decl->type, &var->value.type);
        if (result.state != ANALYZE_PASSED) return result;
    }

    if (var->is_global && !var->is_struct_typedef) {
        // Unexported globals have unique linkage name to solve potential conflicts
        // with extern symbols.
        var->linkage_name = gen_uq_name(var->linkage_name);

        // Globals are set by initializer so we can skip all checks, rest of the
        // work is up to set initializer instruction! There is one exceptional case:
        // we use init value as temporary value for incomplete structure
        // declarations (struct can use pointer to self type inside it's body). This
        // value is later replaced by initializer instruction.
        return ANALYZE_RESULT(PASSED, 0);
    }

    if (IS_FLAG(var->flags, FLAG_THREAD_LOCAL)) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_DIRECTIVE,
                    var->decl_node->location,
                    BUILDER_CUR_WORD,
                    "Thread local variable must be global.");
        return ANALYZE_RESULT(FAILED, 0);
    }
    // Continue only with local variables and struct typedefs.
    bool has_initializer = decl->init;
    if (has_initializer) {
        // Resolve variable intializer. Here we use analyze_slot_initializer call to
        // fulfill possible array to slice cast.
        if (var->value.type) {
            if (analyze_slot_initializer(
                    cnt, &analyze_slot_conf_default, &decl->init, var->value.type) !=
                ANALYZE_PASSED) {
                return ANALYZE_RESULT(FAILED, 0);
            }
        } else {
            if (analyze_slot_initializer(cnt, &analyze_slot_conf_basic, &decl->init, NULL) !=
                ANALYZE_PASSED) {
                return ANALYZE_RESULT(FAILED, 0);
            }

            // infer type
            MirType *type = decl->init->value.type;
            BL_ASSERT(type);
            if (type->kind == MIR_TYPE_NULL) type = type->data.null.base_type;
            var->value.type = type;
        }

        // Immutable and comptime initializer value.
        is_decl_comptime &= decl->init->value.is_comptime;
    } else if (IS_NOT_FLAG(var->flags, FLAG_NO_INIT)) {
        // Create default initilializer for locals without explicit initialization.
        MirType * type         = var->value.type;
        MirInstr *default_init = create_default_value_for_type(cnt, type);
        insert_instr_before(&decl->base, default_init);
        ANALYZE_INSTR_RQ(default_init);
        decl->init = default_init;
    }
    decl->base.value.is_comptime = var->value.is_comptime = is_decl_comptime;
    AnalyzeResult state                                   = analyze_var(cnt, decl->var);
    if (state.state != ANALYZE_PASSED) return state;
    if (decl->base.value.is_comptime && decl->init) {
        // initialize when known in compiletime
        var->value.data = decl->init->value.data;
        BL_ASSERT(var->value.data && "Incomplete comptime var initialization.");
    }
    return ANALYZE_RESULT(PASSED, 0);
}

static INLINE MirType *_check_buitin_array_type(MirInstr *arr)
{
    MirType *arr_type = arr->value.type;
    if (is_load_needed(arr)) arr_type = mir_deref_type(arr_type);
    if (arr_type->kind != MIR_TYPE_DYNARR) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    arr->node->location,
                    BUILDER_CUR_WORD,
                    "Expected dynamic array!");
        return NULL;
    }

    MirType *expected_type = mir_get_struct_elem_type(arr_type, MIR_DYNARR_PTR_INDEX);
    return mir_deref_type(expected_type);
}

static INLINE MirType *_check_buitin_slice_type(MirInstr *slice)
{
    MirType *slice_type = slice->value.type;
    if (is_load_needed(slice)) slice_type = mir_deref_type(slice_type);
    if (slice_type->kind != MIR_TYPE_SLICE) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_TYPE,
                    slice->node->location,
                    BUILDER_CUR_WORD,
                    "Expected slice!");
        return NULL;
    }

    MirType *expected_type = mir_get_struct_elem_type(slice_type, MIR_SLICE_PTR_INDEX);
    return mir_deref_type(expected_type);
}

AnalyzeResult analyze_builtin_call(Context UNUSED(*cnt), MirInstrCall *call)
{
    MirType *callee_type = call->callee->value.type;

    TSmallArray_InstrPtr *args = call->args;

    const MirBuiltinIdKind id = callee_type->data.fn.builtin_id;
    switch (id) {
        // Array
    case MIR_BUILTIN_ID_ARRAY_PUSH_FN: {
        MirType *expected_type = _check_buitin_array_type(args->data[0]);
        if (!expected_type) return ANALYZE_RESULT(FAILED, 0);

        MirInstr *v      = args->data[1];
        MirType * v_type = v->value.type;
        if (is_load_needed(v)) v_type = mir_deref_type(v_type);

        if (!type_cmp(expected_type, v_type)) {
            char expected_name[256];
            mir_type_to_str(expected_name, 256, expected_type, true);

            char got_name[256];
            mir_type_to_str(got_name, 256, v_type, true);

            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_TYPE,
                        v->node->location,
                        BUILDER_CUR_WORD,
                        "Expected '%s' type, but got '%s'!",
                        expected_name,
                        got_name);
        }
        break;
    }

    case MIR_BUILTIN_ID_ARRAY_INIT_FN:
    case MIR_BUILTIN_ID_ARRAY_RESERVE_FN:
    case MIR_BUILTIN_ID_ARRAY_CLEAR_FN:
    case MIR_BUILTIN_ID_ARRAY_ERASE_FN:
    case MIR_BUILTIN_ID_ARRAY_TERMINATE_FN: {
        if (!_check_buitin_array_type(args->data[0])) return ANALYZE_RESULT(PASSED, 0);
        break;
    }
        // Slice
    case MIR_BUILTIN_ID_SLICE_INIT_FN:
    case MIR_BUILTIN_ID_SLICE_TERMINATE_FN: {
        if (!_check_buitin_slice_type(args->data[0])) return ANALYZE_RESULT(PASSED, 0);
        break;
    }

    default:
        break;
    }

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_call(Context *cnt, MirInstrCall *call)
{
    BL_ASSERT(call->callee);
    if (!call->callee_analyzed) {
        ID *missing_any = lookup_builtins_any(cnt);
        if (missing_any) return ANALYZE_RESULT(WAITING, missing_any->hash);

        // callee has not been analyzed yet -> postpone call analyze
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
        call->callee_analyzed = true;
    }

    // Direct call is call without any reference lookup, usually call to anonymous
    // function, type resolver or variable initializer. Contant value of callee
    // instruction must containt pointer to the MirFn object.
    const MirInstrKind callee_kind    = call->callee->kind;
    const bool         is_direct_call = callee_kind == MIR_INSTR_FN_PROTO;
    MirType *          type           = call->callee->value.type;
    BL_ASSERT(type && "invalid type of called object");

    if (mir_is_pointer_type(type)) {
        // we want to make calls also via pointer to functions so in such case
        // we need to resolve pointed function
        type = mir_deref_type(type);
    }

    if (type->kind != MIR_TYPE_FN_GROUP && type->kind != MIR_TYPE_FN) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_EXPECTED_FUNC,
                    call->callee->node->location,
                    BUILDER_CUR_WORD,
                    "Expected a function of function group name.");
        return ANALYZE_RESULT(FAILED, 0);
    }

    // Pre-scan of all arguments passed to function call is needed in case we want to convert some
    // arguments to Any type, because to Any conversion requires generation of rtti metadata about
    // argument value type, we must check all argument types for it's completeness.

    // @PERFORMANCE This is really needed only in case the function argument list contains
    // conversion to Any, there is no need to scan everything, also there is possible option do this
    // check in analyze_instr_decl_ref pass. @travis  14-Oct-2020
    if (call->args) {
        MirInstr *it;
        TSA_FOREACH(call->args, it)
        {
            MirType *t = it->value.type;
            if (t->kind == MIR_TYPE_PTR && mir_deref_type(t)->kind == MIR_TYPE_TYPE) {
                t = *MIR_CEV_READ_AS(MirType **, &it->value);
                BL_MAGIC_ASSERT(t);
            }
            if (!is_complete_type(cnt, t)) {
                if (t->user_id) return ANALYZE_RESULT(WAITING, t->user_id->hash);
                return ANALYZE_RESULT(POSTPONE, 0);
            }
        }
    }

    if (is_direct_call) {
        MirFn *fn = MIR_CEV_READ_AS(MirFn *, &call->callee->value);
        BL_MAGIC_ASSERT(fn);
        if (call->base.value.is_comptime) {
            if (!fn->fully_analyzed) return ANALYZE_RESULT(POSTPONE, 0);
        } else if (call->callee->kind == MIR_INSTR_FN_PROTO) {
            // Direct call of anonymous function.

            // NOTE: We increase ref count of called function here, but this
            // will not work for functions called by pointer in obtained in
            // runtime
            ++fn->ref_count;
            fn->emit_llvm = true;
        }
    }

    const bool is_group = type->kind == MIR_TYPE_FN_GROUP;
    if (is_group) {
        // Function group will be replaced with constant function reference. Best callee
        // candidate selection is based on call arguments not on return type! The best
        // function is selected but it could be still invalid so we have to validate it as
        // usual.
        MirFnGroup *group = MIR_CEV_READ_AS(MirFnGroup *, &call->callee->value);
        BL_MAGIC_ASSERT(group);
        MirFn *selected_overload_fn = NULL;
        { // lookup best call candidate in group
            TSmallArray_TypePtr arg_types;
            tsa_init(&arg_types);
            tsa_resize_TypePtr(&arg_types, call->args->size);
            MirInstr *it;
            TSA_FOREACH(call->args, it)
            {
                MirType *t        = it->value.type;
                arg_types.data[i] = is_load_needed(it) ? mir_deref_type(t) : t;
            }
            selected_overload_fn = group_select_overload(cnt, group, &arg_types);
            tsa_terminate(&arg_types);
        }
        BL_MAGIC_ASSERT(selected_overload_fn);
        erase_instr_tree(call->callee, false, true);
        mutate_instr(call->callee, MIR_INSTR_CONST);
        call->callee->value.data = (VMStackPtr)&call->callee->value._tmp;
        call->callee->node       = selected_overload_fn->decl_node;
        type = call->callee->value.type = selected_overload_fn->type;
        MIR_CEV_WRITE_AS(MirFn *, &call->callee->value, selected_overload_fn);
    }

    MirType *result_type = type->data.fn.ret_type;
    BL_ASSERT(result_type && "invalid type of call result");
    call->base.value.type = result_type;

    // validate arguments
    usize       callee_argc      = type->data.fn.args ? type->data.fn.args->size : 0;
    const usize call_argc        = call->args ? call->args->size : 0;
    const bool  is_vargs         = type->data.fn.is_vargs;
    const bool  has_default_args = type->data.fn.has_default_args;

    bool is_last_call_arg_vargs = false;
    if (call_argc) {
        MirInstr *last_arg = call->args->data[call_argc - 1];
        if (is_load_needed(last_arg)) {
            is_last_call_arg_vargs = mir_deref_type(last_arg->value.type)->kind == MIR_TYPE_VARGS;
        }
    }

    BL_ASSERT(!(is_vargs && has_default_args));
    if (is_vargs && !is_last_call_arg_vargs) {
        // This is gonna be tricky...
        --callee_argc;
        if ((call_argc < callee_argc)) {
            goto INVALID_ARGC;
        }
        MirType *vargs_type = mir_get_fn_arg_type(type, (u32)callee_argc);
        BL_ASSERT(vargs_type->kind == MIR_TYPE_VARGS && "VArgs is expected to be last!!!");
        vargs_type = mir_get_struct_elem_type(vargs_type, 1);
        BL_ASSERT(vargs_type && mir_is_pointer_type(vargs_type));
        vargs_type = mir_deref_type(vargs_type);

        // Prepare vargs values.
        const usize           vargsc = call_argc - callee_argc;
        TSmallArray_InstrPtr *values = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
        MirInstr *            vargs  = create_instr_vargs_impl(cnt, vargs_type, values);
        ref_instr(vargs);
        if (vargsc > 0) {
            // One or more vargs passed.
            // @INCOMPLETE: check it this is ok!!!
            for (usize i = 0; i < vargsc; ++i) {
                tsa_push_InstrPtr(values, call->args->data[callee_argc + i]);
            }

            MirInstr *insert_loc = call->args->data[callee_argc];
            insert_instr_after(insert_loc, vargs);
        } else if (callee_argc > 0) {
            // No arguments passed into vargs but there are more regular
            // arguments before vargs.
            MirInstr *insert_loc = call->args->data[call_argc - 1];
            insert_instr_before(insert_loc, vargs);
        } else {
            insert_instr_before(&call->base, vargs);
        }

        if (analyze_instr_vargs(cnt, (MirInstrVArgs *)vargs).state != ANALYZE_PASSED)
            return ANALYZE_RESULT(FAILED, 0);
        vargs->analyzed = true;
        // Erase vargs from arguments. @NOTE: function does nothing when array size is equal
        // to callee_argc.
        tsa_resize_InstrPtr(call->args, callee_argc);
        // Replace last with vargs.
        tsa_push_InstrPtr(call->args, vargs);
    } else if (has_default_args) {
        // Call have more arguments than a function.
        if (callee_argc < call_argc) {
            goto INVALID_ARGC;
        }

        // Check if all arguments are explicitly provided.
        if (callee_argc > call_argc) {
            for (usize i = call_argc; i < callee_argc; ++i) {
                MirArg *arg = type->data.fn.args->data[i];
                // Missing argument has no default value!
                if (!arg->value) {
                    // @INCOMPLETE: Consider better error message...
                    goto INVALID_ARGC;
                }

                // Create direct reference to default value and insert it into call
                // argument list. Here we modify call->args array!!!
                MirInstr *insert_location =
                    call->args->size > 0 ? call->args->data[call->args->size - 1] : &call->base;

                MirInstr *call_default_arg;
                if (arg->value->kind == MIR_INSTR_CALL_LOC) {
                    // Original InstrCallLoc is used only as note that we must
                    // generate real one containing information about call
                    // instruction location.
                    BL_ASSERT(call->base.node);
                    BL_ASSERT(call->base.node->location);
                    Ast *orig_node = arg->value->node;
                    call_default_arg =
                        create_instr_call_loc(cnt, orig_node, call->base.node->location);
                } else {
                    call_default_arg = create_instr_decl_direct_ref(cnt, arg->value);
                }

                tsa_push_InstrPtr(call->args, call_default_arg);
                insert_instr_before(insert_location, call_default_arg);
                const AnalyzeResult result = analyze_instr(cnt, call_default_arg);
                // Default value reference MUST be analyzed before any call to owner
                // function!
                if (result.state != ANALYZE_PASSED) return ANALYZE_RESULT(FAILED, 0);
            }
        }
    } else {
        if ((callee_argc != call_argc)) {
            goto INVALID_ARGC;
        }
    }

    if (type->data.fn.builtin_id != MIR_BUILTIN_ID_NONE) {
        AnalyzeResult result = analyze_builtin_call(cnt, call);
        if (result.state != ANALYZE_PASSED) return result;
    }

    // validate argument types
    if (!callee_argc) return ANALYZE_RESULT(PASSED, 0);
    for (u32 i = 0; i < callee_argc; ++i) {
        MirInstr **call_arg   = &call->args->data[i];
        MirArg *   callee_arg = type->data.fn.args->data[i];
        BL_ASSERT(callee_arg);

        if (analyze_slot(cnt, &analyze_slot_conf_full, call_arg, callee_arg->type) !=
            ANALYZE_PASSED) {
            goto REPORT_OVERLOAD_LOCATION;
        }
    }

    return ANALYZE_RESULT(PASSED, 0);
    // ERROR
INVALID_ARGC:
    builder_msg(BUILDER_MSG_ERROR,
                ERR_INVALID_ARG_COUNT,
                call->base.node->location,
                BUILDER_CUR_WORD,
                "Expected %u %s, but called with %u.",
                callee_argc,
                callee_argc == 1 ? "argument" : "arguments",
                call_argc);
REPORT_OVERLOAD_LOCATION:
    if (is_group) {
        builder_msg(BUILDER_MSG_NOTE,
                    0,
                    call->callee->node->location,
                    BUILDER_CUR_WORD,
                    "Overloaded function implementation:");
    }
    return ANALYZE_RESULT(FAILED, 0);
}

AnalyzeResult analyze_instr_store(Context *cnt, MirInstrStore *store)
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

    if (dest->value.addr_mode == MIR_VAM_LVALUE_CONST || dest->value.addr_mode == MIR_VAM_RVALUE) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_EXPR,
                    store->base.node->location,
                    BUILDER_CUR_WORD,
                    "Cannot assign to constant.");
    }

    MirType *dest_type = mir_deref_type(dest->value.type);
    BL_ASSERT(dest_type && "store destination has invalid base type");

    if (analyze_slot(cnt, &analyze_slot_conf_default, &store->src, dest_type) != ANALYZE_PASSED) {
        return ANALYZE_RESULT(FAILED, 0);
    }

    // @BUG Global immutable array converted implicitly to slice cause problems when this check is
    // enabled INDENT_AFTER :: {:[1]u8: '{'}; opt.indent_after = INDENT_AFTER;

#if BL_DEBUG
    // If store instruction source value is compound expression it should not be naked.
    if (store->src->kind == MIR_INSTR_COMPOUND) {
        BL_ASSERT(!((MirInstrCompound *)store->src)->is_naked);
    }
#endif

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeResult analyze_instr_block(Context *cnt, MirInstrBlock *block)
{
    BL_ASSERT(block);

    MirFn *fn = block->owner_fn;
    if (!fn) { // block in global scope
        return ANALYZE_RESULT(PASSED, 0);
    }

    MirInstrFnProto *fn_proto = (MirInstrFnProto *)fn->prototype;
    BL_ASSERT(fn_proto);

    block->base.is_unreachable = block->base.ref_count == 0;
    if (!fn->first_unreachable_loc && block->base.is_unreachable && block->entry_instr &&
        block->entry_instr->node) {
        // Report unreachable code if there is one only once inside funcition body.
        fn->first_unreachable_loc = block->entry_instr->node->location;

        builder_msg(BUILDER_MSG_WARNING,
                    0,
                    fn->first_unreachable_loc,
                    BUILDER_CUR_NONE,
                    "Unreachable code detected.");
    }

    // Append implicit return for void functions or generate error when last
    // block is not terminated
    if (!is_block_terminated(block)) {
        if (fn->type->data.fn.ret_type->kind == MIR_TYPE_VOID) {
            set_current_block(cnt, block);
            append_instr_ret(cnt, block->base.node, NULL);
        } else if (block->base.is_unreachable) {
            set_current_block(cnt, block);
            append_instr_br(cnt, block->base.node, block);
        } else {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_MISSING_RETURN,
                        fn->decl_node->location,
                        BUILDER_CUR_WORD,
                        "Not every path inside function return value.");
        }
    }

    return ANALYZE_RESULT(PASSED, 0);
}

AnalyzeState _analyze_slot(Context *                cnt,
                           const AnalyzeSlotConfig *conf,
                           MirInstr **              input,
                           MirType *                slot_type,
                           bool                     is_initializer)
{
    AnalyzeStageState state;
    for (s32 i = 0; i < conf->count; ++i) {
        state = conf->stages[i](cnt, input, slot_type, is_initializer);
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
    return ANALYZE_PASSED;

FAILED:
    return ANALYZE_FAILED;
}

ANALYZE_STAGE_FN(load)
{
    if (is_load_needed(*input)) {
        *input = insert_instr_load(cnt, *input);

        AnalyzeResult r = analyze_instr(cnt, *input);
        if (r.state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;
    }

    return ANALYZE_STAGE_CONTINUE;
}

ANALYZE_STAGE_FN(unroll)
{
    MirInstrUnroll *unroll = (*input)->kind == MIR_INSTR_UNROLL ? ((MirInstrUnroll *)*input) : NULL;
    if (!unroll) return ANALYZE_STAGE_CONTINUE;
    // Erase unroll instruction in case it's marked for remove.
    if (unroll->remove) {
        if (unroll->remove_src) {
            MirInstr *ref = create_instr_decl_direct_ref(cnt, unroll->remove_src);
            insert_instr_after(*input, ref);
            ANALYZE_INSTR_RQ(ref);
            (*input) = ref;
        } else {
            (*input) = unroll->src;
        }
        unref_instr(&unroll->base);
        erase_instr_tree(&unroll->base, false, false);
    }
    return ANALYZE_STAGE_CONTINUE;
}

ANALYZE_STAGE_FN(set_null)
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

ANALYZE_STAGE_FN(set_auto)
{
    BL_ASSERT(slot_type);

    if ((*input)->kind != MIR_INSTR_CAST) return ANALYZE_STAGE_CONTINUE;
    MirInstrCast *cast = (MirInstrCast *)*input;
    if (!cast->auto_cast) return ANALYZE_STAGE_CONTINUE;

    cast->base.value.type = slot_type;
    if (analyze_instr_cast(cnt, cast, true).state != ANALYZE_PASSED) {
        return ANALYZE_STAGE_FAILED;
    }

    if (!evaluate(cnt, &cast->base)) return ANALYZE_STAGE_FAILED;

    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(toany)
{
    BL_ASSERT(slot_type);

    // check any
    if (!is_to_any_needed(cnt, *input, slot_type)) return ANALYZE_STAGE_CONTINUE;

    AnalyzeResult result;
    *input = insert_instr_toany(cnt, *input);
    result = analyze_instr(cnt, *input);
    if (result.state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;

    *input = insert_instr_load(cnt, *input);
    result = analyze_instr(cnt, *input);
    if (result.state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;

    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(dynarrtoslice)
{
    // Cast from dynamic array to slice can be done by bitcast from pointer to dynamic array to
    // slice pointer, both structures have same data layout of first two members.
    BL_ASSERT(slot_type);

    MirType *from_type = (*input)->value.type;
    BL_ASSERT(from_type);

    if (!mir_is_pointer_type(from_type)) return ANALYZE_STAGE_CONTINUE;

    from_type = mir_deref_type(from_type);
    if (from_type->kind != MIR_TYPE_DYNARR || slot_type->kind != MIR_TYPE_SLICE)
        return ANALYZE_STAGE_CONTINUE;

    { // Compare elem type of array and slot slice
        MirType *elem_from_type = mir_get_struct_elem_type(from_type, MIR_SLICE_PTR_INDEX);
        MirType *elem_to_type   = mir_get_struct_elem_type(slot_type, MIR_SLICE_PTR_INDEX);

        BL_ASSERT(mir_is_pointer_type(elem_from_type) && "Expected pointer type!");
        BL_ASSERT(mir_is_pointer_type(elem_to_type) && "Expected pointer type!");
        elem_from_type = mir_deref_type(elem_from_type);
        elem_to_type   = mir_deref_type(elem_to_type);

        BL_ASSERT(elem_from_type && "Invalid type after pointer type dereference!");
        BL_ASSERT(elem_to_type && "Invalid type after pointer type dereference!");

        if (!type_cmp(elem_from_type, elem_to_type)) return ANALYZE_STAGE_CONTINUE;
    }

    { // Build bitcast
        *input = insert_instr_cast(cnt, *input, create_type_ptr(cnt, slot_type));
        if (analyze_instr(cnt, *input).state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;
    }

    { // Build load
        *input = insert_instr_load(cnt, *input);
        if (analyze_instr(cnt, *input).state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;
    }

    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(arrtoslice)
{
    // Produce implicit cast from array type to slice. This will create implicit compound
    // initializer representing array legth and pointer to array data.
    BL_ASSERT(slot_type);

    MirType *from_type = (*input)->value.type;
    BL_ASSERT(from_type);

    if (!mir_is_pointer_type(from_type)) return ANALYZE_STAGE_CONTINUE;

    from_type = mir_deref_type(from_type);
    if (from_type->kind != MIR_TYPE_ARRAY || slot_type->kind != MIR_TYPE_SLICE)
        return ANALYZE_STAGE_CONTINUE;

    { // Compare elem type of array and slot slice
        MirType *elem_from_type = from_type->data.array.elem_type;
        MirType *elem_to_type   = mir_get_struct_elem_type(slot_type, MIR_SLICE_PTR_INDEX);
        BL_ASSERT(mir_is_pointer_type(elem_to_type) && "Expected pointer type!");
        elem_to_type = mir_deref_type(elem_to_type);
        BL_ASSERT(elem_to_type && "Invalid type after pointer type dereference!");

        if (!type_cmp(elem_from_type, elem_to_type)) return ANALYZE_STAGE_CONTINUE;
    }

    { // Build slice initializer.
        const s64             len       = from_type->data.array.len;
        MirInstr *            instr_arr = *input;
        TSmallArray_InstrPtr *values    = create_sarr(TSmallArray_InstrPtr, cnt->assembly);

        // Build array pointer
        MirInstr *instr_ptr =
            create_instr_member_ptr(cnt, NULL, instr_arr, NULL, NULL, MIR_BUILTIN_ID_ARR_PTR);
        insert_instr_after(*input, instr_ptr);
        *input = instr_ptr;
        ANALYZE_INSTR_RQ(instr_ptr);

        // Build array len constant
        MirInstr *instr_len = create_instr_const_int(cnt, NULL, cnt->builtin_types->t_s64, len);
        insert_instr_after(*input, instr_len);
        *input = instr_len;
        ANALYZE_INSTR_RQ(instr_len);

        // push values
        tsa_push_InstrPtr(values, instr_len);
        tsa_push_InstrPtr(values, instr_ptr);

        MirInstr *compound = create_instr_compound_impl(cnt, NULL, slot_type, values);
        ((MirInstrCompound *)compound)->is_naked = !is_initializer;
        ref_instr(compound);

        insert_instr_after(*input, compound);
        *input = compound;

        ANALYZE_INSTR_RQ(compound);
    }

    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(set_volatile_expr)
{
    BL_ASSERT(slot_type);
    if (slot_type->kind != MIR_TYPE_INT) return ANALYZE_STAGE_CONTINUE;
    if (!is_instr_type_volatile(*input)) return ANALYZE_STAGE_CONTINUE;
    const MirCastOp op = get_cast_op((*input)->value.type, slot_type);
    BL_ASSERT(op != MIR_CAST_INVALID);
    MirConstExprValue *value = &(*input)->value;
    VMValue            tmp   = {0};
    vm_do_cast((VMStackPtr)&tmp[0], value->data, slot_type, value->type, op);
    memcpy(&value->_tmp[0], &tmp[0], sizeof(VMValue));
    (*input)->value.type = slot_type;
    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(implicit_cast)
{
    if (type_cmp((*input)->value.type, slot_type)) return ANALYZE_STAGE_BREAK;
    if (!can_impl_cast((*input)->value.type, slot_type)) return ANALYZE_STAGE_CONTINUE;

    *input = insert_instr_cast(cnt, *input, slot_type);

    AnalyzeResult r = analyze_instr(cnt, *input);
    if (r.state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;
    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(report_type_mismatch)
{
    error_types((*input)->value.type, slot_type, (*input)->node, NULL);
    return ANALYZE_STAGE_FAILED;
}

AnalyzeResult analyze_instr(Context *cnt, MirInstr *instr)
{
    if (!instr) return ANALYZE_RESULT(PASSED, 0);

    // skip already analyzed instructions
    if (instr->analyzed) return ANALYZE_RESULT(PASSED, 0);
    AnalyzeResult state = ANALYZE_RESULT(PASSED, 0);

    TracyCZone(_tctx, true);
    BL_TRACY_MESSAGE("ANALYZE", "[%llu] %s", instr->id, mir_instr_name(instr));

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
    case MIR_INSTR_FN_GROUP:
        state = analyze_instr_fn_group(cnt, (MirInstrFnGroup *)instr);
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
    case MIR_INSTR_TYPE_FN_GROUP:
        state = analyze_instr_type_fn_group(cnt, (MirInstrTypeFnGroup *)instr);
        break;
    case MIR_INSTR_TYPE_STRUCT:
        state = analyze_instr_type_struct(cnt, (MirInstrTypeStruct *)instr);
        break;
    case MIR_INSTR_TYPE_SLICE:
        state = analyze_instr_type_slice(cnt, (MirInstrTypeSlice *)instr);
        break;
    case MIR_INSTR_TYPE_DYNARR:
        state = analyze_instr_type_dynarr(cnt, (MirInstrTypeDynArr *)instr);
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
    case MIR_INSTR_SWITCH:
        state = analyze_instr_switch(cnt, (MirInstrSwitch *)instr);
        break;
    case MIR_INSTR_SET_INITIALIZER:
        state = analyze_instr_set_initializer(cnt, (MirInstrSetInitializer *)instr);
        break;
    case MIR_INSTR_TEST_CASES:
        state = analyze_instr_test_cases(cnt, (MirInstrTestCases *)instr);
        break;
    case MIR_INSTR_CALL_LOC:
        state = analyze_instr_call_loc(cnt, (MirInstrCallLoc *)instr);
        break;
    case MIR_INSTR_UNROLL:
        state = analyze_instr_unroll(cnt, (MirInstrUnroll *)instr);
        break;
    default:
        BL_ABORT("Missing analyze of instruction!");
    }
#if TRACY_ENABLE
    TracyCZoneEnd(_tctx);
#endif
    if (state.state == ANALYZE_PASSED) {
        instr->analyzed = true;
        if (instr->kind == MIR_INSTR_CAST && ((MirInstrCast *)instr)->auto_cast) {
            // An auto cast cannot be directly evaluated because it's destination type
            // could change based on usage.
            return state;
        }

        if (!evaluate(cnt, instr)) {
            return ANALYZE_RESULT(FAILED, 0);
        }
    }

    return state;
}

static INLINE MirInstr *analyze_try_get_next(MirInstr *instr)
{
    if (!instr) return NULL;
    if (instr->kind == MIR_INSTR_BLOCK) {
        MirInstrBlock *block = (MirInstrBlock *)instr;
        return block->entry_instr;
    }

    // Instruction can be the last instruction inside block, but block may not
    // be the last block inside function, we try to get following one.
    MirInstrBlock *owner_block = instr->owner_block;
    if (owner_block && instr == owner_block->last_instr) {
        if (owner_block->base.next == NULL && owner_block->owner_fn) {
            // Instruction is last instruction of the function body, so the
            // function can be executed in compile time if needed, we need to
            // set flag with this information here.
            owner_block->owner_fn->fully_analyzed = true;
#if BL_DEBUG && VERBOSE_ANALYZE
            printf("Analyze: " BLUE("Function '%s' completely analyzed.\n"),
                   owner_block->owner_fn->linkage_name);
#endif
        }

        // Return following block.
        return owner_block->base.next;
    }

    return instr->next;
}

void analyze(Context *cnt)
{
//*********************************************************************************************/
#if BL_DEBUG && VERBOSE_ANALYZE
#define LOG_ANALYZE_PASSED printf("Analyze: [ " PASSED " ] %16s\n", mir_instr_name(ip));
#define LOG_ANALYZE_FAILED printf("Analyze: [ " FAILED " ] %16s\n", mir_instr_name(ip));

#define LOG_ANALYZE_POSTPONE                                                                       \
    printf("Analyze: [" MAGENTA("POSTPONE") "] %16s\n", mir_instr_name(ip));
#define LOG_ANALYZE_WAITING                                                                        \
    printf("Analyze: [  " YELLOW("WAIT") "  ] %16s is waiting for: '%llu'\n",                      \
           mir_instr_name(ip),                                                                     \
           (unsigned long long)result.waiting_for);
#else
#define LOG_ANALYZE_PASSED
#define LOG_ANALYZE_FAILED
#define LOG_ANALYZE_POSTPONE
#define LOG_ANALYZE_WAITING
#endif
    //*********************************************************************************************/

    TracyCZone(_tctx, true);
    // PERFORMANCE: use array???
    TList *       q = &cnt->analyze.queue;
    AnalyzeResult result;
    usize         postpone_loop_count = 0;
    MirInstr *    ip                  = NULL;
    MirInstr *    prev_ip             = NULL;
    bool          skip                = false;

    if (tlist_empty(q)) return;

    while (true) {
        prev_ip = ip;
        ip      = skip ? NULL : analyze_try_get_next(ip);

        if (prev_ip && prev_ip->analyzed) {
            // Remove unused instructions here!
            erase_instr_tree(prev_ip, false, false);
        }

        if (!ip) {
            if (tlist_empty(q)) break;

            ip = tlist_front(MirInstr *, q);
            tlist_pop_front(q);
            skip = false;
        }

        result = analyze_instr(cnt, ip);

        switch (result.state) {
        case ANALYZE_PASSED:
            LOG_ANALYZE_PASSED
            postpone_loop_count = 0;
            break;

        case ANALYZE_FAILED:
            LOG_ANALYZE_FAILED
            skip                = true;
            postpone_loop_count = 0;
            break;

        case ANALYZE_POSTPONE:
            LOG_ANALYZE_POSTPONE

            skip = true;
            if (postpone_loop_count++ <= q->size) tlist_push_back(q, ip);
            break;

        case ANALYZE_WAITING: {
            LOG_ANALYZE_WAITING

            TArray *  wq   = NULL;
            TIterator iter = thtbl_find(&cnt->analyze.waiting, result.waiting_for);
            TIterator end  = thtbl_end(&cnt->analyze.waiting);
            if (TITERATOR_EQUAL(iter, end)) {
                wq = thtbl_insert_empty(&cnt->analyze.waiting, result.waiting_for);
                tarray_init(wq, sizeof(MirInstr *));
                tarray_reserve(wq, 16);
            } else {
                wq = &thtbl_iter_peek_value(TArray, iter);
            }

            BL_ASSERT(wq);
            tarray_push(wq, ip);
            skip                = true;
            postpone_loop_count = 0;
        }
        }
    }

    TracyCZoneEnd(_tctx);
    //******************************************************************************************/
#undef LOG_ANALYZE_PASSED
#undef LOG_ANALYZE_FAILED
#undef LOG_ANALYZE_POSTPONE
#undef LOG_ANALYZE_WAITING
    //******************************************************************************************/
}

void analyze_report_unresolved(Context *cnt)
{
    MirInstr *instr;
    TArray *  wq;
    TIterator iter;
    s32       reported = 0;

    THTBL_FOREACH(&cnt->analyze.waiting, iter)
    {
        wq = &thtbl_iter_peek_value(TArray, iter);
        BL_ASSERT(wq);
        TARRAY_FOREACH(MirInstr *, wq, instr)
        {
            BL_ASSERT(instr);
            const char *sym_name = NULL;
            switch (instr->kind) {
            case MIR_INSTR_DECL_REF: {
                MirInstrDeclRef *ref = (MirInstrDeclRef *)instr;
                if (!ref->scope) continue;
                if (!ref->rid) continue;
                sym_name = ref->rid->str;
                if (scope_is_local(ref->scope)) break;
                if (scope_lookup(ref->scope, ref->rid, true, false, NULL)) {
                    continue;
                }
                break;
            }
            default:
                BL_LOG(">> %s", mir_instr_name(instr));
                continue;
            }
            BL_ASSERT(sym_name && "Invalid unresolved symbol name!");
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_UNKNOWN_SYMBOL,
                        instr->node->location,
                        BUILDER_CUR_WORD,
                        "Unknown symbol '%s'.",
                        sym_name);
            ++reported;
        }
    }
    if (cnt->analyze.waiting.size && !reported) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_UNKNOWN_SYMBOL,
                    NULL,
                    BUILDER_CUR_WORD,
                    "Unknown symbol/s detected but not correctly reported, this is compiler bug!");
    }
}

void analyze_report_unused(Context *cnt)
{
    TArray *    queue = &cnt->analyze.usage_check_queue;
    ScopeEntry *entry;
    TARRAY_FOREACH(ScopeEntry *, queue, entry)
    {
        if (entry->ref_count > 0) continue;
        if (entry->kind != SCOPE_ENTRY_VAR && entry->kind != SCOPE_ENTRY_FN) continue;
        if (!entry->node || !entry->id) continue;
        BL_ASSERT(entry->node->location);
        builder_msg(BUILDER_MSG_WARNING,
                    0,
                    entry->node->location,
                    BUILDER_CUR_WORD,
                    "Unused symbol '%s'. (Use unnamed identificator '_' if this is intentional)",
                    entry->id->str);
    }
}

MirVar *testing_gen_meta(Context *cnt)
{
    const s32 len = cnt->testing.expected_test_count;
    if (len == 0) return NULL;
    if (cnt->assembly->testing.meta_var) return cnt->assembly->testing.meta_var;

    MirType *type = create_type_array(cnt, NULL, cnt->builtin_types->t_TestCase, len);
    MirVar * var  = create_var_impl(cnt, IMPL_TESTCASES_TMP, type, false, true, true);
    vm_alloc_global(cnt->vm, cnt->assembly, var);

    cnt->assembly->testing.meta_var = var;
    return var;
}

INLINE void testing_add_test_case(Context *cnt, MirFn *fn)
{
    MirVar *var = testing_gen_meta(cnt);
    BL_ASSERT(var);
    BL_ASSERT(var->value.data);

    tarray_push(&cnt->assembly->testing.cases, fn);
    const s64 i = cnt->assembly->testing.cases.size - 1;

    VMStackPtr var_ptr  = vm_read_var(cnt->vm, var);
    MirType *  var_type = var->value.type;
    BL_ASSERT(var_type->kind == MIR_TYPE_ARRAY);
    MirType *       elem_type = var_type->data.array.elem_type;
    const ptrdiff_t offset    = vm_get_array_elem_offset(var_type, i);

    MirType *func_type = mir_get_struct_elem_type(elem_type, 0);
    MirType *name_type = mir_get_struct_elem_type(elem_type, 1);

    VMStackPtr func_ptr = vm_get_struct_elem_ptr(cnt->assembly, elem_type, var_ptr + offset, 0);
    VMStackPtr name_ptr = vm_get_struct_elem_ptr(cnt->assembly, elem_type, var_ptr + offset, 1);

    BL_ASSERT(fn->id);

    vm_write_ptr(func_type, func_ptr, (VMStackPtr)fn);
    vm_write_string(cnt->vm, name_type, name_ptr, fn->id->str, strlen(fn->id->str));
}

// Top-level rtti generation.
INLINE MirVar *rtti_gen(Context *cnt, MirType *type)
{
    MirVar *tmp = _rtti_gen(cnt, type);

    TSmallArray_RTTIIncomplete *pending = &cnt->analyze.incomplete_rtti;
    while (pending->size) {
        RTTIIncomplete incomplete = tsa_pop_RTTIIncomplete(pending);
        rtti_satisfy_incomplete(cnt, &incomplete);
    }

    return tmp;
}

void rtti_satisfy_incomplete(Context *cnt, RTTIIncomplete *incomplete)
{
    MirType *type     = incomplete->type;
    MirVar * rtti_var = incomplete->var;

    BL_ASSERT(type->kind == MIR_TYPE_PTR);
    rtti_gen_ptr(cnt, type, rtti_var);
}

MirVar *_rtti_gen(Context *cnt, MirType *type)
{
    BL_ASSERT(type);
    if (assembly_has_rtti(cnt->assembly, type->id.hash))
        return assembly_get_rtti(cnt->assembly, type->id.hash);

    MirVar *rtti_var = NULL;

    switch (type->kind) {
    case MIR_TYPE_INT:
        rtti_var = rtti_gen_integer(cnt, type);
        break;

    case MIR_TYPE_ENUM:
        rtti_var = rtti_gen_enum(cnt, type);
        break;

    case MIR_TYPE_REAL:
        rtti_var = rtti_gen_real(cnt, type);
        break;

    case MIR_TYPE_BOOL:
        rtti_var = rtti_gen_empty(cnt, type, cnt->builtin_types->t_TypeInfoBool);
        break;

    case MIR_TYPE_TYPE:
        rtti_var = rtti_gen_empty(cnt, type, cnt->builtin_types->t_TypeInfoType);
        break;

    case MIR_TYPE_VOID:
        rtti_var = rtti_gen_empty(cnt, type, cnt->builtin_types->t_TypeInfoVoid);
        break;

    case MIR_TYPE_NULL:
        rtti_var = rtti_gen_empty(cnt, type, cnt->builtin_types->t_TypeInfoNull);
        break;

    case MIR_TYPE_STRING:
        rtti_var = rtti_gen_empty(cnt, type, cnt->builtin_types->t_TypeInfoString);
        break;

    case MIR_TYPE_PTR:
        // We generate dummy pointer RTTI when incomplete is enabled and complete
        // this in second pass to prove endless looping.
        rtti_var = rtti_gen_ptr(cnt, cnt->builtin_types->t_u8_ptr, NULL);
        tsa_push_RTTIIncomplete(&cnt->analyze.incomplete_rtti,
                                (RTTIIncomplete){.var = rtti_var, .type = type});
        break;

    case MIR_TYPE_ARRAY:
        rtti_var = rtti_gen_array(cnt, type);
        break;

    case MIR_TYPE_DYNARR:
    case MIR_TYPE_SLICE:
    case MIR_TYPE_VARGS:
    case MIR_TYPE_STRUCT:
        rtti_var = rtti_gen_struct(cnt, type);
        break;

    case MIR_TYPE_FN:
        rtti_var = rtti_gen_fn(cnt, type);
        break;

    case MIR_TYPE_FN_GROUP:
        rtti_var = rtti_gen_fn_group(cnt, type);
        break;

    default: {
        char type_name[256];
        mir_type_to_str(type_name, 256, type, true);
        BL_ABORT("missing RTTI generation for type '%s'", type_name);
    }
    }

    BL_ASSERT(rtti_var);
    assembly_add_rtti(cnt->assembly, type->id.hash, rtti_var);
    return rtti_var;
}

INLINE MirVar *rtti_create_and_alloc_var(Context *cnt, MirType *type)
{
    MirVar *var = create_var_impl(cnt, IMPL_RTTI_ENTRY, type, false, true, true);
    vm_alloc_global(cnt->vm, cnt->assembly, var);
    return var;
}

static INLINE void rtti_gen_base(Context *cnt, VMStackPtr dest, u8 kind, usize size_bytes)
{
    MirType *  rtti_type      = cnt->builtin_types->t_TypeInfo;
    MirType *  dest_kind_type = mir_get_struct_elem_type(rtti_type, 0);
    VMStackPtr dest_kind      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 0);

    MirType *  dest_size_bytes_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr dest_size_bytes      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);

    vm_write_int(dest_kind_type, dest_kind, (u64)kind);
    vm_write_int(dest_size_bytes_type, dest_size_bytes, (u64)size_bytes);
}

MirVar *rtti_gen_integer(Context *cnt, MirType *type)
{
    MirType *  rtti_type = cnt->builtin_types->t_TypeInfoInt;
    MirVar *   rtti_var  = rtti_create_and_alloc_var(cnt, rtti_type);
    VMStackPtr dest      = vm_read_var(cnt->vm, rtti_var);
    rtti_gen_base(cnt, dest, type->kind, type->store_size_bytes);

    MirType *  dest_bit_count_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr dest_bit_count      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);

    MirType *  dest_is_signed_type = mir_get_struct_elem_type(rtti_type, 2);
    VMStackPtr dest_is_signed      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 2);

    vm_write_int(dest_bit_count_type, dest_bit_count, (u64)type->data.integer.bitcount);
    vm_write_int(dest_is_signed_type, dest_is_signed, (u64)type->data.integer.is_signed);

    return rtti_var;
}

MirVar *rtti_gen_real(Context *cnt, MirType *type)
{
    MirType *  rtti_type = cnt->builtin_types->t_TypeInfoReal;
    MirVar *   rtti_var  = rtti_create_and_alloc_var(cnt, rtti_type);
    VMStackPtr dest      = vm_read_var(cnt->vm, rtti_var);
    rtti_gen_base(cnt, dest, type->kind, type->store_size_bytes);

    MirType *  dest_bit_count_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr dest_bit_count      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);

    vm_write_int(dest_bit_count_type, dest_bit_count, (u64)type->data.real.bitcount);
    return rtti_var;
}

MirVar *rtti_gen_ptr(Context *cnt, MirType *type, MirVar *incomplete)
{
    MirVar *rtti_var =
        incomplete ? incomplete : rtti_create_and_alloc_var(cnt, cnt->builtin_types->t_TypeInfoPtr);

    VMStackPtr dest = vm_read_var(cnt->vm, rtti_var);
    rtti_gen_base(cnt, dest, type->kind, type->store_size_bytes);

    MirType *  dest_pointee_type = mir_get_struct_elem_type(cnt->builtin_types->t_TypeInfoPtr, 1);
    VMStackPtr dest_pointee =
        vm_get_struct_elem_ptr(cnt->assembly, cnt->builtin_types->t_TypeInfoPtr, dest, 1);

    MirVar *pointee = _rtti_gen(cnt, type->data.ptr.expr);
    vm_write_ptr(dest_pointee_type, dest_pointee, vm_read_var(cnt->vm, pointee));

    return rtti_var;
}

MirVar *rtti_gen_array(Context *cnt, MirType *type)
{
    MirType *  rtti_type = cnt->builtin_types->t_TypeInfoArray;
    MirVar *   rtti_var  = rtti_create_and_alloc_var(cnt, rtti_type);
    VMStackPtr dest      = vm_read_var(cnt->vm, rtti_var);
    rtti_gen_base(cnt, dest, type->kind, type->store_size_bytes);

    // name
    MirType *   dest_name_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr  dest_name      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);
    const char *name           = type->user_id ? type->user_id->str : type->id.str;

    vm_write_string(cnt->vm, dest_name_type, dest_name, name, strlen(name));

    // elem_type
    MirType *  dest_elem_type = mir_get_struct_elem_type(rtti_type, 2);
    VMStackPtr dest_elem      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 2);

    MirVar *elem = _rtti_gen(cnt, type->data.array.elem_type);
    vm_write_ptr(dest_elem_type, dest_elem, vm_read_var(cnt->vm, elem));

    // len
    MirType *  dest_len_type = mir_get_struct_elem_type(rtti_type, 3);
    VMStackPtr dest_len      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 3);

    vm_write_int(dest_len_type, dest_len, (u64)type->data.array.len);

    return rtti_var;
}

MirVar *rtti_gen_empty(Context *cnt, MirType *type, MirType *rtti_type)
{
    MirVar *   rtti_var = rtti_create_and_alloc_var(cnt, rtti_type);
    VMStackPtr dest     = vm_read_var(cnt->vm, rtti_var);
    rtti_gen_base(cnt, dest, type->kind, type->store_size_bytes);
    return rtti_var;
}

void rtti_gen_enum_variant(Context *cnt, VMStackPtr dest, MirVariant *variant)
{
    MirType *  rtti_type      = cnt->builtin_types->t_TypeInfoEnumVariant;
    MirType *  dest_name_type = mir_get_struct_elem_type(rtti_type, 0);
    VMStackPtr dest_name      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 0);

    MirType *  dest_value_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr dest_value      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);

    vm_write_string(cnt->vm, dest_name_type, dest_name, variant->id->str, strlen(variant->id->str));
    vm_write_int(dest_value_type, dest_value, MIR_CEV_READ_AS(u64, variant->value));
}

VMStackPtr rtti_gen_enum_variants_array(Context *cnt, TSmallArray_VariantPtr *variants)
{
    MirType *rtti_type    = cnt->builtin_types->t_TypeInfoEnumVariant;
    MirType *arr_tmp_type = create_type_array(cnt, NULL, rtti_type, variants->size);

    VMStackPtr dest_arr_tmp = vm_alloc_raw(cnt->vm, cnt->assembly, arr_tmp_type);

    MirVariant *it;
    TSA_FOREACH(variants, it)
    {
        VMStackPtr dest_arr_tmp_elem = vm_get_array_elem_ptr(arr_tmp_type, dest_arr_tmp, (u32)i);
        rtti_gen_enum_variant(cnt, dest_arr_tmp_elem, it);
    }

    return dest_arr_tmp;
}

void rtti_gen_enum_variants_slice(Context *cnt, VMStackPtr dest, TSmallArray_VariantPtr *variants)
{
    MirType *  rtti_type     = cnt->builtin_types->t_TypeInfoEnumVariants_slice;
    MirType *  dest_len_type = mir_get_struct_elem_type(rtti_type, 0);
    VMStackPtr dest_len      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 0);

    MirType *  dest_ptr_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr dest_ptr      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);

    VMStackPtr variants_ptr = rtti_gen_enum_variants_array(cnt, variants);

    vm_write_int(dest_len_type, dest_len, (u64)variants->size);
    vm_write_ptr(dest_ptr_type, dest_ptr, variants_ptr);
}

MirVar *rtti_gen_enum(Context *cnt, MirType *type)
{
    MirType *  rtti_type = cnt->builtin_types->t_TypeInfoEnum;
    MirVar *   rtti_var  = rtti_create_and_alloc_var(cnt, rtti_type);
    VMStackPtr dest      = vm_read_var(cnt->vm, rtti_var);
    rtti_gen_base(cnt, dest, type->kind, type->store_size_bytes);

    // name
    MirType *   dest_name_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr  dest_name      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);
    const char *name           = type->user_id ? type->user_id->str : type->id.str;
    vm_write_string(cnt->vm, dest_name_type, dest_name, name, strlen(name));

    // base_type
    MirType *  dest_base_type_type = mir_get_struct_elem_type(rtti_type, 2);
    VMStackPtr dest_base_type      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 2);

    MirVar *base_type = _rtti_gen(cnt, type->data.enm.base_type);
    vm_write_ptr(dest_base_type_type, dest_base_type, vm_read_var(cnt->vm, base_type));

    // variants
    VMStackPtr dest_variants = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 3);
    rtti_gen_enum_variants_slice(cnt, dest_variants, type->data.enm.variants);

    return rtti_var;
}

void rtti_gen_struct_member(Context *cnt, VMStackPtr dest, MirMember *member)
{
    MirType *rtti_type = cnt->builtin_types->t_TypeInfoStructMember;

    // name
    MirType *  dest_name_type = mir_get_struct_elem_type(rtti_type, 0);
    VMStackPtr dest_name      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 0);
    vm_write_string(cnt->vm, dest_name_type, dest_name, member->id->str, strlen(member->id->str));

    // base_type
    MirType *  dest_base_type_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr dest_base_type      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);
    MirVar *   base_type           = _rtti_gen(cnt, member->type);
    vm_write_ptr(dest_base_type_type, dest_base_type, vm_read_var(cnt->vm, base_type));

    // offset_bytes
    MirType *  dest_offset_type = mir_get_struct_elem_type(rtti_type, 2);
    VMStackPtr dest_offset      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 2);
    vm_write_int(dest_offset_type, dest_offset, (u64)member->offset_bytes);

    // index
    MirType *  dest_index_type = mir_get_struct_elem_type(rtti_type, 3);
    VMStackPtr dest_index      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 3);
    vm_write_int(dest_index_type, dest_index, (u64)member->index);

    // tag
    MirType *  dest_tags_type = mir_get_struct_elem_type(rtti_type, 4);
    VMStackPtr dest_tags      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 4);
    vm_write_int(dest_tags_type, dest_tags, (u64)member->tags);

    // is_base
    MirType *  dest_is_base_type = mir_get_struct_elem_type(rtti_type, 5);
    VMStackPtr dest_is_base      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 5);
    vm_write_int(dest_is_base_type, dest_is_base, (u64)member->is_base);
}

VMStackPtr rtti_gen_struct_members_array(Context *cnt, TSmallArray_MemberPtr *members)
{
    MirType *rtti_type    = cnt->builtin_types->t_TypeInfoStructMember;
    MirType *arr_tmp_type = create_type_array(cnt, NULL, rtti_type, (s64)members->size);

    VMStackPtr dest_arr_tmp = vm_alloc_raw(cnt->vm, cnt->assembly, arr_tmp_type);

    MirMember *it;
    TSA_FOREACH(members, it)
    {
        VMStackPtr dest_arr_tmp_elem = vm_get_array_elem_ptr(arr_tmp_type, dest_arr_tmp, (u32)i);
        rtti_gen_struct_member(cnt, dest_arr_tmp_elem, it);
    }

    return dest_arr_tmp;
}

void rtti_gen_struct_members_slice(Context *cnt, VMStackPtr dest, TSmallArray_MemberPtr *members)
{
    MirType *  rtti_type     = cnt->builtin_types->t_TypeInfoStructMembers_slice;
    MirType *  dest_len_type = mir_get_struct_elem_type(rtti_type, 0);
    VMStackPtr dest_len      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 0);

    MirType *  dest_ptr_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr dest_ptr      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);

    VMStackPtr members_ptr = rtti_gen_struct_members_array(cnt, members);

    vm_write_int(dest_len_type, dest_len, (u64)members->size);
    vm_write_ptr(dest_ptr_type, dest_ptr, members_ptr);
}

MirVar *rtti_gen_struct(Context *cnt, MirType *type)
{
    BL_ASSERT(!is_incomplete_struct_type(type) &&
              "Attempt to generate RTTI for incomplete struct type!");
    MirType *  rtti_type = cnt->builtin_types->t_TypeInfoStruct;
    MirVar *   rtti_var  = rtti_create_and_alloc_var(cnt, rtti_type);
    VMStackPtr dest      = vm_read_var(cnt->vm, rtti_var);
    rtti_gen_base(cnt, dest, MIR_TYPE_STRUCT, type->store_size_bytes);

    // name
    MirType *   dest_name_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr  dest_name      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);
    const char *name           = type->user_id ? type->user_id->str : type->id.str;
    vm_write_string(cnt->vm, dest_name_type, dest_name, name, strlen(name));

    // members
    VMStackPtr dest_members = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 2);
    rtti_gen_struct_members_slice(cnt, dest_members, type->data.strct.members);

    // is_slice
    MirType *  dest_is_slice_type = mir_get_struct_elem_type(rtti_type, 3);
    VMStackPtr dest_is_slice      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 3);
    const bool is_slice           = type->kind == MIR_TYPE_SLICE || type->kind == MIR_TYPE_VARGS;
    vm_write_int(dest_is_slice_type, dest_is_slice, (u64)is_slice);

    // is_union
    MirType *  dest_is_union_type = mir_get_struct_elem_type(rtti_type, 4);
    VMStackPtr dest_is_union      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 4);
    vm_write_int(dest_is_union_type, dest_is_union, (u64)type->data.strct.is_union);

    // is_dynamic_array
    MirType *  dest_is_da_type = mir_get_struct_elem_type(rtti_type, 5);
    VMStackPtr dest_is_da      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 5);
    const bool is_da           = type->kind == MIR_TYPE_DYNARR;
    vm_write_int(dest_is_da_type, dest_is_da, (u64)is_da);

    return rtti_var;
}

void rtti_gen_fn_arg(Context *cnt, VMStackPtr dest, MirArg *arg)
{
    MirType *rtti_type = cnt->builtin_types->t_TypeInfoFnArg;

    // name
    MirType *   dest_name_type = mir_get_struct_elem_type(rtti_type, 0);
    VMStackPtr  dest_name      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 0);
    const char *arg_name       = arg->id ? arg->id->str : "";
    vm_write_string(cnt->vm, dest_name_type, dest_name, arg_name, strlen(arg_name));

    // base_type
    MirType *  dest_base_type_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr dest_base_type      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);
    MirVar *   base_type           = _rtti_gen(cnt, arg->type);
    vm_write_ptr(dest_base_type_type, dest_base_type, base_type->value.data);
}

VMStackPtr rtti_gen_fn_args_array(Context *cnt, TSmallArray_ArgPtr *args)
{
    MirType *rtti_type    = cnt->builtin_types->t_TypeInfoFnArg;
    MirType *arr_tmp_type = create_type_array(cnt, NULL, rtti_type, (s64)args->size);

    VMStackPtr dest_arr_tmp = vm_alloc_raw(cnt->vm, cnt->assembly, arr_tmp_type);

    MirArg *it;
    TSA_FOREACH(args, it)
    {
        VMStackPtr dest_arr_tmp_elem = vm_get_array_elem_ptr(arr_tmp_type, dest_arr_tmp, (u32)i);
        rtti_gen_fn_arg(cnt, dest_arr_tmp_elem, it);
    }

    return dest_arr_tmp;
}

VMStackPtr rtti_gen_fns_array(Context *cnt, TSmallArray_TypePtr *fns)
{
    MirType *  rtti_type    = cnt->builtin_types->t_TypeInfoFn_ptr;
    MirType *  arr_tmp_type = create_type_array(cnt, NULL, rtti_type, (s64)fns->size);
    VMStackPtr dest_arr_tmp = vm_alloc_raw(cnt->vm, cnt->assembly, arr_tmp_type);
    MirType *  it;
    TSA_FOREACH(fns, it)
    {
        VMStackPtr dest_arr_tmp_elem = vm_get_array_elem_ptr(arr_tmp_type, dest_arr_tmp, (u32)i);
        MirVar *   fn                = _rtti_gen(cnt, it);
        vm_write_ptr(rtti_type, dest_arr_tmp_elem, vm_read_var(cnt->vm, fn));
    }
    return dest_arr_tmp;
}

void rtti_gen_fn_args_slice(Context *cnt, VMStackPtr dest, TSmallArray_ArgPtr *args)
{
    MirType *  rtti_type     = cnt->builtin_types->t_TypeInfoFnArgs_slice;
    MirType *  dest_len_type = mir_get_struct_elem_type(rtti_type, 0);
    VMStackPtr dest_len      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 0);

    MirType *  dest_ptr_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr dest_ptr      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);

    const usize argc     = args ? args->size : 0;
    VMStackPtr  args_ptr = NULL;
    if (argc) args_ptr = rtti_gen_fn_args_array(cnt, args);

    vm_write_int(dest_len_type, dest_len, (u64)argc);
    vm_write_ptr(dest_ptr_type, dest_ptr, args_ptr);
}

void rtti_gen_fn_slice(Context *cnt, VMStackPtr dest, TSmallArray_TypePtr *fns)
{
    MirType *  rtti_type     = cnt->builtin_types->t_TypeInfoFn_ptr_slice;
    MirType *  dest_len_type = mir_get_struct_elem_type(rtti_type, 0);
    VMStackPtr dest_len      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 0);

    MirType *  dest_ptr_type = mir_get_struct_elem_type(rtti_type, 1);
    VMStackPtr dest_ptr      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);

    const usize fnc     = fns ? fns->size : 0;
    VMStackPtr  fns_ptr = NULL;
    if (fnc) fns_ptr = rtti_gen_fns_array(cnt, fns);

    vm_write_int(dest_len_type, dest_len, (u64)fnc);
    vm_write_ptr(dest_ptr_type, dest_ptr, fns_ptr);
}

MirVar *rtti_gen_fn(Context *cnt, MirType *type)
{
    MirType *  rtti_type = cnt->builtin_types->t_TypeInfoFn;
    MirVar *   rtti_var  = rtti_create_and_alloc_var(cnt, rtti_type);
    VMStackPtr dest      = vm_read_var(cnt->vm, rtti_var);
    rtti_gen_base(cnt, dest, type->kind, type->store_size_bytes);

#if 0
	// name
	MirType *   dest_name_type = mir_get_struct_elem_type(rtti_type, 1);
	VMStackPtr  dest_name      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);
	const char *name           = type->user_id ? type->user_id->str : type->id.str;
	vm_write_string(cnt->vm, dest_name_type, dest_name, name, strlen(name));
#endif

    // args
    VMStackPtr dest_args = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);
    rtti_gen_fn_args_slice(cnt, dest_args, type->data.fn.args);

    // ret_type
    MirType *  dest_ret_type_type = mir_get_struct_elem_type(rtti_type, 2);
    VMStackPtr dest_ret_type      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 2);
    MirVar *   ret_type           = _rtti_gen(cnt, type->data.fn.ret_type);
    vm_write_ptr(dest_ret_type_type, dest_ret_type, ret_type->value.data);

    // is_vargs
    MirType *  dest_is_vargs_type = mir_get_struct_elem_type(rtti_type, 3);
    VMStackPtr dest_is_vargs      = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 3);
    vm_write_int(dest_is_vargs_type, dest_is_vargs, (u64)type->data.fn.is_vargs);

    return rtti_var;
}

MirVar *rtti_gen_fn_group(Context *cnt, MirType *type)
{
    MirType *  rtti_type = cnt->builtin_types->t_TypeInfoFnGroup;
    MirVar *   rtti_var  = rtti_create_and_alloc_var(cnt, rtti_type);
    VMStackPtr dest      = vm_read_var(cnt->vm, rtti_var);
    rtti_gen_base(cnt, dest, type->kind, type->store_size_bytes);

    // variants
    VMStackPtr dest_args = vm_get_struct_elem_ptr(cnt->assembly, rtti_type, dest, 1);
    rtti_gen_fn_slice(cnt, dest_args, type->data.fn_group.variants);

    return rtti_var;
}

// MIR builting
void ast_defer_block(Context *cnt, Ast *block, bool whole_tree)
{
    TSmallArray_DeferStack *stack = &cnt->ast.fnctx->defer_stack;
    Ast *                   defer;
    for (usize i = stack->size; i-- > 0;) {
        defer = stack->data[i];
        if (defer->owner_scope == block->owner_scope) {
            tsa_pop_DeferStack(stack);
        } else if (!whole_tree) {
            break;
        }
        ast(cnt, defer->data.stmt_defer.expr);
    }
}

void ast_ublock(Context *cnt, Ast *ublock)
{
    Ast *tmp;
    TARRAY_FOREACH(Ast *, ublock->data.ublock.nodes, tmp) ast(cnt, tmp);
}

void ast_block(Context *cnt, Ast *block)
{
    if (cnt->debug_mode) {
        MirFn *current_fn = ast_current_fn(cnt);
        BL_ASSERT(current_fn);
    }

    Ast *tmp;
    TARRAY_FOREACH(Ast *, block->data.block.nodes, tmp) ast(cnt, tmp);

    if (!block->data.block.has_return) ast_defer_block(cnt, block, false);
}

void ast_unrecheable(Context *cnt, Ast *unr)
{
    append_instr_unrecheable(cnt, unr);
}

void ast_stmt_if(Context *cnt, Ast *stmt_if)
{
    Ast *ast_cond = stmt_if->data.stmt_if.test;
    Ast *ast_then = stmt_if->data.stmt_if.true_stmt;
    Ast *ast_else = stmt_if->data.stmt_if.false_stmt;
    BL_ASSERT(ast_cond && ast_then);

    MirFn *fn = ast_current_fn(cnt);
    BL_ASSERT(fn);

    MirInstrBlock *tmp_block  = NULL;
    MirInstrBlock *then_block = append_block(cnt, fn, "if_then");
    MirInstrBlock *else_block = append_block(cnt, fn, "if_else");
    MirInstrBlock *cont_block = append_block(cnt, fn, "if_cont");

    MirInstr *cond = ast(cnt, ast_cond);
    append_instr_cond_br(cnt, stmt_if, cond, then_block, else_block);

    // then block
    set_current_block(cnt, then_block);
    ast(cnt, ast_then);

    tmp_block = ast_current_block(cnt);
    if (!get_block_terminator(tmp_block)) {
        // block has not been terminated -> add terminator
        append_instr_br(cnt, NULL, cont_block);
    }

    // else if
    if (ast_else) {
        set_current_block(cnt, else_block);
        ast(cnt, ast_else);

        tmp_block = ast_current_block(cnt);
        if (!is_block_terminated(tmp_block)) {
            append_instr_br(cnt, NULL, cont_block);
        }
    }

    if (!is_block_terminated(else_block)) {
        // block has not been terminated -> add terminator
        set_current_block(cnt, else_block);
        append_instr_br(cnt, NULL, cont_block);
    }

    set_current_block(cnt, cont_block);
}

void ast_stmt_loop(Context *cnt, Ast *loop)
{
    Ast *ast_block     = loop->data.stmt_loop.block;
    Ast *ast_cond      = loop->data.stmt_loop.condition;
    Ast *ast_increment = loop->data.stmt_loop.increment;
    Ast *ast_init      = loop->data.stmt_loop.init;
    BL_ASSERT(ast_block);

    MirFn *fn = ast_current_fn(cnt);
    BL_ASSERT(fn);

    // prepare all blocks
    MirInstrBlock *tmp_block       = NULL;
    MirInstrBlock *increment_block = ast_increment ? append_block(cnt, fn, "loop_increment") : NULL;
    MirInstrBlock *decide_block    = append_block(cnt, fn, "loop_decide");
    MirInstrBlock *body_block      = append_block(cnt, fn, "loop_body");
    MirInstrBlock *cont_block      = append_block(cnt, fn, "loop_continue");

    MirInstrBlock *prev_break_block    = cnt->ast.break_block;
    MirInstrBlock *prev_continue_block = cnt->ast.continue_block;
    cnt->ast.break_block               = cont_block;
    cnt->ast.continue_block            = ast_increment ? increment_block : decide_block;

    // generate initialization if there is one
    if (ast_init) {
        ast(cnt, ast_init);
    }

    // decide block
    append_instr_br(cnt, loop, decide_block);
    set_current_block(cnt, decide_block);

    MirInstr *cond = ast_cond ? ast(cnt, ast_cond) : append_instr_const_bool(cnt, NULL, true);

    append_instr_cond_br(cnt, ast_cond, cond, body_block, cont_block);

    // loop body
    set_current_block(cnt, body_block);
    ast(cnt, ast_block);

    tmp_block = ast_current_block(cnt);
    if (!is_block_terminated(tmp_block)) {
        append_instr_br(cnt, loop, ast_increment ? increment_block : decide_block);
    }

    // increment if there is one
    if (ast_increment) {
        set_current_block(cnt, increment_block);
        ast(cnt, ast_increment);
        append_instr_br(cnt, loop, decide_block);
    }

    cnt->ast.break_block    = prev_break_block;
    cnt->ast.continue_block = prev_continue_block;
    set_current_block(cnt, cont_block);
}

void ast_stmt_break(Context *cnt, Ast *br)
{
    BL_ASSERT(cnt->ast.break_block && "break statement outside the loop");
    append_instr_br(cnt, br, cnt->ast.break_block);
}

void ast_stmt_continue(Context *cnt, Ast *cont)
{
    BL_ASSERT(cnt->ast.continue_block && "break statement outside the loop");
    append_instr_br(cnt, cont, cnt->ast.continue_block);
}

void ast_stmt_switch(Context *cnt, Ast *stmt_switch)
{
    TSmallArray_AstPtr *ast_cases = stmt_switch->data.stmt_switch.cases;
    BL_ASSERT(ast_cases);

    TSmallArray_SwitchCase *cases = create_sarr(TSmallArray_SwitchCase, cnt->assembly);

    MirFn *fn = ast_current_fn(cnt);
    BL_ASSERT(fn);

    MirInstrBlock *src_block            = ast_current_block(cnt);
    MirInstrBlock *cont_block           = append_block(cnt, fn, "switch_continue");
    MirInstrBlock *default_block        = cont_block;
    bool           user_defined_default = false;

    for (usize i = ast_cases->size; i-- > 0;) {
        Ast *      ast_case   = ast_cases->data[i];
        const bool is_default = ast_case->data.stmt_case.is_default;

        MirInstrBlock *case_block = NULL;

        if (ast_case->data.stmt_case.block) {
            case_block = append_block(cnt, fn, is_default ? "switch_default" : "switch_case");
            set_current_block(cnt, case_block);
            ast(cnt, ast_case->data.stmt_case.block);

            MirInstrBlock *curr_block = ast_current_block(cnt);
            if (!is_block_terminated(curr_block)) {
                append_instr_br(cnt, ast_case, cont_block);
            }
        } else {
            // Handle empty cases.
            case_block = cont_block;
        }

        if (is_default) {
            default_block        = case_block;
            user_defined_default = true;
            continue;
        }

        TSmallArray_AstPtr *ast_exprs = ast_case->data.stmt_case.exprs;

        for (usize i2 = ast_exprs->size; i2-- > 0;) {
            Ast *ast_expr = ast_exprs->data[i2];

            set_current_block(cnt, src_block);
            MirSwitchCase c = {.on_value = ast(cnt, ast_expr), .block = case_block};
            tsa_push_SwitchCase(cases, c);
        }
    }

    // Generate instructions for switch value and create switch itself.
    set_current_block(cnt, src_block);
    MirInstr *value = ast(cnt, stmt_switch->data.stmt_switch.expr);
    append_instr_switch(cnt, stmt_switch, value, default_block, user_defined_default, cases);
    set_current_block(cnt, cont_block);
}

void ast_stmt_return(Context *cnt, Ast *ret)
{
    // Return statement produce only setup of .ret temporary and break into the exit
    // block of the function.
    TSmallArray_AstPtr *ast_values     = ret->data.stmt_return.exprs;
    const bool          is_multireturn = ast_values && ast_values->size > 1;
    MirInstr *          value          = NULL;
    if (is_multireturn) {
        // Generate multi-return compound expression to group all values into single one.
        const usize           valc   = ast_values->size;
        TSmallArray_InstrPtr *values = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
        tsa_resize_InstrPtr(values, valc);
        Ast *ast_value = NULL;
        for (usize i = valc; i-- > 0;) {
            ast_value = ast_values->data[i];
            value     = ast(cnt, ast_value);
            BL_ASSERT(value);
            values->data[i] = value;
            SET_IS_NAKED_IF_COMPOUND(value, false);
        }
        BL_ASSERT(ast_value);
        value = append_instr_compound(cnt, ast_value, NULL, values, true);
    } else if (ast_values) {
        Ast *ast_value = ast_values->data[0];
        BL_ASSERT(ast_value &&
                  "Expected at least one return value when return expression array is not NULL.");
        value = ast(cnt, ast_value);
    }
    if (!is_current_block_terminated(cnt)) {
        MirFn *fn = ast_current_fn(cnt);
        BL_ASSERT(fn);
        if (fn->ret_tmp) {
            if (!value) {
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_EXPECTED_EXPR,
                            ret->location,
                            BUILDER_CUR_AFTER,
                            "Expected return value.");
                return;
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
    BL_ASSERT(cnt->ast.fnctx->exit_block);
    append_instr_br(cnt, ret, cnt->ast.fnctx->exit_block);
}

void ast_stmt_defer(Context *cnt, Ast *defer)
{
    // push new defer record
    tsa_push_DeferStack(&cnt->ast.fnctx->defer_stack, defer);
}

MirInstr *ast_call_loc(Context *cnt, Ast *loc)
{
    return append_instr_call_loc(cnt, loc);
}

MirInstr *ast_expr_compound(Context *cnt, Ast *cmp)
{
    TSmallArray_AstPtr *ast_values = cmp->data.expr_compound.values;
    Ast *               ast_type   = cmp->data.expr_compound.type;
    MirInstr *          type       = NULL;
    BL_ASSERT(ast_type);

    type = ast(cnt, ast_type);
    BL_ASSERT(type);
    if (!ast_values) return append_instr_compound(cnt, cmp, type, NULL, false);
    const usize           valc   = ast_values->size;
    TSmallArray_InstrPtr *values = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
    tsa_resize_InstrPtr(values, valc);
    Ast *     ast_value;
    MirInstr *value;
    // Values must be appended in reverse order.
    for (usize i = valc; i-- > 0;) {
        ast_value = ast_values->data[i];
        value     = ast(cnt, ast_value);
        BL_ASSERT(value);
        values->data[i] = value;
        SET_IS_NAKED_IF_COMPOUND(value, false);
    }
    return append_instr_compound(cnt, cmp, type, values, false);
}

MirInstr *ast_expr_addrof(Context *cnt, Ast *addrof)
{
    MirInstr *src = ast(cnt, addrof->data.expr_addrof.next);
    BL_ASSERT(src);

    return append_instr_addrof(cnt, addrof, src);
}

MirInstr *ast_expr_cast(Context *cnt, Ast *cast)
{
    const bool auto_cast = cast->data.expr_cast.auto_cast;
    Ast *      ast_type  = cast->data.expr_cast.type;
    Ast *      ast_next  = cast->data.expr_cast.next;
    BL_ASSERT(ast_next);

    // INCOMPLETE: const type!!!
    MirInstr *type = NULL;

    if (!auto_cast) {
        BL_ASSERT(ast_type);
        type = CREATE_TYPE_RESOLVER_CALL(ast_type);
    }

    MirInstr *next = ast(cnt, ast_next);

    return append_instr_cast(cnt, cast, type, next);
}

MirInstr *ast_expr_sizeof(Context *cnt, Ast *szof)
{
    Ast *ast_node = szof->data.expr_sizeof.node;
    BL_ASSERT(ast_node);

    MirInstr *expr = ast(cnt, ast_node);
    return append_instr_sizeof(cnt, szof, expr);
}

MirInstr *ast_expr_type_info(Context *cnt, Ast *type_info)
{
    Ast *ast_node = type_info->data.expr_type_info.node;
    BL_ASSERT(ast_node);

    MirInstr *expr = ast(cnt, ast_node);
    return append_instr_type_info(cnt, type_info, expr);
}

MirInstr *ast_expr_test_cases(Context *cnt, Ast *test_cases)
{
    return append_instr_test_cases(cnt, test_cases);
}

MirInstr *ast_expr_alignof(Context *cnt, Ast *szof)
{
    Ast *ast_node = szof->data.expr_alignof.node;
    BL_ASSERT(ast_node);

    MirInstr *expr = ast(cnt, ast_node);
    return append_instr_alignof(cnt, szof, expr);
}

MirInstr *ast_expr_deref(Context *cnt, Ast *deref)
{
    MirInstr *next = ast(cnt, deref->data.expr_deref.next);
    BL_ASSERT(next);
    MirInstrLoad *load = (MirInstrLoad *)append_instr_load(cnt, deref, next);
    load->is_deref     = true;
    return &load->base;
}

MirInstr *ast_expr_lit_int(Context *cnt, Ast *expr)
{
    u64 val = expr->data.expr_integer.val;

    if (expr->data.expr_integer.overflow) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_NUM_LIT_OVERFLOW,
                    expr->location,
                    BUILDER_CUR_WORD,
                    "Integer literal is too big and cannot be represented as any "
                    "integer type.");
    }

    MirType * type         = NULL;
    const int desired_bits = count_bits(val);

    // Here we choose best type for const integer literal: s32, s64 or u64. When u64 is
    // selected, this number cannot be negative.
    if (desired_bits < 32) {
        type = cnt->builtin_types->t_s32;
    } else if (desired_bits < 64) {
        type = cnt->builtin_types->t_s64;
    } else {
        type = cnt->builtin_types->t_u64;
    }

    return append_instr_const_int(cnt, expr, type, val);
}

MirInstr *ast_expr_lit_float(Context *cnt, Ast *expr)
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

MirInstr *ast_expr_lit_double(Context *cnt, Ast *expr)
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

MirInstr *ast_expr_lit_bool(Context *cnt, Ast *expr)
{
    return append_instr_const_bool(cnt, expr, expr->data.expr_boolean.val);
}

MirInstr *ast_expr_lit_char(Context *cnt, Ast *expr)
{
    return append_instr_const_char(cnt, expr, (s8)expr->data.expr_character.val);
}

MirInstr *ast_expr_null(Context *cnt, Ast *nl)
{
    return append_instr_const_null(cnt, nl);
}

MirInstr *ast_expr_call(Context *cnt, Ast *call)
{
    Ast *               ast_callee = call->data.expr_call.ref;
    TSmallArray_AstPtr *ast_args   = call->data.expr_call.args;
    BL_ASSERT(ast_callee);

    TSmallArray_InstrPtr *args = create_sarr(TSmallArray_InstrPtr, cnt->assembly);

    // arguments need to be generated into reverse order due to bytecode call
    // conventions
    if (ast_args) {
        const usize argc = ast_args->size;
        tsa_resize_InstrPtr(args, argc);
        MirInstr *arg;
        Ast *     ast_arg;
        for (usize i = argc; i-- > 0;) {
            ast_arg = ast_args->data[i];
            arg     = ast(cnt, ast_arg);

            args->data[i] = arg;
        }
    }

    MirInstr *callee = ast(cnt, ast_callee);

    return append_instr_call(cnt, call, callee, args);
}

MirInstr *ast_expr_elem(Context *cnt, Ast *elem)
{
    Ast *ast_arr   = elem->data.expr_elem.next;
    Ast *ast_index = elem->data.expr_elem.index;
    BL_ASSERT(ast_arr && ast_index);

    MirInstr *arr_ptr = ast(cnt, ast_arr);
    MirInstr *index   = ast(cnt, ast_index);

    return append_instr_elem_ptr(cnt, elem, arr_ptr, index);
}

MirInstr *ast_expr_lit_fn(Context *        cnt,
                          Ast *            lit_fn,
                          Ast *            decl_node,
                          Ast *            explicit_linkage_name,
                          bool             is_global,
                          u32              flags,
                          MirBuiltinIdKind builtin_id)
{
    // creates function prototype
    Ast *ast_block   = lit_fn->data.expr_fn.block;
    Ast *ast_fn_type = lit_fn->data.expr_fn.type;

    MirInstrFnProto *fn_proto =
        (MirInstrFnProto *)append_instr_fn_proto(cnt, lit_fn, NULL, NULL, true);

    // Generate type resolver for function type.
    fn_proto->type = CREATE_TYPE_RESOLVER_CALL(ast_fn_type);
    BL_ASSERT(fn_proto->type);

    // Prepare new function context. Must be in sync with pop at the end of scope!
    // DON'T CALL FINISH BEFORE THIS!!!
    // DON'T CALL FINISH BEFORE THIS!!!
    // DON'T CALL FINISH BEFORE THIS!!!
    ast_push_fn_context(cnt);
    MirInstrBlock *prev_block = ast_current_block(cnt);

    const char *linkage_name =
        explicit_linkage_name ? explicit_linkage_name->data.ident.id.str : NULL;

    BL_ASSERT(decl_node ? decl_node->kind == AST_IDENT : true);
    MirFn *fn = create_fn(cnt,
                          decl_node ? decl_node : lit_fn,
                          decl_node ? &decl_node->data.ident.id : NULL,
                          linkage_name,
                          (u32)flags,
                          fn_proto,
                          true,
                          is_global,
                          builtin_id);

    MIR_CEV_WRITE_AS(MirFn *, &fn_proto->base.value, fn);

    // FUNCTION BODY
    // External or intrinsic function declaration has no body so we can skip body generation.
    if (IS_FLAG(flags, FLAG_EXTERN) || IS_FLAG(flags, FLAG_INTRINSIC)) {
        if (ast_block) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_UNEXPECTED_FUNCTION_BODY,
                        ast_block->location,
                        BUILDER_CUR_WORD,
                        "Unexpected body, for %s function.",
                        IS_FLAG(flags, FLAG_EXTERN) ? "external" : "intrinsic");
        }
        goto FINISH;
    }

    if (!ast_block) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_EXPECTED_BODY,
                    decl_node ? decl_node->location : lit_fn->location,
                    BUILDER_CUR_WORD,
                    "Missing function body.");
        goto FINISH;
    }

    // Set body scope for DI.
    fn->body_scope = ast_block->owner_scope;

    // create block for initialization locals and arguments
    MirInstrBlock *init_block = append_block(cnt, fn, "entry");

    // Every user generated function must contain exit block; this block is invoked last
    // in every function eventually can return .ret value stored in temporary storage.
    // When ast parser hit user defined 'return' statement it sets up .ret temporary if
    // there is one and produce break into exit block. This approach is needed due to
    // defer statement, because we need to call defer blocks after return value
    // evaluation and before terminal instruction of the function. Last defer block
    // always breaks into the exit block.
    cnt->ast.fnctx->exit_block = append_block(cnt, fn, "exit");
    ref_instr(&cnt->ast.fnctx->exit_block->base);

    if (ast_fn_type->data.type_fn.ret_type) {
        set_current_block(cnt, init_block);
        fn->ret_tmp =
            append_instr_decl_var_impl(cnt, gen_uq_name(IMPL_RET_TMP), NULL, NULL, true, false);

        set_current_block(cnt, cnt->ast.fnctx->exit_block);
        MirInstr *ret_init = append_instr_decl_direct_ref(cnt, fn->ret_tmp);

        append_instr_ret(cnt, ast_block, ret_init);
    } else {
        set_current_block(cnt, cnt->ast.fnctx->exit_block);
        append_instr_ret(cnt, ast_block, NULL);
    }

    set_current_block(cnt, init_block);

    // build MIR for fn arguments
    TSmallArray_AstPtr *ast_args = ast_fn_type->data.type_fn.args;
    if (ast_args) {
        Ast *ast_arg;
        Ast *ast_arg_name;

        const usize argc = ast_args->size;
        for (usize i = argc; i-- > 0;) {
            ast_arg = ast_args->data[i];
            BL_ASSERT(ast_arg->kind == AST_DECL_ARG);
            ast_arg_name = ast_arg->data.decl.name;
            BL_ASSERT(ast_arg_name);
            BL_ASSERT(ast_arg_name->kind == AST_IDENT && "Expected identificator.");

            // create tmp declaration for arg variable
            MirInstr *       arg = append_instr_arg(cnt, NULL, (u32)i);
            MirInstrDeclVar *decl_var =
                (MirInstrDeclVar *)append_instr_decl_var(cnt,
                                                         ast_arg_name,
                                                         &ast_arg_name->data.ident.id,
                                                         ast_arg_name->owner_scope,
                                                         NULL,
                                                         arg,
                                                         true,
                                                         0,
                                                         MIR_BUILTIN_ID_NONE);

            decl_var->var->entry = register_symbol(
                cnt, ast_arg_name, &ast_arg_name->data.ident.id, ast_arg_name->owner_scope, false);
        }
    }

    if (IS_FLAG(flags, FLAG_TEST_FN)) {
        ++cnt->testing.expected_test_count;
    }

    // generate body instructions
    ast(cnt, ast_block);

FINISH:
    set_current_block(cnt, prev_block);
    ast_pop_fn_context(cnt);
    return &fn_proto->base;
}

MirInstr *ast_expr_lit_fn_group(Context *cnt, Ast *group)
{
    TSmallArray_AstPtr *ast_variants = group->data.expr_fn_group.variants;
    BL_ASSERT(ast_variants);
    BL_ASSERT(ast_variants->size);
    TSmallArray_InstrPtr *variants = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
    tsa_resize_InstrPtr(variants, ast_variants->size);
    Ast *it;
    TSA_FOREACH(group->data.expr_fn_group.variants, it)
    {
        MirInstr *variant;
        if (it->kind == AST_EXPR_LIT_FN) {
            variant = ast_expr_lit_fn(cnt, it, NULL, NULL, true, 0, MIR_BUILTIN_ID_NONE);
        } else {
            variant = ast(cnt, it);
        }
        variants->data[i] = variant;
    }
    return append_instr_fn_group(cnt, group, variants);
}

MirInstr *ast_expr_lit_string(Context *cnt, Ast *lit_string)
{
    const char *cstr = lit_string->data.expr_string.val;
    BL_ASSERT(cstr);
    return append_instr_const_string(cnt, lit_string, cstr);
}

MirInstr *ast_expr_binop(Context *cnt, Ast *binop)
{
    Ast *ast_lhs = binop->data.expr_binop.lhs;
    Ast *ast_rhs = binop->data.expr_binop.rhs;
    BL_ASSERT(ast_lhs && ast_rhs);

    const BinopKind op = binop->data.expr_binop.kind;

    switch (op) {
    case BINOP_ASSIGN: {
        MirInstr *rhs = ast(cnt, ast_rhs);
        MirInstr *lhs = ast(cnt, ast_lhs);

        // In case right hand side expression is compound initializer, we don't need
        // temp storage for it, we can just copy compound content directly into
        // variable, so we set it here as non-naked.
        SET_IS_NAKED_IF_COMPOUND(rhs, false);
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

    case BINOP_LOGIC_AND:
    case BINOP_LOGIC_OR: {
        const bool     swap_condition   = op == BINOP_LOGIC_AND;
        MirFn *        fn               = ast_current_fn(cnt);
        MirInstrBlock *rhs_block        = append_block(cnt, fn, "rhs_block");
        MirInstrBlock *end_block        = cnt->ast.current_phi_end_block;
        MirInstrPhi *  phi              = cnt->ast.current_phi;
        bool           append_end_block = false;
        // If no end block is specified, we are on the top level of PHI expresion generation and we
        // must create one. Also PHI instruction must be crated (but not appended yet); created PHI
        // gather incomes from all nested branches created by expression.
        if (!end_block) {
            BL_ASSERT(!phi);
            end_block                      = create_block(cnt, "end_block");
            phi                            = (MirInstrPhi *)create_instr_phi(cnt, binop);
            cnt->ast.current_phi_end_block = end_block;
            cnt->ast.current_phi           = phi;
            append_end_block               = true;
        }

        MirInstr *lhs = ast(cnt, ast_lhs);
        MirInstr *brk = NULL;
        if (swap_condition) {
            brk = append_instr_cond_br(cnt, NULL, lhs, rhs_block, end_block);
        } else {
            brk = append_instr_cond_br(cnt, NULL, lhs, end_block, rhs_block);
        }
        phi_add_income(phi, brk, ast_current_block(cnt));
        set_current_block(cnt, rhs_block);
        MirInstr *rhs = ast(cnt, ast_rhs);
        if (append_end_block) {
            append_instr_br(cnt, NULL, end_block);
            phi_add_income(phi, rhs, ast_current_block(cnt));
            append_block2(cnt, fn, end_block);
            set_current_block(cnt, end_block);
            append_current_block(cnt, &phi->base);
            cnt->ast.current_phi_end_block = NULL;
            cnt->ast.current_phi           = NULL;
            return &phi->base;
        }
        return rhs;
    }

    default: {
        MirInstr *rhs = ast(cnt, ast_rhs);
        MirInstr *lhs = ast(cnt, ast_lhs);
        return append_instr_binop(cnt, binop, lhs, rhs, op);
    }
    }
}

MirInstr *ast_expr_unary(Context *cnt, Ast *unop)
{
    Ast *ast_next = unop->data.expr_unary.next;
    BL_ASSERT(ast_next);

    MirInstr *next = ast(cnt, ast_next);
    BL_ASSERT(next);

    return append_instr_unop(cnt, unop, next, unop->data.expr_unary.kind);
}

MirInstr *ast_expr_type(Context *cnt, Ast *type)
{
    Ast *next_type = type->data.expr_type.type;
    BL_ASSERT(next_type);

    return ast(cnt, next_type);
}

static INLINE MirBuiltinIdKind check_symbol_marked_compiler(Ast *ident)
{
    // Check builtin ids for symbols marked as compiler.
    MirBuiltinIdKind builtin_id = get_builtin_kind(ident);
    if (builtin_id == MIR_BUILTIN_ID_NONE) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_UNKNOWN_SYMBOL,
                    ident->location,
                    BUILDER_CUR_WORD,
                    "Symbol marked as #compiler is not known compiler internal.");
    }
    return builtin_id;
}

// Helper for function declaration generation.
static void ast_decl_fn(Context *cnt, Ast *ast_fn)
{
    Ast *ast_name  = ast_fn->data.decl.name;
    Ast *ast_type  = ast_fn->data.decl.type;
    Ast *ast_value = ast_fn->data.decl_entity.value;

    // recognized named function declaration
    Ast *      ast_explicit_linkage_name = ast_fn->data.decl_entity.explicit_linkage_name;
    const s32  flags                     = ast_fn->data.decl_entity.flags;
    const bool is_mutable                = ast_fn->data.decl_entity.mut;
    const bool generate_entry            = cnt->assembly->target->kind == ASSEMBLY_EXECUTABLE;
    if (!generate_entry && IS_FLAG(flags, FLAG_ENTRY)) {
        // Generate entry function only in case we are compiling executable binary, otherwise it's
        // not needed, and main should be also optional.
        return;
    }
    if (is_mutable) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_MUTABILITY,
                    ast_name->location,
                    BUILDER_CUR_WORD,
                    "Function declaration is expected to be immutable.");
    }

    const bool is_multidecl = ast_name->data.ident.next;
    if (is_multidecl) {
        const Ast *ast_next_name = ast_name->data.ident.next;
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_NAME,
                    ast_next_name->location,
                    BUILDER_CUR_WORD,
                    "Function cannot be multi-declared.");
    }

    MirBuiltinIdKind builtin_id  = MIR_BUILTIN_ID_NONE;
    const bool       is_compiler = IS_FLAG(ast_fn->data.decl_entity.flags, FLAG_COMPILER);
    if (is_compiler) builtin_id = check_symbol_marked_compiler(ast_name);
    const bool is_global = ast_fn->data.decl_entity.is_global;
    MirInstr * value     = ast_expr_lit_fn(
        cnt, ast_value, ast_name, ast_explicit_linkage_name, is_global, flags, builtin_id);
    if (ast_type) ((MirInstrFnProto *)value)->user_type = CREATE_TYPE_RESOLVER_CALL(ast_type);

    BL_ASSERT(value);
    MirFn *fn = MIR_CEV_READ_AS(MirFn *, &value->value);
    BL_MAGIC_ASSERT(fn);

    // check main
    if (is_builtin(ast_name, MIR_BUILTIN_ID_MAIN)) {
        // This is reported as an error.
        fn->emit_llvm               = true;
        cnt->assembly->vm_run.entry = fn;
        if (scope_is_subtree_of_kind(ast_name->owner_scope, SCOPE_PRIVATE)) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_UNEXPECTED_DIRECTIVE,
                        ast_name->location,
                        BUILDER_CUR_WORD,
                        "Main function cannot be declared in private scope.");
        }
    }

    if (IS_FLAG(flags, FLAG_EXPORT) &&
        scope_is_subtree_of_kind(ast_name->owner_scope, SCOPE_PRIVATE)) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_UNEXPECTED_DIRECTIVE,
                    ast_name->location,
                    BUILDER_CUR_WORD,
                    "Exported function cannot be declared in private scope.");
    }

    ID *   id    = &ast_name->data.ident.id;
    Scope *scope = ast_name->owner_scope;
    fn->entry    = register_symbol(cnt, ast_name, id, scope, is_compiler);
}

// Helper for local variable declaration generation.
static void ast_decl_var_local(Context *cnt, Ast *ast_local)
{
    Ast *ast_name  = ast_local->data.decl.name;
    Ast *ast_type  = ast_local->data.decl.type;
    Ast *ast_value = ast_local->data.decl_entity.value;
    // Create type resolver if there is explicitly defined type mentioned by user in declaration.
    MirInstr * type        = ast_type ? CREATE_TYPE_RESOLVER_CALL(ast_type) : NULL;
    MirInstr * value       = ast(cnt, ast_value);
    const bool is_compiler = IS_FLAG(ast_local->data.decl_entity.flags, FLAG_COMPILER);
    const bool is_unroll   = ast_value && ast_value->kind == AST_EXPR_CALL;
    const bool is_mutable  = ast_local->data.decl_entity.mut;

    Scope *scope = ast_name->owner_scope;

    // Generate variables for all declarations.
    //
    // Variable groups can be initialized by multi-return function, in such situation we
    // must handle unrolling of function return value into separate variables. Sad thing
    // here is that we don't know if function return is multi-return because called function
    // has not been analyzed yet; however only multi-return (structs) produced by function
    // call has ability to unroll into separate variables; this is indicated by is_unroll
    // flag. We decide later during analyze if unroll is kept or not. So finally there
    // are tree possible cases:
    //
    // 1) Variable is not group: We generate unroll instruction as initializer in case value
    //    is function call; once when function return type is known we decide if only first
    //    return value should be picked or not. (unroll can be removed if not)
    //
    // 2) Variables in group: For every variable in group we generate unroll instruction
    //    with two possibilities what to do during analyze; when called function returns
    //    multiple values we use unroll to set every variable (by index) to proper value;
    //    initializer is any other value we generate vN .. v3 = v2 = v1 = value.
    //
    // 3) Variables in group initialized by single value: In such case we only follow rule
    //    vN .. v3 = v2 = v1 = value. Unroll is still generated when value is function call but
    //    is removed when function type is analyzed and considered to be multi-return.
    Ast *     ast_current_name = ast_name;
    MirInstr *current_value    = value;
    MirInstr *prev_var         = NULL;
    s32       index            = 0;
    while (ast_current_name) {
        MirBuiltinIdKind builtin_id = MIR_BUILTIN_ID_NONE;
        if (is_compiler) builtin_id = check_symbol_marked_compiler(ast_current_name);
        if (is_unroll) {
            current_value = append_instr_unroll(cnt, ast_current_name, value, prev_var, index++);
        } else if (prev_var) {
            current_value = append_instr_decl_direct_ref(cnt, prev_var);
        }
        ID *      id  = &ast_current_name->data.ident.id;
        MirInstr *var = append_instr_decl_var(cnt,
                                              ast_current_name,
                                              id,
                                              scope,
                                              type,
                                              current_value,
                                              is_mutable,
                                              ast_local->data.decl_entity.flags,
                                              builtin_id);
        ((MirInstrDeclVar *)var)->var->entry =
            register_symbol(cnt, ast_current_name, id, scope, is_compiler);

        BL_ASSERT(ast_current_name->kind == AST_IDENT);
        ast_current_name = ast_current_name->data.ident.next;
        prev_var         = var;
    }
}

static void ast_decl_var_global_or_struct(Context *cnt, Ast *ast_global)
{
    Ast *ast_name  = ast_global->data.decl.name;
    Ast *ast_type  = ast_global->data.decl.type;
    Ast *ast_value = ast_global->data.decl_entity.value;
    // Create type resolver if there is explicitly defined type mentioned by user in declaration.
    MirInstr * type           = ast_type ? CREATE_TYPE_RESOLVER_CALL(ast_type) : NULL;
    const bool is_struct_decl = ast_value && ast_value->kind == AST_EXPR_TYPE &&
                                ast_value->data.expr_type.type->kind == AST_TYPE_STRUCT;
    const bool is_mutable  = ast_global->data.decl_entity.mut;
    const bool is_compiler = IS_FLAG(ast_global->data.decl_entity.flags, FLAG_COMPILER);

    MirInstr *value = NULL;
    Scope *   scope = ast_name->owner_scope;

    // Struct use forward type declarations!
    if (is_struct_decl) {
        const bool is_multidecl = ast_name->data.ident.next;
        if (is_multidecl) {
            const Ast *ast_next_name = ast_name->data.ident.next;
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_NAME,
                        ast_next_name->location,
                        BUILDER_CUR_WORD,
                        " cannot be multi-declared.");
        }
        // Set to const type fwd decl
        MirType *fwd_decl_type =
            create_type_struct_incomplete(cnt, cnt->ast.current_entity_id, false);

        value = create_instr_const_type(cnt, ast_value, fwd_decl_type);
        ANALYZE_INSTR_RQ(value);

        // Set current fwd decl
        cnt->ast.current_fwd_struct_decl = value;
    }

    TSmallArray_InstrPtr *decls            = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
    Ast *                 ast_current_name = ast_name;
    while (ast_current_name) {
        MirBuiltinIdKind builtin_id = MIR_BUILTIN_ID_NONE;
        if (is_compiler) builtin_id = check_symbol_marked_compiler(ast_name);
        ID *      id   = &ast_current_name->data.ident.id;
        MirInstr *decl = append_instr_decl_var(cnt,
                                               ast_current_name,
                                               id,
                                               scope,
                                               type,
                                               value,
                                               is_mutable,
                                               ast_global->data.decl_entity.flags,
                                               builtin_id);
        tsa_push_InstrPtr(decls, decl);

        MirVar *var            = ((MirInstrDeclVar *)decl)->var;
        var->is_struct_typedef = is_struct_decl;
        var->entry             = register_symbol(cnt, ast_current_name, id, scope, is_compiler);
        ast_current_name       = ast_current_name->data.ident.next;
    }

    // For globals we must generate initialization after variable declaration,
    // SetInitializer instruction will be used to set actual value, also
    // implicit initialization block is created into MIR (such block does not
    // have LLVM representation -> globals must be evaluated in compile time).
    if (ast_value) {
        // Generate implicit global initializer block.
        ast_create_global_initializer2(cnt, ast_value, decls);
    } else {
        // Global has no explicit initialization in code so we must
        // create default global initializer.
        ast_create_global_initializer2(cnt, NULL, decls);
    }

    // Struct decl cleanup.
    if (is_struct_decl) {
        cnt->ast.current_fwd_struct_decl = NULL;
    }
}

MirInstr *ast_decl_entity(Context *cnt, Ast *entity)
{
    Ast *      ast_name       = entity->data.decl.name;
    Ast *      ast_value      = entity->data.decl_entity.value;
    const bool is_fn_decl     = ast_value && ast_value->kind == AST_EXPR_LIT_FN;
    const bool is_global      = entity->data.decl_entity.is_global;
    const bool is_struct_decl = ast_value && ast_value->kind == AST_EXPR_TYPE &&
                                ast_value->data.expr_type.type->kind == AST_TYPE_STRUCT;
    BL_ASSERT(ast_name && "Missing entity name.");
    BL_ASSERT(ast_name->kind == AST_IDENT && "Expected identificator.");
    if (is_fn_decl) {
        ast_decl_fn(cnt, entity);
    } else {
        cnt->ast.current_entity_id = &ast_name->data.ident.id;
        if (is_global || is_struct_decl) {
            ast_decl_var_global_or_struct(cnt, entity);
        } else {
            ast_decl_var_local(cnt, entity);
        }
        cnt->ast.current_entity_id = NULL;
    }
    return NULL;
}

MirInstr *ast_decl_arg(Context *cnt, Ast *arg)
{
    Ast *     ast_value = arg->data.decl_arg.value;
    Ast *     ast_name  = arg->data.decl.name;
    Ast *     ast_type  = arg->data.decl.type;
    MirInstr *value     = NULL;
    // Type is resolved in type resolver in case we have default value defined.
    MirInstr *type = NULL;

    if (ast_value) {
        type = CREATE_TYPE_RESOLVER_CALL(ast_type);
        // Main idea here is create implicit global constant to hold default value, since
        // value can be more complex compound with references, we need to use same solution
        // like we already use for globals. This approach is also more effective than
        // reinserting of whole MIR generated by expression on call side.
        //
        // Note: #call_location node must be created on caller side as constant so we don't
        // generate global container.
        if (ast_value->kind == AST_CALL_LOC) {
            value = ast(cnt, ast_value);
        } else {
            value = append_instr_decl_var_impl(cnt, IMPL_ARG_DEFAULT, type, NULL, false, true);
            ast_create_global_initializer(cnt, ast_value, value);
        }
    } else {
        BL_ASSERT(ast_type && "Function argument must have explicit type when no default "
                              "value is specified!");
        type = CREATE_TYPE_RESOLVER_CALL(ast_type);
    }
    return append_instr_decl_arg(cnt, ast_name, type, value);
}

MirInstr *ast_decl_member(Context *cnt, Ast *arg)
{
    Ast *                 ast_type = arg->data.decl.type;
    Ast *                 ast_name = arg->data.decl.name;
    Ast *                 ast_tags = arg->data.decl.tags;
    TSmallArray_InstrPtr *tags     = NULL;
    BL_ASSERT(ast_name);
    BL_ASSERT(ast_type);

    // has member user defined tags?
    if (ast_tags) {
        TSmallArray_AstPtr *ast_values = ast_tags->data.tags.values;
        BL_ASSERT(ast_values && "Invalid tag values array.");
        BL_ASSERT(ast_values->size && "Tag array must contains one value at least.");

        tags = create_sarr(TSmallArray_InstrPtr, cnt->assembly);

        Ast *ast_value;
        TSA_FOREACH(ast_values, ast_value)
        {
            MirInstr *value = ast(cnt, ast_value);
            tsa_push_InstrPtr(tags, value);
        }
    }
    MirInstr *result = ast(cnt, ast_type);
    BL_ASSERT(ast_name->kind == AST_IDENT);
    result = append_instr_decl_member(cnt, ast_name, result, tags);
    ((MirInstrDeclMember *)result)->member->entry =
        register_symbol(cnt, ast_name, &ast_name->data.ident.id, ast_name->owner_scope, false);
    BL_ASSERT(result);
    return result;
}

MirInstr *ast_decl_variant(Context *cnt, Ast *variant)
{
    Ast *ast_name  = variant->data.decl.name;
    Ast *ast_value = variant->data.decl_variant.value;
    BL_ASSERT(ast_name && "Missing enum variant name!");
    MirInstr *           value = ast(cnt, ast_value);
    MirInstrDeclVariant *variant_instr =
        (MirInstrDeclVariant *)append_instr_decl_variant(cnt, ast_name, value);
    variant_instr->variant->entry =
        register_symbol(cnt, ast_name, &ast_name->data.ident.id, ast_name->owner_scope, false);
    return (MirInstr *)variant_instr;
}

MirInstr *ast_ref(Context *cnt, Ast *ref)
{
    Ast *ident = ref->data.ref.ident;
    Ast *next  = ref->data.ref.next;
    BL_ASSERT(ident);
    Scope *scope = ident->owner_scope;
    Unit * unit  = ident->location->unit;
    BL_ASSERT(unit);
    BL_ASSERT(scope);
    if (next) {
        MirInstr *target = ast(cnt, next);
        return append_instr_member_ptr(cnt, ref, target, ident, NULL, MIR_BUILTIN_ID_NONE);
    }
    return append_instr_decl_ref(cnt, ref, unit, &ident->data.ident.id, scope, NULL);
}

MirInstr *ast_type_fn(Context *cnt, Ast *type_fn)
{
    Ast *               ast_ret_type  = type_fn->data.type_fn.ret_type;
    TSmallArray_AstPtr *ast_arg_types = type_fn->data.type_fn.args;

    // Discard current entity ID to fix bug when multi-return structure takes this name as an alias.
    // There should be probably better way to solve this issue, but lets keep this for now.
    cnt->ast.current_entity_id = NULL;
    // return type
    MirInstr *ret_type = NULL;
    if (ast_ret_type) {
        ret_type = ast(cnt, ast_ret_type);
        ref_instr(ret_type);
    }
    TSmallArray_InstrPtr *args = NULL;
    if (ast_arg_types && ast_arg_types->size) {
        const usize c = ast_arg_types->size;
        args          = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
        tsa_resize_InstrPtr(args, c);

        Ast *     ast_arg_type;
        MirInstr *arg;
        for (usize i = c; i-- > 0;) {
            ast_arg_type = ast_arg_types->data[i];
            arg          = ast(cnt, ast_arg_type);
            ref_instr(arg);
            args->data[i] = arg;
        }
    }
    return append_instr_type_fn(cnt, type_fn, ret_type, args);
}

MirInstr *ast_type_fn_group(Context *cnt, Ast *group)
{
    TSmallArray_AstPtr *ast_variants = group->data.type_fn_group.variants;
    BL_ASSERT(ast_variants);
    const usize           c        = ast_variants->size;
    TSmallArray_InstrPtr *variants = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
    tsa_resize_InstrPtr(variants, c);

    Ast *it;
    TSA_FOREACH(ast_variants, it)
    {
        variants->data[i] = ast(cnt, it);
    }
    // Consume declaration identificator.
    ID *id                     = cnt->ast.current_entity_id;
    cnt->ast.current_entity_id = NULL;
    return append_instr_type_fn_group(cnt, group, id, variants);
}

MirInstr *ast_type_arr(Context *cnt, Ast *type_arr)
{
    ID *id                     = cnt->ast.current_entity_id;
    cnt->ast.current_entity_id = NULL;

    Ast *ast_elem_type = type_arr->data.type_arr.elem_type;
    Ast *ast_len       = type_arr->data.type_arr.len;
    BL_ASSERT(ast_elem_type && ast_len);

    MirInstr *len       = ast(cnt, ast_len);
    MirInstr *elem_type = ast(cnt, ast_elem_type);
    return append_instr_type_array(cnt, type_arr, id, elem_type, len);
}

MirInstr *ast_type_slice(Context *cnt, Ast *type_slice)
{
    Ast *ast_elem_type = type_slice->data.type_slice.elem_type;
    BL_ASSERT(ast_elem_type);

    MirInstr *elem_type = ast(cnt, ast_elem_type);
    return append_instr_type_slice(cnt, type_slice, elem_type);
}

MirInstr *ast_type_dynarr(Context *cnt, Ast *type_dynarr)
{
    Ast *ast_elem_type = type_dynarr->data.type_dynarr.elem_type;
    BL_ASSERT(ast_elem_type);

    MirInstr *elem_type = ast(cnt, ast_elem_type);
    return append_instr_type_dynarr(cnt, type_dynarr, elem_type);
}

MirInstr *ast_type_ptr(Context *cnt, Ast *type_ptr)
{
    Ast *ast_type = type_ptr->data.type_ptr.type;
    BL_ASSERT(ast_type && "invalid pointee type");
    MirInstr *type = ast(cnt, ast_type);
    BL_ASSERT(type);

    if (type->kind == MIR_INSTR_DECL_REF) {
        // Enable incomplete types for pointers to declarations.
        ((MirInstrDeclRef *)type)->accept_incomplete_type = true;
    }

    return append_instr_type_ptr(cnt, type_ptr, type);
}

MirInstr *ast_type_vargs(Context *cnt, Ast *type_vargs)
{
    // type is optional (Any will be used when no type was specified)
    Ast *     ast_type = type_vargs->data.type_vargs.type;
    MirInstr *type     = ast(cnt, ast_type);
    return append_instr_type_vargs(cnt, type_vargs, type);
}

MirInstr *ast_type_enum(Context *cnt, Ast *type_enum)
{
    TSmallArray_AstPtr *ast_variants  = type_enum->data.type_enm.variants;
    Ast *               ast_base_type = type_enum->data.type_enm.type;
    BL_ASSERT(ast_variants);

    const usize varc = ast_variants->size;
    if (varc == 0) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_EMPTY_ENUM,
                    type_enum->location,
                    BUILDER_CUR_WORD,
                    "Empty enumerator.");
        return NULL;
    }

    MirInstr *base_type = ast(cnt, ast_base_type);

    Scope *               scope    = type_enum->data.type_enm.scope;
    TSmallArray_InstrPtr *variants = create_sarr(TSmallArray_InstrPtr, cnt->assembly);

    // Build variant instructions
    MirInstr *variant;
    Ast *     ast_variant;
    TSA_FOREACH(ast_variants, ast_variant)
    {
        variant = ast(cnt, ast_variant);
        BL_ASSERT(variant);
        tsa_push_InstrPtr(variants, variant);
    }
    // Consume declaration identificator.
    ID *id                     = cnt->ast.current_entity_id;
    cnt->ast.current_entity_id = NULL;
    return append_instr_type_enum(cnt, type_enum, id, scope, variants, base_type);
}

MirInstr *ast_type_struct(Context *cnt, Ast *type_struct)
{
    // Consume declaration identificator.
    ID *id                     = cnt->ast.current_entity_id;
    cnt->ast.current_entity_id = NULL;

    // Consume current struct fwd decl.
    MirInstr *fwd_decl               = cnt->ast.current_fwd_struct_decl;
    cnt->ast.current_fwd_struct_decl = NULL;

    TSmallArray_AstPtr *ast_members    = type_struct->data.type_strct.members;
    const bool          is_union       = type_struct->data.type_strct.is_union;
    const bool is_multiple_return_type = type_struct->data.type_strct.is_multiple_return_type;
    BL_ASSERT(ast_members);
    Ast *       ast_base_type = type_struct->data.type_strct.base_type;
    const usize memc          = ast_members->size;
    if (!memc && !ast_base_type) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_EMPTY_STRUCT,
                    type_struct->location,
                    BUILDER_CUR_WORD,
                    "Empty structure.");
        return NULL;
    }

    TSmallArray_InstrPtr *members = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
    Scope *               scope   = type_struct->data.type_strct.scope;
    BL_ASSERT(scope);

    if (ast_base_type) {
        // Structure has base type, in such case we generate implicit first member
        // 'base'.
        MirInstr *base_type = ast(cnt, ast_base_type);
        ID *      id2       = &builtin_ids[MIR_BUILTIN_ID_STRUCT_BASE];
        base_type = append_instr_decl_member_impl(cnt, ast_base_type, id2, base_type, NULL);

        MirMember *base_member = ((MirInstrDeclMember *)base_type)->member;
        base_member->is_base   = true;
        provide_builtin_member(cnt, scope, base_member);

        tsa_push_InstrPtr(members, base_type);
    }

    MirInstr *tmp = NULL;
    Ast *     ast_member;
    TSA_FOREACH(ast_members, ast_member)
    {
        tmp = ast(cnt, ast_member);
        BL_ASSERT(tmp);
        tsa_push_InstrPtr(members, tmp);
    }

    return append_instr_type_struct(
        cnt, type_struct, id, fwd_decl, scope, members, false, is_union, is_multiple_return_type);
}

MirInstr *ast_create_global_initializer2(Context *cnt, Ast *ast_value, TSmallArray_InstrPtr *decls)
{
    MirInstrBlock *prev_block = ast_current_block(cnt);
    MirInstrBlock *block      = append_global_block(cnt, INIT_VALUE_FN_NAME);
    set_current_block(cnt, block);
    MirInstr *result = ast(cnt, ast_value);

    if (ast_value)
        result = append_instr_set_initializer(cnt, ast_value, decls, result);
    else
        result = append_instr_set_initializer_impl(cnt, decls, result);

    set_current_block(cnt, prev_block);
    return result;
}

MirInstr *ast_create_global_initializer(Context *cnt, Ast *ast_value, MirInstr *decl)
{
    BL_ASSERT(decl);
    TSmallArray_InstrPtr *decls = create_sarr(TSmallArray_InstrPtr, cnt->assembly);
    tsa_push_InstrPtr(decls, decl);
    return ast_create_global_initializer2(cnt, ast_value, decls);
}

MirInstr *ast_create_impl_fn_call(Context *   cnt,
                                  Ast *       node,
                                  const char *fn_name,
                                  MirType *   fn_type,
                                  bool        schedule_analyze)
{
    if (!node) return NULL;

    MirInstrBlock *prev_block = ast_current_block(cnt);
    MirInstr *     fn_proto   = append_instr_fn_proto(cnt, NULL, NULL, NULL, schedule_analyze);
    fn_proto->value.type      = fn_type;

    MirFn *fn = create_fn(
        cnt, NULL, NULL, fn_name, 0, (MirInstrFnProto *)fn_proto, false, true, MIR_BUILTIN_ID_NONE);

    MIR_CEV_WRITE_AS(MirFn *, &fn_proto->value, fn);

    fn->type = fn_type;

    MirInstrBlock *entry = append_block(cnt, fn, "entry");
    set_current_block(cnt, entry);

    MirInstr *result = ast(cnt, node);
    append_instr_ret(cnt, NULL, result);

    set_current_block(cnt, prev_block);
    return create_instr_call_comptime(cnt, node, fn_proto);
}

MirInstr *ast(Context *cnt, Ast *node)
{
    if (!node) return NULL;
    switch (node->kind) {
    case AST_UBLOCK:
        ast_ublock(cnt, node);
        break;
    case AST_BLOCK:
        ast_block(cnt, node);
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
    case AST_STMT_SWITCH:
        ast_stmt_switch(cnt, node);
        break;
    case AST_DECL_ENTITY:
        return ast_decl_entity(cnt, node);
    case AST_DECL_ARG:
        return ast_decl_arg(cnt, node);
    case AST_DECL_MEMBER:
        return ast_decl_member(cnt, node);
    case AST_DECL_VARIANT:
        return ast_decl_variant(cnt, node);
    case AST_REF:
        return ast_ref(cnt, node);
    case AST_TYPE_STRUCT:
        return ast_type_struct(cnt, node);
    case AST_TYPE_FN:
        return ast_type_fn(cnt, node);
    case AST_TYPE_FN_GROUP:
        return ast_type_fn_group(cnt, node);
    case AST_TYPE_ARR:
        return ast_type_arr(cnt, node);
    case AST_TYPE_SLICE:
        return ast_type_slice(cnt, node);
    case AST_TYPE_DYNARR:
        return ast_type_dynarr(cnt, node);
    case AST_TYPE_PTR:
        return ast_type_ptr(cnt, node);
    case AST_TYPE_VARGS:
        return ast_type_vargs(cnt, node);
    case AST_TYPE_ENUM:
        return ast_type_enum(cnt, node);
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
        return ast_expr_lit_fn(cnt, node, NULL, NULL, false, 0, MIR_BUILTIN_ID_NONE);
    case AST_EXPR_LIT_FN_GROUP:
        return ast_expr_lit_fn_group(cnt, node);
    case AST_EXPR_LIT_STRING:
        return ast_expr_lit_string(cnt, node);
    case AST_EXPR_LIT_CHAR:
        return ast_expr_lit_char(cnt, node);
    case AST_EXPR_BINOP:
        return ast_expr_binop(cnt, node);
    case AST_EXPR_UNARY:
        return ast_expr_unary(cnt, node);
    case AST_EXPR_CALL:
        return ast_expr_call(cnt, node);
    case AST_EXPR_ELEM:
        return ast_expr_elem(cnt, node);
    case AST_EXPR_NULL:
        return ast_expr_null(cnt, node);
    case AST_EXPR_TYPE:
        return ast_expr_type(cnt, node);
    case AST_EXPR_COMPOUND:
        return ast_expr_compound(cnt, node);
    case AST_EXPR_TYPE_INFO:
        return ast_expr_type_info(cnt, node);
    case AST_EXPR_TEST_CASES:
        return ast_expr_test_cases(cnt, node);
    case AST_CALL_LOC:
        return ast_call_loc(cnt, node);

    case AST_LOAD:
    case AST_IMPORT:
    case AST_LINK:
    case AST_PRIVATE:
    case AST_SCOPE:
        break;
    default:
        BL_ABORT("invalid node %s", ast_get_name(node));
    }

    return NULL;
}

const char *mir_instr_name(const MirInstr *instr)
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
    case MIR_INSTR_FN_GROUP:
        return "InstrFnGroup";
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
    case MIR_INSTR_TYPE_FN_GROUP:
        return "InstrTypeFnGroup";
    case MIR_INSTR_TYPE_STRUCT: {
        const MirInstrTypeStruct *is = (MirInstrTypeStruct *)instr;
        return is->is_union ? "InstrTypeUnion" : "InstrTypeStruct";
    }
    case MIR_INSTR_TYPE_ARRAY:
        return "InstrTypeArray";
    case MIR_INSTR_TYPE_SLICE:
        return "InstrTypeSlice";
    case MIR_INSTR_TYPE_DYNARR:
        return "InstrTypeDynArr";
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
    case MIR_INSTR_SWITCH:
        return "InstrSwitch";
    case MIR_INSTR_SET_INITIALIZER:
        return "InstrSetInitializer";
    case MIR_INSTR_TEST_CASES:
        return "InstrTestCases";
    case MIR_INSTR_CALL_LOC:
        return "InstrCallLoc";
    case MIR_INSTR_UNROLL:
        return "InstrUnroll";
    }

    return "UNKNOWN";
}

// public
static void _type_to_str(char *buf, usize len, const MirType *type, bool prefer_name)
{
//*********************************************************************************************/
#define append_buf(buf, len, str)                                                                  \
    {                                                                                              \
        const usize filled = strlen(buf);                                                          \
        snprintf((buf) + filled, (len)-filled, "%s", str);                                         \
    }
    //*********************************************************************************************/

    if (!buf) return;
    if (!type) {
        append_buf(buf, len, "<unknown>");
        return;
    }

    if (type->user_id && prefer_name) {
        BL_ASSERT(type->user_id->str);
        append_buf(buf, len, type->user_id->str);
        return;
    }

    switch (type->kind) {
    case MIR_TYPE_TYPE:
        append_buf(buf, len, "type");
        break;

    case MIR_TYPE_SLICE: {
        const bool has_members = type->data.strct.members;
        append_buf(buf, len, "[]");

        if (has_members) {
            MirType *tmp = mir_get_struct_elem_type(type, MIR_SLICE_PTR_INDEX);
            tmp          = mir_deref_type(tmp);
            _type_to_str(buf, len, tmp, true);
        }
        break;
    }

    case MIR_TYPE_DYNARR: {
        const bool has_members = type->data.strct.members;
        append_buf(buf, len, "[..]");

        if (has_members) {
            MirType *tmp = mir_get_struct_elem_type(type, MIR_DYNARR_PTR_INDEX);
            tmp          = mir_deref_type(tmp);
            _type_to_str(buf, len, tmp, true);
        }
        break;
    }

    case MIR_TYPE_VARGS: {
        const bool has_members = type->data.strct.members;
        append_buf(buf, len, "...");

        if (has_members) {
            MirType *tmp = mir_get_struct_elem_type(type, MIR_SLICE_PTR_INDEX);
            tmp          = mir_deref_type(tmp);
            _type_to_str(buf, len, tmp, true);
        }
        break;
    }

    case MIR_TYPE_STRUCT: {
        TSmallArray_MemberPtr *members = type->data.strct.members;
        MirMember *            tmp;
        if (type->data.strct.is_union) {
            append_buf(buf, len, "union{");
        } else {
            append_buf(buf, len, "struct{");
        }
        if (members) {
            TSA_FOREACH(members, tmp)
            {
                _type_to_str(buf, len, tmp->type, true);
                if (i < members->size - 1) append_buf(buf, len, ", ");
            }
        }
        append_buf(buf, len, "}");
        break;
    }

    case MIR_TYPE_ENUM: {
        TSmallArray_VariantPtr *variants = type->data.enm.variants;
        append_buf(buf, len, "enum{");
        if (variants) {
            MirVariant *variant;
            TSA_FOREACH(variants, variant)
            {
                append_buf(buf, len, variant->id->str);
                append_buf(buf, len, " :: ");
                if (variant->value) {
                    char value_str[35];
                    snprintf(value_str,
                             TARRAY_SIZE(value_str),
                             "%lld",
                             MIR_CEV_READ_AS(long long, variant->value));
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
        MirArg *            it;
        TSmallArray_ArgPtr *args = type->data.fn.args;
        if (args) {
            TSA_FOREACH(args, it)
            {
                _type_to_str(buf, len, it->type, true);
                if (i < args->size - 1) append_buf(buf, len, ", ");
            }
        }
        append_buf(buf, len, ") ");
        _type_to_str(buf, len, type->data.fn.ret_type, true);
        break;
    }

    case MIR_TYPE_FN_GROUP: {
        append_buf(buf, len, "fn{");
        MirType *            it;
        TSmallArray_TypePtr *variants = type->data.fn_group.variants;
        if (variants) {
            TSA_FOREACH(variants, it)
            {
                _type_to_str(buf, len, it, true);
                if (i < variants->size - 1) append_buf(buf, len, "; ");
            }
        }
        append_buf(buf, len, "} ");
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

#if BL_DEBUG
VMStackPtr _mir_cev_read(MirConstExprValue *value)
{
    BL_ASSERT(value && "Attempt to read null value!");
    BL_ASSERT(value->is_comptime && "Attempt to read non-comptime value!");
    BL_ASSERT(value->data && "Invalid const expression data!");
    return value->data;
}
#endif

void mir_type_to_str(char *buf, usize len, const MirType *type, bool prefer_name)
{
    if (!buf || !len) return;
    buf[0] = '\0';
    _type_to_str(buf, len, type, prefer_name);
}

void builtin_inits(Context *cnt)
{
#define PROVIDE(N) provide_builtin_type(cnt, bt->t_##N) // initialize all hashes once
    for (s32 i = 0; i < _MIR_BUILTIN_ID_COUNT; ++i) {
        builtin_ids[i].hash = thash_from_str(builtin_ids[i].str);
    }

    struct BuiltinTypes *bt = cnt->builtin_types;
    bt->t_type              = create_type_type(cnt);
    bt->t_scope             = create_type_named_scope(cnt);
    bt->t_void              = create_type_void(cnt);

    bt->t_s8     = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_S8], 8, true);
    bt->t_s16    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_S16], 16, true);
    bt->t_s32    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_S32], 32, true);
    bt->t_s64    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_S64], 64, true);
    bt->t_u8     = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_U8], 8, false);
    bt->t_u16    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_U16], 16, false);
    bt->t_u32    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_U32], 32, false);
    bt->t_u64    = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_U64], 64, false);
    bt->t_usize  = create_type_int(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_USIZE], 64, false);
    bt->t_bool   = create_type_bool(cnt);
    bt->t_f32    = create_type_real(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_F32], 32);
    bt->t_f64    = create_type_real(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_F64], 64);
    bt->t_u8_ptr = create_type_ptr(cnt, bt->t_u8);
    bt->t_string =
        CREATE_TYPE_STRUCT_STRING(cnt, &builtin_ids[MIR_BUILTIN_ID_TYPE_STRING], bt->t_u8_ptr);

    bt->t_string_ptr = create_type_ptr(cnt, bt->t_string);

    bt->t_string_slice = CREATE_TYPE_STRUCT_SLICE(cnt, NULL, bt->t_string_ptr);

    bt->t_resolve_type_fn = create_type_fn(cnt, NULL, bt->t_type, NULL, false, false);
    bt->t_test_case_fn    = create_type_fn(cnt, NULL, bt->t_void, NULL, false, false);

    // Provide types into global scope
    PROVIDE(type);
    PROVIDE(s8);
    PROVIDE(s16);
    PROVIDE(s32);
    PROVIDE(s64);
    PROVIDE(u8);
    PROVIDE(u16);
    PROVIDE(u32);
    PROVIDE(u64);
    PROVIDE(usize);
    PROVIDE(bool);
    PROVIDE(f32);
    PROVIDE(f64);
    PROVIDE(string);

    // Add IS_DEBUG immutable into the global scope to provide information about enabled
    // debug mode.
    add_global_bool(cnt, &builtin_ids[MIR_BUILTIN_ID_IS_DEBUG], false, cnt->debug_mode);

    // Add IS_COMPTIME_RUN immutable into the global scope to provide information about compile time
    // run.
    cnt->assembly->vm_run.is_comptime_run =
        add_global_bool(cnt, &builtin_ids[MIR_BUILTIN_ID_IS_COMPTIME_RUN], false, false);
#undef PROVIDE
}

const char *get_intrinsic(const char *name)
{
    if (!name) return NULL;
    if (strcmp(name, "sin.f32") == 0) return "__intrinsic_sin_f32";
    if (strcmp(name, "sin.f64") == 0) return "__intrinsic_sin_f64";
    if (strcmp(name, "cos.f32") == 0) return "__intrinsic_cos_f32";
    if (strcmp(name, "cos.f64") == 0) return "__intrinsic_cos_f64";
    if (strcmp(name, "pow.f32") == 0) return "__intrinsic_pow_f32";
    if (strcmp(name, "pow.f64") == 0) return "__intrinsic_pow_f64";
    if (strcmp(name, "log.f32") == 0) return "__intrinsic_log_f32";
    if (strcmp(name, "log.f64") == 0) return "__intrinsic_log_f64";
    if (strcmp(name, "log2.f32") == 0) return "__intrinsic_log2_f32";
    if (strcmp(name, "log2.f64") == 0) return "__intrinsic_log2_f64";
    if (strcmp(name, "sqrt.f32") == 0) return "__intrinsic_sqrt_f32";
    if (strcmp(name, "sqrt.f64") == 0) return "__intrinsic_sqrt_f64";
    if (strcmp(name, "ceil.f32") == 0) return "__intrinsic_ceil_f32";
    if (strcmp(name, "ceil.f64") == 0) return "__intrinsic_ceil_f64";
    if (strcmp(name, "round.f32") == 0) return "__intrinsic_round_f32";
    if (strcmp(name, "round.f64") == 0) return "__intrinsic_round_f64";
    if (strcmp(name, "floor.f32") == 0) return "__intrinsic_floor_f32";
    if (strcmp(name, "floor.f64") == 0) return "__intrinsic_floor_f64";
    if (strcmp(name, "log10.f32") == 0) return "__intrinsic_log10_f32";
    if (strcmp(name, "log10.f64") == 0) return "__intrinsic_log10_f64";
    return NULL;
}

MirFn *group_select_overload(Context *                  cnt,
                             const MirFnGroup *         group,
                             const TSmallArray_TypePtr *expected_args)
{
    BL_MAGIC_ASSERT(group);
    BL_ASSERT(expected_args);
    const TSmallArray_FnPtr *variants = group->variants;
    BL_ASSERT(variants && variants->size);
    MirFn *selected          = variants->data[0];
    s32    selected_priority = 0;
    MirFn *it_fn;
    TSA_FOREACH(variants, it_fn)
    {
        s32 p = 0;

        const usize argc  = it_fn->type->data.fn.args ? it_fn->type->data.fn.args->size : 0;
        const usize eargc = expected_args->size;
        if (argc == eargc) p += 1;
        const TSmallArray_ArgPtr *args = it_fn->type->data.fn.args;
        if (args) {
            for (usize j = 0; j < args->size && j < expected_args->size; ++j) {
                const MirType *t  = args->data[j]->type;
                const MirType *et = expected_args->data[j];
                if (type_cmp(et, t)) {
                    p += 3;
                    continue;
                }
                if (can_impl_cast(et, t)) {
                    p += 2;
                    continue;
                }
                if (type_cmp(t, cnt->builtin_types->t_Any)) {
                    p += 2;
                    continue;
                }
            }
        }
        // BL_LOG("%s [%d]", it_fn->linkage_name, p);
        if (p > selected_priority) {
            selected          = it_fn;
            selected_priority = p;
        }
    }
    BL_MAGIC_ASSERT(selected);
    return selected;
}

void mir_arenas_init(MirArenas *arenas)
{
    const size_t instr_size = SIZEOF_MIR_INSTR;
    arena_init(&arenas->instr, instr_size, ARENA_INSTR_CHUNK_COUNT, NULL);
    arena_init(&arenas->type, sizeof(MirType), ARENA_CHUNK_COUNT, NULL);
    arena_init(&arenas->var, sizeof(MirVar), ARENA_CHUNK_COUNT, NULL);
    arena_init(&arenas->fn, sizeof(MirFn), ARENA_CHUNK_COUNT, (ArenaElemDtor)&fn_dtor);
    arena_init(&arenas->member, sizeof(MirMember), ARENA_CHUNK_COUNT, NULL);
    arena_init(&arenas->variant, sizeof(MirVariant), ARENA_CHUNK_COUNT, NULL);
    arena_init(&arenas->arg, sizeof(MirArg), ARENA_CHUNK_COUNT / 2, NULL);
    arena_init(&arenas->fn_group, sizeof(MirFnGroup), ARENA_CHUNK_COUNT / 2, NULL);
}

void mir_arenas_terminate(MirArenas *arenas)
{
    arena_terminate(&arenas->fn);
    arena_terminate(&arenas->instr);
    arena_terminate(&arenas->member);
    arena_terminate(&arenas->type);
    arena_terminate(&arenas->var);
    arena_terminate(&arenas->variant);
    arena_terminate(&arenas->arg);
    arena_terminate(&arenas->fn_group);
}

void mir_run(Assembly *assembly)
{
    Context cnt;
    TracyCZone(_tctx, true);
    memset(&cnt, 0, sizeof(Context));
    cnt.assembly      = assembly;
    cnt.debug_mode    = assembly->target->opt == ASSEMBLY_OPT_DEBUG;
    cnt.builtin_types = &assembly->builtin_types;
    cnt.vm            = &assembly->vm;
    cnt.testing.cases = &assembly->testing.cases;

    thtbl_init(&cnt.analyze.waiting, sizeof(TArray), ANALYZE_TABLE_SIZE);
    tlist_init(&cnt.analyze.queue, sizeof(MirInstr *));
    tstring_init(&cnt.tmp_sh);

    tlist_reserve(&cnt.analyze.queue, 1024);

    tarray_init(&cnt.ast._fnctx_stack, sizeof(AstFnContext));
    tarray_init(&cnt.analyze.usage_check_queue, sizeof(ScopeEntry *));
    tsa_init(&cnt.analyze.incomplete_rtti);
    tsa_init(&cnt.analyze.complete_check_type_stack);
    thtbl_init(&cnt.analyze.complete_check_visited, 0, 128);
    thtbl_init(&cnt.analyze.presented_switch_cases, sizeof(MirSwitchCase *), 64);

    cnt.analyze.void_entry =
        scope_create_entry(&cnt.assembly->arenas.scope, SCOPE_ENTRY_VOID, NULL, NULL, true);

    // initialize all builtin types
    builtin_inits(&cnt);

    // Gen MIR from AST pass
    Unit *unit;
    TARRAY_FOREACH(Unit *, &assembly->units, unit) ast(&cnt, unit->ast);

    if (builder.errorc) goto SKIP;

    // Skip analyze if no_analyze is set by user.
    if (assembly->target->no_analyze) goto SKIP;

    // Analyze pass
    analyze(&cnt);
    analyze_report_unresolved(&cnt);

    if (builder.errorc) goto SKIP;
    analyze_report_unused(&cnt);

    BL_LOG("Analyze queue push count: %i", push_count);
SKIP:
    tlist_terminate(&cnt.analyze.queue);
    thtbl_terminate(&cnt.analyze.waiting);
    tstring_terminate(&cnt.tmp_sh);

    tarray_terminate(&cnt.analyze.usage_check_queue);
    tarray_terminate(&cnt.ast._fnctx_stack);
    tsa_terminate(&cnt.analyze.incomplete_rtti);
    tsa_terminate(&cnt.analyze.complete_check_type_stack);
    thtbl_terminate(&cnt.analyze.complete_check_visited);
    thtbl_terminate(&cnt.analyze.presented_switch_cases);
    TracyCZoneEnd(_tctx);
}
