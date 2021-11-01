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
#include "builder.h"
#include "stb_ds.h"
#include <stdarg.h>

#if BL_DEBUG && false
#include "mir_printer.h"
#define DEBUG_PRINT_MIR(instr) mir_print_fn(stdout, ctx->assembly, instr_owner_fn(instr));
#else
#define DEBUG_PRINT_MIR(instr)                                                                     \
    while (0) {                                                                                    \
    }
#endif

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
#define STORE_REPLACE_SIZE_BYTES 16

#define ANALYZE_INSTR_RQ(i)                                                                        \
    {                                                                                              \
        const struct result r = analyze_instr(ctx, (i));                                           \
        bassert(r.state == ANALYZE_PASSED);                                                        \
        (void)r;                                                                                   \
    }

#define ANALYZE_RESULT(_state, _waiting_for)                                                       \
    (struct result)                                                                                \
    {                                                                                              \
        .state = ANALYZE_##_state, .waiting_for = (_waiting_for)                                   \
    }

#define CREATE_TYPE_RESOLVER_CALL(_ast)                                                            \
    ast_create_impl_fn_call(                                                                       \
        ctx, (_ast), RESOLVE_TYPE_FN_NAME, ctx->builtin_types->t_resolve_type_fn, false)

#define TEXT_LINE "--------------------------------------------------------------------------------"

#define GEN_INSTR_SIZEOF
#include "mir.inc"
#undef GEN_INSTR_SIZEOF

#define CREATE_TYPE_STRUCT_SLICE(ctx, id, elem_ptr_type)                                           \
    _create_type_struct_slice(ctx, MIR_TYPE_SLICE, id, elem_ptr_type)

#define CREATE_TYPE_STRUCT_VARGS(ctx, id, elem_ptr_type)                                           \
    _create_type_struct_slice(ctx, MIR_TYPE_VARGS, id, elem_ptr_type)

#define CREATE_TYPE_STRUCT_STRING(ctx, id, elem_ptr_type)                                          \
    _create_type_struct_slice(ctx, MIR_TYPE_STRING, id, elem_ptr_type)

// Sets is_naked flag to 'v' if '_instr' is valid compound expression.
#define SET_IS_NAKED_IF_COMPOUND(_instr, v)                                                        \
    if ((_instr) && (_instr)->kind == MIR_INSTR_COMPOUND) {                                        \
        ((struct mir_instr_compound *)(_instr))->is_naked = v;                                     \
    }

struct rtti_incomplete {
    struct mir_var  *var;
    struct mir_type *type;
};

typedef sarr_t(struct mir_instr *, 32) instrs_t;
typedef sarr_t(LLVMTypeRef, 8) llvm_types_t;
typedef sarr_t(struct rtti_incomplete, 64) rttis_t;
typedef sarr_t(struct ast *, 16) defer_stack_t;

// Instance in run method is zero initialized, no need to set default values explicitly.
struct context {
    struct virtual_machine *vm;
    struct assembly        *assembly;
    bool                    debug_mode;

    // Ast -> MIR generation
    struct {

        struct mir_instr_block *current_block;
        struct mir_instr_block *current_phi_end_block;
        struct mir_instr_phi   *current_phi;
        struct mir_instr_block *break_block;
        struct mir_instr_block *continue_block;
        struct id              *current_entity_id;
        struct mir_instr       *current_fwd_struct_decl;

        defer_stack_t *defer_stack;
        s32            current_defer_stack_index;
    } ast;

    struct {
        mir_types_t replacement_queue;
        s32         current_scope_layer_index;
        bool        is_replacement_active;
    } polymorph;

    // Analyze MIR generated from Ast
    struct {
        // Instructions waiting for analyze.
        struct mir_instr **stack[2];
        s32                si; // Current stack index

        // Hash table of arrays. Hash is id of symbol and array contains queue of waiting
        // instructions (DeclRefs).
        struct {
            hash_t   key;
            instrs_t value;
        } * waiting;

        // Structure members can sometimes point to self, in such case we end up with
        // endless looping RTTI generation, to solve this problem we create dummy RTTI
        // variable for all pointer types and store them in this array. When structure RTTI
        // is complete we can fill missing pointer RTTIs in second generation pass.
        rttis_t incomplete_rtti;

        // Incomplete type check stack.
        mir_types_t          complete_check_type_stack;
        struct scope_entry **usage_check_arr;
        struct scope_entry  *void_entry;
        struct mir_instr    *last_analyzed_instr;
    } analyze;

    struct {
        // Same as assembly->testing.cases.
        struct mir_fn **cases;

        // Expected unit test count is evaluated before analyze pass. We need this
        // information before we analyze all test functions because metadata runtime
        // variable must be preallocated (testcases builtin operator cannot wait for all
        // test case functions to be analyzed). This count must match cases len.
        s32 expected_test_count;
    } testing;

    // Builtins
    struct BuiltinTypes *builtin_types;
};

enum result_state {
    // Analyze pass failed.
    ANALYZE_FAILED = 0,

    // Analyze pass passed.
    ANALYZE_PASSED = 1,

    // Analyze pass cannot be done because some of sub-parts has not been
    // analyzed yet and probably needs to be executed during analyze pass. In
    // such case we push analyzed instruction at the end of analyze queue.
    ANALYZE_POSTPONE = 2,

    // In this case struct result will contain hash of desired symbol which be satisfied later,
    // instruction is pushed into waiting table.
    ANALYZE_WAITING = 3,

    // Completely skip analyze of instruction.
    ANALYZE_SKIP = 4,
};

struct result {
    enum result_state state;
    hash_t            waiting_for;
};

enum stage_state {
    ANALYZE_STAGE_BREAK,
    ANALYZE_STAGE_CONTINUE,
    ANALYZE_STAGE_FAILED,
};

// Argument list used in slot analyze functions
#define ANALYZE_STAGE_ARGS                                                                         \
    struct context UNUSED(*ctx), struct mir_instr UNUSED(**input),                                 \
        struct mir_type UNUSED(*slot_type), bool UNUSED(is_initializer)

#define ANALYZE_STAGE_FN(N) enum stage_state analyze_stage_##N(ANALYZE_STAGE_ARGS)
typedef enum stage_state (*AnalyzeStageFn)(ANALYZE_STAGE_ARGS);

struct slot_config {
    s32            count;
    AnalyzeStageFn stages[];
};

// Arena destructor for functions.
static void fn_dtor(struct mir_fn *fn)
{
    BL_MAGIC_ASSERT(fn);
    if (fn->dyncall.extern_callback_handle) dcbFreeCallback(fn->dyncall.extern_callback_handle);
    arrfree(fn->variables);
}

static void fn_poly_dtor(struct mir_fn_poly_recipe *recipe)
{
    BL_MAGIC_ASSERT(recipe);
    hmfree(recipe->entries);
}

// FW decls
static void            report_poly(struct mir_instr *instr);
static void            initialize_builtins(struct context *ctx);
static void            testing_add_test_case(struct context *ctx, struct mir_fn *fn);
static struct mir_var *testing_gen_meta(struct context *ctx);

// Execute all registered test cases in current assembly.
static const char    *get_intrinsic(const char *name);
static struct mir_fn *group_select_overload(struct context            *ctx,
                                            const struct mir_fn_group *group,
                                            const mir_types_t         *expected_args);

// Register incomplete scope entry for symbol.
static struct scope_entry *register_symbol(struct context *ctx,
                                           struct ast     *node,
                                           struct id      *id,
                                           struct scope   *scope,
                                           bool            is_builtin);

// Lookup builtin by builtin kind in global scope. Return NULL even if builtin is valid symbol in
// case when it's not been analyzed yet or is incomplete struct type. In such case caller must
// postpone analyze process. This is an error in any post-analyze processing (every type must be
// complete when analyze pass id completed!).
static struct mir_type *lookup_builtin_type(struct context *ctx, enum builtin_id_kind kind);
static struct mir_fn   *lookup_builtin_fn(struct context *ctx, enum builtin_id_kind kind);

// @HACK: Better way to do this will be enable compiler to have default preload file; we need to
//  make lexing, parsing, MIR generation and analyze of this file first and then process rest of the
//  source base. Then it will be guaranteed that all desired builtins are ready to use.

// Try to complete cached RTTI related types, return NULL if all types are resolved or return ID for
// first missing type.
static struct id *lookup_builtins_rtti(struct context *ctx);
static struct id *lookup_builtins_any(struct context *ctx);
static struct id *lookup_builtins_test_cases(struct context *ctx);
static struct id *lookup_builtins_code_loc(struct context *ctx);

// Lookup member in composit structure type. Searching also in base types. When 'out_base_type' is
// set to base member type if entry was found in parent.
static struct scope_entry *
lookup_composit_member(struct mir_type *type, struct id *rid, struct mir_type **out_base_type);

static struct mir_var *add_global_variable(struct context   *ctx,
                                           struct id        *id,
                                           bool              is_mutable,
                                           struct mir_instr *initializer);
static struct mir_var *add_global_bool(struct context *ctx, struct id *id, bool is_mutable, bool v);
static struct mir_var *
add_global_int(struct context *ctx, struct id *id, bool is_mutable, struct mir_type *type, s32 v);
static void type_init_id(struct context *ctx, struct mir_type *type);

// Create new type. The 'user_id' is optional.
static struct mir_type *
create_type(struct context *ctx, enum mir_type_kind kind, struct id *user_id);
static struct mir_type *create_type_type(struct context *ctx);
static struct mir_type *create_type_named_scope(struct context *ctx);
static struct mir_type *create_type_null(struct context *ctx, struct mir_type *base_type);
static struct mir_type *create_type_void(struct context *ctx);
static struct mir_type *create_type_bool(struct context *ctx);
static struct mir_type *create_type_poly(struct context *ctx, struct id *user_id, bool is_master);
static struct mir_type *
create_type_int(struct context *ctx, struct id *id, s32 bitcount, bool is_signed);
static struct mir_type *create_type_real(struct context *ctx, struct id *id, s32 bitcount);
static struct mir_type *create_type_ptr(struct context *ctx, struct mir_type *src_type);
static struct mir_type *create_type_fn(struct context  *ctx,
                                       struct id       *id,
                                       struct mir_type *ret_type,
                                       mir_args_t      *args,
                                       bool             is_vargs,
                                       bool             has_default_args,
                                       bool             is_polymorph);
static struct mir_type *
create_type_fn_group(struct context *ctx, struct id *id, mir_types_t *variants);
static struct mir_type *
create_type_array(struct context *ctx, struct id *id, struct mir_type *elem_type, s64 len);
static struct mir_type *create_type_struct(struct context    *ctx,
                                           enum mir_type_kind kind,
                                           struct id         *id,
                                           struct scope      *scope,
                                           mir_members_t     *members,   // struct mir_member
                                           struct mir_type   *base_type, // optional
                                           bool               is_union,
                                           bool               is_packed,
                                           bool               is_multiple_return_type);

// Make incomplete type struct declaration complete. This function sets all desired information
// about struct to the forward declaration type.
static struct mir_type *complete_type_struct(struct context   *ctx,
                                             struct mir_instr *fwd_decl,
                                             struct scope     *scope,
                                             mir_members_t    *members,
                                             struct mir_type  *base_type, // optional
                                             bool              is_packed,
                                             bool              is_union,
                                             bool              is_multiple_return_type);

// Create incomplete struct type placeholder to be filled later.
static struct mir_type *
create_type_struct_incomplete(struct context *ctx, struct id *user_id, bool is_union);
static struct mir_type *create_type_enum(struct context  *ctx,
                                         struct id       *id,
                                         struct scope    *scope,
                                         struct mir_type *base_type,
                                         mir_variants_t  *variants,
                                         const bool       is_flags);

struct mir_type *_create_type_struct_slice(struct context    *ctx,
                                           enum mir_type_kind kind,
                                           struct id         *id,
                                           struct mir_type   *elem_ptr_type);
static void      type_init_llvm_int(struct context *ctx, struct mir_type *type);
static void      type_init_llvm_real(struct context *ctx, struct mir_type *type);
static void      type_init_llvm_ptr(struct context *ctx, struct mir_type *type);
static void      type_init_llvm_null(struct context *ctx, struct mir_type *type);
static void      type_init_llvm_void(struct context *ctx, struct mir_type *type);
static void      type_init_llvm_bool(struct context *ctx, struct mir_type *type);
static void      type_init_llvm_fn(struct context *ctx, struct mir_type *type);
static void      type_init_llvm_array(struct context *ctx, struct mir_type *type);
static void      type_init_llvm_struct(struct context *ctx, struct mir_type *type);
static void      type_init_llvm_enum(struct context *ctx, struct mir_type *type);
static void      type_init_llvm_dummy(struct context *ctx, struct mir_type *type);

static struct mir_var *create_var(struct context      *ctx,
                                  struct ast          *decl_node,
                                  struct scope        *scope,
                                  struct id           *id,
                                  struct mir_type     *alloc_type,
                                  bool                 is_mutable,
                                  bool                 is_global,
                                  bool                 is_comptime,
                                  u32                  flags,
                                  enum builtin_id_kind builtin_id);

static struct mir_var *create_var_impl(struct context  *ctx,
                                       struct ast      *decl_node, // Optional
                                       const char      *name,
                                       struct mir_type *alloc_type,
                                       bool             is_mutable,
                                       bool             is_global,
                                       bool             is_comptime);

static struct mir_fn *create_fn(struct context            *ctx,
                                struct ast                *node,
                                struct id                 *id,
                                const char                *linkage_name,
                                u32                        flags,
                                struct mir_instr_fn_proto *prototype,
                                bool                       is_global,
                                enum builtin_id_kind       builtin_id);

static struct mir_fn_group *
create_fn_group(struct context *ctx, struct ast *decl_node, mir_fns_t *variants);
static struct mir_fn_poly_recipe *create_fn_poly_recipe(struct context *ctx,
                                                        struct ast     *ast_lit_fn);
static struct mir_member         *create_member(struct context  *ctx,
                                                struct ast      *node,
                                                struct id       *id,
                                                s64              index,
                                                struct mir_type *type);
static struct mir_arg            *create_arg(struct context   *ctx,
                                             struct ast       *node,
                                             struct id        *id,
                                             struct scope     *scope,
                                             struct mir_type  *type,
                                             struct mir_instr *value);
static struct mir_variant *
create_variant(struct context *ctx, struct id *id, struct mir_type *value_type, const u64 value);
// Create block without owner function.
static struct mir_instr_block *create_block(struct context *ctx, const char *name);
// Create and append block into function specified.
static struct mir_instr_block *
append_block(struct context *ctx, struct mir_fn *fn, const char *name);
// Append already created block into function. Block cannot be already member of other function.
static struct mir_instr_block *
append_block2(struct context *ctx, struct mir_fn *fn, struct mir_instr_block *block);
static struct mir_instr_block *append_global_block(struct context *ctx, const char *name);

// instructions
static void *create_instr(struct context *ctx, enum mir_instr_kind kind, struct ast *node);
static struct mir_instr *
create_instr_const_type(struct context *ctx, struct ast *node, struct mir_type *type);
static struct mir_instr *
create_instr_const_int(struct context *ctx, struct ast *node, struct mir_type *type, u64 val);
static struct mir_instr *create_instr_const_ptr(struct context  *ctx,
                                                struct ast      *node,
                                                struct mir_type *type,
                                                vm_stack_ptr_t   ptr);
static struct mir_instr *create_instr_const_float(struct context *ctx, struct ast *node, float val);
static struct mir_instr *
create_instr_const_double(struct context *ctx, struct ast *node, double val);
static struct mir_instr *create_instr_const_bool(struct context *ctx, struct ast *node, bool val);
static struct mir_instr *
create_instr_addrof(struct context *ctx, struct ast *node, struct mir_instr *src);
static struct mir_instr *create_instr_vargs_impl(struct context  *ctx,
                                                 struct ast      *node,
                                                 struct mir_type *type,
                                                 mir_instrs_t    *values);
static struct mir_instr *
create_instr_decl_direct_ref(struct context *ctx, struct ast *node, struct mir_instr *ref);
static struct mir_instr *
create_instr_call_comptime(struct context *ctx, struct ast *node, struct mir_instr *fn);
static struct mir_instr *
create_instr_call_loc(struct context *ctx, struct ast *node, struct location *call_location);
static struct mir_instr *create_instr_compound(struct context   *ctx,
                                               struct ast       *node,
                                               struct mir_instr *type,
                                               mir_instrs_t     *values,
                                               bool              is_multiple_return_value);

static struct mir_instr *create_instr_compound_impl(struct context  *ctx,
                                                    struct ast      *node,
                                                    struct mir_type *type,
                                                    mir_instrs_t    *values);

static struct mir_instr *create_default_value_for_type(struct context *ctx, struct mir_type *type);
static struct mir_instr *create_instr_elem_ptr(struct context   *ctx,
                                               struct ast       *node,
                                               struct mir_instr *arr_ptr,
                                               struct mir_instr *index);
static struct mir_instr *
create_instr_type_info(struct context *ctx, struct ast *node, struct mir_instr *expr);
static struct mir_instr *create_instr_member_ptr(struct context      *ctx,
                                                 struct ast          *node,
                                                 struct mir_instr    *target_ptr,
                                                 struct ast          *member_ident,
                                                 struct scope_entry  *scope_entry,
                                                 enum builtin_id_kind builtin_id);
static struct mir_instr *create_instr_phi(struct context *ctx, struct ast *node);

static struct mir_instr *insert_instr_load(struct context *ctx, struct mir_instr *src);
static struct mir_instr *
insert_instr_cast(struct context *ctx, struct mir_instr *src, struct mir_type *to_type);
static struct mir_instr *insert_instr_addrof(struct context *ctx, struct mir_instr *src);
static struct mir_instr *insert_instr_toany(struct context *ctx, struct mir_instr *expr);
static enum mir_cast_op  get_cast_op(struct mir_type *from, struct mir_type *to);
static void              append_current_block(struct context *ctx, struct mir_instr *instr);
static struct mir_instr *append_instr_arg(struct context *ctx, struct ast *node, unsigned i);
static struct mir_instr *append_instr_unroll(struct context   *ctx,
                                             struct ast       *node,
                                             struct mir_instr *src,
                                             struct mir_instr *remove_src,
                                             s32               index);
static struct mir_instr *append_instr_set_initializer(struct context   *ctx,
                                                      struct ast       *node,
                                                      mir_instrs_t     *dests,
                                                      struct mir_instr *src);

static struct mir_instr *
append_instr_set_initializer_impl(struct context *ctx, mir_instrs_t *dests, struct mir_instr *src);
static struct mir_instr *append_instr_compound(struct context   *ctx,
                                               struct ast       *node,
                                               struct mir_instr *type,
                                               mir_instrs_t     *values,
                                               bool              is_multiple_return_value);

static struct mir_instr *append_instr_compound_impl(struct context  *ctx,
                                                    struct ast      *node,
                                                    struct mir_type *type,
                                                    mir_instrs_t    *values);

static struct mir_instr *append_instr_cast(struct context   *ctx,
                                           struct ast       *node,
                                           struct mir_instr *type,
                                           struct mir_instr *next);
static struct mir_instr *
append_instr_sizeof(struct context *ctx, struct ast *node, struct mir_instr *expr);
static struct mir_instr *
append_instr_type_info(struct context *ctx, struct ast *node, struct mir_instr *expr);
static struct mir_instr *append_instr_test_cases(struct context *ctx, struct ast *node);
static struct mir_instr *
append_instr_alignof(struct context *ctx, struct ast *node, struct mir_instr *expr);
static struct mir_instr *append_instr_elem_ptr(struct context   *ctx,
                                               struct ast       *node,
                                               struct mir_instr *arr_ptr,
                                               struct mir_instr *index);

static struct mir_instr *append_instr_member_ptr(struct context      *ctx,
                                                 struct ast          *node,
                                                 struct mir_instr    *target_ptr,
                                                 struct ast          *member_ident,
                                                 struct scope_entry  *scope_entry,
                                                 enum builtin_id_kind builtin_id);

static struct mir_instr *append_instr_cond_br(struct context         *ctx,
                                              struct ast             *node,
                                              struct mir_instr       *cond,
                                              struct mir_instr_block *then_block,
                                              struct mir_instr_block *else_block,
                                              const bool              is_static);

static struct mir_instr *
append_instr_br(struct context *ctx, struct ast *node, struct mir_instr_block *then_block);
static struct mir_instr *append_instr_switch(struct context         *ctx,
                                             struct ast             *node,
                                             struct mir_instr       *value,
                                             struct mir_instr_block *default_block,
                                             bool                    user_defined_default,
                                             mir_switch_cases_t     *cases);

static struct mir_instr *
append_instr_load(struct context *ctx, struct ast *node, struct mir_instr *src);
static struct mir_instr *append_instr_type_fn(struct context   *ctx,
                                              struct ast       *node,
                                              struct mir_instr *ret_type,
                                              mir_instrs_t     *args,
                                              bool              is_polymorph);

static struct mir_instr *append_instr_type_fn_group(struct context *ctx,
                                                    struct ast     *node,
                                                    struct id      *id,
                                                    mir_instrs_t   *variants);

static struct mir_instr *append_instr_type_struct(struct context   *ctx,
                                                  struct ast       *node,
                                                  struct id        *id,
                                                  struct mir_instr *fwd_decl, // Optional
                                                  struct scope     *scope,
                                                  mir_instrs_t     *members,
                                                  bool              is_packed,
                                                  bool              is_union,
                                                  bool              is_multiple_return_type);

static struct mir_instr *append_instr_type_enum(struct context   *ctx,
                                                struct ast       *node,
                                                struct id        *id,
                                                struct scope     *scope,
                                                mir_instrs_t     *variants,
                                                struct mir_instr *base_type,
                                                const bool        is_flags);

static struct mir_instr *
append_instr_type_ptr(struct context *ctx, struct ast *node, struct mir_instr *type);
static struct mir_instr *
append_instr_type_poly(struct context *ctx, struct ast *node, struct id *T_id);
static struct mir_instr *append_instr_type_array(struct context   *ctx,
                                                 struct ast       *node,
                                                 struct id        *id,
                                                 struct mir_instr *elem_type,
                                                 struct mir_instr *len);

static struct mir_instr *
append_instr_type_slice(struct context *ctx, struct ast *node, struct mir_instr *elem_type);
static struct mir_instr *
append_instr_type_dynarr(struct context *ctx, struct ast *node, struct mir_instr *elem_type);
static struct mir_instr *
append_instr_type_vargs(struct context *ctx, struct ast *node, struct mir_instr *elem_type);
static struct mir_instr *append_instr_fn_proto(struct context   *ctx,
                                               struct ast       *node,
                                               struct mir_instr *type,
                                               struct mir_instr *user_type,
                                               bool              schedule_analyze);
static struct mir_instr *
append_instr_fn_group(struct context *ctx, struct ast *node, mir_instrs_t *variants);
static struct mir_instr *append_instr_decl_ref(struct context     *ctx,
                                               struct ast         *node,
                                               struct unit        *parent_unit,
                                               struct id          *rid,
                                               struct scope       *scope,
                                               s32                 scope_layer,
                                               struct scope_entry *scope_entry);

static struct mir_instr *
append_instr_decl_direct_ref(struct context *ctx, struct ast *node, struct mir_instr *ref);

static struct mir_instr *append_instr_call(struct context   *ctx,
                                           struct ast       *node,
                                           struct mir_instr *callee,
                                           mir_instrs_t     *args,
                                           const bool        call_in_compile_time);

static struct mir_instr *append_instr_decl_var(struct context      *ctx,
                                               struct ast          *node, // Optional
                                               struct id           *id,
                                               struct scope        *scope,
                                               struct mir_instr    *type,
                                               struct mir_instr    *init,
                                               bool                 is_mutable,
                                               u32                  flags,
                                               enum builtin_id_kind builtin_id);

static struct mir_instr *create_instr_decl_var_impl(struct context   *ctx,
                                                    struct ast       *node, // Optional
                                                    const char       *name,
                                                    struct mir_instr *type,
                                                    struct mir_instr *init,
                                                    bool              is_mutable,
                                                    bool              is_global);

static struct mir_instr *append_instr_decl_var_impl(struct context   *ctx,
                                                    struct ast       *node, // Optional
                                                    const char       *name,
                                                    struct mir_instr *type,
                                                    struct mir_instr *init,
                                                    bool              is_mutable,
                                                    bool              is_global);

static struct mir_instr *append_instr_decl_member(struct context   *ctx,
                                                  struct ast       *node,
                                                  struct mir_instr *type,
                                                  mir_instrs_t     *tags);

static struct mir_instr *append_instr_decl_member_impl(struct context   *ctx,
                                                       struct ast       *node,
                                                       struct id        *id,
                                                       struct mir_instr *type,
                                                       mir_instrs_t     *tags);

static struct mir_instr *append_instr_decl_arg(struct context   *ctx,
                                               struct ast       *node,
                                               struct mir_instr *type,
                                               struct mir_instr *value);
static struct mir_instr *append_instr_decl_variant(struct context     *ctx,
                                                   struct ast         *node,
                                                   struct mir_instr   *value,
                                                   struct mir_instr   *base_type,
                                                   struct mir_variant *prev_variant,
                                                   const bool          is_flags);
static struct mir_instr *
append_instr_const_type(struct context *ctx, struct ast *node, struct mir_type *type);
static struct mir_instr *
append_instr_const_int(struct context *ctx, struct ast *node, struct mir_type *type, u64 val);
static struct mir_instr *append_instr_const_float(struct context *ctx, struct ast *node, float val);
static struct mir_instr *
append_instr_const_double(struct context *ctx, struct ast *node, double val);
static struct mir_instr *append_instr_const_bool(struct context *ctx, struct ast *node, bool val);
static struct mir_instr *
append_instr_const_string(struct context *ctx, struct ast *node, const char *str);
static struct mir_instr *append_instr_const_char(struct context *ctx, struct ast *node, char c);
static struct mir_instr *append_instr_const_null(struct context *ctx, struct ast *node);
static struct mir_instr *append_instr_const_void(struct context *ctx, struct ast *node);
static struct mir_instr *
append_instr_ret(struct context *ctx, struct ast *node, struct mir_instr *value);
static struct mir_instr *append_instr_store(struct context   *ctx,
                                            struct ast       *node,
                                            struct mir_instr *src,
                                            struct mir_instr *dest);
static struct mir_instr *append_instr_binop(struct context   *ctx,
                                            struct ast       *node,
                                            struct mir_instr *lhs,
                                            struct mir_instr *rhs,
                                            enum binop_kind   op);

static struct mir_instr *append_instr_unop(struct context   *ctx,
                                           struct ast       *node,
                                           struct mir_instr *instr,
                                           enum unop_kind    op);
static struct mir_instr *append_instr_unreachable(struct context *ctx, struct ast *node);
static struct mir_instr *append_instr_debugbreak(struct context *ctx, struct ast *node);
static struct mir_instr *
append_instr_addrof(struct context *ctx, struct ast *node, struct mir_instr *src);

// This will erase whole instruction tree of instruction with ref_count == 0. When force is set
// ref_count is ignored.
static void              erase_instr_tree(struct mir_instr *instr, bool keep_root, bool force);
static struct mir_instr *append_instr_call_loc(struct context *ctx, struct ast *node);
static struct mir_instr *append_instr_msg(struct context *ctx, struct ast *node);

// struct ast
static struct mir_instr *
ast_create_global_initializer2(struct context *ctx, struct ast *ast_value, mir_instrs_t *decls);
static struct mir_instr *
ast_create_global_initializer(struct context *ctx, struct ast *node, struct mir_instr *decls);
static struct mir_instr *ast_create_impl_fn_call(struct context  *ctx,
                                                 struct ast      *node,
                                                 const char      *fn_name,
                                                 struct mir_type *fn_type,
                                                 bool             schedule_analyze);

static void              ast_push_defer_stack(struct context *ctx);
static void              ast_pop_defer_stack(struct context *ctx);
static void              ast_free_defer_stack(struct context *ctx);
static struct mir_instr *ast(struct context *ctx, struct ast *node);
static void              ast_ublock(struct context *ctx, struct ast *ublock);
static void              ast_unreachable(struct context *ctx, struct ast *unr);
static void              ast_debugbreak(struct context *ctx, struct ast *debug_break);
static void              ast_defer_block(struct context *ctx, struct ast *block, bool whole_tree);
static void              ast_block(struct context *ctx, struct ast *block);
static void              ast_stmt_if(struct context *ctx, struct ast *stmt_if);
static void              ast_stmt_return(struct context *ctx, struct ast *ret);
static void              ast_stmt_defer(struct context *ctx, struct ast *defer);
static void              ast_stmt_loop(struct context *ctx, struct ast *loop);
static void              ast_stmt_break(struct context *ctx, struct ast *br);
static void              ast_stmt_continue(struct context *ctx, struct ast *cont);
static void              ast_stmt_switch(struct context *ctx, struct ast *stmt_switch);
static struct mir_instr *ast_decl_entity(struct context *ctx, struct ast *entity);
static struct mir_instr *ast_decl_arg(struct context *ctx, struct ast *arg);
static struct mir_instr *ast_decl_member(struct context *ctx, struct ast *arg);
static struct mir_instr *ast_decl_variant(struct context     *ctx,
                                          struct ast         *variant,
                                          struct mir_instr   *base_type,
                                          struct mir_variant *prev_variant,
                                          const bool          is_flags);
static struct mir_instr *ast_ref(struct context *ctx, struct ast *ref);
static struct mir_instr *ast_type_struct(struct context *ctx, struct ast *type_struct);
static struct mir_instr *ast_type_fn(struct context *ctx, struct ast *type_fn);
static struct mir_instr *ast_type_fn_group(struct context *ctx, struct ast *group);
static struct mir_instr *ast_type_arr(struct context *ctx, struct ast *type_arr);
static struct mir_instr *ast_type_slice(struct context *ctx, struct ast *type_slice);
static struct mir_instr *ast_type_dynarr(struct context *ctx, struct ast *type_dynarr);
static struct mir_instr *ast_type_ptr(struct context *ctx, struct ast *type_ptr);
static struct mir_instr *ast_type_vargs(struct context *ctx, struct ast *type_vargs);
static struct mir_instr *ast_type_enum(struct context *ctx, struct ast *type_enum);
static struct mir_instr *ast_type_poly(struct context *ctx, struct ast *type_poly);
static struct mir_instr *ast_expr_addrof(struct context *ctx, struct ast *addrof);
static struct mir_instr *ast_expr_cast(struct context *ctx, struct ast *cast);
static struct mir_instr *ast_expr_sizeof(struct context *ctx, struct ast *szof);
static struct mir_instr *ast_expr_type_info(struct context *ctx, struct ast *type_info);
static struct mir_instr *ast_expr_test_cases(struct context *ctx, struct ast *tc);
static struct mir_instr *ast_expr_alignof(struct context *ctx, struct ast *szof);
static struct mir_instr *ast_expr_type(struct context *ctx, struct ast *type);
static struct mir_instr *ast_expr_deref(struct context *ctx, struct ast *deref);
static struct mir_instr *ast_expr_call(struct context *ctx, struct ast *call);
static struct mir_instr *ast_expr_elem(struct context *ctx, struct ast *elem);
static struct mir_instr *ast_expr_null(struct context *ctx, struct ast *nl);
static struct mir_instr *ast_expr_lit_int(struct context *ctx, struct ast *expr);
static struct mir_instr *ast_expr_lit_float(struct context *ctx, struct ast *expr);
static struct mir_instr *ast_expr_lit_double(struct context *ctx, struct ast *expr);
static struct mir_instr *ast_expr_lit_bool(struct context *ctx, struct ast *expr);
static struct mir_instr *ast_expr_lit_fn(struct context      *ctx,
                                         struct ast          *lit_fn,
                                         struct ast          *decl_node,
                                         const char          *explicit_linkage_name, // optional
                                         bool                 is_global,
                                         u32                  flags,
                                         enum builtin_id_kind builtin_id);
static struct mir_instr *ast_expr_lit_fn_group(struct context *ctx, struct ast *group);
static struct mir_instr *ast_expr_lit_string(struct context *ctx, struct ast *lit_string);
static struct mir_instr *ast_expr_lit_char(struct context *ctx, struct ast *lit_char);
static struct mir_instr *ast_expr_binop(struct context *ctx, struct ast *binop);
static struct mir_instr *ast_expr_unary(struct context *ctx, struct ast *unop);
static struct mir_instr *ast_expr_compound(struct context *ctx, struct ast *cmp);
static struct mir_instr *ast_call_loc(struct context *ctx, struct ast *loc);
static struct mir_instr *ast_msg(struct context *ctx, struct ast *msg);

// analyze
static bool                      evaluate(struct context *ctx, struct mir_instr *instr);
static struct result             analyze_var(struct context *ctx, struct mir_var *var);
FORCEINLINE static struct result analyze_instr(struct context *ctx, struct mir_instr *instr);

#define analyze_slot(ctx, conf, input, slot_type)                                                  \
    _analyze_slot((ctx), (conf), (input), (slot_type), false)

#define analyze_slot_initializer(ctx, conf, input, slot_type)                                      \
    _analyze_slot((ctx), (conf), (input), (slot_type), true)

static enum result_state _analyze_slot(struct context           *ctx,
                                       const struct slot_config *conf,
                                       struct mir_instr        **input,
                                       struct mir_type          *slot_type,
                                       bool                      is_initilizer);

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

static const struct slot_config analyze_slot_conf_dummy = {.count = 0};

static const struct slot_config analyze_slot_conf_basic = {
    .count  = 2,
    .stages = {analyze_stage_unroll, analyze_stage_load}};

static const struct slot_config analyze_slot_conf_default = {.count  = 8,
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

static const struct slot_config analyze_slot_conf_full = {.count  = 9,
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
static struct result analyze_resolve_type(struct context   *ctx,
                                          struct mir_instr *resolver_call,
                                          struct mir_type **out_type);
static struct result analyze_instr_unroll(struct context *ctx, struct mir_instr_unroll *unroll);
static struct result analyze_instr_compound(struct context *ctx, struct mir_instr_compound *cmp);
static struct result analyze_instr_set_initializer(struct context                   *ctx,
                                                   struct mir_instr_set_initializer *si);
static struct result analyze_instr_phi(struct context *ctx, struct mir_instr_phi *phi);
static struct result analyze_instr_toany(struct context *ctx, struct mir_instr_to_any *toany);
static struct result analyze_instr_vargs(struct context *ctx, struct mir_instr_vargs *vargs);
static struct result analyze_instr_elem_ptr(struct context            *ctx,
                                            struct mir_instr_elem_ptr *elem_ptr);
static struct result analyze_instr_member_ptr(struct context              *ctx,
                                              struct mir_instr_member_ptr *member_ptr);
static struct result analyze_instr_addrof(struct context *ctx, struct mir_instr_addrof *addrof);
static struct result analyze_instr_block(struct context *ctx, struct mir_instr_block *block);
static struct result analyze_instr_ret(struct context *ctx, struct mir_instr_ret *ret);
static struct result analyze_instr_arg(struct context *ctx, struct mir_instr_arg *arg);
static struct result analyze_instr_unop(struct context *ctx, struct mir_instr_unop *unop);
static struct result analyze_instr_test_cases(struct context *ctx, struct mir_instr_test_case *tc);
static struct result analyze_instr_unreachable(struct context               *ctx,
                                               struct mir_instr_unreachable *unr);
static struct result analyze_instr_debugbreak(struct context              *ctx,
                                              struct mir_instr_debugbreak *debug_break);
static struct result analyze_instr_cond_br(struct context *ctx, struct mir_instr_cond_br *br);
static struct result analyze_instr_br(struct context *ctx, struct mir_instr_br *br);
static struct result analyze_instr_switch(struct context *ctx, struct mir_instr_switch *sw);
static struct result analyze_instr_load(struct context *ctx, struct mir_instr_load *load);
static struct result analyze_instr_store(struct context *ctx, struct mir_instr_store *store);
static struct result analyze_instr_fn_proto(struct context            *ctx,
                                            struct mir_instr_fn_proto *fn_proto);
static struct result analyze_instr_fn_group(struct context *ctx, struct mir_instr_fn_group *group);
static struct result analyze_instr_type_fn(struct context *ctx, struct mir_instr_type_fn *type_fn);
static struct result analyze_instr_type_fn_group(struct context                 *ctx,
                                                 struct mir_instr_type_fn_group *group);
static struct result analyze_instr_type_struct(struct context               *ctx,
                                               struct mir_instr_type_struct *type_struct);
static struct result analyze_instr_type_slice(struct context              *ctx,
                                              struct mir_instr_type_slice *type_slice);
static struct result analyze_instr_type_dynarr(struct context                *ctx,
                                               struct mir_instr_type_dyn_arr *type_dynarr);
static struct result analyze_instr_type_vargs(struct context              *ctx,
                                              struct mir_instr_type_vargs *type_vargs);
static struct result analyze_instr_type_ptr(struct context            *ctx,
                                            struct mir_instr_type_ptr *type_ptr);
static struct result analyze_instr_type_array(struct context              *ctx,
                                              struct mir_instr_type_array *type_arr);
static struct result analyze_instr_type_enum(struct context             *ctx,
                                             struct mir_instr_type_enum *type_enum);
static struct result analyze_instr_type_poly(struct context             *ctx,
                                             struct mir_instr_type_poly *type_poly);
static struct result analyze_instr_decl_var(struct context *ctx, struct mir_instr_decl_var *decl);
static struct result analyze_instr_decl_member(struct context               *ctx,
                                               struct mir_instr_decl_member *decl);
static struct result analyze_instr_decl_variant(struct context                *ctx,
                                                struct mir_instr_decl_variant *variant_instr);
static struct result analyze_instr_decl_arg(struct context *ctx, struct mir_instr_decl_arg *decl);
static struct result analyze_instr_decl_ref(struct context *ctx, struct mir_instr_decl_ref *ref);
static struct result analyze_instr_decl_direct_ref(struct context                   *ctx,
                                                   struct mir_instr_decl_direct_ref *ref);
static struct result analyze_instr_const(struct context *ctx, struct mir_instr_const *cnst);
static struct result analyze_instr_call(struct context *ctx, struct mir_instr_call *call);
static struct result
analyze_instr_cast(struct context *ctx, struct mir_instr_cast *cast, bool analyze_op_only);
static struct result analyze_instr_sizeof(struct context *ctx, struct mir_instr_sizeof *szof);
static struct result analyze_instr_type_info(struct context             *ctx,
                                             struct mir_instr_type_info *type_info);
static struct result analyze_instr_alignof(struct context *ctx, struct mir_instr_alignof *alof);
static struct result analyze_instr_binop(struct context *ctx, struct mir_instr_binop *binop);
static struct result analyze_instr_call_loc(struct context *ctx, struct mir_instr_call_loc *loc);
static struct result analyze_instr_msg(struct context *ctx, struct mir_instr_msg *msg);
static void          analyze_report_unresolved(struct context *ctx);
static void          analyze_report_unused(struct context *ctx);

// Find or generate implementation of polymorph function template. Function will try to find already
// generated function based on expected argument list or create new one.
static struct result generate_fn_poly(struct context             *ctx,
                                      struct ast                 *call,
                                      struct mir_fn              *fn,
                                      mir_instrs_t               *expected_args,
                                      struct mir_instr_fn_proto **out_fn_proto);

//***********/
//*  RTTI   */
//***********/
static struct mir_var *_rtti_gen(struct context *ctx, struct mir_type *type);
static struct mir_var *rtti_gen(struct context *ctx, struct mir_type *type);
static struct mir_var *rtti_create_and_alloc_var(struct context *ctx, struct mir_type *type);
static void rtti_satisfy_incomplete(struct context *ctx, struct rtti_incomplete *incomplete);
static struct mir_var *rtti_gen_integer(struct context *ctx, struct mir_type *type);
static struct mir_var *rtti_gen_real(struct context *ctx, struct mir_type *type);
static struct mir_var *
rtti_gen_ptr(struct context *ctx, struct mir_type *type, struct mir_var *incomplete);
static struct mir_var *rtti_gen_array(struct context *ctx, struct mir_type *type);
static struct mir_var *
rtti_gen_empty(struct context *ctx, struct mir_type *type, struct mir_type *rtti_type);
static struct mir_var *rtti_gen_enum(struct context *ctx, struct mir_type *type);
static void
rtti_gen_enum_variant(struct context *ctx, vm_stack_ptr_t dest, struct mir_variant *variant);
static vm_stack_ptr_t rtti_gen_enum_variants_array(struct context *ctx, mir_variants_t *variants);
static void
rtti_gen_enum_variants_slice(struct context *ctx, vm_stack_ptr_t dest, mir_variants_t *variants);

static void
rtti_gen_struct_member(struct context *ctx, vm_stack_ptr_t dest, struct mir_member *member);
static vm_stack_ptr_t rtti_gen_struct_members_array(struct context *ctx, mir_members_t *members);
static void
rtti_gen_struct_members_slice(struct context *ctx, vm_stack_ptr_t dest, mir_members_t *members);

static struct mir_var *rtti_gen_struct(struct context *ctx, struct mir_type *type);
static void rtti_gen_fn_arg(struct context *ctx, vm_stack_ptr_t dest, struct mir_arg *arg);
static vm_stack_ptr_t rtti_gen_fn_args_array(struct context *ctx, mir_args_t *args);
static vm_stack_ptr_t rtti_gen_fns_array(struct context *ctx, mir_types_t *fns);
static void rtti_gen_fn_args_slice(struct context *ctx, vm_stack_ptr_t dest, mir_args_t *args);
static struct mir_var *rtti_gen_fn(struct context *ctx, struct mir_type *type);
static void rtti_gen_fn_slice(struct context *ctx, vm_stack_ptr_t dest, mir_types_t *fns);
static struct mir_var *rtti_gen_fn_group(struct context *ctx, struct mir_type *type);

// INLINES

static INLINE struct mir_fn *instr_owner_fn(struct mir_instr *instr)
{
    bassert(instr);
    if (instr->kind == MIR_INSTR_BLOCK) {
        return ((struct mir_instr_block *)instr)->owner_fn;
    }
    if (!instr->owner_block) return NULL;
    return instr->owner_block->owner_fn;
}

#define report_error(code, node, format, ...)                                                      \
    _report(ctx->analyze.last_analyzed_instr,                                                      \
            BUILDER_MSG_ERROR,                                                                     \
            ERR_##code,                                                                            \
            (node),                                                                                \
            BUILDER_CUR_WORD,                                                                      \
            (format),                                                                              \
            ##__VA_ARGS__)

#define report_error_after(code, node, format, ...)                                                \
    _report(ctx->analyze.last_analyzed_instr,                                                      \
            BUILDER_MSG_ERROR,                                                                     \
            ERR_##code,                                                                            \
            (node),                                                                                \
            BUILDER_CUR_AFTER,                                                                     \
            (format),                                                                              \
            ##__VA_ARGS__)

#define report_warning(node, format, ...)                                                          \
    _report(ctx->analyze.last_analyzed_instr,                                                      \
            BUILDER_MSG_WARNING,                                                                   \
            0,                                                                                     \
            (node),                                                                                \
            BUILDER_CUR_WORD,                                                                      \
            (format),                                                                              \
            ##__VA_ARGS__)

#define report_note(node, format, ...)                                                             \
    _report(ctx->analyze.last_analyzed_instr,                                                      \
            BUILDER_MSG_NOTE,                                                                      \
            0,                                                                                     \
            (node),                                                                                \
            BUILDER_CUR_WORD,                                                                      \
            (format),                                                                              \
            ##__VA_ARGS__)

static INLINE void _report(struct mir_instr     *current_instr,
                           enum builder_msg_type type,
                           s32                   code,
                           const struct ast     *node,
                           enum builder_cur_pos  cursor_position,
                           const char           *format,
                           ...)
{
    struct location *loc = node ? node->location : NULL;
    va_list          args;
    va_start(args, format);
    builder_vmsg(type, code, loc, cursor_position, format, args);
    va_end(args);
    if (type == BUILDER_MSG_ERROR) report_poly(current_instr);
}

static INLINE struct ast *get_last_instruction_node(struct mir_instr_block *block)
{
    if (!block->last_instr) return NULL;
    return block->last_instr->node;
}

static INLINE void usage_check_push(struct context *ctx, struct scope_entry *entry)
{
    bassert(entry);
    struct scope *scope = entry->parent_scope;
    bassert(scope);
    if (builder.options->no_usage_check) return;
    if (!entry->id) return;
    if (!entry->node) return;
    if (entry->kind != SCOPE_ENTRY_VAR && entry->kind != SCOPE_ENTRY_FN) return;
    if (entry->kind == SCOPE_ENTRY_FN && isflag(entry->data.fn->flags, FLAG_TEST_FN)) return;
    // No usage checking in general is done only for symbols in function local scope and symbols
    // in global private scope.
    if (!scope_is_subtree_of_kind(scope, SCOPE_FN) &&
        !scope_is_subtree_of_kind(scope, SCOPE_PRIVATE)) {
        return;
    }
    arrpush(ctx->analyze.usage_check_arr, entry);
}

static INLINE bool can_mutate_comptime_to_const(struct mir_instr *instr)
{
    bassert(isflag(instr->flags, MIR_IS_ANALYZED) && "Non-analyzed instruction.");
    bassert(mir_is_comptime(instr));

    switch (instr->kind) {
    case MIR_INSTR_CONST:
    case MIR_INSTR_BLOCK:
    case MIR_INSTR_FN_PROTO:
        return false;
    case MIR_INSTR_CALL: {
        // This will allow us to convert function calls supposed to be executed in compile-time to
        // be replaced by constant value containing the result. Notice that this allows generation
        // of 'void' constants if the function does not return.
        return ((struct mir_instr_call *)instr)->call_in_compile_time;
    }
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

// Get struct base type if there is one.
static INLINE struct mir_type *get_base_type(const struct mir_type *struct_type)
{
    if (struct_type->kind != MIR_TYPE_STRUCT) return NULL;
    struct mir_type *base_type = struct_type->data.strct.base_type;
    return base_type;
}

// Get base type scope if there is one.
static INLINE struct scope *get_base_type_scope(struct mir_type *struct_type)
{
    struct mir_type *base_type = get_base_type(struct_type);
    if (!base_type) return NULL;
    if (!mir_is_composit_type(base_type)) return NULL;

    return base_type->data.strct.scope;
}

// Determinate if type is incomplete struct type.
static INLINE bool is_incomplete_struct_type(struct mir_type *type)
{
    return mir_is_composit_type(type) && type->data.strct.is_incomplete;
}

// Checks whether type is complete type, checks also dependencies. In practice only composit types
// can be incomplete, but in some cases (RTTI generation) we need to check whole dependency type
// tree for completeness.
static bool is_complete_type(struct context *ctx, struct mir_type *type)
{
    zone();

    struct {
        struct mir_type *key;
        u8               value; // this is not used
    } *visited = NULL;

    mir_types_t *stack = &ctx->analyze.complete_check_type_stack;
    sarrput(stack, type);
    bool result = true;
    while (sarrlenu(stack)) {
        struct mir_type *top = sarrpop(stack);
        bassert(top);
        if (top->checked_and_complete) continue;
        if (is_incomplete_struct_type(top)) {
            result = false;
            goto DONE;
        }
        switch (top->kind) {
        case MIR_TYPE_PTR: {
            sarrput(stack, top->data.ptr.expr);
            break;
        }
        case MIR_TYPE_ARRAY: {
            sarrput(stack, top->data.array.elem_type);
            break;
        }
        case MIR_TYPE_FN: {
            if (top->data.fn.ret_type) sarrput(stack, top->data.fn.ret_type);
            for (usize i = 0; i < sarrlenu(top->data.fn.args); ++i) {
                struct mir_arg *arg = sarrpeek(top->data.fn.args, i);
                sarrput(stack, arg->type);
            }
            break;
        }
        case MIR_TYPE_DYNARR:
        case MIR_TYPE_SLICE:
        case MIR_TYPE_STRING:
        case MIR_TYPE_VARGS:
        case MIR_TYPE_STRUCT: {
            const s64 index = hmgeti(visited, top);
            if (index != -1) break;
            hmput(visited, top, 0);
            mir_members_t *members = top->data.strct.members;
            for (usize i = 0; i < sarrlenu(members); ++i) {
                struct mir_member *member = sarrpeek(members, i);
                sarrput(stack, member->type);
            }
            break;
        }
        default:
            continue;
        }
    }
DONE:
    sarrclear(stack);
    hmfree(visited);
    type->checked_and_complete = result;
    return_zone(result);
}

// Determinate if instruction has volatile type, that means we can change type of the value during
// analyze pass as needed. This is used for constant integer literals.
static INLINE bool is_instr_type_volatile(struct mir_instr *instr)
{
    switch (instr->kind) {
    case MIR_INSTR_CONST:
        return ((struct mir_instr_const *)instr)->volatile_type;

    case MIR_INSTR_UNOP:
        return ((struct mir_instr_unop *)instr)->volatile_type;

    case MIR_INSTR_BINOP:
        return ((struct mir_instr_binop *)instr)->volatile_type;

    default:
        return false;
    }
}

static INLINE bool type_cmp(const struct mir_type *first, const struct mir_type *second)
{
    bassert(first && second);
    return first->id.hash == second->id.hash;
}

static INLINE bool can_impl_cast(const struct mir_type *from, const struct mir_type *to)
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
    // Polymorph type can be casted into any other type because it can be replaced.
    if (from->kind == MIR_TYPE_POLY || to->kind == MIR_TYPE_POLY) return true;
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

static INLINE struct mir_instr_block *ast_current_block(struct context *ctx)
{
    return ctx->ast.current_block;
}

static INLINE struct mir_fn *ast_current_fn(struct context *ctx)
{
    return ctx->ast.current_block ? ctx->ast.current_block->owner_fn : NULL;
}

static INLINE void terminate_block(struct mir_instr_block *block, struct mir_instr *terminator)
{
    bassert(block);
    if (block->terminal) babort("basic block '%s' already terminated!", block->name);
    block->terminal = terminator;
}

static INLINE bool is_block_terminated(struct mir_instr_block *block)
{
    return block->terminal;
}

static INLINE bool is_current_block_terminated(struct context *ctx)
{
    return ast_current_block(ctx)->terminal;
}

static INLINE void *mutate_instr(struct mir_instr *instr, enum mir_instr_kind kind)
{
    bassert(instr);
    instr->kind = kind;
    return instr;
}

static INLINE struct mir_instr *unref_instr(struct mir_instr *instr)
{
    if (!instr) return NULL;
    if (instr->ref_count == NO_REF_COUNTING) return instr;
    --instr->ref_count;
    return instr;
}

static INLINE struct mir_instr *ref_instr(struct mir_instr *instr)
{
    if (!instr) return NULL;
    if (instr->ref_count == NO_REF_COUNTING) return instr;
    ++instr->ref_count;
    return instr;
}

static INLINE void erase_instr(struct mir_instr *instr)
{
    if (!instr) return;
    if (instr->owner_block) {
        struct mir_instr_block *block = instr->owner_block;
        if (block->entry_instr == instr) block->entry_instr = instr->next;
    }
    if (instr->prev) instr->prev->next = instr->next;
    if (instr->next) instr->next->prev = instr->prev;
    instr->prev = NULL;
    instr->next = NULL;
}

static INLINE void erase_block(struct mir_instr *instr)
{
    instrs_t queue = SARR_ZERO;
    sarrput(&queue, instr);
    while (sarrlenu(&queue)) {
        struct mir_instr *block = sarrpop(&queue);
        bassert(block);
        bassert(block->kind == MIR_INSTR_BLOCK && "Use erase_instr instead.");
        bassert(block->ref_count == 0 && "Cannot erase referenced block!");
        erase_instr(block);
        struct mir_instr *terminal = ((struct mir_instr_block *)block)->terminal;
        if (!terminal) continue;
        // bassert(terminal && "Unterminated block!");
        switch (terminal->kind) {
        case MIR_INSTR_BR: {
            struct mir_instr_br *br = (struct mir_instr_br *)terminal;
            if (unref_instr(&br->then_block->base)->ref_count == 0) {
                sarrput(&queue, &br->then_block->base);
            }
            break;
        }
        case MIR_INSTR_COND_BR: {
            struct mir_instr_cond_br *br = (struct mir_instr_cond_br *)terminal;
            if (unref_instr(&br->then_block->base)->ref_count == 0) {
                sarrput(&queue, &br->then_block->base);
            }
            if (unref_instr(&br->else_block->base)->ref_count == 0) {
                sarrput(&queue, &br->else_block->base);
            }
            break;
        }
        case MIR_INSTR_SWITCH: {
            struct mir_instr_switch *sw = (struct mir_instr_switch *)terminal;
            bassert(sw->cases);
            for (usize i = 0; i < sarrlenu(sw->cases); ++i) {
                struct mir_switch_case *c = &sarrpeek(sw->cases, i);
                if (unref_instr(&c->block->base)->ref_count == 0) {
                    sarrput(&queue, &c->block->base);
                }
            }
            break;
        }
        default:
            bassert(false && "Unhandled terminal instruction!");
            break;
        }
    }
    sarrfree(&queue);
}

static INLINE void insert_instr_after(struct mir_instr *after, struct mir_instr *instr)
{
    bassert(after && instr);

    struct mir_instr_block *block = after->owner_block;

    instr->next = after->next;
    instr->prev = after;
    if (after->next) after->next->prev = instr;
    after->next = instr;

    instr->owner_block = block;
    if (block->last_instr == after) instr->owner_block->last_instr = instr;
}

static INLINE void insert_instr_before(struct mir_instr *before, struct mir_instr *instr)
{
    bassert(before && instr);

    struct mir_instr_block *block = before->owner_block;

    instr->next = before;
    instr->prev = before->prev;
    if (before->prev) before->prev->next = instr;
    before->prev = instr;

    instr->owner_block = block;
    if (block->entry_instr == before) instr->owner_block->entry_instr = instr;
}

static INLINE void push_into_gscope(struct context *ctx, struct mir_instr *instr)
{
    bassert(instr);
    instr->id = arrlenu(ctx->assembly->MIR.global_instrs);
    arrput(ctx->assembly->MIR.global_instrs, instr);
};

// =================================================================================================
// Analyze stack manipulation tools
// =================================================================================================

// Get current analyze stack.
#define analyze_current(ctx) ((ctx)->analyze.stack[(ctx)->analyze.si])
// Swap analyze stacks and return previous index.
#define analyze_swap(ctx) ((ctx)->analyze.si ^= 1, (ctx)->analyze.si ^ 1)
#define analyze_pending_count(ctx)                                                                 \
    (arrlenu((ctx)->analyze.stack[0]) + arrlenu((ctx)->analyze.stack[1]))

static int         push_count = 0;
static INLINE void analyze_schedule(struct context *ctx, struct mir_instr *instr)
{
    bassert(instr);
    ++push_count;
    arrput(analyze_current(ctx), instr);
}

static INLINE void analyze_notify_provided(struct context *ctx, hash_t hash)
{
    const s64 index = hmgeti(ctx->analyze.waiting, hash);
    if (index == -1) return; // No one is waiting for this...

    instrs_t *wq = &ctx->analyze.waiting[index].value;
    bassert(wq);

    for (usize i = 0; i < sarrlenu(wq); ++i) {
        analyze_schedule(ctx, sarrpeek(wq, i));
    }

    // Also clear element content!
    sarrfree(wq);
    hmdel(ctx->analyze.waiting, hash);
}
// =================================================================================================

static INLINE const char *create_unique_name(struct context *ctx, const char *prefix)
{
    static u64 ui = 0;
    return scprint(&ctx->assembly->string_cache, "%s.%llu", prefix, ui++);
}

static INLINE bool is_builtin2(struct id *id, enum builtin_id_kind kind)
{
    if (!id) return false;
    return id->hash == builtin_ids[kind].hash;
}

static INLINE bool is_builtin(struct ast *ident, enum builtin_id_kind kind)
{
    if (!ident) return false;
    bassert(ident->kind == AST_IDENT);
    return is_builtin2(&ident->data.ident.id, kind);
}

static enum builtin_id_kind get_builtin_kind(struct ast *ident)
{
    if (!ident) return false;
    bassert(ident->kind == AST_IDENT);
    // @PERFORMANCE: Eventually use hash table.
    for (u32 i = 0; i < static_arrlenu(builtin_ids); ++i) {
        if (builtin_ids[i].hash == ident->data.ident.id.hash) {
            return i;
        }
    }
    return BUILTIN_ID_NONE;
}

static INLINE bool get_block_terminator(struct mir_instr_block *block)
{
    return block->terminal;
}

static INLINE void set_current_block(struct context *ctx, struct mir_instr_block *block)
{
    ctx->ast.current_block = block;
}

static INLINE void error_types(struct context   *ctx,
                               struct mir_instr *instr,
                               struct mir_type  *from,
                               struct mir_type  *to,
                               struct ast       *node,
                               const char       *msg)
{
    bassert(from && to);
    if (!msg) msg = "No implicit cast for type '%s' and '%s'.";
    char tmp_from[256];
    char tmp_to[256];
    mir_type_to_str(tmp_from, 256, from, true);
    mir_type_to_str(tmp_to, 256, to, true);
    report_error(INVALID_TYPE, node, msg, tmp_from, tmp_to);
    DEBUG_PRINT_MIR(instr);
}

static INLINE void commit_fn(struct context *ctx, struct mir_fn *fn)
{
    struct id *id = fn->id;
    bassert(id);
    struct scope_entry *entry = fn->scope_entry;
    BL_MAGIC_ASSERT(entry);
    bassert(entry->kind != SCOPE_ENTRY_VOID);
    entry->kind    = SCOPE_ENTRY_FN;
    entry->data.fn = fn;
    analyze_notify_provided(ctx, id->hash);
    usage_check_push(ctx, entry);
}

static INLINE void commit_variant(struct context UNUSED(*ctx), struct mir_variant *variant)
{
    struct scope_entry *entry = variant->entry;
    BL_MAGIC_ASSERT(entry);
    bassert(entry->kind != SCOPE_ENTRY_VOID);
    entry->kind         = SCOPE_ENTRY_VARIANT;
    entry->data.variant = variant;
}

static INLINE void commit_member(struct context UNUSED(*ctx), struct mir_member *member)
{
    struct scope_entry *entry = member->entry;
    BL_MAGIC_ASSERT(entry);
    // Do not commit void entries
    if (entry->kind == SCOPE_ENTRY_VOID) return;
    entry->kind        = SCOPE_ENTRY_MEMBER;
    entry->data.member = member;
}

static INLINE void commit_var(struct context *ctx, struct mir_var *var)
{
    struct id *id = var->id;
    bassert(id);
    struct scope_entry *entry = var->entry;
    BL_MAGIC_ASSERT(entry);
    // Do not commit void entries
    if (entry->kind == SCOPE_ENTRY_VOID) return;
    entry->kind     = SCOPE_ENTRY_VAR;
    entry->data.var = var;
    BL_TRACY_MESSAGE("COMMIT_VAR", "%s", id->str);
    if (var->is_global || var->is_struct_typedef) analyze_notify_provided(ctx, id->hash);
    usage_check_push(ctx, entry);
}

// Provide builtin type. Register & commit.
static INLINE void provide_builtin_type(struct context *ctx, struct mir_type *type)
{
    struct scope_entry *entry =
        register_symbol(ctx, NULL, type->user_id, ctx->assembly->gscope, true);
    if (!entry) return;
    bassert(entry->kind != SCOPE_ENTRY_VOID);
    entry->kind      = SCOPE_ENTRY_TYPE;
    entry->data.type = type;
}

static INLINE void
provide_builtin_member(struct context *ctx, struct scope *scope, struct mir_member *member)
{
    struct scope_entry *entry = register_symbol(ctx, NULL, member->id, scope, true);
    if (!entry) return;
    bassert(entry->kind != SCOPE_ENTRY_VOID);
    entry->kind        = SCOPE_ENTRY_MEMBER;
    entry->data.member = member;
    member->entry      = entry;
}

static INLINE void
provide_builtin_variant(struct context *ctx, struct scope *scope, struct mir_variant *variant)
{
    struct scope_entry *entry = register_symbol(ctx, NULL, variant->id, scope, true);
    if (!entry) return;
    bassert(entry->kind != SCOPE_ENTRY_VOID);
    entry->kind         = SCOPE_ENTRY_VARIANT;
    entry->data.variant = variant;
    variant->entry      = entry;
}

static INLINE void
phi_add_income(struct mir_instr_phi *phi, struct mir_instr *value, struct mir_instr_block *block)
{
    bassert(phi && value && block);
    sarrput(phi->incoming_values, ref_instr(value));
    sarrput(phi->incoming_blocks, ref_instr(&block->base));
    if (value->kind == MIR_INSTR_COND_BR) {
        ((struct mir_instr_cond_br *)value)->keep_stack_value = true;
    }
}

static INLINE bool is_load_needed(struct mir_instr *instr)
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
        struct mir_instr_load *load = (struct mir_instr_load *)instr;
        return load->is_deref && is_load_needed(load->src);
    }

    default:
        break;
    }

    return true;
}

static INLINE bool
is_to_any_needed(struct context *ctx, struct mir_instr *src, struct mir_type *dest_type)
{
    if (!dest_type || !src) return false;
    struct mir_type *any_type = lookup_builtin_type(ctx, BUILTIN_ID_ANY);
    bassert(any_type);

    if (dest_type != any_type) return false;

    if (is_load_needed(src)) {
        struct mir_type *src_type = src->value.type;
        if (mir_deref_type(src_type) == any_type) return false;
    }

    return true;
}

void ast_push_defer_stack(struct context *ctx)
{
    if (++ctx->ast.current_defer_stack_index == arrlen(ctx->ast.defer_stack)) {
        arrput(ctx->ast.defer_stack, (defer_stack_t){0});
    }
}

void ast_pop_defer_stack(struct context *ctx)
{
    bassert(ctx->ast.current_defer_stack_index >= 0);
    sarrclear(&ctx->ast.defer_stack[ctx->ast.current_defer_stack_index]);
    ctx->ast.current_defer_stack_index--;
}

void ast_free_defer_stack(struct context *ctx)
{
    for (usize i = 0; i < arrlenu(ctx->ast.defer_stack); ++i) {
        sarrfree(&ctx->ast.defer_stack[i]);
    }
    arrfree(ctx->ast.defer_stack);
}

void type_init_id(struct context *ctx, struct mir_type *type)
{
    // =============================================================================================
#define gen_id_struct(tmp)                                                                         \
    if (type->user_id) {                                                                           \
        strappend(tmp, "%s", type->user_id->str);                                                  \
    }                                                                                              \
    strappend(tmp, "{");                                                                           \
    for (usize i = 0; i < sarrlenu(type->data.strct.members); ++i) {                               \
        struct mir_member *member = sarrpeek(type->data.strct.members, i);                         \
        bassert(member->type->id.str);                                                             \
        strappend(tmp, "%s", member->type->id.str);                                                \
        if (i != sarrlenu(type->data.strct.members) - 1) strappend(tmp, ",");                      \
    }                                                                                              \
    strappend(tmp, "}");
    // =============================================================================================
    bassert(type && "Invalid type pointer!");
    char *tmp = gettmpstr();

    switch (type->kind) {
    case MIR_TYPE_BOOL:
    case MIR_TYPE_VOID:
    case MIR_TYPE_TYPE:
    case MIR_TYPE_REAL:
    case MIR_TYPE_NAMED_SCOPE:
    case MIR_TYPE_INT: {
        bassert(type->user_id);
        strprint(tmp, "%s", type->user_id->str);
        break;
    }

    case MIR_TYPE_NULL: {
        strprint(tmp, "n.%s", type->data.null.base_type->id.str);
        break;
    }

    case MIR_TYPE_POLY: {
        static u64 serial = 0;
        bassert(type->user_id);
        strprint(tmp, "?%llu.%s", serial, type->user_id->str);
        serial++;
        break;
    }

    case MIR_TYPE_PTR: {
        strprint(tmp, "p.%s", type->data.ptr.expr->id.str);
        break;
    }

    case MIR_TYPE_FN: {
        strprint(tmp, "f.(");
        // append all arg types isd
        for (usize i = 0; i < sarrlenu(type->data.fn.args); ++i) {
            struct mir_arg *arg = sarrpeek(type->data.fn.args, i);
            bassert(arg->type->id.str);
            if (i != sarrlenu(type->data.fn.args) - 1) {
                strappend(tmp, "%s,", arg->type->id.str);
            } else {
                strappend(tmp, "%s", arg->type->id.str);
            }
        }
        strappend(tmp, ")");
        type->data.fn.argument_hash = strhash(tmp);
        if (type->data.fn.ret_type) {
            bassert(type->data.fn.ret_type->id.str);
            strappend(tmp, "%s", type->data.fn.ret_type->id.str);
        } else {
            // implicit return void
            strappend(tmp, "%s", ctx->builtin_types->t_void->id.str);
        }
        break;
    }

    case MIR_TYPE_FN_GROUP: {
        strappend(tmp, "f.{");
        // append all arg types isd
        mir_types_t *variants = type->data.fn_group.variants;
        for (usize i = 0; i < sarrlenu(variants); ++i) {
            struct mir_type *variant = sarrpeek(variants, i);
            bassert(variant->id.str);
            if (i != sarrlenu(variants) - 1) {
                strappend(tmp, "%s,", variant->id.str);
            } else {
                strappend(tmp, "%s", variant->id.str);
            }
        }
        strappend(tmp, "}");
        break;
    }

    case MIR_TYPE_ARRAY: {
        strappend(tmp,
                  "%llu.%s",
                  (unsigned long long)type->data.array.len,
                  type->data.array.elem_type->id.str);
        break;
    }

    case MIR_TYPE_STRING: {
        strappend(tmp, "ss.");
        gen_id_struct(tmp);
        break;
    }

    case MIR_TYPE_SLICE: {
        strappend(tmp, "sl.");
        gen_id_struct(tmp);
        break;
    }

    case MIR_TYPE_DYNARR: {
        strappend(tmp, "da.");
        gen_id_struct(tmp);
        break;
    }

    case MIR_TYPE_VARGS: {
        strappend(tmp, "sv.");
        gen_id_struct(tmp);
        break;
    }

    case MIR_TYPE_STRUCT: {
        static u64 serial   = 0;
        const bool is_union = type->data.strct.is_union;
        if (type->user_id) {
            strprint(tmp, "%s%llu.%s", is_union ? "u" : "s", serial, type->user_id->str);
            serial++;
            break;
        }
        strappend(tmp, is_union ? "u." : "s.");
        gen_id_struct(tmp);
        break;
    }

    case MIR_TYPE_ENUM: {
        if (type->user_id) {
            strappend(tmp, "e.%s", type->user_id->str);
        } else {
            strappend(tmp, "e.");
        }
        strappend(tmp, "(%s){", type->data.enm.base_type->id.str);
        mir_variants_t *variants = type->data.enm.variants;
        for (usize i = 0; i < sarrlenu(variants); ++i) {
            struct mir_variant *variant = sarrpeek(variants, i);
            if (i != sarrlenu(type->data.enm.variants) - 1) {
                strappend(tmp, "%lld,", variant->value);
            } else {
                strappend(tmp, "%lld", variant->value);
            }
        }
        strappend(tmp, "}");
        break;
    }

    default:
        BL_UNIMPLEMENTED;
    }

    id_init(&type->id, scdup(&ctx->assembly->string_cache, tmp, strlenu(tmp)));
    puttmpstr(tmp);
#if TRACY_ENABLE
    static int tc = 0;
    TracyCPlot("Type count", ++tc);
    BL_TRACY_MESSAGE("CREATE_TYPE",
                     "%s %s (%lluB)",
                     type->id.str,
                     !is_incomplete_struct_type(type) ? "<INCOMPLETE>" : "",
                     (unsigned long long)sizeof(struct mir_type));
#endif

#undef gen_id_struct
}

struct mir_type *create_type(struct context *ctx, enum mir_type_kind kind, struct id *user_id)
{
    struct mir_type *type = arena_safe_alloc(&ctx->assembly->arenas.mir.type);
    BL_MAGIC_SET(type);
    type->kind    = kind;
    type->user_id = user_id;
    return type;
}

struct scope_entry *register_symbol(struct context *ctx,
                                    struct ast     *node,
                                    struct id      *id,
                                    struct scope   *scope,
                                    bool            is_builtin)
{
    bassert(id && "Missing symbol ID.");
    bassert(scope && "Missing entry scope.");
    // Do not register explicitly unused symbol!!!
    if (is_ignored_id(id)) {
        bassert(!is_builtin);
        return ctx->analyze.void_entry;
    }
    const bool          is_private  = scope->kind == SCOPE_PRIVATE;
    const s32           layer_index = ctx->polymorph.current_scope_layer_index;
    struct scope_entry *collision   = scope_lookup(scope, layer_index, id, is_private, false, NULL);
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
    struct scope_entry *entry = scope_create_entry(
        &ctx->assembly->arenas.scope, SCOPE_ENTRY_INCOMPLETE, id, node, is_builtin);
    scope_insert(scope, layer_index, entry);
    BL_TRACY_MESSAGE("REGISTER_SYMBOL", "%s", id->str);
    return entry;

COLLIDE : {
    char *err_msg = (collision->is_builtin || is_builtin)
                        ? "Symbol name collision with compiler builtin '%s'."
                        : "Duplicate symbol";
    report_error(DUPLICATE_SYMBOL, node, err_msg, id->str);
    if (collision->node) {
        report_note(collision->node, "Previous declaration found here.");
    }
    return NULL;
}
}

struct mir_type *lookup_builtin_type(struct context *ctx, enum builtin_id_kind kind)
{
    struct id          *id    = &builtin_ids[kind];
    struct scope       *scope = ctx->assembly->gscope;
    struct scope_entry *found = scope_lookup(scope, SCOPE_DEFAULT_LAYER, id, true, false, NULL);

    if (!found) babort("Missing compiler internal symbol '%s'", id->str);
    if (found->kind == SCOPE_ENTRY_INCOMPLETE) return NULL;

    if (!found->is_builtin) {
        report_warning(found->node, "Builtins used by compiler must have '#compiler' flag!");
    }

    bassert(found->kind == SCOPE_ENTRY_VAR);
    struct mir_var *var = found->data.var;
    bassert(var && var->value.is_comptime && var->value.type->kind == MIR_TYPE_TYPE);
    struct mir_type *var_type = MIR_CEV_READ_AS(struct mir_type *, &var->value);
    BL_MAGIC_ASSERT(var_type);

    // Wait when internal is not complete!
    if (is_incomplete_struct_type(var_type)) {
        return NULL;
    }

    return var_type;
}

struct mir_fn *lookup_builtin_fn(struct context *ctx, enum builtin_id_kind kind)
{
    struct id          *id    = &builtin_ids[kind];
    struct scope       *scope = ctx->assembly->gscope;
    struct scope_entry *found = scope_lookup(scope, SCOPE_DEFAULT_LAYER, id, true, false, NULL);

    if (!found) babort("Missing compiler internal symbol '%s'", id->str);
    if (found->kind == SCOPE_ENTRY_INCOMPLETE) return NULL;

    if (!found->is_builtin) {
        report_warning(found->node, "Builtins used by compiler must have '#compiler' flag!");
    }

    bassert(found->kind == SCOPE_ENTRY_FN);
    ref_instr(found->data.fn->prototype);
    return found->data.fn;
}

struct id *lookup_builtins_rtti(struct context *ctx)
{
#define LOOKUP_TYPE(N, K)                                                                          \
    if (!ctx->builtin_types->t_Type##N) {                                                          \
        ctx->builtin_types->t_Type##N = lookup_builtin_type(ctx, BUILTIN_ID_TYPE_##K);             \
        if (!ctx->builtin_types->t_Type##N) {                                                      \
            return &builtin_ids[BUILTIN_ID_TYPE_##K];                                              \
        }                                                                                          \
    }

    if (ctx->builtin_types->is_rtti_ready) return NULL;
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

    ctx->builtin_types->t_TypeInfo_ptr   = create_type_ptr(ctx, ctx->builtin_types->t_TypeInfo);
    ctx->builtin_types->t_TypeInfoFn_ptr = create_type_ptr(ctx, ctx->builtin_types->t_TypeInfoFn);
    ctx->builtin_types->t_TypeInfo_slice =
        CREATE_TYPE_STRUCT_SLICE(ctx, NULL, ctx->builtin_types->t_TypeInfo_ptr);
    ctx->builtin_types->t_TypeInfoStructMembers_slice = CREATE_TYPE_STRUCT_SLICE(
        ctx, NULL, create_type_ptr(ctx, ctx->builtin_types->t_TypeInfoStructMember));
    ctx->builtin_types->t_TypeInfoEnumVariants_slice = CREATE_TYPE_STRUCT_SLICE(
        ctx, NULL, create_type_ptr(ctx, ctx->builtin_types->t_TypeInfoEnumVariant));
    ctx->builtin_types->t_TypeInfoFnArgs_slice = CREATE_TYPE_STRUCT_SLICE(
        ctx, NULL, create_type_ptr(ctx, ctx->builtin_types->t_TypeInfoFnArg));
    ctx->builtin_types->t_TypeInfoFn_ptr_slice = CREATE_TYPE_STRUCT_SLICE(
        ctx, NULL, create_type_ptr(ctx, ctx->builtin_types->t_TypeInfoFn_ptr));
    ctx->builtin_types->is_rtti_ready = true;
    return NULL;
#undef LOOKUP_TYPE
}

struct id *lookup_builtins_any(struct context *ctx)
{
    if (ctx->builtin_types->is_any_ready) return NULL;
    ctx->builtin_types->t_Any = lookup_builtin_type(ctx, BUILTIN_ID_ANY);
    if (!ctx->builtin_types->t_Any) {
        return &builtin_ids[BUILTIN_ID_ANY];
    }
    ctx->builtin_types->t_Any_ptr    = create_type_ptr(ctx, ctx->builtin_types->t_Any);
    ctx->builtin_types->is_any_ready = true;
    return NULL;
}

struct id *lookup_builtins_test_cases(struct context *ctx)
{
    if (ctx->builtin_types->is_test_cases_ready) return NULL;
    ctx->builtin_types->t_TestCase = lookup_builtin_type(ctx, BUILTIN_ID_TYPE_TEST_CASES);
    if (!ctx->builtin_types->t_TestCase) {
        return &builtin_ids[BUILTIN_ID_TYPE_TEST_CASES];
    }
    ctx->builtin_types->t_TestCases_slice =
        CREATE_TYPE_STRUCT_SLICE(ctx, NULL, create_type_ptr(ctx, ctx->builtin_types->t_TestCase));
    return NULL;
}

struct id *lookup_builtins_code_loc(struct context *ctx)
{
    if (ctx->builtin_types->t_CodeLocation) return NULL;
    ctx->builtin_types->t_CodeLocation = lookup_builtin_type(ctx, BUILTIN_ID_TYPE_CALL_LOCATION);
    if (!ctx->builtin_types->t_CodeLocation) {
        return &builtin_ids[BUILTIN_ID_TYPE_CALL_LOCATION];
    }
    ctx->builtin_types->t_CodeLocation_ptr =
        create_type_ptr(ctx, ctx->builtin_types->t_CodeLocation);
    return NULL;
}

struct scope_entry *
lookup_composit_member(struct mir_type *type, struct id *rid, struct mir_type **out_base_type)
{
    bassert(type);
    bassert(mir_is_composit_type(type) && "Expected composit type!");

    struct scope       *scope = type->data.strct.scope;
    struct scope_entry *found = NULL;

    while (true) {
        found = scope_lookup(scope, SCOPE_DEFAULT_LAYER, rid, false, true, NULL);
        if (found) break;
        scope = get_base_type_scope(type);
        type  = get_base_type(type);
        if (!scope) break;
    }
    if (out_base_type) *out_base_type = type;
    return found;
}

struct mir_var *add_global_variable(struct context   *ctx,
                                    struct id        *id,
                                    bool              is_mutable,
                                    struct mir_instr *initializer)
{
    bassert(initializer);
    struct scope     *scope = ctx->assembly->gscope;
    struct mir_instr *decl_var =
        append_instr_decl_var(ctx, NULL, id, scope, NULL, NULL, is_mutable, 0, BUILTIN_ID_NONE);

    struct mir_instr_block *prev_block = ast_current_block(ctx);
    struct mir_instr_block *block      = append_global_block(ctx, INIT_VALUE_FN_NAME);
    set_current_block(ctx, block);
    append_current_block(ctx, initializer);
    mir_instrs_t *decls = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    sarrput(decls, decl_var);
    append_instr_set_initializer(ctx, NULL, decls, initializer);
    set_current_block(ctx, prev_block);
    struct mir_var *var = ((struct mir_instr_decl_var *)decl_var)->var;
    var->entry          = register_symbol(ctx, NULL, id, scope, true);
    return var;
}

struct mir_var *add_global_bool(struct context *ctx, struct id *id, bool is_mutable, bool v)
{
    return add_global_variable(ctx, id, is_mutable, create_instr_const_bool(ctx, NULL, v));
}

struct mir_var *
add_global_int(struct context *ctx, struct id *id, bool is_mutable, struct mir_type *type, s32 v)
{
    return add_global_variable(ctx, id, is_mutable, create_instr_const_int(ctx, NULL, type, v));
}

struct mir_type *create_type_type(struct context *ctx)
{
    struct mir_type *tmp = create_type(ctx, MIR_TYPE_TYPE, &builtin_ids[BUILTIN_ID_TYPE_TYPE]);
    type_init_id(ctx, tmp);
    type_init_llvm_dummy(ctx, tmp);
    return tmp;
}

struct mir_type *create_type_named_scope(struct context *ctx)
{
    struct mir_type *tmp =
        create_type(ctx, MIR_TYPE_NAMED_SCOPE, &builtin_ids[BUILTIN_ID_TYPE_NAMED_SCOPE]);
    type_init_id(ctx, tmp);
    type_init_llvm_dummy(ctx, tmp);
    return tmp;
}

struct mir_type *create_type_null(struct context *ctx, struct mir_type *base_type)
{
    bassert(base_type);
    struct mir_type *tmp     = create_type(ctx, MIR_TYPE_NULL, &builtin_ids[BUILTIN_ID_NULL]);
    tmp->data.null.base_type = base_type;
    type_init_id(ctx, tmp);
    type_init_llvm_null(ctx, tmp);
    return tmp;
}

struct mir_type *create_type_void(struct context *ctx)
{
    struct mir_type *tmp = create_type(ctx, MIR_TYPE_VOID, &builtin_ids[BUILTIN_ID_TYPE_VOID]);
    type_init_id(ctx, tmp);
    type_init_llvm_void(ctx, tmp);
    return tmp;
}

struct mir_type *create_type_bool(struct context *ctx)
{
    struct mir_type *tmp = create_type(ctx, MIR_TYPE_BOOL, &builtin_ids[BUILTIN_ID_TYPE_BOOL]);
    type_init_id(ctx, tmp);
    type_init_llvm_bool(ctx, tmp);
    return tmp;
}

struct mir_type *create_type_poly(struct context *ctx, struct id *user_id, bool is_master)
{
    bassert(user_id);
    struct mir_type *tmp     = create_type(ctx, MIR_TYPE_POLY, user_id);
    tmp->data.poly.is_master = is_master;
    type_init_id(ctx, tmp);
    type_init_llvm_dummy(ctx, tmp);
    return tmp;
}

struct mir_type *create_type_int(struct context *ctx, struct id *id, s32 bitcount, bool is_signed)
{
    bassert(id);
    bassert(bitcount > 0);
    struct mir_type *tmp        = create_type(ctx, MIR_TYPE_INT, id);
    tmp->data.integer.bitcount  = bitcount;
    tmp->data.integer.is_signed = is_signed;

    type_init_id(ctx, tmp);
    type_init_llvm_int(ctx, tmp);

    return tmp;
}

struct mir_type *create_type_real(struct context *ctx, struct id *id, s32 bitcount)
{
    bassert(bitcount > 0);
    struct mir_type *tmp    = create_type(ctx, MIR_TYPE_REAL, id);
    tmp->data.real.bitcount = bitcount;

    type_init_id(ctx, tmp);
    type_init_llvm_real(ctx, tmp);

    return tmp;
}

struct mir_type *create_type_ptr(struct context *ctx, struct mir_type *src_type)
{
    bassert(src_type && "Invalid src type for pointer type.");
    struct mir_type *tmp = create_type(ctx, MIR_TYPE_PTR, NULL);
    tmp->data.ptr.expr   = src_type;
    type_init_id(ctx, tmp);
    type_init_llvm_ptr(ctx, tmp);
    return tmp;
}

struct mir_type *create_type_fn(struct context  *ctx,
                                struct id       *id,
                                struct mir_type *ret_type,
                                mir_args_t      *args,
                                bool             is_vargs,
                                bool             has_default_args,
                                bool             is_polymorph)
{
    struct mir_type *tmp    = create_type(ctx, MIR_TYPE_FN, id);
    tmp->data.fn.args       = args;
    tmp->data.fn.ret_type   = ret_type ? ret_type : ctx->builtin_types->t_void;
    tmp->data.fn.builtin_id = BUILTIN_ID_NONE;

    if (is_vargs) setflag(tmp->data.fn.flags, MIR_TYPE_FN_FLAG_IS_VARGS);
    if (has_default_args) setflag(tmp->data.fn.flags, MIR_TYPE_FN_FLAG_HAS_DEFAULT_ARGS);
    if (is_polymorph) setflag(tmp->data.fn.flags, MIR_TYPE_FN_FLAG_IS_POLYMORPH);
    type_init_id(ctx, tmp);
    type_init_llvm_fn(ctx, tmp);
    return tmp;
}

struct mir_type *create_type_fn_group(struct context *ctx, struct id *id, mir_types_t *variants)
{
    bassert(sarrlenu(variants));
    struct mir_type *tmp        = create_type(ctx, MIR_TYPE_FN_GROUP, id);
    tmp->data.fn_group.variants = variants;
    type_init_id(ctx, tmp);
    type_init_llvm_dummy(ctx, tmp);
    return tmp;
}

struct mir_type *
create_type_array(struct context *ctx, struct id *id, struct mir_type *elem_type, s64 len)
{
    bassert(elem_type);
    struct mir_type *tmp      = create_type(ctx, MIR_TYPE_ARRAY, id);
    tmp->data.array.elem_type = elem_type;
    tmp->data.array.len       = len;
    type_init_id(ctx, tmp);
    type_init_llvm_array(ctx, tmp);
    return tmp;
}

struct mir_type *create_type_struct(struct context    *ctx,
                                    enum mir_type_kind kind,
                                    struct id         *id, // optional
                                    struct scope      *scope,
                                    mir_members_t     *members,   // struct mir_member
                                    struct mir_type   *base_type, // optional
                                    bool               is_union,
                                    bool               is_packed,
                                    bool               is_multiple_return_type)
{
    struct mir_type *tmp                    = create_type(ctx, kind, id);
    tmp->data.strct.members                 = members;
    tmp->data.strct.scope                   = scope;
    tmp->data.strct.is_packed               = is_packed;
    tmp->data.strct.is_union                = is_union;
    tmp->data.strct.is_multiple_return_type = is_multiple_return_type;
    tmp->data.strct.base_type               = base_type;
    type_init_id(ctx, tmp);
    type_init_llvm_struct(ctx, tmp);
    return tmp;
}

struct mir_type *complete_type_struct(struct context   *ctx,
                                      struct mir_instr *fwd_decl,
                                      struct scope     *scope,
                                      mir_members_t    *members,
                                      struct mir_type  *base_type,
                                      bool              is_packed,
                                      bool              is_union,
                                      bool              is_multiple_return_type)
{
    bassert(fwd_decl && "Invalid fwd_decl pointer!");

    bassert(fwd_decl->value.type->kind == MIR_TYPE_TYPE &&
            "Forward struct declaration does not point to type definition!");

    struct mir_type *incomplete_type = MIR_CEV_READ_AS(struct mir_type *, &fwd_decl->value);
    BL_MAGIC_ASSERT(incomplete_type);
    bassert(incomplete_type->kind == MIR_TYPE_STRUCT && "Incomplete type is not struct type!");
    bassert(incomplete_type->data.strct.is_incomplete &&
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
    type_init_llvm_struct(ctx, incomplete_type);
    return incomplete_type;
}

struct mir_type *
create_type_struct_incomplete(struct context *ctx, struct id *user_id, bool is_union)
{
    struct mir_type *type          = create_type(ctx, MIR_TYPE_STRUCT, user_id);
    type->data.strct.is_incomplete = true;
    type->data.strct.is_union      = is_union;

    type_init_id(ctx, type);
    type_init_llvm_struct(ctx, type);
    return type;
}

struct mir_type *_create_type_struct_slice(struct context    *ctx,
                                           enum mir_type_kind kind,
                                           struct id         *id,
                                           struct mir_type   *elem_ptr_type)
{
    bassert(mir_is_pointer_type(elem_ptr_type));
    bassert(kind == MIR_TYPE_STRING || kind == MIR_TYPE_VARGS || kind == MIR_TYPE_SLICE);
    mir_members_t *members = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    // Slice layout struct { s64, *T }
    struct scope *body_scope =
        scope_create(&ctx->assembly->arenas.scope, SCOPE_TYPE_STRUCT, ctx->assembly->gscope, NULL);

    struct mir_member *tmp;
    tmp = create_member(ctx, NULL, &builtin_ids[BUILTIN_ID_ARR_LEN], 0, ctx->builtin_types->t_s64);

    sarrput(members, tmp);
    provide_builtin_member(ctx, body_scope, tmp);

    tmp = create_member(ctx, NULL, &builtin_ids[BUILTIN_ID_ARR_PTR], 1, elem_ptr_type);

    sarrput(members, tmp);
    provide_builtin_member(ctx, body_scope, tmp);

    return create_type_struct(ctx, kind, id, body_scope, members, NULL, false, false, false);
}

struct mir_type *
create_type_struct_dynarr(struct context *ctx, struct id *id, struct mir_type *elem_ptr_type)
{
    bassert(mir_is_pointer_type(elem_ptr_type));
    mir_members_t *members = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    // Dynamic array layout struct { s64, *T, usize, allocator }
    struct scope *body_scope =
        scope_create(&ctx->assembly->arenas.scope, SCOPE_TYPE_STRUCT, ctx->assembly->gscope, NULL);

    struct mir_member *tmp;
    { // .len
        tmp = create_member(
            ctx, NULL, &builtin_ids[BUILTIN_ID_ARR_LEN], 0, ctx->builtin_types->t_s64);

        sarrput(members, tmp);
        provide_builtin_member(ctx, body_scope, tmp);
    }

    { // .ptr
        tmp = create_member(ctx, NULL, &builtin_ids[BUILTIN_ID_ARR_PTR], 1, elem_ptr_type);

        sarrput(members, tmp);
        provide_builtin_member(ctx, body_scope, tmp);
    }

    { // .allocated
        tmp = create_member(
            ctx, NULL, &builtin_ids[BUILTIN_ID_ARR_ALLOCATED], 2, ctx->builtin_types->t_usize);

        sarrput(members, tmp);
        provide_builtin_member(ctx, body_scope, tmp);
    }

    { // .allocator
        tmp = create_member(
            ctx, NULL, &builtin_ids[BUILTIN_ID_ARR_ALLOCATOR], 3, ctx->builtin_types->t_u8_ptr);

        sarrput(members, tmp);
        provide_builtin_member(ctx, body_scope, tmp);
    }

    return create_type_struct(
        ctx, MIR_TYPE_DYNARR, id, body_scope, members, NULL, false, false, false);
}

struct mir_type *create_type_enum(struct context  *ctx,
                                  struct id       *id,
                                  struct scope    *scope,
                                  struct mir_type *base_type,
                                  mir_variants_t  *variants,
                                  const bool       is_flags)
{
    bassert(base_type);
    struct mir_type *tmp    = create_type(ctx, MIR_TYPE_ENUM, id);
    tmp->data.enm.scope     = scope;
    tmp->data.enm.base_type = base_type;
    tmp->data.enm.variants  = variants;
    tmp->data.enm.is_flags  = is_flags;

    type_init_id(ctx, tmp);
    type_init_llvm_enum(ctx, tmp);

    return tmp;
}

void type_init_llvm_int(struct context *ctx, struct mir_type *type)
{
    type->llvm_type =
        LLVMIntTypeInContext(ctx->assembly->llvm.ctx, (unsigned int)type->data.integer.bitcount);
    type->size_bits        = LLVMSizeOfTypeInBits(ctx->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(ctx->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s8)LLVMABIAlignmentOfType(ctx->assembly->llvm.TD, type->llvm_type);
}

void type_init_llvm_real(struct context *ctx, struct mir_type *type)
{
    if (type->data.real.bitcount == 32)
        type->llvm_type = LLVMFloatTypeInContext(ctx->assembly->llvm.ctx);
    else if (type->data.real.bitcount == 64)
        type->llvm_type = LLVMDoubleTypeInContext(ctx->assembly->llvm.ctx);
    else
        babort("invalid floating point type");

    type->size_bits        = LLVMSizeOfTypeInBits(ctx->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(ctx->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s8)LLVMABIAlignmentOfType(ctx->assembly->llvm.TD, type->llvm_type);
}

void type_init_llvm_ptr(struct context *ctx, struct mir_type *type)
{
    struct mir_type *tmp = mir_deref_type(type);
    // Pointer to Type has no LLVM representation and cannot not be generated into IR.
    // if (!mir_type_has_llvm_representation(tmp)) return;
    bassert(tmp);
    bassert(tmp->llvm_type);
    type->llvm_type        = LLVMPointerType(tmp->llvm_type, 0);
    type->size_bits        = LLVMSizeOfTypeInBits(ctx->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(ctx->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s8)LLVMABIAlignmentOfType(ctx->assembly->llvm.TD, type->llvm_type);
}

void type_init_llvm_void(struct context *ctx, struct mir_type *type)
{
    type->alignment        = 0;
    type->size_bits        = 0;
    type->store_size_bytes = 0;
    type->llvm_type        = LLVMVoidTypeInContext(ctx->assembly->llvm.ctx);
}

void type_init_llvm_dummy(struct context *ctx, struct mir_type *type)
{
    struct mir_type *dummy = ctx->builtin_types->t_dummy_ptr;
    bassert(dummy);
    type->llvm_type        = dummy->llvm_type;
    type->size_bits        = dummy->size_bits;
    type->store_size_bytes = dummy->store_size_bytes;
    type->alignment        = dummy->alignment;
}

void type_init_llvm_null(struct context UNUSED(*ctx), struct mir_type *type)
{
    struct mir_type *tmp = type->data.null.base_type;
    bassert(tmp);
    bassert(tmp->llvm_type);
    type->llvm_type        = tmp->llvm_type;
    type->alignment        = tmp->alignment;
    type->size_bits        = tmp->size_bits;
    type->store_size_bytes = tmp->store_size_bytes;
}

void type_init_llvm_bool(struct context *ctx, struct mir_type *type)
{
    type->llvm_type        = LLVMIntTypeInContext(ctx->assembly->llvm.ctx, 1);
    type->size_bits        = LLVMSizeOfTypeInBits(ctx->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(ctx->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s8)LLVMABIAlignmentOfType(ctx->assembly->llvm.TD, type->llvm_type);
}

static INLINE usize struct_split_fit(struct context  *ctx,
                                     struct mir_type *struct_type,
                                     u32              bound,
                                     u32             *start)
{
    s64 so     = vm_get_struct_elem_offset(ctx->assembly, struct_type, *start);
    u32 offset = 0;
    u32 size   = 0;
    u32 total  = 0;
    for (; *start < sarrlenu(struct_type->data.strct.members); ++(*start)) {
        offset = (u32)vm_get_struct_elem_offset(ctx->assembly, struct_type, *start) - (u32)so;
        size   = (u32)mir_get_struct_elem_type(struct_type, *start)->store_size_bytes;
        if (offset + size > bound) return bound;
        total = offset + size;
    }

    return total > 1 ? next_pow_2((u32)total) : total;
}

void type_init_llvm_fn(struct context *ctx, struct mir_type *type)
{
    struct mir_type *ret_type = type->data.fn.ret_type;

    LLVMTypeRef llvm_ret  = NULL;
    mir_args_t *args      = type->data.fn.args;
    const bool  has_ret   = ret_type;
    bool        has_byval = false;

    bassert(ret_type);
    if (has_ret && ret_type->kind == MIR_TYPE_TYPE) {
        return;
    }

    llvm_types_t llvm_args = SARR_ZERO;
    if (has_ret) {
        if (ctx->assembly->target->reg_split && mir_is_composit_type(ret_type) &&
            ret_type->store_size_bytes >= 16) {
            setflag(type->data.fn.flags, MIR_TYPE_FN_FLAG_HAS_SRET);
            sarrput(&llvm_args, LLVMPointerType(ret_type->llvm_type, 0));
            llvm_ret = LLVMVoidTypeInContext(ctx->assembly->llvm.ctx);
        } else {
            llvm_ret = ret_type->llvm_type;
        }
    } else {
        llvm_ret = LLVMVoidTypeInContext(ctx->assembly->llvm.ctx);
    }

    bassert(llvm_ret);

    for (usize i = 0; i < sarrlenu(args); ++i) {
        struct mir_arg *arg = sarrpeek(args, i);
        arg->llvm_index     = (u32)sarrlenu(&llvm_args);

        // Composit types.
        if (ctx->assembly->target->reg_split && mir_is_composit_type(arg->type)) {
            LLVMContextRef llvm_cnt = ctx->assembly->llvm.ctx;

            u32   start = 0;
            usize low   = 0;
            usize high  = 0;

            if (!has_byval) has_byval = true;

            low = struct_split_fit(ctx, arg->type, sizeof(usize), &start);

            if (start < sarrlenu(arg->type->data.strct.members))
                high = struct_split_fit(ctx, arg->type, sizeof(usize), &start);

            if (start < sarrlenu(arg->type->data.strct.members)) {
                arg->llvm_easgm = LLVM_EASGM_BYVAL;

                bassert(arg->type->llvm_type);
                sarrput(&llvm_args, LLVMPointerType(arg->type->llvm_type, 0));
            } else {
                switch (low) {
                case 1:
                    arg->llvm_easgm = LLVM_EASGM_8;
                    sarrput(&llvm_args, LLVMInt8TypeInContext(llvm_cnt));
                    break;
                case 2:
                    arg->llvm_easgm = LLVM_EASGM_16;
                    sarrput(&llvm_args, LLVMInt16TypeInContext(llvm_cnt));
                    break;
                case 4:
                    arg->llvm_easgm = LLVM_EASGM_32;
                    sarrput(&llvm_args, LLVMInt32TypeInContext(llvm_cnt));
                    break;
                case 8: {
                    switch (high) {
                    case 0:
                        arg->llvm_easgm = LLVM_EASGM_64;
                        sarrput(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                        break;
                    case 1:
                        arg->llvm_easgm = LLVM_EASGM_64_8;
                        sarrput(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                        sarrput(&llvm_args, LLVMInt8TypeInContext(llvm_cnt));
                        break;
                    case 2:
                        arg->llvm_easgm = LLVM_EASGM_64_16;
                        sarrput(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                        sarrput(&llvm_args, LLVMInt16TypeInContext(llvm_cnt));
                        break;
                    case 4:
                        arg->llvm_easgm = LLVM_EASGM_64_32;
                        sarrput(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                        sarrput(&llvm_args, LLVMInt32TypeInContext(llvm_cnt));
                        break;
                    case 8:
                        arg->llvm_easgm = LLVM_EASGM_64_64;
                        sarrput(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                        sarrput(&llvm_args, LLVMInt64TypeInContext(llvm_cnt));
                        break;
                    default:
                        bassert(false);
                        break;
                    }
                    break;
                }
                default:
                    bassert(false);
                    break;
                }
            }
        } else {
            bassert(arg->type->llvm_type);
            sarrput(&llvm_args, arg->type->llvm_type);
        }
    }

    type->llvm_type =
        LLVMFunctionType(llvm_ret, sarrdata(&llvm_args), (unsigned)sarrlenu(&llvm_args), false);
    type->alignment        = __alignof(struct mir_fn *);
    type->size_bits        = sizeof(struct mir_fn *) * 8;
    type->store_size_bytes = sizeof(struct mir_fn *);
    if (has_byval) setflag(type->data.fn.flags, MIR_TYPE_FN_FLAG_HAS_BYVAL);
    sarrfree(&llvm_args);
}

void type_init_llvm_array(struct context *ctx, struct mir_type *type)
{
    LLVMTypeRef llvm_elem_type = type->data.array.elem_type->llvm_type;
    bassert(llvm_elem_type);
    const unsigned int len = (const unsigned int)type->data.array.len;

    type->llvm_type        = LLVMArrayType(llvm_elem_type, len);
    type->size_bits        = LLVMSizeOfTypeInBits(ctx->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(ctx->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s8)LLVMABIAlignmentOfType(ctx->assembly->llvm.TD, type->llvm_type);
}

void type_init_llvm_struct(struct context *ctx, struct mir_type *type)
{
    if (type->data.strct.is_incomplete) {
        bassert(type->user_id && "Missing user id for incomplete struct type.");
        type->llvm_type = LLVMStructCreateNamed(ctx->assembly->llvm.ctx, type->user_id->str);
        return;
    }

    mir_members_t *members = type->data.strct.members;
    bassert(members);

    const bool is_packed = type->data.strct.is_packed;
    const bool is_union  = type->data.strct.is_union;
    bassert(sarrlen(members) > 0);

    llvm_types_t llvm_members = SARR_ZERO;

    // When structure is union we have to find biggest member and use only found one since LLVM
    // has no explicit union representation. We select biggest one. We have to consider better
    // selection later due to correct union alignment.
    struct mir_member *union_member = NULL;

    for (usize i = 0; i < sarrlenu(members); ++i) {
        struct mir_member *member = sarrpeek(members, i);
        bassert(member->type->llvm_type);
        if (is_union) {
            if (!union_member) {
                union_member = member;
                continue;
            }
            if (member->type->store_size_bytes > union_member->type->store_size_bytes)
                union_member = member;
        } else {
            sarrput(&llvm_members, member->type->llvm_type);
        }
    }

    if (union_member) sarrput(&llvm_members, union_member->type->llvm_type);

    // named structure type
    if (type->user_id) {
        if (type->llvm_type == NULL) {
            // Create new named type only if it's not already created (by incomplete
            // type declaration).
            type->llvm_type = LLVMStructCreateNamed(ctx->assembly->llvm.ctx, type->user_id->str);
        }

        LLVMStructSetBody(
            type->llvm_type, sarrdata(&llvm_members), (unsigned)sarrlenu(&llvm_members), is_packed);
    } else {
        type->llvm_type = LLVMStructTypeInContext(ctx->assembly->llvm.ctx,
                                                  sarrdata(&llvm_members),
                                                  (unsigned)sarrlenu(&llvm_members),
                                                  is_packed);
    }
    type->size_bits        = LLVMSizeOfTypeInBits(ctx->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(ctx->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s8)LLVMABIAlignmentOfType(ctx->assembly->llvm.TD, type->llvm_type);
    sarrfree(&llvm_members);

    for (usize i = 0; i < sarrlenu(members); ++i) {
        struct mir_member *member = sarrpeek(members, i);
        // set offsets for members
        // Note: Union members has 0 offset.
        member->offset_bytes = (s32)vm_get_struct_elem_offset(ctx->assembly, type, (u32)i);
    }
}

void type_init_llvm_enum(struct context *ctx, struct mir_type *type)
{
    struct mir_type *base_type = type->data.enm.base_type;
    bassert(base_type->kind == MIR_TYPE_INT);
    LLVMTypeRef llvm_base_type = base_type->llvm_type;
    bassert(llvm_base_type);

    type->llvm_type        = llvm_base_type;
    type->size_bits        = LLVMSizeOfTypeInBits(ctx->assembly->llvm.TD, type->llvm_type);
    type->store_size_bytes = LLVMStoreSizeOfType(ctx->assembly->llvm.TD, type->llvm_type);
    type->alignment        = (s8)LLVMABIAlignmentOfType(ctx->assembly->llvm.TD, type->llvm_type);
}

static INLINE void push_var(struct context *ctx, struct mir_var *var)
{
    bassert(var);
    if (var->is_global) return;
    struct mir_fn *fn = ast_current_fn(ctx);
    bassert(fn);
    arrput(fn->variables, var);
}

struct mir_var *create_var(struct context      *ctx,
                           struct ast          *decl_node,
                           struct scope        *scope,
                           struct id           *id,
                           struct mir_type     *alloc_type,
                           bool                 is_mutable,
                           bool                 is_global,
                           bool                 is_comptime,
                           u32                  flags,
                           enum builtin_id_kind builtin_id)
{
    bassert(id);
    struct mir_var *tmp    = arena_safe_alloc(&ctx->assembly->arenas.mir.var);
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
    push_var(ctx, tmp);
    return tmp;
}

struct mir_var *create_var_impl(struct context  *ctx,
                                struct ast      *decl_node, // Optional
                                const char      *name,
                                struct mir_type *alloc_type,
                                bool             is_mutable,
                                bool             is_global,
                                bool             is_comptime)
{
    bassert(name);
    struct mir_var *tmp    = arena_safe_alloc(&ctx->assembly->arenas.mir.var);
    tmp->value.type        = alloc_type;
    tmp->value.is_comptime = is_comptime;
    tmp->is_mutable        = is_mutable;
    tmp->is_global         = is_global;
    tmp->ref_count         = 1;
    tmp->linkage_name      = name;
    tmp->decl_node         = decl_node;
    tmp->is_implicit       = true;
    tmp->emit_llvm         = true;
    push_var(ctx, tmp);
    return tmp;
}

struct mir_fn *create_fn(struct context            *ctx,
                         struct ast                *node,
                         struct id                 *id,
                         const char                *linkage_name,
                         u32                        flags,
                         struct mir_instr_fn_proto *prototype,
                         bool                       is_global,
                         enum builtin_id_kind       builtin_id)
{
    struct mir_fn *tmp = arena_safe_alloc(&ctx->assembly->arenas.mir.fn);
    BL_MAGIC_SET(tmp);
    tmp->linkage_name = linkage_name;
    tmp->id           = id;
    tmp->flags        = flags;
    tmp->decl_node    = node;
    tmp->prototype    = &prototype->base;
    tmp->is_global    = is_global;
    tmp->builtin_id   = builtin_id;
    arrsetcap(tmp->variables, 8);
    return tmp;
}

struct mir_fn_group *
create_fn_group(struct context *ctx, struct ast *decl_node, mir_fns_t *variants)
{
    bassert(decl_node);
    bassert(variants);
    struct mir_fn_group *tmp = arena_safe_alloc(&ctx->assembly->arenas.mir.fn_group);
    BL_MAGIC_SET(tmp);
    tmp->decl_node = decl_node;
    tmp->variants  = variants;
    return tmp;
}

struct mir_fn_poly_recipe *create_fn_poly_recipe(struct context *ctx, struct ast *ast_lit_fn)
{
    bassert(ast_lit_fn && ast_lit_fn->kind == AST_EXPR_LIT_FN);
    struct mir_fn_poly_recipe *tmp = arena_safe_alloc(&ctx->assembly->arenas.mir.fn_poly);
    BL_MAGIC_SET(tmp);
    tmp->ast_lit_fn = ast_lit_fn;
    return tmp;
}

struct mir_member *create_member(struct context  *ctx,
                                 struct ast      *node,
                                 struct id       *id,
                                 s64              index,
                                 struct mir_type *type)
{
    struct mir_member *tmp = arena_safe_alloc(&ctx->assembly->arenas.mir.member);
    BL_MAGIC_SET(tmp);
    tmp->decl_node = node;
    tmp->id        = id;
    tmp->index     = index;
    tmp->type      = type;
    return tmp;
}

struct mir_arg *create_arg(struct context   *ctx,
                           struct ast       *node,
                           struct id        *id,
                           struct scope     *scope,
                           struct mir_type  *type,
                           struct mir_instr *value)
{
    struct mir_arg *tmp = arena_safe_alloc(&ctx->assembly->arenas.mir.arg);
    tmp->decl_node      = node;
    tmp->id             = id;
    tmp->type           = type;
    tmp->decl_scope     = scope;
    tmp->value          = value;
    return tmp;
}

struct mir_variant *
create_variant(struct context *ctx, struct id *id, struct mir_type *value_type, const u64 value)
{
    struct mir_variant *tmp = arena_safe_alloc(&ctx->assembly->arenas.mir.variant);
    tmp->id                 = id;
    tmp->value_type         = value_type;
    tmp->value              = value;
    return tmp;
}

// instructions
void append_current_block(struct context *ctx, struct mir_instr *instr)
{
    bassert(instr);
    struct mir_instr_block *block = ast_current_block(ctx);
    bassert(block);
    if (is_block_terminated(block)) {
        // Append this instruction into unreachable block if current block was terminated
        // already. Unreachable block will never be generated into LLVM and compiler can
        // complain later about this and give hit to the user.
        block = append_block(ctx, block->owner_fn, ".unreachable");
        set_current_block(ctx, block);
    }

    instr->owner_block = block;
    instr->prev        = block->last_instr;
    if (!block->entry_instr) block->entry_instr = instr;
    if (instr->prev) instr->prev->next = instr;
    block->last_instr = instr;
}

struct mir_instr *
insert_instr_cast(struct context *ctx, struct mir_instr *src, struct mir_type *to_type)
{
    struct mir_instr_cast *tmp = create_instr(ctx, MIR_INSTR_CAST, src->node);
    tmp->base.value.type       = to_type;
    setflag(tmp->base.flags, MIR_IS_IMPLICIT);
    tmp->expr = src;
    tmp->op   = MIR_CAST_INVALID;
    ref_instr(&tmp->base);

    insert_instr_after(src, &tmp->base);
    return &tmp->base;
}

struct mir_instr *insert_instr_addrof(struct context *ctx, struct mir_instr *src)
{
    struct mir_instr *tmp = create_instr_addrof(ctx, src->node, src);
    setflag(tmp->flags, MIR_IS_IMPLICIT);
    insert_instr_after(src, tmp);
    return tmp;
}

struct mir_instr *insert_instr_toany(struct context *ctx, struct mir_instr *expr)
{
    bassert(ctx->builtin_types->is_any_ready &&
            "All 'Any' related types must be ready before this!");

    struct mir_instr_to_any *tmp = create_instr(ctx, MIR_INSTR_TOANY, expr->node);
    tmp->base.value.type         = ctx->builtin_types->t_Any_ptr;
    setflag(tmp->base.flags, MIR_IS_IMPLICIT);
    tmp->expr = expr;
    ref_instr(&tmp->base);

    insert_instr_after(expr, &tmp->base);
    return &tmp->base;
}

struct mir_instr *insert_instr_load(struct context *ctx, struct mir_instr *src)
{
    bassert(src);
    bassert(src->value.type);
    bassert(src->value.type->kind == MIR_TYPE_PTR);
    struct mir_instr_load *tmp = create_instr(ctx, MIR_INSTR_LOAD, src->node);
    setflag(tmp->base.flags, MIR_IS_IMPLICIT);
    tmp->src = src;

    ref_instr(&tmp->base);
    insert_instr_after(src, &tmp->base);

    return &tmp->base;
}

enum mir_cast_op get_cast_op(struct mir_type *from, struct mir_type *to)
{
    bassert(from);
    bassert(to);
    const usize fsize = from->store_size_bytes;
    const usize tsize = to->store_size_bytes;

    if (type_cmp(from, to)) return MIR_CAST_NONE;

    // Allow casting of anything to polymorph type. Polymorph types should exist only in polymorph
    // function argument list and should not produce any executable code directly; such casting is
    // allowed only due to analyze of valid concepts like defautl argument values set for deduced
    // polymorph slave-typed arguments.
    if (to->kind == MIR_TYPE_POLY) return MIR_CAST_NONE;
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

void *create_instr(struct context *ctx, enum mir_instr_kind kind, struct ast *node)
{
    struct mir_instr *tmp = arena_safe_alloc(&ctx->assembly->arenas.mir.instr);
    tmp->value.data       = (vm_stack_ptr_t)&tmp->value._tmp;
    tmp->kind             = kind;
    tmp->node             = node;
    tmp->id               = _id_counter++;
    return tmp;
}

struct mir_instr *
create_instr_call_comptime(struct context *ctx, struct ast *node, struct mir_instr *fn)
{
    bassert(fn && fn->kind == MIR_INSTR_FN_PROTO);
    struct mir_instr_call *tmp  = create_instr(ctx, MIR_INSTR_CALL, node);
    tmp->base.value.addr_mode   = MIR_VAM_LVALUE_CONST;
    tmp->base.value.is_comptime = true;
    tmp->base.ref_count         = 2;
    tmp->callee                 = ref_instr(fn);
    return &tmp->base;
}

struct mir_instr *create_instr_compound(struct context   *ctx,
                                        struct ast       *node,
                                        struct mir_instr *type,
                                        mir_instrs_t     *values,
                                        bool              is_multiple_return_value)
{
    for (usize i = 0; i < sarrlenu(values); ++i) {
        struct mir_instr *value = sarrpeek(values, i);
        ref_instr(value);
    }
    struct mir_instr_compound *tmp = create_instr(ctx, MIR_INSTR_COMPOUND, node);
    tmp->base.value.addr_mode      = MIR_VAM_RVALUE;
    tmp->type                      = ref_instr(type);
    tmp->values                    = values;
    tmp->is_naked                  = true;
    tmp->is_multiple_return_value  = is_multiple_return_value;
    return &tmp->base;
}

struct mir_instr *create_instr_compound_impl(struct context  *ctx,
                                             struct ast      *node,
                                             struct mir_type *type,
                                             mir_instrs_t    *values)
{
    struct mir_instr *tmp = create_instr_compound(ctx, node, NULL, values, false);
    tmp->value.type       = type;
    setflag(tmp->flags, MIR_IS_IMPLICIT);
    return tmp;
}

struct mir_instr *
create_instr_type_info(struct context *ctx, struct ast *node, struct mir_instr *expr)
{
    struct mir_instr_type_info *tmp = create_instr(ctx, MIR_INSTR_TYPE_INFO, node);
    tmp->expr                       = ref_instr(expr);
    return &tmp->base;
}

struct mir_instr *create_instr_elem_ptr(struct context   *ctx,
                                        struct ast       *node,
                                        struct mir_instr *arr_ptr,
                                        struct mir_instr *index)
{
    bassert(arr_ptr && index);
    struct mir_instr_elem_ptr *tmp = create_instr(ctx, MIR_INSTR_ELEM_PTR, node);
    tmp->arr_ptr                   = ref_instr(arr_ptr);
    tmp->index                     = ref_instr(index);
    return &tmp->base;
}

struct mir_instr *create_instr_member_ptr(struct context      *ctx,
                                          struct ast          *node,
                                          struct mir_instr    *target_ptr,
                                          struct ast          *member_ident,
                                          struct scope_entry  *scope_entry,
                                          enum builtin_id_kind builtin_id)
{
    struct mir_instr_member_ptr *tmp = create_instr(ctx, MIR_INSTR_MEMBER_PTR, node);
    tmp->target_ptr                  = ref_instr(target_ptr);
    tmp->member_ident                = member_ident;
    tmp->scope_entry                 = scope_entry;
    tmp->builtin_id                  = builtin_id;
    return &tmp->base;
}

struct mir_instr *create_instr_addrof(struct context *ctx, struct ast *node, struct mir_instr *src)
{
    struct mir_instr_addrof *tmp = create_instr(ctx, MIR_INSTR_ADDROF, node);
    tmp->src                     = ref_instr(src);
    return &tmp->base;
}

struct mir_instr *
create_instr_decl_direct_ref(struct context *ctx, struct ast *node, struct mir_instr *ref)
{
    bassert(ref);
    struct mir_instr_decl_direct_ref *tmp = create_instr(ctx, MIR_INSTR_DECL_DIRECT_REF, node);
    tmp->ref                              = ref_instr(ref);
    return &tmp->base;
}

struct mir_instr *
create_instr_const_int(struct context *ctx, struct ast *node, struct mir_type *type, u64 val)
{
    struct mir_instr_const *tmp = create_instr(ctx, MIR_INSTR_CONST, node);
    tmp->base.value.type        = type;
    tmp->base.value.addr_mode   = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime = true;
    tmp->volatile_type          = true;
    MIR_CEV_WRITE_AS(u64, &tmp->base.value, val);
    return &tmp->base;
}

struct mir_instr *
create_instr_const_type(struct context *ctx, struct ast *node, struct mir_type *type)
{
    struct mir_instr *tmp  = create_instr(ctx, MIR_INSTR_CONST, node);
    tmp->value.type        = ctx->builtin_types->t_type;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    tmp->value.is_comptime = true;
    MIR_CEV_WRITE_AS(struct mir_type *, &tmp->value, type);
    return tmp;
}

struct mir_instr *create_instr_const_float(struct context *ctx, struct ast *node, float val)
{
    struct mir_instr *tmp  = create_instr(ctx, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = ctx->builtin_types->t_f32;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    MIR_CEV_WRITE_AS(float, &tmp->value, val);
    return tmp;
}

struct mir_instr *create_instr_const_double(struct context *ctx, struct ast *node, double val)
{
    struct mir_instr *tmp  = create_instr(ctx, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = ctx->builtin_types->t_f64;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    MIR_CEV_WRITE_AS(double, &tmp->value, val);
    return tmp;
}

struct mir_instr *create_instr_const_bool(struct context *ctx, struct ast *node, bool val)
{
    struct mir_instr *tmp  = create_instr(ctx, MIR_INSTR_CONST, node);
    tmp->value.type        = ctx->builtin_types->t_bool;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    tmp->value.is_comptime = true;
    MIR_CEV_WRITE_AS(bool, &tmp->value, val);
    return tmp;
}

struct mir_instr *create_instr_const_ptr(struct context  *ctx,
                                         struct ast      *node,
                                         struct mir_type *type,
                                         vm_stack_ptr_t   ptr)
{
    bassert(mir_is_pointer_type(type) && "Expected pointer type!");
    struct mir_instr *tmp  = create_instr(ctx, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = type;
    tmp->value.addr_mode   = MIR_VAM_LVALUE_CONST;
    MIR_CEV_WRITE_AS(vm_stack_ptr_t, &tmp->value, ptr);
    return tmp;
}

struct mir_instr *
create_instr_call_loc(struct context *ctx, struct ast *node, struct location *call_location)
{
    struct mir_instr_call_loc *tmp = create_instr(ctx, MIR_INSTR_CALL_LOC, node);
    tmp->base.value.addr_mode      = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime    = true;
    tmp->call_location             = call_location;
    return &tmp->base;
}

struct mir_instr_block *create_block(struct context *ctx, const char *name)
{
    bassert(name);
    struct mir_instr_block *tmp = create_instr(ctx, MIR_INSTR_BLOCK, NULL);
    tmp->base.value.type        = ctx->builtin_types->t_void;
    tmp->name                   = name;
    tmp->owner_fn               = NULL;
    return tmp;
}

struct mir_instr_block *
append_block2(struct context UNUSED(*ctx), struct mir_fn *fn, struct mir_instr_block *block)
{
    bassert(block && fn);
    bassert(!block->owner_fn && "Block is already appended to function!");
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

struct mir_instr_block *append_block(struct context *ctx, struct mir_fn *fn, const char *name)
{
    bassert(fn && name);
    struct mir_instr_block *tmp = create_block(ctx, name);
    append_block2(ctx, fn, tmp);
    return tmp;
}

struct mir_instr_block *append_global_block(struct context *ctx, const char *name)
{
    struct mir_instr_block *tmp = create_instr(ctx, MIR_INSTR_BLOCK, NULL);
    tmp->base.value.type        = ctx->builtin_types->t_void;
    tmp->base.value.is_comptime = true;
    tmp->name                   = name;
    ref_instr(&tmp->base);
    push_into_gscope(ctx, &tmp->base);
    analyze_schedule(ctx, &tmp->base);
    return tmp;
}

struct mir_instr *append_instr_set_initializer(struct context   *ctx,
                                               struct ast       *node,
                                               mir_instrs_t     *dests,
                                               struct mir_instr *src)
{
    struct mir_instr_set_initializer *tmp = create_instr(ctx, MIR_INSTR_SET_INITIALIZER, node);
    tmp->base.value.type                  = ctx->builtin_types->t_void;
    tmp->base.value.is_comptime           = true;
    tmp->base.ref_count                   = NO_REF_COUNTING;
    tmp->dests                            = dests;
    tmp->src                              = ref_instr(src);
    for (usize i = 0; i < sarrlenu(tmp->dests); ++i) {
        struct mir_instr *dest = sarrpeek(tmp->dests, i);
        bassert(dest && dest->kind == MIR_INSTR_DECL_VAR && "Expected variable declaration!");
        ref_instr(dest);
        struct mir_instr_decl_var *dest_var = (struct mir_instr_decl_var *)dest;
        struct mir_var            *var      = dest_var->var;
        bassert(var && "Missing variable!");
        var->initializer_block = (struct mir_instr *)ast_current_block(ctx);
    }
    append_current_block(ctx, &tmp->base);
    struct mir_instr_block *block = ast_current_block(ctx);
    if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
    return &tmp->base;
}

struct mir_instr *
append_instr_set_initializer_impl(struct context *ctx, mir_instrs_t *dests, struct mir_instr *src)
{
    struct mir_instr *tmp = append_instr_set_initializer(ctx, NULL, dests, src);
    setflag(tmp->flags, MIR_IS_IMPLICIT);
    return tmp;
}

struct mir_instr *append_instr_type_fn(struct context   *ctx,
                                       struct ast       *node,
                                       struct mir_instr *ret_type,
                                       mir_instrs_t     *args,
                                       bool              is_polymorph)
{
    struct mir_instr_type_fn *tmp = create_instr(ctx, MIR_INSTR_TYPE_FN, node);
    tmp->base.value.type          = ctx->builtin_types->t_type;
    tmp->base.value.addr_mode     = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime   = true;
    tmp->ret_type                 = ret_type;
    tmp->args                     = args;
    tmp->is_polymorph             = is_polymorph;
    for (usize i = 0; i < sarrlenu(args); ++i) {
        struct mir_instr *it = sarrpeek(args, i);
        ref_instr(it);
    }
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_type_fn_group(struct context *ctx,
                                             struct ast     *node,
                                             struct id      *id,
                                             mir_instrs_t   *variants)
{
    bassert(variants);
    struct mir_instr_type_fn_group *tmp = create_instr(ctx, MIR_INSTR_TYPE_FN_GROUP, node);
    tmp->base.value.type                = ctx->builtin_types->t_type;
    tmp->base.value.addr_mode           = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime         = true;
    tmp->variants                       = variants;
    tmp->id                             = id;
    for (usize i = 0; i < sarrlenu(variants); ++i) {
        struct mir_instr *it = sarrpeek(variants, i);
        ref_instr(it);
    }
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_type_struct(struct context   *ctx,
                                           struct ast       *node,
                                           struct id        *id,
                                           struct mir_instr *fwd_decl,
                                           struct scope     *scope,
                                           mir_instrs_t     *members,
                                           bool              is_packed,
                                           bool              is_union,
                                           bool              is_multiple_return_type)
{
    struct mir_instr_type_struct *tmp = create_instr(ctx, MIR_INSTR_TYPE_STRUCT, node);
    tmp->base.value.type              = ctx->builtin_types->t_type;
    tmp->base.value.is_comptime       = true;
    tmp->base.value.addr_mode         = MIR_VAM_RVALUE;
    tmp->members                      = members;
    tmp->scope                        = scope;
    tmp->is_packed                    = is_packed;
    tmp->is_union                     = is_union;
    tmp->is_multiple_return_type      = is_multiple_return_type;

    tmp->id       = id;
    tmp->fwd_decl = fwd_decl;

    for (usize i = 0; i < sarrlenu(members); ++i) {
        struct mir_instr *it = sarrpeek(members, i);
        ref_instr(it);
    }

    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_type_enum(struct context   *ctx,
                                         struct ast       *node,
                                         struct id        *id,
                                         struct scope     *scope,
                                         mir_instrs_t     *variants,
                                         struct mir_instr *base_type,
                                         const bool        is_flags)
{
    struct mir_instr_type_enum *tmp = create_instr(ctx, MIR_INSTR_TYPE_ENUM, node);
    tmp->base.value.type            = ctx->builtin_types->t_type;
    tmp->base.value.is_comptime     = true;
    tmp->base.value.addr_mode       = MIR_VAM_RVALUE;
    tmp->variants                   = variants;
    tmp->scope                      = scope;
    tmp->id                         = id;
    tmp->base_type                  = base_type;
    tmp->is_flags                   = is_flags;

    for (usize i = 0; i < sarrlenu(variants); ++i) {
        struct mir_instr *it = sarrpeek(variants, i);
        ref_instr(it);
    }

    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *
append_instr_type_ptr(struct context *ctx, struct ast *node, struct mir_instr *type)
{
    struct mir_instr_type_ptr *tmp = create_instr(ctx, MIR_INSTR_TYPE_PTR, node);
    tmp->base.value.type           = ctx->builtin_types->t_type;
    tmp->base.value.addr_mode      = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime    = true;
    tmp->type                      = ref_instr(type);
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_type_poly(struct context *ctx, struct ast *node, struct id *T_id)
{
    bassert(T_id && "Missing id for polymorph type!");
    struct mir_instr_type_poly *tmp = create_instr(ctx, MIR_INSTR_TYPE_POLY, node);
    tmp->base.value.type            = ctx->builtin_types->t_type;
    tmp->base.value.addr_mode       = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime     = true;
    tmp->T_id                       = T_id;
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_type_array(struct context   *ctx,
                                          struct ast       *node,
                                          struct id        *id,
                                          struct mir_instr *elem_type,
                                          struct mir_instr *len)
{
    struct mir_instr_type_array *tmp = create_instr(ctx, MIR_INSTR_TYPE_ARRAY, node);
    tmp->base.value.type             = ctx->builtin_types->t_type;
    tmp->base.value.addr_mode        = MIR_VAM_LVALUE_CONST;
    tmp->base.value.is_comptime      = true;
    tmp->elem_type                   = ref_instr(elem_type);
    tmp->len                         = ref_instr(len);
    tmp->id                          = id;
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *
append_instr_type_slice(struct context *ctx, struct ast *node, struct mir_instr *elem_type)
{
    struct mir_instr_type_slice *tmp = create_instr(ctx, MIR_INSTR_TYPE_SLICE, node);
    tmp->base.value.type             = ctx->builtin_types->t_type;
    tmp->base.value.is_comptime      = true;
    tmp->elem_type                   = ref_instr(elem_type);
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *
append_instr_type_dynarr(struct context *ctx, struct ast *node, struct mir_instr *elem_type)
{
    struct mir_instr_type_slice *tmp = create_instr(ctx, MIR_INSTR_TYPE_DYNARR, node);
    tmp->base.value.type             = ctx->builtin_types->t_type;
    tmp->base.value.is_comptime      = true;
    tmp->elem_type                   = ref_instr(elem_type);
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *
append_instr_type_vargs(struct context *ctx, struct ast *node, struct mir_instr *elem_type)
{
    struct mir_instr_type_vargs *tmp = create_instr(ctx, MIR_INSTR_TYPE_VARGS, node);
    tmp->base.value.type             = ctx->builtin_types->t_type;
    tmp->base.value.is_comptime      = true;
    tmp->elem_type                   = ref_instr(elem_type);
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_arg(struct context *ctx, struct ast *node, unsigned i)
{
    struct mir_instr_arg *tmp = create_instr(ctx, MIR_INSTR_ARG, node);
    tmp->i                    = i;
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_unroll(struct context   *ctx,
                                      struct ast       *node,
                                      struct mir_instr *src,
                                      struct mir_instr *remove_src,
                                      s32               index)
{
    bassert(index >= 0);
    bassert(src);
    struct mir_instr_unroll *tmp = create_instr(ctx, MIR_INSTR_UNROLL, node);
    tmp->src                     = ref_instr(src);
    tmp->remove_src              = ref_instr(remove_src);
    tmp->index                   = index;
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *create_instr_phi(struct context *ctx, struct ast *node)
{
    struct mir_instr_phi *tmp = create_instr(ctx, MIR_INSTR_PHI, node);
    tmp->incoming_values      = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    tmp->incoming_blocks      = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    return &tmp->base;
}

struct mir_instr *append_instr_compound(struct context   *ctx,
                                        struct ast       *node,
                                        struct mir_instr *type,
                                        mir_instrs_t     *values,
                                        bool              is_multiple_return_value)
{
    struct mir_instr *tmp =
        create_instr_compound(ctx, node, type, values, is_multiple_return_value);
    append_current_block(ctx, tmp);
    return tmp;
}

struct mir_instr *append_instr_compound_impl(struct context  *ctx,
                                             struct ast      *node,
                                             struct mir_type *type,
                                             mir_instrs_t    *values)
{
    struct mir_instr *tmp = append_instr_compound(ctx, node, NULL, values, false);
    tmp->value.type       = type;
    setflag(tmp->flags, MIR_IS_IMPLICIT);
    return tmp;
}

struct mir_instr *create_default_value_for_type(struct context *ctx, struct mir_type *type)
{
    // Default initializer is only zero initialized compound expression known in compile time,
    // this is universal for every type.
    bassert(type && "Missing type for default zero initializer!");

    struct mir_instr *default_value = NULL;

    switch (type->kind) {
    case MIR_TYPE_ENUM: {
        // Use first enum variant as default.
        struct mir_type    *base_type = type->data.enm.base_type;
        struct mir_variant *variant   = sarrpeek(type->data.enm.variants, 0);
        default_value = create_instr_const_int(ctx, NULL, base_type, variant->value);
        break;
    }

    case MIR_TYPE_INT: {
        default_value = create_instr_const_int(ctx, NULL, type, 0);
        break;
    }

    case MIR_TYPE_REAL: {
        if (type->data.real.bitcount == 32) {
            default_value = create_instr_const_float(ctx, NULL, 0);
        } else {
            default_value = create_instr_const_double(ctx, NULL, 0);
        }
        break;
    }

    case MIR_TYPE_BOOL: {
        default_value = create_instr_const_bool(ctx, NULL, false);
        break;
    }

    default: {
        // Use zero initialized compound.
        struct mir_instr_compound *compound =
            (struct mir_instr_compound *)create_instr_compound_impl(ctx, NULL, type, NULL);
        compound->is_naked               = false;
        compound->is_zero_initialized    = true;
        compound->base.value.is_comptime = true;
        default_value                    = &compound->base;
        break;
    }
    }

    bassert(default_value && "Invalid default value!");
    return ref_instr(default_value);
}

struct mir_instr *append_instr_cast(struct context   *ctx,
                                    struct ast       *node,
                                    struct mir_instr *type,
                                    struct mir_instr *next)
{
    struct mir_instr_cast *tmp = create_instr(ctx, MIR_INSTR_CAST, node);
    tmp->base.value.addr_mode  = MIR_VAM_RVALUE;
    tmp->op                    = MIR_CAST_INVALID;
    tmp->type                  = ref_instr(type);
    tmp->expr                  = ref_instr(next);
    tmp->auto_cast             = type == NULL;

    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_sizeof(struct context *ctx, struct ast *node, struct mir_instr *expr)
{
    struct mir_instr_sizeof *tmp = create_instr(ctx, MIR_INSTR_SIZEOF, node);
    tmp->base.value.type         = ctx->builtin_types->t_usize;
    tmp->base.value.is_comptime  = true;
    tmp->base.value.addr_mode    = MIR_VAM_RVALUE;
    tmp->expr                    = ref_instr(expr);

    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *
append_instr_type_info(struct context *ctx, struct ast *node, struct mir_instr *expr)
{
    struct mir_instr *tmp  = create_instr_type_info(ctx, node, expr);
    tmp->value.is_comptime = true;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    append_current_block(ctx, tmp);
    return tmp;
}

struct mir_instr *append_instr_test_cases(struct context *ctx, struct ast *node)
{
    struct mir_instr *tmp  = create_instr(ctx, MIR_INSTR_TEST_CASES, node);
    tmp->value.is_comptime = true;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    append_current_block(ctx, tmp);
    return tmp;
}

struct mir_instr *
append_instr_alignof(struct context *ctx, struct ast *node, struct mir_instr *expr)
{
    struct mir_instr_alignof *tmp = create_instr(ctx, MIR_INSTR_ALIGNOF, node);
    tmp->base.value.type          = ctx->builtin_types->t_usize;
    tmp->base.value.is_comptime   = true;
    tmp->expr                     = ref_instr(expr);

    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_cond_br(struct context         *ctx,
                                       struct ast             *node,
                                       struct mir_instr       *cond,
                                       struct mir_instr_block *then_block,
                                       struct mir_instr_block *else_block,
                                       const bool              is_static)
{
    bassert(cond && then_block && else_block);
    ref_instr(&then_block->base);
    ref_instr(&else_block->base);
    struct mir_instr_cond_br *tmp = create_instr(ctx, MIR_INSTR_COND_BR, node);
    tmp->base.value.type          = ctx->builtin_types->t_void;
    tmp->base.ref_count           = NO_REF_COUNTING;
    tmp->cond                     = ref_instr(cond);
    tmp->then_block               = then_block;
    tmp->else_block               = else_block;
    tmp->is_static                = is_static;
    append_current_block(ctx, &tmp->base);
    struct mir_instr_block *block = ast_current_block(ctx);
    if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
    return &tmp->base;
}

struct mir_instr *
append_instr_br(struct context *ctx, struct ast *node, struct mir_instr_block *then_block)
{
    bassert(then_block);
    ref_instr(&then_block->base);
    struct mir_instr_br *tmp = create_instr(ctx, MIR_INSTR_BR, node);
    tmp->base.value.type     = ctx->builtin_types->t_void;
    tmp->base.ref_count      = NO_REF_COUNTING;
    tmp->then_block          = then_block;

    struct mir_instr_block *block = ast_current_block(ctx);

    append_current_block(ctx, &tmp->base);
    if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_switch(struct context         *ctx,
                                      struct ast             *node,
                                      struct mir_instr       *value,
                                      struct mir_instr_block *default_block,
                                      bool                    user_defined_default,
                                      mir_switch_cases_t     *cases)
{
    bassert(default_block);
    bassert(cases);
    bassert(value);

    ref_instr(&default_block->base);
    ref_instr(value);

    for (usize i = 0; i < sarrlenu(cases); ++i) {
        struct mir_switch_case *c = &sarrpeek(cases, i);
        ref_instr(&c->block->base);
        ref_instr(c->on_value);
    }

    struct mir_instr_switch *tmp  = create_instr(ctx, MIR_INSTR_SWITCH, node);
    tmp->base.value.type          = ctx->builtin_types->t_void;
    tmp->base.ref_count           = NO_REF_COUNTING;
    tmp->value                    = value;
    tmp->default_block            = default_block;
    tmp->cases                    = cases;
    tmp->has_user_defined_default = user_defined_default;

    struct mir_instr_block *block = ast_current_block(ctx);

    append_current_block(ctx, &tmp->base);
    if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_elem_ptr(struct context   *ctx,
                                        struct ast       *node,
                                        struct mir_instr *arr_ptr,
                                        struct mir_instr *index)
{
    struct mir_instr *tmp = create_instr_elem_ptr(ctx, node, arr_ptr, index);
    append_current_block(ctx, tmp);
    return tmp;
}

struct mir_instr *append_instr_member_ptr(struct context      *ctx,
                                          struct ast          *node,
                                          struct mir_instr    *target_ptr,
                                          struct ast          *member_ident,
                                          struct scope_entry  *scope_entry,
                                          enum builtin_id_kind builtin_id)
{
    struct mir_instr *tmp =
        create_instr_member_ptr(ctx, node, target_ptr, member_ident, scope_entry, builtin_id);

    append_current_block(ctx, tmp);
    return tmp;
}

struct mir_instr *append_instr_load(struct context *ctx, struct ast *node, struct mir_instr *src)
{
    struct mir_instr_load *tmp = create_instr(ctx, MIR_INSTR_LOAD, node);
    tmp->src                   = ref_instr(src);
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_addrof(struct context *ctx, struct ast *node, struct mir_instr *src)
{
    struct mir_instr *tmp = create_instr_addrof(ctx, node, src);
    append_current_block(ctx, tmp);
    return tmp;
}

struct mir_instr *append_instr_unreachable(struct context *ctx, struct ast *node)
{
    struct mir_instr_unreachable *tmp = create_instr(ctx, MIR_INSTR_UNREACHABLE, node);
    tmp->base.value.type              = ctx->builtin_types->t_void;
    tmp->base.ref_count               = NO_REF_COUNTING;
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_debugbreak(struct context *ctx, struct ast *node)
{
    struct mir_instr_debugbreak *tmp = create_instr(ctx, MIR_INSTR_DEBUGBREAK, node);
    tmp->base.value.type             = ctx->builtin_types->t_void;
    tmp->base.ref_count              = NO_REF_COUNTING;
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_fn_proto(struct context   *ctx,
                                        struct ast       *node,
                                        struct mir_instr *type,
                                        struct mir_instr *user_type,
                                        bool              schedule_analyze)
{
    struct mir_instr_fn_proto *tmp = create_instr(ctx, MIR_INSTR_FN_PROTO, node);
    tmp->base.value.addr_mode      = MIR_VAM_LVALUE_CONST;
    tmp->base.value.is_comptime    = true;
    tmp->type                      = type;
    tmp->user_type                 = user_type;
    tmp->base.ref_count            = NO_REF_COUNTING;
    tmp->pushed_for_analyze        = schedule_analyze;

    push_into_gscope(ctx, &tmp->base);

    if (schedule_analyze) analyze_schedule(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *
append_instr_fn_group(struct context *ctx, struct ast *node, mir_instrs_t *variants)
{
    bassert(variants);
    struct mir_instr_fn_group *tmp = create_instr(ctx, MIR_INSTR_FN_GROUP, node);
    tmp->base.value.addr_mode      = MIR_VAM_LVALUE_CONST;
    tmp->base.value.is_comptime    = true;
    tmp->base.ref_count            = NO_REF_COUNTING;
    tmp->variants                  = variants;
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_decl_ref(struct context     *ctx,
                                        struct ast         *node,
                                        struct unit        *parent_unit,
                                        struct id          *rid,
                                        struct scope       *scope,
                                        s32                 scope_layer,
                                        struct scope_entry *scope_entry)
{
    bassert(scope && rid);
    bassert(scope_layer >= 0);
    struct mir_instr_decl_ref *tmp = create_instr(ctx, MIR_INSTR_DECL_REF, node);
    tmp->scope_entry               = scope_entry;
    tmp->scope                     = scope;
    tmp->scope_layer               = scope_layer;
    tmp->rid                       = rid;
    tmp->parent_unit               = parent_unit;

    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *
append_instr_decl_direct_ref(struct context *ctx, struct ast *node, struct mir_instr *ref)
{
    struct mir_instr *tmp                          = create_instr_decl_direct_ref(ctx, node, ref);
    ((struct mir_instr_decl_direct_ref *)tmp)->ref = ref;
    append_current_block(ctx, tmp);
    return tmp;
}

struct mir_instr *append_instr_call(struct context   *ctx,
                                    struct ast       *node,
                                    struct mir_instr *callee,
                                    mir_instrs_t     *args,
                                    const bool        call_in_compile_time)
{
    bassert(callee);
    struct mir_instr_call *tmp  = create_instr(ctx, MIR_INSTR_CALL, node);
    tmp->base.value.addr_mode   = MIR_VAM_RVALUE;
    tmp->base.value.is_comptime = call_in_compile_time;
    tmp->args                   = args;
    tmp->callee                 = callee;
    tmp->call_in_compile_time   = call_in_compile_time;
    ref_instr(&tmp->base);
    // Callee must be referenced even if we call no-ref counted fn_proto instructions, because
    // sometimes callee is declaration reference pointing to variable containing pointer to some
    // function.
    ref_instr(callee);
    // reference all arguments
    for (usize i = 0; i < sarrlenu(args); ++i) {
        struct mir_instr *instr = sarrpeek(args, i);
        ref_instr(instr);
    }
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_decl_var(struct context      *ctx,
                                        struct ast          *node,
                                        struct id           *id,
                                        struct scope        *scope,
                                        struct mir_instr    *type,
                                        struct mir_instr    *init,
                                        bool                 is_mutable,
                                        u32                  flags,
                                        enum builtin_id_kind builtin_id)
{
    bassert(id && "Missing id.");
    bassert(scope && "Missing scope.");
    struct mir_instr_decl_var *tmp = create_instr(ctx, MIR_INSTR_DECL_VAR, node);
    tmp->base.value.type           = ctx->builtin_types->t_void;
    tmp->base.ref_count            = NO_REF_COUNTING;
    tmp->type                      = ref_instr(type);
    tmp->init                      = ref_instr(init);
    const bool is_global           = !scope_is_local(scope);
    tmp->var =
        create_var(ctx, node, scope, id, NULL, is_mutable, is_global, false, flags, builtin_id);
    if (is_global) {
        push_into_gscope(ctx, &tmp->base);
        analyze_schedule(ctx, &tmp->base);
    } else {
        append_current_block(ctx, &tmp->base);
    }
    SET_IS_NAKED_IF_COMPOUND(init, false);
    return &tmp->base;
}

struct mir_instr *create_instr_decl_var_impl(struct context   *ctx,
                                             struct ast       *node, // Optional
                                             const char       *name,
                                             struct mir_instr *type,
                                             struct mir_instr *init,
                                             bool              is_mutable,
                                             bool              is_global)
{
    struct mir_instr_decl_var *tmp = create_instr(ctx, MIR_INSTR_DECL_VAR, node);
    tmp->base.value.type           = ctx->builtin_types->t_void;
    tmp->base.ref_count            = NO_REF_COUNTING;
    tmp->type                      = ref_instr(type);
    tmp->init                      = ref_instr(init);
    tmp->var = create_var_impl(ctx, node, name, NULL, is_mutable, is_global, false);
    if (!init) setflag(tmp->var->flags, FLAG_NO_INIT);
    SET_IS_NAKED_IF_COMPOUND(init, false);
    return &tmp->base;
}

struct mir_instr *append_instr_decl_var_impl(struct context   *ctx,
                                             struct ast       *node, // Optional
                                             const char       *name,
                                             struct mir_instr *type,
                                             struct mir_instr *init,
                                             bool              is_mutable,
                                             bool              is_global)
{
    struct mir_instr *tmp =
        create_instr_decl_var_impl(ctx, node, name, type, init, is_mutable, is_global);
    if (is_global) {
        push_into_gscope(ctx, tmp);
        analyze_schedule(ctx, tmp);
    } else {
        append_current_block(ctx, tmp);
    }
    return tmp;
}

struct mir_instr *append_instr_decl_member(struct context   *ctx,
                                           struct ast       *node,
                                           struct mir_instr *type,
                                           mir_instrs_t     *tags)
{
    struct id *id = node ? &node->data.ident.id : NULL;
    return append_instr_decl_member_impl(ctx, node, id, type, tags);
}

struct mir_instr *append_instr_decl_member_impl(struct context   *ctx,
                                                struct ast       *node,
                                                struct id        *id,
                                                struct mir_instr *type,
                                                mir_instrs_t     *tags)
{
    struct mir_instr_decl_member *tmp = create_instr(ctx, MIR_INSTR_DECL_MEMBER, node);
    tmp->base.value.is_comptime       = true;
    tmp->base.value.type              = ctx->builtin_types->t_void;
    tmp->base.ref_count               = NO_REF_COUNTING;
    tmp->type                         = ref_instr(type);
    tmp->tags                         = tags;

    tmp->member = create_member(ctx, node, id, -1, NULL);

    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_decl_arg(struct context   *ctx,
                                        struct ast       *node,
                                        struct mir_instr *type,
                                        struct mir_instr *value)
{
    ref_instr(value);
    struct mir_instr_decl_arg *tmp = create_instr(ctx, MIR_INSTR_DECL_ARG, node);
    tmp->base.value.is_comptime    = true;
    tmp->base.value.type           = ctx->builtin_types->t_void;

    tmp->base.ref_count = NO_REF_COUNTING;
    tmp->type           = ref_instr(type);

    struct id *id = node ? &node->data.ident.id : NULL;
    tmp->arg      = create_arg(ctx, node, id, NULL, NULL, value);

    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_decl_variant(struct context     *ctx,
                                            struct ast         *node,
                                            struct mir_instr   *value,
                                            struct mir_instr   *base_type,
                                            struct mir_variant *prev_variant,
                                            const bool          is_flags)
{
    struct mir_instr_decl_variant *tmp = create_instr(ctx, MIR_INSTR_DECL_VARIANT, node);
    tmp->base.value.is_comptime        = true;
    tmp->base.value.type               = ctx->builtin_types->t_void;
    tmp->base.value.addr_mode          = MIR_VAM_LVALUE_CONST;
    tmp->base.ref_count                = NO_REF_COUNTING;
    tmp->value                         = value;
    tmp->base_type                     = base_type;
    tmp->prev_variant                  = prev_variant;
    tmp->is_flags                      = is_flags;

    bassert(node && node->kind == AST_IDENT);
    struct id *id = &node->data.ident.id;
    tmp->variant  = create_variant(ctx, id, NULL, 0);

    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

INLINE struct mir_instr *
append_instr_const_int(struct context *ctx, struct ast *node, struct mir_type *type, u64 val)
{
    struct mir_instr *tmp = create_instr_const_int(ctx, node, type, val);
    append_current_block(ctx, tmp);
    return tmp;
}

INLINE struct mir_instr *
append_instr_const_type(struct context *ctx, struct ast *node, struct mir_type *type)
{
    struct mir_instr *tmp = create_instr_const_type(ctx, node, type);
    append_current_block(ctx, tmp);
    return tmp;
}

INLINE struct mir_instr *append_instr_const_float(struct context *ctx, struct ast *node, float val)
{
    struct mir_instr *tmp = create_instr_const_float(ctx, node, val);
    append_current_block(ctx, tmp);
    return tmp;
}

INLINE struct mir_instr *
append_instr_const_double(struct context *ctx, struct ast *node, double val)
{
    struct mir_instr *tmp = create_instr_const_double(ctx, node, val);
    append_current_block(ctx, tmp);
    return tmp;
}

INLINE struct mir_instr *append_instr_const_bool(struct context *ctx, struct ast *node, bool val)
{
    struct mir_instr *tmp = create_instr_const_bool(ctx, node, val);
    append_current_block(ctx, tmp);
    return tmp;
}

struct mir_instr *append_instr_const_string(struct context *ctx, struct ast *node, const char *str)
{
    // Build up string as compound expression of lenght and pointer to data.
    mir_instrs_t *values = arena_safe_alloc(&ctx->assembly->arenas.sarr);

    struct mir_instr *len =
        create_instr_const_int(ctx, node, ctx->builtin_types->t_s64, strlen(str));
    struct mir_instr *ptr =
        create_instr_const_ptr(ctx, node, ctx->builtin_types->t_u8_ptr, (vm_stack_ptr_t)str);

    ANALYZE_INSTR_RQ(len);
    ANALYZE_INSTR_RQ(ptr);

    sarrput(values, len);
    sarrput(values, ptr);

    struct mir_instr_compound *compound = (struct mir_instr_compound *)append_instr_compound_impl(
        ctx, node, ctx->builtin_types->t_string, values);

    compound->is_naked               = false;
    compound->base.value.is_comptime = true;
    compound->base.value.addr_mode   = MIR_VAM_RVALUE;

    return &compound->base;
}

INLINE struct mir_instr *append_instr_const_char(struct context *ctx, struct ast *node, char c)
{
    struct mir_instr *tmp  = create_instr(ctx, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = ctx->builtin_types->t_u8;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    MIR_CEV_WRITE_AS(char, &tmp->value, c);
    append_current_block(ctx, tmp);
    return tmp;
}

INLINE struct mir_instr *append_instr_const_null(struct context *ctx, struct ast *node)
{
    struct mir_instr *tmp  = create_instr(ctx, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = create_type_null(ctx, ctx->builtin_types->t_u8_ptr);
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    MIR_CEV_WRITE_AS(void *, &tmp->value, NULL);
    append_current_block(ctx, tmp);
    return tmp;
}

INLINE struct mir_instr *append_instr_const_void(struct context *ctx, struct ast *node)
{
    struct mir_instr *tmp  = create_instr(ctx, MIR_INSTR_CONST, node);
    tmp->value.is_comptime = true;
    tmp->value.type        = ctx->builtin_types->t_void;
    tmp->value.addr_mode   = MIR_VAM_RVALUE;
    MIR_CEV_WRITE_AS(void *, &tmp->value, NULL);
    append_current_block(ctx, tmp);
    return tmp;
}

struct mir_instr *append_instr_ret(struct context *ctx, struct ast *node, struct mir_instr *value)
{
    if (value) ref_instr(value);

    struct mir_instr_ret *tmp = create_instr(ctx, MIR_INSTR_RET, node);
    tmp->base.value.type      = ctx->builtin_types->t_void;
    tmp->base.value.addr_mode = MIR_VAM_RVALUE;
    tmp->base.ref_count       = NO_REF_COUNTING;
    tmp->value                = value;
    append_current_block(ctx, &tmp->base);
    struct mir_instr_block *block = ast_current_block(ctx);
    if (!is_block_terminated(block)) terminate_block(block, &tmp->base);
    struct mir_fn *fn = block->owner_fn;
    bassert(fn);
    fn->terminal_instr = tmp;
    return &tmp->base;
}

struct mir_instr *append_instr_store(struct context   *ctx,
                                     struct ast       *node,
                                     struct mir_instr *src,
                                     struct mir_instr *dest)
{
    bassert(src && dest);
    struct mir_instr_store *tmp = create_instr(ctx, MIR_INSTR_STORE, node);
    tmp->base.value.type        = ctx->builtin_types->t_void;
    tmp->base.ref_count         = NO_REF_COUNTING;
    tmp->src                    = ref_instr(src);
    tmp->dest                   = ref_instr(dest);
    SET_IS_NAKED_IF_COMPOUND(src, false);
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *append_instr_binop(struct context   *ctx,
                                     struct ast       *node,
                                     struct mir_instr *lhs,
                                     struct mir_instr *rhs,
                                     enum binop_kind   op)
{
    bassert(lhs && rhs);
    struct mir_instr_binop *tmp = create_instr(ctx, MIR_INSTR_BINOP, node);
    tmp->lhs                    = ref_instr(lhs);
    tmp->rhs                    = ref_instr(rhs);
    tmp->op                     = op;
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *
append_instr_unop(struct context *ctx, struct ast *node, struct mir_instr *instr, enum unop_kind op)
{
    bassert(instr);
    struct mir_instr_unop *tmp = create_instr(ctx, MIR_INSTR_UNOP, node);
    tmp->expr                  = ref_instr(instr);
    tmp->op                    = op;
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

struct mir_instr *create_instr_vargs_impl(struct context  *ctx,
                                          struct ast      *node,
                                          struct mir_type *type,
                                          mir_instrs_t    *values)
{
    bassert(type);
    struct mir_instr_vargs *tmp = create_instr(ctx, MIR_INSTR_VARGS, node);
    tmp->type                   = type;
    tmp->values                 = values;
    return &tmp->base;
}

struct mir_instr *append_instr_call_loc(struct context *ctx, struct ast *node)
{
    struct mir_instr *tmp = create_instr_call_loc(ctx, node, NULL);
    append_current_block(ctx, tmp);
    return tmp;
}

struct mir_instr *append_instr_msg(struct context *ctx, struct ast *node)
{
    struct mir_instr_msg *tmp = create_instr(ctx, MIR_INSTR_MSG, node);
    tmp->kind                 = node->data.msg.kind;
    tmp->text                 = node->data.msg.text;
    append_current_block(ctx, &tmp->base);
    return &tmp->base;
}

// analyze
void erase_instr_tree(struct mir_instr *instr, bool keep_root, bool force)
{
    if (!instr) return;
    instrs_t queue = SARR_ZERO;
    sarrput(&queue, instr);
    struct mir_instr *top;
    while (sarrlenu(&queue)) {
        top = sarrpop(&queue);
        if (!top) continue;

        bassert(isflag(top->flags, MIR_IS_ANALYZED) && "Trying to erase not analyzed instruction.");
        if (!force) {
            if (top->ref_count == NO_REF_COUNTING) continue;
            if (top->ref_count > 0) continue;
        }

        switch (top->kind) {
        case MIR_INSTR_COMPOUND: {
            struct mir_instr_compound *compound = (struct mir_instr_compound *)top;
            if (compound->is_zero_initialized) break;
            for (usize i = 0; i < sarrlenu(compound->values); ++i) {
                struct mir_instr *it = sarrpeek(compound->values, i);
                unref_instr(it);
                sarrput(&queue, it);
            }
            break;
        }

        case MIR_INSTR_BINOP: {
            struct mir_instr_binop *binop = (struct mir_instr_binop *)top;
            unref_instr(binop->lhs);
            unref_instr(binop->rhs);
            sarrput(&queue, binop->rhs);
            sarrput(&queue, binop->lhs);
            break;
        }

        case MIR_INSTR_LOAD: {
            struct mir_instr_load *load = (struct mir_instr_load *)top;
            unref_instr(load->src);
            sarrput(&queue, load->src);
            break;
        }

        case MIR_INSTR_ALIGNOF: {
            struct mir_instr_alignof *alof = (struct mir_instr_alignof *)top;
            unref_instr(alof->expr);
            sarrput(&queue, alof->expr);
            break;
        }

        case MIR_INSTR_SIZEOF: {
            struct mir_instr_sizeof *szof = (struct mir_instr_sizeof *)top;
            unref_instr(szof->expr);
            sarrput(&queue, szof->expr);
            break;
        }

        case MIR_INSTR_ELEM_PTR: {
            struct mir_instr_elem_ptr *ep = (struct mir_instr_elem_ptr *)top;
            unref_instr(ep->arr_ptr);
            unref_instr(ep->index);
            sarrput(&queue, ep->arr_ptr);
            sarrput(&queue, ep->index);
            break;
        }

        case MIR_INSTR_MEMBER_PTR: {
            struct mir_instr_member_ptr *mp = (struct mir_instr_member_ptr *)top;
            unref_instr(mp->target_ptr);
            sarrput(&queue, mp->target_ptr);
            break;
        }

        case MIR_INSTR_TYPE_INFO: {
            struct mir_instr_type_info *info = (struct mir_instr_type_info *)top;
            unref_instr(info->expr);
            sarrput(&queue, info->expr);
            break;
        }

        case MIR_INSTR_CAST: {
            struct mir_instr_cast *cast = (struct mir_instr_cast *)top;
            unref_instr(cast->expr);
            unref_instr(cast->type);
            sarrput(&queue, cast->expr);
            sarrput(&queue, cast->type);
            break;
        }

        case MIR_INSTR_CALL: {
            struct mir_instr_call *call = (struct mir_instr_call *)top;
            for (usize i = 0; i < sarrlenu(call->args); ++i) {
                struct mir_instr *it = sarrpeek(call->args, i);
                unref_instr(it);
                sarrput(&queue, it);
            }
            break;
        }

        case MIR_INSTR_ADDROF: {
            struct mir_instr_addrof *addrof = (struct mir_instr_addrof *)top;
            unref_instr(addrof->src);
            sarrput(&queue, addrof->src);
            break;
        }

        case MIR_INSTR_UNOP: {
            struct mir_instr_unop *unop = (struct mir_instr_unop *)top;
            unref_instr(unop->expr);
            sarrput(&queue, unop->expr);
            break;
        }

        case MIR_INSTR_TYPE_PTR: {
            struct mir_instr_type_ptr *tp = (struct mir_instr_type_ptr *)top;
            unref_instr(tp->type);
            sarrput(&queue, tp->type);
            break;
        }

        case MIR_INSTR_TYPE_ENUM: {
            struct mir_instr_type_enum *te = (struct mir_instr_type_enum *)top;
            unref_instr(te->base_type);
            sarrput(&queue, te->base_type);

            for (usize i = 0; i < sarrlenu(te->variants); ++i) {
                struct mir_instr *it = sarrpeek(te->variants, i);
                unref_instr(it);
                sarrput(&queue, it);
            }
            break;
        }

        case MIR_INSTR_TYPE_FN: {
            struct mir_instr_type_fn *tf = (struct mir_instr_type_fn *)top;
            unref_instr(tf->ret_type);
            sarrput(&queue, tf->ret_type);

            for (usize i = 0; i < sarrlenu(tf->args); ++i) {
                struct mir_instr *it = sarrpeek(tf->args, i);
                unref_instr(it);
                sarrput(&queue, it);
            }
            break;
        }

        case MIR_INSTR_TYPE_FN_GROUP: {
            struct mir_instr_type_fn_group *group = (struct mir_instr_type_fn_group *)top;
            bassert(group->variants);
            for (usize i = 0; i < sarrlenu(group->variants); ++i) {
                struct mir_instr *it = sarrpeek(group->variants, i);
                unref_instr(it);
                sarrput(&queue, it);
            }
            break;
        }

        case MIR_INSTR_TYPE_VARGS: {
            struct mir_instr_type_vargs *vargs = (struct mir_instr_type_vargs *)top;
            unref_instr(vargs->elem_type);
            sarrput(&queue, vargs->elem_type);
            break;
        }

        case MIR_INSTR_TYPE_ARRAY: {
            struct mir_instr_type_array *ta = (struct mir_instr_type_array *)top;
            unref_instr(ta->elem_type);
            unref_instr(ta->len);
            sarrput(&queue, ta->elem_type);
            sarrput(&queue, ta->len);
            break;
        }

        case MIR_INSTR_TYPE_DYNARR:
        case MIR_INSTR_TYPE_SLICE:
        case MIR_INSTR_TYPE_STRUCT: {
            struct mir_instr_type_struct *ts = (struct mir_instr_type_struct *)top;
            for (usize i = 0; i < sarrlenu(ts->members); ++i) {
                struct mir_instr *it = sarrpeek(ts->members, i);
                unref_instr(it);
                sarrput(&queue, it);
            }
            break;
        }

        case MIR_INSTR_VARGS: {
            struct mir_instr_vargs *vargs = (struct mir_instr_vargs *)top;
            for (usize i = 0; i < sarrlenu(vargs->values); ++i) {
                struct mir_instr *it = sarrpeek(vargs->values, i);
                unref_instr(it);
                sarrput(&queue, it);
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
        case MIR_INSTR_TYPE_POLY:
        case MIR_INSTR_MSG:
            break;

        default:
            babort("Missing erase for instruction '%s'", mir_instr_name(top));
        }

        if (keep_root && top == instr) continue;
        erase_instr(top);
    }
    sarrfree(&queue);
}

bool evaluate(struct context *ctx, struct mir_instr *instr)
{
    if (!instr) return true;
    bassert(isflag(instr->flags, MIR_IS_ANALYZED) &&
            "Non-analyzed instruction cannot be evaluated!");
    // We can evaluate compile time know instructions only.
    if (!instr->value.is_comptime) return true;
    // Special cases
    switch (instr->kind) {
    case MIR_INSTR_CALL: {
        // Call instruction must be evaluated and then later converted to constant in case it's
        // supposed to be called in compile time, otherwise we leave it as it is.
        struct mir_instr_call *call = (struct mir_instr_call *)instr;
        if (!call->call_in_compile_time) return true;
        if (!vm_execute_comptime_call(ctx->vm, ctx->assembly, call)) return false;
        break;
    }
    case MIR_INSTR_PHI: {
        // Comptime PHI instruction must be resolvable in this stage; it must have only one
        // possible income. It's converted to constant value containing resolved phi value.
        struct mir_instr_phi *phi = (struct mir_instr_phi *)instr;
        bassert(sarrlenu(phi->incoming_blocks) == sarrlenu(phi->incoming_values) == 1);
        struct mir_instr *value = sarrpeek(phi->incoming_values, 0);
        bassert(value);
        // @Incomplete: Check if the value is constant?
        struct mir_instr_const *placeholder = mutate_instr(instr, MIR_INSTR_CONST);
        // Duplicate constant value.
        memcpy(&placeholder->base.value, &value->value, sizeof(placeholder->base.value));
        return true;
    }
    case MIR_INSTR_COND_BR: {
        // Comptime conditional break can be simplified into direct break instruction in case
        // the expression is known in compile time. Dropped branch is kept for analyze but
        // reference count is reduced. This can eventually lead to marking the branch as
        // unreachable and produce compiler warning.
        struct mir_instr_cond_br *cond_br        = (struct mir_instr_cond_br *)instr;
        const bool                cond           = MIR_CEV_READ_AS(bool, &cond_br->cond->value);
        struct mir_instr_block   *continue_block = cond ? cond_br->then_block : cond_br->else_block;
        struct mir_instr_block   *discard_block = !cond ? cond_br->then_block : cond_br->else_block;
        unref_instr(&discard_block->base);
        unref_instr(cond_br->cond);
        if (cond_br->is_static && discard_block->base.ref_count == 0) {
            erase_block(&discard_block->base);
        }
        struct mir_instr_br *br    = mutate_instr(&cond_br->base, MIR_INSTR_BR);
        br->then_block             = continue_block;
        br->base.value.is_comptime = false; // ???
        return true;
    }
    default:
        if (!vm_eval_instr(ctx->vm, ctx->assembly, instr)) {
            // Evaluation was aborted due to error.
            return false;
        }
        break;
    }
    if (can_mutate_comptime_to_const(instr)) {
        const bool is_volatile = is_instr_type_volatile(instr);
        erase_instr_tree(instr, true, true);
        mutate_instr(instr, MIR_INSTR_CONST);
        ((struct mir_instr_const *)instr)->volatile_type = is_volatile;
    }
    return true;
}

struct result analyze_resolve_type(struct context   *ctx,
                                   struct mir_instr *resolver_call,
                                   struct mir_type **out_type)
{
    bassert(resolver_call && "Expected resolver call.");
    bassert(resolver_call->kind == MIR_INSTR_CALL &&
            "Type resolver is expected to be call to resolve function.");

    if (isnotflag(resolver_call->flags, MIR_IS_ANALYZED) &&
        analyze_instr(ctx, resolver_call).state != ANALYZE_PASSED) {
        return ANALYZE_RESULT(POSTPONE, 0);
    }

    if (vm_execute_comptime_call(ctx->vm, ctx->assembly, (struct mir_instr_call *)resolver_call)) {
        struct mir_type *resolved_type = MIR_CEV_READ_AS(struct mir_type *, &resolver_call->value);
        BL_MAGIC_ASSERT(resolved_type);
        *out_type = resolved_type;
        return ANALYZE_RESULT(PASSED, 0);
    } else {
        return ANALYZE_RESULT(FAILED, 0);
    }
}

struct result analyze_instr_toany(struct context *ctx, struct mir_instr_to_any *toany)
{
    zone();
    struct mir_instr *expr      = toany->expr;
    struct mir_type  *any_type  = ctx->builtin_types->t_Any;
    struct mir_type  *expr_type = expr->value.type;

    bassert(any_type && expr && expr_type);

    struct id *missing_rtti_type_id = lookup_builtins_rtti(ctx);
    if (missing_rtti_type_id) {
        return_zone(ANALYZE_RESULT(WAITING, missing_rtti_type_id->hash));
    }

    struct mir_type *rtti_type       = expr_type;
    const bool       is_deref_needed = is_load_needed(expr);
    if (is_deref_needed) rtti_type = mir_deref_type(rtti_type);

    const bool is_type       = rtti_type->kind == MIR_TYPE_TYPE || rtti_type->kind == MIR_TYPE_FN;
    const bool is_tmp_needed = expr->value.addr_mode == MIR_VAM_RVALUE && !is_type;

    if (rtti_type->kind == MIR_TYPE_VOID) {
        report_error(INVALID_TYPE,
                     expr->node,
                     "Expression yields 'void' value and cannot be converted to Any.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    if (is_tmp_needed && expr_type->store_size_bytes > 0) {
        // Target expression is not allocated object on the stack, so we need to crate
        // temporary variable containing the value and fetch pointer to this variable.
        const char *tmp_var_name = create_unique_name(ctx, IMPL_ANY_EXPR_TMP);
        toany->expr_tmp = create_var_impl(ctx, NULL, tmp_var_name, rtti_type, false, false, false);
    }

    // Generate RTTI for expression's type.
    toany->rtti_type = rtti_type;
    rtti_gen(ctx, rtti_type);

    if (is_type) {
        bassert(mir_is_comptime(expr));
        bassert(!is_tmp_needed);

        vm_stack_ptr_t expr_data = NULL;
        if (is_deref_needed) {
            expr_data = *MIR_CEV_READ_AS(vm_stack_ptr_t *, &expr->value);
        } else {
            expr_data = MIR_CEV_READ_AS(vm_stack_ptr_t, &expr->value);
        }

        struct mir_type *rtti_data = NULL;

        switch (rtti_type->kind) {
        case MIR_TYPE_FN: {
            struct mir_fn *fn = (struct mir_fn *)expr_data;
            rtti_data         = fn->type;
            break;
        }

        case MIR_TYPE_TYPE: {
            rtti_data = (struct mir_type *)expr_data;
            break;
        }

        default:
            babort("Invalid expression type!");
        }

        bassert(rtti_data && "Missing specification type for RTTI generation!");
        toany->rtti_data = rtti_data;

        rtti_gen(ctx, rtti_data);
        erase_instr_tree(expr, false, true);
    }

    // This is temporary variable used for Any data.
    const char *tmp_var_name = create_unique_name(ctx, IMPL_ANY_TMP);
    toany->tmp = create_var_impl(ctx, NULL, tmp_var_name, any_type, false, false, false);

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_phi(struct context *ctx, struct mir_instr_phi *phi)
{
    zone();
    bassert(phi->incoming_blocks && phi->incoming_values);
    bassert(sarrlenu(phi->incoming_values) == sarrlenu(phi->incoming_blocks));
    // @Performance: Recreating small arrays here is probably faster then removing elements?
    mir_instrs_t                 *new_blocks      = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    mir_instrs_t                 *new_values      = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    const struct mir_instr_block *phi_owner_block = phi->base.owner_block;
    struct mir_type              *type            = NULL;
    bool                          is_comptime     = true;
    bool                          cnt             = true;
    for (usize i = 0; i < sarrlenu(phi->incoming_values) && cnt; ++i) {
        struct mir_instr **value_ref = &sarrpeek(phi->incoming_values, i);
        struct mir_instr  *block     = sarrpeek(phi->incoming_blocks, i);
        bassert(block && block->kind == MIR_INSTR_BLOCK);
        bassert(isflag((*value_ref)->flags, MIR_IS_ANALYZED) &&
                "Phi incoming value is not analyzed!");
        if ((*value_ref)->kind == MIR_INSTR_COND_BR) {
            *value_ref = ((struct mir_instr_cond_br *)(*value_ref))->cond;
            bassert(value_ref && *value_ref);
            bassert(isflag((*value_ref)->flags, MIR_IS_ANALYZED) &&
                    "Phi incoming value is not analyzed!");
        } else if ((*value_ref)->kind == MIR_INSTR_BR) {
            const struct mir_instr_br *br = (struct mir_instr_br *)(*value_ref);
            bassert(is_comptime);
            // THE RESULT VALUE MUST BE LISTED BEFORE THE BREAK INSTRUCTION.
            *value_ref = br->base.prev;
            bassert(*value_ref); // @Incomplete: Check for constant instr?
            if (br->then_block->base.id == phi_owner_block->base.id) {
                cnt = false;
            } else {
                // erase from incoming
                unref_instr(*value_ref);
                continue;
            }
        } else {
            const struct slot_config *conf =
                type ? &analyze_slot_conf_default : &analyze_slot_conf_basic;
            if (analyze_slot(ctx, conf, value_ref, type) != ANALYZE_PASSED)
                return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        if (!type) type = (*value_ref)->value.type;
        is_comptime = is_comptime ? (*value_ref)->value.is_comptime : false;
        sarrput(new_blocks, block);
        sarrput(new_values, *value_ref);
    }
    bassert(type && "Cannot resolve type of phi instruction!");
    phi->incoming_blocks        = new_blocks;
    phi->incoming_values        = new_values;
    phi->base.value.type        = type;
    phi->base.value.addr_mode   = MIR_VAM_RVALUE;
    phi->base.value.is_comptime = is_comptime;
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_unroll(struct context *ctx, struct mir_instr_unroll *unroll)
{
    zone();
    struct mir_instr *src   = unroll->src;
    const s32         index = unroll->index;
    bassert(src && "Missing unroll input!");
    bassert(index >= 0);
    bassert(src->value.type);
    struct mir_type *src_type = src->value.type;
    struct mir_type *type     = src_type;
    if (mir_is_composit_type(src_type) && src_type->data.strct.is_multiple_return_type) {
        if (index >= (s32)sarrlen(src_type->data.strct.members)) {
            report_error(INVALID_MEMBER_ACCESS, unroll->base.node, "Expected more return values.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        bassert(src->kind == MIR_INSTR_CALL && "Unroll expects source to be CALL instruction!");
        struct mir_instr_call *src_call = (struct mir_instr_call *)src;
        struct mir_instr      *tmp_var  = src_call->unroll_tmp_var;
        if (!tmp_var) {
            // no tmp var to unroll from; create one and insert it after call
            tmp_var = create_instr_decl_var_impl(ctx,
                                                 NULL,
                                                 create_unique_name(ctx, IMPL_UNROLL_TMP),
                                                 NULL,
                                                 src,
                                                 false, // is_mutable
                                                 false  // is_global
            );
            insert_instr_after(src, tmp_var);
            ANALYZE_INSTR_RQ(tmp_var);
            src_call->unroll_tmp_var = tmp_var;
        }
        tmp_var = create_instr_decl_direct_ref(ctx, NULL, tmp_var);
        insert_instr_before(&unroll->base, tmp_var);
        ANALYZE_INSTR_RQ(tmp_var);
        unroll->src = ref_instr(tmp_var);
        type        = create_type_ptr(ctx, mir_get_struct_elem_type(src_type, index));
    } else {
        unroll->remove = true;
    }
    bassert(type);
    unroll->base.value.type        = type;
    unroll->base.value.is_comptime = src->value.is_comptime;
    unroll->base.value.addr_mode   = src->value.addr_mode;
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_compound(struct context *ctx, struct mir_instr_compound *cmp)
{
    zone();
    mir_instrs_t *values = cmp->values;
    if (cmp->is_multiple_return_value) {
        // Compound expression used as multiple return value has no type specified; function
        // return type must by used.
        struct mir_fn *fn = instr_owner_fn(&cmp->base);
        bassert(fn && fn->type);
        cmp->base.value.type = fn->type->data.fn.ret_type;
        bassert(cmp->base.value.type);
    }

    struct mir_type *type = cmp->base.value.type;
    if (isflag(cmp->base.flags, MIR_IS_IMPLICIT)) {
        bassert(type && "Missing type for implicit compound!");
        bassert((cmp->is_zero_initialized || sarrlen(values)) &&
                "Missing type for implicit compound!");

        goto SKIP_IMPLICIT;
    }
    // Setup compound type.
    if (!type) {
        // generate load instruction if needed
        bassert(isflag(cmp->type->flags, MIR_IS_ANALYZED));
        if (analyze_slot(ctx, &analyze_slot_conf_basic, &cmp->type, NULL) != ANALYZE_PASSED)
            return_zone(ANALYZE_RESULT(FAILED, 0));

        struct mir_instr *instr_type = cmp->type;
        if (instr_type->value.type->kind != MIR_TYPE_TYPE) {
            report_error(
                INVALID_TYPE, instr_type->node, "Expected type before compound expression.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        type = MIR_CEV_READ_AS(struct mir_type *, &instr_type->value);
        BL_MAGIC_ASSERT(type);
    }

    bassert(type);

    if (!values) {
        report_error_after(INVALID_INITIALIZER, cmp->type->node, "Expected expression.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    cmp->base.value.type        = type;
    cmp->base.value.is_comptime = true; // can be overriden later

    // Check if array is supposed to be initilialized to {0}
    if (sarrlen(values) == 1) {
        struct mir_instr *value = sarrpeek(values, 0);
        if (value->kind == MIR_INSTR_CONST && value->value.type->kind == MIR_TYPE_INT &&
            MIR_CEV_READ_AS(u64, &value->value) == 0) {
            cmp->is_zero_initialized = true;
        }
    }

    switch (type->kind) {
    case MIR_TYPE_ARRAY: {
        if (cmp->is_zero_initialized) break;

        if (sarrlenu(values) != (usize)type->data.array.len) {
            report_error(INVALID_INITIALIZER,
                         cmp->base.node,
                         "Array initializer must explicitly set all array elements or "
                         "initialize array to 0 by zero initializer {0}. Expected is "
                         "%llu but given %llu.",
                         (unsigned long long)type->data.array.len,
                         sarrlenu(values));
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        // Else iterate over values
        for (usize i = 0; i < sarrlenu(values); ++i) {
            struct mir_instr **value_ref = &sarrpeek(values, i);
            if (analyze_slot(
                    ctx, &analyze_slot_conf_default, value_ref, type->data.array.elem_type) !=
                ANALYZE_PASSED)
                return_zone(ANALYZE_RESULT(FAILED, 0));

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
        const usize memc                    = sarrlenu(type->data.strct.members);

        if (is_union) {
            report_error(
                INVALID_INITIALIZER, cmp->base.node, "Union can be zero initialized only.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        } else {
            if (sarrlenu(values) != memc) {
                const char *msg =
                    is_multiple_return_type
                        ? "Expected %llu return values but given %llu."
                        : "Structure initializer must explicitly set all members of the "
                          "structure or initialize structure to 0 by zero initializer "
                          "{0}. Expected is %llu but given %llu.";
                report_error(INVALID_INITIALIZER,
                             cmp->base.node,
                             msg,
                             (unsigned long long)memc,
                             (unsigned long long)sarrlenu(values));
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }

            // Else iterate over values
            struct mir_instr **value_ref;
            struct mir_type   *member_type;
            for (usize i = 0; i < sarrlenu(values); ++i) {
                value_ref   = &sarrpeek(values, i);
                member_type = mir_get_struct_elem_type(type, i);

                if (analyze_slot(ctx, &analyze_slot_conf_default, value_ref, member_type) !=
                    ANALYZE_PASSED)
                    return_zone(ANALYZE_RESULT(FAILED, 0));

                cmp->base.value.is_comptime =
                    (*value_ref)->value.is_comptime ? cmp->base.value.is_comptime : false;
            }
        }

        break;
    }

    default: {
        // Non-aggregate type.
        if (sarrlen(values) > 1) {
            struct mir_instr *value = sarrpeek(values, 1);
            report_error(INVALID_INITIALIZER,
                         value->node,
                         "One value only is expected for non-aggregate types.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        struct mir_instr        **value_ref = &sarrpeek(values, 0);
        const struct slot_config *conf =
            type ? &analyze_slot_conf_default : &analyze_slot_conf_basic;
        if (analyze_slot(ctx, conf, value_ref, type) != ANALYZE_PASSED)
            return_zone(ANALYZE_RESULT(FAILED, 0));
        cmp->base.value.is_comptime = (*value_ref)->value.is_comptime;
    }
    }

SKIP_IMPLICIT:
    if (!mir_is_global(&cmp->base) && cmp->is_naked) {
        // For naked non-compile time compounds we need to generate implicit temp storage to
        // keep all data.
        struct mir_var *tmp_var = create_var_impl(
            ctx, NULL, create_unique_name(ctx, IMPL_COMPOUND_TMP), type, true, false, false);
        cmp->tmp_var = tmp_var;
    }

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_var(struct context *ctx, struct mir_var *var)
{
    zone();
    BL_TRACY_MESSAGE("ANALYZE_VAR", "%s", var->linkage_name);
    if (!var->value.type) {
        babort("unknown declaration type");
    }
    switch (var->value.type->kind) {
    case MIR_TYPE_TYPE:
        // Disable LLVM generation of typedefs.
        if (!var->is_mutable) break;
        // Typedef must be immutable!
        report_error(INVALID_MUTABILITY, var->decl_node, "Type declaration must be immutable.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    case MIR_TYPE_FN:
        report_error(INVALID_TYPE,
                     var->decl_node,
                     "Invalid type of the variable, functions can be referenced "
                     "only by pointers.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    case MIR_TYPE_FN_GROUP:
        if (!var->is_mutable) break;
        report_error(INVALID_TYPE, var->decl_node, "Function group must be immutable.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    case MIR_TYPE_VOID:
        // Allocated type is void type.
        report_error(INVALID_TYPE, var->decl_node, "Cannot allocate unsized type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    default:
        break;
    }

    if (!var->is_implicit) {
        if (var->is_global && var->entry->kind == SCOPE_ENTRY_VOID) {
            report_error(
                INVALID_NAME, var->decl_node, "Global variable cannot be explicitly unnamed.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        commit_var(ctx, var);
    }
    // Type declaration should not be generated in LLVM.
    const enum mir_type_kind var_type_kind = var->value.type->kind;
    var->emit_llvm = var_type_kind != MIR_TYPE_TYPE && var_type_kind != MIR_TYPE_FN_GROUP;
    // Just take note whether variable was fully analyzed.
    var->is_analyzed = true;
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_set_initializer(struct context                   *ctx,
                                            struct mir_instr_set_initializer *si)
{
    zone();
    mir_instrs_t *dests = si->dests;
    bassert(sarrlen(dests) && "Expected at least one variable.");
    for (usize i = 0; i < sarrlenu(dests); ++i) {
        struct mir_instr *dest = sarrpeek(dests, i);
        // Just pre-scan to check if all destination variables are analyzed.
        bassert(dest && dest->kind == MIR_INSTR_DECL_VAR);
        if (isnotflag(dest->flags, MIR_IS_ANALYZED))
            return_zone(ANALYZE_RESULT(POSTPONE, 0)); // PERFORMANCE: use wait???
    }

    struct mir_type *type = ((struct mir_instr_decl_var *)sarrpeek(dests, 0))->var->value.type;
    // When there is no source initialization value to set global we can omit type inferring and
    // initialization value slot analyze pass.
    const bool is_default = !si->src;
    if (!is_default) {
        const struct slot_config *config =
            type ? &analyze_slot_conf_default : &analyze_slot_conf_basic;

        if (analyze_slot(ctx, config, &si->src, type) != ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        // Infer variable type if needed.
        if (!type) type = si->src->value.type;
    } else {
        // Generate default value based on type!
        bassert(type && "Missing variable initializer type for default global initializer!");
        struct mir_instr *default_init = create_default_value_for_type(ctx, type);
        insert_instr_before(&si->base, default_init);
        ANALYZE_INSTR_RQ(default_init);
        si->src = default_init;
    }

    bassert(type && "Missing variable initializer type for default global initializer!");
    bassert(type->kind != MIR_TYPE_VOID && "Global value cannot be void!");
    bassert(si->src && "Invalid global initializer source value.");
    // Global initializer must be compile time known.
    if (!si->src->value.is_comptime) {
        report_error(EXPECTED_COMPTIME,
                     si->src->node,
                     "Global variables must be initialized with compile time known value.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    SET_IS_NAKED_IF_COMPOUND(si->src, false);
    for (usize i = 0; i < sarrlenu(dests); ++i) {
        struct mir_instr *dest = sarrpeek(dests, i);
        struct mir_var   *var  = ((struct mir_instr_decl_var *)dest)->var;
        bassert(var && "Missing struct mir_var for variable declaration!");
        bassert((var->is_global || var->is_struct_typedef) &&
                "Only global variables can be initialized by initializer!");
        var->value.type = type;
        // Initializer value is guaranteed to be comptime so we just check variable mutability.
        // (mutable variables cannot be comptime)
        var->value.is_comptime = !var->is_mutable;

        if (isflag(var->flags, FLAG_THREAD_LOCAL)) {
            switch (var->value.type->kind) {
            case MIR_TYPE_FN:
            case MIR_TYPE_FN_GROUP:
            case MIR_TYPE_TYPE:
                report_error(
                    INVALID_TYPE, var->decl_node, "Invalid type of thread local variable.");
                return ANALYZE_RESULT(FAILED, 0);
            default:
                break;
            }
        }

        struct result state = analyze_var(ctx, var);
        if (state.state != ANALYZE_PASSED) return state;

        if (!var->value.is_comptime) {
            // Global variables which are not compile time constants are allocated
            // on the stack, one option is to do allocation every time when we
            // invoke comptime function execution, but we don't know which globals
            // will be used by function and we also don't known whatever function
            // has some side effect or not. So we produce allocation here. Variable
            // will be stored in static data segment. There is no need to use
            // relative pointers here.
            vm_alloc_global(ctx->vm, ctx->assembly, var);
        }

        // Check whether variable is command_line_arguments, we store pointer to this variable
        // for later use (it's going to be set to user input arguments in case of compile-time
        // execution).
        if (var->builtin_id == BUILTIN_ID_COMMAND_LINE_ARGUMENTS) {
            bassert(!ctx->assembly->vm_run.command_line_arguments);
            ctx->assembly->vm_run.command_line_arguments = var;
        }
    }
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_vargs(struct context *ctx, struct mir_instr_vargs *vargs)
{
    zone();
    struct mir_type *type   = vargs->type;
    mir_instrs_t    *values = vargs->values;
    bassert(type && values);
    type             = CREATE_TYPE_STRUCT_VARGS(ctx, NULL, create_type_ptr(ctx, type));
    const usize valc = sarrlen(values);
    if (valc > 0) {
        // Prepare tmp array for values
        const char      *tmp_name = create_unique_name(ctx, IMPL_VARGS_TMP_ARR);
        struct mir_type *tmp_type = create_type_array(ctx, NULL, vargs->type, (u32)valc);
        vargs->arr_tmp = create_var_impl(ctx, NULL, tmp_name, tmp_type, true, false, false);
    }

    {
        // Prepare tmp slice for vargs
        const char *tmp_name = create_unique_name(ctx, IMPL_VARGS_TMP);
        vargs->vargs_tmp     = create_var_impl(ctx, NULL, tmp_name, type, true, false, false);
    }

    struct mir_instr **value;
    bool               is_valid = true;

    for (usize i = 0; i < valc && is_valid; ++i) {
        value = &sarrpeek(values, i);

        if (analyze_slot(ctx, &analyze_slot_conf_full, value, vargs->type) != ANALYZE_PASSED)
            return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    vargs->base.value.type = type;
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_elem_ptr(struct context *ctx, struct mir_instr_elem_ptr *elem_ptr)
{
    zone();
    if (analyze_slot(
            ctx, &analyze_slot_conf_default, &elem_ptr->index, ctx->builtin_types->t_s64) !=
        ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    struct mir_instr *arr_ptr = elem_ptr->arr_ptr;
    bassert(arr_ptr);
    bassert(arr_ptr->value.type);

    if (!mir_is_pointer_type(arr_ptr->value.type)) {
        report_error(INVALID_TYPE, elem_ptr->arr_ptr->node, "Expected array type or slice.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    struct mir_type *arr_type = mir_deref_type(arr_ptr->value.type);
    bassert(arr_type);

    switch (arr_type->kind) {
    case MIR_TYPE_ARRAY: {
        if (mir_is_comptime(elem_ptr->index)) {
            const s64 len = arr_type->data.array.len;
            const s64 i   = MIR_CEV_READ_AS(s64, &elem_ptr->index->value);
            if (i >= len || i < 0) {
                report_error(BOUND_CHECK_FAILED,
                             elem_ptr->index->node,
                             "Array index is out of the bounds, array size is %lli so index must "
                             "fit in range from 0 to %lli.",
                             len,
                             len - 1);
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }
        }

        // setup ElemPtr instruction const_value type
        struct mir_type *elem_type = arr_type->data.array.elem_type;
        bassert(elem_type);
        elem_ptr->base.value.type = create_type_ptr(ctx, elem_type);
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
        struct mir_type *elem_type = mir_get_struct_elem_type(arr_type, MIR_SLICE_PTR_INDEX);
        bassert(elem_type);
        elem_ptr->base.value.type = elem_type;
        break;
    }
    default: {
        report_error(INVALID_TYPE, arr_ptr->node, "Expected array or slice type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }
    }

    elem_ptr->base.value.addr_mode   = arr_ptr->value.addr_mode;
    elem_ptr->base.value.is_comptime = mir_is_comptime(arr_ptr) && mir_is_comptime(elem_ptr->index);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_member_ptr(struct context *ctx, struct mir_instr_member_ptr *member_ptr)
{
    zone();
    struct mir_instr *target_ptr = member_ptr->target_ptr;
    bassert(target_ptr);
    struct mir_type *target_type = target_ptr->value.type;
    bassert(target_type);

    enum mir_value_address_mode target_addr_mode = target_ptr->value.addr_mode;
    struct ast                 *ast_member_ident = member_ptr->member_ident;

    if (target_type->kind == MIR_TYPE_NAMED_SCOPE) {
        struct scope_entry *scope_entry = MIR_CEV_READ_AS(struct scope_entry *, &target_ptr->value);
        bassert(scope_entry);
        BL_MAGIC_ASSERT(scope_entry);
        bassert(scope_entry->kind == SCOPE_ENTRY_NAMED_SCOPE && "Expected named scope.");
        struct scope *scope = scope_entry->data.scope;
        bassert(scope);
        BL_MAGIC_ASSERT(scope);
        struct id   *rid         = &ast_member_ident->data.ident.id;
        struct unit *parent_unit = ast_member_ident->location->unit;
        bassert(rid);
        bassert(parent_unit);
        struct mir_instr_decl_ref *decl_ref =
            (struct mir_instr_decl_ref *)mutate_instr(&member_ptr->base, MIR_INSTR_DECL_REF);
        decl_ref->scope                  = scope;
        decl_ref->scope_entry            = NULL;
        decl_ref->scope_layer            = ctx->polymorph.current_scope_layer_index;
        decl_ref->accept_incomplete_type = false;
        decl_ref->parent_unit            = parent_unit;
        decl_ref->rid                    = rid;
        unref_instr(target_ptr);
        erase_instr_tree(target_ptr, false, false);
        return_zone(ANALYZE_RESULT(POSTPONE, 0));
    }

    if (mir_is_pointer_type(target_type)) {
        target_type = mir_deref_type(target_type);
    }

    // Array type
    if (target_type->kind == MIR_TYPE_ARRAY) {
        // check array builtin members
        if (member_ptr->builtin_id == BUILTIN_ID_ARR_LEN ||
            is_builtin(ast_member_ident, BUILTIN_ID_ARR_LEN)) {
            // .len
            // mutate instruction into constant
            unref_instr(member_ptr->target_ptr);
            erase_instr_tree(member_ptr->target_ptr, false, false);
            struct mir_instr_const *len =
                (struct mir_instr_const *)mutate_instr(&member_ptr->base, MIR_INSTR_CONST);
            len->volatile_type          = false;
            len->base.value.is_comptime = true;
            len->base.value.type        = ctx->builtin_types->t_s64;
            MIR_CEV_WRITE_AS(s64, &len->base.value, target_type->data.array.len);
        } else if (member_ptr->builtin_id == BUILTIN_ID_ARR_PTR ||
                   is_builtin(ast_member_ident, BUILTIN_ID_ARR_PTR)) {
            // .ptr -> This will be replaced by:
            //     elemptr
            //     addrof
            // to match syntax: &array[0]
            struct mir_instr *index =
                create_instr_const_int(ctx, NULL, ctx->builtin_types->t_s64, 0);

            insert_instr_before(&member_ptr->base, index);

            struct mir_instr *elem_ptr = create_instr_elem_ptr(ctx, NULL, target_ptr, index);
            ref_instr(elem_ptr);

            insert_instr_before(&member_ptr->base, elem_ptr);

            ANALYZE_INSTR_RQ(index);
            ANALYZE_INSTR_RQ(elem_ptr);

            struct mir_instr_addrof *addrof_elem =
                (struct mir_instr_addrof *)mutate_instr(&member_ptr->base, MIR_INSTR_ADDROF);
            clrflag(addrof_elem->base.flags, MIR_IS_ANALYZED);
            addrof_elem->src = elem_ptr;
            ANALYZE_INSTR_RQ(&addrof_elem->base);
        } else {
            report_error(INVALID_MEMBER_ACCESS, ast_member_ident, "Unknown member.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        member_ptr->base.value.addr_mode = target_addr_mode;
        return_zone(ANALYZE_RESULT(PASSED, 0));
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
            return_zone(ANALYZE_RESULT(WAITING, target_type->user_id->hash));

        if (additional_load_needed) {
            member_ptr->target_ptr = insert_instr_load(ctx, member_ptr->target_ptr);
            ANALYZE_INSTR_RQ(member_ptr->target_ptr);
        }

        struct id          *rid   = &ast_member_ident->data.ident.id;
        struct mir_type    *type  = target_type;
        struct scope_entry *found = lookup_composit_member(target_type, rid, &type);

        // Check if member was found in base type's scope.
        if (found && found->parent_scope != target_type->data.strct.scope) {
            // @HACK: It seems to be the best way for now just create implicit
            // cast to desired base type and use this as target, that also
            // should solve problems with deeper nesting (bitcast of pointer is
            // better then multiple GEPs?)
            if (is_load_needed(member_ptr->target_ptr)) {
                member_ptr->target_ptr = insert_instr_addrof(ctx, member_ptr->target_ptr);

                ANALYZE_INSTR_RQ(member_ptr->target_ptr);
            }
            member_ptr->target_ptr =
                insert_instr_cast(ctx, member_ptr->target_ptr, create_type_ptr(ctx, type));
            ANALYZE_INSTR_RQ(member_ptr->target_ptr);
        }

        if (!found) {
            // Member not found!
            report_error(UNKNOWN_SYMBOL, member_ptr->member_ident, "Unknown structure member.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        bassert(found->kind == SCOPE_ENTRY_MEMBER);
        struct mir_member *member = found->data.member;

        // setup member_ptr type
        struct mir_type *member_ptr_type   = create_type_ptr(ctx, member->type);
        member_ptr->base.value.type        = member_ptr_type;
        member_ptr->base.value.addr_mode   = target_addr_mode;
        member_ptr->base.value.is_comptime = target_ptr->value.is_comptime;
        member_ptr->scope_entry            = found;

        return_zone(ANALYZE_RESULT(PASSED, 0));
    }

    // Sub type member.
    if (target_type->kind == MIR_TYPE_TYPE) {
        // generate load instruction if needed
        if (analyze_slot(ctx, &analyze_slot_conf_basic, &member_ptr->target_ptr, NULL) !=
            ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        struct mir_type *sub_type =
            MIR_CEV_READ_AS(struct mir_type *, &member_ptr->target_ptr->value);
        BL_MAGIC_ASSERT(sub_type);

        if (sub_type->kind != MIR_TYPE_ENUM) {
            goto INVALID;
        }

        // lookup for member inside struct
        struct scope       *scope       = sub_type->data.enm.scope;
        struct id          *rid         = &ast_member_ident->data.ident.id;
        const s32           layer_index = ctx->polymorph.current_scope_layer_index;
        struct scope_entry *found       = scope_lookup(scope, layer_index, rid, false, true, NULL);
        if (!found) {
            report_error(UNKNOWN_SYMBOL, member_ptr->member_ident, "Unknown enumerator variant.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        bassert(found->kind == SCOPE_ENTRY_VARIANT);

        member_ptr->scope_entry            = found;
        member_ptr->base.value.type        = sub_type;
        member_ptr->base.value.addr_mode   = target_addr_mode;
        member_ptr->base.value.is_comptime = true;

        return_zone(ANALYZE_RESULT(PASSED, 0));
    }

    // Invalid
INVALID:
    report_error(INVALID_TYPE, target_ptr->node, "Expected structure or enumerator type.");
    return_zone(ANALYZE_RESULT(FAILED, 0));
}

struct result analyze_instr_addrof(struct context *ctx, struct mir_instr_addrof *addrof)
{
    zone();
    struct mir_instr *src = addrof->src;
    bassert(src);
    if (isnotflag(src->flags, MIR_IS_ANALYZED)) return_zone(ANALYZE_RESULT(POSTPONE, 0));
    const enum mir_value_address_mode src_addr_mode = src->value.addr_mode;
    const bool                        is_source_polymorph =
        src->value.type->kind == MIR_TYPE_FN &&
        isflag(src->value.type->data.fn.flags, MIR_TYPE_FN_FLAG_IS_POLYMORPH);
    const bool can_grab_address =
        (src_addr_mode == MIR_VAM_LVALUE || src_addr_mode == MIR_VAM_LVALUE_CONST ||
         (src->value.type->kind == MIR_TYPE_FN && !is_source_polymorph));

    if (!can_grab_address) {
        report_error(
            EXPECTED_DECL, addrof->base.node, "Cannot take the address of unallocated object.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    // setup type
    struct mir_type *type = NULL;
    bassert(src->value.type);
    if (src->value.type->kind == MIR_TYPE_FN) {
        struct mir_fn *fn = MIR_CEV_READ_AS(struct mir_fn *, &src->value);
        BL_MAGIC_ASSERT(fn);
        // NOTE: Here we increase function ref count.
        ++fn->ref_count;
        type = create_type_ptr(ctx, src->value.type);
    } else {
        type = src->value.type;
    }
    addrof->base.value.type        = type;
    addrof->base.value.is_comptime = addrof->src->value.is_comptime && mir_is_global(addrof->src);
    addrof->base.value.addr_mode   = MIR_VAM_RVALUE;
    bassert(addrof->base.value.type && "invalid type");
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result
analyze_instr_cast(struct context *ctx, struct mir_instr_cast *cast, bool analyze_op_only)
{
    zone();
    struct mir_type *dest_type = cast->base.value.type;

    if (!analyze_op_only) {
        if (!dest_type && !cast->auto_cast) {
            struct result result = analyze_resolve_type(ctx, cast->type, &dest_type);
            if (result.state != ANALYZE_PASSED) return_zone(result);
        }

        const struct slot_config *config = isflag(cast->base.flags, MIR_IS_IMPLICIT)
                                               ? &analyze_slot_conf_dummy
                                               : &analyze_slot_conf_basic;

        if (analyze_slot(ctx, config, &cast->expr, dest_type) != ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        bassert(cast->expr->value.type && "invalid cast source type");

        if (!dest_type && cast->auto_cast) {
            dest_type = cast->expr->value.type;
        }
    }

    bassert(dest_type && "invalid cast destination type");
    bassert(cast->expr->value.type && "invalid cast source type");

    struct mir_type *expr_type = cast->expr->value.type;

    // Setup const int type.
    if (analyze_stage_set_volatile_expr(ctx, &cast->expr, dest_type, false) ==
        ANALYZE_STAGE_BREAK) {
        cast->op = MIR_CAST_NONE;
        goto DONE;
    }

    cast->op = get_cast_op(expr_type, dest_type);
    if (cast->op == MIR_CAST_INVALID) {
        error_types(ctx,
                    &cast->base,
                    expr_type,
                    dest_type,
                    cast->base.node,
                    "Invalid cast from '%s' to '%s'.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

DONE:
    cast->base.value.type        = dest_type;
    cast->base.value.is_comptime = cast->expr->value.is_comptime;

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_sizeof(struct context *ctx, struct mir_instr_sizeof *szof)
{
    zone();
    bassert(szof->expr);

    if (analyze_slot(ctx, &analyze_slot_conf_basic, &szof->expr, NULL) != ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    struct mir_type *type = szof->expr->value.type;
    bassert(type);

    if (type->kind == MIR_TYPE_TYPE) {
        type = MIR_CEV_READ_AS(struct mir_type *, &szof->expr->value);
        BL_MAGIC_ASSERT(type);
    }

    // sizeof operator needs only type of input expression so we can erase whole call
    // tree generated to get this expression
    unref_instr(szof->expr);
    erase_instr_tree(szof->expr, false, false);

    MIR_CEV_WRITE_AS(u64, &szof->base.value, type->store_size_bytes);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_info(struct context *ctx, struct mir_instr_type_info *type_info)
{
    zone();
    if (!type_info->rtti_type) {
        bassert(type_info->expr);
        struct id *missing_rtti_type_id = lookup_builtins_rtti(ctx);
        if (missing_rtti_type_id) {
            return_zone(ANALYZE_RESULT(WAITING, missing_rtti_type_id->hash));
        }
        if (analyze_slot(ctx, &analyze_slot_conf_basic, &type_info->expr, NULL) != ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        struct mir_type *type = type_info->expr->value.type;
        BL_MAGIC_ASSERT(type);
        if (type->kind == MIR_TYPE_TYPE) {
            type = MIR_CEV_READ_AS(struct mir_type *, &type_info->expr->value);
            BL_MAGIC_ASSERT(type);
        }
        type_info->rtti_type = type;
    }
    if (!is_complete_type(ctx, type_info->rtti_type)) {
        return_zone(ANALYZE_RESULT(POSTPONE, 0));
    }
    if (type_info->rtti_type->kind == MIR_TYPE_NAMED_SCOPE) {
        report_error(INVALID_TYPE, type_info->expr->node, "No type info available for scope type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }
    if (type_info->rtti_type->kind == MIR_TYPE_FN &&
        isflag(type_info->rtti_type->data.fn.flags, MIR_TYPE_FN_FLAG_IS_POLYMORPH)) {
        report_error(INVALID_TYPE,
                     type_info->expr->node,
                     "No type info available for polymorph function recipe.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }
    rtti_gen(ctx, type_info->rtti_type);
    type_info->base.value.type = ctx->builtin_types->t_TypeInfo_ptr;

    erase_instr_tree(type_info->expr, false, true);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_alignof(struct context *ctx, struct mir_instr_alignof *alof)
{
    zone();
    bassert(alof->expr);

    if (analyze_slot(ctx, &analyze_slot_conf_basic, &alof->expr, NULL) != ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    struct mir_type *type = alof->expr->value.type;
    bassert(type);

    if (type->kind == MIR_TYPE_TYPE) {
        type = MIR_CEV_READ_AS(struct mir_type *, &alof->expr->value);
        BL_MAGIC_ASSERT(type);
    }

    MIR_CEV_WRITE_AS(s32, &alof->base.value, type->alignment);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_decl_ref(struct context *ctx, struct mir_instr_decl_ref *ref)
{
    zone();
    bassert(ref->rid && ref->scope);

    struct scope_entry *found                        = ref->scope_entry;
    struct scope       *private_scope                = ref->parent_unit->private_scope;
    bool                is_ref_out_of_fn_local_scope = false;

    if (!found) {
        if (private_scope && scope_is_subtree_of_kind(private_scope, SCOPE_NAMED) &&
            !scope_is_subtree_of_kind(ref->scope, SCOPE_NAMED)) {
            private_scope = NULL;
        }

        if (!private_scope) { // reference in unit without private scope
            found = scope_lookup(
                ref->scope, ref->scope_layer, ref->rid, true, false, &is_ref_out_of_fn_local_scope);
        } else { // reference in unit with private scope
            // search in current tree and ignore global scope
            found = scope_lookup(
                ref->scope, ref->scope_layer, ref->rid, true, false, &is_ref_out_of_fn_local_scope);

            // lookup in private scope and global scope also (private scope has global
            // scope as parent every time)
            if (!found) {
                found = scope_lookup(private_scope,
                                     ref->scope_layer,
                                     ref->rid,
                                     true,
                                     false,
                                     &is_ref_out_of_fn_local_scope);
            }
        }
        if (!found) return ANALYZE_RESULT(WAITING, ref->rid->hash);
    }
    bassert(found);
    // Cache found reference here.
    ref->scope_entry = found;
    if (found->kind == SCOPE_ENTRY_INCOMPLETE) {
        BL_TRACY_MESSAGE("INCOMPLETE_DECL_REF", "%s", ref->rid->str);
        return_zone(ANALYZE_RESULT(WAITING, ref->rid->hash));
    }
    scope_entry_ref(found);
    switch (found->kind) {
    case SCOPE_ENTRY_FN: {
        struct mir_fn *fn = found->data.fn;
        bassert(fn);
        struct mir_type *type = fn->type;
        bassert(type);

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
        ref->base.value.type        = ctx->builtin_types->t_type;
        ref->base.value.is_comptime = true;
        ref->base.value.addr_mode   = MIR_VAM_RVALUE;
        break;
    }

    case SCOPE_ENTRY_NAMED_SCOPE: {
        ref->base.value.type        = ctx->builtin_types->t_scope;
        ref->base.value.is_comptime = true;
        ref->base.value.addr_mode   = MIR_VAM_RVALUE;
        break;
    }

    case SCOPE_ENTRY_VARIANT: {
        struct mir_variant *variant = found->data.variant;
        bassert(variant);
        struct mir_type *type = variant->value_type;
        bassert(type);
        type                        = create_type_ptr(ctx, type);
        ref->base.value.type        = type;
        ref->base.value.is_comptime = true;
        ref->base.value.addr_mode   = MIR_VAM_RVALUE;
        break;
    }

    case SCOPE_ENTRY_VAR: {
        struct mir_var *var = found->data.var;
        bassert(var);
        struct mir_type *type = var->value.type;
        bassert(type);
        ++var->ref_count;
        // Check if we try get reference to incomplete structure type.
        if (type->kind == MIR_TYPE_TYPE) {
            struct mir_type *t = MIR_CEV_READ_AS(struct mir_type *, &var->value);
            BL_MAGIC_ASSERT(t);
            if (!ref->accept_incomplete_type && is_incomplete_struct_type(t)) {
                bassert(t->user_id);
                return_zone(ANALYZE_RESULT(WAITING, t->user_id->hash));
            }
        } else if (!var->is_global && is_ref_out_of_fn_local_scope) {
            // Here we must handle situation when we try to reference variables
            // declared in parent functions of local functions. (We try to
            // implicitly capture those variables and this leads to invalid LLVM
            // IR.)
            report_error(INVALID_REFERENCE,
                         ref->base.node,
                         "Attempt to reference variable from parent "
                         "function. This is not allowed.");
            report_note(var->decl_node, "Variable declared here.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        ref->base.value.type        = create_type_ptr(ctx, type);
        ref->base.value.is_comptime = var->value.is_comptime;
        if (type->kind == MIR_TYPE_FN_GROUP || type->kind == MIR_TYPE_TYPE) {
            ref->base.value.addr_mode = MIR_VAM_RVALUE;
        } else {
            ref->base.value.addr_mode = var->is_mutable ? MIR_VAM_LVALUE : MIR_VAM_LVALUE_CONST;
        }
        break;
    }

    default:
        babort("invalid scope entry kind");
    }

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_decl_direct_ref(struct context                   *ctx,
                                            struct mir_instr_decl_direct_ref *ref)
{
    zone();
    bassert(ref->ref && "Missing declaration reference for direct ref.");
    if (ref->ref->kind == MIR_INSTR_DECL_VAR) {
        if (isnotflag(ref->ref->flags, MIR_IS_ANALYZED)) return_zone(ANALYZE_RESULT(POSTPONE, 0));
        struct mir_var *var = ((struct mir_instr_decl_var *)ref->ref)->var;
        bassert(var);
        ++var->ref_count;
        struct mir_type *type = var->value.type;
        bassert(type);
        type                        = create_type_ptr(ctx, type);
        ref->base.value.type        = type;
        ref->base.value.is_comptime = var->value.is_comptime;
        ref->base.value.addr_mode   = var->is_mutable ? MIR_VAM_LVALUE : MIR_VAM_LVALUE_CONST;
    } else if (ref->ref->kind == MIR_INSTR_FN_PROTO) {
        if (isnotflag(ref->ref->flags, MIR_IS_ANALYZED)) return_zone(ANALYZE_RESULT(POSTPONE, 0));
        struct mir_fn *fn = MIR_CEV_READ_AS(struct mir_fn *, &ref->ref->value);
        BL_MAGIC_ASSERT(fn);
        bassert(fn->type && fn->type == ref->ref->value.type);
        ++fn->ref_count;

        struct mir_instr_const *replacement =
            (struct mir_instr_const *)mutate_instr(&ref->base, MIR_INSTR_CONST);
        replacement->volatile_type   = false;
        replacement->base.value.data = (vm_stack_ptr_t)&ref->base.value._tmp;

        replacement->base.value.type = fn->type;
        MIR_CEV_WRITE_AS(struct mir_fn *, &replacement->base.value, fn);
    }
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_arg(struct context UNUSED(*ctx), struct mir_instr_arg *arg)
{
    zone();
    struct mir_fn *fn = arg->base.owner_block->owner_fn;
    bassert(fn);

    struct mir_type *type = mir_get_fn_arg_type(fn->type, arg->i);
    bassert(type);
    arg->base.value.type = type;

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_unreachable(struct context *ctx, struct mir_instr_unreachable *unr)
{
    zone();
    struct mir_fn *abort_fn = lookup_builtin_fn(ctx, BUILTIN_ID_ABORT_FN);
    if (!abort_fn) return_zone(ANALYZE_RESULT(POSTPONE, 0));
    ++abort_fn->ref_count;
    unr->abort_fn = abort_fn;
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_debugbreak(struct context              *ctx,
                                       struct mir_instr_debugbreak *debug_break)
{
    zone();
    struct mir_fn *break_fn = lookup_builtin_fn(ctx, BUILTIN_ID_OS_DEBUG_BREAK_FN);
    if (!break_fn) return_zone(ANALYZE_RESULT(POSTPONE, 0));
    ++break_fn->ref_count;
    debug_break->break_fn = break_fn;
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_test_cases(struct context *ctx, struct mir_instr_test_case UNUSED(*tc))
{
    zone();
    struct id *missing = lookup_builtins_test_cases(ctx);
    if (missing) return_zone(ANALYZE_RESULT(WAITING, missing->hash));
    tc->base.value.type = ctx->builtin_types->t_TestCases_slice;
    if (ctx->testing.expected_test_count == 0) return_zone(ANALYZE_RESULT(PASSED, 0));
    testing_gen_meta(ctx);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_fn_proto(struct context *ctx, struct mir_instr_fn_proto *fn_proto)
{
    zone();
    // resolve type
    if (!fn_proto->base.value.type) {
        struct mir_type *fn_type = NULL;
        struct result    result  = analyze_resolve_type(ctx, fn_proto->type, &fn_type);
        if (result.state != ANALYZE_PASSED) return_zone(result);

        // Analyze user defined type (this must be compared with infered type).
        if (fn_proto->user_type) {
            struct mir_type *user_fn_type = NULL;
            result = analyze_resolve_type(ctx, fn_proto->user_type, &user_fn_type);
            if (result.state != ANALYZE_PASSED) return_zone(result);

            if (!type_cmp(fn_type, user_fn_type)) {
                error_types(
                    ctx, &fn_proto->base, fn_type, user_fn_type, fn_proto->user_type->node, NULL);
            }
        }

        fn_proto->base.value.type = fn_type;
    }

    struct mir_const_expr_value *value = &fn_proto->base.value;

    bassert(value->type && "function has no valid type");
    bassert(value->data);

    // @INCOMPLETE: Read struct mir_fn_poly_recipe here directly?
    struct mir_fn *fn = MIR_CEV_READ_AS(struct mir_fn *, value);
    BL_MAGIC_ASSERT(fn);

    const bool is_polymorph = fn->poly;

    if (isflag(fn->flags, FLAG_TEST_FN)) {
        // We must wait for builtin types for test cases.
        struct id *missing = lookup_builtins_test_cases(ctx);
        if (missing) return_zone(ANALYZE_RESULT(WAITING, missing->hash));
    }
    fn->type          = value->type;
    fn->type->user_id = fn->id;

    bassert(fn->type);

    // Set builtin ID to type if there is one. We must store this information in function
    // type because we need it during call analyze pass, in this case function can be called via
    // pointer or directly, so we don't have access to struct mir_fn instance.
    fn->type->data.fn.builtin_id = fn->builtin_id;

    if (fn->ret_tmp) {
        bassert(fn->ret_tmp->kind == MIR_INSTR_DECL_VAR);
        ((struct mir_instr_decl_var *)fn->ret_tmp)->var->value.type = value->type->data.fn.ret_type;
    }

    char *name_prefix = gettmpstr();
    if (fn->decl_node) {
        struct scope *owner_scope = fn->decl_node->owner_scope;
        bassert(owner_scope);
        scope_get_full_name(&name_prefix, owner_scope);
    }

    // Setup function linkage name, this will be later used by LLVM backend.
    if (fn->id && !fn->linkage_name) { // Has ID and has no linkage name specified.
        // Setup function full name.
        if (strlenu(name_prefix)) {
            const char *fmt = "%s.%s";
            const s32   len = snprintf(NULL, 0, fmt, name_prefix, fn->id->str);
            bassert(len > 0);
            char *buf = scdup(&ctx->assembly->string_cache, NULL, len);
            snprintf(buf, len, fmt, name_prefix, fn->id->str);
            fn->full_name = buf;
        } else {
            fn->full_name = fn->id->str;
        }
        bassert(fn->full_name);

        // Setup function linkage name.
        if (isflag(fn->flags, FLAG_EXTERN) || isflag(fn->flags, FLAG_ENTRY) ||
            isflag(fn->flags, FLAG_EXPORT) || isflag(fn->flags, FLAG_INTRINSIC)) {
            fn->linkage_name = fn->id->str;
        } else {
            // Here we generate unique linkage name
            fn->linkage_name = create_unique_name(ctx, fn->full_name);
        }
    } else if (!fn->linkage_name) {
        // Anonymous function use implicit unique name.
        char *full_name = gettmpstr();
        if (strlenu(name_prefix)) {
            strprint(full_name, "%s%s", name_prefix, IMPL_FN_NAME);
        } else {
            strprint(full_name, IMPL_FN_NAME);
        }
        fn->linkage_name = create_unique_name(ctx, full_name);
        fn->full_name    = fn->linkage_name;
        puttmpstr(full_name);
    }
    puttmpstr(name_prefix);
    bassert(fn->linkage_name);
    if (!fn->full_name) fn->full_name = fn->linkage_name;

    // Check build entry function.
    if (isflag(fn->flags, FLAG_BUILD_ENTRY)) {
        if (sarrlenu(fn->type->data.fn.args) > 0) {
            report_error(INVALID_ARG_COUNT,
                         fn_proto->base.node,
                         "Build entry function cannot take arguments.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        if (fn->type->data.fn.ret_type->kind != MIR_TYPE_VOID) {
            report_error(
                INVALID_TYPE, fn_proto->base.node, "Build entry function cannot return value.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        if (ctx->assembly->target->kind == ASSEMBLY_BUILD_PIPELINE) {
            ctx->assembly->vm_run.build_entry = fn;
        } else {
            report_error(DUPLICATE_ENTRY,
                         fn_proto->base.node,
                         "More than one build entry per assembly is not allowed.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
    }

    bassert(fn->linkage_name && "Function without linkage name!");

    if (isflag(fn->flags, FLAG_EXTERN)) {
        // lookup external function exec handle
        fn->dyncall.extern_entry = assembly_find_extern(ctx->assembly, fn->linkage_name);
        fn->is_fully_analyzed    = true;
    } else if (isflag(fn->flags, FLAG_INTRINSIC)) {
        // For intrinsics we use shorter names defined in user code, so we need username ->
        // internal name mapping in this case.
        const char *intrinsic_name = get_intrinsic(fn->linkage_name);
        if (!intrinsic_name) {
            report_error(UNKNOWN_SYMBOL,
                         fn_proto->base.node,
                         "Unknown compiler intrinsic '%s'.",
                         fn->linkage_name);

            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        fn->dyncall.extern_entry = assembly_find_extern(ctx->assembly, intrinsic_name);
        fn->is_fully_analyzed    = true;
    } else if (is_polymorph) {
        // Nothing to do
    } else {
        // Add entry block of the function into analyze queue.
        struct mir_instr *entry_block = (struct mir_instr *)fn->first_block;
        if (!entry_block) {
            // INCOMPLETE: not the best place to do this check, move into struct ast
            // generation later
            report_error(EXPECTED_BODY, fn_proto->base.node, "Missing function body.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        analyze_schedule(ctx, entry_block);
    }

    bool schedule_llvm_generation = false;
    if (isflag(fn->flags, FLAG_EXPORT)) {
        schedule_llvm_generation = true;
        ++fn->ref_count;
        if (is_polymorph) {
            report_error(UNEXPECTED_DIRECTIVE,
                         fn_proto->base.node,
                         "Polymorph function cannot be exported.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
    }

    if (isflag(fn->flags, FLAG_ENTRY)) {
        schedule_llvm_generation = true;
        fn->ref_count            = NO_REF_COUNTING;
    }

    // Store test case for later use if it's going to be tested.
    if (isflag(fn->flags, FLAG_TEST_FN)) {
        schedule_llvm_generation = true;
        bassert(fn->id && "Test case without name!");

        if (sarrlenu(fn->type->data.fn.args) > 0) {
            report_error(INVALID_ARG_COUNT,
                         fn_proto->base.node,
                         "Test case function cannot have arguments.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        if (fn->type->data.fn.ret_type->kind != MIR_TYPE_VOID) {
            report_error(
                UNEXPECTED_RETURN, fn_proto->base.node, "Test case function cannot return value.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        testing_add_test_case(ctx, fn);
        ++fn->ref_count;
    }

    if (fn->id && fn->scope_entry) {
        if (fn->scope_entry->kind == SCOPE_ENTRY_VOID) {
            report_error(INVALID_NAME, fn->decl_node, "Functions cannot be explicitly unnamed.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        commit_fn(ctx, fn);
    } else if (fn->id) {
        // This is special case for functions generated from polymorph replacement. In general
        // we can replace polymorphs based only on call side information (all argument types are
        // provided) and call instruction will wait until replacement function is analyzed.
        // However polymorph replacement does not exist in any scope and cannot be called
        // directly by user code.
        analyze_notify_provided(ctx, fn->id->hash);
    }

    if (schedule_llvm_generation) arrput(ctx->assembly->MIR.exported_instrs, &fn_proto->base);

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

s32 _group_compare(const void *_first, const void *_second)
{
    struct mir_fn *first  = *(struct mir_fn **)_first;
    struct mir_fn *second = *(struct mir_fn **)_second;
    BL_MAGIC_ASSERT(first);
    BL_MAGIC_ASSERT(second);
    bassert(first->type && second->type);
    return (s32)(first->type->data.fn.argument_hash - second->type->data.fn.argument_hash);
}

struct result analyze_instr_fn_group(struct context *ctx, struct mir_instr_fn_group *group)
{
    zone();
    mir_instrs_t *variants = group->variants;
    bassert(variants);
    const usize vc = sarrlenu(variants);
    bassert(vc);
    for (usize i = 0; i < vc; ++i) {
        struct mir_instr *variant_ref = sarrpeek(variants, i);
        if (isnotflag(variant_ref->flags, MIR_IS_ANALYZED))
            return_zone(ANALYZE_RESULT(POSTPONE, 0));
    }
    struct result result           = ANALYZE_RESULT(PASSED, 0);
    mir_types_t  *variant_types    = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    mir_fns_t    *variant_fns      = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    mir_fns_t     validation_queue = SARR_ZERO;
    sarrsetlen(variant_types, vc);
    sarrsetlen(variant_fns, vc);
    sarrsetlen(&validation_queue, vc);
    for (usize i = 0; i < vc; ++i) {
        struct mir_instr **variant_ref = &sarrpeek(variants, i);
        if (analyze_slot(ctx, &analyze_slot_conf_basic, variant_ref, NULL) != ANALYZE_PASSED) {
            result = ANALYZE_RESULT(FAILED, 0);
            goto FINALLY;
        }
        struct mir_instr *variant = *variant_ref;
        if (variant->value.type->kind != MIR_TYPE_FN) {
            report_error(EXPECTED_FUNC, variant->node, "Expected a function name.");
            result = ANALYZE_RESULT(FAILED, 0);
            goto FINALLY;
        }

        bassert(mir_is_comptime(variant));
        struct mir_fn *fn = MIR_CEV_READ_AS(struct mir_fn *, &variant->value);
        BL_MAGIC_ASSERT(fn);
        bassert(fn->type && "Missing function type!");
        sarrpeek(variant_fns, i)       = fn;
        sarrpeek(variant_types, i)     = fn->type;
        sarrpeek(&validation_queue, i) = fn;
        if (variant->kind == MIR_INSTR_FN_PROTO) {
            ++fn->ref_count;
        }
    }
    // Validate group.
    qsort(sarrdata(&validation_queue),
          sarrlenu(&validation_queue),
          sizeof(struct mir_fn *),
          &_group_compare);
    struct mir_fn *prev_fn = NULL;
    for (usize i = sarrlenu(&validation_queue); i-- > 0;) {
        struct mir_fn *it = sarrpeek(&validation_queue, i);
        const hash_t   h1 = it->type->data.fn.argument_hash;
        const hash_t   h2 = prev_fn ? prev_fn->type->data.fn.argument_hash : 0;
        if (h1 == h2) {
            report_error(AMBIGUOUS, it->decl_node, "Function overload is ambiguous in group.");
            report_note(group->base.node, "Group defined here:");
        }
        prev_fn = it;
    }
    group->base.value.type = create_type_fn_group(ctx, NULL, variant_types);
    MIR_CEV_WRITE_AS(struct mir_fn_group *,
                     &group->base.value,
                     create_fn_group(ctx, group->base.node, variant_fns));
FINALLY:
    sarrfree(&validation_queue);
    return_zone(result);
}

struct result analyze_instr_cond_br(struct context *ctx, struct mir_instr_cond_br *br)
{
    zone();
    bassert(br->cond && br->then_block && br->else_block);
    bassert(isflag(br->cond->flags, MIR_IS_ANALYZED));
    if (analyze_slot(ctx, &analyze_slot_conf_default, &br->cond, ctx->builtin_types->t_bool) !=
        ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }
    const bool is_condition_comptime = mir_is_comptime(br->cond);
    if (br->is_static && !is_condition_comptime) {
        report_error(EXPECTED_COMPTIME,
                     br->cond->node,
                     "Static if expression is supposed to be known in compile time.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }
    // Compile-time known conditional break can be later evaluated into direct break.
    br->base.value.is_comptime = is_condition_comptime;
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_br(struct context UNUSED(*ctx), struct mir_instr_br *br)
{
    zone();
    bassert(br->then_block);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_switch(struct context *ctx, struct mir_instr_switch *sw)
{
    zone();
    if (analyze_slot(ctx, &analyze_slot_conf_basic, &sw->value, NULL) != ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    struct mir_type *expected_case_type = sw->value->value.type;
    bassert(expected_case_type);

    if (expected_case_type->kind != MIR_TYPE_INT && expected_case_type->kind != MIR_TYPE_ENUM) {
        report_error(INVALID_TYPE,
                     sw->value->node,
                     "Invalid type of switch expression. Only integer types and "
                     "enums can be used.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    if (!sarrlen(sw->cases)) {
        report_warning(sw->base.node, "Empty switch statement.");
        return_zone(ANALYZE_RESULT(PASSED, 0));
    }

    struct {
        s64                     key;
        struct mir_switch_case *value;
    } *presented = NULL;
    for (usize i = sarrlenu(sw->cases); i-- > 0;) {
        struct mir_switch_case *c = &sarrpeek(sw->cases, i);
        if (!mir_is_comptime(c->on_value)) {
            report_error(EXPECTED_COMPTIME,
                         c->on_value->node,
                         "Switch case value must be compile-time known.");
            hmfree(presented);
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        if (analyze_slot(ctx, &analyze_slot_conf_default, &c->on_value, expected_case_type) !=
            ANALYZE_PASSED) {
            hmfree(presented);
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        { // validate value
            const s64 v               = MIR_CEV_READ_AS(s64, &c->on_value->value);
            const s64 collision_index = hmgeti(presented, v);
            if (collision_index != -1) {
                struct mir_switch_case *ce = presented[collision_index].value;
                report_error(DUPLICIT_SWITCH_CASE,
                             c->on_value->node,
                             "Switch already contains case for this value!");
                report_note(ce->on_value->node, "Same value found here.");
                hmfree(presented);
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }
            hmput(presented, v, c);
        }
    }
    hmfree(presented);

    mir_variants_t *variants = expected_case_type->data.enm.variants;
    s64 expected_case_count  = expected_case_type->kind == MIR_TYPE_ENUM ? sarrlen(variants) : -1;

    if ((expected_case_count > sarrlen(sw->cases)) && !sw->has_user_defined_default) {
        report_warning(sw->base.node, "Switch does not handle all possible enumerator values.");

        bassert(expected_case_type->kind == MIR_TYPE_ENUM);
        for (usize i = 0; i < sarrlenu(variants); ++i) {
            struct mir_variant *variant = sarrpeek(variants, i);
            bool                hit     = false;
            for (usize i2 = 0; i2 < sarrlenu(sw->cases); ++i2) {
                struct mir_switch_case *c             = &sarrpeek(sw->cases, i2);
                const s64               on_value      = MIR_CEV_READ_AS(s64, &c->on_value->value);
                const s64               variant_value = variant->value;
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
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_load(struct context *ctx, struct mir_instr_load *load)
{
    zone();
    struct mir_instr *src = load->src;
    bassert(src);
    if (!mir_is_pointer_type(src->value.type)) {
        report_error(INVALID_TYPE, src->node, "Expected pointer.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    struct mir_type *type = mir_deref_type(src->value.type);
    bassert(type);
    load->base.value.type        = type;
    load->base.value.is_comptime = src->value.is_comptime;
    load->base.value.addr_mode   = src->value.addr_mode;
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_fn(struct context *ctx, struct mir_instr_type_fn *type_fn)
{
    zone();
    bassert(type_fn->ret_type ? isflag(type_fn->ret_type->flags, MIR_IS_ANALYZED) : true);

    bool       is_vargs         = false;
    bool       has_default_args = false;
    const bool is_polymorph     = type_fn->is_polymorph;

    mir_args_t *args = NULL;
    if (type_fn->args) {
        const usize argc = sarrlenu(type_fn->args);

        for (usize i = 0; i < argc; ++i) {
            bassert(sarrpeek(type_fn->args, i)->kind == MIR_INSTR_DECL_ARG);
            struct mir_instr_decl_arg *decl_arg =
                (struct mir_instr_decl_arg *)sarrpeek(type_fn->args, i);
            struct mir_arg *arg = decl_arg->arg;
            if (arg->value && isnotflag(arg->value->flags, MIR_IS_ANALYZED)) {
                return_zone(ANALYZE_RESULT(POSTPONE, 0));
            }
        }

        args = arena_safe_alloc(&ctx->assembly->arenas.sarr);
        for (usize i = 0; i < argc; ++i) {
            bassert(sarrpeek(type_fn->args, i)->kind == MIR_INSTR_DECL_ARG);
            struct mir_instr_decl_arg **arg_ref =
                (struct mir_instr_decl_arg **)&sarrpeek(type_fn->args, i);
            bassert((*arg_ref)->base.value.is_comptime);

            if (analyze_slot(ctx, &analyze_slot_conf_basic, (struct mir_instr **)arg_ref, NULL) !=
                ANALYZE_PASSED) {
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }

            struct mir_arg *arg = (*arg_ref)->arg;
            bassert(arg);
            bassert(arg->type && "Unknown argument type!");

            // Validate arg type
            switch (arg->type->kind) {
            case MIR_TYPE_INVALID:
            case MIR_TYPE_TYPE:
            case MIR_TYPE_VOID:
            case MIR_TYPE_FN:
            case MIR_TYPE_FN_GROUP:
            case MIR_TYPE_NAMED_SCOPE: {
                char type_name[256];
                mir_type_to_str(type_name, 256, arg->type, true);
                report_error(INVALID_TYPE,
                             arg->decl_node,
                             "Invalid function argument type '%s'.",
                             type_name);
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }
            default:
                break;
            }

            is_vargs         = arg->type->kind == MIR_TYPE_VARGS ? true : is_vargs;
            has_default_args = arg->value ? true : has_default_args;
            if (is_vargs && i != sarrlenu(type_fn->args) - 1) {
                report_error(INVALID_TYPE,
                             arg->decl_node,
                             "VArgs function argument must be last in "
                             "the argument list.");
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }
            if (is_vargs && has_default_args) {
                struct mir_instr *arg_prev = sarrpeek(type_fn->args, i - 1);
                report_error(INVALID_TYPE,
                             arg_prev->node,
                             "Argument with default value cannot be used with VArgs "
                             "presented in the function argument list.");
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }
            if (!arg->value && has_default_args) {
                struct mir_instr *arg_prev = sarrpeek(type_fn->args, i - 1);
                report_error(INVALID_TYPE,
                             arg_prev->node,
                             "All arguments with default value must be listed last "
                             "in the function argument list. Before arguments "
                             "without default value.");
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }
            sarrput(args, arg);
        }
    }

    struct mir_type *ret_type = NULL;
    if (type_fn->ret_type) {
        if (analyze_slot(ctx, &analyze_slot_conf_basic, &type_fn->ret_type, NULL) !=
            ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        bassert(type_fn->ret_type->value.is_comptime);
        ret_type = MIR_CEV_READ_AS(struct mir_type *, &type_fn->ret_type->value);
        BL_MAGIC_ASSERT(ret_type);
        // Disable polymorph master as return type.
        if (ret_type->kind == MIR_TYPE_POLY && ret_type->data.poly.is_master) {
            report_error(INVALID_TYPE,
                         type_fn->ret_type->node,
                         "Polymorph master type cannot be used as return type.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
    }

    struct mir_type *result_type =
        create_type_fn(ctx, NULL, ret_type, args, is_vargs, has_default_args, is_polymorph);
    MIR_CEV_WRITE_AS(struct mir_type *, &type_fn->base.value, result_type);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_fn_group(struct context                 *ctx,
                                          struct mir_instr_type_fn_group *group)
{
    zone();
    mir_instrs_t *variants = group->variants;
    bassert(variants);
    const usize varc = sarrlenu(variants);
    if (varc == 0) {
        report_error(INVALID_TYPE,
                     group->base.node,
                     "Function group type must contain one function type at least.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }
    mir_types_t *variant_types = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    sarrsetlen(variant_types, varc);
    for (usize i = 0; i < varc; ++i) {
        struct mir_instr **variant_ref = &sarrpeek(variants, i);
        if (analyze_slot(ctx, &analyze_slot_conf_basic, variant_ref, NULL) != ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        struct mir_instr *variant = *variant_ref;
        bassert(variant->value.type);
        if (variant->value.type->kind != MIR_TYPE_TYPE) {
            report_error(INVALID_TYPE,
                         variant->node,
                         "Function group type variant is supposed to be function type.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        struct mir_type *variant_type = MIR_CEV_READ_AS(struct mir_type *, &variant->value);
        BL_MAGIC_ASSERT(variant_type);
        if (variant_type->kind != MIR_TYPE_FN) {
            report_error(INVALID_TYPE,
                         variant->node,
                         "Function group type variant is supposed to be function type.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        sarrpeek(variant_types, i) = variant_type;
    }
    MIR_CEV_WRITE_AS(
        struct mir_type *, &group->base.value, create_type_fn_group(ctx, NULL, variant_types));
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_decl_member(struct context *ctx, struct mir_instr_decl_member *decl)
{
    zone();
    struct mir_member *member    = decl->member;
    mir_instrs_t      *tags      = decl->tags;
    s32                tag_group = 0;

    // Analyze struct member tags.
    for (usize i = 0; i < sarrlenu(tags); ++i) {
        struct mir_instr **tag = &sarrpeek(tags, i);
        if (analyze_slot(ctx, &analyze_slot_conf_default, tag, ctx->builtin_types->t_s32) !=
            ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        if (!mir_is_comptime(*tag)) {
            report_error(
                EXPECTED_CONST, (*tag)->node, "Struct member tag must be compile-time constant.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        // NOTE:
        // Tag values are used in the same way as flags, here we produce
        // mergin of all into one integer value.
        tag_group |= MIR_CEV_READ_AS(s32, &(*tag)->value);
    }

    member->tags = tag_group;

    if (analyze_slot(ctx, &analyze_slot_conf_basic, &decl->type, NULL) != ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    // NOTE: Members will be provided by instr type struct because we need to
    // know right ordering of members inside structure layout. (index and llvm
    // element offet need to be calculated)
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_decl_variant(struct context                *ctx,
                                         struct mir_instr_decl_variant *variant_instr)
{
    zone();
    struct mir_variant *variant = variant_instr->variant;
    bassert(variant && "Missing variant.");
    const bool       is_flags  = variant_instr->is_flags;
    struct mir_type *base_type = NULL;
    if (variant_instr->base_type && variant_instr->base_type->kind == MIR_INSTR_CONST) {
        // In case the type resolver was already called (call in supposed to be converted into
        // the compile time constant containing resolved type).
        base_type = MIR_CEV_READ_AS(struct mir_type *, &variant_instr->base_type->value);
    } else if (variant_instr->base_type) {
        struct result result = analyze_resolve_type(ctx, variant_instr->base_type, &base_type);
        if (result.state != ANALYZE_PASSED) return_zone(result);
    } else {
        base_type = is_flags ? ctx->builtin_types->t_u32 : ctx->builtin_types->t_s32;
    }
    BL_MAGIC_ASSERT(base_type);

    if (variant_instr->value) {
        // User defined initialization value.
        if (!mir_is_comptime(variant_instr->value)) {
            report_error(INVALID_EXPR,
                         variant_instr->value->node,
                         "Enum variant value must be compile time known.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        if (analyze_slot(ctx, &analyze_slot_conf_default, &variant_instr->value, base_type) !=
            ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        // Setup value.
        variant_instr->variant->value      = MIR_CEV_READ_AS(u64, &variant_instr->value->value);
        variant_instr->variant->value_type = variant_instr->value->value.type;
    } else if (variant_instr->prev_variant) {
        u64       value      = 0;
        const u64 prev_value = variant_instr->prev_variant->value;
        const s32 bitcount   = base_type->data.integer.bitcount;
        const u64 max_value  = bitcount == 64 ? ULLONG_MAX : (1ull << bitcount) - 1;
        if (is_flags && prev_value == 0) {
            value = 1;
        } else if (is_flags) {
            value = (prev_value << 1) & ~prev_value;
        } else {
            value = prev_value + 1;
        }
        const bool u64overflow = value == 0;
        if (is_flags && (value > max_value || u64overflow)) {
            // @Incomplete: Detect overflow also for regular enums?
            char base_type_name[256];
            mir_type_to_str(base_type_name, 256, base_type, true);
            report_error(NUM_LIT_OVERFLOW,
                         variant_instr->base.node,
                         "Enum variant value overflow on variant '%s', maximum value for type "
                         "'%s' is %llu.",
                         variant->id->str,
                         base_type_name,
                         max_value);
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        variant_instr->variant->value      = value;
        variant_instr->variant->value_type = variant_instr->prev_variant->value_type;
    } else {
        variant_instr->variant->value      = is_flags ? 1 : 0;
        variant_instr->variant->value_type = base_type;
    }
    if (variant->entry->kind == SCOPE_ENTRY_VOID) {
        report_error(
            INVALID_NAME, variant_instr->base.node, "Enum variant cannot be explicitly unnamed.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }
    commit_variant(ctx, variant);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_decl_arg(struct context *ctx, struct mir_instr_decl_arg *decl)
{
    zone();
    struct mir_arg *arg = decl->arg;
    bassert(arg);
    if (decl->type) {
        // Variable type is explicitly defined.
        if (decl->type->kind == MIR_INSTR_CALL) {
            struct result result = analyze_resolve_type(ctx, decl->type, &arg->type);
            if (result.state != ANALYZE_PASSED) return_zone(result);
        } else {
            if (analyze_slot(ctx, &analyze_slot_conf_basic, &decl->type, NULL) != ANALYZE_PASSED) {
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }
            arg->type = MIR_CEV_READ_AS(struct mir_type *, &decl->type->value);
            BL_MAGIC_ASSERT(arg->type);
        }
        // @NOTE: Argument default value is generated as an implicit global constant
        // variable with proper expected type defined, so there is no need to do any type
        // validation with user type, variable analyze pass will do it for us.*/
    } else {
        // There is no explicitly defined argument type, but we have default argument value
        // to infer type from.
        bassert(arg->value);
        if (isnotflag(arg->value->flags, MIR_IS_ANALYZED)) {
            // @PERFORMANCE: WAITING is preferred here, but we don't have ID to wait
            // for.
            return_zone(ANALYZE_RESULT(POSTPONE, 0));
        }
        if (arg->value->kind == MIR_INSTR_DECL_VAR) {
            struct mir_var *var = ((struct mir_instr_decl_var *)arg->value)->var;
            bassert(var);
            if (!var->is_analyzed) return_zone(ANALYZE_RESULT(POSTPONE, 0));
            arg->type = var->value.type;
        } else {
            arg->type = arg->value->value.type;
        }
    }
    bassert(arg->type && "Invalid argument type!");
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_struct(struct context               *ctx,
                                        struct mir_instr_type_struct *type_struct)
{
    zone();
    mir_members_t   *members   = NULL;
    struct mir_type *base_type = NULL;
    const bool       is_union  = type_struct->is_union;

    if (type_struct->members) {
        struct mir_instr            **member_instr;
        struct mir_instr_decl_member *decl_member;
        struct mir_type              *member_type;
        members = arena_safe_alloc(&ctx->assembly->arenas.sarr);
        for (usize i = 0; i < sarrlenu(type_struct->members); ++i) {
            member_instr = &sarrpeek(type_struct->members, i);
            if (analyze_slot(ctx, &analyze_slot_conf_basic, member_instr, NULL) != ANALYZE_PASSED) {
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }
            decl_member = (struct mir_instr_decl_member *)*member_instr;
            bassert(decl_member->base.kind == MIR_INSTR_DECL_MEMBER);
            bassert(mir_is_comptime(&decl_member->base));

            // solve member type
            member_type = MIR_CEV_READ_AS(struct mir_type *, &decl_member->type->value);
            BL_MAGIC_ASSERT(member_type);
            if (member_type->kind == MIR_TYPE_FN) {
                report_error(INVALID_TYPE,
                             (*member_instr)->node,
                             "Invalid type of the structure member, functions can "
                             "be referenced only by pointers.");
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }
            if (member_type->kind == MIR_TYPE_TYPE) {
                report_error(INVALID_TYPE,
                             decl_member->type->node,
                             "Generic 'type' cannot be used as struct member type.");
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }

            // setup and provide member
            struct mir_member *member = decl_member->member;
            bassert(member);
            member->type            = member_type;
            member->index           = (s64)i;
            member->is_parent_union = is_union;

            if (member->is_base) {
                bassert(!base_type && "Structure cannot have more than one base type!");
                base_type = member_type;
            }

            sarrput(members, member);
            commit_member(ctx, member);
        }
    }

    struct mir_type *result_type = NULL;

    if (type_struct->fwd_decl) {
        // Type has fwd declaration. In this case we set all desired information
        // about struct type into previously created forward declaration.
        result_type = complete_type_struct(ctx,
                                           type_struct->fwd_decl,
                                           type_struct->scope,
                                           members,
                                           base_type,
                                           type_struct->is_packed,
                                           type_struct->is_union,
                                           type_struct->is_multiple_return_type);

        analyze_notify_provided(ctx, result_type->user_id->hash);
    } else {
        result_type = create_type_struct(ctx,
                                         MIR_TYPE_STRUCT,
                                         type_struct->id,
                                         type_struct->scope,
                                         members,
                                         base_type,
                                         is_union,
                                         type_struct->is_packed,
                                         type_struct->is_multiple_return_type);
    }

    bassert(result_type);
    MIR_CEV_WRITE_AS(struct mir_type *, &type_struct->base.value, result_type);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_slice(struct context *ctx, struct mir_instr_type_slice *type_slice)
{
    zone();
    bassert(type_slice->elem_type);

    if (analyze_slot(ctx, &analyze_slot_conf_basic, &type_slice->elem_type, NULL) !=
        ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    struct id *id = NULL;
    if (type_slice->base.node && type_slice->base.node->kind == AST_IDENT) {
        id = &type_slice->base.node->data.ident.id;
    }

    if (type_slice->elem_type->value.type->kind != MIR_TYPE_TYPE) {
        report_error(INVALID_TYPE, type_slice->elem_type->node, "Expected type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    bassert(mir_is_comptime(type_slice->elem_type) && "This should be an error");
    struct mir_type *elem_type = MIR_CEV_READ_AS(struct mir_type *, &type_slice->elem_type->value);
    BL_MAGIC_ASSERT(elem_type);

    if (elem_type->kind == MIR_TYPE_TYPE) {
        report_error(INVALID_TYPE,
                     type_slice->elem_type->node,
                     "Generic 'type' cannot be used as slice base type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    elem_type = create_type_ptr(ctx, elem_type);

    MIR_CEV_WRITE_AS(
        struct mir_type *, &type_slice->base.value, CREATE_TYPE_STRUCT_SLICE(ctx, id, elem_type));

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_dynarr(struct context                *ctx,
                                        struct mir_instr_type_dyn_arr *type_dynarr)
{
    zone();
    // We need TypeInfo initialized because it's needed as member of dynamic array type!
    struct id *missing_rtti_type_id = lookup_builtins_rtti(ctx);
    if (missing_rtti_type_id) {
        return_zone(ANALYZE_RESULT(WAITING, missing_rtti_type_id->hash));
    }

    bassert(type_dynarr->elem_type);

    if (analyze_slot(ctx, &analyze_slot_conf_basic, &type_dynarr->elem_type, NULL) !=
        ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    struct id *id = NULL;
    if (type_dynarr->base.node && type_dynarr->base.node->kind == AST_IDENT) {
        id = &type_dynarr->base.node->data.ident.id;
    }

    if (type_dynarr->elem_type->value.type->kind != MIR_TYPE_TYPE) {
        report_error(INVALID_TYPE, type_dynarr->elem_type->node, "Expected type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    bassert(mir_is_comptime(type_dynarr->elem_type) && "This should be an error");
    struct mir_type *elem_type = MIR_CEV_READ_AS(struct mir_type *, &type_dynarr->elem_type->value);
    BL_MAGIC_ASSERT(elem_type);

    if (elem_type->kind == MIR_TYPE_TYPE) {
        report_error(INVALID_TYPE,
                     type_dynarr->elem_type->node,
                     "Generic 'type' cannot be used as dynamic array base type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    elem_type = create_type_ptr(ctx, elem_type);

    MIR_CEV_WRITE_AS(
        struct mir_type *, &type_dynarr->base.value, create_type_struct_dynarr(ctx, id, elem_type));

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_vargs(struct context *ctx, struct mir_instr_type_vargs *type_vargs)
{
    zone();
    struct mir_type *elem_type = NULL;
    if (type_vargs->elem_type) {
        if (analyze_slot(ctx, &analyze_slot_conf_basic, &type_vargs->elem_type, NULL) !=
            ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        if (type_vargs->elem_type->value.type->kind != MIR_TYPE_TYPE) {
            report_error(INVALID_TYPE, type_vargs->elem_type->node, "Expected type.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }

        bassert(mir_is_comptime(type_vargs->elem_type) && "This should be an error");
        elem_type = MIR_CEV_READ_AS(struct mir_type *, &type_vargs->elem_type->value);
        BL_MAGIC_ASSERT(elem_type);
    } else {
        // use Any
        elem_type = lookup_builtin_type(ctx, BUILTIN_ID_ANY);
        if (!elem_type) return_zone(ANALYZE_RESULT(WAITING, builtin_ids[BUILTIN_ID_ANY].hash));
    }
    bassert(elem_type);
    elem_type = create_type_ptr(ctx, elem_type);
    MIR_CEV_WRITE_AS(
        struct mir_type *, &type_vargs->base.value, CREATE_TYPE_STRUCT_VARGS(ctx, NULL, elem_type));

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_array(struct context *ctx, struct mir_instr_type_array *type_arr)
{
    zone();
    bassert(type_arr->base.value.type);
    bassert(isflag(type_arr->elem_type->flags, MIR_IS_ANALYZED));

    if (analyze_slot(ctx, &analyze_slot_conf_default, &type_arr->len, ctx->builtin_types->t_s64) !=
        ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    if (analyze_slot(ctx, &analyze_slot_conf_basic, &type_arr->elem_type, NULL) != ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    // len
    if (!mir_is_comptime(type_arr->len)) {
        report_error(
            EXPECTED_CONST, type_arr->len->node, "Array size must be compile-time constant.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    if (type_arr->elem_type->value.type->kind != MIR_TYPE_TYPE) {
        report_error(INVALID_TYPE, type_arr->elem_type->node, "Expected type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    const s64 len = MIR_CEV_READ_AS(s64, &type_arr->len->value);
    if (len == 0) {
        report_error(INVALID_ARR_SIZE, type_arr->len->node, "Array size cannot be 0.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    // elem type
    bassert(mir_is_comptime(type_arr->elem_type));

    struct mir_type *elem_type = MIR_CEV_READ_AS(struct mir_type *, &type_arr->elem_type->value);
    BL_MAGIC_ASSERT(elem_type);

    if (elem_type->kind == MIR_TYPE_TYPE) {
        report_error(INVALID_TYPE,
                     type_arr->elem_type->node,
                     "Generic 'type' cannot be used as array element type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    MIR_CEV_WRITE_AS(struct mir_type *,
                     &type_arr->base.value,
                     create_type_array(ctx, type_arr->id, elem_type, len));
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_poly(struct context *ctx, struct mir_instr_type_poly *type_poly)
{
    zone();
    // nothing to do here...
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_enum(struct context *ctx, struct mir_instr_type_enum *type_enum)
{
    zone();
    mir_instrs_t *variant_instrs = type_enum->variants;
    struct scope *scope          = type_enum->scope;
    const bool    is_flags       = type_enum->is_flags;
    bassert(scope);
    bassert(sarrlenu(variant_instrs));
    // Validate and setup enum base type.
    struct mir_type *base_type;
    if (type_enum->base_type) {
        // @Incomplete: Enum should probably use type resoler as well?
        base_type = MIR_CEV_READ_AS(struct mir_type *, &type_enum->base_type->value);
        BL_MAGIC_ASSERT(base_type);

        // Enum type must be integer!
        if (base_type->kind != MIR_TYPE_INT) {
            report_error(INVALID_TYPE,
                         type_enum->base_type->node,
                         "Base type of enumerator must be an integer type.");
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        if (is_flags && base_type->data.integer.is_signed) {
            report_warning(type_enum->base_type->node,
                           "Base type of 'flags' enumerator should be unsigned number.");
        }
    } else {
        base_type = is_flags ? ctx->builtin_types->t_u32 : ctx->builtin_types->t_s32;
    }
    bassert(base_type && "Invalid enum base type.");
    mir_variants_t *variants = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    for (usize i = 0; i < sarrlenu(variant_instrs); ++i) {
        struct mir_instr              *it            = sarrpeek(variant_instrs, i);
        struct mir_instr_decl_variant *variant_instr = (struct mir_instr_decl_variant *)it;
        struct mir_variant            *variant       = variant_instr->variant;
        bassert(variant && "Missing variant.");
        sarrput(variants, variant);
    }
    struct mir_type *type =
        create_type_enum(ctx, type_enum->id, scope, base_type, variants, is_flags);
    MIR_CEV_WRITE_AS(struct mir_type *, &type_enum->base.value, type);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_type_ptr(struct context *ctx, struct mir_instr_type_ptr *type_ptr)
{
    zone();
    bassert(type_ptr->type);

    if (analyze_slot(ctx, &analyze_slot_conf_basic, &type_ptr->type, NULL) != ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    if (!mir_is_comptime(type_ptr->type)) {
        report_error(INVALID_TYPE,
                     type_ptr->type->node,
                     "Expected compile time known type after '*' pointer type declaration.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    if (type_ptr->type->value.type->kind != MIR_TYPE_TYPE) {
        report_error(INVALID_TYPE, type_ptr->type->node, "Expected type name.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    struct mir_type *src_type_value = MIR_CEV_READ_AS(struct mir_type *, &type_ptr->type->value);
    BL_MAGIC_ASSERT(src_type_value);

    if (src_type_value->kind == MIR_TYPE_TYPE) {
        report_error(INVALID_TYPE, type_ptr->base.node, "Cannot create pointer to type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    MIR_CEV_WRITE_AS(
        struct mir_type *, &type_ptr->base.value, create_type_ptr(ctx, src_type_value));
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

static INLINE bool is_type_valid_for_binop(const struct mir_type *type, const enum binop_kind op)
{
    bassert(type);
    switch (type->kind) {
    case MIR_TYPE_INT:
    case MIR_TYPE_NULL:
    case MIR_TYPE_REAL:
    case MIR_TYPE_PTR:
    case MIR_TYPE_POLY:
        return true;
    case MIR_TYPE_BOOL:
        return ast_binop_is_logic(op);
    case MIR_TYPE_TYPE:
    case MIR_TYPE_ENUM:
        return op == BINOP_EQ || op == BINOP_NEQ;
    default:
        break;
    }
    return false;
}

struct result analyze_instr_binop(struct context *ctx, struct mir_instr_binop *binop)
{
    zone();

    { // Handle type propagation.
        struct mir_type *lhs_type = binop->lhs->value.type;
        struct mir_type *rhs_type = binop->rhs->value.type;

        if (is_load_needed(binop->lhs)) lhs_type = mir_deref_type(lhs_type);
        if (is_load_needed(binop->rhs)) rhs_type = mir_deref_type(rhs_type);

        const bool lhs_is_null = binop->lhs->value.type->kind == MIR_TYPE_NULL;
        const bool can_propagate_RtoL =
            can_impl_cast(lhs_type, rhs_type) || is_instr_type_volatile(binop->lhs);

        if (can_propagate_RtoL) {
            // Propagate right hand side expression type to the left.
            if (analyze_slot(ctx, &analyze_slot_conf_default, &binop->lhs, rhs_type) !=
                ANALYZE_PASSED)
                return_zone(ANALYZE_RESULT(FAILED, 0));

            if (analyze_slot(ctx, &analyze_slot_conf_basic, &binop->rhs, NULL) != ANALYZE_PASSED)
                return_zone(ANALYZE_RESULT(FAILED, 0));
        } else {
            // Propagate left hand side expression type to the right.
            if (analyze_slot(ctx, &analyze_slot_conf_basic, &binop->lhs, NULL) != ANALYZE_PASSED)
                return_zone(ANALYZE_RESULT(FAILED, 0));

            if (analyze_slot(ctx,
                             lhs_is_null ? &analyze_slot_conf_basic : &analyze_slot_conf_default,
                             &binop->rhs,
                             lhs_is_null ? NULL : binop->lhs->value.type) != ANALYZE_PASSED)
                return_zone(ANALYZE_RESULT(FAILED, 0));

            if (lhs_is_null) {
                if (analyze_stage_set_null(ctx, &binop->lhs, binop->rhs->value.type, false) !=
                    ANALYZE_STAGE_BREAK)
                    return_zone(ANALYZE_RESULT(FAILED, 0));
            }
        }
    }
    struct mir_instr *lhs = binop->lhs;
    struct mir_instr *rhs = binop->rhs;
    bassert(lhs && rhs);
    bassert(isflag(lhs->flags, MIR_IS_ANALYZED));
    bassert(isflag(rhs->flags, MIR_IS_ANALYZED));
    const bool lhs_valid = is_type_valid_for_binop(lhs->value.type, binop->op);
    const bool rhs_valid = is_type_valid_for_binop(rhs->value.type, binop->op);
    if (!(lhs_valid && rhs_valid)) {
        error_types(ctx,
                    &binop->base,
                    lhs->value.type,
                    rhs->value.type,
                    binop->base.node,
                    "Invalid operation for %s type.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }
    struct mir_type *type =
        ast_binop_is_logic(binop->op) ? ctx->builtin_types->t_bool : lhs->value.type;
    bassert(type);
    binop->base.value.type = type;
    // when binary operation has lhs and rhs values known in compile it is known
    // in compile time also
    binop->base.value.is_comptime = lhs->value.is_comptime && rhs->value.is_comptime;
    binop->base.value.addr_mode   = MIR_VAM_RVALUE;
    binop->volatile_type          = is_instr_type_volatile(lhs) && is_instr_type_volatile(rhs);

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_call_loc(struct context *ctx, struct mir_instr_call_loc *loc)
{
    zone();
    struct id *missing = lookup_builtins_code_loc(ctx);
    if (missing) return_zone(ANALYZE_RESULT(WAITING, missing->hash));
    loc->base.value.type = ctx->builtin_types->t_CodeLocation_ptr;
    if (!loc->call_location) return_zone(ANALYZE_RESULT(PASSED, 0));

    struct mir_type *type = ctx->builtin_types->t_CodeLocation;
    struct mir_var  *var  = create_var_impl(ctx, NULL, IMPL_CALL_LOC, type, false, true, true);
    vm_alloc_global(ctx->vm, ctx->assembly, var);

    vm_stack_ptr_t   dest           = vm_read_var(ctx->vm, var);
    struct mir_type *dest_file_type = mir_get_struct_elem_type(type, 0);
    vm_stack_ptr_t   dest_file      = vm_get_struct_elem_ptr(ctx->assembly, type, dest, 0);
    struct mir_type *dest_line_type = mir_get_struct_elem_type(type, 1);
    vm_stack_ptr_t   dest_line      = vm_get_struct_elem_ptr(ctx->assembly, type, dest, 1);
    struct mir_type *dest_hash_type = mir_get_struct_elem_type(type, 2);
    vm_stack_ptr_t   dest_hash      = vm_get_struct_elem_ptr(ctx->assembly, type, dest, 2);

    const char *filepath = loc->call_location->unit->filepath;
    bassert(filepath);

    char *str_hash = gettmpstr();
    strprint(str_hash, "%s%d", filepath, loc->call_location->line);
    const hash_t hash = strhash(str_hash);
    puttmpstr(str_hash);

    vm_write_string(ctx->vm, dest_file_type, dest_file, filepath, (s64)strlen(filepath));
    vm_write_int(dest_line_type, dest_line, (u64)loc->call_location->line);
    vm_write_int(dest_hash_type, dest_hash, (u64)hash);

    loc->meta_var = var;
    loc->hash     = hash;
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_msg(struct context *ctx, struct mir_instr_msg *msg)
{
    switch (msg->kind) {
    case AST_MSG_WARNING:
        report_warning(msg->base.node, "%s", msg->text);
        return ANALYZE_RESULT(PASSED, 0);
    case AST_MSG_ERROR:
        report_error(USER, msg->base.node, "%s", msg->text);
        return ANALYZE_RESULT(FAILED, 0);
    }
    BL_UNREACHABLE;
}

struct result analyze_instr_unop(struct context *ctx, struct mir_instr_unop *unop)
{
    zone();
    struct mir_type *expected_type = unop->op == UNOP_NOT ? ctx->builtin_types->t_bool : NULL;
    const struct slot_config *conf =
        unop->op == UNOP_NOT ? &analyze_slot_conf_default : &analyze_slot_conf_basic;

    if (analyze_slot(ctx, conf, &unop->expr, expected_type) != ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    bassert(unop->expr && isflag(unop->expr->flags, MIR_IS_ANALYZED));

    struct mir_type *expr_type = unop->expr->value.type;
    bassert(expr_type);

    switch (unop->op) {
    case UNOP_NOT: {
        if (expr_type->kind != MIR_TYPE_BOOL) return_zone(ANALYZE_RESULT(FAILED, 0));
        break;
    }

    case UNOP_BIT_NOT: {
        if (expr_type->kind != MIR_TYPE_INT) {
            char tmp[256];
            mir_type_to_str(tmp, 256, expr_type, true);
            report_error_after(INVALID_TYPE,
                               unop->base.node,
                               "Invalid operation for type '%s'. This operation "
                               "is valid for integer types only.",
                               tmp);
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        break;
    }

    case UNOP_POS:
    case UNOP_NEG: {
        if (expr_type->kind != MIR_TYPE_INT && expr_type->kind != MIR_TYPE_REAL) {
            char tmp[256];
            mir_type_to_str(tmp, 256, expr_type, true);
            report_error_after(INVALID_TYPE,
                               unop->base.node,
                               "Invalid operation for type '%s'. This operation "
                               "is valid for integer or real types only.",
                               tmp);
            return_zone(ANALYZE_RESULT(FAILED, 0));
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

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_const(struct context UNUSED(*ctx), struct mir_instr_const *cnst)
{
    zone();
    bassert(cnst->base.value.type);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_ret(struct context *ctx, struct mir_instr_ret *ret)
{
    zone();
    // compare return value with current function type
    struct mir_instr_block *block = ret->base.owner_block;
    if (!block->terminal) block->terminal = &ret->base;

    struct mir_type *fn_type = ast_current_fn(ctx)->type;
    bassert(fn_type);
    bassert(fn_type->kind == MIR_TYPE_FN);

    if (ret->value) {
        if (analyze_slot(ctx, &analyze_slot_conf_default, &ret->value, fn_type->data.fn.ret_type) !=
            ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
    }

    struct mir_instr *value = ret->value;
    if (value) {
        bassert(isflag(value->flags, MIR_IS_ANALYZED));
    }

    const bool expected_ret_value =
        !type_cmp(fn_type->data.fn.ret_type, ctx->builtin_types->t_void);

    // return value is not expected, and it's not provided
    if (!expected_ret_value && !value) {
        return_zone(ANALYZE_RESULT(PASSED, 0));
    }

    // return value is expected, but it's not provided
    if (expected_ret_value && !value) {
        report_error_after(INVALID_EXPR, ret->base.node, "Expected return value.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    // return value is not expected, but it's provided
    if (!expected_ret_value && value) {
        report_error(INVALID_EXPR, ret->value->node, "Unexpected return value.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_decl_var(struct context *ctx, struct mir_instr_decl_var *decl)
{
    zone();
    struct mir_var *var = decl->var;
    bassert(var);

    // Immutable declaration can be comptime, but only if it's initializer value is also
    // comptime! Value of this variable can be adjusted later during analyze pass when
    // we know actual initialization value.
    bool is_decl_comptime = !var->is_mutable;

    // Resolve declaration type if not set implicitly to the target variable by
    // compiler.
    if (decl->type && !var->value.type) {
        struct result result = analyze_resolve_type(ctx, decl->type, &var->value.type);
        if (result.state != ANALYZE_PASSED) return_zone(result);
    }

    if (var->is_global && !var->is_struct_typedef) {
        // Unexported globals have unique linkage name to solve potential conflicts
        // with extern symbols.
        var->linkage_name = create_unique_name(ctx, var->linkage_name);

        // Globals are set by initializer so we can skip all checks, rest of the
        // work is up to set initializer instruction! There is one exceptional case:
        // we use init value as temporary value for incomplete structure
        // declarations (struct can use pointer to self type inside it's body). This
        // value is later replaced by initializer instruction.
        return_zone(ANALYZE_RESULT(PASSED, 0));
    }

    if (isflag(var->flags, FLAG_THREAD_LOCAL)) {
        report_error(INVALID_DIRECTIVE, var->decl_node, "Thread local variable must be global.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }
    // Continue only with local variables and struct typedefs.
    bool has_initializer = decl->init;
    if (has_initializer) {
        // Resolve variable intializer. Here we use analyze_slot_initializer call to
        // fulfill possible array to slice cast.
        if (var->value.type) {
            if (analyze_slot_initializer(
                    ctx, &analyze_slot_conf_default, &decl->init, var->value.type) !=
                ANALYZE_PASSED) {
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }
        } else {
            if (analyze_slot_initializer(ctx, &analyze_slot_conf_basic, &decl->init, NULL) !=
                ANALYZE_PASSED) {
                return_zone(ANALYZE_RESULT(FAILED, 0));
            }

            // infer type
            struct mir_type *type = decl->init->value.type;
            bassert(type);
            if (type->kind == MIR_TYPE_NULL) type = type->data.null.base_type;
            var->value.type = type;
        }

        // Immutable and comptime initializer value.
        is_decl_comptime &= decl->init->value.is_comptime;
    } else if (isnotflag(var->flags, FLAG_NO_INIT)) {
        // Create default initializer for locals without explicit initialization.
        struct mir_type  *type         = var->value.type;
        struct mir_instr *default_init = create_default_value_for_type(ctx, type);
        insert_instr_before(&decl->base, default_init);
        ANALYZE_INSTR_RQ(default_init);
        decl->init = default_init;
    }
    decl->base.value.is_comptime = var->value.is_comptime = is_decl_comptime;
    struct result state                                   = analyze_var(ctx, decl->var);
    if (state.state != ANALYZE_PASSED) return_zone(state);
    if (decl->base.value.is_comptime && decl->init) {
        // initialize when known in compile-time
        var->value.data = decl->init->value.data;
        bassert(var->value.data && "Incomplete comptime var initialization.");
    }
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

static void poly_type_match(struct mir_type  *recipe,
                            struct mir_type  *other,
                            struct mir_type **poly_type,
                            struct mir_type **matching_type)
{
#define push_if_valid(expr)                                                                        \
    if (is_valid) {                                                                                \
        sarrput(&queue, (expr));                                                                   \
    }
    zone();
    mir_types_t queue = SARR_ZERO;

    bool is_valid = other != NULL;
    push_if_valid(other);
    sarrput(&queue, recipe);
    while (sarrlen(&queue)) {
        struct mir_type *current_recipe = sarrpop(&queue);
        struct mir_type *current_other  = is_valid ? sarrpop(&queue) : NULL;
        bassert(current_recipe);
        const bool is_master =
            current_recipe->kind == MIR_TYPE_POLY && current_recipe->data.poly.is_master;
        if (is_master) {
            *matching_type = current_other;
            *poly_type     = current_recipe;
            break;
        }

        if (current_recipe->kind == MIR_TYPE_SLICE) {
            // Handle conversion of arrays to slice.
            struct mir_type *slice_elem_type =
                mir_deref_type(mir_get_struct_elem_type(current_recipe, MIR_SLICE_PTR_INDEX));
            if (is_valid) {
                if (current_other->kind == MIR_TYPE_ARRAY) {
                    push_if_valid(current_other->data.array.elem_type);
                } else if (current_other->kind == MIR_TYPE_DYNARR) {
                    push_if_valid(mir_deref_type(
                        mir_get_struct_elem_type(current_other, MIR_DYNARR_PTR_INDEX)));
                } else if (current_other->kind == MIR_TYPE_SLICE) {
                    push_if_valid(mir_deref_type(
                        mir_get_struct_elem_type(current_other, MIR_SLICE_PTR_INDEX)));
                } else {
                    is_valid = false;
                }
            }
            sarrput(&queue, slice_elem_type);
            continue;
        }
        if (current_other && (current_recipe->kind != current_other->kind)) is_valid = false;
        switch (current_recipe->kind) {
        case MIR_TYPE_PTR:
            push_if_valid(current_other->data.ptr.expr);
            sarrput(&queue, current_recipe->data.ptr.expr);
            break;
        case MIR_TYPE_FN: {
            mir_args_t *recipe_args = current_recipe->data.fn.args;
            mir_args_t *other_args  = is_valid ? current_other->data.fn.args : NULL;
            for (usize i = 0; i < sarrlenu(recipe_args); ++i) {
                struct mir_arg *arg = sarrpeek(recipe_args, i);
                if (i < sarrlenu(other_args))
                    sarrput(&queue, sarrpeek(other_args, i)->type);
                else
                    is_valid = false;
                sarrput(&queue, arg->type);
            }
            break;
        }
        case MIR_TYPE_DYNARR: {
            push_if_valid(
                mir_deref_type(mir_get_struct_elem_type(current_other, MIR_DYNARR_PTR_INDEX)));
            sarrput(&queue,
                    mir_deref_type(mir_get_struct_elem_type(current_recipe, MIR_DYNARR_PTR_INDEX)));

            break;
        }
        case MIR_TYPE_ARRAY: {
            push_if_valid(current_other->data.array.elem_type);
            sarrput(&queue, current_recipe->data.array.elem_type);
            break;
        }
        case MIR_TYPE_BOOL:
        case MIR_TYPE_REAL:
        case MIR_TYPE_INT:
        case MIR_TYPE_POLY:
        case MIR_TYPE_STRING:
            break;

        default:
            is_valid = false;
        }
    }
    sarrfree(&queue);
    return_zone();
#undef push_if_valid
}

static INLINE hash_t get_current_poly_replacement_hash(struct context *ctx)
{
    mir_types_t *queue = &ctx->polymorph.replacement_queue;
    if (!sarrlenu(queue)) return 0;

    zone();
    hash_t hash = sarrpeek(queue, 0)->id.hash;
    for (usize i = 1; i < sarrlenu(queue); ++i) {
        struct mir_type *type = sarrpeek(queue, i);
        bassert(type->id.hash != 0);
        hash = hashcomb(hash, type->id.hash);
    }
    return_zone(hash);
}

struct result generate_fn_poly(struct context             *ctx,
                               struct ast                 *call,
                               struct mir_fn              *fn,
                               mir_instrs_t               *expected_args,
                               struct mir_instr_fn_proto **out_fn_proto)
{
    // Polymorph types can be divided into two groups, masters and slaves. The master type is
    // defined as '?<type name>', such type acts like type definition (creates scope entry) and
    // should be replaced by matching argument type deduced from call side of the function. The
    // slave type on the other hand cannot be directly replaced (based on type of matching
    // argument), it creates only reference to the master.
    //
    // Example:
    //     sum :: fn (a: ?T, b: T, c: s32) T
    //     sum(10, 20, 30)
    //
    // The type of first argument 'a' is master polymorph type and it's replacement is deduced
    // from the first call argument type (s32 in the example). All other slaves 'T' are
    // references to
    // '?T' so they are replaced by 's32' type even if matching call side argument has different
    // type
    zone();
    struct mir_fn_poly_recipe *recipe = fn->poly;
    BL_MAGIC_ASSERT(recipe);
    bassert(out_fn_proto);

    struct mir_type *recipe_type = fn->type;
    bassert(recipe_type && recipe_type->kind == MIR_TYPE_FN);
    mir_args_t *recipe_args = recipe_type->data.fn.args;

    struct ast *ast_recipe = recipe->ast_lit_fn;
    bassert(ast_recipe && "Missing struct ast recipe for polymorph function!");
    bassert(ast_recipe->kind == AST_EXPR_LIT_FN);

    char        *debug_replacement_str = gettmpstr();
    mir_types_t *queue                 = &ctx->polymorph.replacement_queue;
    const usize  argc                  = sarrlenu(recipe_args);
    for (usize i = 0; i < argc; ++i) {
        const bool       is_expected_arg_valid = expected_args && i < sarrlenu(expected_args);
        struct mir_type *call_arg_type =
            is_expected_arg_valid ? sarrpeek(expected_args, i)->value.type : NULL;
        struct mir_type *recipe_arg_type = sarrpeek(recipe_args, i)->type;
        if (is_expected_arg_valid && is_load_needed(sarrpeek(expected_args, i))) {
            bassert(call_arg_type);
            call_arg_type = mir_deref_type(call_arg_type);
        }
        struct mir_type *poly_type     = NULL;
        struct mir_type *matching_type = NULL;
        poly_type_match(recipe_arg_type, call_arg_type, &poly_type, &matching_type);
        if (poly_type) {
            if (!matching_type) {
                ast_nodes_t *ast_poly_args = ast_recipe->data.expr_fn.type->data.type_fn.args;
                struct ast  *ast_poly_arg  = sarrpeek(ast_poly_args, i);
                char         recipe_type_name[256];
                mir_type_to_str(recipe_type_name, 256, recipe_arg_type, true);
                if (call_arg_type) {
                    char arg_type_name[256];
                    mir_type_to_str(arg_type_name, 256, call_arg_type, true);
                    report_error(INVALID_POLY_MATCH,
                                 ast_poly_arg->data.decl.type,
                                 "Cannot deduce polymorph function argument type '%s'. Expected is "
                                 "'%s' but call-side argument type is '%s'.",
                                 poly_type->user_id->str,
                                 recipe_type_name,
                                 arg_type_name);

                    struct location *call_arg_loc =
                        sarrpeek(call->data.expr_call.args, i)->location;
                    if (!call_arg_loc) call_arg_loc = call->location;
                    builder_msg(
                        BUILDER_MSG_NOTE, 0, call_arg_loc, BUILDER_CUR_WORD, "Called from here.");
                } else {
                    // Missing argument on call side required for polymorph deduction.
                    report_error(INVALID_POLY_MATCH,
                                 ast_poly_arg->data.decl.type,
                                 "Cannot deduce polymorph function argument type '%s'.",
                                 poly_type->user_id->str);
                    report_note(call, "Called from here.");
                }
                sarrput(queue, ctx->builtin_types->t_s32);
                puttmpstr(debug_replacement_str);
                return_zone(ANALYZE_RESULT(FAILED, 0));
            } else {
                bassert(matching_type->kind != MIR_TYPE_POLY);
                // Stringify replacement to get better error reports.
                char type_name1[256];
                char type_name2[256];
                mir_type_to_str(type_name1, 256, poly_type, true);
                mir_type_to_str(type_name2, 256, matching_type, true);
                strappend(debug_replacement_str, "%s = %s; ", type_name1, type_name2);
                sarrput(queue, matching_type);
            }
        }
    }

    const hash_t replacement_hash = get_current_poly_replacement_hash(ctx);

    const s64 index = hmgeti(recipe->entries, replacement_hash);
    if (index == -1) {
        const s32 prev_scope_layer_index         = ctx->polymorph.current_scope_layer_index;
        ctx->polymorph.current_scope_layer_index = ++recipe->scope_layer;
        ctx->polymorph.is_replacement_active     = true;

        // Create name for generated function
        const char *original_fn_name     = fn->id ? fn->id->str : IMPL_FN_NAME;
        const char *linkage_name         = create_unique_name(ctx, original_fn_name);
        const u32   replacement_fn_flags = fn->flags;

        // Generate new function.
        struct mir_instr *instr_fn_proto = ast_expr_lit_fn(ctx,
                                                           ast_recipe,
                                                           fn->decl_node,
                                                           linkage_name,
                                                           fn->is_global,
                                                           replacement_fn_flags,
                                                           BUILTIN_ID_NONE);

        bassert(instr_fn_proto && instr_fn_proto->kind == MIR_INSTR_FN_PROTO);

        struct mir_fn *replacement_fn = MIR_CEV_READ_AS(struct mir_fn *, &instr_fn_proto->value);
        BL_MAGIC_ASSERT(replacement_fn);
        replacement_fn->first_poly_call_node = call;

        char *debug_replacement_str_dup = scdup(
            &ctx->assembly->string_cache, debug_replacement_str, strlenu(debug_replacement_str));
        replacement_fn->debug_poly_replacement = debug_replacement_str_dup;

        ctx->polymorph.is_replacement_active     = false;
        ctx->polymorph.current_scope_layer_index = prev_scope_layer_index;

        // Feed the output
        (*out_fn_proto) = (struct mir_instr_fn_proto *)instr_fn_proto;
        hmput(recipe->entries, replacement_hash, (struct mir_instr_fn_proto *)instr_fn_proto);
        ctx->assembly->stats.polymorph_count += 1;
    } else {
        *out_fn_proto = recipe->entries[index].value;
    }
    sarrclear(queue);
    puttmpstr(debug_replacement_str);
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_call(struct context *ctx, struct mir_instr_call *call)
{
    zone();
    bassert(call->callee);
    if (!call->callee_analyzed) {
        struct id *missing_any = lookup_builtins_any(ctx);
        if (missing_any) return_zone(ANALYZE_RESULT(WAITING, missing_any->hash));

        // callee has not been analyzed yet -> postpone call analyze
        if (isnotflag(call->callee->flags, MIR_IS_ANALYZED)) {
            bassert(call->callee->kind == MIR_INSTR_FN_PROTO);
            struct mir_instr_fn_proto *fn_proto = (struct mir_instr_fn_proto *)call->callee;
            if (!fn_proto->pushed_for_analyze) {
                fn_proto->pushed_for_analyze = true;
                analyze_schedule(ctx, call->callee);
            }
            return_zone(ANALYZE_RESULT(POSTPONE, 0));
        }

        if (analyze_slot(ctx, &analyze_slot_conf_basic, &call->callee, NULL) != ANALYZE_PASSED) {
            return_zone(ANALYZE_RESULT(FAILED, 0));
        }
        call->callee_analyzed = true;
    }

    // Direct call is call without any reference lookup, usually call to anonymous
    // function, type resolver or variable initializer. Constant value of callee
    // instruction must containt pointer to the struct mir_fn object.
    const bool is_direct_call = call->callee->kind == MIR_INSTR_FN_PROTO;
    // Functions called in compile time are supposed to be called right after successful analyze
    // pass and should be replaced by constant in MIR. Keep in mind that such function must be
    // fully analyzed before call.
    struct mir_type *type = call->callee->value.type;
    bassert(type && "invalid type of called object");

    const bool is_called_via_pointer = mir_is_pointer_type(type);
    if (is_called_via_pointer) {
        // we want to make calls also via pointer to functions so in such case
        // we need to resolve pointed function
        type = mir_deref_type(type);
    }
    bool is_fn    = type->kind == MIR_TYPE_FN;
    bool is_group = type->kind == MIR_TYPE_FN_GROUP;
    if (!is_group && !is_fn) {
        report_error(
            EXPECTED_FUNC, call->callee->node, "Expected a function of function group name.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    union fn_or_group {
        struct mir_fn       *fn;
        struct mir_fn_group *group;
        void                *any;
    };

    union fn_or_group optional_fn_or_group;
    optional_fn_or_group.any =
        call->callee->value.is_comptime ? MIR_CEV_READ_AS(void *, &call->callee->value) : NULL;

    // Pre-scan of all arguments passed to function call is needed in case we want to convert
    // some arguments to Any type, because to Any conversion requires generation of rtti
    // metadata about argument value type, we must check all argument types for it's
    // completeness.

    // @Performance: This is really needed only in case the function argument list contains
    // conversion to Any, there is no need to scan everything, also there is possible option do
    // this check in analyze_instr_decl_ref pass. @travis 14-Oct-2020
    for (usize i = 0; i < sarrlenu(call->args); ++i) {
        struct mir_instr *it = sarrpeek(call->args, i);
        struct mir_type  *t  = it->value.type;
        if (t->kind == MIR_TYPE_PTR && mir_deref_type(t)->kind == MIR_TYPE_TYPE) {
            t = *MIR_CEV_READ_AS(struct mir_type **, &it->value);
            BL_MAGIC_ASSERT(t);
        }
        if (!is_complete_type(ctx, t)) {
            if (t->user_id) return_zone(ANALYZE_RESULT(WAITING, t->user_id->hash));
            return_zone(ANALYZE_RESULT(POSTPONE, 0));
        }
    }

    if (is_group) {
        // Function group will be replaced with constant function reference. Best callee
        // candidate selection is based on call arguments not on return type! The best
        // function is selected but it could be still invalid so we have to validate it as
        // usual.
        struct mir_fn_group *group = optional_fn_or_group.group;
        BL_MAGIC_ASSERT(group);
        struct mir_fn *selected_overload_fn;
        { // lookup best call candidate in group
            mir_types_t arg_types = SARR_ZERO;
            sarrsetlen(&arg_types, sarrlenu(call->args));
            for (usize i = 0; i < sarrlenu(call->args); ++i) {
                struct mir_instr *it    = sarrpeek(call->args, i);
                struct mir_type  *t     = it->value.type;
                sarrpeek(&arg_types, i) = is_load_needed(it) ? mir_deref_type(t) : t;
            }
            selected_overload_fn = group_select_overload(ctx, group, &arg_types);
            sarrfree(&arg_types);
        }
        BL_MAGIC_ASSERT(selected_overload_fn);

        // Replace callee instruction with constant containing found overload function.
        erase_instr_tree(call->callee, true, true);
        struct mir_instr_const *callee_replacement =
            (struct mir_instr_const *)mutate_instr(call->callee, MIR_INSTR_CONST);
        callee_replacement->volatile_type   = false;
        callee_replacement->base.value.data = (vm_stack_ptr_t)&call->callee->value._tmp;
        callee_replacement->base.node       = selected_overload_fn->decl_node;
        type = callee_replacement->base.value.type = selected_overload_fn->type;
        MIR_CEV_WRITE_AS(struct mir_fn *, &callee_replacement->base.value, selected_overload_fn);
        optional_fn_or_group.fn = selected_overload_fn;
        is_fn                   = true;
        is_group                = false;
    }

    if (is_direct_call) {
        struct mir_fn *fn = optional_fn_or_group.fn;
        BL_MAGIC_ASSERT(fn);
        if (call->base.value.is_comptime && !fn->is_fully_analyzed)
            return_zone(ANALYZE_RESULT(POSTPONE, 0));
        // Direct call of anonymous function.
        // NOTE: We increase ref count of called function here, but this
        // will not work for functions called by pointer in obtained in
        // runtime
        if (fn->ref_count == 0) {
            ++fn->ref_count;
        }
    }
    bassert(is_fn && !is_group);
    const bool is_polymorph = isflag(type->data.fn.flags, MIR_TYPE_FN_FLAG_IS_POLYMORPH);
    if (is_polymorph) {
        struct mir_fn *fn = optional_fn_or_group.fn;
        BL_MAGIC_ASSERT(fn);
        struct mir_instr_fn_proto *instr_replacement_fn_proto = NULL;
        runtime_measure_begin(poly);
        struct result state =
            generate_fn_poly(ctx, call->base.node, fn, call->args, &instr_replacement_fn_proto);
        if (state.state != ANALYZE_PASSED) return_zone(ANALYZE_RESULT(FAILED, 0));
        ctx->assembly->stats.polymorph_s += runtime_measure_end(poly);
        bassert(instr_replacement_fn_proto);

        // Mutate callee instruction to direct declaration reference.
        struct mir_instr_decl_direct_ref *callee_replacement =
            (struct mir_instr_decl_direct_ref *)mutate_instr(call->callee,
                                                             MIR_INSTR_DECL_DIRECT_REF);
        callee_replacement->ref = ref_instr(&instr_replacement_fn_proto->base);
        clrflag(callee_replacement->base.flags, MIR_IS_ANALYZED);

        // We skip analyze of this instruction, newly created polymorph function is not analyzed
        // yet; however we've changed kind of callee instruction, so we have to roll-back in
        // analyze stack by re-sumbit callee to analyze again.
        analyze_schedule(ctx, &callee_replacement->base);
        return_zone(ANALYZE_RESULT(SKIP, 0));
    }

    struct mir_type *result_type = type->data.fn.ret_type;
    bassert(result_type && "invalid type of call result");
    call->base.value.type = result_type;

    if (call->call_in_compile_time) {
        if (!mir_is_comptime(&call->base))
            babort("Function called in compile time must be comptime.");
        // Postpone analyze in case the function is not fully analyzed -> it cannot be executed
        // yet.
        // @Incomplete: In case the function calls internally another function(s), those can be
        // also
        //  "not fully analyzed", we have to check it somehow before evaluation.
        struct mir_fn *fn = optional_fn_or_group.fn;
        BL_MAGIC_ASSERT(fn);
        if (!fn->is_fully_analyzed) return_zone(ANALYZE_RESULT(POSTPONE, 0));
    }

    // validate arguments
    usize       callee_argc      = sarrlenu(type->data.fn.args);
    const usize call_argc        = sarrlenu(call->args);
    const bool  is_vargs         = isflag(type->data.fn.flags, MIR_TYPE_FN_FLAG_IS_VARGS);
    const bool  has_default_args = isflag(type->data.fn.flags, MIR_TYPE_FN_FLAG_HAS_DEFAULT_ARGS);

    bool is_last_call_arg_vargs = false;
    if (call_argc) {
        struct mir_instr *last_arg = sarrpeek(call->args, call_argc - 1);
        if (is_load_needed(last_arg)) {
            is_last_call_arg_vargs = mir_deref_type(last_arg->value.type)->kind == MIR_TYPE_VARGS;
        }
    }

    bassert(!(is_vargs && has_default_args));
    if (is_vargs && !is_last_call_arg_vargs) {
        // This is gonna be tricky...
        --callee_argc;
        if ((call_argc < callee_argc)) {
            goto INVALID_ARGC;
        }
        struct mir_type *vargs_type = mir_get_fn_arg_type(type, (u32)callee_argc);
        bassert(vargs_type->kind == MIR_TYPE_VARGS && "VArgs is expected to be last!!!");
        vargs_type = mir_get_struct_elem_type(vargs_type, 1);
        bassert(vargs_type && mir_is_pointer_type(vargs_type));
        vargs_type = mir_deref_type(vargs_type);

        // Prepare vargs values.
        const usize       vargsc = call_argc - callee_argc;
        mir_instrs_t     *values = arena_safe_alloc(&ctx->assembly->arenas.sarr);
        struct mir_instr *vargs = create_instr_vargs_impl(ctx, call->base.node, vargs_type, values);
        ref_instr(vargs);
        if (vargsc > 0) {
            // One or more vargs passed.
            // @INCOMPLETE: check it this is ok!!!
            for (usize i = 0; i < vargsc; ++i) {
                sarrput(values, sarrpeek(call->args, callee_argc + i));
            }

            struct mir_instr *insert_loc = sarrpeek(call->args, callee_argc);
            insert_instr_after(insert_loc, vargs);
        } else if (callee_argc > 0) {
            // No arguments passed into vargs but there are more regular
            // arguments before vargs.
            struct mir_instr *insert_loc = sarrpeek(call->args, call_argc - 1);
            insert_instr_before(insert_loc, vargs);
        } else {
            insert_instr_before(&call->base, vargs);
        }

        if (analyze_instr_vargs(ctx, (struct mir_instr_vargs *)vargs).state != ANALYZE_PASSED)
            return_zone(ANALYZE_RESULT(FAILED, 0));
        setflag(vargs->flags, MIR_IS_ANALYZED);
        // Erase vargs from arguments. @NOTE: function does nothing when array size is equal
        // to callee_argc.
        sarrsetlen(call->args, callee_argc);
        // Replace last with vargs.
        sarrput(call->args, vargs);
    } else if (has_default_args) {
        // Call have more arguments than a function.
        if (callee_argc < call_argc) {
            goto INVALID_ARGC;
        }

        // Check if all arguments are explicitly provided.
        if (callee_argc > call_argc) {
            for (usize i = call_argc; i < callee_argc; ++i) {
                struct mir_arg *arg = sarrpeek(type->data.fn.args, i);
                // Missing argument has no default value!
                if (!arg->value) {
                    // @INCOMPLETE: Consider better error message...
                    goto INVALID_ARGC;
                }

                // Create direct reference to default value and insert it into call
                // argument list. Here we modify call->args array!!!
                struct mir_instr *insert_location =
                    sarrlenu(call->args) > 0 ? sarrpeek(call->args, sarrlen(call->args) - 1)
                                             : &call->base;

                struct mir_instr *call_default_arg;
                if (arg->value->kind == MIR_INSTR_CALL_LOC) {
                    // Original InstrCallLoc is used only as note that we must
                    // generate real one containing information about call
                    // instruction location.
                    bassert(call->base.node);
                    bassert(call->base.node->location);
                    struct ast *orig_node = arg->value->node;
                    call_default_arg =
                        create_instr_call_loc(ctx, orig_node, call->base.node->location);
                } else {
                    call_default_arg = create_instr_decl_direct_ref(ctx, NULL, arg->value);
                }

                sarrput(call->args, call_default_arg);
                insert_instr_before(insert_location, call_default_arg);
                const struct result result = analyze_instr(ctx, call_default_arg);
                // Default value reference MUST be analyzed before any call to owner
                // function!
                if (result.state != ANALYZE_PASSED) return_zone(ANALYZE_RESULT(FAILED, 0));
            }
        }
    } else if (callee_argc != call_argc) {
        goto INVALID_ARGC;
    }
    // validate argument types
    for (usize i = 0; i < callee_argc; ++i) {
        struct mir_instr **call_arg   = &sarrpeek(call->args, i);
        struct mir_arg    *callee_arg = sarrpeek(type->data.fn.args, i);
        bassert(callee_arg);
        if (analyze_slot(ctx, &analyze_slot_conf_full, call_arg, callee_arg->type) !=
            ANALYZE_PASSED) {
            goto REPORT_OVERLOAD_LOCATION;
        }
        if (call->call_in_compile_time && !mir_is_comptime(*call_arg)) {
            report_error(EXPECTED_COMPTIME,
                         (*call_arg)->node,
                         "Function argument is supposed to be compile-time known since function is "
                         "called in compile-time.");
            goto REPORT_OVERLOAD_LOCATION;
        }
    }
    return_zone(ANALYZE_RESULT(PASSED, 0));
    // ERROR
INVALID_ARGC:
    report_error(INVALID_ARG_COUNT,
                 call->base.node,
                 "Expected %u %s, but called with %u.",
                 callee_argc,
                 callee_argc == 1 ? "argument" : "arguments",
                 call_argc);

    if (optional_fn_or_group.fn && optional_fn_or_group.fn->decl_node) {
        report_note(optional_fn_or_group.fn->decl_node, "Function is declared here:");
    }
REPORT_OVERLOAD_LOCATION:
    if (is_group) {
        report_note(call->callee->node, "Overloaded function implementation:");
    }
    return_zone(ANALYZE_RESULT(FAILED, 0));
}

struct result analyze_instr_store(struct context *ctx, struct mir_instr_store *store)
{
    zone();
    struct mir_instr *dest = store->dest;
    bassert(dest);
    bassert(isflag(dest->flags, MIR_IS_ANALYZED));

    if (!mir_is_pointer_type(dest->value.type)) {
        report_error(
            INVALID_EXPR, store->base.node, "Left hand side of the expression cannot be assigned.");
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    if (dest->value.addr_mode == MIR_VAM_LVALUE_CONST || dest->value.addr_mode == MIR_VAM_RVALUE) {
        report_error(INVALID_EXPR, store->base.node, "Cannot assign to constant.");
    }

    struct mir_type *dest_type = mir_deref_type(dest->value.type);
    bassert(dest_type && "store destination has invalid base type");

    if (analyze_slot(ctx, &analyze_slot_conf_default, &store->src, dest_type) != ANALYZE_PASSED) {
        return_zone(ANALYZE_RESULT(FAILED, 0));
    }

    // @BUG Global immutable array converted implicitly to slice cause problems when this check
    // is enabled INDENT_AFTER :: {:[1]u8: '{'}; opt.indent_after = INDENT_AFTER;

#if BL_DEBUG
    // If store instruction source value is compound expression it should not be naked.
    if (store->src->kind == MIR_INSTR_COMPOUND) {
        bassert(!((struct mir_instr_compound *)store->src)->is_naked);
    }
#endif

    return_zone(ANALYZE_RESULT(PASSED, 0));
}

struct result analyze_instr_block(struct context *ctx, struct mir_instr_block *block)
{
    zone();
    bassert(block);

    struct mir_fn *fn = block->owner_fn;
    if (!fn) { // block in global scope
        return_zone(ANALYZE_RESULT(PASSED, 0));
    }

    if (block->base.ref_count == 0)
        setflag(block->base.flags, MIR_IS_UNREACHABLE);
    else
        clrflag(block->base.flags, MIR_IS_UNREACHABLE);

    if (!fn->first_unreachable_loc && isflag(block->base.flags, MIR_IS_UNREACHABLE) &&
        block->entry_instr && block->entry_instr->node) {
        // Report unreachable code if there is one only once inside function body.
        fn->first_unreachable_loc = block->entry_instr->node->location;
        const char *poly          = fn->debug_poly_replacement;
        if (poly) {
            builder_msg(
                BUILDER_MSG_WARNING,
                0,
                fn->first_unreachable_loc,
                BUILDER_CUR_NONE,
                "Unreachable code detected in the function '%s' with polymorph replacement: %s",
                mir_get_fn_readable_name(fn),
                poly);
        } else {
            builder_msg(BUILDER_MSG_WARNING,
                        0,
                        fn->first_unreachable_loc,
                        BUILDER_CUR_NONE,
                        "Unreachable code detected in the function '%s'.",
                        mir_get_fn_readable_name(fn));
        }
    }

    // Append implicit return for void functions or generate error when last
    // block is not terminated
    if (!is_block_terminated(block)) {
        // Exit block and it's terminal break instruction are generated during ast
        // instruction pass and it must be already terminated. Here we generate only breaks to
        // the exit block in case analyzed block is not correctly terminated (it's pretty common
        // since instruction generation is just appending blocks).
        bassert(block != fn->exit_block && "Exit block must be terminated!");

        if (fn->type->data.fn.ret_type->kind == MIR_TYPE_VOID) {
            set_current_block(ctx, block);
            bassert(fn->exit_block &&
                    "Current function does not have exit block set or even generated!");
            append_instr_br(ctx, block->base.node, fn->exit_block);
        } else if (isflag(block->base.flags, MIR_IS_UNREACHABLE)) {
            set_current_block(ctx, block);
            append_instr_br(ctx, block->base.node, block);
        } else {
            report_error(
                MISSING_RETURN, fn->decl_node, "Not every path inside function return value.");
            DEBUG_PRINT_MIR(&block->base);
        }
    }
    return_zone(ANALYZE_RESULT(PASSED, 0));
}

enum result_state _analyze_slot(struct context           *ctx,
                                const struct slot_config *conf,
                                struct mir_instr        **input,
                                struct mir_type          *slot_type,
                                bool                      is_initializer)
{
    enum stage_state state;
    for (s32 i = 0; i < conf->count; ++i) {
        state = conf->stages[i](ctx, input, slot_type, is_initializer);
        switch (state) {
        case ANALYZE_STAGE_BREAK:
            return ANALYZE_PASSED;
        case ANALYZE_STAGE_FAILED:
            return ANALYZE_FAILED;
        case ANALYZE_STAGE_CONTINUE:
            break;
        }
    }
    return ANALYZE_PASSED;
}

ANALYZE_STAGE_FN(load)
{
    if (is_load_needed(*input)) {
        *input = insert_instr_load(ctx, *input);

        struct result r = analyze_instr(ctx, *input);
        if (r.state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;
    }

    return ANALYZE_STAGE_CONTINUE;
}

ANALYZE_STAGE_FN(unroll)
{
    struct mir_instr_unroll *unroll =
        (*input)->kind == MIR_INSTR_UNROLL ? ((struct mir_instr_unroll *)*input) : NULL;
    if (!unroll) return ANALYZE_STAGE_CONTINUE;
    // Erase unroll instruction in case it's marked for remove.
    if (unroll->remove) {
        if (unroll->remove_src) {
            struct mir_instr *ref = create_instr_decl_direct_ref(ctx, NULL, unroll->remove_src);
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
    bassert(slot_type);
    struct mir_instr *_input = *input;

    if (_input->kind != MIR_INSTR_CONST) return ANALYZE_STAGE_CONTINUE;
    if (_input->value.type->kind != MIR_TYPE_NULL) return ANALYZE_STAGE_CONTINUE;

    if (slot_type->kind == MIR_TYPE_NULL) {
        _input->value.type = slot_type;
        return ANALYZE_STAGE_BREAK;
    }

    if (mir_is_pointer_type(slot_type)) {
        _input->value.type = create_type_null(ctx, slot_type);
        return ANALYZE_STAGE_BREAK;
    }

    report_error(INVALID_TYPE, _input->node, "Invalid use of null constant.");

    return ANALYZE_STAGE_FAILED;
}

ANALYZE_STAGE_FN(set_auto)
{
    bassert(slot_type);
    if ((*input)->kind != MIR_INSTR_CAST) return ANALYZE_STAGE_CONTINUE;
    struct mir_instr_cast *cast = (struct mir_instr_cast *)*input;
    if (!cast->auto_cast) return ANALYZE_STAGE_CONTINUE;
    cast->base.value.type = slot_type;
    if (analyze_instr_cast(ctx, cast, true).state != ANALYZE_PASSED) {
        return ANALYZE_STAGE_FAILED;
    }
    if (!evaluate(ctx, &cast->base)) return ANALYZE_STAGE_FAILED;
    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(toany)
{
    bassert(slot_type);

    // check any
    if (!is_to_any_needed(ctx, *input, slot_type)) return ANALYZE_STAGE_CONTINUE;

    struct result result;
    *input = insert_instr_toany(ctx, *input);
    result = analyze_instr(ctx, *input);
    if (result.state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;

    *input = insert_instr_load(ctx, *input);
    result = analyze_instr(ctx, *input);
    if (result.state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;

    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(dynarrtoslice)
{
    // Cast from dynamic array to slice can be done by bitcast from pointer to dynamic array to
    // slice pointer, both structures have same data layout of first two members.
    bassert(slot_type);

    struct mir_type *from_type = (*input)->value.type;
    bassert(from_type);

    if (!mir_is_pointer_type(from_type)) return ANALYZE_STAGE_CONTINUE;

    from_type = mir_deref_type(from_type);
    if (from_type->kind != MIR_TYPE_DYNARR || slot_type->kind != MIR_TYPE_SLICE)
        return ANALYZE_STAGE_CONTINUE;

    { // Compare elem type of array and slot slice
        struct mir_type *elem_from_type = mir_get_struct_elem_type(from_type, MIR_SLICE_PTR_INDEX);
        struct mir_type *elem_to_type   = mir_get_struct_elem_type(slot_type, MIR_SLICE_PTR_INDEX);

        bassert(mir_is_pointer_type(elem_from_type) && "Expected pointer type!");
        bassert(mir_is_pointer_type(elem_to_type) && "Expected pointer type!");
        elem_from_type = mir_deref_type(elem_from_type);
        elem_to_type   = mir_deref_type(elem_to_type);

        bassert(elem_from_type && "Invalid type after pointer type dereference!");
        bassert(elem_to_type && "Invalid type after pointer type dereference!");

        if (!type_cmp(elem_from_type, elem_to_type)) return ANALYZE_STAGE_CONTINUE;
    }

    { // Build bitcast
        *input = insert_instr_cast(ctx, *input, create_type_ptr(ctx, slot_type));
        if (analyze_instr(ctx, *input).state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;
    }

    { // Build load
        *input = insert_instr_load(ctx, *input);
        if (analyze_instr(ctx, *input).state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;
    }

    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(arrtoslice)
{
    // Produce implicit cast from array type to slice. This will create implicit compound
    // initializer representing array legth and pointer to array data.
    bassert(slot_type);

    struct mir_type *from_type = (*input)->value.type;
    bassert(from_type);

    if (!mir_is_pointer_type(from_type)) return ANALYZE_STAGE_CONTINUE;

    from_type = mir_deref_type(from_type);
    if (from_type->kind != MIR_TYPE_ARRAY || slot_type->kind != MIR_TYPE_SLICE)
        return ANALYZE_STAGE_CONTINUE;

    { // Compare elem type of array and slot slice
        struct mir_type *elem_from_type = from_type->data.array.elem_type;
        struct mir_type *elem_to_type   = mir_get_struct_elem_type(slot_type, MIR_SLICE_PTR_INDEX);
        bassert(mir_is_pointer_type(elem_to_type) && "Expected pointer type!");
        elem_to_type = mir_deref_type(elem_to_type);
        bassert(elem_to_type && "Invalid type after pointer type dereference!");

        if (!type_cmp(elem_from_type, elem_to_type)) return ANALYZE_STAGE_CONTINUE;
    }

    { // Build slice initializer.
        const s64         len       = from_type->data.array.len;
        struct mir_instr *instr_arr = *input;
        mir_instrs_t     *values    = arena_safe_alloc(&ctx->assembly->arenas.sarr);

        // Build array pointer
        struct mir_instr *instr_ptr =
            create_instr_member_ptr(ctx, NULL, instr_arr, NULL, NULL, BUILTIN_ID_ARR_PTR);
        insert_instr_after(*input, instr_ptr);
        *input = instr_ptr;
        ANALYZE_INSTR_RQ(instr_ptr);

        // Build array len constant
        struct mir_instr *instr_len =
            create_instr_const_int(ctx, NULL, ctx->builtin_types->t_s64, len);
        insert_instr_after(*input, instr_len);
        *input = instr_len;
        ANALYZE_INSTR_RQ(instr_len);

        // push values
        sarrput(values, instr_len);
        sarrput(values, instr_ptr);

        struct mir_instr *compound = create_instr_compound_impl(ctx, NULL, slot_type, values);
        ((struct mir_instr_compound *)compound)->is_naked = !is_initializer;
        ref_instr(compound);

        insert_instr_after(*input, compound);
        *input = compound;

        ANALYZE_INSTR_RQ(compound);
    }

    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(set_volatile_expr)
{
    bassert(slot_type);
    if (slot_type->kind != MIR_TYPE_INT) return ANALYZE_STAGE_CONTINUE;
    if (!is_instr_type_volatile(*input)) return ANALYZE_STAGE_CONTINUE;
    const enum mir_cast_op op = get_cast_op((*input)->value.type, slot_type);
    bassert(op != MIR_CAST_INVALID);
    struct mir_const_expr_value *value = &(*input)->value;
    vm_value_t                   tmp   = {0};
    vm_do_cast((vm_stack_ptr_t)&tmp[0], value->data, slot_type, value->type, op);
    memcpy(&value->_tmp[0], &tmp[0], sizeof(vm_value_t));
    (*input)->value.type = slot_type;
    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(implicit_cast)
{
    if (type_cmp((*input)->value.type, slot_type)) return ANALYZE_STAGE_BREAK;
    if (!can_impl_cast((*input)->value.type, slot_type)) return ANALYZE_STAGE_CONTINUE;

    *input = insert_instr_cast(ctx, *input, slot_type);

    struct result r = analyze_instr(ctx, *input);
    if (r.state != ANALYZE_PASSED) return ANALYZE_STAGE_FAILED;
    return ANALYZE_STAGE_BREAK;
}

ANALYZE_STAGE_FN(report_type_mismatch)
{
    error_types(ctx, *input, (*input)->value.type, slot_type, (*input)->node, NULL);
    return ANALYZE_STAGE_FAILED;
}

FORCEINLINE struct result analyze_instr(struct context *ctx, struct mir_instr *instr)
{
    zone();
    if (!instr) return_zone(ANALYZE_RESULT(PASSED, 0));

    // skip already analyzed instructions
    if (isflag(instr->flags, MIR_IS_ANALYZED)) return_zone(ANALYZE_RESULT(PASSED, 0));
    struct result state = ANALYZE_RESULT(PASSED, 0);

    BL_TRACY_MESSAGE("ANALYZE", "[%llu] %s", instr->id, mir_instr_name(instr));
    ctx->analyze.last_analyzed_instr = instr;
    if (instr->owner_block) set_current_block(ctx, instr->owner_block);

    switch (instr->kind) {
    case MIR_INSTR_VARGS:
    case MIR_INSTR_INVALID:
        break;

    case MIR_INSTR_BLOCK:
        state = analyze_instr_block(ctx, (struct mir_instr_block *)instr);
        break;
    case MIR_INSTR_FN_PROTO:
        state = analyze_instr_fn_proto(ctx, (struct mir_instr_fn_proto *)instr);
        break;
    case MIR_INSTR_FN_GROUP:
        state = analyze_instr_fn_group(ctx, (struct mir_instr_fn_group *)instr);
        break;
    case MIR_INSTR_DECL_VAR:
        state = analyze_instr_decl_var(ctx, (struct mir_instr_decl_var *)instr);
        break;
    case MIR_INSTR_DECL_MEMBER:
        state = analyze_instr_decl_member(ctx, (struct mir_instr_decl_member *)instr);
        break;
    case MIR_INSTR_DECL_VARIANT:
        state = analyze_instr_decl_variant(ctx, (struct mir_instr_decl_variant *)instr);
        break;
    case MIR_INSTR_DECL_ARG:
        state = analyze_instr_decl_arg(ctx, (struct mir_instr_decl_arg *)instr);
        break;
    case MIR_INSTR_CALL:
        state = analyze_instr_call(ctx, (struct mir_instr_call *)instr);
        break;
    case MIR_INSTR_MSG:
        state = analyze_instr_msg(ctx, (struct mir_instr_msg *)instr);
        break;
    case MIR_INSTR_CONST:
        state = analyze_instr_const(ctx, (struct mir_instr_const *)instr);
        break;
    case MIR_INSTR_RET:
        state = analyze_instr_ret(ctx, (struct mir_instr_ret *)instr);
        break;
    case MIR_INSTR_STORE:
        state = analyze_instr_store(ctx, (struct mir_instr_store *)instr);
        break;
    case MIR_INSTR_DECL_REF:
        state = analyze_instr_decl_ref(ctx, (struct mir_instr_decl_ref *)instr);
        break;
    case MIR_INSTR_BINOP:
        state = analyze_instr_binop(ctx, (struct mir_instr_binop *)instr);
        break;
    case MIR_INSTR_TYPE_FN:
        state = analyze_instr_type_fn(ctx, (struct mir_instr_type_fn *)instr);
        break;
    case MIR_INSTR_TYPE_FN_GROUP:
        state = analyze_instr_type_fn_group(ctx, (struct mir_instr_type_fn_group *)instr);
        break;
    case MIR_INSTR_TYPE_STRUCT:
        state = analyze_instr_type_struct(ctx, (struct mir_instr_type_struct *)instr);
        break;
    case MIR_INSTR_TYPE_SLICE:
        state = analyze_instr_type_slice(ctx, (struct mir_instr_type_slice *)instr);
        break;
    case MIR_INSTR_TYPE_DYNARR:
        state = analyze_instr_type_dynarr(ctx, (struct mir_instr_type_dyn_arr *)instr);
        break;
    case MIR_INSTR_TYPE_VARGS:
        state = analyze_instr_type_vargs(ctx, (struct mir_instr_type_vargs *)instr);
        break;
    case MIR_INSTR_TYPE_ARRAY:
        state = analyze_instr_type_array(ctx, (struct mir_instr_type_array *)instr);
        break;
    case MIR_INSTR_TYPE_PTR:
        state = analyze_instr_type_ptr(ctx, (struct mir_instr_type_ptr *)instr);
        break;
    case MIR_INSTR_TYPE_ENUM:
        state = analyze_instr_type_enum(ctx, (struct mir_instr_type_enum *)instr);
        break;
    case MIR_INSTR_TYPE_POLY:
        state = analyze_instr_type_poly(ctx, (struct mir_instr_type_poly *)instr);
        break;
    case MIR_INSTR_LOAD:
        state = analyze_instr_load(ctx, (struct mir_instr_load *)instr);
        break;
    case MIR_INSTR_COMPOUND:
        state = analyze_instr_compound(ctx, (struct mir_instr_compound *)instr);
        break;
    case MIR_INSTR_BR:
        state = analyze_instr_br(ctx, (struct mir_instr_br *)instr);
        break;
    case MIR_INSTR_COND_BR:
        state = analyze_instr_cond_br(ctx, (struct mir_instr_cond_br *)instr);
        break;
    case MIR_INSTR_UNOP:
        state = analyze_instr_unop(ctx, (struct mir_instr_unop *)instr);
        break;
    case MIR_INSTR_UNREACHABLE:
        state = analyze_instr_unreachable(ctx, (struct mir_instr_unreachable *)instr);
        break;
    case MIR_INSTR_DEBUGBREAK:
        state = analyze_instr_debugbreak(ctx, (struct mir_instr_debugbreak *)instr);
        break;
    case MIR_INSTR_ARG:
        state = analyze_instr_arg(ctx, (struct mir_instr_arg *)instr);
        break;
    case MIR_INSTR_ELEM_PTR:
        state = analyze_instr_elem_ptr(ctx, (struct mir_instr_elem_ptr *)instr);
        break;
    case MIR_INSTR_MEMBER_PTR:
        state = analyze_instr_member_ptr(ctx, (struct mir_instr_member_ptr *)instr);
        break;
    case MIR_INSTR_ADDROF:
        state = analyze_instr_addrof(ctx, (struct mir_instr_addrof *)instr);
        break;
    case MIR_INSTR_CAST:
        state = analyze_instr_cast(ctx, (struct mir_instr_cast *)instr, false);
        break;
    case MIR_INSTR_SIZEOF:
        state = analyze_instr_sizeof(ctx, (struct mir_instr_sizeof *)instr);
        break;
    case MIR_INSTR_ALIGNOF:
        state = analyze_instr_alignof(ctx, (struct mir_instr_alignof *)instr);
        break;
    case MIR_INSTR_TYPE_INFO:
        state = analyze_instr_type_info(ctx, (struct mir_instr_type_info *)instr);
        break;
    case MIR_INSTR_PHI:
        state = analyze_instr_phi(ctx, (struct mir_instr_phi *)instr);
        break;
    case MIR_INSTR_TOANY:
        state = analyze_instr_toany(ctx, (struct mir_instr_to_any *)instr);
        break;
    case MIR_INSTR_DECL_DIRECT_REF:
        state = analyze_instr_decl_direct_ref(ctx, (struct mir_instr_decl_direct_ref *)instr);
        break;
    case MIR_INSTR_SWITCH:
        state = analyze_instr_switch(ctx, (struct mir_instr_switch *)instr);
        break;
    case MIR_INSTR_SET_INITIALIZER:
        state = analyze_instr_set_initializer(ctx, (struct mir_instr_set_initializer *)instr);
        break;
    case MIR_INSTR_TEST_CASES:
        state = analyze_instr_test_cases(ctx, (struct mir_instr_test_case *)instr);
        break;
    case MIR_INSTR_CALL_LOC:
        state = analyze_instr_call_loc(ctx, (struct mir_instr_call_loc *)instr);
        break;
    case MIR_INSTR_UNROLL:
        state = analyze_instr_unroll(ctx, (struct mir_instr_unroll *)instr);
        break;
    default:
        babort("Missing analyze of instruction!");
    }
    ctx->analyze.last_analyzed_instr = NULL;
    if (state.state == ANALYZE_PASSED) {
        setflag(instr->flags, MIR_IS_ANALYZED);
        if (instr->kind == MIR_INSTR_CAST && ((struct mir_instr_cast *)instr)->auto_cast) {
            // An auto cast cannot be directly evaluated because it's destination type
            // could change based on usage.
            return_zone(state);
        }
        if (!evaluate(ctx, instr)) {
            state = ANALYZE_RESULT(FAILED, 0);
        }
    }
    return_zone(state);
}

static INLINE struct mir_instr *analyze_try_get_next(struct mir_instr *instr)
{
    if (!instr) return NULL;
    if (instr->kind == MIR_INSTR_BLOCK) {
        struct mir_instr_block *block = (struct mir_instr_block *)instr;
        return block->entry_instr;
    }
    // Instruction can be the last instruction inside block, but block may not
    // be the last block inside function, we try to get following one.
    struct mir_instr_block *owner_block = instr->owner_block;
    if (owner_block && instr == owner_block->last_instr) {
        if (owner_block->base.next == NULL && owner_block->owner_fn) {
            // Instruction is last instruction of the function body, so the
            // function can be executed in compile time if needed, we need to
            // set flag with this information here.
            owner_block->owner_fn->is_fully_analyzed = true;
        }
        // Return following block.
        return owner_block->base.next;
    }
    return instr->next;
}

void analyze(struct context *ctx)
{
    zone();
    struct result     result;
    usize             pc = 0, i = 0, si = analyze_swap(ctx);
    struct mir_instr *ip = NULL, *pip = NULL;
    bool              skip = false;

    while (true) {
        pip = ip;
        ip  = skip ? NULL : analyze_try_get_next(ip);
        // Remove unused instructions here!
        if (pip && isflag(pip->flags, MIR_IS_ANALYZED)) erase_instr_tree(pip, false, false);
        if (ip == NULL) {
            if (i >= arrlenu(ctx->analyze.stack[si])) {
                // No other instructions in current analyzed stack, let's try the other one.
                arrsetlen(ctx->analyze.stack[si], 0);
                i  = 0;
                si = analyze_swap(ctx);
                if (arrlenu(ctx->analyze.stack[si]) == 0) break;
            }
            ip   = ctx->analyze.stack[si][i++];
            skip = false;
        }
        bassert(ip);
        result = analyze_instr(ctx, ip);

        switch (result.state) {
        case ANALYZE_PASSED:
            pc = 0;
            break;

        case ANALYZE_SKIP:
        case ANALYZE_FAILED:
            skip = true;
            pc   = 0;
            break;

        case ANALYZE_POSTPONE:
            skip = true;
            // This is preventing analyze endless looping in case one or more instructions are
            // postponed every time. We do not reschedule analyze of the instruction when
            // postpone count reach total instruction pending count (analyze stack contains only
            // postponed instructions).
            if (pc++ <= analyze_pending_count(ctx)) analyze_schedule(ctx, ip);
            break;

        case ANALYZE_WAITING: {
            instrs_t    *wq;
            const hash_t hash  = result.waiting_for;
            s64          index = hmgeti(ctx->analyze.waiting, hash);
            if (index == -1) {
                hmput(ctx->analyze.waiting, hash, ((instrs_t)SARR_ZERO));
                index = hmgeti(ctx->analyze.waiting, hash);
                bassert(index >= -1);
                wq = &ctx->analyze.waiting[index].value;
            } else {
                wq = &ctx->analyze.waiting[index].value;
            }
            bassert(wq);
            sarrput(wq, ip);
            skip = true;
            pc   = 0;
        }
        }
    }
    return_zone();
}

void analyze_report_unresolved(struct context *ctx)
{
    s32 reported = 0;

    for (usize i = 0; i < hmlenu(ctx->analyze.waiting); ++i) {
        instrs_t *wq = &ctx->analyze.waiting[i].value;
        bassert(wq);
        for (usize j = 0; j < sarrlenu(wq); ++j) {
            struct mir_instr *instr = sarrpeek(wq, j);
            bassert(instr);
            const char *sym_name = NULL;
            switch (instr->kind) {
            case MIR_INSTR_DECL_REF: {
                struct mir_instr_decl_ref *ref = (struct mir_instr_decl_ref *)instr;
                if (!ref->scope) continue;
                if (!ref->rid) continue;
                sym_name = ref->rid->str;
                if (scope_is_local(ref->scope)) break;
                if (scope_lookup(ref->scope, SCOPE_DEFAULT_LAYER, ref->rid, true, false, NULL)) {
                    continue;
                }
                break;
            }
            default:
                continue;
            }
            bassert(sym_name && "Invalid unresolved symbol name!");
            report_error(UNKNOWN_SYMBOL, instr->node, "Unknown symbol '%s'.", sym_name);
            ++reported;
        }
    }
    if (hmlen(ctx->analyze.waiting) && !reported) {
        report_error(UNKNOWN_SYMBOL,
                     NULL,
                     "Unknown symbol/s detected but not correctly reported, this is compiler bug!");
    }
}

void analyze_report_unused(struct context *ctx)
{
    for (usize i = 0; i < arrlenu(ctx->analyze.usage_check_arr); ++i) {
        struct scope_entry *entry = ctx->analyze.usage_check_arr[i];
        if (entry->ref_count > 0) continue;
        if (!entry->node || !entry->id) continue;
        bassert(entry->node->location);
        report_warning(entry->node,
                       "Unused symbol '%s'. (Use unnamed identificator '_' if this is intentional)",
                       entry->id->str);
    }
}

struct mir_var *testing_gen_meta(struct context *ctx)
{
    const s32 len = ctx->testing.expected_test_count;
    if (len == 0) return NULL;
    if (ctx->assembly->testing.meta_var) return ctx->assembly->testing.meta_var;

    struct mir_type *type = create_type_array(ctx, NULL, ctx->builtin_types->t_TestCase, len);
    struct mir_var  *var  = create_var_impl(ctx, NULL, IMPL_TESTCASES_TMP, type, false, true, true);
    vm_alloc_global(ctx->vm, ctx->assembly, var);

    ctx->assembly->testing.meta_var = var;
    return var;
}

INLINE void testing_add_test_case(struct context *ctx, struct mir_fn *fn)
{
    struct mir_var *var = testing_gen_meta(ctx);
    bassert(var);
    bassert(var->value.data);

    arrput(ctx->assembly->testing.cases, fn);
    const usize i = arrlenu(ctx->assembly->testing.cases) - 1;

    vm_stack_ptr_t   var_ptr  = vm_read_var(ctx->vm, var);
    struct mir_type *var_type = var->value.type;
    bassert(var_type->kind == MIR_TYPE_ARRAY);
    struct mir_type *elem_type = var_type->data.array.elem_type;
    const ptrdiff_t  offset    = vm_get_array_elem_offset(var_type, (u32)i);

    struct mir_type *func_type = mir_get_struct_elem_type(elem_type, 0);
    struct mir_type *name_type = mir_get_struct_elem_type(elem_type, 1);

    vm_stack_ptr_t func_ptr = vm_get_struct_elem_ptr(ctx->assembly, elem_type, var_ptr + offset, 0);
    vm_stack_ptr_t name_ptr = vm_get_struct_elem_ptr(ctx->assembly, elem_type, var_ptr + offset, 1);

    bassert(fn->id);

    vm_write_ptr(func_type, func_ptr, (vm_stack_ptr_t)fn);
    vm_write_string(ctx->vm, name_type, name_ptr, fn->id->str, strlen(fn->id->str));
}

// Top-level rtti generation.
INLINE struct mir_var *rtti_gen(struct context *ctx, struct mir_type *type)
{
    struct mir_var *tmp     = _rtti_gen(ctx, type);
    rttis_t        *pending = &ctx->analyze.incomplete_rtti;
    while (sarrlenu(pending)) {
        struct rtti_incomplete incomplete = sarrpop(pending);
        rtti_satisfy_incomplete(ctx, &incomplete);
    }

    return tmp;
}

void rtti_satisfy_incomplete(struct context *ctx, struct rtti_incomplete *incomplete)
{
    struct mir_type *type     = incomplete->type;
    struct mir_var  *rtti_var = incomplete->var;

    bassert(type->kind == MIR_TYPE_PTR);
    rtti_gen_ptr(ctx, type, rtti_var);
}

struct mir_var *_rtti_gen(struct context *ctx, struct mir_type *type)
{
    bassert(type);
    if (assembly_has_rtti(ctx->assembly, type->id.hash))
        return assembly_get_rtti(ctx->assembly, type->id.hash);

    struct mir_var *rtti_var = NULL;

    switch (type->kind) {
    case MIR_TYPE_INT:
        rtti_var = rtti_gen_integer(ctx, type);
        break;

    case MIR_TYPE_ENUM:
        rtti_var = rtti_gen_enum(ctx, type);
        break;

    case MIR_TYPE_REAL:
        rtti_var = rtti_gen_real(ctx, type);
        break;

    case MIR_TYPE_BOOL:
        rtti_var = rtti_gen_empty(ctx, type, ctx->builtin_types->t_TypeInfoBool);
        break;

    case MIR_TYPE_TYPE:
        rtti_var = rtti_gen_empty(ctx, type, ctx->builtin_types->t_TypeInfoType);
        break;

    case MIR_TYPE_VOID:
        rtti_var = rtti_gen_empty(ctx, type, ctx->builtin_types->t_TypeInfoVoid);
        break;

    case MIR_TYPE_NULL:
        rtti_var = rtti_gen_empty(ctx, type, ctx->builtin_types->t_TypeInfoNull);
        break;

    case MIR_TYPE_STRING:
        rtti_var = rtti_gen_empty(ctx, type, ctx->builtin_types->t_TypeInfoString);
        break;

    case MIR_TYPE_PTR:
        // We generate dummy pointer RTTI when incomplete is enabled and complete
        // this in second pass to prove endless looping.
        rtti_var = rtti_gen_ptr(ctx, ctx->builtin_types->t_u8_ptr, NULL);
        sarrput(&ctx->analyze.incomplete_rtti,
                ((struct rtti_incomplete){.var = rtti_var, .type = type}));
        break;

    case MIR_TYPE_ARRAY:
        rtti_var = rtti_gen_array(ctx, type);
        break;

    case MIR_TYPE_DYNARR:
    case MIR_TYPE_SLICE:
    case MIR_TYPE_VARGS:
    case MIR_TYPE_STRUCT:
        rtti_var = rtti_gen_struct(ctx, type);
        break;

    case MIR_TYPE_FN:
        rtti_var = rtti_gen_fn(ctx, type);
        break;

    case MIR_TYPE_FN_GROUP:
        rtti_var = rtti_gen_fn_group(ctx, type);
        break;

    default: {
        char type_name[256];
        mir_type_to_str(type_name, 256, type, true);
        babort("missing RTTI generation for type '%s'", type_name);
    }
    }

    bassert(rtti_var);
    assembly_add_rtti(ctx->assembly, type->id.hash, rtti_var);
    return rtti_var;
}

INLINE struct mir_var *rtti_create_and_alloc_var(struct context *ctx, struct mir_type *type)
{
    struct mir_var *var = create_var_impl(ctx, NULL, IMPL_RTTI_ENTRY, type, false, true, true);
    vm_alloc_global(ctx->vm, ctx->assembly, var);
    return var;
}

static INLINE void
rtti_gen_base(struct context *ctx, vm_stack_ptr_t dest, u8 kind, usize size_bytes)
{
    struct mir_type *rtti_type      = ctx->builtin_types->t_TypeInfo;
    struct mir_type *dest_kind_type = mir_get_struct_elem_type(rtti_type, 0);
    vm_stack_ptr_t   dest_kind      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 0);

    struct mir_type *dest_size_bytes_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_size_bytes = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);

    vm_write_int(dest_kind_type, dest_kind, (u64)kind);
    vm_write_int(dest_size_bytes_type, dest_size_bytes, (u64)size_bytes);
}

struct mir_var *rtti_gen_integer(struct context *ctx, struct mir_type *type)
{
    struct mir_type *rtti_type = ctx->builtin_types->t_TypeInfoInt;
    struct mir_var  *rtti_var  = rtti_create_and_alloc_var(ctx, rtti_type);
    vm_stack_ptr_t   dest      = vm_read_var(ctx->vm, rtti_var);
    rtti_gen_base(ctx, dest, type->kind, type->store_size_bytes);

    struct mir_type *dest_bit_count_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_bit_count = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);

    struct mir_type *dest_is_signed_type = mir_get_struct_elem_type(rtti_type, 2);
    vm_stack_ptr_t   dest_is_signed = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 2);

    vm_write_int(dest_bit_count_type, dest_bit_count, (u64)type->data.integer.bitcount);
    vm_write_int(dest_is_signed_type, dest_is_signed, (u64)type->data.integer.is_signed);

    return rtti_var;
}

struct mir_var *rtti_gen_real(struct context *ctx, struct mir_type *type)
{
    struct mir_type *rtti_type = ctx->builtin_types->t_TypeInfoReal;
    struct mir_var  *rtti_var  = rtti_create_and_alloc_var(ctx, rtti_type);
    vm_stack_ptr_t   dest      = vm_read_var(ctx->vm, rtti_var);
    rtti_gen_base(ctx, dest, type->kind, type->store_size_bytes);

    struct mir_type *dest_bit_count_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_bit_count = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);

    vm_write_int(dest_bit_count_type, dest_bit_count, (u64)type->data.real.bitcount);
    return rtti_var;
}

struct mir_var *rtti_gen_ptr(struct context *ctx, struct mir_type *type, struct mir_var *incomplete)
{
    struct mir_var *rtti_var =
        incomplete ? incomplete : rtti_create_and_alloc_var(ctx, ctx->builtin_types->t_TypeInfoPtr);

    vm_stack_ptr_t dest = vm_read_var(ctx->vm, rtti_var);
    rtti_gen_base(ctx, dest, type->kind, type->store_size_bytes);

    struct mir_type *dest_pointee_type =
        mir_get_struct_elem_type(ctx->builtin_types->t_TypeInfoPtr, 1);
    vm_stack_ptr_t dest_pointee =
        vm_get_struct_elem_ptr(ctx->assembly, ctx->builtin_types->t_TypeInfoPtr, dest, 1);

    struct mir_var *pointee = _rtti_gen(ctx, type->data.ptr.expr);
    vm_write_ptr(dest_pointee_type, dest_pointee, vm_read_var(ctx->vm, pointee));

    return rtti_var;
}

struct mir_var *rtti_gen_array(struct context *ctx, struct mir_type *type)
{
    struct mir_type *rtti_type = ctx->builtin_types->t_TypeInfoArray;
    struct mir_var  *rtti_var  = rtti_create_and_alloc_var(ctx, rtti_type);
    vm_stack_ptr_t   dest      = vm_read_var(ctx->vm, rtti_var);
    rtti_gen_base(ctx, dest, type->kind, type->store_size_bytes);

    // name
    struct mir_type *dest_name_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_name      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);
    const char      *name           = type->user_id ? type->user_id->str : type->id.str;

    vm_write_string(ctx->vm, dest_name_type, dest_name, name, strlen(name));

    // elem_type
    struct mir_type *dest_elem_type = mir_get_struct_elem_type(rtti_type, 2);
    vm_stack_ptr_t   dest_elem      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 2);

    struct mir_var *elem = _rtti_gen(ctx, type->data.array.elem_type);
    vm_write_ptr(dest_elem_type, dest_elem, vm_read_var(ctx->vm, elem));

    // len
    struct mir_type *dest_len_type = mir_get_struct_elem_type(rtti_type, 3);
    vm_stack_ptr_t   dest_len      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 3);

    vm_write_int(dest_len_type, dest_len, (u64)type->data.array.len);

    return rtti_var;
}

struct mir_var *
rtti_gen_empty(struct context *ctx, struct mir_type *type, struct mir_type *rtti_type)
{
    struct mir_var *rtti_var = rtti_create_and_alloc_var(ctx, rtti_type);
    vm_stack_ptr_t  dest     = vm_read_var(ctx->vm, rtti_var);
    rtti_gen_base(ctx, dest, type->kind, type->store_size_bytes);
    return rtti_var;
}

void rtti_gen_enum_variant(struct context *ctx, vm_stack_ptr_t dest, struct mir_variant *variant)
{
    struct mir_type *rtti_type      = ctx->builtin_types->t_TypeInfoEnumVariant;
    struct mir_type *dest_name_type = mir_get_struct_elem_type(rtti_type, 0);
    vm_stack_ptr_t   dest_name      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 0);

    struct mir_type *dest_value_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_value      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);

    vm_write_string(ctx->vm, dest_name_type, dest_name, variant->id->str, strlen(variant->id->str));
    vm_write_int(dest_value_type, dest_value, variant->value);
}

vm_stack_ptr_t rtti_gen_enum_variants_array(struct context *ctx, mir_variants_t *variants)
{
    struct mir_type *rtti_type    = ctx->builtin_types->t_TypeInfoEnumVariant;
    struct mir_type *arr_tmp_type = create_type_array(ctx, NULL, rtti_type, sarrlen(variants));
    vm_stack_ptr_t   dest_arr_tmp = vm_alloc_raw(ctx->vm, ctx->assembly, arr_tmp_type);
    for (usize i = 0; i < sarrlenu(variants); ++i) {
        struct mir_variant *it = sarrpeek(variants, i);
        vm_stack_ptr_t      dest_arr_tmp_elem =
            vm_get_array_elem_ptr(arr_tmp_type, dest_arr_tmp, (u32)i);
        rtti_gen_enum_variant(ctx, dest_arr_tmp_elem, it);
    }
    return dest_arr_tmp;
}

void rtti_gen_enum_variants_slice(struct context *ctx,
                                  vm_stack_ptr_t  dest,
                                  mir_variants_t *variants)
{
    struct mir_type *rtti_type     = ctx->builtin_types->t_TypeInfoEnumVariants_slice;
    struct mir_type *dest_len_type = mir_get_struct_elem_type(rtti_type, 0);
    vm_stack_ptr_t   dest_len      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 0);
    struct mir_type *dest_ptr_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_ptr      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);
    vm_stack_ptr_t   variants_ptr  = rtti_gen_enum_variants_array(ctx, variants);
    vm_write_int(dest_len_type, dest_len, sarrlenu(variants));
    vm_write_ptr(dest_ptr_type, dest_ptr, variants_ptr);
}

struct mir_var *rtti_gen_enum(struct context *ctx, struct mir_type *type)
{
    struct mir_type *rtti_type = ctx->builtin_types->t_TypeInfoEnum;
    struct mir_var  *rtti_var  = rtti_create_and_alloc_var(ctx, rtti_type);
    vm_stack_ptr_t   dest      = vm_read_var(ctx->vm, rtti_var);
    rtti_gen_base(ctx, dest, type->kind, type->store_size_bytes);

    // name
    struct mir_type *dest_name_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_name      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);
    const char      *name           = type->user_id ? type->user_id->str : type->id.str;
    vm_write_string(ctx->vm, dest_name_type, dest_name, name, strlen(name));

    // base_type
    struct mir_type *dest_base_type_type = mir_get_struct_elem_type(rtti_type, 2);
    vm_stack_ptr_t   dest_base_type = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 2);
    struct mir_var  *base_type      = _rtti_gen(ctx, type->data.enm.base_type);
    vm_write_ptr(dest_base_type_type, dest_base_type, vm_read_var(ctx->vm, base_type));

    // variants
    vm_stack_ptr_t dest_variants = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 3);
    rtti_gen_enum_variants_slice(ctx, dest_variants, type->data.enm.variants);

    // is_flags
    struct mir_type *dest_is_flags_type = mir_get_struct_elem_type(rtti_type, 4);
    vm_stack_ptr_t   dest_is_flags      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 4);
    vm_write_int(dest_is_flags_type, dest_is_flags, (u64)type->data.enm.is_flags);

    return rtti_var;
}

void rtti_gen_struct_member(struct context *ctx, vm_stack_ptr_t dest, struct mir_member *member)
{
    struct mir_type *rtti_type = ctx->builtin_types->t_TypeInfoStructMember;

    // name
    struct mir_type *dest_name_type = mir_get_struct_elem_type(rtti_type, 0);
    vm_stack_ptr_t   dest_name      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 0);
    vm_write_string(ctx->vm, dest_name_type, dest_name, member->id->str, strlen(member->id->str));

    // base_type
    struct mir_type *dest_base_type_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_base_type = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);
    struct mir_var  *base_type      = _rtti_gen(ctx, member->type);
    vm_write_ptr(dest_base_type_type, dest_base_type, vm_read_var(ctx->vm, base_type));

    // offset_bytes
    struct mir_type *dest_offset_type = mir_get_struct_elem_type(rtti_type, 2);
    vm_stack_ptr_t   dest_offset      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 2);
    vm_write_int(dest_offset_type, dest_offset, (u64)member->offset_bytes);

    // index
    struct mir_type *dest_index_type = mir_get_struct_elem_type(rtti_type, 3);
    vm_stack_ptr_t   dest_index      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 3);
    vm_write_int(dest_index_type, dest_index, (u64)member->index);

    // tag
    struct mir_type *dest_tags_type = mir_get_struct_elem_type(rtti_type, 4);
    vm_stack_ptr_t   dest_tags      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 4);
    vm_write_int(dest_tags_type, dest_tags, (u64)member->tags);

    // is_base
    struct mir_type *dest_is_base_type = mir_get_struct_elem_type(rtti_type, 5);
    vm_stack_ptr_t   dest_is_base      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 5);
    vm_write_int(dest_is_base_type, dest_is_base, (u64)member->is_base);
}

vm_stack_ptr_t rtti_gen_struct_members_array(struct context *ctx, mir_members_t *members)
{
    struct mir_type *rtti_type    = ctx->builtin_types->t_TypeInfoStructMember;
    struct mir_type *arr_tmp_type = create_type_array(ctx, NULL, rtti_type, sarrlen(members));
    vm_stack_ptr_t   dest_arr_tmp = vm_alloc_raw(ctx->vm, ctx->assembly, arr_tmp_type);
    for (usize i = 0; i < sarrlenu(members); ++i) {
        struct mir_member *it = sarrpeek(members, i);
        vm_stack_ptr_t     dest_arr_tmp_elem =
            vm_get_array_elem_ptr(arr_tmp_type, dest_arr_tmp, (u32)i);
        rtti_gen_struct_member(ctx, dest_arr_tmp_elem, it);
    }
    return dest_arr_tmp;
}

void rtti_gen_struct_members_slice(struct context *ctx, vm_stack_ptr_t dest, mir_members_t *members)
{
    struct mir_type *rtti_type     = ctx->builtin_types->t_TypeInfoStructMembers_slice;
    struct mir_type *dest_len_type = mir_get_struct_elem_type(rtti_type, 0);
    vm_stack_ptr_t   dest_len      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 0);

    struct mir_type *dest_ptr_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_ptr      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);

    vm_stack_ptr_t members_ptr = rtti_gen_struct_members_array(ctx, members);

    vm_write_int(dest_len_type, dest_len, sarrlenu(members));
    vm_write_ptr(dest_ptr_type, dest_ptr, members_ptr);
}

struct mir_var *rtti_gen_struct(struct context *ctx, struct mir_type *type)
{
    bassert(!is_incomplete_struct_type(type) &&
            "Attempt to generate RTTI for incomplete struct type!");
    struct mir_type *rtti_type = ctx->builtin_types->t_TypeInfoStruct;
    struct mir_var  *rtti_var  = rtti_create_and_alloc_var(ctx, rtti_type);
    vm_stack_ptr_t   dest      = vm_read_var(ctx->vm, rtti_var);
    rtti_gen_base(ctx, dest, MIR_TYPE_STRUCT, type->store_size_bytes);

    // name
    struct mir_type *dest_name_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_name      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);
    const char      *name           = type->user_id ? type->user_id->str : type->id.str;
    vm_write_string(ctx->vm, dest_name_type, dest_name, name, strlen(name));

    // members
    vm_stack_ptr_t dest_members = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 2);
    rtti_gen_struct_members_slice(ctx, dest_members, type->data.strct.members);

    // is_slice
    struct mir_type *dest_is_slice_type = mir_get_struct_elem_type(rtti_type, 3);
    vm_stack_ptr_t   dest_is_slice      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 3);
    const bool       is_slice = type->kind == MIR_TYPE_SLICE || type->kind == MIR_TYPE_VARGS;
    vm_write_int(dest_is_slice_type, dest_is_slice, (u64)is_slice);

    // is_union
    struct mir_type *dest_is_union_type = mir_get_struct_elem_type(rtti_type, 4);
    vm_stack_ptr_t   dest_is_union      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 4);
    vm_write_int(dest_is_union_type, dest_is_union, (u64)type->data.strct.is_union);

    // is_dynamic_array
    struct mir_type *dest_is_da_type = mir_get_struct_elem_type(rtti_type, 5);
    vm_stack_ptr_t   dest_is_da      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 5);
    const bool       is_da           = type->kind == MIR_TYPE_DYNARR;
    vm_write_int(dest_is_da_type, dest_is_da, (u64)is_da);

    return rtti_var;
}

void rtti_gen_fn_arg(struct context *ctx, vm_stack_ptr_t dest, struct mir_arg *arg)
{
    struct mir_type *rtti_type = ctx->builtin_types->t_TypeInfoFnArg;

    // name
    struct mir_type *dest_name_type = mir_get_struct_elem_type(rtti_type, 0);
    vm_stack_ptr_t   dest_name      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 0);
    const char      *arg_name       = arg->id ? arg->id->str : "";
    vm_write_string(ctx->vm, dest_name_type, dest_name, arg_name, strlen(arg_name));

    // base_type
    struct mir_type *dest_base_type_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_base_type = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);
    struct mir_var  *base_type      = _rtti_gen(ctx, arg->type);
    vm_write_ptr(dest_base_type_type, dest_base_type, base_type->value.data);
}

vm_stack_ptr_t rtti_gen_fn_args_array(struct context *ctx, mir_args_t *args)
{
    struct mir_type *rtti_type    = ctx->builtin_types->t_TypeInfoFnArg;
    struct mir_type *arr_tmp_type = create_type_array(ctx, NULL, rtti_type, (s64)sarrlenu(args));

    vm_stack_ptr_t dest_arr_tmp = vm_alloc_raw(ctx->vm, ctx->assembly, arr_tmp_type);

    for (usize i = 0; i < sarrlenu(args); ++i) {
        struct mir_arg *it = sarrpeek(args, i);
        vm_stack_ptr_t  dest_arr_tmp_elem =
            vm_get_array_elem_ptr(arr_tmp_type, dest_arr_tmp, (u32)i);
        rtti_gen_fn_arg(ctx, dest_arr_tmp_elem, it);
    }

    return dest_arr_tmp;
}

vm_stack_ptr_t rtti_gen_fns_array(struct context *ctx, mir_types_t *fns)
{
    struct mir_type *rtti_type    = ctx->builtin_types->t_TypeInfoFn_ptr;
    struct mir_type *arr_tmp_type = create_type_array(ctx, NULL, rtti_type, sarrlen(fns));
    vm_stack_ptr_t   dest_arr_tmp = vm_alloc_raw(ctx->vm, ctx->assembly, arr_tmp_type);
    for (usize i = 0; i < sarrlenu(fns); ++i) {
        struct mir_type *it = sarrpeek(fns, i);
        vm_stack_ptr_t   dest_arr_tmp_elem =
            vm_get_array_elem_ptr(arr_tmp_type, dest_arr_tmp, (u32)i);
        struct mir_var *fn = _rtti_gen(ctx, it);
        vm_write_ptr(rtti_type, dest_arr_tmp_elem, vm_read_var(ctx->vm, fn));
    }
    return dest_arr_tmp;
}

void rtti_gen_fn_args_slice(struct context *ctx, vm_stack_ptr_t dest, mir_args_t *args)
{
    struct mir_type *rtti_type     = ctx->builtin_types->t_TypeInfoFnArgs_slice;
    struct mir_type *dest_len_type = mir_get_struct_elem_type(rtti_type, 0);
    vm_stack_ptr_t   dest_len      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 0);

    struct mir_type *dest_ptr_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_ptr      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);

    const usize    argc     = sarrlenu(args);
    vm_stack_ptr_t args_ptr = argc ? rtti_gen_fn_args_array(ctx, args) : NULL;

    vm_write_int(dest_len_type, dest_len, argc);
    vm_write_ptr(dest_ptr_type, dest_ptr, args_ptr);
}

void rtti_gen_fn_slice(struct context *ctx, vm_stack_ptr_t dest, mir_types_t *fns)
{
    struct mir_type *rtti_type     = ctx->builtin_types->t_TypeInfoFn_ptr_slice;
    struct mir_type *dest_len_type = mir_get_struct_elem_type(rtti_type, 0);
    vm_stack_ptr_t   dest_len      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 0);

    struct mir_type *dest_ptr_type = mir_get_struct_elem_type(rtti_type, 1);
    vm_stack_ptr_t   dest_ptr      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);

    const usize    fnc     = sarrlenu(fns);
    vm_stack_ptr_t fns_ptr = fnc ? fns_ptr = rtti_gen_fns_array(ctx, fns) : NULL;

    vm_write_int(dest_len_type, dest_len, fnc);
    vm_write_ptr(dest_ptr_type, dest_ptr, fns_ptr);
}

struct mir_var *rtti_gen_fn(struct context *ctx, struct mir_type *type)
{
    struct mir_type *rtti_type = ctx->builtin_types->t_TypeInfoFn;
    struct mir_var  *rtti_var  = rtti_create_and_alloc_var(ctx, rtti_type);
    vm_stack_ptr_t   dest      = vm_read_var(ctx->vm, rtti_var);
    rtti_gen_base(ctx, dest, type->kind, type->store_size_bytes);

#if 0
	// name
	struct mir_type *   dest_name_type = mir_get_struct_elem_type(rtti_type, 1);
	vm_stack_ptr_t  dest_name      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);
	const char *name           = type->user_id ? type->user_id->str : type->id.str;
	vm_write_string(ctx->vm, dest_name_type, dest_name, name, strlen(name));
#endif

    // args
    vm_stack_ptr_t dest_args = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);
    rtti_gen_fn_args_slice(ctx, dest_args, type->data.fn.args);

    // ret_type
    struct mir_type *dest_ret_type_type = mir_get_struct_elem_type(rtti_type, 2);
    vm_stack_ptr_t   dest_ret_type      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 2);
    struct mir_var  *ret_type           = _rtti_gen(ctx, type->data.fn.ret_type);
    vm_write_ptr(dest_ret_type_type, dest_ret_type, ret_type->value.data);

    // is_vargs
    struct mir_type *dest_is_vargs_type = mir_get_struct_elem_type(rtti_type, 3);
    vm_stack_ptr_t   dest_is_vargs      = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 3);
    const bool       is_vargs           = isflag(type->data.fn.flags, MIR_TYPE_FN_FLAG_IS_VARGS);
    vm_write_int(dest_is_vargs_type, dest_is_vargs, (u64)is_vargs);

    return rtti_var;
}

struct mir_var *rtti_gen_fn_group(struct context *ctx, struct mir_type *type)
{
    struct mir_type *rtti_type = ctx->builtin_types->t_TypeInfoFnGroup;
    struct mir_var  *rtti_var  = rtti_create_and_alloc_var(ctx, rtti_type);
    vm_stack_ptr_t   dest      = vm_read_var(ctx->vm, rtti_var);
    rtti_gen_base(ctx, dest, type->kind, type->store_size_bytes);

    // variants
    vm_stack_ptr_t dest_args = vm_get_struct_elem_ptr(ctx->assembly, rtti_type, dest, 1);
    rtti_gen_fn_slice(ctx, dest_args, type->data.fn_group.variants);

    return rtti_var;
}

// MIR builting
void ast_defer_block(struct context *ctx, struct ast *block, bool whole_tree)
{
    bassert(ctx->ast.current_defer_stack_index >= 0);
    defer_stack_t *stack = &ctx->ast.defer_stack[ctx->ast.current_defer_stack_index];
    struct ast    *defer;
    for (usize i = sarrlenu(stack); i-- > 0;) {
        defer = sarrpeek(stack, i);
        if (defer->owner_scope == block->owner_scope) {
            sarrpop(stack);
        } else if (!whole_tree) {
            break;
        }
        ast(ctx, defer->data.stmt_defer.expr);
    }
}

void ast_ublock(struct context *ctx, struct ast *ublock)
{
    for (usize i = 0; i < arrlenu(ublock->data.ublock.nodes); ++i) {
        ast(ctx, ublock->data.ublock.nodes[i]);
    }
}

void ast_block(struct context *ctx, struct ast *block)
{
    for (usize i = 0; i < sarrlenu(block->data.block.nodes); ++i) {
        struct ast *tmp = sarrpeek(block->data.block.nodes, i);
        ast(ctx, tmp);
    }
    if (!block->data.block.has_return) ast_defer_block(ctx, block, false);
}

void ast_unreachable(struct context *ctx, struct ast *unr)
{
    append_instr_unreachable(ctx, unr);
}

void ast_debugbreak(struct context *ctx, struct ast *debug_break)
{
    append_instr_debugbreak(ctx, debug_break);
}

void ast_stmt_if(struct context *ctx, struct ast *stmt_if)
{
    struct ast *ast_condition = stmt_if->data.stmt_if.test;
    struct ast *ast_then      = stmt_if->data.stmt_if.true_stmt;
    struct ast *ast_else      = stmt_if->data.stmt_if.false_stmt;
    bassert(ast_condition && ast_then);
    struct mir_fn *fn = ast_current_fn(ctx);
    bassert(fn);

    const bool              is_static      = stmt_if->data.stmt_if.is_static;
    struct mir_instr_block *tmp_block      = NULL;
    struct mir_instr_block *then_block     = append_block(ctx, fn, "if_then");
    struct mir_instr_block *continue_block = append_block(ctx, fn, "if_continue");
    struct mir_instr       *cond           = ast(ctx, ast_condition);

    // Note: Else block is optional in this case i.e. if (true) { ... } expression does not have
    // else block at all, so there is no need to generate one and 'conditional break' just
    // breaks into the `continue` block. Skipping generation of else block also fixes issue with
    // missing DI location of the last break emitted into the empty else block. Obviously there
    // is no good source code possition for something not existing in source code!
    const bool              has_else_branch = ast_else;
    struct mir_instr_block *else_block      = NULL;
    if (has_else_branch) {
        else_block = append_block(ctx, fn, "if_else");
        append_instr_cond_br(ctx, stmt_if, cond, then_block, else_block, is_static);
    } else {
        append_instr_cond_br(ctx, stmt_if, cond, then_block, continue_block, is_static);
    }

    // then block
    set_current_block(ctx, then_block);
    ast(ctx, ast_then);
    tmp_block = ast_current_block(ctx);
    if (!get_block_terminator(tmp_block)) {
        // block has not been terminated -> add terminator
        struct ast *location_node = get_last_instruction_node(tmp_block);
        append_instr_br(ctx, location_node, continue_block);
    }

    // else if
    if (has_else_branch) {
        bassert(else_block);
        set_current_block(ctx, else_block);
        ast(ctx, ast_else);

        tmp_block = ast_current_block(ctx);
        if (!is_block_terminated(tmp_block)) {
            append_instr_br(ctx, get_last_instruction_node(tmp_block), continue_block);
        }

        if (!is_block_terminated(else_block)) {
            set_current_block(ctx, else_block);
            append_instr_br(ctx, get_last_instruction_node(else_block), continue_block);
        }
    }
    set_current_block(ctx, continue_block);
}

void ast_stmt_loop(struct context *ctx, struct ast *loop)
{
    struct ast *ast_block     = loop->data.stmt_loop.block;
    struct ast *ast_condition = loop->data.stmt_loop.condition;
    struct ast *ast_increment = loop->data.stmt_loop.increment;
    struct ast *ast_init      = loop->data.stmt_loop.init;
    bassert(ast_block);
    struct mir_fn *fn = ast_current_fn(ctx);
    bassert(fn);
    // prepare all blocks
    struct mir_instr_block *tmp_block = NULL;
    struct mir_instr_block *increment_block =
        ast_increment ? append_block(ctx, fn, "loop_increment") : NULL;
    struct mir_instr_block *decide_block        = append_block(ctx, fn, "loop_decide");
    struct mir_instr_block *body_block          = append_block(ctx, fn, "loop_body");
    struct mir_instr_block *continue_block      = append_block(ctx, fn, "loop_continue");
    struct mir_instr_block *prev_break_block    = ctx->ast.break_block;
    struct mir_instr_block *prev_continue_block = ctx->ast.continue_block;
    ctx->ast.break_block                        = continue_block;
    ctx->ast.continue_block                     = ast_increment ? increment_block : decide_block;
    // generate initialization if there is one
    if (ast_init) ast(ctx, ast_init);
    // decide block
    append_instr_br(ctx, ast_condition, decide_block);
    set_current_block(ctx, decide_block);
    struct mir_instr *condition =
        ast_condition ? ast(ctx, ast_condition) : append_instr_const_bool(ctx, NULL, true);
    append_instr_cond_br(ctx, ast_condition, condition, body_block, continue_block, false);
    // loop body
    set_current_block(ctx, body_block);
    ast(ctx, ast_block);
    tmp_block = ast_current_block(ctx);
    if (!is_block_terminated(tmp_block)) {
        append_instr_br(ctx, ast_block, ast_increment ? increment_block : decide_block);
    }
    // increment if there is one
    if (ast_increment) {
        set_current_block(ctx, increment_block);
        ast(ctx, ast_increment);
        append_instr_br(ctx, ast_increment, decide_block);
    }
    ctx->ast.break_block    = prev_break_block;
    ctx->ast.continue_block = prev_continue_block;
    set_current_block(ctx, continue_block);
}

void ast_stmt_break(struct context *ctx, struct ast *br)
{
    bassert(ctx->ast.break_block && "break statement outside the loop");
    append_instr_br(ctx, br, ctx->ast.break_block);
}

void ast_stmt_continue(struct context *ctx, struct ast *cont)
{
    bassert(ctx->ast.continue_block && "break statement outside the loop");
    append_instr_br(ctx, cont, ctx->ast.continue_block);
}

void ast_stmt_switch(struct context *ctx, struct ast *stmt_switch)
{
    ast_nodes_t *ast_cases = stmt_switch->data.stmt_switch.cases;
    bassert(ast_cases);

    mir_switch_cases_t *cases = arena_safe_alloc(&ctx->assembly->arenas.sarr);

    struct mir_fn *fn = ast_current_fn(ctx);
    bassert(fn);

    struct mir_instr_block *src_block            = ast_current_block(ctx);
    struct mir_instr_block *cont_block           = append_block(ctx, fn, "switch_continue");
    struct mir_instr_block *default_block        = cont_block;
    bool                    user_defined_default = false;

    for (usize i = sarrlenu(ast_cases); i-- > 0;) {
        struct ast *ast_case   = sarrpeek(ast_cases, i);
        const bool  is_default = ast_case->data.stmt_case.is_default;

        struct mir_instr_block *case_block = NULL;

        if (ast_case->data.stmt_case.block) {
            case_block = append_block(ctx, fn, is_default ? "switch_default" : "switch_case");
            set_current_block(ctx, case_block);
            ast(ctx, ast_case->data.stmt_case.block);

            struct mir_instr_block *curr_block = ast_current_block(ctx);
            if (!is_block_terminated(curr_block)) {
                struct mir_instr *last_instr = curr_block->last_instr;
                struct ast       *node       = last_instr ? last_instr->node : ast_case;
                append_instr_br(ctx, node, cont_block);
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

        ast_nodes_t *ast_exprs = ast_case->data.stmt_case.exprs;

        for (usize i2 = sarrlenu(ast_exprs); i2-- > 0;) {
            struct ast *ast_expr = sarrpeek(ast_exprs, i2);

            set_current_block(ctx, src_block);
            struct mir_switch_case c = {.on_value = ast(ctx, ast_expr), .block = case_block};
            sarrput(cases, c);
        }
    }

    // Generate instructions for switch value and create switch itself.
    set_current_block(ctx, src_block);
    struct mir_instr *value = ast(ctx, stmt_switch->data.stmt_switch.expr);
    append_instr_switch(ctx, stmt_switch, value, default_block, user_defined_default, cases);
    set_current_block(ctx, cont_block);
}

void ast_stmt_return(struct context *ctx, struct ast *ret)
{
    // Return statement produce only setup of .ret temporary and break into the exit
    // block of the function.
    ast_nodes_t      *ast_values     = ret->data.stmt_return.exprs;
    const bool        is_multireturn = sarrlenu(ast_values) > 1;
    struct mir_instr *value          = NULL;
    if (is_multireturn) {
        // Generate multi-return compound expression to group all values into single one.
        const usize   valc   = sarrlenu(ast_values);
        mir_instrs_t *values = arena_safe_alloc(&ctx->assembly->arenas.sarr);
        sarrsetlen(values, valc);
        struct ast *ast_value = NULL;
        for (usize i = valc; i-- > 0;) {
            ast_value = sarrpeek(ast_values, i);
            value     = ast(ctx, ast_value);
            bassert(value);
            sarrpeek(values, i) = value;
            SET_IS_NAKED_IF_COMPOUND(value, false);
        }
        bassert(ast_value);
        value = append_instr_compound(ctx, ast_value, NULL, values, true);
    } else if (sarrlenu(ast_values) > 0) {
        struct ast *ast_value = sarrpeek(ast_values, 0);
        bassert(ast_value &&
                "Expected at least one return value when return expression array is not NULL.");
        value = ast(ctx, ast_value);
    }
    struct mir_fn *fn = ast_current_fn(ctx);
    if (!is_current_block_terminated(ctx)) {
        bassert(fn);
        if (fn->ret_tmp) {
            if (!value) {
                report_error_after(EXPECTED_EXPR, ret, "Expected return value.");
                return;
            }
            struct mir_instr *ref = append_instr_decl_direct_ref(ctx, NULL, fn->ret_tmp);
            append_instr_store(ctx, ret, value, ref);
        } else if (value) {
            report_error(UNEXPECTED_EXPR, value->node, "Unexpected return value.");
        }
        ast_defer_block(ctx, ret->data.stmt_return.owner_block, true);
    }
    struct mir_instr_block *exit_block = fn->exit_block;
    bassert(exit_block);
    append_instr_br(ctx, ret, exit_block);
}

void ast_stmt_defer(struct context *ctx, struct ast *defer)
{
    // push new defer record
    sarrput(&ctx->ast.defer_stack[ctx->ast.current_defer_stack_index], defer);
}

struct mir_instr *ast_call_loc(struct context *ctx, struct ast *loc)
{
    return append_instr_call_loc(ctx, loc);
}

struct mir_instr *ast_msg(struct context *ctx, struct ast *msg)
{
    return append_instr_msg(ctx, msg);
}

struct mir_instr *ast_expr_compound(struct context *ctx, struct ast *cmp)
{
    ast_nodes_t      *ast_values = cmp->data.expr_compound.values;
    struct ast       *ast_type   = cmp->data.expr_compound.type;
    struct mir_instr *type       = NULL;
    bassert(ast_type);

    type = ast(ctx, ast_type);
    bassert(type);
    if (!ast_values) return append_instr_compound(ctx, cmp, type, NULL, false);
    const usize   valc   = sarrlenu(ast_values);
    mir_instrs_t *values = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    sarrsetlen(values, valc);
    struct ast       *ast_value;
    struct mir_instr *value;
    // Values must be appended in reverse order.
    for (usize i = valc; i-- > 0;) {
        ast_value = sarrpeek(ast_values, i);
        value     = ast(ctx, ast_value);
        bassert(value);
        sarrpeek(values, i) = value;
        SET_IS_NAKED_IF_COMPOUND(value, false);
    }
    return append_instr_compound(ctx, cmp, type, values, false);
}

struct mir_instr *ast_expr_addrof(struct context *ctx, struct ast *addrof)
{
    struct mir_instr *src = ast(ctx, addrof->data.expr_addrof.next);
    bassert(src);

    return append_instr_addrof(ctx, addrof, src);
}

struct mir_instr *ast_expr_cast(struct context *ctx, struct ast *cast)
{
    const bool  auto_cast = cast->data.expr_cast.auto_cast;
    struct ast *ast_type  = cast->data.expr_cast.type;
    struct ast *ast_next  = cast->data.expr_cast.next;
    bassert(ast_next);

    // INCOMPLETE: const type!!!
    struct mir_instr *type = NULL;
    if (!auto_cast) {
        bassert(ast_type);
        type = CREATE_TYPE_RESOLVER_CALL(ast_type);
    }
    struct mir_instr *next = ast(ctx, ast_next);
    return append_instr_cast(ctx, cast, type, next);
}

struct mir_instr *ast_expr_sizeof(struct context *ctx, struct ast *szof)
{
    struct ast *ast_node = szof->data.expr_sizeof.node;
    bassert(ast_node);

    struct mir_instr *expr = ast(ctx, ast_node);
    return append_instr_sizeof(ctx, szof, expr);
}

struct mir_instr *ast_expr_type_info(struct context *ctx, struct ast *type_info)
{
    struct ast *ast_node = type_info->data.expr_type_info.node;
    bassert(ast_node);

    struct mir_instr *expr = ast(ctx, ast_node);
    return append_instr_type_info(ctx, type_info, expr);
}

struct mir_instr *ast_expr_test_cases(struct context *ctx, struct ast *test_cases)
{
    return append_instr_test_cases(ctx, test_cases);
}

struct mir_instr *ast_expr_alignof(struct context *ctx, struct ast *szof)
{
    struct ast *ast_node = szof->data.expr_alignof.node;
    bassert(ast_node);

    struct mir_instr *expr = ast(ctx, ast_node);
    return append_instr_alignof(ctx, szof, expr);
}

struct mir_instr *ast_expr_deref(struct context *ctx, struct ast *deref)
{
    struct mir_instr *next = ast(ctx, deref->data.expr_deref.next);
    bassert(next);
    struct mir_instr_load *load = (struct mir_instr_load *)append_instr_load(ctx, deref, next);
    load->is_deref              = true;
    return &load->base;
}

struct mir_instr *ast_expr_lit_int(struct context *ctx, struct ast *expr)
{
    u64 val = expr->data.expr_integer.val;

    if (expr->data.expr_integer.overflow) {
        report_error(NUM_LIT_OVERFLOW,
                     expr,
                     "Integer literal is too big and cannot be represented as any "
                     "integer type.");
    }

    struct mir_type *type         = NULL;
    const int        desired_bits = count_bits(val);

    // Here we choose best type for const integer literal: s32, s64 or u64. When u64 is
    // selected, this number cannot be negative.
    if (desired_bits < 32) {
        type = ctx->builtin_types->t_s32;
    } else if (desired_bits < 64) {
        type = ctx->builtin_types->t_s64;
    } else {
        type = ctx->builtin_types->t_u64;
    }

    return append_instr_const_int(ctx, expr, type, val);
}

struct mir_instr *ast_expr_lit_float(struct context *ctx, struct ast *expr)
{
    if (expr->data.expr_float.overflow) {
        report_error(
            NUM_LIT_OVERFLOW, expr, "Float literal is too big and cannot be represented as f32.");
    }

    return append_instr_const_float(ctx, expr, expr->data.expr_float.val);
}

struct mir_instr *ast_expr_lit_double(struct context *ctx, struct ast *expr)
{
    if (expr->data.expr_double.overflow) {
        report_error(
            NUM_LIT_OVERFLOW, expr, "Double literal is too big and cannot be represented as f64.");
    }

    return append_instr_const_double(ctx, expr, expr->data.expr_double.val);
}

struct mir_instr *ast_expr_lit_bool(struct context *ctx, struct ast *expr)
{
    return append_instr_const_bool(ctx, expr, expr->data.expr_boolean.val);
}

struct mir_instr *ast_expr_lit_char(struct context *ctx, struct ast *expr)
{
    return append_instr_const_char(ctx, expr, (s8)expr->data.expr_character.val);
}

struct mir_instr *ast_expr_null(struct context *ctx, struct ast *nl)
{
    return append_instr_const_null(ctx, nl);
}

struct mir_instr *ast_expr_call(struct context *ctx, struct ast *call)
{
    struct ast  *ast_callee = call->data.expr_call.ref;
    ast_nodes_t *ast_args   = call->data.expr_call.args;
    bassert(ast_callee);

    if (ast_callee->kind == AST_REF &&
        is_builtin(ast_callee->data.ref.ident, BUILTIN_ID_ASSERT_FN)) {
        bool                   remove_assert = false;
        const bool             is_debug      = ctx->debug_mode;
        const enum assert_mode mode          = ctx->assembly->target->assert_mode;
        switch (mode) {
        case ASSERT_DEFAULT:
            remove_assert = !is_debug;
            break;
        case ASSERT_ALWAYS_ENABLED:
            remove_assert = false;
            break;
        case ASSERT_ALWAYS_DISABLED:
            remove_assert = true;
            break;
        }
        if (remove_assert) {
            return append_instr_const_void(ctx, call);
        }
    }

    mir_instrs_t *args = arena_safe_alloc(&ctx->assembly->arenas.sarr);

    // arguments need to be generated into reverse order due to bytecode call
    // conventions
    if (ast_args) {
        const s64 argc = sarrlenu(ast_args);
        sarrsetlen(args, argc);
        struct mir_instr *arg;
        struct ast       *ast_arg;
        for (usize i = argc; i-- > 0;) {
            ast_arg           = sarrpeek(ast_args, i);
            arg               = ast(ctx, ast_arg);
            sarrpeek(args, i) = arg;
        }
    }
    struct mir_instr *callee               = ast(ctx, ast_callee);
    const bool        call_in_compile_time = call->data.expr_call.call_in_compile_time;
    return append_instr_call(ctx, call, callee, args, call_in_compile_time);
}

struct mir_instr *ast_expr_elem(struct context *ctx, struct ast *elem)
{
    struct ast *ast_arr   = elem->data.expr_elem.next;
    struct ast *ast_index = elem->data.expr_elem.index;
    bassert(ast_arr && ast_index);

    struct mir_instr *arr_ptr = ast(ctx, ast_arr);
    struct mir_instr *index   = ast(ctx, ast_index);

    return append_instr_elem_ptr(ctx, elem, arr_ptr, index);
}

struct mir_instr *ast_expr_lit_fn(struct context      *ctx,
                                  struct ast          *lit_fn,
                                  struct ast          *decl_node,
                                  const char          *explicit_linkage_name, // optional
                                  bool                 is_global,
                                  u32                  flags,
                                  enum builtin_id_kind builtin_id)
{
    // creates function prototype
    struct ast *ast_block   = lit_fn->data.expr_fn.block;
    struct ast *ast_fn_type = lit_fn->data.expr_fn.type;
    struct id  *id          = decl_node ? &decl_node->data.ident.id : NULL;
    bassert(ast_fn_type->kind == AST_TYPE_FN);
    const bool is_polymorph =
        ast_fn_type->data.type_fn.is_polymorph && !ctx->polymorph.is_replacement_active;

    struct mir_instr_fn_proto *fn_proto =
        (struct mir_instr_fn_proto *)append_instr_fn_proto(ctx, lit_fn, NULL, NULL, true);

    // Generate type resolver for function type.
    fn_proto->type = CREATE_TYPE_RESOLVER_CALL(ast_fn_type);
    bassert(fn_proto->type);

    bassert(!(ctx->polymorph.is_replacement_active && sarrlenu(&ctx->polymorph.replacement_queue)));
    ctx->polymorph.is_replacement_active = false;

    // Prepare new function context. Must be in sync with pop at the end of scope!
    // DON'T CALL FINISH BEFORE THIS!!!
    ast_push_defer_stack(ctx);
    struct mir_instr_block *prev_block = ast_current_block(ctx);

    const char *linkage_name = explicit_linkage_name;

    // bassert(decl_node ? decl_node->kind == AST_IDENT : true);
    struct mir_fn *fn = create_fn(ctx,
                                  decl_node ? decl_node : lit_fn,
                                  id,
                                  linkage_name,
                                  (u32)flags,
                                  fn_proto,
                                  is_global,
                                  builtin_id);

    MIR_CEV_WRITE_AS(struct mir_fn *, &fn_proto->base.value, fn);

    // FUNCTION BODY
    // External or intrinsic function declaration has no body so we can skip body generation.
    if (isflag(flags, FLAG_EXTERN) || isflag(flags, FLAG_INTRINSIC)) {
        if (ast_block) {
            report_error(UNEXPECTED_FUNCTION_BODY,
                         ast_block,
                         "Unexpected body, for %s function.",
                         isflag(flags, FLAG_EXTERN) ? "external" : "intrinsic");
        }
        goto FINISH;
    }

    if (is_polymorph) {
        fn->poly = create_fn_poly_recipe(ctx, lit_fn);
        goto FINISH;
    }

    if (!ast_block) {
        report_error(EXPECTED_BODY, decl_node ? decl_node : lit_fn, "Missing function body.");
        goto FINISH;
    }

    // Set body scope for DI.
    fn->body_scope = ast_block->owner_scope;

    // create block for initialization locals and arguments
    struct mir_instr_block *init_block = append_block(ctx, fn, "entry");
    init_block->base.ref_count         = NO_REF_COUNTING;
    // Every user generated function must contain exit block; this block is invoked last
    // in every function eventually can return .ret value stored in temporary storage.
    // When ast parser hit user defined 'return' statement it sets up .ret temporary if
    // there is one and produce break into exit block. This approach is needed due to
    // defer statement, because we need to call defer blocks after return value
    // evaluation and before terminal instruction of the function. Last defer block
    // always breaks into the exit block.
    struct mir_instr_block *exit_block = append_block(ctx, fn, "exit");
    fn->exit_block =
        (struct mir_instr_block *)ref_instr(&exit_block->base); // Exit block is always referenced

    if (ast_fn_type->data.type_fn.ret_type) {
        set_current_block(ctx, init_block);
        fn->ret_tmp = append_instr_decl_var_impl(
            ctx, NULL, create_unique_name(ctx, IMPL_RET_TMP), NULL, NULL, true, false);
        set_current_block(ctx, exit_block);
        struct mir_instr *ret_init = append_instr_decl_direct_ref(ctx, ast_block, fn->ret_tmp);
        append_instr_ret(ctx, ast_block, ret_init);
    } else {
        set_current_block(ctx, exit_block);
        append_instr_ret(ctx, ast_block, NULL);
    }
    set_current_block(ctx, init_block);

    // build MIR for fn arguments
    ast_nodes_t *ast_args = ast_fn_type->data.type_fn.args;
    if (ast_args) {
        struct ast *ast_arg;
        struct ast *ast_arg_name;
        for (usize i = sarrlenu(ast_args); i-- > 0;) {
            ast_arg = sarrpeek(ast_args, i);
            bassert(ast_arg->kind == AST_DECL_ARG);
            ast_arg_name = ast_arg->data.decl.name;
            bassert(ast_arg_name);
            bassert(ast_arg_name->kind == AST_IDENT && "Expected identificator.");

            // create tmp declaration for arg variable
            struct mir_instr          *arg = append_instr_arg(ctx, NULL, (u32)i);
            struct mir_instr_decl_var *decl_var =
                (struct mir_instr_decl_var *)append_instr_decl_var(ctx,
                                                                   ast_arg_name,
                                                                   &ast_arg_name->data.ident.id,
                                                                   ast_arg_name->owner_scope,
                                                                   NULL,
                                                                   arg,
                                                                   true,
                                                                   0,
                                                                   BUILTIN_ID_NONE);

            decl_var->var->entry = register_symbol(
                ctx, ast_arg_name, &ast_arg_name->data.ident.id, ast_arg_name->owner_scope, false);
        }
    }

    if (isflag(flags, FLAG_TEST_FN)) {
        ++ctx->testing.expected_test_count;
    }

    // generate body instructions
    ast(ctx, ast_block);

FINISH:
    set_current_block(ctx, prev_block);
    ast_pop_defer_stack(ctx);
    return &fn_proto->base;
}

struct mir_instr *ast_expr_lit_fn_group(struct context *ctx, struct ast *group)
{
    ast_nodes_t *ast_variants = group->data.expr_fn_group.variants;
    bassert(ast_variants);
    bassert(sarrlenu(ast_variants));
    mir_instrs_t *variants = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    sarrsetlen(variants, sarrlenu(ast_variants));
    for (usize i = 0; i < sarrlenu(ast_variants); ++i) {
        struct ast       *it = sarrpeek(ast_variants, i);
        struct mir_instr *variant;
        if (it->kind == AST_EXPR_LIT_FN) {
            variant = ast_expr_lit_fn(ctx, it, NULL, NULL, true, 0, BUILTIN_ID_NONE);
        } else {
            variant = ast(ctx, it);
        }
        sarrpeek(variants, i) = variant;
    }
    return append_instr_fn_group(ctx, group, variants);
}

struct mir_instr *ast_expr_lit_string(struct context *ctx, struct ast *lit_string)
{
    const char *cstr = lit_string->data.expr_string.val;
    bassert(cstr);
    return append_instr_const_string(ctx, lit_string, cstr);
}

struct mir_instr *ast_expr_binop(struct context *ctx, struct ast *binop)
{
    struct ast *ast_lhs = binop->data.expr_binop.lhs;
    struct ast *ast_rhs = binop->data.expr_binop.rhs;
    bassert(ast_lhs && ast_rhs);

    const enum binop_kind op = binop->data.expr_binop.kind;

    switch (op) {
    case BINOP_ASSIGN: {
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        struct mir_instr *lhs = ast(ctx, ast_lhs);

        // In case right hand side expression is compound initializer, we don't need
        // temp storage for it, we can just copy compound content directly into
        // variable, so we set it here as non-naked.
        SET_IS_NAKED_IF_COMPOUND(rhs, false);
        return append_instr_store(ctx, binop, rhs, lhs);
    }

    // @CLEANUP Create helper function for these.
    case BINOP_ADD_ASSIGN: {
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        struct mir_instr *lhs = ast(ctx, ast_lhs);
        struct mir_instr *tmp = append_instr_binop(ctx, binop, lhs, rhs, BINOP_ADD);
        lhs                   = ast(ctx, ast_lhs);

        return append_instr_store(ctx, binop, tmp, lhs);
    }

    case BINOP_SUB_ASSIGN: {
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        struct mir_instr *lhs = ast(ctx, ast_lhs);
        struct mir_instr *tmp = append_instr_binop(ctx, binop, lhs, rhs, BINOP_SUB);
        lhs                   = ast(ctx, ast_lhs);

        return append_instr_store(ctx, binop, tmp, lhs);
    }

    case BINOP_MUL_ASSIGN: {
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        struct mir_instr *lhs = ast(ctx, ast_lhs);
        struct mir_instr *tmp = append_instr_binop(ctx, binop, lhs, rhs, BINOP_MUL);
        lhs                   = ast(ctx, ast_lhs);

        return append_instr_store(ctx, binop, tmp, lhs);
    }

    case BINOP_DIV_ASSIGN: {
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        struct mir_instr *lhs = ast(ctx, ast_lhs);
        struct mir_instr *tmp = append_instr_binop(ctx, binop, lhs, rhs, BINOP_DIV);
        lhs                   = ast(ctx, ast_lhs);

        return append_instr_store(ctx, binop, tmp, lhs);
    }

    case BINOP_MOD_ASSIGN: {
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        struct mir_instr *lhs = ast(ctx, ast_lhs);
        struct mir_instr *tmp = append_instr_binop(ctx, binop, lhs, rhs, BINOP_MOD);
        lhs                   = ast(ctx, ast_lhs);

        return append_instr_store(ctx, binop, tmp, lhs);
    }

    case BINOP_AND_ASSIGN: {
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        struct mir_instr *lhs = ast(ctx, ast_lhs);
        struct mir_instr *tmp = append_instr_binop(ctx, binop, lhs, rhs, BINOP_AND);
        lhs                   = ast(ctx, ast_lhs);

        return append_instr_store(ctx, binop, tmp, lhs);
    }

    case BINOP_OR_ASSIGN: {
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        struct mir_instr *lhs = ast(ctx, ast_lhs);
        struct mir_instr *tmp = append_instr_binop(ctx, binop, lhs, rhs, BINOP_OR);
        lhs                   = ast(ctx, ast_lhs);

        return append_instr_store(ctx, binop, tmp, lhs);
    }

    case BINOP_XOR_ASSIGN: {
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        struct mir_instr *lhs = ast(ctx, ast_lhs);
        struct mir_instr *tmp = append_instr_binop(ctx, binop, lhs, rhs, BINOP_XOR);
        lhs                   = ast(ctx, ast_lhs);

        return append_instr_store(ctx, binop, tmp, lhs);
    }

    case BINOP_LOGIC_AND:
    case BINOP_LOGIC_OR: {
        const bool              swap_condition   = op == BINOP_LOGIC_AND;
        struct mir_fn          *fn               = ast_current_fn(ctx);
        struct mir_instr_block *rhs_block        = append_block(ctx, fn, "rhs_block");
        struct mir_instr_block *end_block        = ctx->ast.current_phi_end_block;
        struct mir_instr_phi   *phi              = ctx->ast.current_phi;
        bool                    append_end_block = false;
        // If no end block is specified, we are on the top level of PHI expresion generation and
        // we must create one. Also PHI instruction must be crated (but not appended yet);
        // created PHI gather incomes from all nested branches created by expression.
        if (!end_block) {
            bassert(!phi);
            end_block                      = create_block(ctx, "end_block");
            phi                            = (struct mir_instr_phi *)create_instr_phi(ctx, binop);
            ctx->ast.current_phi_end_block = end_block;
            ctx->ast.current_phi           = phi;
            append_end_block               = true;
        }

        struct mir_instr *lhs = ast(ctx, ast_lhs);
        struct mir_instr *brk = NULL;
        if (swap_condition) {
            brk = append_instr_cond_br(ctx, NULL, lhs, rhs_block, end_block, false);
        } else {
            brk = append_instr_cond_br(ctx, NULL, lhs, end_block, rhs_block, false);
        }
        phi_add_income(phi, brk, ast_current_block(ctx));
        set_current_block(ctx, rhs_block);
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        if (append_end_block) {
            append_instr_br(ctx, NULL, end_block);
            phi_add_income(phi, rhs, ast_current_block(ctx));
            append_block2(ctx, fn, end_block);
            set_current_block(ctx, end_block);
            append_current_block(ctx, &phi->base);
            ctx->ast.current_phi_end_block = NULL;
            ctx->ast.current_phi           = NULL;
            return &phi->base;
        }
        return rhs;
    }

    default: {
        struct mir_instr *rhs = ast(ctx, ast_rhs);
        struct mir_instr *lhs = ast(ctx, ast_lhs);
        return append_instr_binop(ctx, binop, lhs, rhs, op);
    }
    }
}

struct mir_instr *ast_expr_unary(struct context *ctx, struct ast *unop)
{
    struct ast *ast_next = unop->data.expr_unary.next;
    bassert(ast_next);

    struct mir_instr *next = ast(ctx, ast_next);
    bassert(next);

    return append_instr_unop(ctx, unop, next, unop->data.expr_unary.kind);
}

struct mir_instr *ast_expr_type(struct context *ctx, struct ast *type)
{
    struct ast *next_type = type->data.expr_type.type;
    bassert(next_type);
    return ast(ctx, next_type);
}

static INLINE enum builtin_id_kind check_symbol_marked_compiler(struct context *ctx,
                                                                struct ast     *ident)
{
    // Check builtin ids for symbols marked as compiler.
    enum builtin_id_kind builtin_id = get_builtin_kind(ident);
    if (builtin_id == BUILTIN_ID_NONE) {
        report_error(
            UNKNOWN_SYMBOL, ident, "Symbol marked as #compiler is not known compiler internal.");
    }
    return builtin_id;
}

void report_poly(struct mir_instr *instr)
{
    if (!instr) return;
    struct mir_fn *owner_fn = instr_owner_fn(instr);
    if (!owner_fn) return;
    if (!owner_fn->first_poly_call_node) return;
    if (!owner_fn->first_poly_call_node->location) return;
    const char *poly = owner_fn->debug_poly_replacement;
    if (poly) {
        builder_msg(BUILDER_MSG_NOTE,
                    0,
                    owner_fn->decl_node->location,
                    BUILDER_CUR_WORD,
                    "In polymorph of function with substitution: %s",
                    poly);
    } else {
        builder_msg(BUILDER_MSG_NOTE,
                    0,
                    owner_fn->decl_node->location,
                    BUILDER_CUR_WORD,
                    "In polymorph of function.");
    }
    builder_msg(BUILDER_MSG_NOTE,
                0,
                owner_fn->first_poly_call_node->location,
                BUILDER_CUR_WORD,
                "First called here:");
}

// Helper for function declaration generation.
static void ast_decl_fn(struct context *ctx, struct ast *ast_fn)
{
    struct ast *ast_name  = ast_fn->data.decl.name;
    struct ast *ast_type  = ast_fn->data.decl.type;
    struct ast *ast_value = ast_fn->data.decl_entity.value;

    // recognized named function declaration
    struct ast *ast_explicit_linkage_name = ast_fn->data.decl_entity.explicit_linkage_name;
    const s32   flags                     = ast_fn->data.decl_entity.flags;
    const bool  is_mutable                = ast_fn->data.decl_entity.mut;
    const bool  generate_entry            = ctx->assembly->target->kind == ASSEMBLY_EXECUTABLE;
    if (!generate_entry && isflag(flags, FLAG_ENTRY)) {
        // Generate entry function only in case we are compiling executable binary, otherwise
        // it's not needed, and main should be also optional.
        return;
    }
    if (is_mutable) {
        report_error(
            INVALID_MUTABILITY, ast_name, "Function declaration is expected to be immutable.");
    }

    const bool is_multidecl = ast_name->data.ident.next;
    if (is_multidecl) {
        const struct ast *ast_next_name = ast_name->data.ident.next;
        report_error(INVALID_NAME, ast_next_name, "Function cannot be multi-declared.");
    }

    enum builtin_id_kind builtin_id  = BUILTIN_ID_NONE;
    const bool           is_compiler = isflag(ast_fn->data.decl_entity.flags, FLAG_COMPILER);
    if (is_compiler) builtin_id = check_symbol_marked_compiler(ctx, ast_name);
    const bool  is_global = ast_fn->data.decl_entity.is_global;
    const char *optional_explicit_linkage_name =
        ast_explicit_linkage_name ? ast_explicit_linkage_name->data.ident.id.str : NULL;
    struct mir_instr *value = ast_expr_lit_fn(
        ctx, ast_value, ast_name, optional_explicit_linkage_name, is_global, flags, builtin_id);
    bassert(value);

    if (ast_type)
        ((struct mir_instr_fn_proto *)value)->user_type = CREATE_TYPE_RESOLVER_CALL(ast_type);

    bassert(value);
    struct mir_fn *fn = MIR_CEV_READ_AS(struct mir_fn *, &value->value);
    BL_MAGIC_ASSERT(fn);

    // check main
    if (is_builtin(ast_name, BUILTIN_ID_MAIN)) {
        // This is reported as an error.
        ctx->assembly->vm_run.entry = fn;
        if (scope_is_subtree_of_kind(ast_name->owner_scope, SCOPE_PRIVATE)) {
            report_error(UNEXPECTED_DIRECTIVE,
                         ast_name,
                         "Main function cannot be declared in private scope.");
        }
    }

    if (isflag(flags, FLAG_EXPORT) &&
        scope_is_subtree_of_kind(ast_name->owner_scope, SCOPE_PRIVATE)) {
        report_error(UNEXPECTED_DIRECTIVE,
                     ast_name,
                     "Exported function cannot be declared in private scope.");
    }

    struct id    *id    = &ast_name->data.ident.id;
    struct scope *scope = ast_name->owner_scope;
    fn->scope_entry     = register_symbol(ctx, ast_name, id, scope, is_compiler);
}

// Helper for local variable declaration generation.
static void ast_decl_var_local(struct context *ctx, struct ast *ast_local)
{
    struct ast *ast_name  = ast_local->data.decl.name;
    struct ast *ast_type  = ast_local->data.decl.type;
    struct ast *ast_value = ast_local->data.decl_entity.value;
    // Create type resolver if there is explicitly defined type mentioned by user in
    // declaration.
    struct mir_instr *type        = ast_type ? CREATE_TYPE_RESOLVER_CALL(ast_type) : NULL;
    struct mir_instr *value       = ast(ctx, ast_value);
    const bool        is_compiler = isflag(ast_local->data.decl_entity.flags, FLAG_COMPILER);
    const bool        is_unroll   = ast_value && ast_value->kind == AST_EXPR_CALL;
    const bool        is_mutable  = ast_local->data.decl_entity.mut;

    struct scope *scope = ast_name->owner_scope;

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
    struct ast       *ast_current_name = ast_name;
    struct mir_instr *current_value    = value;
    struct mir_instr *prev_var         = NULL;
    s32               index            = 0;
    while (ast_current_name) {
        enum builtin_id_kind builtin_id = BUILTIN_ID_NONE;
        if (is_compiler) builtin_id = check_symbol_marked_compiler(ctx, ast_current_name);
        if (is_unroll) {
            current_value = append_instr_unroll(ctx, ast_current_name, value, prev_var, index++);
        } else if (prev_var) {
            current_value = append_instr_decl_direct_ref(ctx, NULL, prev_var);
        }
        struct id        *id  = &ast_current_name->data.ident.id;
        struct mir_instr *var = append_instr_decl_var(ctx,
                                                      ast_current_name,
                                                      id,
                                                      scope,
                                                      type,
                                                      current_value,
                                                      is_mutable,
                                                      ast_local->data.decl_entity.flags,
                                                      builtin_id);
        ((struct mir_instr_decl_var *)var)->var->entry =
            register_symbol(ctx, ast_current_name, id, scope, is_compiler);

        bassert(ast_current_name->kind == AST_IDENT);
        ast_current_name = ast_current_name->data.ident.next;
        prev_var         = var;
    }
}

static void ast_decl_var_global_or_struct(struct context *ctx, struct ast *ast_global)
{
    struct ast *ast_name  = ast_global->data.decl.name;
    struct ast *ast_type  = ast_global->data.decl.type;
    struct ast *ast_value = ast_global->data.decl_entity.value;
    // Create type resolver if there is explicitly defined type mentioned by user in
    // declaration.
    struct mir_instr *type           = ast_type ? CREATE_TYPE_RESOLVER_CALL(ast_type) : NULL;
    const bool        is_struct_decl = ast_value && ast_value->kind == AST_EXPR_TYPE &&
                                ast_value->data.expr_type.type->kind == AST_TYPE_STRUCT;
    const bool is_mutable  = ast_global->data.decl_entity.mut;
    const bool is_compiler = isflag(ast_global->data.decl_entity.flags, FLAG_COMPILER);

    struct mir_instr *value = NULL;
    struct scope     *scope = ast_name->owner_scope;

    // Struct use forward type declarations!
    if (is_struct_decl) {
        const bool is_multidecl = ast_name->data.ident.next;
        if (is_multidecl) {
            const struct ast *ast_next_name = ast_name->data.ident.next;
            report_error(INVALID_NAME, ast_next_name, " cannot be multi-declared.");
        }
        // Set to const type fwd decl
        struct mir_type *fwd_decl_type =
            create_type_struct_incomplete(ctx, ctx->ast.current_entity_id, false);

        value = create_instr_const_type(ctx, ast_value, fwd_decl_type);
        ANALYZE_INSTR_RQ(value);

        // Set current fwd decl
        ctx->ast.current_fwd_struct_decl = value;
    }

    mir_instrs_t *decls            = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    struct ast   *ast_current_name = ast_name;
    while (ast_current_name) {
        enum builtin_id_kind builtin_id = BUILTIN_ID_NONE;
        if (is_compiler) builtin_id = check_symbol_marked_compiler(ctx, ast_name);
        struct id        *id   = &ast_current_name->data.ident.id;
        struct mir_instr *decl = append_instr_decl_var(ctx,
                                                       ast_current_name,
                                                       id,
                                                       scope,
                                                       type,
                                                       value,
                                                       is_mutable,
                                                       ast_global->data.decl_entity.flags,
                                                       builtin_id);
        sarrput(decls, decl);

        struct mir_var *var    = ((struct mir_instr_decl_var *)decl)->var;
        var->is_struct_typedef = is_struct_decl;
        var->entry             = register_symbol(ctx, ast_current_name, id, scope, is_compiler);
        ast_current_name       = ast_current_name->data.ident.next;
    }

    // For globals we must generate initialization after variable declaration,
    // SetInitializer instruction will be used to set actual value, also
    // implicit initialization block is created into MIR (such block does not
    // have LLVM representation -> globals must be evaluated in compile time).
    if (ast_value) {
        // Generate implicit global initializer block.
        ast_create_global_initializer2(ctx, ast_value, decls);
    } else {
        // Global has no explicit initialization in code so we must
        // create default global initializer.
        ast_create_global_initializer2(ctx, NULL, decls);
    }

    // Struct decl cleanup.
    if (is_struct_decl) {
        ctx->ast.current_fwd_struct_decl = NULL;
    }
}

struct mir_instr *ast_decl_entity(struct context *ctx, struct ast *entity)
{
    struct ast *ast_name       = entity->data.decl.name;
    struct ast *ast_value      = entity->data.decl_entity.value;
    const bool  is_fn_decl     = ast_value && ast_value->kind == AST_EXPR_LIT_FN;
    const bool  is_global      = entity->data.decl_entity.is_global;
    const bool  is_struct_decl = ast_value && ast_value->kind == AST_EXPR_TYPE &&
                                ast_value->data.expr_type.type->kind == AST_TYPE_STRUCT;
    bassert(ast_name && "Missing entity name.");
    bassert(ast_name->kind == AST_IDENT && "Expected identificator.");
    if (is_fn_decl) {
        ast_decl_fn(ctx, entity);
    } else {
        ctx->ast.current_entity_id = &ast_name->data.ident.id;
        if (is_global || is_struct_decl) {
            ast_decl_var_global_or_struct(ctx, entity);
        } else {
            ast_decl_var_local(ctx, entity);
        }
        ctx->ast.current_entity_id = NULL;
    }
    return NULL;
}

struct mir_instr *ast_decl_arg(struct context *ctx, struct ast *arg)
{
    struct ast       *ast_value = arg->data.decl_arg.value;
    struct ast       *ast_name  = arg->data.decl.name;
    struct ast       *ast_type  = arg->data.decl.type;
    struct mir_instr *value     = NULL;
    // Type is resolved in type resolver in case we have default value defined.
    struct mir_instr *type = NULL;

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
            value = ast(ctx, ast_value);
        } else {
            value = append_instr_decl_var_impl(
                ctx, ast_name, IMPL_ARG_DEFAULT, type, NULL, false, true);
            ast_create_global_initializer(ctx, ast_value, value);
        }
    } else {
        bassert(ast_type && "Function argument must have explicit type when no default "
                            "value is specified!");
        type = CREATE_TYPE_RESOLVER_CALL(ast_type);
    }
    return append_instr_decl_arg(ctx, ast_name, type, value);
}

struct mir_instr *ast_decl_member(struct context *ctx, struct ast *arg)
{
    struct ast   *ast_type = arg->data.decl.type;
    struct ast   *ast_name = arg->data.decl.name;
    struct ast   *ast_tags = arg->data.decl.tags;
    mir_instrs_t *tags     = NULL;
    bassert(ast_name);
    bassert(ast_type);

    // has member user defined tags?
    if (ast_tags) {
        ast_nodes_t *ast_values = ast_tags->data.tags.values;
        bassert(ast_values && "Invalid tag values array.");
        bassert(sarrlenu(ast_values) && "Tag array must contains one value at least.");
        tags = arena_safe_alloc(&ctx->assembly->arenas.sarr);
        for (usize i = 0; i < sarrlenu(ast_values); ++i) {
            struct ast       *ast_value = sarrpeek(ast_values, i);
            struct mir_instr *value     = ast(ctx, ast_value);
            sarrput(tags, value);
        }
    }
    struct mir_instr *result = ast(ctx, ast_type);
    bassert(ast_name->kind == AST_IDENT);
    result = append_instr_decl_member(ctx, ast_name, result, tags);
    ((struct mir_instr_decl_member *)result)->member->entry =
        register_symbol(ctx, ast_name, &ast_name->data.ident.id, ast_name->owner_scope, false);
    bassert(result);
    return result;
}

struct mir_instr *ast_decl_variant(struct context     *ctx,
                                   struct ast         *variant,
                                   struct mir_instr   *base_type,
                                   struct mir_variant *prev_variant,
                                   const bool          is_flags)
{
    struct ast *ast_name  = variant->data.decl.name;
    struct ast *ast_value = variant->data.decl_variant.value;
    bassert(ast_name && "Missing enum variant name!");
    struct mir_instr              *value = ast(ctx, ast_value);
    struct mir_instr_decl_variant *variant_instr =
        (struct mir_instr_decl_variant *)append_instr_decl_variant(
            ctx, ast_name, value, base_type, prev_variant, is_flags);
    variant_instr->variant->entry =
        register_symbol(ctx, ast_name, &ast_name->data.ident.id, ast_name->owner_scope, false);
    return (struct mir_instr *)variant_instr;
}

struct mir_instr *ast_ref(struct context *ctx, struct ast *ref)
{
    struct ast *ident = ref->data.ref.ident;
    struct ast *next  = ref->data.ref.next;
    bassert(ident);
    struct scope *scope       = ident->owner_scope;
    const s32     layer_index = ctx->polymorph.current_scope_layer_index;
    struct unit  *unit        = ident->location->unit;
    bassert(unit);
    bassert(scope);
    if (next) {
        struct mir_instr *target = ast(ctx, next);
        return append_instr_member_ptr(ctx, ref, target, ident, NULL, BUILTIN_ID_NONE);
    }
    return append_instr_decl_ref(ctx, ref, unit, &ident->data.ident.id, scope, layer_index, NULL);
}

struct mir_instr *ast_type_fn(struct context *ctx, struct ast *type_fn)
{
    bassert(type_fn->kind == AST_TYPE_FN);
    struct ast  *ast_ret_type  = type_fn->data.type_fn.ret_type;
    ast_nodes_t *ast_arg_types = type_fn->data.type_fn.args;
    const bool   is_polymorph =
        type_fn->data.type_fn.is_polymorph && !ctx->polymorph.is_replacement_active;
    // Discard current entity ID to fix bug when multi-return structure takes this name as an
    // alias. There should be probably better way to solve this issue, but lets keep this for
    // now.
    ctx->ast.current_entity_id = NULL;
    // return type
    struct mir_instr *ret_type = NULL;
    if (ast_ret_type) {
        ret_type = ast(ctx, ast_ret_type);
        ref_instr(ret_type);
    }
    mir_instrs_t *args = NULL;
    if (sarrlenu(ast_arg_types) > 0) {
        const usize c = sarrlenu(ast_arg_types);
        args          = arena_safe_alloc(&ctx->assembly->arenas.sarr);
        sarrsetlen(args, c);

        struct ast       *ast_arg_type;
        struct mir_instr *arg;
        for (usize i = c; i-- > 0;) {
            ast_arg_type = sarrpeek(ast_arg_types, i);
            arg          = ast(ctx, ast_arg_type);
            ref_instr(arg);
            sarrpeek(args, i) = arg;
        }
    }
    return append_instr_type_fn(ctx, type_fn, ret_type, args, is_polymorph);
}

struct mir_instr *ast_type_fn_group(struct context *ctx, struct ast *group)
{
    ast_nodes_t *ast_variants = group->data.type_fn_group.variants;
    bassert(ast_variants);
    mir_instrs_t *variants = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    sarrsetlen(variants, sarrlenu(ast_variants));

    for (usize i = 0; i < sarrlenu(ast_variants); ++i) {
        struct ast *it        = sarrpeek(ast_variants, i);
        sarrpeek(variants, i) = ast(ctx, it);
    }
    // Consume declaration identificator.
    struct id *id              = ctx->ast.current_entity_id;
    ctx->ast.current_entity_id = NULL;
    return append_instr_type_fn_group(ctx, group, id, variants);
}

struct mir_instr *ast_type_arr(struct context *ctx, struct ast *type_arr)
{
    struct id *id              = ctx->ast.current_entity_id;
    ctx->ast.current_entity_id = NULL;

    struct ast *ast_elem_type = type_arr->data.type_arr.elem_type;
    struct ast *ast_len       = type_arr->data.type_arr.len;
    bassert(ast_elem_type && ast_len);

    struct mir_instr *len       = ast(ctx, ast_len);
    struct mir_instr *elem_type = ast(ctx, ast_elem_type);
    return append_instr_type_array(ctx, type_arr, id, elem_type, len);
}

struct mir_instr *ast_type_slice(struct context *ctx, struct ast *type_slice)
{
    struct ast *ast_elem_type = type_slice->data.type_slice.elem_type;
    bassert(ast_elem_type);

    struct mir_instr *elem_type = ast(ctx, ast_elem_type);
    return append_instr_type_slice(ctx, type_slice, elem_type);
}

struct mir_instr *ast_type_dynarr(struct context *ctx, struct ast *type_dynarr)
{
    struct ast *ast_elem_type = type_dynarr->data.type_dynarr.elem_type;
    bassert(ast_elem_type);

    struct mir_instr *elem_type = ast(ctx, ast_elem_type);
    return append_instr_type_dynarr(ctx, type_dynarr, elem_type);
}

struct mir_instr *ast_type_ptr(struct context *ctx, struct ast *type_ptr)
{
    struct ast *ast_type = type_ptr->data.type_ptr.type;
    bassert(ast_type && "invalid pointee type");
    struct mir_instr *type = ast(ctx, ast_type);
    bassert(type);

    if (type->kind == MIR_INSTR_DECL_REF) {
        // Enable incomplete types for pointers to declarations.
        ((struct mir_instr_decl_ref *)type)->accept_incomplete_type = true;
    }

    return append_instr_type_ptr(ctx, type_ptr, type);
}

struct mir_instr *ast_type_vargs(struct context *ctx, struct ast *type_vargs)
{
    // type is optional (Any will be used when no type was specified)
    struct ast       *ast_type = type_vargs->data.type_vargs.type;
    struct mir_instr *type     = ast(ctx, ast_type);
    return append_instr_type_vargs(ctx, type_vargs, type);
}

struct mir_instr *ast_type_enum(struct context *ctx, struct ast *type_enum)
{
    ast_nodes_t *ast_variants  = type_enum->data.type_enm.variants;
    struct ast  *ast_base_type = type_enum->data.type_enm.type;
    const bool   is_flags      = type_enum->data.type_enm.is_flags;
    bassert(ast_variants);

    if (!sarrlenu(ast_variants)) {
        report_error(EMPTY_ENUM, type_enum, "Empty enumerator.");
        return NULL;
    }

    // @Incomplete: Enum should probably use type resoler as well?
    struct mir_instr *base_type = ast(ctx, ast_base_type);

    struct scope *scope    = type_enum->data.type_enm.scope;
    mir_instrs_t *variants = arena_safe_alloc(&ctx->assembly->arenas.sarr);

    // Build variant instructions
    struct mir_instr   *variant;
    struct mir_variant *prev_variant = NULL;
    for (usize i = 0; i < sarrlenu(ast_variants); ++i) {
        struct ast *ast_variant = sarrpeek(ast_variants, i);
        bassert(ast_variant->kind == AST_DECL_VARIANT);
        variant = ast_decl_variant(ctx, ast_variant, base_type, prev_variant, is_flags);
        bassert(variant);
        sarrput(variants, variant);
        prev_variant = ((struct mir_instr_decl_variant *)variant)->variant;
    }
    // Consume declaration identificator.
    struct id *id              = ctx->ast.current_entity_id;
    ctx->ast.current_entity_id = NULL;
    return append_instr_type_enum(ctx, type_enum, id, scope, variants, base_type, is_flags);
}

struct mir_instr *ast_type_struct(struct context *ctx, struct ast *type_struct)
{
    // Consume declaration identificator.
    struct id *id              = ctx->ast.current_entity_id;
    ctx->ast.current_entity_id = NULL;

    // Consume current struct fwd decl.
    struct mir_instr *fwd_decl       = ctx->ast.current_fwd_struct_decl;
    ctx->ast.current_fwd_struct_decl = NULL;

    ast_nodes_t *ast_members             = type_struct->data.type_strct.members;
    const bool   is_union                = type_struct->data.type_strct.is_union;
    const bool   is_multiple_return_type = type_struct->data.type_strct.is_multiple_return_type;
    bassert(ast_members);
    struct ast *ast_base_type = type_struct->data.type_strct.base_type;
    const usize memc          = sarrlenu(ast_members);
    if (!memc && !ast_base_type) {
        report_error(EMPTY_STRUCT, type_struct, "Empty structure.");
        return NULL;
    }

    mir_instrs_t *members = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    struct scope *scope   = type_struct->data.type_strct.scope;
    bassert(scope);

    if (ast_base_type) {
        // Structure has base type, in such case we generate implicit first member
        // 'base'.
        struct mir_instr *base_type = ast(ctx, ast_base_type);
        struct id        *id2       = &builtin_ids[BUILTIN_ID_STRUCT_BASE];
        base_type = append_instr_decl_member_impl(ctx, ast_base_type, id2, base_type, NULL);

        struct mir_member *base_member = ((struct mir_instr_decl_member *)base_type)->member;
        base_member->is_base           = true;
        provide_builtin_member(ctx, scope, base_member);

        sarrput(members, base_type);
    }

    struct mir_instr *tmp = NULL;
    for (usize i = 0; i < sarrlenu(ast_members); ++i) {
        struct ast *ast_member = sarrpeek(ast_members, i);
        tmp                    = ast(ctx, ast_member);
        bassert(tmp);
        sarrput(members, tmp);
    }

    return append_instr_type_struct(
        ctx, type_struct, id, fwd_decl, scope, members, false, is_union, is_multiple_return_type);
}

struct mir_instr *ast_type_poly(struct context *ctx, struct ast *poly)
{
    struct ast   *ast_ident = poly->data.type_poly.ident;
    struct scope *scope     = poly->owner_scope;
    bassert(ast_ident);

    mir_types_t        *queue       = &ctx->polymorph.replacement_queue;
    struct id          *T_id        = &ast_ident->data.ident.id;
    struct scope_entry *scope_entry = register_symbol(ctx, ast_ident, T_id, scope, false);
    if (!scope_entry) goto USE_DUMMY;
    if (ctx->polymorph.is_replacement_active) {
        if (sarrlen(queue) == 0) {
            // Use s32 as dummy when polymorph replacement fails.
            goto USE_DUMMY;
        } else {
            // We are generating function specification -> we have replacement for polymorph
            // types.
            // Notice that pop takes the last element from the queue, this is possible due to
            // reverse order of generated argument instructions.
            struct mir_type *replacement_type = sarrpop(queue);
            BL_MAGIC_ASSERT(replacement_type);

            scope_entry->kind      = SCOPE_ENTRY_TYPE;
            scope_entry->data.type = replacement_type;
            return append_instr_const_type(ctx, poly, replacement_type);
        }
    }

    // We directly create polymorphic type here, because we want all references to T available
    // anywhere inside function argument list declaration; even before ?T is appears in
    // declaration. So polymorphic type is completed right after this function ends and no
    // additional analyze is needed.
    struct mir_type *master_type = create_type_poly(ctx, scope_entry->id, true);
    struct mir_type *slave_type  = create_type_poly(ctx, scope_entry->id, false);
    scope_entry->kind            = SCOPE_ENTRY_TYPE;
    scope_entry->data.type       = slave_type;

    struct mir_instr *instr_poly = append_instr_type_poly(ctx, poly, scope_entry->id);
    MIR_CEV_WRITE_AS(struct mir_type *, &instr_poly->value, master_type);
    return instr_poly;
USE_DUMMY:
    sarrclear(queue);
    return append_instr_const_type(ctx, poly, ctx->builtin_types->t_s32);
}

struct mir_instr *
ast_create_global_initializer2(struct context *ctx, struct ast *ast_value, mir_instrs_t *decls)
{
    struct mir_instr_block *prev_block = ast_current_block(ctx);
    struct mir_instr_block *block      = append_global_block(ctx, INIT_VALUE_FN_NAME);
    set_current_block(ctx, block);
    struct mir_instr *result = ast(ctx, ast_value);

    if (ast_value)
        result = append_instr_set_initializer(ctx, ast_value, decls, result);
    else
        result = append_instr_set_initializer_impl(ctx, decls, result);

    set_current_block(ctx, prev_block);
    return result;
}

struct mir_instr *
ast_create_global_initializer(struct context *ctx, struct ast *ast_value, struct mir_instr *decl)
{
    bassert(decl);
    mir_instrs_t *decls = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    sarrput(decls, decl);
    return ast_create_global_initializer2(ctx, ast_value, decls);
}

struct mir_instr *ast_create_impl_fn_call(struct context  *ctx,
                                          struct ast      *node,
                                          const char      *fn_name,
                                          struct mir_type *fn_type,
                                          bool             schedule_analyze)
{
    if (!node) return NULL;

    struct mir_instr_block *prev_block = ast_current_block(ctx);
    struct mir_instr *fn_proto = append_instr_fn_proto(ctx, NULL, NULL, NULL, schedule_analyze);
    fn_proto->value.type       = fn_type;

    struct mir_fn *fn = create_fn(
        ctx, NULL, NULL, fn_name, 0, (struct mir_instr_fn_proto *)fn_proto, true, BUILTIN_ID_NONE);
    MIR_CEV_WRITE_AS(struct mir_fn *, &fn_proto->value, fn);
    fn->type                      = fn_type;
    struct mir_instr_block *entry = append_block(ctx, fn, "entry");
    entry->base.ref_count         = NO_REF_COUNTING;
    set_current_block(ctx, entry);
    struct mir_instr *result = ast(ctx, node);
    append_instr_ret(ctx, NULL, result);
    set_current_block(ctx, prev_block);
    return create_instr_call_comptime(ctx, node, fn_proto);
}

struct mir_instr *ast(struct context *ctx, struct ast *node)
{
    if (!node) return NULL;
    switch (node->kind) {
    case AST_UBLOCK:
        ast_ublock(ctx, node);
        break;
    case AST_BLOCK:
        ast_block(ctx, node);
        break;
    case AST_UNREACHABLE:
        ast_unreachable(ctx, node);
        break;
    case AST_DEBUGBREAK:
        ast_debugbreak(ctx, node);
        break;
    case AST_STMT_DEFER:
        ast_stmt_defer(ctx, node);
        break;
    case AST_STMT_RETURN:
        ast_stmt_return(ctx, node);
        break;
    case AST_STMT_LOOP:
        ast_stmt_loop(ctx, node);
        break;
    case AST_STMT_BREAK:
        ast_stmt_break(ctx, node);
        break;
    case AST_STMT_CONTINUE:
        ast_stmt_continue(ctx, node);
        break;
    case AST_STMT_IF:
        ast_stmt_if(ctx, node);
        break;
    case AST_STMT_SWITCH:
        ast_stmt_switch(ctx, node);
        break;
    case AST_DECL_ENTITY:
        return ast_decl_entity(ctx, node);
    case AST_DECL_ARG:
        return ast_decl_arg(ctx, node);
    case AST_DECL_MEMBER:
        return ast_decl_member(ctx, node);
    case AST_DECL_VARIANT:
        BL_UNIMPLEMENTED;
    case AST_REF:
        return ast_ref(ctx, node);
    case AST_TYPE_STRUCT:
        return ast_type_struct(ctx, node);
    case AST_TYPE_FN:
        return ast_type_fn(ctx, node);
    case AST_TYPE_FN_GROUP:
        return ast_type_fn_group(ctx, node);
    case AST_TYPE_ARR:
        return ast_type_arr(ctx, node);
    case AST_TYPE_SLICE:
        return ast_type_slice(ctx, node);
    case AST_TYPE_DYNARR:
        return ast_type_dynarr(ctx, node);
    case AST_TYPE_PTR:
        return ast_type_ptr(ctx, node);
    case AST_TYPE_VARGS:
        return ast_type_vargs(ctx, node);
    case AST_TYPE_ENUM:
        return ast_type_enum(ctx, node);
    case AST_TYPE_POLY:
        return ast_type_poly(ctx, node);
    case AST_EXPR_ADDROF:
        return ast_expr_addrof(ctx, node);
    case AST_EXPR_CAST:
        return ast_expr_cast(ctx, node);
    case AST_EXPR_SIZEOF:
        return ast_expr_sizeof(ctx, node);
    case AST_EXPR_ALIGNOF:
        return ast_expr_alignof(ctx, node);
    case AST_EXPR_DEREF:
        return ast_expr_deref(ctx, node);
    case AST_EXPR_LIT_INT:
        return ast_expr_lit_int(ctx, node);
    case AST_EXPR_LIT_FLOAT:
        return ast_expr_lit_float(ctx, node);
    case AST_EXPR_LIT_DOUBLE:
        return ast_expr_lit_double(ctx, node);
    case AST_EXPR_LIT_BOOL:
        return ast_expr_lit_bool(ctx, node);
    case AST_EXPR_LIT_FN:
        return ast_expr_lit_fn(ctx, node, NULL, NULL, false, 0, BUILTIN_ID_NONE);
    case AST_EXPR_LIT_FN_GROUP:
        return ast_expr_lit_fn_group(ctx, node);
    case AST_EXPR_LIT_STRING:
        return ast_expr_lit_string(ctx, node);
    case AST_EXPR_LIT_CHAR:
        return ast_expr_lit_char(ctx, node);
    case AST_EXPR_BINOP:
        return ast_expr_binop(ctx, node);
    case AST_EXPR_UNARY:
        return ast_expr_unary(ctx, node);
    case AST_EXPR_CALL:
        return ast_expr_call(ctx, node);
    case AST_EXPR_ELEM:
        return ast_expr_elem(ctx, node);
    case AST_EXPR_NULL:
        return ast_expr_null(ctx, node);
    case AST_EXPR_TYPE:
        return ast_expr_type(ctx, node);
    case AST_EXPR_COMPOUND:
        return ast_expr_compound(ctx, node);
    case AST_EXPR_TYPE_INFO:
        return ast_expr_type_info(ctx, node);
    case AST_EXPR_TEST_CASES:
        return ast_expr_test_cases(ctx, node);
    case AST_CALL_LOC:
        return ast_call_loc(ctx, node);
    case AST_MSG:
        return ast_msg(ctx, node);

    case AST_LOAD:
    case AST_IMPORT:
    case AST_LINK:
    case AST_PRIVATE:
    case AST_SCOPE:
        break;
    default:
        babort("invalid node %s", ast_get_name(node));
    }

    return NULL;
}

const char *mir_instr_name(const struct mir_instr *instr)
{
    if (!instr) return "unknown";
    switch (instr->kind) {
    case MIR_INSTR_INVALID:
        return "InstrInvalid";
    case MIR_INSTR_DEBUGBREAK:
        return "InstrDebugBreak";
    case MIR_INSTR_MSG:
        return "InstrMsg";
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
        const struct mir_instr_type_struct *is = (struct mir_instr_type_struct *)instr;
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
    case MIR_INSTR_TYPE_POLY:
        return "InstrTypePoly";
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

// =================================================================================================
// public
// =================================================================================================

struct id builtin_ids[_BUILTIN_ID_COUNT] = {
#define GEN_BUILTIN_IDS
#include "builtin.inc"
#undef GEN_BUILTIN_IDS
};

const char *mir_get_fn_readable_name(struct mir_fn *fn)
{
    bassert(fn);
    if (fn->id) {
        return fn->id->str;
    }
    return fn->linkage_name;
}

struct mir_fn *mir_get_callee(const struct mir_instr_call *call)
{
    struct mir_const_expr_value *val = &call->callee->value;
    bassert(val->type && val->type->kind == MIR_TYPE_FN);
    struct mir_fn *fn = MIR_CEV_READ_AS(struct mir_fn *, val);
    BL_MAGIC_ASSERT(fn);
    return fn;
}

static void _type_to_str(char *buf, usize len, const struct mir_type *type, bool prefer_name)
{
#define append_buf(buf, len, str)                                                                  \
    {                                                                                              \
        const usize filled = strlen(buf);                                                          \
        snprintf((buf) + filled, (len)-filled, "%s", str);                                         \
    }

    if (!buf) return;
    if (!type) {
        append_buf(buf, len, "<unknown>");
        return;
    }

    if (type->user_id && prefer_name) {
        bassert(type->user_id->str);
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
            struct mir_type *tmp = mir_get_struct_elem_type(type, MIR_SLICE_PTR_INDEX);
            tmp                  = mir_deref_type(tmp);
            _type_to_str(buf, len, tmp, true);
        }
        break;
    }

    case MIR_TYPE_DYNARR: {
        const bool has_members = type->data.strct.members;
        append_buf(buf, len, "[..]");

        if (has_members) {
            struct mir_type *tmp = mir_get_struct_elem_type(type, MIR_DYNARR_PTR_INDEX);
            tmp                  = mir_deref_type(tmp);
            _type_to_str(buf, len, tmp, true);
        }
        break;
    }

    case MIR_TYPE_VARGS: {
        const bool has_members = type->data.strct.members;
        append_buf(buf, len, "...");

        if (has_members) {
            struct mir_type *tmp = mir_get_struct_elem_type(type, MIR_SLICE_PTR_INDEX);
            tmp                  = mir_deref_type(tmp);
            _type_to_str(buf, len, tmp, true);
        }
        break;
    }

    case MIR_TYPE_STRUCT: {
        mir_members_t *members = type->data.strct.members;
        if (type->data.strct.is_union) {
            append_buf(buf, len, "union{");
        } else {
            append_buf(buf, len, "struct{");
        }
        for (usize i = 0; i < sarrlenu(members); ++i) {
            struct mir_member *member = sarrpeek(members, i);
            _type_to_str(buf, len, member->type, true);
            if (i < sarrlenu(members) - 1) append_buf(buf, len, ", ");
        }
        append_buf(buf, len, "}");
        break;
    }

    case MIR_TYPE_ENUM: {
        mir_variants_t *variants = type->data.enm.variants;
        append_buf(buf, len, "enum{");
        for (usize i = 0; i < sarrlenu(variants); ++i) {
            struct mir_variant *variant = sarrpeek(variants, i);
            append_buf(buf, len, variant->id->str);
            append_buf(buf, len, " :: ");
            char value_str[35];
            snprintf(value_str, static_arrlenu(value_str), "%lld", variant->value);
            append_buf(buf, len, value_str);
            if (i < sarrlenu(variants) - 1) append_buf(buf, len, ", ");
        }
        append_buf(buf, len, "}");
        break;
    }

    case MIR_TYPE_FN: {
        append_buf(buf, len, "fn(");
        mir_args_t *args = type->data.fn.args;
        for (usize i = 0; i < sarrlenu(args); ++i) {
            struct mir_arg *arg = sarrpeek(args, i);
            _type_to_str(buf, len, arg->type, true);
            if (i < sarrlenu(args) - 1) append_buf(buf, len, ", ");
        }
        append_buf(buf, len, ") ");
        _type_to_str(buf, len, type->data.fn.ret_type, true);
        break;
    }

    case MIR_TYPE_FN_GROUP: {
        append_buf(buf, len, "fn{");
        mir_types_t *variants = type->data.fn_group.variants;
        for (usize i = 0; i < sarrlenu(variants); ++i) {
            struct mir_type *it = sarrpeek(variants, i);
            _type_to_str(buf, len, it, true);
            if (i < sarrlenu(variants) - 1) append_buf(buf, len, "; ");
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
vm_stack_ptr_t _mir_cev_read(struct mir_const_expr_value *value)
{
    bassert(value && "Attempt to read null value!");
    bassert(value->is_comptime && "Attempt to read non-comptime value!");
    bassert(value->data && "Invalid const expression data!");
    return value->data;
}
#endif

void mir_type_to_str(char *buf, usize len, const struct mir_type *type, bool prefer_name)
{
    if (!buf || !len) return;
    buf[0] = '\0';
    _type_to_str(buf, len, type, prefer_name);
}

static void provide_builtin_arch(struct context *ctx)
{
    struct BuiltinTypes *bt = ctx->builtin_types;
    struct scope        *scope =
        scope_create(&ctx->assembly->arenas.scope, SCOPE_TYPE_ENUM, ctx->assembly->gscope, NULL);
    mir_variants_t  *variants = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    static struct id ids[static_arrlenu(arch_names)];
    for (usize i = 0; i < static_arrlenu(arch_names); ++i) {
        struct mir_variant *variant =
            create_variant(ctx, id_init(&ids[i], arch_names[i]), bt->t_s32, i);
        sarrput(variants, variant);
        provide_builtin_variant(ctx, scope, variant);
    }
    struct mir_type *t_arch =
        create_type_enum(ctx, BID(ARCH_ENUM), scope, bt->t_s32, variants, false);
    provide_builtin_type(ctx, t_arch);
    add_global_int(ctx, BID(ARCH), false, t_arch, ctx->assembly->target->triple.arch);
}

static void provide_builtin_os(struct context *ctx)
{
    struct BuiltinTypes *bt = ctx->builtin_types;
    struct scope        *scope =
        scope_create(&ctx->assembly->arenas.scope, SCOPE_TYPE_ENUM, ctx->assembly->gscope, NULL);
    mir_variants_t  *variants = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    static struct id ids[static_arrlenu(os_names)];
    for (usize i = 0; i < static_arrlenu(os_names); ++i) {
        struct mir_variant *variant =
            create_variant(ctx, id_init(&ids[i], os_names[i]), bt->t_s32, i);
        sarrput(variants, variant);
        provide_builtin_variant(ctx, scope, variant);
    }
    struct mir_type *t_os =
        create_type_enum(ctx, BID(PLATFORM_ENUM), scope, bt->t_s32, variants, false);
    provide_builtin_type(ctx, t_os);
    add_global_int(ctx, BID(PLATFORM), false, t_os, ctx->assembly->target->triple.os);
}

static void provide_builtin_env(struct context *ctx)
{
    struct BuiltinTypes *bt = ctx->builtin_types;
    struct scope        *scope =
        scope_create(&ctx->assembly->arenas.scope, SCOPE_TYPE_ENUM, ctx->assembly->gscope, NULL);
    mir_variants_t  *variants = arena_safe_alloc(&ctx->assembly->arenas.sarr);
    static struct id ids[static_arrlenu(env_names)];
    for (usize i = 0; i < static_arrlenu(env_names); ++i) {
        struct mir_variant *variant =
            create_variant(ctx, id_init(&ids[i], env_names[i]), bt->t_s32, i);
        sarrput(variants, variant);
        provide_builtin_variant(ctx, scope, variant);
    }
    struct mir_type *t_env =
        create_type_enum(ctx, BID(ENV_ENUM), scope, bt->t_s32, variants, false);
    provide_builtin_type(ctx, t_env);
    add_global_int(ctx, BID(ENV), false, t_env, ctx->assembly->target->triple.env);
}

void initialize_builtins(struct context *ctx)
{
#define PROVIDE(N) provide_builtin_type(ctx, bt->t_##N)

    struct BuiltinTypes *bt = ctx->builtin_types;
    bt->t_s8                = create_type_int(ctx, BID(TYPE_S8), 8, true);
    bt->t_s16               = create_type_int(ctx, BID(TYPE_S16), 16, true);
    bt->t_s32               = create_type_int(ctx, BID(TYPE_S32), 32, true);
    bt->t_s64               = create_type_int(ctx, BID(TYPE_S64), 64, true);
    bt->t_u8                = create_type_int(ctx, BID(TYPE_U8), 8, false);
    bt->t_u16               = create_type_int(ctx, BID(TYPE_U16), 16, false);
    bt->t_u32               = create_type_int(ctx, BID(TYPE_U32), 32, false);
    bt->t_u64               = create_type_int(ctx, BID(TYPE_U64), 64, false);
    bt->t_usize             = create_type_int(ctx, BID(TYPE_USIZE), 64, false);
    bt->t_bool              = create_type_bool(ctx);
    bt->t_f32               = create_type_real(ctx, BID(TYPE_F32), 32);
    bt->t_f64               = create_type_real(ctx, BID(TYPE_F64), 64);

    bt->t_dummy_ptr = create_type_ptr(ctx, bt->t_u8);
    bt->t_type      = create_type_type(ctx);
    bt->t_scope     = create_type_named_scope(ctx);
    bt->t_void      = create_type_void(ctx);

    bt->t_u8_ptr = create_type_ptr(ctx, bt->t_u8);
    bt->t_string = CREATE_TYPE_STRUCT_STRING(ctx, BID(TYPE_STRING), bt->t_u8_ptr);

    bt->t_string_ptr = create_type_ptr(ctx, bt->t_string);

    bt->t_string_slice = CREATE_TYPE_STRUCT_SLICE(ctx, NULL, bt->t_string_ptr);

    bt->t_resolve_type_fn = create_type_fn(ctx, NULL, bt->t_type, NULL, false, false, false);
    bt->t_test_case_fn    = create_type_fn(ctx, NULL, bt->t_void, NULL, false, false, false);

    provide_builtin_arch(ctx);
    provide_builtin_os(ctx);
    provide_builtin_env(ctx);

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
    add_global_bool(ctx, BID(IS_DEBUG), false, ctx->debug_mode);

    // Add IS_COMPTIME_RUN immutable into the global scope to provide information about compile
    // time run.
    ctx->assembly->vm_run.is_comptime_run = add_global_bool(ctx, BID(IS_COMPTIME_RUN), true, false);
#undef PROVIDE
}

const char *get_intrinsic(const char *name)
{
    if (!name) return NULL;
    if (strcmp(name, "memset.p0i8.i64") == 0) return "__intrinsic_memset_p0i8_i64";
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

struct mir_fn *group_select_overload(struct context            *ctx,
                                     const struct mir_fn_group *group,
                                     const mir_types_t         *expected_args)
{
    BL_MAGIC_ASSERT(group);
    bassert(expected_args);
    const mir_fns_t *variants = group->variants;
    bassert(sarrlenu(variants));
    struct mir_fn *selected          = sarrpeek(variants, 0);
    s32            selected_priority = 0;
    for (usize i = 0; i < sarrlenu(variants); ++i) {
        struct mir_fn    *it_fn = sarrpeek(variants, i);
        s32               p     = 0;
        const mir_args_t *args  = it_fn->type->data.fn.args;
        const usize       argc  = sarrlenu(args);
        const usize       eargc = sarrlenu(expected_args);
        if (argc == eargc) p += 1;
        for (usize j = 0; j < sarrlenu(args) && j < sarrlenu(expected_args); ++j) {
            const struct mir_type *t  = sarrpeek(args, j)->type;
            const struct mir_type *et = sarrpeek(expected_args, j);
            if (type_cmp(et, t)) {
                p += 3;
                continue;
            }
            if (can_impl_cast(et, t)) {
                p += 2;
                continue;
            }
            if (type_cmp(t, ctx->builtin_types->t_Any)) {
                p += 2;
                continue;
            }
        }
        if (p > selected_priority) {
            selected          = it_fn;
            selected_priority = p;
        }
    }
    BL_MAGIC_ASSERT(selected);
    return selected;
}

void mir_arenas_init(struct mir_arenas *arenas)
{
    const usize instr_size = SIZEOF_MIR_INSTR;
    arena_init(&arenas->instr, instr_size, ALIGNOF_MIR_INSTR, ARENA_INSTR_CHUNK_COUNT, NULL);
    arena_init(&arenas->type,
               sizeof(struct mir_type),
               alignment_of(struct mir_type),
               ARENA_CHUNK_COUNT,
               NULL);
    arena_init(&arenas->var,
               sizeof(struct mir_var),
               alignment_of(struct mir_var),
               ARENA_CHUNK_COUNT,
               NULL);
    arena_init(&arenas->fn_poly,
               sizeof(struct mir_fn_poly_recipe),
               alignment_of(struct mir_fn_poly_recipe),
               ARENA_CHUNK_COUNT,
               (arena_elem_dtor_t)&fn_poly_dtor);
    arena_init(&arenas->fn,
               sizeof(struct mir_fn),
               alignment_of(struct mir_fn),
               ARENA_CHUNK_COUNT,
               (arena_elem_dtor_t)&fn_dtor);
    arena_init(&arenas->member,
               sizeof(struct mir_member),
               alignment_of(struct mir_member),
               ARENA_CHUNK_COUNT,
               NULL);
    arena_init(&arenas->variant,
               sizeof(struct mir_variant),
               alignment_of(struct mir_variant),
               ARENA_CHUNK_COUNT,
               NULL);
    arena_init(&arenas->arg,
               sizeof(struct mir_arg),
               alignment_of(struct mir_arg),
               ARENA_CHUNK_COUNT / 2,
               NULL);
    arena_init(&arenas->fn_group,
               sizeof(struct mir_fn_group),
               alignment_of(struct mir_fn_group),
               ARENA_CHUNK_COUNT / 2,
               NULL);
}

void mir_arenas_terminate(struct mir_arenas *arenas)
{
    arena_terminate(&arenas->fn);
    arena_terminate(&arenas->instr);
    arena_terminate(&arenas->member);
    arena_terminate(&arenas->type);
    arena_terminate(&arenas->var);
    arena_terminate(&arenas->variant);
    arena_terminate(&arenas->arg);
    arena_terminate(&arenas->fn_group);
    arena_terminate(&arenas->fn_poly);
}

void mir_run(struct assembly *assembly)
{
    runtime_measure_begin(mir);
    struct context ctx;
    zone();
    memset(&ctx, 0, sizeof(struct context));
    ctx.assembly                            = assembly;
    ctx.debug_mode                          = assembly->target->opt == ASSEMBLY_OPT_DEBUG;
    ctx.builtin_types                       = &assembly->builtin_types;
    ctx.vm                                  = &assembly->vm;
    ctx.testing.cases                       = assembly->testing.cases;
    ctx.polymorph.current_scope_layer_index = SCOPE_DEFAULT_LAYER;
    ctx.ast.current_defer_stack_index       = -1;

    arrsetcap(ctx.analyze.usage_check_arr, 256);
    arrsetcap(ctx.analyze.stack[0], 256);
    arrsetcap(ctx.analyze.stack[1], 256);

    ctx.analyze.void_entry =
        scope_create_entry(&ctx.assembly->arenas.scope, SCOPE_ENTRY_VOID, NULL, NULL, true);

    // initialize all builtin types
    initialize_builtins(&ctx);

    // Gen MIR from ast pass
    for (usize i = 0; i < arrlenu(assembly->units); ++i) {
        struct unit *unit = assembly->units[i];
        ast(&ctx, unit->ast);
    }

    if (builder.errorc) goto SKIP;

    // Skip analyze if no_analyze is set by user.
    if (assembly->target->no_analyze) goto SKIP;

    // Analyze pass
    analyze(&ctx);
    if (builder.errorc) goto SKIP;
    bassert(arrlen(ctx.analyze.stack[0]) == 0 && arrlen(ctx.analyze.stack[1]) == 0);
    analyze_report_unresolved(&ctx);
    if (builder.errorc) goto SKIP;
    analyze_report_unused(&ctx);

    blog("Analyze queue push count: %i", push_count);
SKIP:
    assembly->stats.mir_s = runtime_measure_end(mir);
    ast_free_defer_stack(&ctx);
    arrfree(ctx.analyze.stack[0]);
    arrfree(ctx.analyze.stack[1]);
    hmfree(ctx.analyze.waiting);

    arrfree(ctx.analyze.usage_check_arr);
    sarrfree(&ctx.analyze.incomplete_rtti);
    sarrfree(&ctx.polymorph.replacement_queue);
    sarrfree(&ctx.analyze.complete_check_type_stack);
    return_zone();
}
