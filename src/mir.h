// =================================================================================================
// bl
//
// File:   mir.h
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

#ifndef BL_MIR_H
#define BL_MIR_H

#include "arena.h"
#include "ast.h"
#include "common.h"
#include "scope.h"
#include "vm.h"
#include <dyncall.h>
#include <dyncall_callback.h>
#include <dynload.h>

// Slice member indices
#define MIR_SLICE_LEN_INDEX 0
#define MIR_SLICE_PTR_INDEX 1

// String member indices
#define MIR_STRING_LEN_INDEX MIR_SLICE_LEN_INDEX
#define MIR_STRING_PTR_INDEX MIR_SLICE_PTR_INDEX

// Dynamic array member indices
#define MIR_DYNARR_LEN_INDEX MIR_SLICE_LEN_INDEX
#define MIR_DYNARR_PTR_INDEX MIR_SLICE_PTR_INDEX
#define MIR_DYNARR_ALLOCATED_INDEX 2

// Helper macro for reading Const Expression Values of fundamental types.
#if BL_DEBUG
#define MIR_CEV_READ_AS(T, src) (*((T *)_mir_cev_read(src)))
#else
#define MIR_CEV_READ_AS(T, src) (*((T *)(src)->data))
#endif
#define MIR_CEV_WRITE_AS(T, dest, src) (*((T *)(dest)->data) = (src))

struct assembly;
struct builder;
struct unit;
struct location;

struct mir_type;
struct mir_member;
struct mir_variant;
struct mir_arg;
struct mir_var;
struct mir_fn;
struct mir_fn_group;
struct mir_fn_poly_recipe;
struct mir_const_expr_value;

struct mir_instr;
struct mir_instr_unreachable;
struct mir_instr_debugbreak;
struct mir_instr_block;
struct mir_instr_decl_var;
struct mir_instr_decl_member;
struct mir_instr_decl_variant;
struct mir_instr_decl_arg;
struct mir_instr_const;
struct mir_instr_load;
struct mir_instr_store;
struct mir_instr_ret;
struct mir_instr_binop;
struct mir_instr_unop;
struct mir_instr_fn_proto;
struct mir_instr_fn_group;
struct mir_instr_call;
struct mir_instr_addrof;
struct mir_instr_cond_br;
struct mir_instr_br;
struct mir_instr_arg;
struct mir_instr_elem_ptr;
struct mir_instr_member_ptr;
struct mir_instr_type_fn;
struct mir_instr_type_fn_group;
struct mir_instr_type_struct;
struct mir_instr_type_array;
struct mir_instr_type_slice;
struct mir_instr_type_dyn_arr;
struct mir_instr_type_vargs;
struct mir_instr_type_ptr;
struct mir_instr_type_enum;
struct mir_instr_type_poly;
struct mir_instr_decl_ref;
struct mir_instr_decl_direct_ref;
struct mir_instr_cast;
struct mir_instr_sizeof;
struct mir_instr_alignof;
struct mir_instr_compound;
struct mir_instr_vargs;
struct mir_instr_type_info;
struct mir_instr_phi;
struct mir_instr_to_any;
struct mir_instr_switch;
struct mir_instr_set_initializer;
struct mir_instr_test_case;
struct mir_instr_call_loc;
struct mir_instr_unroll;

struct mir_arenas {
    struct arena instr;
    struct arena type;
    struct arena var;
    struct arena fn;
    struct arena member;
    struct arena variant;
    struct arena arg;
    struct arena fn_group;
    struct arena fn_poly;
};

struct mir_switch_case {
    struct mir_instr       *on_value;
    struct mir_instr_block *block;
};

TSMALL_ARRAY_TYPE(SwitchCase, struct mir_switch_case, 64)

enum mir_type_kind {
    MIR_TYPE_INVALID     = 0,
    MIR_TYPE_TYPE        = 1,
    MIR_TYPE_VOID        = 2,
    MIR_TYPE_INT         = 3,
    MIR_TYPE_REAL        = 4,
    MIR_TYPE_FN          = 5,
    MIR_TYPE_PTR         = 6,
    MIR_TYPE_BOOL        = 7,
    MIR_TYPE_ARRAY       = 8,
    MIR_TYPE_STRUCT      = 9,
    MIR_TYPE_ENUM        = 10,
    MIR_TYPE_NULL        = 11,
    MIR_TYPE_STRING      = 12,
    MIR_TYPE_VARGS       = 13,
    MIR_TYPE_SLICE       = 14,
    MIR_TYPE_DYNARR      = 15,
    MIR_TYPE_FN_GROUP    = 16,
    MIR_TYPE_NAMED_SCOPE = 17,
    MIR_TYPE_POLY        = 18,
};

// External function arguments passing composit types by value needs special handling in IR.
enum llvm_extern_arg_struct_generation_mode {
    LLVM_EASGM_NONE,  // No special handling
    LLVM_EASGM_8,     // Promote composit as i8
    LLVM_EASGM_16,    // Promote composit as i16
    LLVM_EASGM_32,    // Promote composit as i32
    LLVM_EASGM_64,    // Promote composit as i64
    LLVM_EASGM_64_8,  // Promote composit as i64, i8
    LLVM_EASGM_64_16, // Promote composit as i64, i16
    LLVM_EASGM_64_32, // Promote composit as i64, i32
    LLVM_EASGM_64_64, // Promote composit as i64, i64
    LLVM_EASGM_BYVAL, // Promote composit as byval
};

enum mir_instr_kind {
    MIR_INSTR_INVALID = 0,
#define GEN_INSTR_KINDS
#include "mir.inc"
#undef GEN_INSTR_KINDS
};

enum mir_cast_op {
    MIR_CAST_INVALID,
    MIR_CAST_NONE,
    MIR_CAST_BITCAST,
    MIR_CAST_SEXT,
    MIR_CAST_ZEXT,
    MIR_CAST_TRUNC,
    MIR_CAST_FPTRUNC,
    MIR_CAST_FPEXT,
    MIR_CAST_FPTOSI,
    MIR_CAST_FPTOUI,
    MIR_CAST_SITOFP,
    MIR_CAST_UITOFP,
    MIR_CAST_PTRTOINT,
    MIR_CAST_INTTOPTR,
    MIR_CAST_PTRTOBOOL,
};

enum builtin_id_kind {
    BUILTIN_ID_NONE = -1,
#define GEN_BUILTIN_NAMES
#include "builtin.inc"
#undef GEN_BUILTIN_NAMES
    _BUILTIN_ID_COUNT,
};

extern struct id builtin_ids[_BUILTIN_ID_COUNT];

#define BID(name) &builtin_ids[BUILTIN_ID_##name]

struct dyncall_cb_context {
    struct virtual_machine *vm;
    struct mir_fn          *fn;
};

struct mir_fn_poly_recipe {
    // Function literal (used for function replacement generation).
    struct ast *ast_lit_fn;
    // Scope layer solves symbol collisions in reused scopes.
    s32 scope_layer;
    // Cache of already generated functions (replacement hash -> struct mir_fn*).
    struct {
        hash_t                     key;
        struct mir_instr_fn_proto *value;
    } * entries;

    BL_MAGIC_ADD
};

// FN
struct mir_fn {
    // Must be first!!!
    struct mir_instr   *prototype;
    struct id          *id;
    struct ast         *decl_node;
    struct scope_entry *scope_entry;

    // Optional, set only for polymorphic functions.
    // @CLEANUP we can use this type directly without function to save some memory.
    struct mir_fn_poly_recipe *poly;

    // Optional, this is set to first call location used for generation of this function from
    // polymorph recipe.
    struct ast *first_poly_call_node;
    const char *debug_poly_replacement;

    // function body scope if there is one (optional)
    struct scope    *body_scope;
    struct mir_type *type;
    struct mir_var **variables;

    // Linkage name of the function, this name is used during linking to identify function,
    // actual implementation can be external, internal or intrinsic embedded in compiler,
    // depending on function flags.
    const char *linkage_name;
    // Full name contains full function name with parent scopes.
    const char *full_name;

    LLVMValueRef llvm_value;
    bool         is_fully_analyzed; // @Cleanup: Remove this!
    bool         is_global;
    s32          ref_count;

    u32                  flags;
    enum builtin_id_kind builtin_id;

    // pointer to the first block inside function body
    struct mir_instr_block *first_block;
    struct mir_instr_block *last_block;
    struct mir_instr_block *exit_block;

    // Temporary variable used for return value.
    struct mir_instr *ret_tmp;

    // Return instruction of function.
    struct mir_instr_ret *terminal_instr;
    struct location      *first_unreachable_loc;

    // dyncall external context
    struct {
        DCpointer                 extern_entry;
        DCCallback               *extern_callback_handle;
        struct dyncall_cb_context context;
    } dyncall;
    BL_MAGIC_ADD
};

struct mir_fn_group {
    struct ast        *decl_node;
    TSmallArray_FnPtr *variants;
    BL_MAGIC_ADD
};

// MEMBER
struct mir_member {
    struct mir_type    *type;
    struct id          *id;
    struct ast         *decl_node;
    struct scope_entry *entry;
    s64                 index;
    s32                 offset_bytes;
    s32                 tags;
    bool                is_base; // inherrited struct base
    bool                is_parent_union;
    BL_MAGIC_ADD
};

// FUNCTION ARGUMENT
struct mir_arg {
    struct mir_type *type;
    struct id       *id;
    struct ast      *decl_node;
    struct scope    *decl_scope;

    // This is index of this argument in LLVM IR not in MIR, it can be different based on
    // compiler configuration (via. System V ABI)
    u32 llvm_index;

    // Optional default value.
    struct mir_instr *value;

    enum llvm_extern_arg_struct_generation_mode llvm_easgm;
};

// TYPE
struct mir_type_int {
    s32  bitcount;
    bool is_signed;
};

struct mir_type_real {
    s32 bitcount;
};

enum mir_type_fn_flags {
    MIR_TYPE_FN_FLAG_IS_VARGS         = 1 << 0,
    MIR_TYPE_FN_FLAG_HAS_BYVAL        = 1 << 1,
    MIR_TYPE_FN_FLAG_HAS_SRET         = 1 << 2,
    MIR_TYPE_FN_FLAG_HAS_DEFAULT_ARGS = 1 << 3,
    MIR_TYPE_FN_FLAG_IS_POLYMORPH     = 1 << 4,
};

struct mir_type_fn {
    struct mir_type     *ret_type;
    TSmallArray_ArgPtr  *args;
    hash_t               argument_hash;
    enum builtin_id_kind builtin_id;
    u32                  flags;
};

struct mir_type_poly {
    bool is_master;
};

struct mir_type_fn_group {
    TSmallArray_TypePtr *variants;
};

struct mir_type_named_scope {
    void *_;
};

struct mir_type_ptr {
    struct mir_type *expr;
};

struct mir_type_struct {
    struct scope          *scope; // struct body scope
    TSmallArray_MemberPtr *members;
    // This is optional base type, only structures with #base hash directive has this
    // information.
    struct mir_type *base_type;
    bool             is_packed;
    // C-style union is represented as regular structure with special memory layout. Every
    // member is stored at same memory offset.
    bool is_union;
    // Set true only for incomplete forward declarations of the struct.
    bool is_incomplete;
    // Set true for struct type used as multiple return temporary.
    bool is_multiple_return_type;
};

// Enum variants must be baked into enum type.
struct mir_type_enum {
    struct scope           *scope;
    struct mir_type        *base_type;
    TSmallArray_VariantPtr *variants; // struct mir_variant *
    bool                    is_flags;
};

struct mir_type_null {
    struct mir_type *base_type;
};

struct mir_type_array {
    struct mir_type *elem_type;
    s64              len;
};

struct mir_type {
    struct id         *user_id;
    struct id          id;
    LLVMTypeRef        llvm_type;
    LLVMMetadataRef    llvm_meta;
    usize              size_bits;
    usize              store_size_bytes;
    enum mir_type_kind kind;
    s8                 alignment;
    bool               checked_and_complete;

    // Optionally set pointer to RTTI var used by Virtual Machine.
    struct mir_var *vm_rtti_var_cache;

    union {
        struct mir_type_int         integer;
        struct mir_type_fn          fn;
        struct mir_type_fn_group    fn_group;
        struct mir_type_ptr         ptr;
        struct mir_type_real        real;
        struct mir_type_array       array;
        struct mir_type_struct      strct;
        struct mir_type_enum        enm;
        struct mir_type_null        null;
        struct mir_type_named_scope named_scope;
        struct mir_type_poly        poly;
    } data;

    BL_MAGIC_ADD
};

// VARIANT
struct mir_variant {
    struct id          *id;
    struct scope_entry *entry;
    struct mir_type    *value_type;
    u64                 value;
};

// VAR
struct mir_var {
    struct mir_const_expr_value value; // contains also allocated type
    struct id                  *id;
    struct ast                 *decl_node;
    struct scope               *decl_scope;
    struct scope_entry         *entry;
    struct mir_instr           *initializer_block;
    vm_relative_stack_ptr_t     rel_stack_ptr;
    LLVMValueRef                llvm_value;
    const char                 *linkage_name;
    enum builtin_id_kind        builtin_id;
    s32                         ref_count;
    u32                         flags;
    // @Performance use flags to reduce size?
    bool is_mutable;
    bool is_global;
    bool is_implicit;
    bool is_struct_typedef;
    bool emit_llvm; // Keep this, we sometimes have i.e. type defs in scope of the function.
    bool is_analyzed;
};

enum mir_instr_flags {
    MIR_IS_ANALYZED    = 1,
    MIR_IS_UNREACHABLE = 2,
    MIR_IS_IMPLICIT    = 4, // generated by compiler
};

struct mir_instr {
    struct mir_const_expr_value value;
    u64                         id;
    struct ast                 *node;
    struct mir_instr_block     *owner_block;
    LLVMValueRef                llvm_value;
    struct mir_instr           *prev;
    struct mir_instr           *next;
    enum mir_instr_kind         kind;
    s32                         ref_count;
    u32                         flags;
};

struct mir_instr_msg {
    struct mir_instr base;

    enum ast_msg_kind kind;
    const char       *text;
};

struct mir_instr_block {
    struct mir_instr base;

    const char       *name;
    struct mir_instr *entry_instr;
    struct mir_instr *last_instr;
    struct mir_instr *terminal;
    // Optional; when not set block is implicit global block.
    struct mir_fn *owner_fn;
};

struct mir_instr_decl_var {
    struct mir_instr base;

    struct mir_var   *var;
    struct mir_instr *type;
    struct mir_instr *init;
};

struct mir_instr_decl_member {
    struct mir_instr base;

    struct mir_member *member;
    struct mir_instr  *type;

    TSmallArray_InstrPtr *tags; // Optional.
};

struct mir_instr_decl_variant {
    struct mir_instr base;

    struct mir_variant *variant;
    struct mir_variant *prev_variant; // Optional.
    struct mir_instr   *value;        // Optional.
    struct mir_instr   *base_type;    // Optional.
    bool                is_flags;
};

struct mir_instr_decl_arg {
    struct mir_instr base;

    struct mir_arg   *arg;
    struct mir_instr *type;
    bool              llvm_byval;
};

struct mir_instr_elem_ptr {
    struct mir_instr base;

    struct mir_instr *arr_ptr;
    struct mir_instr *index;
};

struct mir_instr_member_ptr {
    struct mir_instr base;

    struct ast          *member_ident;
    struct mir_instr    *target_ptr;
    struct scope_entry  *scope_entry;
    enum builtin_id_kind builtin_id;
};

struct mir_instr_cast {
    struct mir_instr base;

    enum mir_cast_op  op;
    struct mir_instr *type;
    struct mir_instr *expr;
    bool              auto_cast;
};

struct mir_instr_sizeof {
    struct mir_instr base;

    struct mir_instr *expr;
};

struct mir_instr_alignof {
    struct mir_instr base;

    struct mir_instr *expr;
};

struct mir_instr_arg {
    struct mir_instr base;

    unsigned i;
};

struct mir_instr_const {
    struct mir_instr base;
    // Constant marked as volatile can change it's type as needed by expression.
    bool volatile_type;
};

struct mir_instr_load {
    struct mir_instr base;

    struct mir_instr *src;
    // This flag is set when load is user-level dereference.
    bool is_deref;
};

struct mir_instr_store {
    struct mir_instr base;

    struct mir_instr *src;
    struct mir_instr *dest;
};

struct mir_instr_addrof {
    struct mir_instr base;

    struct mir_instr *src;
};

struct mir_instr_ret {
    struct mir_instr base;

    struct mir_instr *value;
};

struct mir_instr_set_initializer {
    struct mir_instr base;

    struct mir_instr     *dest;
    TSmallArray_InstrPtr *dests;
    struct mir_instr     *src;
};

struct mir_instr_binop {
    struct mir_instr base;

    enum binop_kind   op;
    struct mir_instr *lhs;
    struct mir_instr *rhs;
    bool              volatile_type;
};

struct mir_instr_unop {
    struct mir_instr base;

    enum unop_kind    op;
    struct mir_instr *expr;
    bool              volatile_type;
};

struct mir_instr_fn_proto {
    struct mir_instr base;

    struct mir_instr *type;
    struct mir_instr *user_type;
    bool              pushed_for_analyze;
};

struct mir_instr_fn_group {
    struct mir_instr base;

    TSmallArray_InstrPtr *variants;
};

struct mir_instr_type_fn {
    struct mir_instr base;

    struct mir_instr     *ret_type;
    TSmallArray_InstrPtr *args;
    enum builtin_id_kind  builtin_id;
    bool                  is_polymorph;
};

struct mir_instr_type_fn_group {
    struct mir_instr base;

    struct id            *id;
    TSmallArray_InstrPtr *variants;
};

struct mir_instr_type_struct {
    struct mir_instr base;

    // fwd_decl is optional pointer to forward declaration of this structure type.
    struct mir_instr     *fwd_decl;
    struct id            *id;
    struct scope         *scope;
    TSmallArray_InstrPtr *members;
    bool                  is_packed;
    bool                  is_union;
    // Set true for struct type used as multiple return temporary.
    bool is_multiple_return_type;
};

struct mir_instr_type_enum {
    struct mir_instr base;

    struct id            *id;
    struct scope         *scope;
    TSmallArray_InstrPtr *variants;
    struct mir_instr     *base_type;
    bool                  is_flags;
};

struct mir_instr_type_ptr {
    struct mir_instr base;

    struct mir_instr *type;
};

struct mir_instr_type_array {
    struct mir_instr base;

    struct mir_instr *elem_type;
    struct mir_instr *len;
    struct id        *id;
};

struct mir_instr_type_slice {
    struct mir_instr base;

    struct mir_instr *elem_type;
};

struct mir_instr_type_dyn_arr {
    struct mir_instr base;

    struct mir_instr *elem_type;
};

struct mir_instr_type_vargs {
    struct mir_instr base;

    struct mir_instr *elem_type;
};

struct mir_instr_type_poly {
    struct mir_instr base;

    struct id *T_id;
};

struct mir_instr_call {
    struct mir_instr base;

    struct mir_instr     *callee;
    TSmallArray_InstrPtr *args; // Optional
    bool                  callee_analyzed;
    bool                  call_in_compile_time;

    // Optional temporary variable for unroll multi-return struct type.
    struct mir_instr *unroll_tmp_var;
};

struct mir_instr_decl_ref {
    struct mir_instr base;

    struct unit        *parent_unit;
    struct id          *rid;
    struct scope       *scope;
    s32                 scope_layer;
    struct scope_entry *scope_entry;

    // Set only for decl_refs inside struct member type resolvers.
    bool accept_incomplete_type;
};

struct mir_instr_decl_direct_ref {
    struct mir_instr base;

    struct mir_instr *ref;
};

struct mir_instr_unreachable {
    struct mir_instr base;

    struct mir_fn *abort_fn;
};

struct mir_instr_debugbreak {
    struct mir_instr base;

    struct mir_fn *break_fn;
};

struct mir_instr_cond_br {
    struct mir_instr base;

    struct mir_instr       *cond;
    struct mir_instr_block *then_block;
    struct mir_instr_block *else_block;

    // This value is used only during execution in Virtual Machine, when conditional break is
    // generated to be used as pre-instruction to PHI, we must keep condition value on stack (if
    // it's not compile time known) in order to be used as resolution of PHI expression.
    bool keep_stack_value;
    // Conditional break generated from static if.
    bool is_static;
};

struct mir_instr_br {
    struct mir_instr base;

    struct mir_instr_block *then_block;
};

struct mir_instr_compound {
    struct mir_instr base;

    struct mir_instr     *type;
    TSmallArray_InstrPtr *values;
    struct mir_var       *tmp_var;
    bool                  is_naked;
    bool                  is_zero_initialized;
    // Set when compound is used as multiple return value.
    bool is_multiple_return_value;
};

struct mir_instr_vargs {
    struct mir_instr base;

    struct mir_var       *arr_tmp;
    struct mir_var       *vargs_tmp;
    struct mir_type      *type;
    TSmallArray_InstrPtr *values;
};

struct mir_instr_type_info {
    struct mir_instr base;

    struct mir_instr *expr;
    struct mir_type  *rtti_type;
};

struct mir_instr_test_case {
    struct mir_instr base;
};

struct mir_instr_call_loc {
    struct mir_instr base;

    struct location *call_location; // Optional call location
    struct mir_var  *meta_var;      // Optional meta var.
    hash_t           hash;
};

struct mir_instr_unroll {
    struct mir_instr base;

    struct mir_instr *src;
    struct mir_instr *remove_src;
    s32               index;
    bool              remove;
};

struct mir_instr_phi {
    struct mir_instr base;

    TSmallArray_InstrPtr *incoming_values;
    TSmallArray_InstrPtr *incoming_blocks;
};

struct mir_instr_to_any {
    struct mir_instr base;

    struct mir_instr *expr;
    struct mir_type  *rtti_type;
    struct mir_type  *rtti_data; // optional
    struct mir_var   *tmp;
    struct mir_var   *expr_tmp; // optional
};

struct mir_instr_switch {
    struct mir_instr base;

    struct mir_instr       *value;
    struct mir_instr_block *default_block;
    TSmallArray_SwitchCase *cases;
    bool                    has_user_defined_default;
};

// public
static INLINE bool mir_is_pointer_type(const struct mir_type *type)
{
    bassert(type);
    return type->kind == MIR_TYPE_PTR;
}

static INLINE struct mir_type *mir_deref_type(const struct mir_type *ptr)
{
    if (!mir_is_pointer_type(ptr)) return NULL;
    return ptr->data.ptr.expr;
}

static INLINE bool mir_is_composit_type(const struct mir_type *type)
{
    switch (type->kind) {
    case MIR_TYPE_STRUCT:
    case MIR_TYPE_STRING:
    case MIR_TYPE_SLICE:
    case MIR_TYPE_VARGS:
    case MIR_TYPE_DYNARR:
        return true;

    default:
        break;
    }

    return false;
}

static INLINE struct mir_type *mir_get_struct_elem_type(const struct mir_type *type, u32 i)
{
    bassert(mir_is_composit_type(type) && "Expected structure type");
    TSmallArray_MemberPtr *members = type->data.strct.members;
    bassert(members && members->size > i);

    return members->data[i]->type;
}

static INLINE struct mir_type *mir_get_fn_arg_type(const struct mir_type *type, u32 i)
{
    bassert(type->kind == MIR_TYPE_FN && "Expected function type");
    TSmallArray_ArgPtr *args = type->data.fn.args;
    if (!args) return NULL;
    bassert(args->size > i);

    return args->data[i]->type;
}

// Determinate if the instruction has compile time known value.
static INLINE bool mir_is_comptime(const struct mir_instr *instr)
{
    return instr->value.is_comptime;
}

static INLINE bool mir_is_global_block(const struct mir_instr_block *instr)
{
    return instr->owner_fn == NULL;
}

// Determinates if the instruction is in the global block.
static INLINE bool mir_is_global(const struct mir_instr *instr)
{
    // Instructions without owner block lives in global scope.
    if (!instr->owner_block) return true;
    return mir_is_global_block(instr->owner_block);
}

static INLINE bool mir_type_has_llvm_representation(const struct mir_type *type)
{
    bassert(type);
    return type->kind != MIR_TYPE_TYPE && type->kind != MIR_TYPE_FN_GROUP &&
           type->kind != MIR_TYPE_NAMED_SCOPE && type->kind != MIR_TYPE_POLY;
}

void           mir_arenas_init(struct mir_arenas *arenas);
void           mir_arenas_terminate(struct mir_arenas *arenas);
void           mir_type_to_str(char *buf, usize len, const struct mir_type *type, bool prefer_name);
const char    *mir_instr_name(const struct mir_instr *instr);
void           mir_run(struct assembly *assembly);
struct mir_fn *mir_get_callee(const struct mir_instr_call *call);
const char    *mir_get_fn_readable_name(struct mir_fn *fn);

#if BL_DEBUG
vm_stack_ptr_t _mir_cev_read(struct mir_const_expr_value *value);
#endif

#endif
