//************************************************************************************************
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
//************************************************************************************************

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

struct Assembly;
struct Builder;
struct Unit;
struct Location;

typedef struct MirType           MirType;
typedef struct MirMember         MirMember;
typedef struct MirVariant        MirVariant;
typedef struct MirArg            MirArg;
typedef struct MirVar            MirVar;
typedef struct MirFn             MirFn;
typedef struct MirFnGroup        MirFnGroup;
typedef struct MirConstExprValue MirConstExprValue;

typedef struct MirInstr               MirInstr;
typedef struct MirInstrUnreachable    MirInstrUnreachable;
typedef struct MirInstrBlock          MirInstrBlock;
typedef struct MirInstrDeclVar        MirInstrDeclVar;
typedef struct MirInstrDeclMember     MirInstrDeclMember;
typedef struct MirInstrDeclVariant    MirInstrDeclVariant;
typedef struct MirInstrDeclArg        MirInstrDeclArg;
typedef struct MirInstrConst          MirInstrConst;
typedef struct MirInstrLoad           MirInstrLoad;
typedef struct MirInstrStore          MirInstrStore;
typedef struct MirInstrRet            MirInstrRet;
typedef struct MirInstrBinop          MirInstrBinop;
typedef struct MirInstrUnop           MirInstrUnop;
typedef struct MirInstrFnProto        MirInstrFnProto;
typedef struct MirInstrFnGroup        MirInstrFnGroup;
typedef struct MirInstrCall           MirInstrCall;
typedef struct MirInstrAddrOf         MirInstrAddrOf;
typedef struct MirInstrCondBr         MirInstrCondBr;
typedef struct MirInstrBr             MirInstrBr;
typedef struct MirInstrArg            MirInstrArg;
typedef struct MirInstrElemPtr        MirInstrElemPtr;
typedef struct MirInstrMemberPtr      MirInstrMemberPtr;
typedef struct MirInstrTypeFn         MirInstrTypeFn;
typedef struct MirInstrTypeFnGroup    MirInstrTypeFnGroup;
typedef struct MirInstrTypeStruct     MirInstrTypeStruct;
typedef struct MirInstrTypeArray      MirInstrTypeArray;
typedef struct MirInstrTypeSlice      MirInstrTypeSlice;
typedef struct MirInstrTypeDynArr     MirInstrTypeDynArr;
typedef struct MirInstrTypeVArgs      MirInstrTypeVArgs;
typedef struct MirInstrTypePtr        MirInstrTypePtr;
typedef struct MirInstrTypeEnum       MirInstrTypeEnum;
typedef struct MirInstrDeclRef        MirInstrDeclRef;
typedef struct MirInstrDeclDirectRef  MirInstrDeclDirectRef;
typedef struct MirInstrCast           MirInstrCast;
typedef struct MirInstrSizeof         MirInstrSizeof;
typedef struct MirInstrAlignof        MirInstrAlignof;
typedef struct MirInstrCompound       MirInstrCompound;
typedef struct MirInstrVArgs          MirInstrVArgs;
typedef struct MirInstrTypeInfo       MirInstrTypeInfo;
typedef struct MirInstrTypeKind       MirInstrTypeKind;
typedef struct MirInstrPhi            MirInstrPhi;
typedef struct MirInstrToAny          MirInstrToAny;
typedef struct MirInstrSwitch         MirInstrSwitch;
typedef struct MirInstrSetInitializer MirInstrSetInitializer;
typedef struct MirInstrTestCases      MirInstrTestCases;
typedef struct MirInstrCallLoc        MirInstrCallLoc;
typedef struct MirInstrUnroll         MirInstrUnroll;

typedef struct MirArenas {
    Arena instr;
    Arena type;
    Arena var;
    Arena fn;
    Arena member;
    Arena variant;
    Arena arg;
    Arena fn_group;
} MirArenas;

typedef struct MirSwitchCase {
    MirInstr *     on_value;
    MirInstrBlock *block;
} MirSwitchCase;

TSMALL_ARRAY_TYPE(SwitchCase, MirSwitchCase, 64);

typedef enum MirBuiltinIdKind {
    MIR_BUILTIN_ID_NONE = -1,
#define GEN_BUILTIN_NAMES
#include "mir.inc"
#undef GEN_BUILTIN_NAMES
    _MIR_BUILTIN_ID_COUNT,
} MirBuiltinIdKind;

typedef enum MirTypeKind {
    MIR_TYPE_INVALID  = 0,
    MIR_TYPE_TYPE     = 1,
    MIR_TYPE_VOID     = 2,
    MIR_TYPE_INT      = 3,
    MIR_TYPE_REAL     = 4,
    MIR_TYPE_FN       = 5,
    MIR_TYPE_PTR      = 6,
    MIR_TYPE_BOOL     = 7,
    MIR_TYPE_ARRAY    = 8,
    MIR_TYPE_STRUCT   = 9,
    MIR_TYPE_ENUM     = 10,
    MIR_TYPE_NULL     = 11,
    MIR_TYPE_STRING   = 12,
    MIR_TYPE_VARGS    = 13,
    MIR_TYPE_SLICE    = 14,
    MIR_TYPE_DYNARR   = 15,
    MIR_TYPE_FN_GROUP = 16
} MirTypeKind;

typedef enum MirValueAddressMode {
    MIR_VAM_UNKNOWN,

    // Value points to memory allocation on the stack or heap.
    MIR_VAM_LVALUE,
    // Value points to memeory allocation on the stack or heap but value itself is immutable and
    // cannot be modified.
    MIR_VAM_LVALUE_CONST,
    // Does not point to allocated memory (ex: const literals).
    MIR_VAM_RVALUE,
} MirValueAddressMode;

// External function arguments passing composit types by value needs special handling in IR.
typedef enum LLVMExternArgStructGenerationMode {
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
} LLVMExternArgStructGenerationMode;

typedef enum MirInstrKind {
    MIR_INSTR_INVALID = 0,
#define GEN_INSTR_KINDS
#include "mir.inc"
#undef GEN_INSTR_KINDS
} MirInstrKind;

typedef enum MirCastOp {
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
} MirCastOp;

typedef struct {
    VM *   vm;
    MirFn *fn;
} DyncallCBContext;

// FN
struct MirFn {
    // Must be first!!!
    MirInstr *prototype;
    ID *      id;
    Ast *     decl_node;

    // function body scope if there is one (optional)
    Scope *  body_scope;
    MirType *type;
    TArray * variables;

    // Linkage name of the function, this name is used during linking to identify function,
    // actual implementation can be external, internal or intrinsic embedded in compiler,
    // depending on function flags.
    const char *linkage_name;

    LLVMValueRef llvm_value;
    bool         fully_analyzed;
    bool         emit_llvm;
    bool         is_global;
    s32          ref_count;

    u32              flags;
    MirBuiltinIdKind builtin_id;

    // pointer to the first block inside function body
    MirInstrBlock *first_block;
    MirInstrBlock *last_block;
    s32            block_count;

    // Teporary variable used for return value.
    MirInstr *ret_tmp;

    // Return instruction of function.
    MirInstrRet *    terminal_instr;
    struct Location *first_unreachable_loc;

    // dyncall external context
    struct {
        DCpointer        extern_entry;
        DCCallback *     extern_callback_handle;
        DyncallCBContext context;
    } dyncall;
    BL_MAGIC_ADD
};

struct MirFnGroup {
    Ast *              decl_node;
    TSmallArray_FnPtr *variants;
    BL_MAGIC_ADD
};

// MEMBER
struct MirMember {
    MirType *type;
    ID *     id;
    Ast *    decl_node;
    Scope *  decl_scope;
    s32      offset_bytes;
    s64      index;
    s32      tags;
    bool     is_base; // inherrited struct base
    bool     is_parent_union;
    BL_MAGIC_ADD
};

// FUNCTION ARGUMENT
struct MirArg {
    MirType *type;
    ID *     id;
    Ast *    decl_node;
    Scope *  decl_scope;

    // This is index of this argument in LLVM IR not in MIR, it can be different based on
    // compiler configuration (vix. System V ABI)
    u32 llvm_index;

    // Optional default value.
    MirInstr *value;

    LLVMExternArgStructGenerationMode llvm_easgm;
};

// TYPE
struct MirTypeInt {
    s32  bitcount;
    bool is_signed;
};

struct MirTypeReal {
    s32 bitcount;
};

struct MirTypeFn {
    MirType *           ret_type;
    TSmallArray_ArgPtr *args;
    u64                 argument_hash;
    bool                is_vargs;
    bool                has_byval;
    bool                has_sret;
    bool                has_default_args;
    MirBuiltinIdKind    builtin_id;
};

struct MirTypeFnGroup {
    TSmallArray_TypePtr *variants;
};

struct MirTypePtr {
    MirType *expr;
};

struct MirTypeStruct {
    Scope *                scope; // struct body scope
    TSmallArray_MemberPtr *members;
    bool                   is_packed;

    // C-style union is represented as regular structure with special memory layout. Every
    // member is stored at same memory offset.
    bool is_union;

    // Set true only for incomplete forward declarations of the struct.
    bool is_incomplete;

    // Set true for struct type used as multiple return temporary.
    bool is_multiple_return_type;

    // This is optional base type, only structures with #base hash directive has this
    // information.
    MirType *base_type;
};

// Enum variants must be baked into enum type.
struct MirTypeEnum {
    Scope *                 scope;
    MirType *               base_type;
    TSmallArray_VariantPtr *variants; // MirVariant *
};

struct MirTypeNull {
    MirType *base_type;
};

struct MirTypeArray {
    MirType *elem_type;
    s64      len;
};

struct MirType {
    MirTypeKind     kind;
    ID *            user_id;
    ID              id;
    LLVMTypeRef     llvm_type;
    LLVMMetadataRef llvm_meta;
    usize           size_bits;
    usize           store_size_bytes;
    s32             alignment;
    bool            checked_and_complete;

    // Optionally set pointer to RTTI var used by VM.
    MirVar *vm_rtti_var_cache;

    union {
        struct MirTypeInt     integer;
        struct MirTypeFn      fn;
        struct MirTypeFnGroup fn_group;
        struct MirTypePtr     ptr;
        struct MirTypeReal    real;
        struct MirTypeArray   array;
        struct MirTypeStruct  strct;
        struct MirTypeEnum    enm;
        struct MirTypeNull    null;
    } data;
    BL_MAGIC_ADD
};

// VALUE
struct MirConstExprValue {
    VMValue             _tmp;
    VMStackPtr          data;
    MirType *           type;
    MirValueAddressMode addr_mode;
    bool                is_comptime;
};

// VARIANT
struct MirVariant {
    ID *               id;
    Scope *            decl_scope;
    MirConstExprValue *value;
};

// VAR
struct MirVar {
    MirConstExprValue  value; // contains also allocated type
    ID *               id;
    Ast *              decl_node;
    Scope *            decl_scope;
    s32                ref_count;
    bool               is_mutable;
    bool               is_global;
    bool               is_implicit;
    bool               is_struct_typedef;
    bool               emit_llvm;
    bool               analyzed;
    u32                flags;
    VMRelativeStackPtr rel_stack_ptr;
    LLVMValueRef       llvm_value;
    const char *       linkage_name;
};

struct MirInstr {
    MirConstExprValue value;
    MirInstrKind      kind;
    u64               id;
    Ast *             node;
    MirInstrBlock *   owner_block;
    LLVMValueRef      llvm_value;

    s32  ref_count;
    bool analyzed;
    bool is_unreachable;
    bool is_implicit; // generated by compiler

    MirInstr *prev;
    MirInstr *next;
};

struct MirInstrBlock {
    MirInstr base;

    const char *name;
    bool        emit_llvm;
    MirInstr *  entry_instr;
    MirInstr *  last_instr;
    MirInstr *  terminal;
    // Optional; when not set block is implicit global block.
    MirFn *owner_fn;
};

struct MirInstrDeclVar {
    MirInstr base;

    MirVar *  var;
    MirInstr *type;
    MirInstr *init;
};

struct MirInstrDeclMember {
    MirInstr base;

    MirMember *member;
    MirInstr * type;

    TSmallArray_InstrPtr *tags; // Optional.
};

struct MirInstrDeclVariant {
    MirInstr base;

    MirVariant *variant;
    MirInstr *  value; // Optional.
};

struct MirInstrDeclArg {
    MirInstr base;

    MirArg *arg;

    // @CLEANUP: are in MirArg and can be replaced?
    MirInstr *type;
    bool      llvm_byval;
};

struct MirInstrElemPtr {
    MirInstr base;

    MirInstr *arr_ptr;
    MirInstr *index;
};

struct MirInstrMemberPtr {
    MirInstr base;

    Ast *            member_ident;
    MirInstr *       target_ptr;
    ScopeEntry *     scope_entry;
    MirBuiltinIdKind builtin_id;
};

struct MirInstrCast {
    MirInstr base;

    MirCastOp op;
    MirInstr *type;
    MirInstr *expr;
    bool      auto_cast;
};

struct MirInstrSizeof {
    MirInstr base;

    MirInstr *expr;
};

struct MirInstrAlignof {
    MirInstr base;

    MirInstr *expr;
};

struct MirInstrArg {
    MirInstr base;

    unsigned i;
};

struct MirInstrConst {
    MirInstr base;
    // Constant marked as volatile can change it's type as needed by expression.
    bool volatile_type;
};

struct MirInstrLoad {
    MirInstr base;

    // This flag is set when laod is user-level dereference.
    bool      is_deref;
    MirInstr *src;
};

struct MirInstrStore {
    MirInstr base;

    MirInstr *src;
    MirInstr *dest;
};

struct MirInstrAddrOf {
    MirInstr base;

    MirInstr *src;
};

struct MirInstrRet {
    MirInstr base;

    MirInstr *value;
};

struct MirInstrSetInitializer {
    MirInstr base;

    MirInstr *dest;
    MirInstr *src;
};

struct MirInstrBinop {
    MirInstr base;

    BinopKind op;
    MirInstr *lhs;
    MirInstr *rhs;
    bool      volatile_type;
};

struct MirInstrUnop {
    MirInstr base;

    UnopKind  op;
    MirInstr *expr;
    bool      volatile_type;
};

struct MirInstrFnProto {
    MirInstr base;

    MirInstr *type;
    MirInstr *user_type;
    bool      pushed_for_analyze;
};

struct MirInstrFnGroup {
    MirInstr base;

    TSmallArray_InstrPtr *variants;
};

struct MirInstrTypeFn {
    MirInstr base;

    MirInstr *            ret_type;
    TSmallArray_InstrPtr *args;
    MirBuiltinIdKind      builtin_id;
};

struct MirInstrTypeFnGroup {
    MirInstr base;

    ID *                  id;
    TSmallArray_InstrPtr *variants;
};

struct MirInstrTypeStruct {
    MirInstr base;

    // fwd_decl is optional pointer to forward declaration of this structure type.
    MirInstr *            fwd_decl;
    ID *                  id;
    Scope *               scope;
    TSmallArray_InstrPtr *members;
    // @CLEANUP: use flags here
    bool is_packed;
    bool is_union;
    // Set true for struct type used as multiple return temporary.
    bool is_multiple_return_type;
};

struct MirInstrTypeEnum {
    MirInstr base;

    ID *                  id;
    Scope *               scope;
    TSmallArray_InstrPtr *variants;
    MirInstr *            base_type;
};

struct MirInstrTypePtr {
    MirInstr base;

    MirInstr *type;
};

struct MirInstrTypeArray {
    MirInstr base;

    MirInstr *elem_type;
    MirInstr *len;
    ID *      id;
};

struct MirInstrTypeSlice {
    MirInstr base;

    MirInstr *elem_type;
};

struct MirInstrTypeDynArr {
    MirInstr base;

    MirInstr *elem_type;
};

struct MirInstrTypeVArgs {
    MirInstr base;

    MirInstr *elem_type;
};

struct MirInstrCall {
    MirInstr base;

    MirInstr *            callee;
    TSmallArray_InstrPtr *args; // Optional
    bool                  callee_analyzed;

    // Optional temporary variable for unroll multi-return struct type.
    MirInstr *unroll_tmp_var;
};

struct MirInstrDeclRef {
    MirInstr base;

    struct Unit *parent_unit;
    ID *         rid;
    Scope *      scope;
    ScopeEntry * scope_entry;

    // Set only for decl_refs inside struct member type resolvers.
    bool accept_incomplete_type;
};

struct MirInstrDeclDirectRef {
    MirInstr base;

    MirInstr *ref;
};

struct MirInstrUnreachable {
    MirInstr base;

    MirFn *abort_fn;
};

struct MirInstrCondBr {
    MirInstr base;

    MirInstr *     cond;
    MirInstrBlock *then_block;
    MirInstrBlock *else_block;

    // This value is used only during execution in VM, when conditional break is generated to be
    // used as pre-instruction to PHI, we must keep condition value on stack (if it's not compile
    // time known) in order to be used as resolution of PHI expression.
    bool keep_stack_value;
};

struct MirInstrBr {
    MirInstr base;

    MirInstrBlock *then_block;
};

struct MirInstrCompound {
    MirInstr base;

    MirInstr *            type;
    TSmallArray_InstrPtr *values;
    MirVar *              tmp_var;
    bool                  is_naked;
    bool                  is_zero_initialized;
    // Set when compound is used as multiple return value.
    bool is_multiple_return_value;
};

struct MirInstrVArgs {
    MirInstr base;

    MirVar *              arr_tmp;
    MirVar *              vargs_tmp;
    MirType *             type;
    TSmallArray_InstrPtr *values;
};

struct MirInstrTypeInfo {
    MirInstr base;

    MirInstr *expr;
    MirType * rtti_type;
};

struct MirInstrTestCases {
    MirInstr base;
};

struct MirInstrCallLoc {
    MirInstr base;

    struct Location *call_location; // Optional call location
    MirVar *         meta_var;      // Optional meta var.
};

struct MirInstrUnroll {
    MirInstr base;

    MirInstr *src;
    s32       index;
    bool      remove;
};

struct MirInstrTypeKind {
    MirInstr base;
};

struct MirInstrPhi {
    MirInstr base;

    TSmallArray_InstrPtr *incoming_values;
    TSmallArray_InstrPtr *incoming_blocks;
};

struct MirInstrToAny {
    MirInstr base;

    MirInstr *expr;
    MirType * rtti_type;
    MirType * rtti_data; // optional
    MirVar *  tmp;
    MirVar *  expr_tmp; // optional
};

struct MirInstrSwitch {
    MirInstr base;

    MirInstr *              value;
    MirInstrBlock *         default_block;
    TSmallArray_SwitchCase *cases;
    bool                    has_user_defined_default;
};

// public
static bool mir_is_pointer_type(const MirType *type)
{
    BL_ASSERT(type);
    return type->kind == MIR_TYPE_PTR;
}

static MirType *mir_deref_type(const MirType *ptr)
{
    if (!mir_is_pointer_type(ptr)) return NULL;
    return ptr->data.ptr.expr;
}

static bool mir_is_composit_type(const MirType *type)
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

static INLINE MirType *mir_get_struct_elem_type(const MirType *type, u32 i)
{
    BL_ASSERT(mir_is_composit_type(type) && "Expected structure type");
    TSmallArray_MemberPtr *members = type->data.strct.members;
    BL_ASSERT(members && members->size > i);

    return members->data[i]->type;
}

static INLINE MirType *mir_get_fn_arg_type(const MirType *type, u32 i)
{
    BL_ASSERT(type->kind == MIR_TYPE_FN && "Expected function type");
    TSmallArray_ArgPtr *args = type->data.fn.args;
    if (!args) return NULL;
    BL_ASSERT(args->size > i);

    return args->data[i]->type;
}

// Determinates if the instruction has compile time known value.
static INLINE bool mir_is_comptime(const MirInstr *instr)
{
    return instr->value.is_comptime;
}

static INLINE bool mir_is_global_block(const MirInstrBlock *instr)
{
    return instr->owner_fn == NULL;
}

// Determinates if the instruction is in the global block.
static INLINE bool mir_is_global(const MirInstr *instr)
{
    return mir_is_global_block(instr->owner_block);
}

static INLINE bool mir_type_has_llvm_representation(const MirType *type)
{
    BL_ASSERT(type);
    return type->kind != MIR_TYPE_TYPE && type->kind != MIR_TYPE_FN_GROUP;
}

void        mir_arenas_init(MirArenas *arenas);
void        mir_arenas_terminate(MirArenas *arenas);
void        mir_type_to_str(char *buf, usize len, const MirType *type, bool prefer_name);
const char *mir_instr_name(const MirInstr *instr);
void        mir_run(struct Assembly *assembly);

#if BL_DEBUG
VMStackPtr _mir_cev_read(MirConstExprValue *value);
#endif

#endif
