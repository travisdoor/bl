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
#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include <dyncall.h>
#include <dyncall_callback.h>
#include <dynload.h>

#define MIR_SLICE_LEN_INDEX 0
#define MIR_SLICE_PTR_INDEX 1

struct Assembly;
struct Builder;
struct Unit;

typedef struct MirType       MirType;
typedef struct MirMember     MirMember;
typedef struct MirVariant    MirVariant;
typedef struct MirArg        MirArg;
typedef struct MirVar        MirVar;
typedef struct MirFn         MirFn;
typedef struct MirConstValue MirConstValue;
typedef struct MirConstPtr   MirConstPtr;

typedef struct MirInstr              MirInstr;
typedef struct MirInstrUnreachable   MirInstrUnreachable;
typedef struct MirInstrBlock         MirInstrBlock;
typedef struct MirInstrDeclVar       MirInstrDeclVar;
typedef struct MirInstrDeclMember    MirInstrDeclMember;
typedef struct MirInstrDeclVariant   MirInstrDeclVariant;
typedef struct MirInstrDeclArg       MirInstrDeclArg;
typedef struct MirInstrConst         MirInstrConst;
typedef struct MirInstrLoad          MirInstrLoad;
typedef struct MirInstrStore         MirInstrStore;
typedef struct MirInstrRet           MirInstrRet;
typedef struct MirInstrBinop         MirInstrBinop;
typedef struct MirInstrUnop          MirInstrUnop;
typedef struct MirInstrFnProto       MirInstrFnProto;
typedef struct MirInstrCall          MirInstrCall;
typedef struct MirInstrAddrOf        MirInstrAddrOf;
typedef struct MirInstrCondBr        MirInstrCondBr;
typedef struct MirInstrBr            MirInstrBr;
typedef struct MirInstrArg           MirInstrArg;
typedef struct MirInstrElemPtr       MirInstrElemPtr;
typedef struct MirInstrMemberPtr     MirInstrMemberPtr;
typedef struct MirInstrTypeFn        MirInstrTypeFn;
typedef struct MirInstrTypeStruct    MirInstrTypeStruct;
typedef struct MirInstrTypeArray     MirInstrTypeArray;
typedef struct MirInstrTypeSlice     MirInstrTypeSlice;
typedef struct MirInstrTypeVArgs     MirInstrTypeVArgs;
typedef struct MirInstrTypePtr       MirInstrTypePtr;
typedef struct MirInstrTypeEnum      MirInstrTypeEnum;
typedef struct MirInstrDeclRef       MirInstrDeclRef;
typedef struct MirInstrDeclDirectRef MirInstrDeclDirectRef;
typedef struct MirInstrCast          MirInstrCast;
typedef struct MirInstrSizeof        MirInstrSizeof;
typedef struct MirInstrAlignof       MirInstrAlignof;
typedef struct MirInstrCompound      MirInstrCompound;
typedef struct MirInstrVArgs         MirInstrVArgs;
typedef struct MirInstrTypeInfo      MirInstrTypeInfo;
typedef struct MirInstrTypeKind      MirInstrTypeKind;
typedef struct MirInstrPhi           MirInstrPhi;
typedef struct MirInstrToAny         MirInstrToAny;

typedef union MirConstValueData MirConstValueData;

typedef struct MirArenas {
	Arena instr;
	Arena type;
	Arena var;
	Arena fn;
	Arena member;
	Arena variant;
	Arena arg;
	Arena value;
} MirArenas;

typedef enum MirBuiltinIdKind {
	MIR_BUILTIN_ID_NONE = -1,

	MIR_BUILTIN_ID_TYPE_TYPE,
	MIR_BUILTIN_ID_TYPE_S8,
	MIR_BUILTIN_ID_TYPE_S16,
	MIR_BUILTIN_ID_TYPE_S32,
	MIR_BUILTIN_ID_TYPE_S64,
	MIR_BUILTIN_ID_TYPE_U8,
	MIR_BUILTIN_ID_TYPE_U16,
	MIR_BUILTIN_ID_TYPE_U32,
	MIR_BUILTIN_ID_TYPE_U64,
	MIR_BUILTIN_ID_TYPE_USIZE,
	MIR_BUILTIN_ID_TYPE_BOOL,
	MIR_BUILTIN_ID_TYPE_F32,
	MIR_BUILTIN_ID_TYPE_F64,
	MIR_BUILTIN_ID_TYPE_VOID,
	MIR_BUILTIN_ID_TYPE_STRING,

	MIR_BUILTIN_ID_NULL,
	MIR_BUILTIN_ID_MAIN,
	MIR_BUILTIN_ID_ARR_LEN,
	MIR_BUILTIN_ID_ARR_PTR,

	MIR_BUILTIN_ID_ANY,
	MIR_BUILTIN_ID_TYPE_KIND,
	MIR_BUILTIN_ID_TYPE_INFO,
	MIR_BUILTIN_ID_TYPE_INFO_TYPE,
	MIR_BUILTIN_ID_TYPE_INFO_VOID,
	MIR_BUILTIN_ID_TYPE_INFO_INT,
	MIR_BUILTIN_ID_TYPE_INFO_REAL,
	MIR_BUILTIN_ID_TYPE_INFO_FN,
	MIR_BUILTIN_ID_TYPE_INFO_PTR,
	MIR_BUILTIN_ID_TYPE_INFO_BOOL,
	MIR_BUILTIN_ID_TYPE_INFO_ARRAY,
	MIR_BUILTIN_ID_TYPE_INFO_STRUCT,
	MIR_BUILTIN_ID_TYPE_INFO_ENUM,
	MIR_BUILTIN_ID_TYPE_INFO_NULL,
	MIR_BUILTIN_ID_TYPE_INFO_STRING,
	MIR_BUILTIN_ID_TYPE_INFO_VARGS,
	MIR_BUILTIN_ID_TYPE_INFO_SLICE,
	MIR_BUILTIN_ID_TYPE_INFO_STRUCT_MEMBER,
	MIR_BUILTIN_ID_TYPE_INFO_ENUM_VARIANT,

	_MIR_BUILTIN_ID_COUNT,
} MirBuiltinIdKind;

typedef enum MirTypeKind {
	MIR_TYPE_INVALID = 0,
	MIR_TYPE_TYPE    = 1,
	MIR_TYPE_VOID    = 2,
	MIR_TYPE_INT     = 3,
	MIR_TYPE_REAL    = 4,
	MIR_TYPE_FN      = 5,
	MIR_TYPE_PTR     = 6,
	MIR_TYPE_BOOL    = 7,
	MIR_TYPE_ARRAY   = 8,
	MIR_TYPE_STRUCT  = 9,
	MIR_TYPE_ENUM    = 10,
	MIR_TYPE_NULL    = 11,
	MIR_TYPE_STRING  = 12,
	MIR_TYPE_VARGS   = 13,
	MIR_TYPE_SLICE   = 14,
} MirTypeKind;

typedef enum MirConstPtrKind {
	MIR_CP_UNKNOWN,
	MIR_CP_TYPE,
	MIR_CP_VALUE,
	MIR_CP_FN,
	MIR_CP_VAR,
	MIR_CP_STR,
	MIR_CP_STACK
} MirConstPtrKind;

typedef enum MirValueAddressMode {
	MIR_VAM_LVALUE,
	MIR_VAM_LVALUE_CONST,
	MIR_VAM_RVALUE,
} MirValueAddressMode;

/* External function arguments passing composit types by value needs special handling in IR. */
typedef enum LLVMExternArgStructGenerationMode {
	LLVM_EASGM_NONE,  /* No special handling */
	LLVM_EASGM_8,     /* Promote composit as i8 */
	LLVM_EASGM_16,    /* Promote composit as i16 */
	LLVM_EASGM_32,    /* Promote composit as i32 */
	LLVM_EASGM_64,    /* Promote composit as i64 */
	LLVM_EASGM_64_8,  /* Promote composit as i64, i8 */
	LLVM_EASGM_64_16, /* Promote composit as i64, i16 */
	LLVM_EASGM_64_32, /* Promote composit as i64, i32 */
	LLVM_EASGM_64_64, /* Promote composit as i64, i64 */
	LLVM_EASGM_BYVAL, /* Promote composit as byval */
} LLVMExternArgStructGenerationMode;

typedef enum MirInstrKind {
	MIR_INSTR_INVALID,
	MIR_INSTR_BLOCK,
	MIR_INSTR_DECL_VAR,
	MIR_INSTR_DECL_MEMBER,
	MIR_INSTR_DECL_VARIANT,
	MIR_INSTR_DECL_ARG,
	MIR_INSTR_CONST,
	MIR_INSTR_LOAD,
	MIR_INSTR_STORE,
	MIR_INSTR_BINOP,
	MIR_INSTR_RET,
	MIR_INSTR_FN_PROTO,
	MIR_INSTR_TYPE_FN,
	MIR_INSTR_TYPE_STRUCT,
	MIR_INSTR_TYPE_PTR,
	MIR_INSTR_TYPE_ARRAY,
	MIR_INSTR_TYPE_SLICE,
	MIR_INSTR_TYPE_VARGS,
	MIR_INSTR_TYPE_ENUM,
	MIR_INSTR_CALL,
	MIR_INSTR_DECL_REF,
	MIR_INSTR_DECL_DIRECT_REF,
	MIR_INSTR_UNREACHABLE,
	MIR_INSTR_COND_BR,
	MIR_INSTR_BR,
	MIR_INSTR_UNOP,
	MIR_INSTR_ARG,
	MIR_INSTR_ELEM_PTR,
	MIR_INSTR_MEMBER_PTR,
	MIR_INSTR_ADDROF,
	MIR_INSTR_CAST,
	MIR_INSTR_SIZEOF,
	MIR_INSTR_ALIGNOF,
	MIR_INSTR_COMPOUND,
	MIR_INSTR_VARGS,
	MIR_INSTR_TYPE_INFO,
	MIR_INSTR_PHI,
	MIR_INSTR_TOANY,
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
} MirCastOp;

typedef struct {
	VM *   vm;
	MirFn *fn;
} DyncallCBContext;

/* FN */
struct MirFn {
	/* Must be first!!! */
	MirInstr *prototype;
	ID *      id;
	Ast *     decl_node;

	/* function body scope if there is one (optional) */
	Scope *     body_scope;
	MirType *   type;
	BArray *    variables;
	const char *linkage_name;
	const char *linkage_orig_name;

	/* Valid only for external functions, implicit wrapper is needed in IR when true. */
	bool         llvm_extern_wrap;
	LLVMValueRef llvm_value;
	bool         fully_analyzed;
	bool         emit_llvm;
	bool         is_in_gscope;

	s32         flags;
	const char *test_case_desc;

	/* pointer to the first block inside function body */
	MirInstrBlock *first_block;
	MirInstrBlock *last_block;
	s32            block_count;

	/* Teporary variable used for return value. */
	MirInstr *ret_tmp;

	/* Return instruction of function. */
	MirInstrRet *terminal_instr;

	struct {
		DCpointer        extern_entry;
		DCCallback *     extern_callback_handle;
		DyncallCBContext context;
	} dyncall;
};

/* MEMBER */
struct MirMember {
	MirType *type;
	ID *     id;
	Ast *    decl_node;
	Scope *  decl_scope;
	s32      offset_bytes;
	s64      index;
};

/* FUNCTION ARGUMENT */
struct MirArg {
	MirType *type;
	ID *     id;
	Ast *    decl_node;
	Scope *  decl_scope;

	LLVMExternArgStructGenerationMode llvm_easgm;
};

/* TYPE */
struct MirTypeInt {
	s32  bitcount;
	bool is_signed;
};

struct MirTypeReal {
	s32 bitcount;
};

struct MirTypeFn {
	MirType *          ret_type;
	SmallArray_ArgPtr *args;
	bool               is_vargs;
};

struct MirTypePtr {
	MirType *expr;
};

struct MirTypeStruct {
	Scope *               scope; /* struct body scope */
	SmallArray_MemberPtr *members;
	bool                  is_packed;
};

/* Enum variants must be baked into enum type. */
struct MirTypeEnum {
	Scope *                scope;
	MirType *              base_type;
	SmallArray_VariantPtr *variants; /* MirVariant * */
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
	size_t          size_bits;
	size_t          store_size_bytes;
	s32             alignment;

	/*
	 * Every unique type will cause generation of type info global constant in program
	 * data segment.
	 */
	struct {
		MirVar *var;
	} rtti;

	union {
		struct MirTypeInt    integer;
		struct MirTypeFn     fn;
		struct MirTypePtr    ptr;
		struct MirTypeReal   real;
		struct MirTypeArray  array;
		struct MirTypeStruct strct;
		struct MirTypeEnum   enm;
		struct MirTypeNull   null;
	} data;
};

struct MirConstPtr {
	union {
		MirType *          type;          /* type value */
		MirConstValue *    value;         /* remove */
		MirFn *            fn;            /* function */
		MirVar *           var;           /* variable */
		VMStackPtr         stack_ptr;     /* absolute pointer to the stack */
		VMRelativeStackPtr rel_stack_ptr; /* relative pointer to the stack */
		const char *       str;           /* constant string array */

		void *any; /* universal pointer value */
	} data;

	MirConstPtrKind kind;
};

/* VALUE */
union MirConstValueData {
	/* atomic types */
	s64  v_s64;
	s32  v_s32;
	s16  v_s16;
	s8   v_s8;
	u64  v_u64;
	u32  v_u32;
	u16  v_u16;
	u8   v_u8;
	f32  v_f32;
	f64  v_f64;
	bool v_bool;
	char v_char;

	MirConstPtr v_ptr;

	struct {
		SmallArray_ConstValuePtr *members; // array of MirConstValues *
		bool                      is_zero_initializer;
	} v_struct;

	struct {
		SmallArray_ConstValuePtr *elems; // array of MirConstValues *
		bool                      is_zero_initializer;
	} v_array;
};

struct MirConstValue {
	union MirConstValueData data; /* data must be first!!! */
	MirType *               type;
	MirValueAddressMode     addr_mode;
};

/* VARIANT */
struct MirVariant {
	ID *           id;
	Scope *        decl_scope;
	MirConstValue *value;
};

/* VAR */
struct MirVar {
	MirConstValue      value; /* contains also allocated type */
	ID *               id;
	Ast *              decl_node;
	Scope *            decl_scope;
	s32                ref_count;
	bool               is_mutable;
	bool               comptime;
	bool               is_in_gscope;
	bool               is_implicit;
	bool               gen_llvm;
	u32                flags;
	VMRelativeStackPtr rel_stack_ptr;
	LLVMValueRef       llvm_value;
	const char *       llvm_name;
};

struct MirInstr {
	MirConstValue  value; // must be first
	MirInstrKind   kind;
	u64            id;
	Ast *          node;
	MirInstrBlock *owner_block;
	LLVMValueRef   llvm_value;

	s32  ref_count;
	bool analyzed;
	bool comptime;
	bool unrechable;

	MirInstr *prev;
	MirInstr *next;
};

struct MirInstrBlock {
	MirInstr base;

	const char *name;
	MirInstr *  entry_instr;
	MirInstr *  last_instr;
	MirInstr *  terminal;
	MirFn *     owner_fn;
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
};

struct MirInstrDeclVariant {
	MirInstr base;

	MirVariant *variant;
	MirInstr *  value; /* Optional. */
};

struct MirInstrDeclArg {
	MirInstr base;

	MirArg *  arg;
	MirInstr *type;
	bool      llvm_byval;
};

struct MirInstrElemPtr {
	MirInstr base;

	MirInstr *arr_ptr;
	MirInstr *index;
	bool      target_is_slice;
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
};

struct MirInstrLoad {
	MirInstr base;

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
	bool      infer_type;
};

struct MirInstrBinop {
	MirInstr base;

	BinopKind op;
	MirInstr *lhs;
	MirInstr *rhs;

	/* volatile type flag, if true, this instruction can change type during analyze pass, this
	 * is used for const integer literals like (123 * 123) */
	bool volatile_type;
};

struct MirInstrUnop {
	MirInstr base;

	UnopKind  op;
	MirInstr *expr;

	/* volatile type flag, if true, this instruction can change type during analyze pass, this
	 * is used for const integer literals like (-123) */
	bool volatile_type;
};

struct MirInstrFnProto {
	MirInstr base;

	MirInstr *       type;
	MirInstr *       user_type;
	struct Location *first_unrechable_location;
	bool             pushed_for_analyze;
};

struct MirInstrTypeFn {
	MirInstr base;

	MirInstr *           ret_type;
	SmallArray_InstrPtr *args;
};

struct MirInstrTypeStruct {
	MirInstr base;

	ID *                 id;
	Scope *              scope;
	SmallArray_InstrPtr *members;
	bool                 is_packed;
};

struct MirInstrTypeEnum {
	MirInstr base;

	ID *                 id;
	Scope *              scope;
	SmallArray_InstrPtr *variants;
	MirInstr *           base_type;
};

struct MirInstrTypePtr {
	MirInstr base;

	MirInstr *type;
};

struct MirInstrTypeArray {
	MirInstr base;

	MirInstr *elem_type;
	MirInstr *len;
};

struct MirInstrTypeSlice {
	MirInstr base;

	MirInstr *elem_type;
};

struct MirInstrTypeVArgs {
	MirInstr base;

	MirInstr *elem_type;
};

struct MirInstrCall {
	MirInstr base;

	MirInstr *           callee;
	SmallArray_InstrPtr *args;
};

struct MirInstrDeclRef {
	MirInstr base;

	struct Unit *parent_unit;
	ID *         rid;
	Scope *      scope;
	ScopeEntry * scope_entry;
};

struct MirInstrDeclDirectRef {
	MirInstr base;

	MirInstr *ref;
};

struct MirInstrUnreachable {
	MirInstr base;
};

struct MirInstrCondBr {
	MirInstr base;

	MirInstr *     cond;
	MirInstrBlock *then_block;
	MirInstrBlock *else_block;
};

struct MirInstrBr {
	MirInstr base;

	MirInstrBlock *then_block;
};

struct MirInstrCompound {
	MirInstr base;

	MirInstr *           type;
	SmallArray_InstrPtr *values;
	MirVar *             tmp_var;
	bool                 is_naked;
	bool                 is_zero_initialized;
};

struct MirInstrVArgs {
	MirInstr base;

	MirVar *             arr_tmp;
	MirVar *             vargs_tmp;
	MirType *            type;
	SmallArray_InstrPtr *values;
};

struct MirInstrTypeInfo {
	MirInstr base;

	/* pointer to the type of expression */
	MirType * expr_type;
	MirInstr *expr;
};

struct MirInstrTypeKind {
	MirInstr base;
};

struct MirInstrPhi {
	MirInstr base;

	SmallArray_InstrPtr *incoming_values;
	SmallArray_InstrPtr *incoming_blocks;
};

struct MirInstrToAny {
	MirInstr base;

	/* CLEANUP: We try to handle expressions, constants and types passed into ToAny instruction
	 * and maybe there is cleaner solution, but for now it works. */
	bool      has_data;
	MirType * rtti_type;
	MirType * rtti_type_specification; /* optional */
	MirVar *  tmp;
	MirVar *  expr_tmp; /* optional */
	MirInstr *expr;
};

/* public */
static inline bool
mir_is_pointer_type(MirType *type)
{
	BL_ASSERT(type);
	return type->kind == MIR_TYPE_PTR;
}

static inline MirType *
mir_deref_type(MirType *ptr)
{
	if (!mir_is_pointer_type(ptr)) return NULL;
	return ptr->data.ptr.expr;
}

static inline bool
mir_is_composit_type(MirType *type)
{
	return type->kind == MIR_TYPE_STRUCT || type->kind == MIR_TYPE_STRING ||
	       type->kind == MIR_TYPE_SLICE || type->kind == MIR_TYPE_VARGS;
}

static inline MirType *
mir_get_struct_elem_type(MirType *type, u32 i)
{
	BL_ASSERT(mir_is_composit_type(type) && "Expected structure type");
	SmallArray_MemberPtr *members = type->data.strct.members;
	BL_ASSERT(members && members->size > i);

	return members->data[i]->type;
}

static inline void
mir_set_const_ptr(MirConstPtr *value, void *ptr, MirConstPtrKind kind)
{
	value->data.any = ptr;
	value->kind     = kind;
}

ptrdiff_t
mir_get_struct_elem_offest(struct Assembly *assembly, MirType *type, u32 i);

ptrdiff_t
mir_get_array_elem_offset(MirType *type, u32 i);

static inline MirType *
mir_get_fn_arg_type(MirType *type, u32 i)
{
	BL_ASSERT(type->kind == MIR_TYPE_FN && "Expected function type");
	SmallArray_ArgPtr *args = type->data.fn.args;
	if (!args) return NULL;
	BL_ASSERT(args->size > i);

	return args->data[i]->type;
}

void
mir_arenas_init(MirArenas *arenas);

void
mir_arenas_terminate(MirArenas *arenas);

void
mir_type_to_str(char *buf, s32 len, MirType *type, bool prefer_name);

const char *
mir_instr_name(MirInstr *instr);

void
mir_run(struct Assembly *assembly);

#endif
