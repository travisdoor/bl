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
#include "assembly.h"
#include "ast.h"
#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>

struct Assembly;
struct Builder;

typedef ptrdiff_t MirRelativeStackPtr;
typedef uint8_t * MirStackPtr;

typedef struct MirModule     MirModule;
typedef struct MirType       MirType;
typedef struct MirVar        MirVar;
typedef struct MirFn         MirFn;
typedef struct MirMember     MirMember;
typedef struct MirVariant    MirVariant;
typedef struct MirConstValue MirConstValue;

typedef struct MirInstr            MirInstr;
typedef struct MirInstrUnreachable MirInstrUnreachable;
typedef struct MirInstrBlock       MirInstrBlock;
typedef struct MirInstrDeclVar     MirInstrDeclVar;
typedef struct MirInstrDeclMember  MirInstrDeclMember;
typedef struct MirInstrDeclVariant MirInstrDeclVariant;
typedef struct MirInstrConst       MirInstrConst;
typedef struct MirInstrLoad        MirInstrLoad;
typedef struct MirInstrStore       MirInstrStore;
typedef struct MirInstrRet         MirInstrRet;
typedef struct MirInstrBinop       MirInstrBinop;
typedef struct MirInstrUnop        MirInstrUnop;
typedef struct MirInstrFnProto     MirInstrFnProto;
typedef struct MirInstrCall        MirInstrCall;
typedef struct MirInstrAddrOf      MirInstrAddrOf;
typedef struct MirInstrCondBr      MirInstrCondBr;
typedef struct MirInstrBr          MirInstrBr;
typedef struct MirInstrArg         MirInstrArg;
typedef struct MirInstrElemPtr     MirInstrElemPtr;
typedef struct MirInstrMemberPtr   MirInstrMemberPtr;
typedef struct MirInstrTypeFn      MirInstrTypeFn;
typedef struct MirInstrTypeStruct  MirInstrTypeStruct;
typedef struct MirInstrTypeArray   MirInstrTypeArray;
typedef struct MirInstrTypeSlice   MirInstrTypeSlice;
typedef struct MirInstrTypeVArgs   MirInstrTypeVArgs;
typedef struct MirInstrTypePtr     MirInstrTypePtr;
typedef struct MirInstrTypeEnum    MirInstrTypeEnum;
typedef struct MirInstrDeclRef     MirInstrDeclRef;
typedef struct MirInstrCast        MirInstrCast;
typedef struct MirInstrSizeof      MirInstrSizeof;
typedef struct MirInstrAlignof     MirInstrAlignof;
typedef struct MirInstrCompound    MirInstrCompound;
typedef struct MirInstrVArgs       MirInstrVArgs;
typedef struct MirInstrTypeInfo    MirInstrTypeInfo;
typedef struct MirInstrTypeKind    MirInstrTypeKind;
typedef struct MirInstrPhi         MirInstrPhi;

typedef enum MirTypeKind         MirTypeKind;
typedef enum MirInstrKind        MirInstrKind;
typedef enum MirCastOp           MirCastOp;
typedef enum MirBuiltinIdKind    MirBuiltinIdKind;
typedef enum MirTypeStructKind   MirTypeStructKind;
typedef enum MirValueAddressMode MirValueAddressMode;

typedef union MirConstValueData MirConstValueData;

enum MirBuiltinIdKind {
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

	MIR_BUILTIN_ID_TYPE_KIND,
	MIR_BUILTIN_ID_TYPE_INFO,
	MIR_BUILTIN_ID_TYPE_INFO_INT,

	_MIR_BUILTIN_ID_COUNT,
};

/* ALLOCATORS */
struct MirArenas {
	Arena instr_arena;
	Arena type_arena;
	Arena var_arena;
	Arena fn_arena;
	Arena member_arena;
	Arena variant_arena;
	Arena value_arena;
	Arena array_arena;
};

struct MirModule {
	struct MirArenas     arenas;
	BArray *             global_instrs;
	BArray *             global_vars;
	LLVMModuleRef        llvm_module;
	LLVMContextRef       llvm_cnt;
	LLVMTargetDataRef    llvm_td;
	LLVMTargetMachineRef llvm_tm;
	char *               llvm_triple;
};

/* FN */
struct MirFn {
	MirInstr *   prototype;
	ID *         id;
	Ast *        decl_node;
	MirType *    type;
	Scope *      scope;
	BArray *     variables;
	int32_t      ref_count;
	const char * llvm_name;
	LLVMValueRef llvm_value;
	bool         analyzed_for_cmptime_exec;

	DCpointer   extern_entry;
	int32_t     flags;
	const char *test_case_desc;

	/* pointer to the first block inside function body */
	MirInstrBlock *first_block;
	MirInstrBlock *last_block;
	int32_t        block_count;
	// int32_t        instr_count;

	MirConstValueData *exec_ret_value;
};

/* MEMBER */

struct MirMember {
	ID *     id;
	Ast *    decl_node;
	MirType *type;
	Scope *  scope;
	int64_t  index;
};

/* TYPE */
enum MirTypeKind {
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
};

struct MirTypeInt {
	int32_t bitcount;
	bool    is_signed;
};

struct MirTypeReal {
	int32_t bitcount;
};

struct MirTypeFn {
	MirType *ret_type;
	BArray * arg_types;
	bool     is_vargs;
};

struct MirTypePtr {
	MirType *next;
};

enum MirTypeStructKind {
	MIR_TS_NONE   = 0x0, // ordinary user structure
	MIR_TS_SLICE  = 0x1, // slice
	MIR_TS_STRING = 0x3, // string slice
	MIR_TS_VARGS  = 0x5, // vargs slice
};

struct MirTypeStruct {
	MirTypeStructKind kind;
	Scope *           scope;
	BArray *          members;
	bool              is_packed;
};

/* Enum variants must be baked into enum type. */
struct MirTypeEnum {
	Scope *  scope;
	MirType *base_type;
	BArray * variants; /* MirVariant * */
};

struct MirTypeNull {
	MirType *base_type;
};

struct MirTypeArray {
	MirType *elem_type;
	size_t   len;
};

struct MirType {
	MirTypeKind kind;
	ID *        user_id;
	ID          id;
	LLVMTypeRef llvm_type;
	size_t      size_bits;
	size_t      store_size_bytes;
	int32_t     alignment;

	/*
	 * Every unique type will cause generation of type info global constant in program data
	 * segment, here we store pointers to this allocation, one for interpreter and one for LLVM
	 * IR. This pointers can be returned by 'typeinfo(<T>)' operator in user source code. This
	 * is the way RTTI is implemented in bl.
	 */
	struct {
		MirVar *     exec_var;
		LLVMValueRef llvm_ptr;
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

/* VALUE */
union MirConstValueData {
	int64_t             v_s64;
	int32_t             v_s32;
	int16_t             v_s16;
	int8_t              v_s8;
	uint64_t            v_u64;
	uint32_t            v_u32;
	uint16_t            v_u16;
	uint8_t             v_u8;
	float               v_f32;
	double              v_f64;
	bool                v_bool;
	char                v_char;
	const char *        v_str;
	MirType *           v_type;
	MirConstValue *     v_ptr;
	MirFn *             v_fn;
	void *              v_void_ptr;
	MirRelativeStackPtr v_rel_stack_ptr;
	MirStackPtr         v_stack_ptr;

	struct {
		BArray *members; // array of MirConstValues *
		bool    is_zero_initializer;
	} v_struct;

	struct {
		BArray *elems; // array of MirConstValues *
		bool    is_zero_initializer;
	} v_array;
};

enum MirValueAddressMode {
	MIR_VAM_LVALUE,
	MIR_VAM_LVALUE_CONST,
	MIR_VAM_RVALUE,
};

struct MirConstValue {
	// data must be first!!!
	union MirConstValueData data;
	MirType *               type;
	MirValueAddressMode     addr_mode;
};

/* VARIANT */
struct MirVariant {
	ID *           id;
	Ast *          decl_node;
	Scope *        scope;
	MirConstValue *value;
};

/* VAR */
struct MirVar {
	MirType *alloc_type;
	ID *     id;
	Ast *    decl_node;
	Scope *  scope;
	int32_t  ref_count;
	bool     is_mutable;
	bool     comptime;
	bool     is_in_gscope;
	bool     is_implicit;
	bool     gen_llvm;
	uint32_t flags;

	MirConstValue *     value;
	LLVMValueRef        llvm_value;
	const char *        llvm_name;
	MirRelativeStackPtr rel_stack_ptr;
};

/* INSTRUCTIONS */
enum MirInstrKind {
	MIR_INSTR_INVALID,
	MIR_INSTR_BLOCK,
	MIR_INSTR_DECL_VAR,
	MIR_INSTR_DECL_MEMBER,
	MIR_INSTR_DECL_VARIANT,
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
	MIR_INSTR_TYPE_KIND,
	MIR_INSTR_PHI,
};

struct MirInstr {
	MirConstValue  const_value; // must be first
	MirInstrKind   kind;
	uint64_t       id;
	LLVMValueRef   llvm_value;
	Ast *          node;
	MirInstrBlock *owner_block;

	int32_t ref_count;
	bool    analyzed;
	bool    comptime;

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

enum MirCastOp {
	MIR_CAST_INVALID,
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
};

struct MirInstrCast {
	MirInstr base;

	MirCastOp op;
	MirInstr *type;
	MirInstr *next;
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
	bool      allow_fn_ret_type_override;
};

struct MirInstrBinop {
	MirInstr base;

	BinopKind op;
	MirInstr *lhs;
	MirInstr *rhs;
};

struct MirInstrUnop {
	MirInstr base;

	UnopKind  op;
	MirInstr *instr;
};

struct MirInstrFnProto {
	MirInstr base;

	MirInstr *type;
	MirInstr *user_type;
};

struct MirInstrTypeFn {
	MirInstr base;

	MirInstr *ret_type;
	BArray *  arg_types;
};

struct MirInstrTypeStruct {
	MirInstr base;

	ID *    id;
	Scope * scope;
	BArray *members;
	bool    is_packed;
};

struct MirInstrTypeEnum {
	MirInstr base;

	ID *      id;
	Scope *   scope;
	BArray *  variants;
	MirInstr *base_type;
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

	MirInstr *callee;
	BArray *  args;
};

struct MirInstrDeclRef {
	MirInstr base;

	ID *        rid;
	Scope *     scope;
	ScopeEntry *scope_entry;
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

	MirInstr *type;
	BArray *  values;
	MirVar *  tmp_var;
	bool      is_naked;
	bool      is_zero_initialized;
};

struct MirInstrVArgs {
	MirInstr base;

	MirVar * arr_tmp;
	MirVar * vargs_tmp;
	MirType *type;
	BArray * values;
};

struct MirInstrTypeInfo {
	MirInstr base;

	/* pointer to the type of expression */
	MirType * expr_type;
	MirInstr *expr;
};

struct MirInstrTypeKind {
	MirInstr base;

	MirInstr *expr;
};

struct MirInstrPhi {
	MirInstr base;

	BArray *incoming_values;
	BArray *incoming_blocks;
};

/* public */
static inline bool
mir_is_pointer_type(MirType *type)
{
	assert(type);
	return type->kind == MIR_TYPE_PTR;
}

static inline bool
mir_is_slice_type(MirType *type)
{
	assert(type);
	return type->kind == MIR_TYPE_STRUCT && (type->data.strct.kind & MIR_TS_SLICE);
}

static inline bool
mir_is_vargs_type(MirType *type)
{
	assert(type);
	return type->kind == MIR_TYPE_STRUCT && (type->data.strct.kind == MIR_TS_VARGS);
}

static inline bool
mir_is_string_type(MirType *type)
{
	assert(type);
	return type->kind == MIR_TYPE_STRUCT && (type->data.strct.kind == MIR_TS_STRING);
}

static inline MirType *
mir_deref_type(MirType *ptr)
{
	if (!mir_is_pointer_type(ptr)) return NULL;
	return ptr->data.ptr.next;
}

void
mir_type_to_str(char *buf, int32_t len, MirType *type, bool prefer_name);

const char *
mir_instr_name(MirInstr *instr);

MirModule *
mir_new_module(const char *name);

void
mir_delete_module(MirModule *module);

void
mir_run(struct Builder *builder, struct Assembly *assembly);

#endif
