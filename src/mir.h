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

#include <dyncall.h>
#include <dynload.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include "arena.h"
#include "ast.h"

struct Assembly;
struct Builder;

typedef ptrdiff_t MirRelativeStackPtr;
typedef uint8_t * MirStackPtr;

typedef struct MirModule     MirModule;
typedef struct MirType       MirType;
typedef struct MirVar        MirVar;
typedef struct MirFn         MirFn;
typedef struct MirMember     MirMember;
typedef struct MirConstValue MirConstValue;

typedef struct MirInstr            MirInstr;
typedef struct MirInstrUnreachable MirInstrUnreachable;
typedef struct MirInstrBlock       MirInstrBlock;
typedef struct MirInstrDeclVar     MirInstrDeclVar;
typedef struct MirInstrDeclMember  MirInstrDeclMember;
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
typedef struct MirInstrTypePtr     MirInstrTypePtr;
typedef struct MirInstrDeclRef     MirInstrDeclRef;
typedef struct MirInstrCast        MirInstrCast;

typedef enum MirConstValueKind MirConstValueKind;
typedef enum MirTypeKind       MirTypeKind;
typedef enum MirInstrKind      MirInstrKind;
typedef enum MirCastOp         MirCastOp;
typedef enum MirBuiltinKind    MirBuiltinKind;

typedef union MirConstValueData MirConstValueData;

enum MirBuiltinKind
{
  MIR_BUILTIN_NONE = -1,

  MIR_BUILTIN_TYPE_TYPE,
  MIR_BUILTIN_TYPE_S8,
  MIR_BUILTIN_TYPE_S16,
  MIR_BUILTIN_TYPE_S32,
  MIR_BUILTIN_TYPE_S64,
  MIR_BUILTIN_TYPE_U8,
  MIR_BUILTIN_TYPE_U16,
  MIR_BUILTIN_TYPE_U32,
  MIR_BUILTIN_TYPE_U64,
  MIR_BUILTIN_TYPE_USIZE,
  MIR_BUILTIN_TYPE_BOOL,
  MIR_BUILTIN_TYPE_F32,
  MIR_BUILTIN_TYPE_F64,
  MIR_BUILTIN_TYPE_VOID,

  MIR_BUILTIN_NULL,
  MIR_BUILTIN_MAIN,
  MIR_BUILTIN_ARR_LEN,
  MIR_BUILTIN_ARR_PTR,

  _MIR_BUILTIN_COUNT,
};

/* ALLOCATORS */
struct MirArenas
{
  Arena instr_arena;
  Arena type_arena;
  Arena var_arena;
  Arena fn_arena;
  Arena member_arena;
};

struct MirModule
{
  struct MirArenas     arenas;
  BArray *             globals;
  LLVMModuleRef        llvm_module;
  LLVMContextRef       llvm_cnt;
  LLVMTargetDataRef    llvm_td;
  LLVMTargetMachineRef llvm_tm;
  char *               llvm_triple;
};

/* FN */
struct MirFn
{
  ID *         id;
  Ast *        decl_node;
  MirType *    type;
  Scope *      scope;
  BArray *     variables;
  int32_t      ref_count;
  const char * llvm_name;
  LLVMValueRef llvm_value;

  DCpointer   extern_entry;
  bool        is_external;
  bool        is_test_case;
  const char *test_case_desc;

  /* pointer to the first block inside function body */
  MirInstrBlock *first_block;
  MirInstrBlock *last_block;
  int32_t        block_count;
  int32_t        instr_count;

  MirConstValueData *exec_ret_value;
};

/* MEMBER */

struct MirMember
{
  ID *     id;
  Ast *    decl_node;
  MirType *type;
  Scope *  scope;
  int64_t  index;
};

/* TYPE */
enum MirTypeKind
{
  MIR_TYPE_INVALID,
  MIR_TYPE_TYPE,
  MIR_TYPE_VOID,
  MIR_TYPE_INT,
  MIR_TYPE_REAL,
  MIR_TYPE_FN,
  MIR_TYPE_PTR,
  MIR_TYPE_BOOL,
  MIR_TYPE_ARRAY,
  MIR_TYPE_SLICE,
  MIR_TYPE_STRUCT,
  MIR_TYPE_NULL,
};

struct MirTypeInt
{
  int32_t bitcount;
  bool    is_signed;
};

struct MirTypeReal
{
  int32_t bitcount;
};

struct MirTypeFn
{
  MirType *ret_type;
  BArray * arg_types;
};

struct MirTypePtr
{
  MirType *next;
};

struct MirTypeStruct
{
  Scope * scope;
  BArray *members;
  bool    is_packed;
  bool    is_slice;
};

struct MirTypeNull
{};

struct MirTypeArray
{
  MirType *elem_type;
  size_t   len;
};

struct MirType
{
  MirTypeKind kind;
  ID *        id;
  LLVMTypeRef llvm_type;
  size_t      size_bits;
  size_t      store_size_bytes;
  int32_t     alignment;

  union
  {
    struct MirTypeInt    integer;
    struct MirTypeFn     fn;
    struct MirTypePtr    ptr;
    struct MirTypeReal   real;
    struct MirTypeArray  array;
    struct MirTypeStruct strct;
    struct MirTypeNull   null;
  } data;
};

/* VALUE */
enum MirConstValueKind
{
  MIR_CV_INVALID,
  MIR_CV_BASIC,
  MIR_CV_POINTER,
  MIR_CV_STRING
};

union MirConstValueData
{
  uint64_t            v_uint;
  int64_t             v_int;
  float               v_float;
  double              v_double;
  bool                v_bool;
  const char *        v_str;
  MirType *           v_type;
  MirConstValue *     v_ptr;
  MirFn *             v_fn;
  void *              v_void_ptr;
  MirRelativeStackPtr v_rel_stack_ptr;
  MirStackPtr         v_stack_ptr;
};

struct MirConstValue
{
  union MirConstValueData data;
  MirType *               type;
  MirConstValueKind       kind;
};

/* VAR */
struct MirVar
{
  MirType *alloc_type;
  ID *     id;
  Ast *    decl_node;
  Scope *  scope;
  int32_t  ref_count;
  bool     is_mutable;
  bool     comptime;
  bool     gen_llvm;
  bool     is_in_gscope;

  MirConstValue *     value;
  LLVMValueRef        llvm_value;
  MirRelativeStackPtr rel_stack_ptr;
};

/* INSTRUCTIONS */
enum MirInstrKind
{
  MIR_INSTR_INVALID,
  MIR_INSTR_BLOCK,
  MIR_INSTR_DECL_VAR,
  MIR_INSTR_DECL_MEMBER,
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
};

struct MirInstr
{
  MirConstValue  const_value;
  MirInstrKind   kind;
  int32_t        id;
  LLVMValueRef   llvm_value;
  Ast *          node;
  MirInstrBlock *owner_block;

  int32_t ref_count;
  bool    analyzed;
  bool    comptime;

  MirInstr *prev;
  MirInstr *next;

#if BL_DEBUG
  uint64_t _serial;
#endif
};

struct MirInstrBlock
{
  MirInstr base;

  const char *name;
  MirInstr *  entry_instr;
  MirInstr *  last_instr;
  MirInstr *  terminal;
  MirFn *     owner_fn;
};

struct MirInstrDeclVar
{
  MirInstr base;

  MirVar *  var;
  MirInstr *type;
  MirInstr *init;
};

struct MirInstrDeclMember
{
  MirInstr base;

  MirMember *member;
  MirInstr * type;
};

struct MirInstrElemPtr
{
  MirInstr base;

  MirInstr *arr_ptr;
  MirInstr *index;
  bool      target_is_slice;
};

struct MirInstrMemberPtr
{
  MirInstr base;

  Ast *          member_ident;
  MirInstr *     target_ptr;
  ScopeEntry *   scope_entry;
  MirBuiltinKind builtin_id;
};

enum MirCastOp
{
  MIR_CAST_INVALID,
  MIR_CAST_BITCAST,
  MIR_CAST_SEXT,
  MIR_CAST_ZEXT,
  MIR_CAST_TRUNC,
  MIR_CAST_FPTOSI,
  MIR_CAST_FPTOUI,
  MIR_CAST_PTRTOINT,
  MIR_CAST_INTTOPTR,
};

struct MirInstrCast
{
  MirInstr base;

  MirCastOp op;
  MirInstr *type;
  MirInstr *next;
};

struct MirInstrArg
{
  MirInstr base;

  unsigned i;
};

struct MirInstrConst
{
  MirInstr base;
};

struct MirInstrLoad
{
  MirInstr base;

  MirInstr *src;
};

struct MirInstrStore
{
  MirInstr base;

  MirInstr *src;
  MirInstr *dest;
};

struct MirInstrAddrOf
{
  MirInstr base;

  MirInstr *src;
};

struct MirInstrRet
{
  MirInstr base;

  MirInstr *value;
  bool      allow_fn_ret_type_override;
};

struct MirInstrBinop
{
  MirInstr base;

  BinopKind op;
  MirInstr *lhs;
  MirInstr *rhs;
};

struct MirInstrUnop
{
  MirInstr base;

  UnopKind  op;
  MirInstr *instr;
};

struct MirInstrFnProto
{
  MirInstr base;

  MirInstr *type;
  MirInstr *user_type;
};

struct MirInstrTypeFn
{
  MirInstr base;

  MirInstr *ret_type;
  BArray *  arg_types;
};

struct MirInstrTypeStruct
{
  MirInstr base;

  Scope * scope;
  BArray *members;
  bool    is_packed;
};

struct MirInstrTypePtr
{
  MirInstr base;

  MirInstr *type;
};

struct MirInstrTypeArray
{
  MirInstr base;

  MirInstr *elem_type;
  MirInstr *len;
};

struct MirInstrTypeSlice
{
  MirInstr base;

  MirInstr *elem_type;
};

struct MirInstrCall
{
  MirInstr base;

  MirInstr *callee;
  BArray *  args;
};

struct MirInstrDeclRef
{
  MirInstr base;

  ID *        rid;
  Scope *     scope;
  ScopeEntry *scope_entry;
};

struct MirInstrUnreachable
{
  MirInstr base;
};

struct MirInstrCondBr
{
  MirInstr base;

  MirInstr *     cond;
  MirInstrBlock *then_block;
  MirInstrBlock *else_block;
};

struct MirInstrBr
{
  MirInstr base;

  MirInstrBlock *then_block;
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
  return type->kind == MIR_TYPE_STRUCT && type->data.strct.is_slice;
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