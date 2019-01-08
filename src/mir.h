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

typedef void *MirFrameStackPtr;

typedef struct MirModule MirModule;
typedef struct MirType   MirType;
typedef struct MirVar    MirVar;
typedef struct MirFn     MirFn;
typedef struct MirValue  MirValue;

typedef struct MirInstr            MirInstr;
typedef struct MirInstrUnreachable MirInstrUnreachable;
typedef struct MirInstrBlock       MirInstrBlock;
typedef struct MirInstrDeclVar     MirInstrDeclVar;
typedef struct MirInstrConst       MirInstrConst;
typedef struct MirInstrLoad        MirInstrLoad;
typedef struct MirInstrStore       MirInstrStore;
typedef struct MirInstrRet         MirInstrRet;
typedef struct MirInstrBinop       MirInstrBinop;
typedef struct MirInstrUnop        MirInstrUnop;
typedef struct MirInstrFnProto     MirInstrFnProto;
typedef struct MirInstrTypeFn      MirInstrTypeFn;
typedef struct MirInstrCall        MirInstrCall;
typedef struct MirInstrDeclRef     MirInstrDeclRef;
typedef struct MirInstrAddrOf      MirInstrAddrOf;
typedef struct MirInstrCondBr      MirInstrCondBr;
typedef struct MirInstrBr          MirInstrBr;
typedef struct MirInstrArg         MirInstrArg;

typedef struct MirInstrTryInfer     MirInstrTryInfer;
typedef struct MirInstrValidateType MirInstrValidateType;

/* ALLOCATORS */
struct MirArenas
{
  Arena instr_arena;
  Arena type_arena;
  Arena var_arena;
  Arena fn_arena;
};

struct MirModule
{
  struct MirArenas  arenas;
  BArray *          globals;
  LLVMModuleRef     llvm_module;
  LLVMContextRef    llvm_cnt;
  LLVMTargetDataRef llvm_td;
};

/* FN */
struct MirFn
{
  Ast *        node;
  const char * name;
  MirType *    type;
  LLVMValueRef llvm_value;

  BArray *    arg_slots;
  DCpointer   extern_entry;
  bool        is_external;
  bool        is_test_case;
  const char *test_case_desc;

  /* pointer to the first block inside function body */
  MirInstrBlock *first_block;
  MirInstrBlock *last_block;
  int            block_count;
  int            instr_count;

  MirValue *exec_ret_value;
};

/* TYPE */
typedef enum
{
  MIR_TYPE_INVALID,
  MIR_TYPE_TYPE,
  MIR_TYPE_VOID,
  MIR_TYPE_INT,
  MIR_TYPE_FN,
  MIR_TYPE_PTR,
  MIR_TYPE_BOOL,
} MirTypeKind;

struct MirTypeInt
{
  int  bitcount;
  bool is_signed;
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

struct MirType
{
  MirTypeKind kind;
  const char *name;
  LLVMTypeRef llvm_type;
  size_t      size;
  unsigned    alignment;

  union
  {
    struct MirTypeInt integer;
    struct MirTypeFn  fn;
    struct MirTypePtr ptr;
  } data;
};

/* VALUE */
union MirValueData
{
  unsigned long long v_uint;
  long long          v_int;
  bool               v_bool;
  MirType *          v_type;
  MirValue *         v_ptr;
  MirFn *            v_fn;
  MirFrameStackPtr   v_stack_ptr;
};

struct MirValue
{
  union MirValueData data;
  MirType *          type;
  bool               is_stack_allocated;
};

/* VAR */
struct MirVar
{
  MirValue    value;
  const char *name;
};

/* INSTRUCTIONS */
typedef enum
{
  /* Invalid instructon */
  MIR_INSTR_INVALID,

  /* Atomic basic block without branching. */
  MIR_INSTR_BLOCK,

  /* Variable declaration. This instruction will create named variable of some type. Large variables
     can allocate memory on stack. */
  MIR_INSTR_DECL_VAR,

  /* Constant declaration. */
  MIR_INSTR_CONST,

  /* Produce dereferencing of some input value. */
  MIR_INSTR_LOAD,

  /* Store some value. */
  MIR_INSTR_STORE,

  /* Binary operation */
  MIR_INSTR_BINOP,

  /* Return terminator. */
  MIR_INSTR_RET,

  /* Function prototype. */
  MIR_INSTR_FN_PROTO,

  /* Function type. */
  MIR_INSTR_TYPE_FN,

  /* Function call */
  MIR_INSTR_CALL,

  /* Reference to some named value. */
  MIR_INSTR_DECL_REF,

  /* Unreachable terminatro. */
  MIR_INSTR_UNREACHABLE,

  /* Get address of some value. */
  MIR_INSTR_ADDR_OF,

  /* Conditional break. This instruction can produce jumps to other blocks based on some condition.
   */
  MIR_INSTR_COND_BR,

  /* Break. Simpy jump to some block. */
  MIR_INSTR_BR,

  /* Unary operation. */
  MIR_INSTR_UNOP,

  /* Reference to argument of current function. */
  MIR_INSTR_ARG,

  MIR_INSTR_VALIDATE_TYPE,
  MIR_INSTR_TRY_INFER,
} MirInstrKind;

struct MirInstr
{
  MirValue       value;
  MirInstrKind   kind;
  int            id;
  LLVMValueRef   llvm_value;
  Ast *          node;
  MirInstrBlock *owner_block;

  int  ref_count;
  bool analyzed;
  bool comptime;

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

struct MirInstrRet
{
  MirInstr base;

  MirInstr *value;
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

struct MirInstrCall
{
  MirInstr base;

  MirInstr *callee;
  BArray *  args;
};

struct MirInstrDeclRef
{
  MirInstr base;

  MirInstr *decl;
};

struct MirInstrAddrOf
{
  MirInstr base;

  MirInstr *target;
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

/* analyze helper instructions */
struct MirInstrValidateType
{
  MirInstr base;

  MirInstr *src;
};

struct MirInstrTryInfer
{
  MirInstr base;

  MirInstr *src;
  MirInstr *dest;
};

/* public */
void
mir_type_to_str(char *buf, int len, MirType *type);

const char *
mir_instr_name(MirInstr *instr);

MirModule *
mir_new_module(const char *name);

void
mir_delete_module(MirModule *module);

void
mir_run(struct Builder *builder, struct Assembly *assembly);

#endif
