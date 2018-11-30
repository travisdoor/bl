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

#include <llvm-c/Core.h>
#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include "arena.h"
#include "ast.h"

struct Assembly;
struct Builder;

typedef struct MirArenas MirArenas;
typedef struct MirBlock  MirBlock;
typedef struct MirExec   MirExec;
typedef struct MirType   MirType;
typedef struct MirVar    MirVar;
typedef struct MirFn     MirFn;
typedef struct MirValue  MirValue;

typedef struct MirInstr            MirInstr;
typedef struct MirInstrDeclVar     MirInstrDeclVar;
typedef struct MirInstrConst       MirInstrConst;
typedef struct MirInstrLoad        MirInstrLoad;
typedef struct MirInstrStore       MirInstrStore;
typedef struct MirInstrRet         MirInstrRet;
typedef struct MirInstrBinop       MirInstrBinop;
typedef struct MirInstrFnProto     MirInstrFnProto;
typedef struct MirInstrTypeFn      MirInstrTypeFn;
typedef struct MirInstrCall        MirInstrCall;
typedef struct MirInstrDeclRef     MirInstrDeclRef;
typedef struct MirInstrUnreachable MirInstrUnreachable;
typedef struct MirInstrAddrOf      MirInstrAddrOf;

typedef struct MirInstrTryInfer     MirInstrTryInfer;
typedef struct MirInstrValidateType MirInstrValidateType;

typedef struct MirDep MirDep;

/* ALLOCATORS */
struct MirArenas
{
  Arena instr_arena;
  Arena block_arena;
  Arena type_arena;
  Arena exec_arena;
  Arena var_arena;
  Arena fn_arena;
};

/* EXEC */
/* MirExec represents smallest atomic executable block of code it can be function body or floating
 * compile time executed block */
struct MirExec
{
  BArray *  blocks;
  MirBlock *entry_block;
  MirFn *   owner_fn;
};

/* BASIC BLOCK */
struct MirBlock
{
  const char *name;
  BArray *    instructions;
  MirInstr *  terminal;
  MirExec *   owner_exec;
};

/* VAR */
struct MirVar
{
  struct Ast *name;
  MirValue *  value;
};

/* FN */
struct MirFn
{
  MirType *type;
  MirExec *exec;
  MirExec *exec_analyzed;
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

  union
  {
    struct MirTypeInt integer;
    struct MirTypeFn  fn;
    struct MirTypePtr ptr;
  } data;
};

/* VALUE */
struct MirValue
{
  MirType *type;
  union
  {
    unsigned long long v_uint;
    long long          v_int;
    bool               v_bool;
    MirType *          v_type;
    MirFn *            v_fn;
    MirValue *         v_ptr;
  } data;
};

/* INSTRUCTIONS */
typedef enum
{
  MIR_INSTR_INVALID,
  MIR_INSTR_DECL_VAR,
  MIR_INSTR_CONST,
  MIR_INSTR_LOAD,
  MIR_INSTR_STORE,
  MIR_INSTR_BINOP,
  MIR_INSTR_RET,
  MIR_INSTR_FN_PROTO,
  MIR_INSTR_TYPE_FN,
  MIR_INSTR_CALL,
  MIR_INSTR_DECL_REF,
  MIR_INSTR_UNREACHABLE,
  MIR_INSTR_ADDR_OF,

  MIR_INSTR_VALIDATE_TYPE,
  MIR_INSTR_TRY_INFER,
} MirInstrKind;

typedef enum
{
  MIR_DEP_LAX,
  MIR_DEP_STRICT,
} MirDepKind;

struct MirDep
{
  MirDepKind kind;
  MirInstr * dep;
};

struct MirInstr
{
  MirValue     value;
  MirInstrKind kind;
  unsigned     id;
  LLVMValueRef llvm_value;
  Ast *        node;
  MirBlock *   owner_block;
  BHashTable * deps;

  int  ref_count;
  bool analyzed;
  bool comptime;
};

struct MirInstrDeclVar
{
  MirInstr base;

  MirVar *  var;
  MirInstr *type;
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

void
mir_arenas_init(MirArenas *arenas);

void
mir_arenas_terminate(MirArenas *arenas);

void
mir_run(struct Builder *builder, struct Assembly *assembly);

#endif
