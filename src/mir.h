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

struct Assembly;
struct Builder;

typedef struct MirArenas MirArenas;
typedef struct MirBlock  MirBlock;
typedef struct MirModule MirModule;
typedef struct MirType   MirType;

typedef struct MirInstr MirInstr;

/* ALLOCATORS */
struct MirArenas
{
  Arena instr_arena;
  Arena block_arena;
  Arena type_arena;
};

/* MODULE */
struct MirModule
{
  const char *name;
  BArray *    fns;
  BArray *    globals;
};

/* BASIC BLOCK */
struct MirBlock
{
  const char *name;
  BArray *    instructions;
};

/* TYPE */
typedef enum
{
  MIR_TYPE_INVALID,
  MIR_TYPE_TYPE,
  MIR_TYPE_INT,
  MIR_TYPE_FN,
} MirTypeKind;

struct MirTypeInt
{
  int  bitcount;
  bool is_signed;
};

struct MirTypeFn
{};

struct MirType
{
  MirTypeKind kind;
  const char *name;
  LLVMTypeRef llvm_type;

  union
  {
    struct MirTypeInt integer;
    struct MirTypeFn  fn;
  } data;
};

/* INSTRUCTIONS */
typedef enum
{
  MIR_INSTR_INVALID,
  MIR_INSTR_FN,
} MirInstrKind;

struct MirInstr
{
  MirInstrKind kind;
  unsigned     id;
  MirType *    type;
  LLVMValueRef llvm_value;
};

struct MirInstrFn
{
  MirInstr base;
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
