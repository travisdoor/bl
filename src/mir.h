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

#include <bobject/containers/array.h>
#include "type.h"

struct Assembly;
struct Builder;
struct Arena;

typedef struct MirInstr MirInstr;
typedef struct MirBlock MirBlock;
typedef struct MirValue MirValue;

struct MirBlock
{
  BArray *  instructions;
  MirValue *ret_value;
};

struct MirValue
{
  Type *type;
};

typedef enum
{
  MIR_INSTR_INVALID,
  MIR_INSTR_CONSTANT
} MirInstrKind;

struct MirInstr
{
  MirInstrKind kind;

  union
  { } data; };

/* public */
void
mir_instr_arena_init(struct Arena *arena);

void
mir_block_arena_init(struct Arena *arena);

void
mir_value_arena_init(struct Arena *arena);

void
mir_run(struct Builder *builder, struct Assembly *assembly);

#endif
