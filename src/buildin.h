//************************************************************************************************
// bl
//
// File:   buildin.h
// Author: Martin Dorazil
// Date:   3/14/18
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

#ifndef BL_BUILDIN_H
#define BL_BUILDIN_H

#include <bobject/containers/htbl.h>
#include <bobject/containers/hash.h>
#include "common.h"

struct Ast;
typedef struct Buildin Buildin;

typedef enum
{
  BUILDIN_U8,
  BUILDIN_U16,
  BUILDIN_U32,
  BUILDIN_U64,
  BUILDIN_USIZE,
  BUILDIN_S8,
  BUILDIN_S16,
  BUILDIN_S32,
  BUILDIN_S64,
  BUILDIN_COUNT,
} Buildins;

struct Buildin
{
  uint64_t    hashes[BUILDIN_COUNT];
  const char *names[BUILDIN_COUNT];
  BHashTable *table;
};

void
buildin_init(Buildin *buildin);

void
buildin_terminate(Buildin *buildin);

struct Ast *
buildin_get(Buildin *buildin, uint64_t hash);

#endif
