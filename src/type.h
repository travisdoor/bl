//************************************************************************************************
// bl
//
// File:   type.h
// Author: Martin Dorazil
// Date:   11/11/18
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

#ifndef BL_TYPE_H
#define BL_TYPE_H

#include <llvm-c/Core.h>
#include "common.h"

/* fwd decls */
struct Arena;

typedef struct Type Type;

typedef enum
{
  TYPE_INVALID,
  TYPE_NOVAL,
  TYPE_INT,
  TYPE_FN,
  TYPE_STRUCT
} TypeKind;

struct TypeInt
{
  int  bitcount;
  bool is_signed;
};

struct TypeFn
{};

struct TypeStruct
{};

struct Type
{
  TypeKind    kind;
  const char *name;
  LLVMTypeRef llvm_type;

  union
  {
    struct TypeInt    integer;
    struct TypeFn     fn;
    struct TypeStruct strct;
  } data;
};

void
type_arena_init(struct Arena *arena);

#define type_create(_arena, _kind, _type) ((_type)_type_create((_arena), (_kind)));

Type *
_type_create(struct Arena *arena, TypeKind kind);

void
type_to_str(char *buf, int len, Type *type);

#endif
