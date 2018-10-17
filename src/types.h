//************************************************************************************************
// bl
//
// File:   types.h
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

#ifndef BL_TYPES_H
#define BL_TYPES_H

#include <bobject/containers/htbl.h>
#include "common.h"

struct Arena;
struct Scope;

typedef enum
{
  TYPE_TYPE,
  TYPE_VOID,
  TYPE_INTEGER,
  TYPE_REAL,
  TYPE_BOOL,
  TYPE_STRING,
  TYPE_CHAR,
  TYPE_COUNT,
} TypeCode;

typedef enum
{
  TYPE_BUILDIN_TYPE,
  TYPE_BUILDIN_VOID,
  TYPE_BUILDIN_U8,
  TYPE_BUILDIN_U16,
  TYPE_BUILDIN_U32,
  TYPE_BUILDIN_U64,
  TYPE_BUILDIN_S8,
  TYPE_BUILDIN_S16,
  TYPE_BUILDIN_S32,
  TYPE_BUILDIN_S64,
  TYPE_BUILDIN_USIZE,
  TYPE_BUILDIN_F32,
  TYPE_BUILDIN_F64,
  TYPE_BUILDIN_COUNT,
} TypeBuildin;

struct TypeType
{
  void *_;
};

struct TypeVoid
{
  void *_;
};

struct TypeInteger
{
  bool  is_signed;
  short bit_size;
};

struct TypeReal
{
  short bit_size;
};

struct TypeBool
{
  void *_;
};

struct TypeString
{
  void *_;
};

struct TypeChar
{
  void *_;
};

typedef struct Type        Type;
typedef struct TypeType    TypeType;
typedef struct TypeVoid    TypeVoid;
typedef struct TypeInteger TypeInteger;
typedef struct TypeReal    TypeReal;
typedef struct TypeBool    TypeBool;
typedef struct TypeString  TypeString;
typedef struct TypeChar    TypeChar;

struct Type
{
  union
  {
    TypeType    type;
    TypeVoid    void_;
    TypeInteger integer;
    TypeReal    real;
    TypeBool    boolean;
    TypeString  string;
    TypeChar    character;
  };

  Type *      base;
  TypeCode    code;
  const char *name;
};
// clang-format on

/* buildin types */
extern Type types_buildins[];

void
types_init(struct Arena *arena);

void
types_add_builinds(struct Scope *scope, struct Arena *scope_entry_arena);

Type *
types_create_type(struct Arena *arena, TypeCode code);

/* peek helpers */
#define _TYPE_PEEK(_name, _code, _type)                                                            \
  static inline _type *types_peek_##_name(Type *t)                                                 \
  {                                                                                                \
    assert(t->code == _code);                                                                      \
    return (_type *)t;                                                                             \
  }

_TYPE_PEEK(peek_type, TYPE_TYPE, TypeType)
_TYPE_PEEK(peek_void, TYPE_VOID, TypeVoid)
_TYPE_PEEK(peek_integer, TYPE_INTEGER, TypeInteger)
_TYPE_PEEK(peek_real, TYPE_REAL, TypeReal)
_TYPE_PEEK(peek_bool, TYPE_BOOL, TypeBool)
_TYPE_PEEK(peek_string, TYPE_STRING, TypeString)
_TYPE_PEEK(peek_char, TYPE_CHAR, TypeChar)

#undef _TYPE_PEEK

#endif
