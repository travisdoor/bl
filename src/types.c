//************************************************************************************************
// bl
//
// File:   types.c
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

#include "types.h"
#include "arena.h"

#define ARENA_CHUNK_COUNT 256

Type type_buildins[TYPE_BUILDIN_COUNT] = {
    // type
    {.code = TYPE_TYPE, .base = NULL, .name = "type"},

    // void
    {.code = TYPE_VOID, .base = &type_buildins[TYPE_BUILDIN_TYPE], .name = "void"},

    // u8
    {.code              = TYPE_INTEGER,
     .base              = &type_buildins[TYPE_BUILDIN_TYPE],
     .name              = "u8",
     .integer.bit_size  = 8,
     .integer.is_signed = false},

    // u16
    {.code              = TYPE_INTEGER,
     .base              = &type_buildins[TYPE_BUILDIN_TYPE],
     .name              = "u16",
     .integer.bit_size  = 16,
     .integer.is_signed = false},

    // u32
    {.code              = TYPE_INTEGER,
     .base              = &type_buildins[TYPE_BUILDIN_TYPE],
     .name              = "u32",
     .integer.bit_size  = 32,
     .integer.is_signed = false},

    // u64
    {.code              = TYPE_INTEGER,
     .base              = &type_buildins[TYPE_BUILDIN_TYPE],
     .name              = "u64",
     .integer.bit_size  = 64,
     .integer.is_signed = false},

    // s8
    {.code              = TYPE_INTEGER,
     .base              = &type_buildins[TYPE_BUILDIN_TYPE],
     .name              = "s8",
     .integer.bit_size  = 8,
     .integer.is_signed = true},

    // s16
    {.code              = TYPE_INTEGER,
     .base              = &type_buildins[TYPE_BUILDIN_TYPE],
     .name              = "s16",
     .integer.bit_size  = 16,
     .integer.is_signed = true},

    // s32
    {.code              = TYPE_INTEGER,
     .base              = &type_buildins[TYPE_BUILDIN_TYPE],
     .name              = "s32",
     .integer.bit_size  = 32,
     .integer.is_signed = true},

    // s64
    {.code              = TYPE_INTEGER,
     .base              = &type_buildins[TYPE_BUILDIN_TYPE],
     .name              = "s64",
     .integer.bit_size  = 64,
     .integer.is_signed = true},

    // usize
    {.code              = TYPE_INTEGER,
     .base              = &type_buildins[TYPE_BUILDIN_TYPE],
     .name              = "usize",
     .integer.bit_size  = 64,
     .integer.is_signed = true},

    // f32
    {.code          = TYPE_REAL,
     .base          = &type_buildins[TYPE_BUILDIN_TYPE],
     .name          = "f32",
     .real.bit_size = 32},

    // f64
    {.code          = TYPE_REAL,
     .base          = &type_buildins[TYPE_BUILDIN_TYPE],
     .name          = "f64",
     .real.bit_size = 64},
};

void
types_init(Arena *arena)
{
  arena_init(arena, sizeof(Type), ARENA_CHUNK_COUNT, NULL);
}

Type *
types_create_type(Arena *arena, TypeCode code)
{
  Type *type = arena_alloc(arena);
  type->code = code;
  return type;
}
