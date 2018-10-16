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

#define EXPECTED_TYPE_COUNT 4096
#define ARENA_CHUNK_SIZE 512

BlType type_buildins[8] = {
    {.code = BLTYPE_INTEGER, .integer.bit_size = 8, .integer.is_signed = false},
    {.code = BLTYPE_INTEGER, .integer.bit_size = 16, .integer.is_signed = false},
    {.code = BLTYPE_INTEGER, .integer.bit_size = 32, .integer.is_signed = false},
    {.code = BLTYPE_INTEGER, .integer.bit_size = 64, .integer.is_signed = false},
    {.code = BLTYPE_INTEGER, .integer.bit_size = 8, .integer.is_signed = true},
    {.code = BLTYPE_INTEGER, .integer.bit_size = 16, .integer.is_signed = true},
    {.code = BLTYPE_INTEGER, .integer.bit_size = 32, .integer.is_signed = true},
    {.code = BLTYPE_INTEGER, .integer.bit_size = 64, .integer.is_signed = true},
};

BlType *
types_create_type(struct Arena *arena, BlTypeCode code)
{
  BlType *type = arena_alloc(arena);
  type->code = code;
  return type;
}
