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
#include "arena.h"

#define _BLTYPE_KIND_LIST                                                                          \
  tk(INTEGER, Integer, integer, struct {                                                           \
    bool  is_signed;                                                                               \
    short bit_size;                                                                                \
  })

#define tk(code, Name, name, data) BLTYPE_##code,
typedef enum
{
  _BLTYPE_KIND_LIST BLTYPE_COUNT
} BlTypeCode;
#undef tk

#define tk(code, Name, name, data) typedef data BlType##Name;
_BLTYPE_KIND_LIST
#undef tk

typedef struct
{
  union
  {
#define tk(code, Name, name, data) BlType##Name name;
    _BLTYPE_KIND_LIST
#undef tk
  };

  BlTypeCode code;
} BlType;

/* buildin types */
extern BlType type_buildins[];

BlType *
types_create_type(struct Arena *arena, BlTypeCode code);

#endif
