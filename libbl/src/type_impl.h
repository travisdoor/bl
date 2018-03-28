//*****************************************************************************
// bl
//
// File:   type_impl.h
// Author: Martin Dorazil
// Date:   3/1/18
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
//*****************************************************************************

#ifndef BL_TYPE_IMPL_H
#define BL_TYPE_IMPL_H

#include <stdint.h>
#include <stdbool.h>

#define BL_TYPE_LIST                                                                               \
  tp(NONE, "") tp(VOID, "void") tp(CHAR, "char") tp(BOOL, "bool") tp(I8, "i8") tp(U8, "u8")        \
      tp(I32, "i32") tp(I64, "i64") tp(U32, "u32") tp(U64, "u64") tp(PTR, "ptr") tp(F32, "f32")    \
          tp(F64, "f64") tp(SIZE, "size") tp(STRING, "string")

typedef enum {
#define tp(tok, str) BL_TYPE_##tok,
  BL_TYPE_LIST
#undef tp
      BL_TYPE_COUNT
} bl_type_e;

typedef struct bl_type
{
  const char *name;
  uint32_t hash;
  struct bl_node *ref;
} bl_type_t;

void
bl_type_init(bl_type_t *type, const char *name);

bool
bl_type_is_fundamental(bl_type_t *type);

bool
bl_type_is_user_defined(bl_type_t *type);

#endif // BL_TYPE_IMPL_H
