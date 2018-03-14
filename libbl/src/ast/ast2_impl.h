//*****************************************************************************
// bl
//
// File:   ast2_impl.h
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
//*****************************************************************************

#ifndef BL_NODE2_IMPL_H
#define BL_NODE2_IMPL_H

#include <bobject/containers/array.h>
#include "id_impl.h"

typedef struct {
  int line;
  int col;
  const char *file;
} bl_src_t;

typedef struct
{
  BArray *items;
} bl_module_t;

typedef struct
{
  BArray *params;
  // TODO: return type
} bl_func_decl_t;

typedef struct
{
  BArray *stmts;
} bl_block_t;

typedef enum
{
  BL_ITEM_MODULE,
  BL_ITEM_FUNC
} bl_item_e;

typedef struct
{
  bl_src_t src;
  bl_id_t id;
  bl_item_e t;
  union
  {
    bl_module_t module;

    struct
    {
      bl_func_decl_t func_decl;
      bl_block_t block;
    } func;

  } node;
} bl_item_t;

#endif //BL_NODE2_IMPL_H
