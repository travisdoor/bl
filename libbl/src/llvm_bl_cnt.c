//*****************************************************************************
// blc
//
// File:   llvm_bl_cnt.c
// Author: Martin Dorazil
// Date:   19/02/2018
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

#include <bobject/containers/htbl.h>
#include <bobject/containers/array.h>
#include "llvm_bl_cnt_impl.h"

void
bl_llvm_bl_cnt_init(bl_llvm_bl_cnt_t *bl_cnt)
{
  bl_cnt->blocks = bo_array_new(sizeof(LLVMValueRef));
}

void
bl_llvm_bl_cnt_terminate(bl_llvm_bl_cnt_t *bl_cnt)
{
  bo_unref(bl_cnt->blocks);
}

bool
bl_llvm_bl_cnt_add(bl_llvm_bl_cnt_t *bl_cnt, LLVMValueRef val, bl_ident_t *id)
{
  const size_t i = bo_array_size(bl_cnt->blocks);
  if (val == NULL || i == 0) {
    return false;
  }

  BHashTable *block = bo_array_at(bl_cnt->blocks, i - 1, BHashTable *);

  if (bo_htbl_has_key(block, id->hash)) {
    return false;
  }

  bo_htbl_insert(block, id->hash, val);
  return true;
}

LLVMValueRef
bl_llvm_bl_cnt_get(bl_llvm_bl_cnt_t *bl_cnt, bl_ident_t *id)
{
  const size_t c    = bo_array_size(bl_cnt->blocks);
  BHashTable *block = NULL;

  for (size_t i = 0; i < c; i++) {
    block = bo_array_at(bl_cnt->blocks, i, BHashTable *);
    if (bo_htbl_has_key(block, id->hash)) {
      return bo_htbl_at(block, id->hash, LLVMValueRef);
    }
  }

  return NULL;
}

void
bl_llvm_bl_cnt_push_block(bl_llvm_bl_cnt_t *bl_cnt)
{
  BHashTable *block = bo_htbl_new(sizeof(LLVMValueRef), 256);
  bo_array_push_back(bl_cnt->blocks, block);
}

void
bl_llvm_bl_cnt_pop_block(bl_llvm_bl_cnt_t *bl_cnt)
{
  bo_array_pop_back(bl_cnt->blocks);
}
