//*****************************************************************************
// bl
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
#include "bl/bllimits.h"
#include "llvm_bl_cnt_impl.h"

/* class LlvmBlockContext */

/* class LlvmBlockContext constructor params */
bo_decl_params_begin(LlvmBlockContext)
  /* constructor params */
bo_end();

/* class LlvmBlockContext object members */
bo_decl_members_begin(LlvmBlockContext, BObject)
  /* members */
  BArray *blocks;
bo_end();

bo_impl_type(LlvmBlockContext, BObject);

void
LlvmBlockContextKlass_init(LlvmBlockContextKlass *klass)
{
}

void
LlvmBlockContext_ctor(LlvmBlockContext *self,
                      LlvmBlockContextParams *p)
{
  /* constructor */
  /* initialize self */
  self->blocks = bo_array_new_bo(bo_typeof(BHashTable), true);
}

void
LlvmBlockContext_dtor(LlvmBlockContext *self)
{
  bo_unref(self->blocks);
}

bo_copy_result
LlvmBlockContext_copy(LlvmBlockContext *self,
                      LlvmBlockContext *other)
{
  return BO_NO_COPY;
}

/* class LlvmBlockContext end */

LlvmBlockContext *
bl_llvm_block_context_new(void)
{
  LlvmBlockContextParams p = {};
  return bo_new(LlvmBlockContext, &p);
}

bool
bl_llvm_block_context_add(LlvmBlockContext *self,
                          LLVMValueRef val,
                          Ident *id)
{
  const int i = (const int) bo_array_size(self->blocks);
  if (val == NULL || i == 0) {
    return false;
  }

  const uint32_t hash = bl_ident_get_hash(id);
  BHashTable *block = bo_array_at(self->blocks, i - 1, BHashTable *);

  if (bo_htbl_has_key(block, hash)) {
    return false;
  }

  bo_htbl_insert(block, hash, val);
  return true;
}

LLVMValueRef
bl_llvm_block_context_get(LlvmBlockContext *self,
                          Ident *id)
{
  const int c = bo_array_size(self->blocks);
  const uint32_t hash = bl_ident_get_hash(id);
  BHashTable *block = NULL;

  for (int i = 0; i < c; i++) {
    block = bo_array_at(self->blocks, i, BHashTable *);
    if (bo_htbl_has_key(block, hash)) {
      return bo_htbl_at(block, hash, LLVMValueRef);
    }
  }

  return NULL;
}

void
bl_llvm_block_context_push_block(LlvmBlockContext *self)
{
  BHashTable *block = bo_htbl_new(sizeof(LLVMValueRef), 256);
  bo_array_push_back(self->blocks, block);
}

void
bl_llvm_block_context_pop_block(LlvmBlockContext *self)
{
  bo_array_pop_back(self->blocks);
}
