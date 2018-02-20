//*****************************************************************************
// blc
//
// File:   llvm_bl_cnt_impl.h
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

#ifndef BL_LLVM_BL_CNT_BUF_IMPL_H
#define BL_LLVM_BL_CNT_BUF_IMPL_H

#include <bobject/bobject.h>
#include <llvm-c/Core.h>
#include "ast/node_decl_impl.h"

BO_BEGIN_DECLS

/*
 * TODO: this class can be more abstract to be reused in parses also.
 */

/* class LlvmBlockContext declaration */
bo_decl_type_begin(LlvmBlockContext, BObject)
  /* virtuals */
bo_end();

LlvmBlockContext *
bl_llvm_block_context_new(void);

bool
bl_llvm_block_context_add(LlvmBlockContext *self,
                          LLVMValueRef val,
                          Ident *id);

LLVMValueRef
bl_llvm_block_context_get(LlvmBlockContext *self,
                          Ident *id);

void
bl_llvm_block_context_push_block(LlvmBlockContext *self);

void
bl_llvm_block_context_pop_block(LlvmBlockContext *self);

BO_END_DECLS

#endif //BL_LLVM_BL_CNT_BUF_IMPL_H
