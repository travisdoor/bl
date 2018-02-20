//*****************************************************************************
// blc
//
// File:   llvm_jit_exec.h
// Author: Martin Dorazil
// Date:   14/02/2018
//
// Copyright 2017 Martin Dorazil
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

#ifndef BL_LLVM_JIT_EXEC_H
#define BL_LLVM_JIT_EXEC_H

#include <bobject/bobject.h>
#include "bl/pipeline/stage.h"

BO_BEGIN_DECLS

/* class LlvmJitExec declaration */
bo_decl_type_begin(LlvmJitExec, Stage)
  /* virtuals */
bo_end();

extern BO_EXPORT LlvmJitExec *
bl_llvm_jit_exec_new(bl_compile_group_e group);

BO_END_DECLS

#endif //BL_LLVM_JIT_EXEC_H
