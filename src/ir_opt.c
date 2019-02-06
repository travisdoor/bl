//************************************************************************************************
// bl
//
// File:   ir_opt.c
// Author: Martin Dorazil
// Date:   5.2.19
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

#include <llvm-c/Analysis.h>
#include <llvm-c/Transforms/Vectorize.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>
#include "stages.h"
#include "assembly.h"
#include "bldebug.h"
#include "error.h"

void
ir_opt_run(Builder *builder, Assembly *assembly)
{
  /* TODO: set by user!!! */
#if BL_DEBUG
  const unsigned opt_lvl = 0;
#else
  const unsigned opt_lvl = 3;
#endif

  LLVMModuleRef llvm_module = assembly->mir_module->llvm_module;

  LLVMPassManagerBuilderRef llvm_pm_builder = LLVMPassManagerBuilderCreate();
  LLVMPassManagerBuilderSetOptLevel(llvm_pm_builder, opt_lvl);


  LLVMPassManagerRef llvm_pm = LLVMCreatePassManager();
  LLVMPassManagerBuilderPopulateModulePassManager(llvm_pm_builder, llvm_pm);
  LLVMRunPassManager(llvm_pm, llvm_module);

  LLVMDisposePassManager(llvm_pm);
  LLVMPassManagerBuilderDispose(llvm_pm_builder);
}
