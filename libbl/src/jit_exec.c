//************************************************************************************************
// blc
//
// File:   jit_exec.c
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
//************************************************************************************************

//#include <llvm-c/Core.h>
#include "stages_impl.h"
#include "bl/bldebug.h"

void
bl_jit_exec_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  char *llvm_error = NULL;
  if (LLVMCreateJITCompilerForModule(&assembly->llvm_runtime_engine, assembly->llvm_module, 0,
                                     &llvm_error) != 0)
    bl_abort("failed to create execution engine for compile-time module with error %s", llvm_error);

  bl_msg_log("\nRunning:");
  LLVMValueRef        llvm_fn = LLVMGetNamedFunction(assembly->llvm_module, "main");
  LLVMGenericValueRef result  = LLVMRunFunction(assembly->llvm_runtime_engine, llvm_fn, 0, NULL);
  int                 ires    = (int)LLVMGenericValueToInt(result, 0);

  if (ires != 0) {
    bl_builder_warning(builder, "executed unit return %i", ires);
  }
  bl_msg_log("");
}
