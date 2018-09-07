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
#include "stages.h"
#include "bldebug.h"

void
jit_exec_run(builder_t *builder, assembly_t *assembly)
{
  LLVMExecutionEngineRef jit;
  char *                 llvm_error = NULL;

  if (LLVMCreateJITCompilerForModule(&jit, assembly->llvm_module, 3, &llvm_error) != 0)
    bl_abort("failed to create execution engine for compile-time module with error %s", llvm_error);

  msg_log("\nRunning:");
  LLVMValueRef        llvm_fn = LLVMGetNamedFunction(assembly->llvm_module, "main");
  LLVMGenericValueRef result  = LLVMRunFunction(jit, llvm_fn, 0, NULL);
  int                 ires    = (int)LLVMGenericValueToInt(result, 0);

  if (ires != 0) {
    builder_warning(builder, "executed unit return %i", ires);
  }
  msg_log("");

  assembly->llvm_run_engine = jit;
}
