//*****************************************************************************
// blc
//
// File:   llvm_jit_exec.c
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

#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include "stages_impl.h"
#include "bl/bldebug.h"

bl_error_e
bl_llvm_jit_exec_run(bl_builder_t *builder,
                     bl_assembly_t *assembly)
{
  bl_assert(assembly->llvm_module, "invalid assembly module");
  LLVMExecutionEngineRef engine;
  char                   *error = NULL;

  LLVMLinkInInterpreter();
  if (LLVMCreateInterpreterForModule(&engine, assembly->llvm_module, &error) != 0) {
    bl_abort("failed to create execution engine with error %s", error);
  }

  LLVMValueRef main = LLVMGetNamedFunction(assembly->llvm_module, "main");
  if (main == NULL) {
    bl_builder_error(
      builder,
      assembly->name,
      "unable to get " BL_YELLOW("'main'") BL_RED(" method"));
    LLVMDisposeExecutionEngine(engine);
    return BL_ERR_NO_MAIN_METHOD;
  }

  LLVMGenericValueRef res = LLVMRunFunction(engine, main, 0, NULL);

  int ires = (int) LLVMGenericValueToInt(res, 0);
  if (ires != 0) {
    bl_builder_error(builder, assembly->name, "executed unit return %i", ires);
    LLVMDisposeExecutionEngine(engine);
    return BL_ERR_INVALID_RESULT;
  }

  return BL_NO_ERR;
}

