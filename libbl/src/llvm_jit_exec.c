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
#include "bl/bldebug.h"
#include "bl/llvm_jit_exec.h"
#include "bl/unit.h"

/* class LlvmJitExec */

static bool
run(LlvmJitExec *self,
    Unit *unit);

bo_decl_params_with_base_begin(LlvmJitExec, Stage)
bo_end();

/* class LlvmJitExec object members */
bo_decl_members_begin(LlvmJitExec, Stage)
  /* members */
bo_end();

bo_impl_type(LlvmJitExec, Stage);

void
LlvmJitExecKlass_init(LlvmJitExecKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run =
    (bool (*)(Stage *,
              Actor *)) run;
}

void
LlvmJitExec_ctor(LlvmJitExec *self,
                 LlvmJitExecParams *p)
{
  /* constructor */

  /* initialize parent */
  bo_parent_ctor(Stage, p);

  /* initialize self */
}

void
LlvmJitExec_dtor(LlvmJitExec *self)
{
}

bo_copy_result
LlvmJitExec_copy(LlvmJitExec *self,
                 LlvmJitExec *other)
{
  return BO_NO_COPY;
}
/* class LlvmJitExec end */

bool
run(LlvmJitExec *self,
    Unit *unit)
{
  LLVMExecutionEngineRef engine;
  char *error = NULL;

  LLVMLinkInInterpreter();
  if (LLVMCreateInterpreterForModule(&engine, bl_unit_get_llvm_module(unit), &error) != 0) {
    bl_abort("failed to create execution engine with error %s", error);
  }

  LLVMValueRef main = LLVMGetNamedFunction(bl_unit_get_llvm_module(unit), "main");
  if (main == NULL) {
    bl_actor_error((Actor *) unit,
                   "(llvm_interpreter) Unable to get " BL_YELLOW("'main'") BL_RED(" method"));
    LLVMDisposeExecutionEngine(engine);
    return false;
  }

  LLVMGenericValueRef res = LLVMRunFunction(engine, main, 0, NULL);

  int ires = (int) LLVMGenericValueToInt(res, 0);
  if (ires != 0) {
    bl_actor_error((Actor *) unit, "(llvm_interpreter) Executed unit return %i", ires);
    LLVMDisposeExecutionEngine(engine);
    return false;
  }

  return true;
}

LlvmJitExec *
bl_llvm_jit_exec_new(bl_compile_group_e group)
{
  LlvmJitExecParams params = {
    .base.group = group
  };

  return bo_new(LlvmJitExec, &params);
}

