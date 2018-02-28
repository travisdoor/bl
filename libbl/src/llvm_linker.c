//*****************************************************************************
// bl
//
// File:   llvm_linker.c
// Author: Martin Dorazil
// Date:   28/02/2018
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

#include <bl/llvm_linker.h>

#include <llvm-c/Linker.h>

#include "bl/assembly.h"
#include "bl/bldebug.h"

/* class LlvmLinker */

#define link_error(self, format, ...) \
  { \
    bl_actor_error((Actor *)self->assembly, (format), ##__VA_ARGS__); \
  }

static bool
run(LlvmLinker *self,
    Assembly *assembly);

static void
diag_handler(LLVMDiagnosticInfoRef info,
             LlvmLinker *self);

/* class LlvmLinker constructor params */
bo_decl_params_with_base_begin(LlvmLinker, Stage)
  /* constructor params */
bo_end();

/* class LlvmLinker object members */
bo_decl_members_begin(LlvmLinker, Stage)
  /* members */
  Assembly *assembly;
  LLVMContextRef cnt;
bo_end();

bo_impl_type(LlvmLinker, Stage);

void
LlvmLinkerKlass_init(LlvmLinkerKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run =
    (bool (*)(Stage *,
              Actor *)) run;
}

void
LlvmLinker_ctor(LlvmLinker *self,
                LlvmLinkerParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Stage, p);

  /* initialize self */
  self->cnt = LLVMContextCreate();
  LLVMContextSetDiagnosticHandler(self->cnt, (LLVMDiagnosticHandler) diag_handler, self);
}

void
LlvmLinker_dtor(LlvmLinker *self)
{
  LLVMContextDispose(self->cnt);
}

bo_copy_result
LlvmLinker_copy(LlvmLinker *self,
                LlvmLinker *other)
{
  return BO_NO_COPY;
}

/* class LlvmLinker end */
void
diag_handler(LLVMDiagnosticInfoRef info,
             LlvmLinker *self)
{
  char *msg = LLVMGetDiagInfoDescription(info);
  link_error(self, "linking failed with error: %s", msg);
  LLVMDisposeMessage(msg);
}

bool
run(LlvmLinker *self,
    Assembly *assembly)
{
  self->assembly = assembly;
  bl_log(BL_GREEN("linking assembly: %s"), bl_assembly_get_name(assembly));

  LLVMModuleRef
    dest_module = LLVMModuleCreateWithNameInContext(bl_assembly_get_name(assembly), self->cnt);

  bl_assembly_set_module(assembly, dest_module);

  const int c = bl_assembly_get_unit_count(assembly);
  Unit *unit = NULL;
  LLVMModuleRef module;
  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    module = bl_unit_get_module(unit);
    bl_unit_set_llvm_module(unit, NULL);
    if (LLVMLinkModules2(dest_module, module)) {
      return false;
    }
  }

  return true;
}

LlvmLinker *
bl_llvm_linker_new(bl_compile_group_e group)
{
  LlvmLinkerParams p = {.base.group = group};

  return bo_new(LlvmLinker, &p);
}

