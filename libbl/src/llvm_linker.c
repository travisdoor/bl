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

#include <llvm-c/Linker.h>

#include "stages_impl.h"
#include "bl/bldebug.h"

int
bl_llvm_linker_run(bl_builder_t *builder,
                   bl_assembly_t *assembly)
{
  bl_log("linking assembly: "
           BL_GREEN("%s"), assembly->name);

  LLVMContextRef llvm_cnt    = LLVMContextCreate();
  LLVMModuleRef  dest_module = LLVMModuleCreateWithNameInContext(assembly->name, llvm_cnt);

  const int     c     = bl_assembly_get_unit_count(assembly);
  bl_unit_t     *unit = NULL;
  LLVMModuleRef module;
  for (int      i     = 0; i < c; i++) {
    unit   = bl_assembly_get_unit(assembly, i);
    module = unit->llvm_module;
    unit->llvm_module = NULL;
    if (LLVMLinkModules2(dest_module, module)) {
      LLVMDisposeModule(dest_module);
      LLVMContextDispose(llvm_cnt);
      return false;
    }
  }

  assembly->llvm_cnt    = llvm_cnt;
  assembly->llvm_module = dest_module;
  return true;
}
