//************************************************************************************************
// bl
//
// File:   linker.c
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
//************************************************************************************************

#include <llvm-c/Linker.h>
#include <llvm-c/TargetMachine.h>

#include "stages_impl.h"
#include "common_impl.h"
#include "bl/error.h"

#ifdef BL_PLATFORM_WIN
#define OBJ_EXT ".obj"
#else
#define OBJ_EXT ".o"
#endif


void
bl_linker_run(builder_t *builder, assembly_t *assembly)
{
  assert(assembly->llvm_module);
  char *filename = bl_malloc(sizeof(char) * (strlen(assembly->name) + strlen(OBJ_EXT) + 1));
  if (!filename) bl_abort("bad alloc");
  strcpy(filename, assembly->name);
  strcat(filename, OBJ_EXT);

  char *triple    = LLVMGetDefaultTargetTriple();
  char *cpu       = "";
  char *features  = "";
  char *error_msg = NULL;

  bl_msg_log("target: %s", triple);

  LLVMTargetRef target = NULL;
  if (LLVMGetTargetFromTriple(triple, &target, &error_msg)) {
    bl_msg_error("cannot get target with error: %s", error_msg);
    LLVMDisposeMessage(error_msg);
    bl_free(filename);
    return;
  }

#if BL_DEBUG
  LLVMCodeGenOptLevel opt_lvl = LLVMCodeGenLevelNone;
#else
  LLVMCodeGenOptLevel opt_lvl = LLVMCodeGenLevelAggressive;
#endif

  LLVMTargetMachineRef target_machine =
      LLVMCreateTargetMachine(target, triple, cpu, features, opt_lvl,
                              LLVMRelocDefault, LLVMCodeModelDefault);

  // TODO: use tmp file first (cause problems on windows)
  remove(filename);
  if (LLVMTargetMachineEmitToFile(target_machine, assembly->llvm_module, filename, LLVMObjectFile,
                                  &error_msg)) {
    bl_msg_error("cannot emit object file: %s with error: %s", filename, error_msg);

    LLVMDisposeMessage(error_msg);
    LLVMDisposeTargetMachine(target_machine);
    bl_free(filename);
    return;
  }

  LLVMDisposeTargetMachine(target_machine);
  bl_free(filename);
}
