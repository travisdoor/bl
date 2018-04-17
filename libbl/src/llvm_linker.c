//************************************************************************************************
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
//************************************************************************************************

#include <llvm-c/Linker.h>
#include <llvm-c/TargetMachine.h>

#include "stages_impl.h"
#include "common_impl.h"
#include "bl/error.h"

bl_error_e
bl_llvm_linker_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  LLVMInitializeAllTargetInfos();
  LLVMInitializeAllTargets();
  LLVMInitializeAllTargetMCs();
  LLVMInitializeAllAsmParsers();
  LLVMInitializeAllAsmPrinters();

  char *filename = bl_malloc(sizeof(char) * (strlen(assembly->name) + 3));
  strcpy(filename, assembly->name);
  strcat(filename, ".o");

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
    return BL_ERR_CANNOT_LINK;
  }

  LLVMTargetMachineRef target_machine =
      LLVMCreateTargetMachine(target, triple, cpu, features, LLVMCodeGenLevelDefault,
                              LLVMRelocDefault, LLVMCodeModelDefault);

  if (LLVMTargetMachineEmitToFile(target_machine, assembly->llvm_module, filename, LLVMObjectFile,
                                  &error_msg)) {
    bl_msg_error("cannot emit object file: %s with error: %s", filename, error_msg);

    LLVMDisposeMessage(error_msg);
    LLVMDisposeTargetMachine(target_machine);
    bl_free(filename);
    return BL_ERR_CANNOT_LINK;
  }

  LLVMDisposeTargetMachine(target_machine);
  bl_free(filename);

  return BL_NO_ERR;
}
