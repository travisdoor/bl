//*****************************************************************************
// bl
//
// File:   llvm_bc_writer.c
// Author: Martin Dorazil
// Date:   14.2.18
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

#include <llvm-c/BitWriter.h>
#include <string.h>
#include "stages_impl.h"
#include "assembly_impl.h"
#include "bl/bldebug.h"

bl_error_e
bl_llvm_bc_writer_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  bl_assert(assembly->llvm_module, "invalid llvm module");

  char *export_file = malloc(sizeof(char) * (strlen(assembly->name) + 4));
  strcpy(export_file, assembly->name);
  strcat(export_file, ".bc");
  if (LLVMWriteBitcodeToFile(assembly->llvm_module, export_file) != 0) {
    bl_builder_error(builder, "(llvm_bc_writer) Error writing bytecode to file " BL_YELLOW("'%s'"),
                     export_file);
    free(export_file);
    return BL_ERR_CANNOT_WRITE_BC;
  }

  bl_log("byte code written into " BL_GREEN("%s"), export_file);

  free(export_file);
  return BL_NO_ERR;
}
