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
#include "bl/llvm_bc_writer.h"
#include "bl/unit.h"
#include "bl/bldebug.h"

/* class LlvmBcWriter */

static bool
run(LlvmBcWriter *self,
    Unit *unit);

/* class LlvmBcWriter constructor params */
bo_decl_params_with_base_begin(LlvmBcWriter, Stage)
  /* constructor params */
bo_end();

/* class LlvmBcWriter object members */
bo_decl_members_begin(LlvmBcWriter, Stage)
  /* members */
bo_end();

bo_impl_type(LlvmBcWriter, Stage);

void
LlvmBcWriterKlass_init(LlvmBcWriterKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run =
    (bool (*)(Stage *,
              Actor *)) run;
}

void
LlvmBcWriter_ctor(LlvmBcWriter *self,
                  LlvmBcWriterParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Stage, p);

  /* initialize self */
}

void
LlvmBcWriter_dtor(LlvmBcWriter *self)
{
}

bo_copy_result
LlvmBcWriter_copy(LlvmBcWriter *self,
                  LlvmBcWriter *other)
{
  return BO_NO_COPY;
}

/* class LlvmBcWriter end */

bool
run(LlvmBcWriter *self,
    Unit *unit)
{
  char *export_file = malloc(sizeof(char) * (strlen(bl_unit_get_src_file(unit)) + 4));
  strcpy(export_file, bl_unit_get_src_file(unit));
  strcat(export_file, ".bc");
  if (LLVMWriteBitcodeToFile(bl_unit_get_llvm_module(unit), export_file) != 0) {
    free(export_file);
    bl_actor_error((Actor *) unit,
                   "(llvm_bc_writer) Error writing bytecode to file " BL_YELLOW("'%s'"),
                   export_file);
    return false;
  }
  free(export_file);
  return true;
}

LlvmBcWriter *
bl_llvm_bc_writer_new(bl_compile_group_e group)
{
  LlvmBcWriterParams p = {.base.group = group};

  return bo_new(LlvmBcWriter, &p);
}
