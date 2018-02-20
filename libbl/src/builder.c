//*****************************************************************************
// blc
//
// File:   builder.c
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

#include "bl/builder.h"
#include "bl/file_loader.h"
#include "bl/lexer.h"
#include "bl/parser.h"
#include "bl/analyzer.h"
#include "bl/llvm_backend.h"
#include "bl/llvm_jit_exec.h"
#include "bl/token_printer.h"
#include "bl/ast_printer.h"
#include "bl/llvm_bc_writer.h"
#include "bl/bldebug.h"

/* class Builder */

static bool
compile_group(Builder *self,
              Assembly *assembly,
              bl_compile_group_e group);

/* class Builder constructor params */
bo_decl_params_begin(Builder)
  /* constructor params */
  unsigned int flags;
  Pipeline *pipeline;
bo_end();

/* class Builder object members */
bo_decl_members_begin(Builder, BObject)
  /* members */
  Pipeline *pipeline;
  Actor *failed;
bo_end();

bo_impl_type(Builder, BObject);

void
BuilderKlass_init(BuilderKlass *klass)
{
}

void
Builder_ctor(Builder *self,
             BuilderParams *p)
{
  /* constructor */
  /* initialize self */
  if (p->pipeline != NULL) {
    self->pipeline = bo_ref(p->pipeline);
  } else {
    self->pipeline = bl_pipeline_new();

    if (p->flags & BL_BUILDER_LOAD_FROM_FILE) {
      Stage *file_loader = (Stage *) bl_file_loader_new(BL_CGROUP_PRE_ANALYZE);
      bl_pipeline_add_stage(self->pipeline, file_loader);
    }

    Stage *lexer = (Stage *) bl_lexer_new(BL_CGROUP_PRE_ANALYZE);
    bl_pipeline_add_stage(self->pipeline, lexer);

    if (p->flags & BL_BUILDER_PRINT_TOKENS) {
      Stage *token_printer = (Stage *) bl_token_printer_new(stdout, BL_CGROUP_PRE_ANALYZE);
      bl_pipeline_add_stage(self->pipeline, token_printer);
    }

    Stage *parser = (Stage *) bl_parser_new(BL_CGROUP_PRE_ANALYZE);
    bl_pipeline_add_stage(self->pipeline, parser);

    if (p->flags & BL_BUILDER_PRINT_AST) {
      Stage *ast_printer = (Stage *) bl_ast_printer_new(stdout, BL_CGROUP_PRE_ANALYZE);
      bl_pipeline_add_stage(self->pipeline, ast_printer);
    }

    Stage *analyzer = (Stage *) bl_analyzer_new(BL_CGROUP_ANALYZE);
    bl_pipeline_add_stage(self->pipeline, analyzer);

    Stage *llvm = (Stage *) bl_llvm_backend_new(BL_CGROUP_GENERATE);
    bl_pipeline_add_stage(self->pipeline, llvm);

    if (p->flags & BL_BUILDER_RUN) {
      Stage *llvm_jit = (Stage *) bl_llvm_jit_exec_new(BL_CGROUP_POST_GENERATE);
      bl_pipeline_add_stage(self->pipeline, llvm_jit);
    }

    if (p->flags & BL_BUILDER_EXPORT_BC) {
      Stage *llvm_bc_writer = (Stage *) bl_llvm_bc_writer_new(BL_CGROUP_POST_GENERATE);
      bl_pipeline_add_stage(self->pipeline, llvm_bc_writer);
    }
  }
}

void
Builder_dtor(Builder *self)
{
  bo_unref(self->pipeline);
}

bo_copy_result
Builder_copy(Builder *self,
             Builder *other)
{
  return BO_NO_COPY;
}

/* class Builder end */

bool
compile_group(Builder *self,
              Assembly *assembly,
              bl_compile_group_e group)
{
  const int c = bl_assembly_get_unit_count(assembly);
  Unit *unit = NULL;
  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    if (!bl_pipeline_run(self->pipeline, (Actor *) unit, group)) {
      self->failed = bl_pipeline_get_failed(self->pipeline);
      return false;
    }
  }

  return true;
}

Builder *
bl_builder_new(unsigned int flags)
{
  BuilderParams p = {.flags = flags, .pipeline = NULL};
  return bo_new(Builder, &p);
}

Builder *
bl_builder_new_custom(Pipeline *pipeline)
{
  BuilderParams p = {.flags = 0, .pipeline = pipeline};
  return bo_new(Builder, &p);
}

bool
bl_builder_compile(Builder *self,
                   Assembly *assembly)
{
  self->failed = NULL;

  for (int i = 0; i < BL_CGROUP_COUNT; i++) {
    if (!compile_group(self, assembly, i))
      return false;
  }

  return true;
}

Actor *
bl_builder_get_failed(Builder *self)
{
  return self->failed;
}
