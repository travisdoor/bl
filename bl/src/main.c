//*****************************************************************************
// bl 
//
// File:   main.c
// Author: Martin Dorazil
// Date:   04/02/2018
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

#include <stdio.h>
#include "bl/bl.h"

#define ENABLE_TOKEN_PRINTER   0
#define ENABLE_AST_PRINTER     0

int main(int argc, char *argv[])
{
  puts("BL Compiler version 0.1.0\n");

  if (argc < 2) {
    bl_warning("nothing to do, no input files, sorry :(");
    return 1;
  }

  Pipeline *pipeline = bl_pipeline_new();
  Assembly *assembly = bl_assembly_new("main_assembly", pipeline);

  /* init actors */
  for (int i = 1; i < argc; i++) {
    Unit *unit = bl_unit_new_file(argv[i]);
    bl_assembly_add_unit(assembly, unit);
  }


  Stage *file_loader = (Stage *)bl_file_loader_new(BL_CGROUP_PRE_ANALYZE);
  bl_pipeline_add_stage(pipeline, file_loader);

  Stage *lexer = (Stage *)bl_lexer_new(BL_CGROUP_PRE_ANALYZE);
  bl_pipeline_add_stage(pipeline, lexer);

#if ENABLE_TOKEN_PRINTER
  Stage *token_printer = (Stage *)bl_token_printer_new(stdout);
  bl_pipeline_add_stage(pipeline, token_printer, BL_CGROUP_PRE_ANALYZE);
#endif

  Stage *parser = (Stage *)bl_parser_new(BL_CGROUP_PRE_ANALYZE);
  bl_pipeline_add_stage(pipeline, parser);

#if ENABLE_AST_PRINTER
  Stage *ast_printer = (Stage *)bl_ast_printer_new(stdout, BL_CGROUP_PRE_ANALYZE);
  bl_pipeline_add_stage(pipeline, ast_printer);
#endif

  Stage *analyzer = (Stage *)bl_analyzer_new(BL_CGROUP_ANALYZE);
  bl_pipeline_add_stage(pipeline, analyzer);

  Stage *llvm = (Stage *)bl_llvm_backend_new(BL_CGROUP_GENERATE);
  bl_pipeline_add_stage(pipeline, llvm);

  Stage *llvm_jit = (Stage *)bl_llvm_jit_exec_new(BL_CGROUP_GENERATE);
  bl_pipeline_add_stage(pipeline, llvm_jit);

  if (!bl_assembly_compile(assembly)) {
    Actor *failed = (Actor *) bl_assembly_get_failed(assembly);
    bl_error("%s", bl_actor_get_error(failed));
  } else 
    bl_log(ANSI_COLOR_GREEN "DONE" ANSI_COLOR_RESET);

  bo_unref(assembly);
  bo_unref(pipeline);

  return 0;
}

