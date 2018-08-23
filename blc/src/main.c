//*****************************************************************************
// blc
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
#include <locale.h>
#include <string.h>
#include "bl/bl.h"

int
main(int argc, char *argv[])
{
  setlocale(LC_ALL, "C");
  unsigned int build_flags = BL_BUILDER_LOAD_FROM_FILE;
  puts("compiler version: " BL_VERSION);

  size_t optind;
  for (optind = 1; optind < argc && argv[optind][0] == '-'; optind++) {
    if (strcmp(&argv[optind][1], "ast-dump") == 0) {
      build_flags |= BL_BUILDER_PRINT_AST;
    } else if (strcmp(&argv[optind][1], "lex-dump") == 0) {
      build_flags |= BL_BUILDER_PRINT_TOKENS;
    } else if (strcmp(&argv[optind][1], "syntax-only") == 0) {
      build_flags |= BL_BUILDER_SYNTAX_ONLY;
    } else if (strcmp(&argv[optind][1], "emit-llvm") == 0) {
      build_flags |= BL_BUILDER_EMIT_LLVM;
    } else if (strcmp(&argv[optind][1], "run") == 0) {
      build_flags |= BL_BUILDER_RUN;
    } else if (strcmp(&argv[optind][1], "run-tests") == 0) {
      build_flags |= BL_BUILDER_RUN_TESTS;
    } else if (strcmp(&argv[optind][1], "no-bin") == 0) {
      build_flags |= BL_BUILDER_NO_BIN;
    } else if (strcmp(&argv[optind][1], "no-warning") == 0) {
      build_flags |= BL_BUILDER_NO_WARN;
    } else {
      fprintf(stderr, "invalid params\n");
      exit(EXIT_FAILURE);
    }
  }
  argv += optind;

  if (*argv == NULL) {
    bl_msg_warning("nothing to do, no input files, sorry :(");
    exit(EXIT_SUCCESS);
  }

  bl_builder_ref builder = bl_builder_new();

  /*
   * HACK: use name of first file as assembly name
   */
  char *assembly_name = strrchr(*argv, BL_PATH_SEPARATORC);
  if (assembly_name == NULL) {
    assembly_name = *argv;
  } else {
    ++assembly_name;
  }

  assembly_name = strdup(assembly_name);
#ifdef BL_COMPILER_MSVC
  PathRemoveExtensionA(assembly_name);
#else
  char *ext = rindex(assembly_name, '.');
  if (ext != NULL) {
    (*ext) = '\0';
  }
#endif

  bl_assembly_ref assembly = bl_assembly_new(assembly_name);
  free(assembly_name);

  /* init actors */
  while (*argv != NULL) {
    bl_unit_ref unit  = bl_unit_new_file(*argv);

    bool        added = bl_assembly_add_unit_unique(assembly, unit);
    if (added == false) {
      bl_unit_delete(unit);
    }

    argv++;
  }

  int state = bl_builder_compile(builder, assembly, build_flags);
  if (state == BL_COMPILE_OK) {
    bl_msg_log(BL_GREEN("done"));
  } 

  bl_assembly_delete(assembly);
  bl_builder_delete(builder);

  return state;
}
