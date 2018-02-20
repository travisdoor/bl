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
#include "bl/bl.h"

int
main(int argc,
     char *argv[])
{
  setlocale(LC_ALL, "C");
  unsigned int build_flag = BL_BUILDER_EXPORT_BC | BL_BUILDER_LOAD_FROM_FILE;
  puts("BL Compiler version 0.1.0\n");

  bool isCaseInsensitive = false;
  size_t optind;
  for (optind = 1; optind < argc && argv[optind][0] == '-'; optind++) {
    switch (argv[optind][1]) {
      case 'r':
        build_flag |= BL_BUILDER_RUN;
        break;
      case 'v':
        build_flag |= BL_BUILDER_PRINT_TOKENS | BL_BUILDER_PRINT_AST;
        break;
      default:fprintf(stderr, "Usage: %s [-rv] [file...]\n", argv[0]);
        exit(EXIT_FAILURE);
    }
  }
  argv += optind;

  if (*argv == NULL) {
    bl_warning("nothing to do, no input files, sorry :(");
    exit(EXIT_SUCCESS);
  }

  Builder *builder = bl_builder_new(build_flag);
  Assembly *assembly = bl_assembly_new("main_assembly");

  /* init actors */
  while (*argv != NULL) {
    Unit *unit = bl_unit_new_file(*argv);
    bl_assembly_add_unit(assembly, unit);
    argv++;
  }

  if (!bl_builder_compile(builder, assembly)) {
    Actor *failed = bl_builder_get_failed(builder);
    bl_error("%s", bl_actor_get_error(failed));
  } else {
    bl_log(BL_GREEN("done"));
  }

  bo_unref(assembly);

  return 0;
}

