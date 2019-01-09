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
#include "bldebug.h"
#include "unit.h"
#include "assembly.h"
#include "builder.h"
#include "error.h"
#include "messages.h"

static void
print_help(void)
{
  fprintf(stdout, "Usage\n\n"
                  "  blc [options] <source-files>\n\n"
                  "Options\n"
                  "  -h, -help           = Print usage information and exit.\n"
                  "  -ast-dump           = Print AST.\n"
                  "  -lex-dump           = Print output of lexer.\n"
                  "  -mir-pre-dump       = Print output of MIR pre analyze stage.\n"
                  "  -mir-post-dump      = Print output of MIR post analyze stage.\n"
                  "  -syntax-only        = Check syntax and exit.\n"
                  "  -emit-llvm          = Write LLVM-IR to file.\n"
                  "  -emit-mir           = Write MIR to file.\n"
                  "  -run                = Execute 'main' method in compile time.\n"
                  "  -run-tests          = Execute all unit tests in compile time.\n"
                  "  -no-bin             = Don't write binary to disk.\n"
                  "  -no-warning         = Ignore all warnings.\n"
                  "  -verbose            = Verbose mode.\n"
                  "  -no-api             = Don't load internal api.\n\n"
                  "  -force-test-to-llvm = Force llvm generation of unit tests.\n\n"
  );
}

int
main(int argc, char *argv[])
{
  setlocale(LC_ALL, "C");
  unsigned int build_flags = BUILDER_LOAD_FROM_FILE;
  puts("compiler version: " BL_VERSION);

  bool   help = false;
  size_t optind;
  for (optind = 1; optind < argc && argv[optind][0] == '-'; optind++) {
    if (strcmp(&argv[optind][1], "ast-dump") == 0) {
      build_flags |= BUILDER_PRINT_AST;
    } else if (strcmp(&argv[optind][1], "h") == 0) {
      help = true;
    } else if (strcmp(&argv[optind][1], "help") == 0) {
      help = true;
    } else if (strcmp(&argv[optind][1], "lex-dump") == 0) {
      build_flags |= BUILDER_PRINT_TOKENS;
    } else if (strcmp(&argv[optind][1], "mir-pre-dump") == 0) {
      build_flags |= BUILDER_VERBOSE_MIR_PRE;
    } else if (strcmp(&argv[optind][1], "mir-post-dump") == 0) {
      build_flags |= BUILDER_VERBOSE_MIR_POST;
    } else if (strcmp(&argv[optind][1], "syntax-only") == 0) {
      build_flags |= BUILDER_SYNTAX_ONLY;
    } else if (strcmp(&argv[optind][1], "emit-llvm") == 0) {
      build_flags |= BUILDER_EMIT_LLVM;
    } else if (strcmp(&argv[optind][1], "emit-mir") == 0) {
      build_flags |= BUILDER_EMIT_MIR;
    } else if (strcmp(&argv[optind][1], "run") == 0) {
      build_flags |= BUILDER_RUN;
    } else if (strcmp(&argv[optind][1], "run-tests") == 0) {
      build_flags |= BUILDER_RUN_TESTS;
    } else if (strcmp(&argv[optind][1], "no-bin") == 0) {
      build_flags |= BUILDER_NO_BIN;
    } else if (strcmp(&argv[optind][1], "no-warning") == 0) {
      build_flags |= BUILDER_NO_WARN;
    } else if (strcmp(&argv[optind][1], "verbose") == 0) {
      build_flags |= BUILDER_VERBOSE;
    } else if (strcmp(&argv[optind][1], "no-api") == 0) {
      build_flags |= BUILDER_NO_API;
    } else if (strcmp(&argv[optind][1], "force-test-to-llvm") == 0) {
      build_flags |= BUILDER_FORCE_TEST_LLVM;
    } else {
      msg_error("invalid params '%s'", &argv[optind][1]);
      print_help();
      exit(EXIT_FAILURE);
    }
  }
  argv += optind;

  if (help) {
    print_help();
    exit(EXIT_SUCCESS);
  }

  if (*argv == NULL) {
    msg_warning("nothing to do, no input files, sorry :(");
    exit(EXIT_SUCCESS);
  }

  Builder *builder = builder_new();

  /*
   * HACK: use name of first file as assembly name
   */
  char *assembly_name = strrchr(*argv, PATH_SEPARATORC);
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

  Assembly *assembly = assembly_new(assembly_name);
  free(assembly_name);

  /* init actors */
  while (*argv != NULL) {
    Unit *unit = unit_new_file(*argv, NULL);

    bool added = assembly_add_unit_unique(assembly, unit);
    if (added == false) {
      unit_delete(unit);
    }

    argv++;
  }

  int state = builder_compile(builder, assembly, build_flags);

  char date[26];
  date_time(date, 26, "%d-%m-%Y %H:%M:%S");
  msg_log("\nfinished at %s", date);
  
  if (state == COMPILE_OK) {
    msg_log(GREEN("done"));
  }

  assembly_delete(assembly);
  builder_delete(builder);

  return state;
}
