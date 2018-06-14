//************************************************************************************************
// bl
//
// File:   check.c
// Author: Martin Dorazil
// Date:   3/15/18
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

#include <setjmp.h>
#include "common_impl.h"
#include "stages_impl.h"
#include "visitor_impl.h"

/* Check perform one pass over AST tree where it solves implicit casting and checking of type
 * compatibility of expression nodes. */

#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define check_error(cnt, code, node, pos, format, ...)                                             \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), (node)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

#define check_warning(cnt, node, pos, format, ...)                                                 \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, (node)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
  bl_ast_t *     ast;

  /* tmps */
  jmp_buf jmp_error;
} context_t;

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/

bl_error_e
bl_check_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder = builder, .assembly = assembly};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e)error;
  }

  bl_visitor_t visitor;
  bl_visitor_init(&visitor, &cnt);

  const int  c    = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; ++i) {
    unit     = bl_assembly_get_unit(assembly, i);
    cnt.unit = unit;
    cnt.ast  = &unit->ast;
    bl_visitor_walk_module(&visitor, &unit->ast.root);
  }

  return BL_NO_ERR;
}
