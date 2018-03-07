//*****************************************************************************
// bl
//
// File:   linker.c
// Author: Martin Dorazil
// Date:   3/7/18
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

#include <setjmp.h>
#include "stages_impl.h"
#include "common_impl.h"

#define link_error(cnt, code, format, ...) \
  { \
    bl_builder_error((cnt)->builder, (format), ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, (code)); \
  }

typedef struct
{
  bl_builder_t  *builder;
  bl_assembly_t *assembly;
  jmp_buf       jmp_error;
} context_t;

static void
link(context_t *cnt,
     bl_unit_t *unit);

void
link(context_t *cnt,
     bl_unit_t *unit)
{
  /* copy unsatisfied nodes into assembly cache */
  bl_unsatisfied_t *uns   = &cnt->assembly->unsatisfied;
  const int        cached = bl_unsatisfied_get_count(uns);
  bo_array_insert(
    uns->unsatisfied,
    (size_t) cached,
    bo_array_data(unit->unsatisfied.unsatisfied),
    (size_t) bl_unsatisfied_get_count(&unit->unsatisfied));

  /* copy global declarations and solve collisions */
}

/* public */
bl_error_e
bl_linker_run(bl_builder_t *builder,
              bl_assembly_t *assembly)
{
  /*
   * Solve unsatisfied expressions and declaration collisions
   * between units.
   */

  context_t cnt = {
    .builder = builder, .assembly = assembly
  };

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e) error;
  }

  const int c     = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    link(&cnt, unit);
  }

  const int c2 = bl_unsatisfied_get_count(&assembly->unsatisfied);
  bl_node_t *node;
  for (int i = 0; i < c2; i++) {
    node = bl_unsatisfied_get_node(&assembly->unsatisfied, i);
    bl_log("unsatisfied node: %s", node->value.call_expr.ident.name);
  }

  return BL_NO_ERR;
}


