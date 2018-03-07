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
  bl_builder_t *builder;
  jmp_buf      jmp_error;
} context_t;

static void
link_local(context_t *cnt,
           bl_unit_t *unit);

static bool
link_call_expr(context_t *cnt,
               bl_unit_t *unit,
               bl_node_t *call_expr);

bool
link_call_expr(context_t *cnt,
               bl_unit_t *unit,
               bl_node_t *call_expr)
{
  bl_ident_t *ident = &call_expr->value.call_expr.ident;

  bl_node_t *callee = bl_scope_get(&unit->scope, ident);
  if (callee == NULL) {
    /* TODO: remove later, callee can be defined in another unit */

    link_error(cnt,
               BL_ERR_UNKNOWN_SYMBOL,
               "%s %d:%d function "
                 BL_YELLOW("'%s'")
                 " does not exist in current context",
               unit->filepath,
               call_expr->line,
               call_expr->col,
               ident->name);
  } else {
    /* TODO: check callee params and return type */
    call_expr->value.call_expr.callee = callee;
  }

  return true;
}

void
link_local(context_t *cnt,
           bl_unit_t *unit)
{
  int       c = bl_unsatisfied_get_count(&unit->unsatisfied);
  bl_node_t *uns;
  int       i = 0;

  while (i < c) {
    uns = bl_unsatisfied_get_node(&unit->unsatisfied, i);
    switch (uns->type) {
      case BL_NODE_CALL_EXPR:
        if (link_call_expr(cnt, unit, uns)) {
          c = bl_unsatisfied_remove(&unit->unsatisfied, i);
          continue;
        }
        break;
      default: bl_abort("invalid node type");
    }
    ++i;
  }
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
    .builder = builder
  };

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e) error;
  }

  const int c     = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    link_local(&cnt, unit);
  }

  return BL_NO_ERR;
}


