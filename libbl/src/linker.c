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

#define PRINT_GLOBALS 1

#define link_error(cnt, code, loc, format, ...) \
  { \
    bl_builder_error((cnt)->builder, "%s %d:%d " format, loc->file, loc->line, loc->col, ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, (code)); \
  }

typedef struct
{
  bl_builder_t *builder;
  bl_assembly_t *assembly;
  jmp_buf jmp_error;
} context_t;

static void
link(context_t *cnt,
     bl_unit_t *unit);

void
link(context_t *cnt,
     bl_unit_t *unit)
{
  /* copy global declarations and solve collisions */
  BHashTable *dest = bl_scope_get_all(&cnt->assembly->scope);
  BHashTable *src = bl_scope_get_all(&unit->scope);

  bo_iterator_t src_iter = bo_htbl_begin(src);
  bo_iterator_t src_end = bo_htbl_end(src);

  bl_node_t *decl;
  bl_node_t *coliding;
  while (!bo_iterator_equal(&src_iter, &src_end)) {
    decl = bo_htbl_iter_peek_value(src, &src_iter, bl_node_t *);
    if (bo_htbl_has_key(dest, decl->value.decl.ident.hash)) {
      coliding = bo_htbl_at(dest, decl->value.decl.ident.hash, bl_node_t *);

      link_error(cnt,
                 BL_ERR_DUPLICATE_SYMBOL,
                 decl,
                 "redeclaration of "
                   BL_YELLOW("'%s'")
                   " previous declaration found here: %s %d:%d",
                 decl->value.decl.ident.name,
                 coliding->file,
                 coliding->line,
                 coliding->col);
    } else {
      bo_htbl_insert(dest, decl->value.decl.ident.hash, decl);
    }
    bo_htbl_iter_next(src, &src_iter);
  }

  bl_scope_clear(&unit->scope);
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

  const int c = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    link(&cnt, unit);
  }

#if PRINT_GLOBALS
  BHashTable *src = bl_scope_get_all(&assembly->scope);
  bo_iterator_t src_iter = bo_htbl_begin(src);
  bo_iterator_t src_end = bo_htbl_end(src);

  bl_node_t *decl;
  while (!bo_iterator_equal(&src_iter, &src_end)) {
    decl = bo_htbl_iter_peek_value(src, &src_iter, bl_node_t *);
    bl_log("decl: %s", decl->value.decl.ident.name);
    bo_htbl_iter_next(src, &src_iter);
  }
#endif

  return BL_NO_ERR;
}


