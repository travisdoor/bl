//************************************************************************************************
// blc
//
// File:   deps_builder.c
// Author: Martin Dorazil
// Date:   20.6.18
//
// Copyright 2018 Martin Dorazil
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, first, publish, distribute, sublicense, and/or sell
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

#include <bobject/containers/htbl.h>
#include "common_impl.h"
#include "stages_impl.h"
#include "visitor_impl.h"

#define EXPECTED_DEPS_COUNT 512
#define peek_cnt(visitor) ((context_t *)(visitor)->context)

typedef struct
{
  bl_node_t * curr_func;
  BHashTable *unique_deps_per_func;
} context_t;

static inline bool
has_dependency(context_t *cnt, bl_node_t *dep)
{
  return bo_htbl_has_key(cnt->unique_deps_per_func, (uint64_t)dep);
}
static inline void
add_dependency_of_curr_func(context_t *cnt, bl_node_t *dep)
{
  if (has_dependency(cnt, dep))
    return;

  bo_htbl_insert_empty(cnt->unique_deps_per_func, (uint64_t)dep);
  // TODO add to function deps array
  bl_log("unique dependency %s -> %s", bl_ast_get_id(cnt->curr_func)->str,
         bl_ast_get_id(dep)->str);
}

static inline void
reset_unique_deps_cache(context_t *cnt)
{
  bo_htbl_clear(cnt->unique_deps_per_func);
}

static void
visit_func(bl_visitor_t *visitor, bl_node_t **func)
{
  context_t *cnt = peek_cnt(visitor);
  cnt->curr_func = *func;
  reset_unique_deps_cache(cnt);
  bl_visitor_walk_func(visitor, func);
}

static void
visit_expr(bl_visitor_t *visitor, bl_node_t **expr)
{
  context_t *cnt = peek_cnt(visitor);

  if (bl_node_is(*expr, BL_EXPR_CALL)) {
    bl_expr_call_t *_call = bl_peek_expr_call(*expr);
    bl_assert(_call->ref, "invalid callee");

    /* Store dependency of current processed function on another callee. Later during generation we
     * need to know which function should go first. */
    add_dependency_of_curr_func(cnt, _call->ref);
  }

  bl_visitor_walk_expr(visitor, expr);
}

bl_error_e
bl_deps_builder_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.curr_func = NULL, .unique_deps_per_func = bo_htbl_new(0, EXPECTED_DEPS_COUNT)};

  bl_visitor_t visitor;
  bl_visitor_init(&visitor, &cnt);
  bl_visitor_add(&visitor, visit_expr, BL_VISIT_EXPR);
  bl_visitor_add(&visitor, visit_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor, BL_SKIP_VISIT, BL_VISIT_STRUCT);
  /* NOTE: enumerators can lead to call any function via #run directive -> this could lead to
   * dependency of function where enum is used as const expression ??? */
  bl_visitor_add(&visitor, BL_SKIP_VISIT, BL_VISIT_ENUM);

  const int c = bl_assembly_get_unit_count(assembly);
  for (int i = 0; i < c; ++i) {
    bl_unit_t *unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_gscope(&visitor, &unit->ast.root);
  }

  bo_unref(cnt.unique_deps_per_func);
  return BL_NO_ERR;
}
