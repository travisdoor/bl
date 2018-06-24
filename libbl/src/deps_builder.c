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

  bl_assembly_t *assembly;
} context_t;

static inline void
get_uname(char *out_buf, int max_len, bl_node_t *node)
{
  int l      = 0;
  out_buf[0] = '\0';

  while (node) {
    const char *tmp = bl_ast_get_id(node)->str;
    if (!tmp)
      break;
    int tmp_len = strlen(tmp) + 1;

    if (tmp_len + l + 1 > max_len)
      break;

    sprintf(out_buf + l, "_%s", tmp);
    l += tmp_len;
    node = bl_ast_get_parent(node);
  }
}

static inline bl_dependency_t *
get_dependency(context_t *cnt, bl_node_t *dep)
{
  if (bo_htbl_has_key(cnt->unique_deps_per_func, (uint64_t)dep))
    return bo_htbl_at(cnt->unique_deps_per_func, (uint64_t)dep, bl_dependency_t *);
  return NULL;
}

static inline void
add_dependency_of_curr_func(context_t *cnt, bl_node_t *dep, int type)
{
  bl_dependency_t *tmp = get_dependency(cnt, dep);
  if (tmp) {
    tmp->type |= type;
    return;
  }

  bl_decl_func_t *_callee = bl_peek_decl_func(cnt->curr_func);
  tmp                     = bl_ast_func_add_dep(_callee, dep, type);
  bo_htbl_insert(cnt->unique_deps_per_func, (uint64_t)dep, tmp);
}

static inline void
reset_unique_deps_cache(context_t *cnt)
{
  bo_htbl_clear(cnt->unique_deps_per_func);
}

static void
visit_func(bl_visitor_t *visitor, bl_node_t **func)
{
  context_t *     cnt   = peek_cnt(visitor);
  bl_decl_func_t *_func = bl_peek_decl_func(*func);

  cnt->curr_func = *func;
  reset_unique_deps_cache(cnt);
  bl_visitor_walk_func(visitor, func);

  bo_list_push_back(cnt->assembly->func_queue, *func);

  /* create function unique name */
  char tmp[BL_MAX_FUNC_NAME_LEN] = {0};
  get_uname(&tmp[0], BL_MAX_FUNC_NAME_LEN, *func);
  _func->uname = strdup(tmp);

  //#define PRINT_DEPS
#ifdef PRINT_DEPS
  // TEST
  if (_func->deps) {
    bl_dependency_t *dep;
    bo_iterator_t    iter = bo_list_begin(_func->deps);
    bo_iterator_t    end  = bo_list_end(_func->deps);
    while (!bo_iterator_equal(&iter, &end)) {
      dep = &bo_list_iter_peek(_func->deps, &iter, bl_dependency_t);

      bl_log("unique dependency %s -> %s (%d)", _func->id.str, bl_ast_get_id(dep->node)->str,
             dep->type);

      bo_list_iter_next(_func->deps, &iter);
    }
  }
#endif
#undef PRINT_DEPS
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
    if (_call->ref != cnt->curr_func)
      add_dependency_of_curr_func(cnt, _call->ref,
                                  _call->run_in_compile_time ? BL_DEP_STRICT : BL_DEP_LAX);
  }

  bl_visitor_walk_expr(visitor, expr);
}

bl_error_e
bl_deps_builder_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.curr_func = NULL,
                   .unique_deps_per_func =
                       bo_htbl_new(sizeof(bl_dependency_t *), EXPECTED_DEPS_COUNT),
                   .assembly = assembly};

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
