//************************************************************************************************
// bl
//
// File:   post.c
// Author: Martin Dorazil
// Date:   31/8/18
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

#include "stages_impl.h"
#include "common_impl.h"
#include "ast_impl.h"

#define VERBOSE 0

#define post_warning_node(cnt, node, pos, format, ...)                                             \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, (node)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

#define peek_cnt(v) ((context_t *)(v->cnt))

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
  node_t *       curr_dependent;
} context_t;

static void
post_decl(bl_ast_visitor_t *visitor, node_t *decl, void *cnt);

static void
post_call(bl_ast_visitor_t *visitor, node_t *call, void *cnt);

static void
post_ident(bl_ast_visitor_t *visitor, node_t *ident, void *cnt);

static inline void
schedule_generation(context_t *cnt, node_t *decl)
{
  assert(decl);
  node_decl_t *_decl = peek_decl(decl);
  if (_decl->used) bo_list_push_back(cnt->assembly->ir_queue, decl);
}

void
post_decl(bl_ast_visitor_t *visitor, node_t *decl, void *cnt)
{
  context_t *_cnt = (context_t *)cnt;
  assert(decl);
  node_decl_t *_decl = peek_decl(decl);
  if (!_decl->in_gscope && !_decl->used && !(_decl->flags & FLAG_EXTERN) &&
      !(_decl->flags & FLAG_MAIN) && _decl->kind != DECL_KIND_VARIANT) {
    post_warning_node(_cnt, _decl->name, BL_BUILDER_CUR_WORD, "symbol is declared but never used");
  }

  if (_decl->flags & FLAG_MAIN) _decl->used++;

  node_t *prev_dependent = _cnt->curr_dependent;
  switch (_decl->kind) {
  case DECL_KIND_FN:
    _cnt->curr_dependent = decl;
    schedule_generation(cnt, decl);
    break;
  case DECL_KIND_FIELD:
    if (_decl->in_gscope) {
      _cnt->curr_dependent = decl;
      schedule_generation(cnt, decl);
    }
    break;
  default: break;
  }

  bl_ast_walk(visitor, decl, cnt);
  _cnt->curr_dependent = prev_dependent;

#if VERBOSE
  if (_decl->deps) {
    bl_log(BL_YELLOW("'%s'") " depends on:", peek_ident(_decl->name)->str);
    bo_iterator_t   it;
    bl_dependency_t tmp;
    bl_bhtbl_foreach(_decl->deps, it)
    {
      tmp = bo_htbl_iter_peek_value(_decl->deps, &it, bl_dependency_t);
      bl_log("  [%s] %s", tmp.type == DEP_STRICT ? BL_RED("STRICT") : BL_GREEN(" LAX "),
             peek_ident(peek_decl(tmp.node)->name)->str);
    }
  }
#endif
}

void
post_call(bl_ast_visitor_t *visitor, node_t *call, void *cnt)
{
  context_t *_cnt = (context_t *)cnt;
  assert(call);
  node_expr_call_t *_call = peek_expr_call(call);

  assert(_cnt->curr_dependent);
  node_t *callee = bl_ast_unroll_ident(_call->ref);
  if (node_is(callee, NODE_DECL) && !(peek_decl(callee)->flags & FLAG_EXTERN))
    bl_ast_add_dep_uq(_cnt->curr_dependent, callee, _call->run ? DEP_STRICT : DEP_LAX);

  bl_ast_walk(visitor, call, cnt);
}

void
post_ident(bl_ast_visitor_t *visitor, node_t *ident, void *cnt)
{
  assert(ident);
  context_t *   _cnt   = (context_t *)cnt;
  node_ident_t *_ident = peek_ident(ident);

  if (!_ident->ref || !_cnt->curr_dependent) {
    bl_ast_walk(visitor, ident, cnt);
    return;
  }

  node_decl_t *_decl = peek_decl(_ident->ref);
  if (_decl->in_gscope || _decl->kind == DECL_KIND_FN)
    bl_ast_add_dep_uq(_cnt->curr_dependent, _ident->ref, DEP_LAX);
  bl_ast_walk(visitor, ident, cnt);
}

void
bl_post_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {
      .builder        = builder,
      .assembly       = assembly,
      .unit           = NULL,
      .curr_dependent = NULL,
  };

  bl_ast_visitor_t visitor;
  bl_ast_visitor_init(&visitor);

  bl_ast_visitor_add(&visitor, post_decl, NODE_DECL);
  bl_ast_visitor_add(&visitor, post_call, NODE_EXPR_CALL);
  bl_ast_visitor_add(&visitor, post_ident, NODE_IDENT);

  bl_unit_t *unit;
  bl_barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    bl_ast_visit(&visitor, unit->ast.root, &cnt);
  }
}
