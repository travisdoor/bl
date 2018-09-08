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

#include "stages.h"
#include "common.h"
#include "ast.h"

#define VERBOSE 0

#define post_warning_node(cnt, node, pos, format, ...)                                             \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_WARNING, 0, (node)->src, (pos), (format),              \
                ##__VA_ARGS__);                                                                    \
  }

#define peek_cnt(v) ((context_t *)(v->cnt))

typedef struct
{
  Builder *builder;
  Assembly * assembly;
  Unit *   unit;
  Node *     curr_dependent;
} Context;

static void
post_decl(Visitor *visitor, Node *decl, void *cnt);

static void
post_call(Visitor *visitor, Node *call, void *cnt);

static void
post_ident(Visitor *visitor, Node *ident, void *cnt);

static inline void
schedule_generation(Context *cnt, Node *decl)
{
  assert(decl);
  node_decl_t *_decl = peek_decl(decl);
  if (_decl->used) bo_list_push_back(cnt->assembly->ir_queue, decl);
}

void
post_decl(Visitor *visitor, Node *decl, void *cnt)
{
  Context *_cnt = (Context *)cnt;
  assert(decl);
  node_decl_t *_decl = peek_decl(decl);
  if (!_decl->in_gscope && !_decl->used && !(_decl->flags & FLAG_EXTERN) &&
      !(_decl->flags & FLAG_MAIN) && _decl->kind != DECL_KIND_VARIANT) {
    post_warning_node(_cnt, _decl->name, BUILDER_CUR_WORD, "symbol is declared but never used");
  }

  if (_decl->flags & FLAG_MAIN) _decl->used++;

  Node *prev_dependent = _cnt->curr_dependent;
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
  default:
    break;
  }

  visitor_walk(visitor, decl, cnt);
  _cnt->curr_dependent = prev_dependent;

#if VERBOSE
  if (_decl->deps) {
    bl_log(BL_YELLOW("'%s'") " depends on:", peek_ident(_decl->name)->str);
    bo_iterator_t   it;
    bl_dependency_t tmp;
    bhtbl_foreach(_decl->deps, it)
    {
      tmp = bo_htbl_iter_peek_value(_decl->deps, &it, bl_dependency_t);
      bl_log("  [%s] %s", tmp.type == DEP_STRICT ? BL_RED("STRICT") : BL_GREEN(" LAX "),
             peek_ident(peek_decl(tmp.node)->name)->str);
    }
  }
#endif
}

void
post_call(Visitor *visitor, Node *call, void *cnt)
{
  Context *_cnt = (Context *)cnt;
  assert(call);
  node_expr_call_t *_call = peek_expr_call(call);

  assert(_cnt->curr_dependent);
  Node *callee = ast_unroll_ident(_call->ref);
  if (node_is(callee, NODE_DECL) && !(peek_decl(callee)->flags & FLAG_EXTERN)) {
    ast_add_dep_uq(_cnt->curr_dependent, callee, _call->run ? DEP_STRICT : DEP_LAX);
  }

  visitor_walk(visitor, call, cnt);
}

void
post_ident(Visitor *visitor, Node *ident, void *cnt)
{
  assert(ident);
  Context *   _cnt   = (Context *)cnt;
  node_ident_t *_ident = peek_ident(ident);

  if (!_ident->ref || !_cnt->curr_dependent) {
    visitor_walk(visitor, ident, cnt);
    return;
  }

  node_decl_t *_decl = peek_decl(_ident->ref);
  if (_decl->in_gscope || _decl->kind == DECL_KIND_FN) {
    ast_add_dep_uq(_cnt->curr_dependent, _ident->ref, DEP_LAX);
  }

  visitor_walk(visitor, ident, cnt);
}

void
post_run(Builder *builder, Assembly *assembly)
{
  Context cnt = {
      .builder        = builder,
      .assembly       = assembly,
      .unit           = NULL,
      .curr_dependent = NULL,
  };

  Visitor visitor;
  visitor_init(&visitor);

  visitor_add(&visitor, post_decl, NODE_DECL);
  visitor_add(&visitor, post_call, NODE_EXPR_CALL);
  visitor_add(&visitor, post_ident, NODE_IDENT);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    visitor_visit(&visitor, unit->ast.root, &cnt);
  }
}
