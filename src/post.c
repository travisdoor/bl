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
  Builder * builder;
  Assembly *assembly;
  Unit *    unit;
  Node *    curr_dependent;
  size_t    type_table_size;
} Context;

#if 0
static void
post_node(Visitor *visitor, Node *node, void *cnt);
#endif

static void
post_decl(Visitor *visitor, Node *decl, void *cnt);

static void
post_call(Visitor *visitor, Node *call, void *cnt);

static void
post_ident(Visitor *visitor, Node *ident, void *cnt);

static void
post_type(Visitor *visitor, Node *type, void *cnt);

static inline void
schedule_generation(Context *cnt, Node *decl)
{
  assert(decl);
  NodeDecl *_decl = peek_decl(decl);
  if (_decl->used) {
    bo_list_push_back(cnt->assembly->ir_queue, decl);
#if VERBOSE
    bl_log("schedule generation of: %s", peek_ident(_decl->name)->str);
#endif
  }
}

#if 0 
void
post_node(Visitor *visitor, Node *node, void *cnt)
{
  if (node->state == NOT_CHECKED) {
    bl_warning("%s (%d) has not been checked!!!", node_name(node), node->_serial);
  }
}
#endif

void
post_decl(Visitor *visitor, Node *decl, void *cnt)
{
  Context *_cnt = (Context *)cnt;
  assert(decl);
  NodeDecl *_decl = peek_decl(decl);
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
    bl_log(YELLOW("'%s'") " depends on:", peek_ident(_decl->name)->str);
    bo_iterator_t it;
    Dependency    tmp;
    bhtbl_foreach(_decl->deps, it)
    {
      tmp = bo_htbl_iter_peek_value(_decl->deps, &it, Dependency);
      bl_log("  [%s] %s", tmp.type == DEP_STRICT ? RED("STRICT") : GREEN(" LAX "),
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
  NodeExprCall *_call = peek_expr_call(call);

  assert(_cnt->curr_dependent);
  Node *callee = ast_unroll_ident(_call->ref);
  if (node_is(callee, NODE_DECL) && !(peek_decl(callee)->flags & FLAG_EXTERN) &&
      !peek_decl(callee)->mutable) {
    ast_add_dep_uq(_cnt->curr_dependent, callee, _call->run ? DEP_STRICT : DEP_LAX);
  }

  visitor_walk(visitor, call, cnt);
}

void
post_ident(Visitor *visitor, Node *ident, void *cnt)
{
  assert(ident);
  Context *  _cnt   = (Context *)cnt;
  NodeIdent *_ident = peek_ident(ident);

  if (!_ident->ref || !_cnt->curr_dependent) {
    visitor_walk(visitor, ident, cnt);
    return;
  }

  NodeDecl *_decl = peek_decl(_ident->ref);
  if ((_decl->in_gscope && _decl->mutable) || _decl->kind == DECL_KIND_FN) {
    ast_add_dep_uq(_cnt->curr_dependent, _ident->ref, DEP_LAX);
  }

  visitor_walk(visitor, ident, cnt);
}

void
post_type(Visitor *visitor, Node *type, void *cnt)
{
#if 0
  Context *_cnt = (Context *)cnt;
  if (bo_htbl_has_key(_cnt->assembly->type_table, (uint64_t)type)) {
    visitor_walk(visitor, type, cnt);
    return;
  }

  char buf[256];
  ast_type_to_string(buf, 256, type);
  bl_log("new type: %s", buf);
  bo_htbl_insert_empty(_cnt->assembly->type_table, (uint64_t)type);
  _cnt->type_table_size++;
#endif
}

void
post_run(Builder *builder, Assembly *assembly)
{
  Context cnt = {
      .builder         = builder,
      .assembly        = assembly,
      .unit            = NULL,
      .curr_dependent  = NULL,
      .type_table_size = 0,
  };

  Visitor visitor;
  visitor_init(&visitor);
#if 0 
  visitor_add_visit_all(&visitor, post_node);
#endif

  visitor_add(&visitor, post_decl, NODE_DECL);
  visitor_add(&visitor, post_call, NODE_EXPR_CALL);
  visitor_add(&visitor, post_ident, NODE_IDENT);
  visitor_add(&visitor, post_type, NODE_TYPE_FUND);
  visitor_add(&visitor, post_type, NODE_TYPE_ENUM);
  visitor_add(&visitor, post_type, NODE_TYPE_STRUCT);
  visitor_add(&visitor, post_type, NODE_TYPE_FN);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    visitor_visit(&visitor, unit->ast.root, &cnt);
  }

#if VERBOSE
  bl_log("found %d unique types", cnt.type_table_size);
#endif
}
