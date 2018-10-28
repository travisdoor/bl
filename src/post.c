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
  AstDecl * curr_dependent;
  size_t    type_table_size;
} Context;

#if 0
static void
post_node(Visitor *visitor, Ast *node, void *cnt);
#endif

static void
post_decl(Visitor *visitor, AstDecl *decl, void *cnt);

static void
post_call(Visitor *visitor, AstExprCall *call, void *cnt);

static void
post_ident(Visitor *visitor, AstIdent *ident, void *cnt);

static inline void
schedule_generation(Context *cnt, AstDecl *decl)
{
  assert(decl);
  if (decl->used) {
    bo_list_push_back(cnt->assembly->ir_queue, decl);
#if VERBOSE
    bl_log("schedule generation of: %s", ast_peek_ident(_decl->name)->str);
#endif
  }
}

#if 0 
void
post_node(Visitor *visitor, Ast *node, void *cnt)
{
  if (node->state == NOT_CHECKED) {
    bl_warning("%s (%d) has not been checked!!!", node_name(node), node->_serial);
  }
}
#endif

void
post_decl(Visitor *visitor, AstDecl *decl, void *cnt)
{
  Context *_cnt = (Context *)cnt;
  assert(decl);
  if (!decl->in_gscope && !decl->used && !(decl->flags & FLAG_EXTERN) &&
      !(decl->flags & FLAG_MAIN)) {
    post_warning_node(_cnt, (Ast *)decl->name, BUILDER_CUR_WORD,
                      "symbol is declared but never used");
  }

  if (decl->flags & FLAG_MAIN) decl->used++;

  AstDecl *prev_dependent = _cnt->curr_dependent;
  switch (decl->kind) {
  case DECL_KIND_FN:
    _cnt->curr_dependent = decl;
    schedule_generation(cnt, decl);
    break;
  case DECL_KIND_FIELD:
    if (decl->in_gscope) {
      _cnt->curr_dependent = decl;
      schedule_generation(cnt, decl);
    }
    break;
  default:
    break;
  }

  visitor_walk(visitor, (Ast *)decl, cnt);
  _cnt->curr_dependent = prev_dependent;

#if VERBOSE
  if (_decl->deps) {
    bl_log(YELLOW("'%s'") " depends on:", ast_peek_ident(_decl->name)->str);
    bo_iterator_t it;
    Dependency    tmp;
    bhtbl_foreach(_decl->deps, it)
    {
      tmp = bo_htbl_iter_peek_value(_decl->deps, &it, Dependency);
      bl_log("  [%s] %s", tmp.type == DEP_STRICT ? RED("STRICT") : GREEN(" LAX "),
             ast_peek_ident(ast_peek_decl(tmp.node)->name)->str);
    }
  }
#endif
}

void
post_call(Visitor *visitor, AstExprCall *call, void *cnt)
{
  Context *_cnt = (Context *)cnt;
  assert(call);

  assert(_cnt->curr_dependent);
  Ast *callee = call->ref;
  if (ast_is(callee, AST_DECL)) {
    AstDecl *_callee = (AstDecl *)callee;
    if (!(_callee->flags & FLAG_EXTERN) && !_callee->mutable)
      ast_add_dep_uq(_cnt->curr_dependent, _callee, call->run ? DEP_STRICT : DEP_LAX);
  }

  visitor_walk(visitor, (Ast *)call, cnt);
}

void
post_ident(Visitor *visitor, AstIdent *ident, void *cnt)
{
  visitor_walk(visitor, (Ast *)ident, cnt);
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

  visitor_add(&visitor, (VisitorFunc)post_decl, AST_DECL);
  visitor_add(&visitor, (VisitorFunc)post_call, AST_EXPR_CALL);
  visitor_add(&visitor, (VisitorFunc)post_ident, AST_IDENT);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    visitor_visit(&visitor, (Ast *)unit->ast, &cnt);
  }

#if VERBOSE
  bl_log("found %d unique types", cnt.type_table_size);
#endif
}
