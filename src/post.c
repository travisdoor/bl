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

#include <bobject/containers/array.h>
#include "stages.h"
#include "common.h"
#include "ast.h"

#define LOG_TAG CYAN("POST")

#define push_curr_dependend(_cnt, _decl)                                                           \
  AstDecl *const _prev_dependent = (_cnt)->curr_dependent;                                         \
  (_cnt)->curr_dependent         = (_decl);

#define pop_curr_dependent(_cnt) (_cnt)->curr_dependent = _prev_dependent;

typedef struct
{
  Builder * builder;
  Assembly *assembly;
  Unit *    unit;
  AstDecl * curr_dependent;
} Context;

static void
post_node(Context *cnt, Ast *node);

static void
post_expr(Context *cnt, AstExpr *expr);

static void
post_ublock(Context *cnt, AstUBlock *ublock);

static void
post_block(Context *cnt, AstBlock *block);

static void
post_decl(Context *cnt, AstDecl *decl);

static void
post_expr_ref(Context *cnt, AstExprRef *ref);

static void
post_expr_lit_fn(Context *cnt, AstExprLitFn *fn);

static void
post_expr_binop(Context *cnt, AstExprBinop *binop);

/* impl */
void
post_ublock(Context *cnt, AstUBlock *ublock)
{
  Ast *tmp;
  node_foreach(ublock->nodes, tmp) post_node(cnt, tmp);
}

void
post_block(Context *cnt, AstBlock *block)
{
  Ast *tmp;
  node_foreach(block->nodes, tmp) post_node(cnt, tmp);
}

void
post_decl(Context *cnt, AstDecl *decl)
{
  push_curr_dependend(cnt, decl);
  post_expr(cnt, decl->value);
  pop_curr_dependent(cnt);

  if (cnt->builder->flags & BUILDER_VERBOSE && decl->deps) {
    msg_log(LOG_TAG ": '%s' depends on:", decl->name->str);

    bo_iterator_t it;
    Dependency    tmp;
    bhtbl_foreach(decl->deps, it)
    {
      tmp = bo_htbl_iter_peek_value(decl->deps, &it, Dependency);
      msg_log(LOG_TAG ": [%s] %s", tmp.type == DEP_STRICT ? RED("STRICT") : GREEN(" LAX "),
              tmp.decl->name->str);
    }
  }
}

void
post_expr_ref(Context *cnt, AstExprRef *ref)
{
  assert(cnt->curr_dependent);
  assert(ref->ref && ast_is(ref->ref, AST_DECL));

  AstDecl *dep = (AstDecl *)ref->ref;
  if (!(dep->flags & FLAG_EXTERN) && dep->kind == DECL_KIND_FIELD && dep->in_gscope) {
    ast_add_dep_uq(cnt->curr_dependent, dep, DEP_LAX);
  }
}

void
post_expr_lit_fn(Context *cnt, AstExprLitFn *fn)
{
  post_node(cnt, fn->block);
}

void
post_expr_binop(Context *cnt, AstExprBinop *binop)
{
  post_expr(cnt, binop->lhs);
  post_expr(cnt, binop->rhs);
}

void
post_node(Context *cnt, Ast *node)
{
  if (!node) return;
  switch (ast_kind(node)) {

  case AST_UBLOCK:
    post_ublock(cnt, (AstUBlock *)node);
    break;
  case AST_BLOCK:
    post_block(cnt, (AstBlock *)node);
    break;
  case AST_EXPR:
    post_expr(cnt, (AstExpr *)node);
    break;
  case AST_DECL:
    post_decl(cnt, (AstDecl *)node);
    break;

  case AST_LOAD:
  case AST_LINK:
  case AST_IDENT:
  case AST_STMT_RETURN:
  case AST_STMT_IF:
  case AST_STMT_LOOP:
  case AST_STMT_BREAK:
  case AST_STMT_CONTINUE:
  case AST_MEMBER:
  case AST_ARG:
  case AST_VARIANT:
  case AST_TYPE:
  case AST_COUNT:
    break;
  case AST_BAD:
    bl_abort("bad node!!!");
  }
}

void
post_expr(Context *cnt, AstExpr *expr)
{
  if (!expr) return;
  switch (ast_expr_kind(expr)) {

  case AST_EXPR_REF:
    post_expr_ref(cnt, (AstExprRef *)expr);
    break;

  case AST_EXPR_LIT_FN:
    post_expr_lit_fn(cnt, (AstExprLitFn *)expr);
    break;

  case AST_EXPR_BINOP:
    post_expr_binop(cnt, (AstExprBinop *)expr);
    break;

  case AST_EXPR_TYPE:
  case AST_EXPR_CAST:
  case AST_EXPR_CALL:
  case AST_EXPR_MEMBER:
  case AST_EXPR_ELEM:
  case AST_EXPR_SIZEOF:
  case AST_EXPR_TYPEOF:
  case AST_EXPR_UNARY:
  case AST_EXPR_NULL:
  case AST_EXPR_LIT_INT:
  case AST_EXPR_LIT_FLOAT:
  case AST_EXPR_LIT_CHAR:
  case AST_EXPR_LIT_STRING:
  case AST_EXPR_LIT_BOOL:
  case AST_EXPR_LIT_CMP:
    break;
  case AST_EXPR_BAD:
    bl_abort("bad expression!!!");
  }
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

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    post_node(&cnt, (Ast *)unit->ast);
  }
}
