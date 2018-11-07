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

typedef struct
{
  Builder *      builder;
  Assembly *     assembly;
  Unit *         unit;
  AstDeclEntity *curr_dependent;
  bool           verbose;
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
post_stmt_return(Context *cnt, AstStmtReturn *stmt_ret);

static void
post_stmt_if(Context *cnt, AstStmtIf *stmt_if);

static void
post_decl(Context *cnt, AstDecl *decl);

static void
post_decl_entity(Context *cnt, AstDeclEntity *entity);

static void
post_expr_ref(Context *cnt, AstExprRef *ref);

static void
post_expr_lit_fn(Context *cnt, AstExprLitFn *fn);

static void
post_expr_binop(Context *cnt, AstExprBinop *binop);

static void
post_expr_call(Context *cnt, AstExprCall *call);

/* impl */
void
post_ublock(Context *cnt, AstUBlock *ublock)
{
  Ast *tmp;
  barray_foreach(ublock->nodes, tmp) post_node(cnt, tmp);
}

void
post_block(Context *cnt, AstBlock *block)
{
  Ast *tmp;
  barray_foreach(block->nodes, tmp) post_node(cnt, tmp);
}

void
post_stmt_return(Context *cnt, AstStmtReturn *stmt_ret)
{
  post_expr(cnt, stmt_ret->expr);
}

void
post_stmt_if(Context *cnt, AstStmtIf *stmt_if)
{
  post_expr(cnt, stmt_if->test);
  post_node(cnt, stmt_if->true_stmt);
  post_node(cnt, stmt_if->false_stmt);
}

void
post_decl(Context *cnt, AstDecl *decl)
{
  switch (decl->kind) {
  case AST_DECL_BAD:
    break;
  case AST_DECL_ENTITY:
    post_decl_entity(cnt, (AstDeclEntity *)decl);
    break;
  case AST_DECL_MEMBER:
    break;
  case AST_DECL_ARG:
    break;
  case AST_DECL_VARIANT:
    break;
  }
}

void
post_decl_entity(Context *cnt, AstDeclEntity *entity)
{
  /* schedule future generation of this declaration, notice that all declarations must be used!!!
   */
  bool generate = false;
  switch (entity->kind) {
  case DECL_ENTITY_FN:
    generate = true;
    break;
  case DECL_ENTITY_FIELD:
    if (entity->in_gscope) generate = true;
    break;
  default:
    break;
  }

  AstDeclEntity *prev_depenedent = NULL;
  if (generate && entity->used) {
    prev_depenedent     = cnt->curr_dependent;
    cnt->curr_dependent = entity;

    bo_list_push_back(cnt->assembly->ir_queue, entity);
    if (cnt->verbose) msg_log(LOG_TAG ": schedule generation of: '%s'", entity->base.name->str);
  }

  post_expr(cnt, entity->value);

  if (prev_depenedent) cnt->curr_dependent = prev_depenedent;

  if (cnt->verbose && entity->deps) {
    msg_log(LOG_TAG ": '%s' depends on:", entity->base.name->str);

    bo_iterator_t it;
    Dependency    tmp;
    bhtbl_foreach(entity->deps, it)
    {
      tmp = bo_htbl_iter_peek_value(entity->deps, &it, Dependency);
      msg_log(LOG_TAG ": [%s] %s", tmp.type == DEP_STRICT ? RED("STRICT") : GREEN(" LAX "),
              ((AstDecl *)tmp.decl)->name->str);
    }
  }
}

void
post_expr_ref(Context *cnt, AstExprRef *ref)
{
  assert(cnt->curr_dependent);
  assert(ref->ref);

  if (ref->ref->kind != AST_DECL_ENTITY) return;

  AstDeclEntity *entity = (AstDeclEntity *)ref->ref;

  if (!(entity->flags & FLAG_EXTERN) &&
      (entity->kind == DECL_ENTITY_FIELD || entity->kind == DECL_ENTITY_FN) && entity->in_gscope) {
    ast_add_dep_uq(cnt->curr_dependent, entity, DEP_LAX);
  }
}

void
post_expr_call(Context *cnt, AstExprCall *call)
{
  assert(call->ref);
  post_expr(cnt, call->ref);

  AstExpr *arg;
  barray_foreach(call->args, arg) post_expr(cnt, arg);
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
  switch (node->kind) {

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
  case AST_STMT_RETURN:
    post_stmt_return(cnt, (AstStmtReturn *)node);
    break;
  case AST_STMT_IF:
    post_stmt_if(cnt, (AstStmtIf *)node);
    break;

  case AST_LOAD:
  case AST_LINK:
  case AST_IDENT:
  case AST_STMT_LOOP:
  case AST_STMT_BREAK:
  case AST_STMT_CONTINUE:
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
  switch (expr->kind) {

  case AST_EXPR_REF:
    post_expr_ref(cnt, (AstExprRef *)expr);
    break;

  case AST_EXPR_LIT_FN:
    post_expr_lit_fn(cnt, (AstExprLitFn *)expr);
    break;

  case AST_EXPR_BINOP:
    post_expr_binop(cnt, (AstExprBinop *)expr);
    break;

  case AST_EXPR_CALL:
    post_expr_call(cnt, (AstExprCall *)expr);
    break;

  case AST_EXPR_CAST:
  case AST_EXPR_TYPE:
  case AST_EXPR_MEMBER:
  case AST_EXPR_ELEM:
  case AST_EXPR_SIZEOF:
  case AST_EXPR_TYPEOF:
  case AST_EXPR_UNARY:
  case AST_EXPR_NULL:
  case AST_EXPR_LIT_INT:
  case AST_EXPR_LIT_FLOAT:
  case AST_EXPR_LIT_DOUBLE:
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
      .verbose        = (bool)(builder->flags & BUILDER_VERBOSE),
  };

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    post_node(&cnt, (Ast *)unit->ast);
  }
}
