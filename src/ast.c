//************************************************************************************************
// bl
//
// File:   ast.c
// Author: Martin Dorazil
// Date:   15/03/2018
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

#include <math.h>
#include "ast.h"
#include "arena.h"

#define ARENA_CHUNK_COUNT 256

const char *ast_node_names[AST_COUNT] = {
    "AstBad",          "AstLoad",       "AstLink",        "AstIdent",      "AstUBlock",
    "AstBlock",        "AstStmtReturn", "AstStmtIf",      "AstStmtLoop",   "AstStmtBreak",
    "AstStmtContinue", "AstDecl",       "AstMember",      "AstArg",        "AstVariant",
    "AstTypeType",     "AstTypeint",    " AstTypeVargs ", " AstTypeArr ",  " AstTypeFn ",
    " AstTypeStruct ", "AstTypeEnum",   "AstTypePtr",     "AstLitFn",      "AstLitInt",
    "AstLitFloat",     "AstLitChar",    "AstLitString",   "AstLitBool",    "AstLitCmp",
    "AstExprCast",     "AstExprBinop",  "AstExprCall",    "AstExprMember", "AstExprElem",
    "AstExprSizeof",   "AstExprTypeof", "AstExprUnary",   "AstExprNull",
};

Ast         ast_type_buildin[AST_TYPE] =

static void
node_dtor(Ast *node)
{
  switch (node->code) {
  case AST_DECL:
    bo_unref(ast_peek_decl(node)->deps);
    break;
  default:
    break;
  }
}

Ast *
_ast_create_node(Arena *arena, AstCode c, Token *tok)
{
  Ast *node  = arena_alloc(arena);
  node->code = c;
  node->src  = tok ? &tok->src : NULL;

#if BL_DEBUG
  static int serial = 0;
  node->_serial     = serial++;
  node->_state      = NOT_CHECKED;
#endif
  return node;
}

/* public */
void
ast_init(struct Arena *arena)
{
  arena_init(arena, sizeof(Ast), ARENA_CHUNK_COUNT, (ArenaElemDtor)node_dtor);
}

/*************************************************************************************************
 * AST visiting
 *************************************************************************************************/

void
visitor_init(Visitor *visitor)
{
  /* default value for all visitor callbacks */
  memset(visitor->visitors, 0, sizeof(VisitorFunc) * AST_COUNT);
  visitor->all_visitor = NULL;
}

void
visitor_add(Visitor *visitor, VisitorFunc fn, AstCode code)
{
  visitor->visitors[code] = fn;
}

void
visitor_add_visit_all(Visitor *visitor, VisitorFunc fn)
{
  visitor->all_visitor = fn;
}

void
visitor_visit(Visitor *visitor, Ast *node, void *cnt)
{
  if (!node) return;
  if (visitor->all_visitor) visitor->all_visitor(visitor, node, cnt);
  if (visitor->visitors[ast_node_code(node)])
    visitor->visitors[ast_node_code(node)](visitor, node, cnt);
  else
    visitor_walk(visitor, node, cnt);
}

void
visitor_walk(Visitor *visitor, Ast *node, void *cnt)
{
#define visit(node) visitor_visit(visitor, node, cnt)
  if (!node) return;
  Ast *tmp = NULL;

  if (!node) return;
  switch (ast_node_code(node)) {

  case AST_UBLOCK: {
    node_foreach(ast_peek_ublock(node)->nodes, tmp) visit(tmp);
    break;
  }

  case AST_BLOCK: {
    node_foreach(ast_peek_block(node)->nodes, tmp) visit(tmp);
    break;
  }

  case AST_DECL: {
    AstDecl *_decl = ast_peek_decl(node);
    visit(_decl->name);
    visit(_decl->type);
    visit(_decl->value);
    break;
  }

  case AST_MEMBER: {
    break;
  }

  case AST_ARG: {
    break;
  }

  case AST_VARIANT: {
    AstVariant *_var = ast_peek_variant(node);
    visit(_var->value);
    break;
  }

  case AST_TYPE_FUND: {
    break;
  }

  case AST_TYPE_FN: {
    break;
  }

  case AST_TYPE_STRUCT: {
    AstTypeStruct *_struct = ast_peek_type_struct(node);
    node_foreach(_struct->members, tmp) visit(tmp);
    break;
  }

  case AST_TYPE_ENUM: {
    AstTypeEnum *_enum = ast_peek_type_enum(node);
    node_foreach(_enum->variants, tmp) visit(tmp);
    break;
  }

  case AST_EXPR_BINOP: {
    AstExprBinop *_binop = ast_peek_expr_binop(node);
    visit(_binop->lhs);
    visit(_binop->rhs);
    break;
  }

  case AST_EXPR_CALL: {
    AstExprCall *_call = ast_peek_expr_call(node);
    node_foreach(_call->args, tmp) visit(tmp);
    break;
  }

  case AST_EXPR_CAST: {
    visit(ast_peek_expr_cast(node)->next);
    break;
  }

  case AST_EXPR_UNARY: {
    visit(ast_peek_expr_unary(node)->next);
    break;
  }

  case AST_EXPR_MEMBER: {
    visit(ast_peek_expr_member(node)->next);
    break;
  }
  case AST_EXPR_ELEM: {
    visit(ast_peek_expr_elem(node)->index);
    visit(ast_peek_expr_elem(node)->next);
    break;
  }

  case AST_STMT_RETURN: {
    visit(ast_peek_stmt_return(node)->expr);
    break;
  }

  case AST_STMT_IF: {
    visit(ast_peek_stmt_if(node)->test);
    visit(ast_peek_stmt_if(node)->true_stmt);
    visit(ast_peek_stmt_if(node)->false_stmt);
    break;
  }

  case AST_STMT_LOOP: {
    visit(ast_peek_stmt_loop(node)->init);
    visit(ast_peek_stmt_loop(node)->condition);
    visit(ast_peek_stmt_loop(node)->increment);
    visit(ast_peek_stmt_loop(node)->block);
    break;
  }

  case AST_LIT_CMP: {
    AstLitCmp *_lit_cmp = ast_peek_lit_cmp(node);
    visit(_lit_cmp->type);
    node_foreach(_lit_cmp->fields, tmp) visit(tmp);
    break;
  }

  case AST_LIT_FN: {
    visit(ast_peek_lit_fn(node)->block);
    break;
  }

  /* defaults (terminal cases) */
  default:
    break;
  }

#undef visit
}

/*************************************************************************************************
 * other
 *************************************************************************************************/

int
ast_is_buildin(Ast *ident)
{
  assert(ident);
  AstIdent *_ident = ast_peek_ident(ident);

  uint64_t hash;
  array_foreach(buildin_hashes, hash)
  {
    if (_ident->hash == hash) return (int)i;
  }

  return -1;
}

Ast *
ast_node_dup(Arena *arena, Ast *node)
{
  Ast *tmp = ast_create_node(arena, -1, NULL, Ast *);
#if BL_DEBUG
  int tmp_serial = tmp->_serial;
#endif

  memcpy(tmp, node, sizeof(Ast));
  tmp->next = NULL;
#if BL_DEBUG
  tmp->_serial = tmp_serial;
#endif

  return tmp;
}

Dependency *
ast_add_dep_uq(Ast *decl, Ast *dep, int type)
{
  assert(dep && "invalid dep");
  BHashTable **deps = &ast_peek_decl(decl)->deps;
  Dependency   tmp  = {.node = dep, .type = type};

  if (!*deps) {
    *deps = bo_htbl_new(sizeof(Dependency), 64);
    bo_htbl_insert(*deps, (uint64_t)dep, tmp);
  } else if (!bo_htbl_has_key(*deps, (uint64_t)dep)) {
    bo_htbl_insert(*deps, (uint64_t)dep, tmp);
  }

  return &bo_htbl_at(*deps, (uint64_t)dep, Dependency);
}
