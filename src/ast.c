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

#include "ast.h"
#include "arena.h"

#define ARENA_CHUNK_COUNT 256

static void
node_dtor(Ast *node)
{
  switch (node->kind) {
  case AST_DECL:
    bo_unref(((AstDecl *)node)->deps);
    break;
  default:
    break;
  }
}

Ast *
_ast_create_node(Arena *arena, AstKind c, Token *tok)
{
  Ast *node  = arena_alloc(arena);
  node->kind = c;
  node->src  = tok ? &tok->src : NULL;

#if BL_DEBUG
  static int serial = 0;
  node->_serial     = serial++;
  node->_state      = NOT_CHECKED;
#endif
  return node;
}

AstType *
_ast_create_type(struct Arena *arena, AstTypeKind c, Token *tok)
{
  AstType *type = (AstType *)_ast_create_node(arena, AST_TYPE, tok);
  type->kind    = c;
  return type;
}

/* public */
void
ast_arena_init(struct Arena *arena)
{
  arena_init(arena, sizeof(Ast), ARENA_CHUNK_COUNT, (ArenaElemDtor)node_dtor);
}

Ast *
ast_dup(Arena *arena, Ast *node)
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
ast_add_dep_uq(AstDecl *decl, AstDecl *dep, int type)
{
  assert(dep && "invalid dep");
  BHashTable **deps = &decl->deps;
  Dependency   tmp  = {.decl = dep, .type = type};

  if (!*deps) {
    *deps = bo_htbl_new(sizeof(Dependency), 64);
    bo_htbl_insert(*deps, (uint64_t)dep, tmp);
  } else if (!bo_htbl_has_key(*deps, (uint64_t)dep)) {
    bo_htbl_insert(*deps, (uint64_t)dep, tmp);
  }

  return &bo_htbl_at(*deps, (uint64_t)dep, Dependency);
}

const char *
ast_get_name(Ast *n)
{
  assert(n);
  switch (ast_kind(n)) {
  case AST_BAD:
    return "Bad";
  case AST_LOAD:
    return "Load";
  case AST_LINK:
    return "Link";
  case AST_IDENT:
    return "Ident";
  case AST_UBLOCK:
    return "UBlock";
  case AST_BLOCK:
    return "Block";
  case AST_STMT_RETURN:
    return "StmtReturn";
  case AST_STMT_IF:
    return "StmtIf";
  case AST_STMT_LOOP:
    return "StmtLoop";
  case AST_STMT_BREAK:
    return "StmtBreak";
  case AST_STMT_CONTINUE:
    return "StmtContinue";
  case AST_DECL:
    return "Decl";
  case AST_MEMBER:
    return "Member";
  case AST_ARG:
    return "Arg";
  case AST_VARIANT:
    return "Variant";
  case AST_TYPE: {
    switch (ast_type_kind(&n->type)) {
    case AST_TYPE_BAD:
      return "TypeBad";
    case AST_TYPE_TYPE:
      return "TypeType";
    case AST_TYPE_REF:
      return "TypeRef";
    case AST_TYPE_INT:
      return "TypeInt";
    case AST_TYPE_VARGS:
      return "TypeVArgs";
    case AST_TYPE_ARR:
      return "TypeArr";
    case AST_TYPE_FN:
      return "TypeFn";
    case AST_TYPE_STRUCT:
      return "TypeStruct";
    case AST_TYPE_ENUM:
      return "TypeEnum";
    case AST_TYPE_PTR:
      return "TypePtr";
    }
  }
  case AST_LIT_FN:
    return "LitFn";
  case AST_LIT_INT:
    return "LitInt";
  case AST_LIT_FLOAT:
    return "LitFloat";
  case AST_LIT_CHAR:
    return "LitChar";
  case AST_LIT_STRING:
    return "LitString";
  case AST_LIT_BOOL:
    return "LitBool";
  case AST_LIT_CMP:
    return "LitCmp";
  case AST_EXPR_REF:
    return "ExprRef";
  case AST_EXPR_CAST:
    return "ExprCast";
  case AST_EXPR_BINOP:
    return "ExprBinop";
  case AST_EXPR_CALL:
    return "ExprCall";
  case AST_EXPR_MEMBER:
    return "ExprMember";
  case AST_EXPR_ELEM:
    return "ExprElem";
  case AST_EXPR_SIZEOF:
    return "ExprSizeof";
  case AST_EXPR_TYPEOF:
    return "ExprTypeof";
  case AST_EXPR_UNARY:
    return "ExprUnary";
  case AST_EXPR_NULL:
    return "ExprNull";
  case AST_COUNT:
    break;
  }

  bl_abort("invalid ast node");
}

AstType *
ast_get_type(Ast *n)
{
  assert(n);
  switch (ast_kind(n)) {
  case AST_EXPR_REF:
    return ((AstExprRef *)n)->type;
  case AST_TYPE_REF:
    return ((AstTypeRef *)n)->type;
  case AST_LIT_INT:
    return ((AstLitInt *)n)->type;
  case AST_LIT_FN:
    return ((AstLitFn *)n)->type;
  case AST_TYPE:
  case AST_TYPE_FN:
    return (AstType *)n;
  default:
    bl_abort("node has no type %s", ast_get_name(n));
  }
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
visitor_add(Visitor *visitor, VisitorFunc fn, AstKind kind)
{
  visitor->visitors[kind] = fn;
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
  if (visitor->visitors[ast_kind(node)])
    visitor->visitors[ast_kind(node)](visitor, node, cnt);
  else
    visitor_walk(visitor, node, cnt);
}

void
visitor_walk(Visitor *visitor, Ast *node, void *cnt)
{
#define visit(node) visitor_visit(visitor, (Ast *)node, cnt)
  if (!node) return;
  Ast *tmp = NULL;

  if (!node) return;
  switch (ast_kind(node)) {

  case AST_UBLOCK: {
    node_foreach(((AstUBlock *)node)->nodes, tmp) visit(tmp);
    break;
  }

  case AST_BLOCK: {
    node_foreach(((AstBlock *)node)->nodes, tmp) visit(tmp);
    break;
  }

  case AST_DECL: {
    AstDecl *_decl = (AstDecl *)node;
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
    AstVariant *_var = (AstVariant *)node;
    visit(_var->value);
    break;
  }

  case AST_EXPR_BINOP: {
    AstExprBinop *_binop = (AstExprBinop *)node;
    visit(_binop->lhs);
    visit(_binop->rhs);
    break;
  }

  case AST_EXPR_CALL: {
    AstExprCall *_call = (AstExprCall *)node;
    node_foreach(_call->args, tmp) visit(tmp);
    break;
  }

  case AST_EXPR_CAST: {
    visit(((AstExprCast *)node)->next);
    break;
  }

  case AST_EXPR_UNARY: {
    visit(((AstExprUnary *)node)->next);
    break;
  }

  case AST_EXPR_MEMBER: {
    visit(((AstExprMember *)node)->next);
    break;
  }
  case AST_EXPR_ELEM: {
    AstExprElem *_elem = (AstExprElem *)node;
    visit(_elem->index);
    visit(_elem->next);
    break;
  }

  case AST_STMT_RETURN: {
    visit(((AstStmtReturn *)node)->expr);
    break;
  }

  case AST_STMT_IF: {
    AstStmtIf *_if = (AstStmtIf *)node;
    visit(_if->test);
    visit(_if->true_stmt);
    visit(_if->false_stmt);
    break;
  }

  case AST_STMT_LOOP: {
    AstStmtLoop *_loop = (AstStmtLoop *)node;
    visit(_loop->init);
    visit(_loop->condition);
    visit(_loop->increment);
    visit(_loop->block);
    break;
  }

  case AST_LIT_CMP: {
    AstLitCmp *_lit_cmp = (AstLitCmp *)node;
    visit(_lit_cmp->type);
    node_foreach(_lit_cmp->fields, tmp) visit(tmp);
    break;
  }

  case AST_LIT_FN: {
    visit(((AstLitFn *)node)->block);
    break;
  }

  /* defaults (terminal cases) */
  default:
    break;
  }

#undef visit
}

static void
_type_to_string(char *buf, size_t len, AstType *type)
{
#define append_buf(buf, len, str)                                                                  \
  {                                                                                                \
    const size_t filled = strlen(buf);                                                             \
    snprintf((buf) + filled, (len)-filled, "%s", str);                                             \
  }

  if (!buf) return;
  if (!type) {
    append_buf(buf, len, "?");
    return;
  }

  assert(ast_is_type((Ast *)type));

  switch (ast_type_kind(type)) {
  case AST_TYPE_BAD:
    append_buf(buf, len, "BAD");
    break;
  case AST_TYPE_TYPE:
    append_buf(buf, len, type->type.name);
    break;
  case AST_TYPE_INT:
    append_buf(buf, len, type->integer.name);
    break;
  case AST_TYPE_FN:
    append_buf(buf, len, "fn");
    break;
  case AST_TYPE_REF:
    _type_to_string(buf, len, type->ref.type);
    break;

  case AST_TYPE_VARGS:
  case AST_TYPE_ARR:
  case AST_TYPE_STRUCT:
  case AST_TYPE_ENUM:
  case AST_TYPE_PTR:
    bl_abort("unimplemented %s", ast_get_name((Ast *)type));
  }
}

void
ast_type_to_str(char *buf, int len, AstType *type)
{
  if (!buf || !len) return;
  buf[0] = '\0';
  _type_to_string(buf, len, type);
}
