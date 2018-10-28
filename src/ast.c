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

AstExpr *
_ast_create_expr(struct Arena *arena, AstExprKind c, Token *tok)
{
  AstExpr *expr = (AstExpr *)_ast_create_node(arena, AST_EXPR, tok);
  expr->kind    = c;
  return expr;
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
    case AST_TYPE_VOID:
      return "TypeVoid";
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

  case AST_EXPR: {
    switch (ast_expr_kind(&n->expr)) {
    case AST_EXPR_BAD:
      return "ExprBad";
    case AST_EXPR_TYPE:
      return "ExprType";
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
    case AST_EXPR_LIT_FN:
      return "ExprLitFn";
    case AST_EXPR_LIT_INT:
      return "ExprLitInt";
    case AST_EXPR_LIT_FLOAT:
      return "ExprLitFloat";
    case AST_EXPR_LIT_CHAR:
      return "ExprLitChar";
    case AST_EXPR_LIT_STRING:
      return "ExprLitString";
    case AST_EXPR_LIT_BOOL:
      return "ExprLitBool";
    case AST_EXPR_LIT_CMP:
      return "ExprLitCmp";
    }
  }

  default:
    bl_abort("invalid ast node");
  }
}

static void
_type_to_str(char *buf, size_t len, AstType *type)
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

  case AST_TYPE_FN: {
    AstTypeFn *fn  = &type->fn;
    Ast *      arg = fn->args;

    append_buf(buf, len, "fn (");
    while (arg) {
      _type_to_str(buf, len, arg->arg.type);
      arg = arg->next;
      if (arg) append_buf(buf, len, ", ");
    }
    append_buf(buf, len, ") ");
    _type_to_str(buf, len, fn->ret_type);
    break;
  }

  case AST_TYPE_REF:
    _type_to_str(buf, len, type->ref.type);
    break;

  case AST_TYPE_STRUCT: {
    AstTypeStruct *strct = &type->strct;
    Ast *          mem   = strct->members;

    append_buf(buf, len, "{");
    while (mem) {
      _type_to_str(buf, len, mem->member.type);
      mem = mem->next;
      if (mem) append_buf(buf, len, ", ");
    }
    append_buf(buf, len, "}");
    break;
  }

  case AST_TYPE_VOID:
    append_buf(buf, len, type->mvoid.name);
    break;

  case AST_TYPE_VARGS:
  case AST_TYPE_ARR:
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
  _type_to_str(buf, len, type);
}
