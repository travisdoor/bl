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

Ast ast_buildin_types[AST_BUILDIN_TYPE_COUNT] = {
    // u8
    {.code               = AST_TYPE_INT,
     .src                = NULL,
     .next               = NULL,
     .type_int.base.name = "u8",
     .type_int.bitcount  = 8,
     .type_int.is_signed = false},

    // u16
    {.code               = AST_TYPE_INT,
     .src                = NULL,
     .next               = NULL,
     .type_int.base.name = "u16",
     .type_int.bitcount  = 16,
     .type_int.is_signed = false},

    // u32
    {.code               = AST_TYPE_INT,
     .src                = NULL,
     .next               = NULL,
     .type_int.base.name = "u32",
     .type_int.bitcount  = 32,
     .type_int.is_signed = false},

    // u64
    {.code               = AST_TYPE_INT,
     .src                = NULL,
     .next               = NULL,
     .type_int.base.name = "u64",
     .type_int.bitcount  = 64,
     .type_int.is_signed = false},

    // usize
    {.code               = AST_TYPE_INT,
     .src                = NULL,
     .next               = NULL,
     .type_int.base.name = "usize",
     .type_int.bitcount  = 64,
     .type_int.is_signed = false},

    // s8
    {.code               = AST_TYPE_INT,
     .src                = NULL,
     .next               = NULL,
     .type_int.base.name = "s8",
     .type_int.bitcount  = 8,
     .type_int.is_signed = true},

    // s16
    {.code               = AST_TYPE_INT,
     .src                = NULL,
     .next               = NULL,
     .type_int.base.name = "s16",
     .type_int.bitcount  = 16,
     .type_int.is_signed = true},

    // s32
    {.code               = AST_TYPE_INT,
     .src                = NULL,
     .next               = NULL,
     .type_int.base.name = "s32",
     .type_int.bitcount  = 32,
     .type_int.is_signed = true},

    // s64
    {.code               = AST_TYPE_INT,
     .src                = NULL,
     .next               = NULL,
     .type_int.base.name = "s64",
     .type_int.bitcount  = 64,
     .type_int.is_signed = true},
};

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

AstType *
ast_get_buildin_type(AstBuildinType c)
{
  assert(c >= 0 && c < AST_BUILDIN_TYPE_COUNT);
  return (AstType *)&ast_buildin_types[c];
}

Ast *
ast_dup(Arena *arena,
        Ast *node)
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

const char *
ast_get_name(Ast *n)
{
  assert(n);
  switch (ast_code(n)) {
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
  case AST_TYPE_TYPE:
    return "TypeType";
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
  switch (ast_code(n)) {
  case AST_IDENT:
    return ast_peek_ident(n)->type;
  case AST_LIT_INT:
    return ast_peek_lit_int(n)->type;
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
  if (visitor->visitors[ast_code(node)])
    visitor->visitors[ast_code(node)](visitor, node, cnt);
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
  switch (ast_code(node)) {

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
    visit((Ast *)_decl->name);
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
