//*****************************************************************************
// Biscuit Engine
//
// File:   parser.c
// Author: Martin Dorazil
// Date:   03/02/2018
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
//*****************************************************************************

#include <setjmp.h>
#include <string.h>
#include <bobject/containers/array.h>
#include "bl/parser.h"
#include "bl/ast/node.h"
#include "bl/pipeline/stage.h"
#include "bl/bldebug.h"
#include "bl/unit.h"

#define parse_error(self, format, ...) \
  { \
    bl_actor_error((Actor *)(self)->unit, (format), ##__VA_ARGS__); \
    longjmp((self)->jmp_error, 1); \
  }

static void
parse_semicolon(Parser *self);

static Node *
parse_global_stmt(Parser *self);

static NodeStmt *
parse_cmp_stmt(Parser *self);

static NodeIfStmt *
parse_if_stmt(Parser *self);

static NodeExpr *
parse_expr(Parser *self);

static NodeExpr *
parse_prim_expr(Parser *self);

static NodeExpr *
parse_expr_1(Parser *self,
             NodeExpr *lhs,
             int min_precedence);

static NodeCall *
parse_call_expr(Parser *self);

static NodeVarDecl *
parse_var_decl(Parser *self);

static NodeFuncDecl *
parse_func_decl(Parser *self);

static NodeParamVarDecl *
parse_param_var_decl(Parser *self);

static NodeReturnStmt *
parse_return_stmt(Parser *self);

static void
reset(Parser *self,
      Unit *unit);

static bool
run(Parser *self,
    Unit *unit);

/* Parser members */
bo_decl_members_begin(Parser, Stage)
  Unit *unit;
  Tokens *tokens;
  BArray *prc_stack;
  BArray *prc_out;

  jmp_buf jmp_error;
bo_end();

/* Parser constructor parameters */
bo_decl_params_with_base_begin(Parser, Stage)
bo_end();

bo_impl_type(Parser, Stage);

/* Parser class init */
void
ParserKlass_init(ParserKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run =
    (bool (*)(Stage *,
              Actor *)) run;
}

/* Parser constructor */
void
Parser_ctor(Parser *self,
            ParserParams *p)
{
  bo_parent_ctor(Stage, p);
  self->prc_stack = bo_array_new(sizeof(bl_token_t *));
  self->prc_out = bo_array_new(sizeof(bl_token_t *));
}

/* Parser destructor */
void
Parser_dtor(Parser *self)
{
  bo_unref(self->prc_stack);
  bo_unref(self->prc_out);
}

/* Parser copy constructor */
bo_copy_result
Parser_copy(Parser *self,
            Parser *other)
{
  return BO_NO_COPY;
}

void
parse_semicolon(Parser *self)
{
  bl_token_t *tok = bl_tokens_consume(self->tokens);
  if (tok->sym != BL_SYM_SEMICOLON) {
    parse_error(self, "%s %d:%d missing semicolon "
      BL_YELLOW("';'")
      " at the end of expression", bl_unit_get_src_file(self->unit), tok->line, tok->col);
  }
}

Node *
parse_global_stmt(Parser *self)
{
  NodeGlobalStmt
    *gstmt =
    bl_ast_node_global_stmt_new(bl_unit_get_ast(self->unit), bl_unit_get_src(self->unit), 1, 0);
stmt:
  if (bl_tokens_consume_if(self->tokens, BL_SYM_SEMICOLON))
    goto stmt;

  if (!bl_node_global_stmt_add_child(gstmt, (Node *) parse_func_decl(self))) {
    bl_token_t *tok = bl_tokens_peek(self->tokens);
    parse_error(self,
                "%s %d:%d expected function declaration",
                bl_unit_get_src_file(self->unit),
                tok->line,
                tok->col);
  }

  if (bl_tokens_current_is_not(self->tokens, BL_SYM_EOF))
    goto stmt;

  return (Node *) gstmt;
}

/*
 * Return statement.
 */
NodeReturnStmt *
parse_return_stmt(Parser *self)
{
  NodeReturnStmt *rstmt = NULL;
  if (bl_tokens_current_is(self->tokens, BL_SYM_RETURN)) {
    bl_token_t *tok = bl_tokens_consume(self->tokens);
    rstmt =
      bl_ast_node_return_stmt_new(bl_unit_get_ast(self->unit), tok->src_loc, tok->line, tok->col);

    /*
     * Here we expect nothing (for void returning functions) or
     * some expression.
     */

    if (bl_tokens_current_is_not(self->tokens, BL_SYM_SEMICOLON)) {
      if (!bl_node_return_stmt_add_expr(rstmt, parse_expr(self))) {
        tok = bl_tokens_consume(self->tokens);
        parse_error(self,
                    "%s %d:%d expected expression or nothing after return statement",
                    bl_unit_get_src_file(self->unit),
                    tok->line,
                    tok->col);
      }
    }

    parse_semicolon(self);
  }
  return rstmt;
}

NodeIfStmt *
parse_if_stmt(Parser *self)
{
  NodeIfStmt *ifstmt = NULL;
  NodeExpr *expr = NULL;
  NodeStmt *then_stmt = NULL;
  NodeStmt *else_stmt = NULL;
  bl_token_t *tok = NULL;

  if (bl_tokens_current_is(self->tokens, BL_SYM_IF)) {
    bl_tokens_consume(self->tokens);

    tok = bl_tokens_consume(self->tokens);
    if (tok->sym != BL_SYM_LPAREN) {
      parse_error(self, "%s %d:%d missing "
        BL_YELLOW("'('")
        " after if statement", bl_unit_get_src_file(self->unit), tok->line, tok->col);
    }

    expr = parse_expr(self);
    if (!expr) {
      parse_error(self,
                  "%s %d:%d expected expression ",
                  bl_unit_get_src_file(self->unit),
                  tok->line,
                  tok->col);
    }

    tok = bl_tokens_consume(self->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(self, "%s %d:%d missing "
        BL_YELLOW("')'")
        " after expression", bl_unit_get_src_file(self->unit), tok->line, tok->col);
    }

    /*
     * Parse then compound statement
     */
    then_stmt = parse_cmp_stmt(self);
    if (!then_stmt) {
      parse_error(self,
                  "%s %d:%d expected if statement body",
                  bl_unit_get_src_file(self->unit),
                  tok->line,
                  tok->col);
    }

    /*
     * Parse else statement if there is one.
     */
    if (bl_tokens_current_is(self->tokens, BL_SYM_ELSE)) {
      tok = bl_tokens_consume(self->tokens);

      else_stmt = parse_cmp_stmt(self);
      if (!else_stmt) {
        parse_error(self,
                    "%s %d:%d expected else statement body",
                    bl_unit_get_src_file(self->unit),
                    tok->line,
                    tok->col);
      }
    }

    ifstmt = bl_ast_node_if_stmt_new(
      bl_unit_get_ast(self->unit), expr, then_stmt, else_stmt, tok->src_loc, tok->line, tok->col);

  }

  return ifstmt;
}

NodeStmt *
parse_cmp_stmt(Parser *self)
{
  /* eat '{' */
  bl_token_t *tok = bl_tokens_consume(self->tokens);
  if (tok->sym != BL_SYM_LBLOCK) {
    parse_error(self, "%s %d:%d expected scope body "
      BL_YELLOW("'{'"), bl_unit_get_src_file(self->unit), tok->line, tok->col);
  }

  NodeStmt
    *stmt = bl_ast_node_stmt_new(bl_unit_get_ast(self->unit), tok->src_loc, tok->line, tok->col);

stmt:
  if (bl_tokens_current_is(self->tokens, BL_SYM_SEMICOLON)) {
    bl_tokens_consume(self->tokens);
    goto stmt;
  }

  /* var decl */
  if (bl_node_stmt_add_child(stmt, (Node *) parse_var_decl(self)))
    goto stmt;

  /* expr */
  if (bl_node_stmt_add_child(stmt, (Node *) parse_expr(self))) {
    parse_semicolon(self);
    goto stmt;
  }

  /* if stmt*/
  if (bl_node_stmt_add_child(stmt, (Node *) parse_if_stmt(self)))
    goto stmt;

  /* return stmt */
  if (bl_node_stmt_add_child(stmt, (Node *) parse_return_stmt(self)))
    goto stmt;

  tok = bl_tokens_consume(self->tokens);

  if (tok->sym != BL_SYM_RBLOCK) parse_error(self, "%s %d:%d expected declaration or scope end "
    BL_YELLOW("'}'"), bl_unit_get_src_file(self->unit), tok->line, tok->col + tok->len);

  return stmt;
}

NodeFuncDecl *
parse_func_decl(Parser *self)
{
  NodeFuncDecl *func_decl = NULL;
  bl_token_t *tok;
  bl_sym_e modif = BL_SYM_NONE;

  /*
   * handle modificators
   */

  /* Store marker in case when current sequence of tokens is not function at all. */
  bl_tokens_set_marker(self->tokens);
  if (bl_tokens_current_is(self->tokens, BL_SYM_EXTERN)) {
    bl_tokens_consume(self->tokens);
    modif = BL_SYM_EXTERN;
  }

  if (bl_tokens_is_seq(
    self->tokens, 3, BL_SYM_IDENT, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    tok = bl_tokens_consume(self->tokens);
    char *type = strndup(tok->content.as_string, tok->len);

    tok = bl_tokens_consume(self->tokens);
    char *ident = strndup(tok->content.as_string, tok->len);

    func_decl = bl_ast_node_func_decl_new(
      bl_unit_get_ast(self->unit), type, ident, modif, tok->src_loc, tok->line, tok->col);

    /*
     * Store the new function into the symbol table.
     */
    SymTbl *sym_tbl = bl_unit_get_sym_tbl(self->unit);
    if (!bl_sym_tbl_register(sym_tbl, (NodeDecl *) func_decl)) {
      parse_error(self,
                  "%s %d:%d function with same name already exists"
                    BL_YELLOW(" '%s'"),
                  bl_unit_get_src_file(self->unit),
                  tok->line,
                  tok->col,
                  bl_ident_get_name(bl_node_decl_get_ident((NodeDecl *) func_decl)));

    }

    /* consume '(' */
    bl_tokens_consume(self->tokens);

    if (bl_tokens_current_is_not(self->tokens, BL_SYM_RPAREN)) {
param:
      bl_node_func_decl_add_param(func_decl, parse_param_var_decl(self));
      if (bl_tokens_consume_if(self->tokens, BL_SYM_COMMA))
        goto param;
    }

    tok = bl_tokens_consume(self->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(self,
                  "%s %d:%d expected "
                    BL_YELLOW("')'")
                    " after function parameter declaration",
                  bl_unit_get_src_file(self->unit),
                  tok->line,
                  tok->col);
    }

    if (modif == BL_SYM_EXTERN) {
      tok = bl_tokens_consume(self->tokens);
      if (tok->sym != BL_SYM_SEMICOLON) {
        parse_error(self,
                    "%s %d:%d missing semicolon "
                      BL_YELLOW("';'")
                      " at the end of extern function definition",
                    bl_unit_get_src_file(self->unit),
                    tok->line,
                    tok->col);
      }
    } else {
      bl_node_func_decl_set_stmt(func_decl, parse_cmp_stmt(self));
    }
  } else {
    /* Roll back to marker. */
    bl_tokens_back_to_marker(self->tokens);
  }

  return func_decl;
}

NodeParamVarDecl *
parse_param_var_decl(Parser *self)
{
  bl_token_t *tok = bl_tokens_consume(self->tokens);
  if (tok->sym != BL_SYM_IDENT) parse_error(self,
                                            "%s %d:%d expected parameter type",
                                            bl_unit_get_src_file(self->unit),
                                            tok->line,
                                            tok->col);

  char *type = strndup(tok->content.as_string, tok->len);

  tok = bl_tokens_consume(self->tokens);
  if (tok->sym != BL_SYM_IDENT) {
    free(type);
    parse_error(self,
                "%s %d:%d expected parameter name",
                bl_unit_get_src_file(self->unit),
                tok->line,
                tok->col);
  }

  char *ident = strndup(tok->content.as_string, tok->len);
  return bl_ast_node_param_var_decl_new(
    bl_unit_get_ast(self->unit), type, ident, tok->src_loc, tok->line, tok->col);
}

NodeExpr *
parse_expr(Parser *self)
{
  return parse_expr_1(self, parse_prim_expr(self), 0);
}

NodeExpr *
parse_prim_expr(Parser *self)
{
  /*
   * Parse
   * 1. call expression
   * 2. variable reference expression
   * 3. binary expression
   */

  NodeExpr *expr = NULL;

  bl_token_t *tok = bl_tokens_peek(self->tokens);
  switch (tok->sym) {
    case BL_SYM_NUM:
      /* Numeric constant */
      bl_tokens_consume(self->tokens);

      expr = (NodeExpr *) bl_ast_node_const_new(
        bl_unit_get_ast(self->unit), tok->src_loc, tok->line, tok->col);
      bl_node_const_set_int((NodeConst *) expr, tok->content.as_ull);
      break;
    default:
      break;
  }

  return expr;
}

NodeExpr *
parse_expr_1(Parser *self,
             NodeExpr *lhs,
             int min_precedence)
{
  NodeExpr *rhs = NULL;
  bl_token_t *lookahead = bl_tokens_peek(self->tokens);
  bl_token_t *op = NULL;

  while (bl_token_prec(lookahead) >= min_precedence) {
    op = lookahead;
    bl_tokens_consume(self->tokens);
    rhs = parse_prim_expr(self);
    lookahead = bl_tokens_peek(self->tokens);

    while ((bl_token_prec(lookahead) > bl_token_prec(op)) ||
      (lookahead->sym == BL_SYM_ASIGN && bl_token_prec(lookahead) == bl_token_prec(op))) {
      rhs = parse_expr_1(self, rhs, bl_token_prec(lookahead));
      lookahead = bl_tokens_peek(self->tokens);
    }

    NodeExpr *tmp = lhs;
    lhs = (NodeExpr *) bl_ast_node_binop_new(
      bl_unit_get_ast(self->unit), op->sym, op->src_loc, op->line, op->col);

    bl_node_binop_set_lhs((NodeBinop *) lhs, tmp);
    bl_node_binop_set_rhs((NodeBinop *) lhs, rhs);
  }

  return lhs;
}

NodeCall *
parse_call_expr(Parser *self)
{
  NodeCall *call = NULL;

  if (bl_tokens_is_seq(self->tokens, 2, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    bl_token_t *tok_calle = bl_tokens_consume(self->tokens);

    /* eat '(' */
    bl_tokens_consume(self->tokens);

    call = bl_ast_node_call_new(
      bl_unit_get_ast(self->unit),
      strndup(tok_calle->content.as_string, tok_calle->len),
      tok_calle->src_loc,
      tok_calle->line,
      tok_calle->col);

    /*
     * Handle callee existence, when symbol was found, store expected return
     * type into call node. When no callee was found, it can be defined later
     * or in another unit in assembly, in such case we only store unsatisfied
     * call into cache and add information about return type later.
     */
    SymTbl *sym_tbl = bl_unit_get_sym_tbl(self->unit);
    Ident *ident = bl_node_call_get_ident(call);
    NodeDecl *callee = bl_sym_tbl_get_sym_of_type(sym_tbl, ident, BL_NODE_FUNC_DECL);
    if (callee == NULL) {
      bl_sym_tbl_add_unsatisfied_expr(sym_tbl, call);
    } else {
      bl_node_call_get_set_callee(call, (NodeFuncDecl *) callee);
    }

arg:
    bl_node_call_add_arg(call, parse_expr(self));
    if (bl_tokens_consume_if(self->tokens, BL_SYM_COMMA))
      goto arg;

    bl_token_t *tok = bl_tokens_consume(self->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(self,
                  "%s %d:%d expected "
                    BL_YELLOW("')'")
                    " after function call argument list",
                  bl_unit_get_src_file(self->unit),
                  tok->line,
                  tok->col);
    }
  }

  return call;
}

NodeVarDecl *
parse_var_decl(Parser *self)
{
  NodeVarDecl *vdcl = NULL;

  if (bl_tokens_is_seq(self->tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    bl_token_t *tok_type = bl_tokens_consume(self->tokens);
    bl_token_t *tok_ident = bl_tokens_consume(self->tokens);

    if (bl_tokens_current_is(self->tokens, BL_SYM_SEMICOLON)) {
      /* declaration only */
      char *type = strndup(tok_type->content.as_string, tok_type->len);
      char *ident = strndup(tok_ident->content.as_string, tok_ident->len);
      vdcl = bl_ast_node_var_decl_new(
        bl_unit_get_ast(self->unit),
        type,
        ident,
        tok_ident->src_loc,
        tok_ident->line,
        tok_ident->col);
    } else if (bl_tokens_consume_if(self->tokens, BL_SYM_ASIGN)) {
      /*
       * Variable is also asigned to some expression.
       */
      char *type = strndup(tok_type->content.as_string, tok_type->len);
      char *ident = strndup(tok_ident->content.as_string, tok_ident->len);
      vdcl = bl_ast_node_var_decl_new(
        bl_unit_get_ast(self->unit),
        type,
        ident,
        tok_ident->src_loc,
        tok_ident->line,
        tok_ident->col);

      /* expected expression */
      if (!bl_node_var_decl_set_expr(vdcl, parse_expr(self))) {
        parse_error(self,
                    "%s %d:%d expected expression after "
                      BL_YELLOW("'='"),
                    bl_unit_get_src_file(self->unit),
                    tok_ident->line,
                    tok_ident->col + tok_ident->len);
      }
    }

    /* always must end with semicolon */
    if (bl_tokens_consume(self->tokens)->sym != BL_SYM_SEMICOLON) {
      parse_error(self, "%s %d:%d missing semicolon "
        BL_YELLOW("';'")
        " at the end of variable declaration", bl_unit_get_src_file(
        self->unit), tok_ident->line, tok_ident->col + tok_ident->len);
    }

  }

  return vdcl;
}

void
reset(Parser *self,
      Unit *unit)
{
  if (bl_unit_get_tokens(unit) == NULL) {
    parse_error(self, "no tokens found for unit");
  }

  bl_unit_set_ast(unit, bl_ast_new());

  self->unit = unit;
  self->tokens = bl_unit_get_tokens(unit);

  bo_array_clear(self->prc_stack);
  bo_array_clear(self->prc_out);
}

bool
run(Parser *self,
    Unit *unit)
{
  if (setjmp(self->jmp_error))
    return false;

  reset(self, unit);

  bl_ast_set_root(bl_unit_get_ast(unit), parse_global_stmt(self));

  /* TODO: move to another stage??? */
  if (!bl_sym_tbl_try_satisfy_all(bl_unit_get_sym_tbl(unit))) {
    parse_error(self, "%s unknown function detected.", bl_unit_get_src_file(unit));
  }

  return true;
}

/* public */
Parser *
bl_parser_new(bl_compile_group_e group)
{
  ParserParams p = {.base.group = group};

  return bo_new(Parser, &p);
}


