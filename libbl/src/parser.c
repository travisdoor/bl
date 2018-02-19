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
#include "bl/parser.h"
#include "bl/ast/node.h"
#include "bl/pipeline/stage.h"
#include "bl/bldebug.h"
#include "bl/unit.h"

#define parse_error(cnt, format, ...) \
  { \
    bl_actor_error((Actor *)(cnt)->unit, (format), ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, 1); \
  }

typedef struct _context_t
{
  Unit *unit;
  Tokens *tokens;
  jmp_buf jmp_error;
} context_t;

static Node *
parse_global_stmt(context_t *cnt);

static NodeStmt *
parse_stmt(context_t *cnt);

static NodeIfStmt *
parse_if_stmt(context_t *cnt);

static NodeExpr *
parse_expr(context_t *cnt);

static NodeCall *
parse_call_expr(context_t *cnt);

static NodeVarDecl *
parse_var_decl(context_t *cnt);

static NodeFuncDecl *
parse_func_decl(context_t *cnt);

static NodeParamVarDecl *
parse_param_var_decl(context_t *cnt);

static NodeReturnStmt *
parse_return_stmt(context_t *cnt);

static NodeBinop *
parse_binop(context_t *cnt,
            NodeExpr *lvalue);

static bool
run(Parser *self,
    Unit *unit);

/* Parser members */
bo_decl_members_begin(Parser, Stage)
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
}

/* Parser destructor */
void
Parser_dtor(Parser *self)
{
}

/* Parser copy constructor */
bo_copy_result
Parser_copy(Parser *self,
            Parser *other)
{
  return BO_NO_COPY;
}

Node *
parse_global_stmt(context_t *cnt)
{
  NodeGlobalStmt
    *gstmt =
    bl_ast_node_global_stmt_new(bl_unit_get_ast(cnt->unit), bl_unit_get_src(cnt->unit), 1, 0);
stmt:
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_SEMICOLON))
    goto stmt;

  if (!bl_node_global_stmt_add_child(gstmt, (Node *) parse_func_decl(cnt))) {
    bl_token_t *tok = bl_tokens_peek(cnt->tokens);
    parse_error(cnt,
                "%s %d:%d expected function declaration",
                bl_unit_get_src_file(cnt->unit),
                tok->line,
                tok->col);
  }

  if (bl_tokens_current_is_not(cnt->tokens, BL_SYM_EOF))
    goto stmt;

  return (Node *) gstmt;
}

/*
 * Return statement.
 */
NodeReturnStmt *
parse_return_stmt(context_t *cnt)
{
  NodeReturnStmt *rstmt = NULL;
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_RETURN)) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    rstmt =
      bl_ast_node_return_stmt_new(bl_unit_get_ast(cnt->unit), tok->src_loc, tok->line, tok->col);

    /*
     * Here we expect nothing (for void returning functions) or
     * some expression.
     */

    if (bl_tokens_current_is_not(cnt->tokens, BL_SYM_SEMICOLON)) {
      if (!bl_node_return_stmt_add_expr(rstmt, parse_expr(cnt))) {
        tok = bl_tokens_consume(cnt->tokens);
        parse_error(cnt,
                    "%s %d:%d expected expression or nothing after return statement",
                    bl_unit_get_src_file(cnt->unit),
                    tok->line,
                    tok->col);
      }
    }

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_SEMICOLON) {
      parse_error(cnt, "%s %d:%d missing semicolon "
        BL_YELLOW("';'")
        " at the end of return statement", bl_unit_get_src_file(cnt->unit), tok->line, tok->col);
    }
  }
  return rstmt;
}

static NodeBinop *
parse_binop(context_t *cnt,
            NodeExpr *lvalue)
{
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  NodeBinop
    *binop =
    bl_ast_node_binop_new(bl_unit_get_ast(cnt->unit), tok->sym, tok->src_loc, tok->line, tok->col);

  bl_node_binop_set_lvalue(binop, lvalue);
  NodeExpr *rvalue = parse_expr(cnt);
  if (rvalue == NULL) {
    parse_error(cnt,
                "%s %d:%d expected rvalue ",
                bl_unit_get_src_file(cnt->unit),
                tok->line,
                tok->col);
  }
  bl_node_binop_set_rvalue(binop, rvalue);

  return binop;
}

NodeIfStmt *
parse_if_stmt(context_t *cnt)
{
  NodeIfStmt *ifstmt = NULL;
  NodeExpr *expr = NULL;
  NodeStmt *then_stmt = NULL;
  NodeStmt *else_stmt = NULL;

  if (bl_tokens_current_is(cnt->tokens, BL_SYM_IF)) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_LPAREN) {
      parse_error(cnt, "%s %d:%d missing "
        BL_YELLOW("'('")
        " after if statement", bl_unit_get_src_file(cnt->unit), tok->line, tok->col);
    }

    expr = parse_expr(cnt);
    if (!expr) {
      parse_error(cnt,
                  "%s %d:%d expected expression ",
                  bl_unit_get_src_file(cnt->unit),
                  tok->line,
                  tok->col);
    }

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(cnt, "%s %d:%d missing "
        BL_YELLOW("')'")
        " after expression", bl_unit_get_src_file(cnt->unit), tok->line, tok->col);
    }

    /*
     * Parse then compound statement
     */
    then_stmt = parse_stmt(cnt);
    if (!then_stmt) {
      parse_error(cnt,
                  "%s %d:%d expected if statement body",
                  bl_unit_get_src_file(cnt->unit),
                  tok->line,
                  tok->col);
    }

    /*
     * Parse else statement if there is one.
     */
    if (bl_tokens_current_is(cnt->tokens, BL_SYM_ELSE)) {
      tok = bl_tokens_consume(cnt->tokens);

      else_stmt = parse_stmt(cnt);
      if (!else_stmt) {
        parse_error(cnt,
                    "%s %d:%d expected else statement body",
                    bl_unit_get_src_file(cnt->unit),
                    tok->line,
                    tok->col);
      }
    }

    ifstmt = bl_ast_node_if_stmt_new(
      bl_unit_get_ast(cnt->unit), expr, then_stmt, else_stmt, tok->src_loc, tok->line, tok->col);

  }

  return ifstmt;
}

NodeStmt *
parse_stmt(context_t *cnt)
{
  /* eat '{' */
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_LBLOCK) {
    parse_error(cnt, "%s %d:%d expected scope body "
      BL_YELLOW("'{'"), bl_unit_get_src_file(cnt->unit), tok->line, tok->col);
  }

  NodeStmt
    *stmt = bl_ast_node_stmt_new(bl_unit_get_ast(cnt->unit), tok->src_loc, tok->line, tok->col);

stmt:
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
    bl_tokens_consume(cnt->tokens);
    goto stmt;
  }

  /* var decl */
  if (bl_node_stmt_add_child(stmt, (Node *) parse_var_decl(cnt)))
    goto stmt;

  /* expr */
  if (bl_node_stmt_add_child(stmt, (Node *) parse_expr(cnt)))
    goto stmt;

  /* if stmt*/
  if (bl_node_stmt_add_child(stmt, (Node *) parse_if_stmt(cnt)))
    goto stmt;

  /* return stmt */
  if (bl_node_stmt_add_child(stmt, (Node *) parse_return_stmt(cnt)))
    goto stmt;

  /* expr */
  if (bl_node_stmt_add_child(stmt, (Node *) parse_expr(cnt))) {
    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_SEMICOLON) {
      parse_error(cnt, "%s %d:%d missing semicolon "
        BL_YELLOW("';'")
        " at the end of expression", bl_unit_get_src_file(cnt->unit), tok->line, tok->col);
    }
    goto stmt;
  }

  tok = bl_tokens_consume(cnt->tokens);

  if (tok->sym != BL_SYM_RBLOCK) parse_error(cnt, "%s %d:%d expected declaration or scope end "
    BL_YELLOW("'}'"), bl_unit_get_src_file(cnt->unit), tok->line, tok->col + tok->len);

  return stmt;
}

NodeFuncDecl *
parse_func_decl(context_t *cnt)
{
  NodeFuncDecl *func_decl = NULL;
  bl_token_t *tok;
  bl_sym_e modif = BL_SYM_NONE;

  /*
   * handle modificators
   */

  /* Store marker in case when current sequence of tokens is not function at all. */
  bl_tokens_set_marker(cnt->tokens);
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_EXTERN)) {
    bl_tokens_consume(cnt->tokens);
    modif = BL_SYM_EXTERN;
  }

  if (bl_tokens_is_seq(
    cnt->tokens, 3, BL_SYM_IDENT, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    tok = bl_tokens_consume(cnt->tokens);
    char *type = strndup(tok->content.as_string, tok->len);

    tok = bl_tokens_consume(cnt->tokens);
    char *ident = strndup(tok->content.as_string, tok->len);

    func_decl = bl_ast_node_func_decl_new(
      bl_unit_get_ast(cnt->unit), type, ident, modif, tok->src_loc, tok->line, tok->col);

    /*
     * Store the new function into the symbol table.
     */
    SymTbl *sym_tbl = bl_unit_get_sym_tbl(cnt->unit);
    if (!bl_sym_tbl_register(sym_tbl, (NodeDecl *) func_decl)) {
      parse_error(cnt,
                  "%s %d:%d function with same name already exists"
                    BL_YELLOW(" '%s'"),
                  bl_unit_get_src_file(cnt->unit),
                  tok->line,
                  tok->col,
                  bl_ident_get_name(bl_node_decl_get_ident((NodeDecl *) func_decl)));

    }

    /* consume '(' */
    bl_tokens_consume(cnt->tokens);

    if (bl_tokens_current_is_not(cnt->tokens, BL_SYM_RPAREN)) {
param:
      bl_node_func_decl_add_param(func_decl, parse_param_var_decl(cnt));
      if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA))
        goto param;
    }

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(cnt,
                  "%s %d:%d expected "
                    BL_YELLOW("')'")
                    " after function parameter declaration",
                  bl_unit_get_src_file(cnt->unit),
                  tok->line,
                  tok->col);
    }

    if (modif == BL_SYM_EXTERN) {
      tok = bl_tokens_consume(cnt->tokens);
      if (tok->sym != BL_SYM_SEMICOLON) {
        parse_error(cnt,
                    "%s %d:%d missing semicolon "
                      BL_YELLOW("';'")
                      " at the end of extern function definition",
                    bl_unit_get_src_file(cnt->unit),
                    tok->line,
                    tok->col);
      }
    } else {
      bl_node_func_decl_set_stmt(func_decl, parse_stmt(cnt));
    }
  } else {
    /* Roll back to marker. */
    bl_tokens_back_to_marker(cnt->tokens);
  }

  return func_decl;
}

NodeParamVarDecl *
parse_param_var_decl(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_IDENT) parse_error(cnt,
                                            "%s %d:%d expected parameter type",
                                            bl_unit_get_src_file(cnt->unit),
                                            tok->line,
                                            tok->col);

  char *type = strndup(tok->content.as_string, tok->len);

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_IDENT) {
    free(type);
    parse_error(cnt,
                "%s %d:%d expected parameter name",
                bl_unit_get_src_file(cnt->unit),
                tok->line,
                tok->col);
  }

  char *ident = strndup(tok->content.as_string, tok->len);
  return bl_ast_node_param_var_decl_new(
    bl_unit_get_ast(cnt->unit), type, ident, tok->src_loc, tok->line, tok->col);
}

NodeExpr *
parse_expr(context_t *cnt)
{
  NodeExpr *expr = NULL;

  bl_token_t *tok = bl_tokens_peek(cnt->tokens);
  switch (tok->sym) {
    case BL_SYM_IDENT:
      expr = (NodeExpr *) parse_call_expr(cnt);
      if (expr)
        break;

      bl_tokens_consume(cnt->tokens);

      expr = (NodeExpr *) bl_ast_node_decl_ref_new(
        bl_unit_get_ast(cnt->unit),
        strndup(tok->content.as_string, tok->len),
        tok->src_loc,
        tok->line,
        tok->col);

      break;
    case BL_SYM_NUM:
      bl_tokens_consume(cnt->tokens);

      expr = (NodeExpr *) bl_ast_node_const_new(
        bl_unit_get_ast(cnt->unit), tok->src_loc, tok->line, tok->col);
      bl_node_const_set_int((NodeConst *) expr, tok->content.as_int);
      break;
    case BL_SYM_TRUE:
      bl_tokens_consume(cnt->tokens);

      expr = (NodeExpr *) bl_ast_node_const_new(
        bl_unit_get_ast(cnt->unit), tok->src_loc, tok->line, tok->col);
      bl_node_const_set_bool((NodeConst *) expr, true);
      break;
    case BL_SYM_FALSE:
      bl_tokens_consume(cnt->tokens);

      expr = (NodeExpr *) bl_ast_node_const_new(
        bl_unit_get_ast(cnt->unit), tok->src_loc, tok->line, tok->col);
      bl_node_const_set_bool((NodeConst *) expr, false);
      break;
    case BL_SYM_STRING:
      bl_tokens_consume(cnt->tokens);

      expr = (NodeExpr *) bl_ast_node_const_new(
        bl_unit_get_ast(cnt->unit), tok->src_loc, tok->line, tok->col);

      bl_node_const_set_str((NodeConst *) expr, strndup(tok->content.as_string, tok->len));
      break;
    default:
      break;
  }

  /* TODO: accept more operators */
  if (expr && bl_token_is_binop(bl_tokens_peek(cnt->tokens))) {
    NodeBinop *binop = parse_binop(cnt, expr);
    if (binop != NULL) {
      expr = (NodeExpr *) binop;
    }
  }

  return expr;
}

NodeCall *
parse_call_expr(context_t *cnt)
{
  NodeCall *call = NULL;

  if (bl_tokens_is_seq(cnt->tokens, 2, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    bl_token_t *tok_calle = bl_tokens_consume(cnt->tokens);

    /* eat '(' */
    bl_tokens_consume(cnt->tokens);

    call = bl_ast_node_call_new(
      bl_unit_get_ast(cnt->unit),
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
    SymTbl *sym_tbl = bl_unit_get_sym_tbl(cnt->unit);
    Ident *ident = bl_node_call_get_ident(call);
    NodeDecl *callee = bl_sym_tbl_get_sym_of_type(sym_tbl, ident, BL_NODE_FUNC_DECL);
    if (callee == NULL) {
      bl_sym_tbl_add_unsatisfied_expr(sym_tbl, call);
    } else {
      bl_node_call_get_set_callee(call, (NodeFuncDecl *) callee);
    }

arg:
    bl_node_call_add_arg(call, parse_expr(cnt));
    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA))
      goto arg;

    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(cnt, "%s %d:%d expected "
        BL_YELLOW("')'")
        " after function call argument list", bl_unit_get_src_file(cnt->unit), tok->line, tok->col);
    }
  }

  return call;
}

NodeVarDecl *
parse_var_decl(context_t *cnt)
{
  NodeVarDecl *vdcl = NULL;

  if (bl_tokens_is_seq(cnt->tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    bl_token_t *tok_type = bl_tokens_consume(cnt->tokens);
    bl_token_t *tok_ident = bl_tokens_consume(cnt->tokens);

    if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
      /* declaration only */
      char *type = strndup(tok_type->content.as_string, tok_type->len);
      char *ident = strndup(tok_ident->content.as_string, tok_ident->len);
      vdcl = bl_ast_node_var_decl_new(
        bl_unit_get_ast(cnt->unit),
        type,
        ident,
        tok_ident->src_loc,
        tok_ident->line,
        tok_ident->col);
    } else if (bl_tokens_consume_if(cnt->tokens, BL_SYM_ASIGN)) {
      /*
       * Variable is also asigned to some expression.
       */
      char *type = strndup(tok_type->content.as_string, tok_type->len);
      char *ident = strndup(tok_ident->content.as_string, tok_ident->len);
      vdcl = bl_ast_node_var_decl_new(
        bl_unit_get_ast(cnt->unit),
        type,
        ident,
        tok_ident->src_loc,
        tok_ident->line,
        tok_ident->col);

      /* expected expression */
      if (!bl_node_var_decl_set_expr(vdcl, parse_expr(cnt))) {
        parse_error(cnt,
                    "%s %d:%d expected expression after "
                      BL_YELLOW("'='"),
                    bl_unit_get_src_file(cnt->unit),
                    tok_ident->line,
                    tok_ident->col + tok_ident->len);
      }
    }

    /* always must end with semicolon */
    if (bl_tokens_consume(cnt->tokens)->sym != BL_SYM_SEMICOLON) {
      parse_error(cnt, "%s %d:%d missing semicolon "
        BL_YELLOW("';'")
        " at the end of variable declaration", bl_unit_get_src_file(
        cnt->unit), tok_ident->line, tok_ident->col + tok_ident->len);
    }

  }

  return vdcl;
}

bool
run(Parser *self,
    Unit *unit)
{
  if (bl_unit_get_tokens(unit) == NULL) {
    bl_actor_error((Actor *) unit, "no tokens found for unit");
    return false;
  }

  context_t cnt = {0};
  cnt.unit = unit;
  cnt.tokens = bl_unit_get_tokens(unit);
  if (setjmp(cnt.jmp_error))
    return false;

  bl_unit_set_ast(unit, bl_ast_new());
  bl_ast_set_root(bl_unit_get_ast(unit), parse_global_stmt(&cnt));

  /* TODO: move to another stage??? */
  if (!bl_sym_tbl_try_satisfy_all(bl_unit_get_sym_tbl(unit))) {
    parse_error(&cnt, "%s unknown function detected.", bl_unit_get_src_file(unit));
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

/* public */
