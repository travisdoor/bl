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
#include "ast/ast_impl.h"
#include "bl/pipeline/stage.h"
#include "bl/bldebug.h"
#include "unit_impl.h"

#define parse_error(self, format, ...) \
  { \
    bl_actor_error((Actor *)(self)->unit, (format), ##__VA_ARGS__); \
    longjmp((self)->jmp_error, 1); \
  }

static void
parse_semicolon(Parser *self);

static bl_node_t *
parse_global_stmt(Parser *self);

static bl_node_t *
parse_cmp_stmt(Parser *self);

static bl_node_t *
parse_if_stmt(Parser *self);

static bl_node_t *
parse_expr(Parser *self);

static bl_node_t *
parse_atom_expr(Parser *self);

static bl_node_t *
parse_expr_1(Parser *self,
             bl_node_t *lhs,
             int min_precedence);

static bl_node_t *
parse_call_expr(Parser *self);

static bl_node_t *
parse_var_decl(Parser *self);

static bl_node_t *
parse_func_decl(Parser *self);

static bl_node_t *
parse_param_var_decl(Parser *self);

static bl_node_t *
parse_return_stmt(Parser *self);

static bl_node_t *
parse_loop_stmt(Parser *self);

static bl_node_t *
parse_break_stmt(Parser *self);

static bl_node_t *
parse_continue_stmt(Parser *self);

static void
reset(Parser *self,
      Unit *unit);

static bool
run(Parser *self,
    Unit *unit);

/* Parser members */
bo_decl_members_begin(Parser, Stage)
  Unit *unit;
  bl_tokens_t *tokens;
  BArray *prc_stack;
  BArray *prc_out;

  jmp_buf jmp_error;
  bool is_loop;
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
      " at the end of expression", self->unit->filepath, tok->line, tok->col);
  }
}

bl_node_t *
parse_global_stmt(Parser *self)
{
  bl_node_t
    *gstmt = bl_ast_new_node(&self->unit->ast, BL_NODE_GLOBAL_STMT, self->unit->src, 1, 0);

stmt:
  if (bl_tokens_current_is(self->tokens, BL_SYM_SEMICOLON)) {
    bl_token_t *tok = bl_tokens_consume(self->tokens);
    bl_warning("%s %d:%d extra semicolon can be removed "
                 BL_YELLOW("';'"), self->unit->filepath, tok->line, tok->col);
    goto stmt;
  }

  if (!bl_node_glob_stmt_add_child(gstmt, parse_func_decl(self))) {
    bl_token_t *tok = bl_tokens_peek(self->tokens);
    parse_error(self,
                "%s %d:%d expected function declaration",
                self->unit->filepath,
                tok->line,
                tok->col);
  }

  if (bl_tokens_current_is_not(self->tokens, BL_SYM_EOF))
    goto stmt;

  return gstmt;
}

/*
 * Return statement.
 */
bl_node_t *
parse_return_stmt(Parser *self)
{
  bl_node_t *rstmt = NULL;
  if (bl_tokens_current_is(self->tokens, BL_SYM_RETURN)) {
    bl_token_t *tok = bl_tokens_consume(self->tokens);
    rstmt =
      bl_ast_new_node(&self->unit->ast, BL_NODE_RETURN_STMT, tok->src_loc, tok->line, tok->col);

    /*
     * Here we expect nothing (for void returning functions) or
     * some expression.
     */

    if (bl_tokens_current_is_not(self->tokens, BL_SYM_SEMICOLON)) {
      bl_node_t *expr = parse_expr(self);
      if (expr) {
        rstmt->value.return_stmt.expr = expr;
      } else {
        tok = bl_tokens_consume(self->tokens);
        parse_error(self,
                    "%s %d:%d expected expression or nothing after return statement",
                    self->unit->filepath,
                    tok->line,
                    tok->col);
      }
    }
  }
  return rstmt;
}

bl_node_t *
parse_loop_stmt(Parser *self)
{
  bl_node_t *loop = NULL;
  bool prev_is_loop = self->is_loop;

  bl_token_t *tok = bl_tokens_peek(self->tokens);
  if (tok->sym == BL_SYM_LOOP) {
    bl_tokens_consume(self->tokens);
    self->is_loop = true;
    bl_node_t *stmt = parse_cmp_stmt(self);
    self->is_loop = prev_is_loop;
    if (!stmt) {
      parse_error(self,
                  "%s %d:%d expected if statement body",
                  self->unit->filepath,
                  tok->line,
                  tok->col);
    }

    loop = bl_ast_new_node(&self->unit->ast, BL_NODE_LOOP_STMT, tok->src_loc, tok->line, tok->col);
    loop->value.loop_stmt.cmp_stmt = stmt;
  }

  return loop;
}

bl_node_t *
parse_break_stmt(Parser *self)
{
  bl_node_t *break_stmt = NULL;

  bl_token_t *tok = bl_tokens_peek(self->tokens);
  if (tok->sym == BL_SYM_BREAK) {
    if (!self->is_loop) {
      parse_error(self,
                  "%s %d:%d "
                    BL_YELLOW("break")
                    " statement outside of a loop or switch",
                  self->unit->filepath,
                  tok->line,
                  tok->col);
    }

    bl_tokens_consume(self->tokens);
    break_stmt =
      bl_ast_new_node(&self->unit->ast, BL_NODE_BREAK_STMT, tok->src_loc, tok->line, tok->col);
  }

  return break_stmt;
}

bl_node_t *
parse_continue_stmt(Parser *self)
{
  bl_node_t *continue_stmt = NULL;

  bl_token_t *tok = bl_tokens_peek(self->tokens);
  if (tok->sym == BL_SYM_CONTINUE) {
    if (!self->is_loop) {
      parse_error(self,
                  "%s %d:%d "
                    BL_YELLOW("continue")
                    " statement outside of a loop or switch",
                  self->unit->filepath,
                  tok->line,
                  tok->col);
    }

    bl_tokens_consume(self->tokens);
    continue_stmt =
      bl_ast_new_node(&self->unit->ast, BL_NODE_CONTINUE_STMT, tok->src_loc, tok->line, tok->col);
  }

  return continue_stmt;
}

bl_node_t *
parse_if_stmt(Parser *self)
{
  bl_node_t *ifstmt = NULL;
  bl_node_t *expr = NULL;
  bl_node_t *then_stmt = NULL;
  bl_node_t *else_stmt = NULL;
  bl_node_t *else_if_stmt = NULL;
  bl_token_t *tok = NULL;

  if (bl_tokens_current_is(self->tokens, BL_SYM_IF)) {
    bl_tokens_consume(self->tokens);

    tok = bl_tokens_consume(self->tokens);
    if (tok->sym != BL_SYM_LPAREN) {
      parse_error(self, "%s %d:%d missing "
        BL_YELLOW("'('")
        " after if statement", self->unit->filepath, tok->line, tok->col);
    }

    expr = parse_expr(self);
    if (!expr) {
      parse_error(self,
                  "%s %d:%d expected expression ",
                  self->unit->filepath,
                  tok->line,
                  tok->col);
    }

    tok = bl_tokens_consume(self->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(self, "%s %d:%d missing "
        BL_YELLOW("')'")
        " after expression", self->unit->filepath, tok->line, tok->col);
    }

    /*
     * Parse then compound statement
     */
    then_stmt = parse_cmp_stmt(self);
    if (!then_stmt) {
      parse_error(self,
                  "%s %d:%d expected if statement body",
                  self->unit->filepath,
                  tok->line,
                  tok->col);
    }

    ifstmt =
      bl_ast_new_node(&self->unit->ast, BL_NODE_CONTINUE_STMT, tok->src_loc, tok->line, tok->col);
    ifstmt->value.if_stmt.expr = expr;
    ifstmt->value.if_stmt.then_stmt = then_stmt;

    /*
     * Parse else statement if there is one.
     */
    if (bl_tokens_current_is(self->tokens, BL_SYM_ELSE)) {
      tok = bl_tokens_consume(self->tokens);

      /* else if */
      if (bl_tokens_current_is(self->tokens, BL_SYM_IF)) {
        ifstmt->value.if_stmt.else_if_stmt = parse_if_stmt(self);
      } else {
        else_stmt = parse_cmp_stmt(self);
        if (!else_stmt) {
          parse_error(self,
                      "%s %d:%d expected else statement body",
                      self->unit,
                      tok->line,
                      tok->col);
        }

        ifstmt->value.if_stmt.else_stmt = else_stmt;
      }
    }
  }

  return ifstmt;
}

bl_node_t *
parse_cmp_stmt(Parser *self)
{
  /* eat '{' */
  bl_token_t *tok = bl_tokens_consume(self->tokens);
  if (tok->sym != BL_SYM_LBLOCK) {
    parse_error(self, "%s %d:%d expected scope body "
      BL_YELLOW("'{'"), self->unit->filepath, tok->line, tok->col);
  }

  bl_node_t *stmt =
    bl_ast_new_node(&self->unit->ast, BL_NODE_CMP_STMT, tok->src_loc, tok->line, tok->col);

stmt:
  if (bl_tokens_current_is(self->tokens, BL_SYM_SEMICOLON)) {
    tok = bl_tokens_consume(self->tokens);
    bl_warning("%s %d:%d extra semicolon can be removed "
                 BL_YELLOW("';'"), self->unit->filepath, tok->line, tok->col);
    goto stmt;
  }

  /* compound sub-statement */
  if (bl_tokens_current_is(self->tokens, BL_SYM_LBLOCK)) {
    bl_node_cmp_stmt_add_child(stmt, parse_cmp_stmt(self));
    goto stmt;
  }

  /* var decl */
  if (bl_node_cmp_stmt_add_child(stmt, parse_var_decl(self))) {
    parse_semicolon(self);
    goto stmt;
  }

  /* expr */
  if (bl_node_cmp_stmt_add_child(stmt, parse_expr(self))) {
    parse_semicolon(self);
    goto stmt;
  }

  /* if stmt*/
  if (bl_node_cmp_stmt_add_child(stmt, parse_if_stmt(self)))
    goto stmt;

  /* loop */
  if (bl_node_cmp_stmt_add_child(stmt, parse_loop_stmt(self)))
    goto stmt;

  /* return stmt */
  if (bl_node_cmp_stmt_add_child(stmt, parse_return_stmt(self))) {
    parse_semicolon(self);
    goto stmt;
  }

  /* break stmt */
  if (bl_node_cmp_stmt_add_child(stmt, parse_break_stmt(self))) {
    parse_semicolon(self);
    goto stmt;
  }

  /* continue stmt */
  if (bl_node_cmp_stmt_add_child(stmt, parse_continue_stmt(self))) {
    parse_semicolon(self);
    goto stmt;
  }

  tok = bl_tokens_consume(self->tokens);

  if (tok->sym != BL_SYM_RBLOCK) {
    parse_error(self, "%s %d:%d expected declaration or scope end "
      BL_YELLOW("'}'"), self->unit->filepath, tok->line, tok->col + tok->len);
  }

  return stmt;
}

bl_node_t *
parse_func_decl(Parser *self)
{
  bl_node_t *func_decl = NULL;
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

    func_decl = bl_ast_new_node(
      &self->unit->ast, BL_NODE_FUNC_DECL, tok->src_loc, tok->line, tok->col);

    tok = bl_tokens_consume(self->tokens);
    bl_type_init(&func_decl->value.func_decl.base.type, tok->value.as_string);

    tok = bl_tokens_consume(self->tokens);
    bl_ident_init(&func_decl->value.func_decl.base.ident, tok->value.as_string);

    /*
     * Store the new function into the symbol table.
     */
    bl_sym_tbl_t *sym_tbl = &self->unit->sym_tbl;
    if (!bl_sym_tbl_register(sym_tbl, func_decl)) {
      parse_error(self,
                  "%s %d:%d function with same name already exists"
                    BL_YELLOW(" '%s'"),
                  self->unit->filepath,
                  tok->line,
                  tok->col,
                  ((bl_node_decl_t) (func_decl->value.func_decl.base)).ident.name);

    }

    /* consume '(' */
    bl_tokens_consume(self->tokens);

    if (bl_tokens_current_is_not(self->tokens, BL_SYM_RPAREN)) {
param:
      bl_node_func_decl_stmt_add_param(func_decl, parse_param_var_decl(self));
      if (bl_tokens_consume_if(self->tokens, BL_SYM_COMMA))
        goto param;
    }

    tok = bl_tokens_consume(self->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(self,
                  "%s %d:%d expected "
                    BL_YELLOW("')'")
                    " after function parameter declaration",
                  self->unit->filepath,
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
                    self->unit->filepath,
                    tok->line,
                    tok->col);
      }
    } else {
      func_decl->value.func_decl.cmp_stmt = parse_cmp_stmt(self);
    }
  } else {
    /* Roll back to marker. */
    bl_tokens_back_to_marker(self->tokens);
  }

  return func_decl;
}

bl_node_t *
parse_param_var_decl(Parser *self)
{
  bl_token_t *tok = bl_tokens_consume(self->tokens);
  if (tok->sym != BL_SYM_IDENT) {
    parse_error(self,
                "%s %d:%d expected parameter type",
                self->unit->filepath,
                tok->line,
                tok->col);
  }

  const char *type = tok->value.as_string;

  tok = bl_tokens_consume(self->tokens);
  if (tok->sym != BL_SYM_IDENT) {
    parse_error(self,
                "%s %d:%d expected parameter name",
                self->unit->filepath,
                tok->line,
                tok->col);
  }

  const char *ident = tok->value.as_string;

  bl_node_t *param = bl_ast_new_node(
    &self->unit->ast, BL_NODE_PARAM_VAR_DECL, tok->src_loc, tok->line, tok->col);

  bl_type_init(&param->value.param_var_decl.base.type, type);
  bl_ident_init(&param->value.param_var_decl.base.ident, ident);
}

bl_node_t *
parse_expr(Parser *self)
{
  return parse_expr_1(self, parse_atom_expr(self), 0);
}

bl_node_t *
parse_atom_expr(Parser *self)
{
  bl_node_t *expr = NULL;

  bl_token_t *tok = bl_tokens_peek(self->tokens);
  switch (tok->sym) {
    case BL_SYM_LPAREN:
      /* parse sub-expression in (...) */

      /* eat ( */
      bl_tokens_consume(self->tokens);
      expr = parse_expr(self);
      if (expr == NULL) {
        parse_error(self,
                    "%s %d:%d expected expression.",
                    self->unit->filepath,
                    tok->line,
                    tok->col);
      }

      /* eat ) */
      tok = bl_tokens_consume(self->tokens);
      if (tok->sym != BL_SYM_RPAREN) {
        parse_error(self, "%s %d:%d unterminated sub-expression, missing "
          BL_YELLOW("')'"), self->unit->filepath, tok->line, tok->col);
      }

      break;
    case BL_SYM_IDENT:
      expr = parse_call_expr(self);
      if (expr)
        break;

      bl_tokens_consume(self->tokens);

      expr =
        bl_ast_new_node(&self->unit->ast, BL_NODE_DECL_REF_EXPR, tok->src_loc, tok->line, tok->col);
      bl_ident_init(&expr->value.decl_ref_expr.ident, tok->value.as_string);
      break;
    case BL_SYM_FLOAT:
      bl_tokens_consume(self->tokens);

      expr =
        bl_ast_new_node(&self->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_float = tok->value.as_float;
      expr->value.const_expr.type = BL_CONST_FLOAT;
      break;
    case BL_SYM_DOUBLE:
      bl_tokens_consume(self->tokens);

      expr =
        bl_ast_new_node(&self->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_double = tok->value.as_double;
      expr->value.const_expr.type = BL_CONST_DOUBLE;
      break;
    case BL_SYM_NUM:
      bl_tokens_consume(self->tokens);

      expr =
        bl_ast_new_node(&self->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_ulong = tok->value.as_ull;
      expr->value.const_expr.type = BL_CONST_INT;
      break;
    case BL_SYM_TRUE:
      bl_tokens_consume(self->tokens);

      expr =
        bl_ast_new_node(&self->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_bool = true;
      expr->value.const_expr.type = BL_CONST_BOOL;
      break;
    case BL_SYM_FALSE:
      bl_tokens_consume(self->tokens);

      expr =
        bl_ast_new_node(&self->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_bool = false;
      expr->value.const_expr.type = BL_CONST_BOOL;
      break;
    case BL_SYM_STRING:
      bl_tokens_consume(self->tokens);

      expr =
        bl_ast_new_node(&self->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_string = tok->value.as_string;
      expr->value.const_expr.type = BL_CONST_STRING;
      break;
    case BL_SYM_CHAR:
      bl_tokens_consume(self->tokens);

      expr =
        bl_ast_new_node(&self->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_char = tok->value.as_char;
      expr->value.const_expr.type = BL_CONST_CHAR;
      break;
    default:
      break;
  }

  return expr;
}

bl_node_t *
parse_expr_1(Parser *self,
             bl_node_t *lhs,
             int min_precedence)
{
  bl_node_t *rhs = NULL;
  bl_token_t *lookahead = bl_tokens_peek(self->tokens);
  bl_token_t *op = NULL;

  while (bl_token_prec(lookahead) >= min_precedence) {
    op = lookahead;
    bl_tokens_consume(self->tokens);
    rhs = parse_atom_expr(self);
    lookahead = bl_tokens_peek(self->tokens);

    while ((bl_token_prec(lookahead) > bl_token_prec(op)) ||
      (lookahead->sym == BL_SYM_ASIGN && bl_token_prec(lookahead) == bl_token_prec(op))) {
      rhs = parse_expr_1(self, rhs, bl_token_prec(lookahead));
      lookahead = bl_tokens_peek(self->tokens);
    }

    bl_node_t *tmp = lhs;
    lhs = bl_ast_new_node(&self->unit->ast, BL_NODE_BINOP, op->src_loc, op->line, op->col);
    lhs->value.binop.lhs = tmp;
    lhs->value.binop.rhs = rhs;
    lhs->value.binop.operator = op->sym;
  }

  return lhs;
}

bl_node_t *
parse_call_expr(Parser *self)
{
  bl_node_t *call = NULL;

  if (bl_tokens_is_seq(self->tokens, 2, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    bl_token_t *tok_calle = bl_tokens_consume(self->tokens);

    /* eat '(' */
    bl_tokens_consume(self->tokens);

    call = bl_ast_new_node(
      &self->unit->ast,
      BL_NODE_CALL_EXPR,
      tok_calle->src_loc,
      tok_calle->line,
      tok_calle->col);

    bl_ident_init(&call->value.call_stmt.ident, tok_calle->value.as_string);

    /*
     * Handle callee existence, when symbol was found, store expected return
     * type into call node. When no callee was found, it can be defined later
     * or in another unit in assembly, in such case we only store unsatisfied
     * call into cache and add information about return type later.
     */
    bl_sym_tbl_t *sym_tbl = &self->unit->sym_tbl;
    bl_ident_t *ident = &call->value.call_stmt.ident;
    bl_node_t *callee = bl_sym_tbl_get_sym_of_type(sym_tbl, ident, BL_NODE_FUNC_DECL);
    if (callee == NULL) {
      bl_sym_tbl_add_unsatisfied_expr(sym_tbl, call);
    } else {
      call->value.call_stmt.callee = callee;
    }

arg:
    bl_node_call_expr_add_arg(call, parse_expr(self));
    if (bl_tokens_consume_if(self->tokens, BL_SYM_COMMA))
      goto arg;

    bl_token_t *tok = bl_tokens_consume(self->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(self,
                  "%s %d:%d expected "
                    BL_YELLOW("')'")
                    " after function call argument list",
                  self->unit->filepath,
                  tok->line,
                  tok->col);
    }
  }

  return call;
}

bl_node_t *
parse_var_decl(Parser *self)
{
  bl_node_t *vdcl = NULL;

  if (bl_tokens_is_seq(self->tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    bl_token_t *tok_type = bl_tokens_consume(self->tokens);
    bl_token_t *tok_ident = bl_tokens_consume(self->tokens);

    if (bl_tokens_current_is(self->tokens, BL_SYM_SEMICOLON)) {
      /* declaration only */
      vdcl = bl_ast_new_node(
        &self->unit->ast,
        BL_NODE_VAR_DECL,
        tok_type->src_loc,
        tok_type->line,
        tok_type->col);

      bl_type_init(&vdcl->value.var_decl.base.type, tok_type->value.as_string);
      bl_ident_init(&vdcl->value.var_decl.base.ident, tok_ident->value.as_string);
    } else if (bl_tokens_consume_if(self->tokens, BL_SYM_ASIGN)) {
      /*
       * Variable is also asigned to some expression.
       */
      vdcl = bl_ast_new_node(
        &self->unit->ast,
        BL_NODE_VAR_DECL,
        tok_type->src_loc,
        tok_type->line,
        tok_type->col);

      bl_type_init(&vdcl->value.var_decl.base.type, tok_type->value.as_string);
      bl_ident_init(&vdcl->value.var_decl.base.ident, tok_ident->value.as_string);

      /* expected expression */
      bl_node_t *expr = parse_expr(self);
      if (expr) {
        vdcl->value.var_decl.expr = expr;
      } else {
        parse_error(self,
                    "%s %d:%d expected expression after "
                      BL_YELLOW("'='"),
                    self->unit->filepath,
                    tok_ident->line,
                    tok_ident->col + tok_ident->len);
      }
    }
  }

  return vdcl;
}

void
reset(Parser *self,
      Unit *unit)
{
  self->unit = unit;
  self->tokens = &unit->tokens;

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

  unit->ast.root = parse_global_stmt(self);

  /* TODO: move to another stage??? */
//  if (!bl_sym_tbl_try_satisfy_all(bl_unit_get_sym_tbl(unit))) {
//    parse_error(self, "%s unknown function detected.", bl_unit_get_src_file(unit));
//  }

  return true;
}

/* public */
Parser *
bl_parser_new(bl_compile_group_e group)
{
  ParserParams p = {.base.group = group};

  return bo_new(Parser, &p);
}


