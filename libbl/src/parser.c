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
#include "stages_impl.h"
#include "bl/bldebug.h"
#include "unit_impl.h"

#define parse_error(cnt, format, ...) \
  { \
    bl_builder_error((cnt)->builder, (format), ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, 1); \
  }

typedef struct context
{
  bl_builder_t *builder;
  bl_unit_t *unit;
  bl_tokens_t *tokens;

  jmp_buf jmp_error;
  bool is_loop;
} context_t;

static void
parse_semicolon(context_t *cnt);

static bl_node_t *
parse_global_stmt(context_t *cnt);

static bl_node_t *
parse_cmp_stmt(context_t *cnt);

static bl_node_t *
parse_if_stmt(context_t *cnt);

static bl_node_t *
parse_expr(context_t *cnt);

static bl_node_t *
parse_atom_expr(context_t *cnt);

static bl_node_t *
parse_expr_1(context_t *cnt,
             bl_node_t *lhs,
             int min_precedence);

static bl_node_t *
parse_call_expr(context_t *cnt);

static bl_node_t *
parse_var_decl(context_t *cnt);

static bl_node_t *
parse_func_decl(context_t *cnt);

static bl_node_t *
parse_enum_decl(context_t *cnt);

static bl_node_t *
parse_param_var_decl(context_t *cnt);

static bl_node_t *
parse_return_stmt(context_t *cnt);

static bl_node_t *
parse_loop_stmt(context_t *cnt);

static bl_node_t *
parse_break_stmt(context_t *cnt);

static bl_node_t *
parse_continue_stmt(context_t *cnt);

void
parse_semicolon(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_SEMICOLON) {
    parse_error(cnt, "%s %d:%d missing semicolon "
      BL_YELLOW("';'")
      " at the end of expression", cnt->unit->filepath, tok->line, tok->col);
  }
}

bl_node_t *
parse_global_stmt(context_t *cnt)
{
  bl_node_t *gstmt = bl_ast_new_node(&cnt->unit->ast, BL_NODE_GLOBAL_STMT, cnt->unit->src, 1, 0);

stmt:
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    bl_warning("%s %d:%d extra semicolon can be removed "
                 BL_YELLOW("';'"), cnt->unit->filepath, tok->line, tok->col);
    goto stmt;
  }

  if (bl_node_glob_stmt_add_child(gstmt, parse_enum_decl(cnt))) {
    goto stmt;
  }

  if (!bl_node_glob_stmt_add_child(gstmt, parse_func_decl(cnt))) {
    bl_token_t *tok = bl_tokens_peek(cnt->tokens);
    parse_error(cnt,
                "%s %d:%d unexpected declaration in global scope",
                cnt->unit->filepath,
                tok->line,
                tok->col);
  }

  if (bl_tokens_current_is_not(cnt->tokens, BL_SYM_EOF))
    goto stmt;

  return gstmt;
}

/*
 * Return statement.
 */
bl_node_t *
parse_return_stmt(context_t *cnt)
{
  bl_node_t *rstmt = NULL;
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_RETURN)) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    rstmt =
      bl_ast_new_node(&cnt->unit->ast, BL_NODE_RETURN_STMT, tok->src_loc, tok->line, tok->col);

    /*
     * Here we expect nothing (for void returning functions) or
     * some expression.
     */

    if (bl_tokens_current_is_not(cnt->tokens, BL_SYM_SEMICOLON)) {
      bl_node_t *expr = parse_expr(cnt);
      if (expr) {
        rstmt->value.return_stmt.expr = expr;
      } else {
        tok = bl_tokens_consume(cnt->tokens);
        parse_error(cnt,
                    "%s %d:%d expected expression or nothing after return statement",
                    cnt->unit->filepath,
                    tok->line,
                    tok->col);
      }
    }
  }
  return rstmt;
}

bl_node_t *
parse_loop_stmt(context_t *cnt)
{
  bl_node_t *loop = NULL;
  bool prev_is_loop = cnt->is_loop;

  bl_token_t *tok = bl_tokens_peek(cnt->tokens);
  if (tok->sym == BL_SYM_LOOP) {
    bl_tokens_consume(cnt->tokens);
    cnt->is_loop = true;
    bl_node_t *stmt = parse_cmp_stmt(cnt);
    cnt->is_loop = prev_is_loop;
    if (!stmt) {
      parse_error(cnt,
                  "%s %d:%d expected if statement body",
                  cnt->unit->filepath,
                  tok->line,
                  tok->col);
    }

    loop = bl_ast_new_node(&cnt->unit->ast, BL_NODE_LOOP_STMT, tok->src_loc, tok->line, tok->col);
    loop->value.loop_stmt.cmp_stmt = stmt;
  }

  return loop;
}

bl_node_t *
parse_break_stmt(context_t *cnt)
{
  bl_node_t *break_stmt = NULL;

  bl_token_t *tok = bl_tokens_peek(cnt->tokens);
  if (tok->sym == BL_SYM_BREAK) {
    if (!cnt->is_loop) {
      parse_error(cnt, "%s %d:%d "
        BL_YELLOW("break")
        " statement outside of a loop or switch", cnt->unit->filepath, tok->line, tok->col);
    }

    bl_tokens_consume(cnt->tokens);
    break_stmt =
      bl_ast_new_node(&cnt->unit->ast, BL_NODE_BREAK_STMT, tok->src_loc, tok->line, tok->col);
  }

  return break_stmt;
}

bl_node_t *
parse_continue_stmt(context_t *cnt)
{
  bl_node_t *continue_stmt = NULL;

  bl_token_t *tok = bl_tokens_peek(cnt->tokens);
  if (tok->sym == BL_SYM_CONTINUE) {
    if (!cnt->is_loop) {
      parse_error(cnt, "%s %d:%d "
        BL_YELLOW("continue")
        " statement outside of a loop or switch", cnt->unit->filepath, tok->line, tok->col);
    }

    bl_tokens_consume(cnt->tokens);
    continue_stmt =
      bl_ast_new_node(&cnt->unit->ast, BL_NODE_CONTINUE_STMT, tok->src_loc, tok->line, tok->col);
  }

  return continue_stmt;
}

bl_node_t *
parse_if_stmt(context_t *cnt)
{
  bl_node_t *ifstmt = NULL;
  bl_node_t *expr = NULL;
  bl_node_t *then_stmt = NULL;
  bl_node_t *else_stmt = NULL;
  bl_node_t *else_if_stmt = NULL;
  bl_token_t *tok = NULL;

  if (bl_tokens_current_is(cnt->tokens, BL_SYM_IF)) {
    bl_tokens_consume(cnt->tokens);

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_LPAREN) {
      parse_error(cnt, "%s %d:%d missing "
        BL_YELLOW("'('")
        " after if statement", cnt->unit->filepath, tok->line, tok->col);
    }

    expr = parse_expr(cnt);
    if (!expr) {
      parse_error(cnt, "%s %d:%d expected expression ", cnt->unit->filepath, tok->line, tok->col);
    }

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(cnt, "%s %d:%d missing "
        BL_YELLOW("')'")
        " after expression", cnt->unit->filepath, tok->line, tok->col);
    }

    /*
     * Parse then compound statement
     */
    then_stmt = parse_cmp_stmt(cnt);
    if (!then_stmt) {
      parse_error(cnt,
                  "%s %d:%d expected if statement body",
                  cnt->unit->filepath,
                  tok->line,
                  tok->col);
    }

    ifstmt = bl_ast_new_node(&cnt->unit->ast, BL_NODE_IF_STMT, tok->src_loc, tok->line, tok->col);
    ifstmt->value.if_stmt.expr = expr;
    ifstmt->value.if_stmt.then_stmt = then_stmt;

    /*
     * Parse else statement if there is one.
     */
    if (bl_tokens_current_is(cnt->tokens, BL_SYM_ELSE)) {
      tok = bl_tokens_consume(cnt->tokens);

      /* else if */
      if (bl_tokens_current_is(cnt->tokens, BL_SYM_IF)) {
        ifstmt->value.if_stmt.else_if_stmt = parse_if_stmt(cnt);
      } else {
        else_stmt = parse_cmp_stmt(cnt);
        if (!else_stmt) {
          parse_error(cnt, "%s %d:%d expected else statement body", cnt->unit, tok->line, tok->col);
        }

        ifstmt->value.if_stmt.else_stmt = else_stmt;
      }
    }
  }

  return ifstmt;
}

bl_node_t *
parse_cmp_stmt(context_t *cnt)
{
  /* eat '{' */
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_LBLOCK) {
    parse_error(cnt, "%s %d:%d expected scope body "
      BL_YELLOW("'{'"), cnt->unit->filepath, tok->line, tok->col);
  }

  bl_node_t
    *stmt = bl_ast_new_node(&cnt->unit->ast, BL_NODE_CMP_STMT, tok->src_loc, tok->line, tok->col);

stmt:
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
    tok = bl_tokens_consume(cnt->tokens);
    bl_warning("%s %d:%d extra semicolon can be removed "
                 BL_YELLOW("';'"), cnt->unit->filepath, tok->line, tok->col);
    goto stmt;
  }

  /* compound sub-statement */
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_LBLOCK)) {
    bl_node_cmp_stmt_add_child(stmt, parse_cmp_stmt(cnt));
    goto stmt;
  }

  /* var decl */
  if (bl_node_cmp_stmt_add_child(stmt, parse_var_decl(cnt))) {
    parse_semicolon(cnt);
    goto stmt;
  }

  /* expr */
  if (bl_node_cmp_stmt_add_child(stmt, parse_expr(cnt))) {
    parse_semicolon(cnt);
    goto stmt;
  }

  /* if stmt*/
  if (bl_node_cmp_stmt_add_child(stmt, parse_if_stmt(cnt)))
    goto stmt;

  /* loop */
  if (bl_node_cmp_stmt_add_child(stmt, parse_loop_stmt(cnt)))
    goto stmt;

  /* return stmt */
  if (bl_node_cmp_stmt_add_child(stmt, parse_return_stmt(cnt))) {
    parse_semicolon(cnt);
    goto stmt;
  }

  /* break stmt */
  if (bl_node_cmp_stmt_add_child(stmt, parse_break_stmt(cnt))) {
    parse_semicolon(cnt);
    goto stmt;
  }

  /* continue stmt */
  if (bl_node_cmp_stmt_add_child(stmt, parse_continue_stmt(cnt))) {
    parse_semicolon(cnt);
    goto stmt;
  }

  tok = bl_tokens_consume(cnt->tokens);

  if (tok->sym != BL_SYM_RBLOCK) {
    parse_error(cnt, "%s %d:%d expected declaration or scope end "
      BL_YELLOW("'}'"), cnt->unit->filepath, tok->line, tok->col + tok->len);
  }

  return stmt;
}

bl_node_t *
parse_func_decl(context_t *cnt)
{
  bl_node_t *func_decl = NULL;
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

    tok = bl_tokens_peek(cnt->tokens);
    func_decl = bl_ast_new_node(
      &cnt->unit->ast, BL_NODE_FUNC_DECL, tok->src_loc, tok->line, tok->col);

    tok = bl_tokens_consume(cnt->tokens);
    bl_type_init(&func_decl->value.func_decl.base.type, tok->value.as_string);

    tok = bl_tokens_consume(cnt->tokens);
    bl_ident_init(&func_decl->value.func_decl.base.ident, tok->value.as_string);

    /*
     * Store the new function into the symbol table.
     */
    bl_sym_tbl_t *sym_tbl = &cnt->unit->sym_tbl;
    if (!bl_sym_tbl_register(sym_tbl, func_decl)) {
      parse_error(cnt,
                  "%s %d:%d function with same name already exists"
                    BL_YELLOW(" '%s'"),
                  cnt->unit->filepath,
                  tok->line,
                  tok->col,
                  ((bl_node_decl_t) (func_decl->value.func_decl.base)).ident.name);

    }

    /* consume '(' */
    bl_tokens_consume(cnt->tokens);

    if (bl_tokens_current_is_not(cnt->tokens, BL_SYM_RPAREN)) {
param:
      bl_node_func_decl_stmt_add_param(func_decl, parse_param_var_decl(cnt));
      if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA))
        goto param;
    }

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(cnt, "%s %d:%d expected "
        BL_YELLOW("')'")
        " after function parameter declaration", cnt->unit->filepath, tok->line, tok->col);
    }

    if (modif == BL_SYM_EXTERN) {
      tok = bl_tokens_consume(cnt->tokens);
      if (tok->sym != BL_SYM_SEMICOLON) {
        parse_error(cnt, "%s %d:%d missing semicolon "
          BL_YELLOW("';'")
          " at the end of extern function definition", cnt->unit->filepath, tok->line, tok->col);
      }
    } else {
      func_decl->value.func_decl.cmp_stmt = parse_cmp_stmt(cnt);
    }
  } else {
    /* Roll back to marker. */
    bl_tokens_back_to_marker(cnt->tokens);
  }

  return func_decl;
}

bl_node_t *
parse_param_var_decl(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_IDENT) {
    parse_error(cnt, "%s %d:%d expected parameter type", cnt->unit->filepath, tok->line, tok->col);
  }

  const char *type = tok->value.as_string;

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_IDENT) {
    parse_error(cnt, "%s %d:%d expected parameter name", cnt->unit->filepath, tok->line, tok->col);
  }

  const char *ident = tok->value.as_string;

  bl_node_t *param = bl_ast_new_node(
    &cnt->unit->ast, BL_NODE_PARAM_VAR_DECL, tok->src_loc, tok->line, tok->col);

  bl_type_init(&param->value.param_var_decl.base.type, type);
  bl_ident_init(&param->value.param_var_decl.base.ident, ident);
  return param;
}

bl_node_t *
parse_enum_decl(context_t *cnt)
{
  bl_node_t *enm = NULL;
  bl_token_t *tok;

  tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_ENUM);
  if (tok) {
    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_IDENT) {
      parse_error(cnt, "%s %d:%d expected enum name", cnt->unit->filepath, tok->line, tok->col);
    }


    enm = bl_ast_new_node(
      &cnt->unit->ast, BL_NODE_ENUM_DECL, tok->src_loc, tok->line, tok->col);

    /* TODO parse base type: enum my_enum : i32 {} */
    bl_type_init(&enm->value.decl.type, "i32");

    /* eat '{' */
    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_LBLOCK) {
      parse_error(cnt, "%s %d:%d expected enum body "
        BL_YELLOW("'{'"), cnt->unit->filepath, tok->line, tok->col);
    }

elem:
    /* eat ident */
    if ((tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_IDENT))) {
      bl_log("%s", tok->value.as_string);
      if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
        goto elem;
      } else if (bl_tokens_peek(cnt->tokens)->sym != BL_SYM_RBLOCK) {
        tok = bl_tokens_consume(cnt->tokens);
        parse_error(cnt, "%s %d:%d enum elements must be separated by comma "
          BL_YELLOW("','"), cnt->unit->filepath, tok->line, tok->col + tok->len);
      }
    }


    /* eat '}' */
    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RBLOCK) {
      parse_error(cnt, "%s %d:%d expected end of enum body "
        BL_YELLOW("'}'"), cnt->unit->filepath, tok->line, tok->col + tok->len);
    }
  }

  return enm;
}

bl_node_t *
parse_expr(context_t *cnt)
{
  return parse_expr_1(cnt, parse_atom_expr(cnt), 0);
}

bl_node_t *
parse_atom_expr(context_t *cnt)
{
  bl_node_t *expr = NULL;

  bl_token_t *tok = bl_tokens_peek(cnt->tokens);
  switch (tok->sym) {
    case BL_SYM_LPAREN:
      /* parse sub-expression in (...) */

      /* eat ( */
      bl_tokens_consume(cnt->tokens);
      expr = parse_expr(cnt);
      if (expr == NULL) {
        parse_error(cnt, "%s %d:%d expected expression.", cnt->unit->filepath, tok->line, tok->col);
      }

      /* eat ) */
      tok = bl_tokens_consume(cnt->tokens);
      if (tok->sym != BL_SYM_RPAREN) {
        parse_error(cnt, "%s %d:%d unterminated sub-expression, missing "
          BL_YELLOW("')'"), cnt->unit->filepath, tok->line, tok->col);
      }

      break;
    case BL_SYM_IDENT:
      expr = parse_call_expr(cnt);
      if (expr)
        break;

      bl_tokens_consume(cnt->tokens);

      expr =
        bl_ast_new_node(&cnt->unit->ast, BL_NODE_DECL_REF_EXPR, tok->src_loc, tok->line, tok->col);
      bl_ident_init(&expr->value.decl_ref_expr.ident, tok->value.as_string);
      break;
    case BL_SYM_FLOAT:
      bl_tokens_consume(cnt->tokens);

      expr =
        bl_ast_new_node(&cnt->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_float = tok->value.as_float;
      expr->value.const_expr.type = BL_CONST_FLOAT;
      break;
    case BL_SYM_DOUBLE:
      bl_tokens_consume(cnt->tokens);

      expr =
        bl_ast_new_node(&cnt->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_double = tok->value.as_double;
      expr->value.const_expr.type = BL_CONST_DOUBLE;
      break;
    case BL_SYM_NUM:
      bl_tokens_consume(cnt->tokens);

      expr =
        bl_ast_new_node(&cnt->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_ulong = tok->value.as_ull;
      expr->value.const_expr.type = BL_CONST_INT;
      break;
    case BL_SYM_TRUE:
      bl_tokens_consume(cnt->tokens);

      expr =
        bl_ast_new_node(&cnt->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_bool = true;
      expr->value.const_expr.type = BL_CONST_BOOL;
      break;
    case BL_SYM_FALSE:
      bl_tokens_consume(cnt->tokens);

      expr =
        bl_ast_new_node(&cnt->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_bool = false;
      expr->value.const_expr.type = BL_CONST_BOOL;
      break;
    case BL_SYM_STRING:
      bl_tokens_consume(cnt->tokens);

      expr =
        bl_ast_new_node(&cnt->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_string = tok->value.as_string;
      expr->value.const_expr.type = BL_CONST_STRING;
      break;
    case BL_SYM_CHAR:
      bl_tokens_consume(cnt->tokens);

      expr =
        bl_ast_new_node(&cnt->unit->ast, BL_NODE_CONST_EXPR, tok->src_loc, tok->line, tok->col);
      expr->value.const_expr.value.as_char = tok->value.as_char;
      expr->value.const_expr.type = BL_CONST_CHAR;
      break;
    default:
      break;
  }

  return expr;
}

bl_node_t *
parse_expr_1(context_t *cnt,
             bl_node_t *lhs,
             int min_precedence)
{
  bl_node_t *rhs = NULL;
  bl_token_t *lookahead = bl_tokens_peek(cnt->tokens);
  bl_token_t *op = NULL;

  while (bl_token_prec(lookahead) >= min_precedence) {
    op = lookahead;
    bl_tokens_consume(cnt->tokens);
    rhs = parse_atom_expr(cnt);
    lookahead = bl_tokens_peek(cnt->tokens);

    while ((bl_token_prec(lookahead) > bl_token_prec(op)) ||
      (lookahead->sym == BL_SYM_ASIGN && bl_token_prec(lookahead) == bl_token_prec(op))) {
      rhs = parse_expr_1(cnt, rhs, bl_token_prec(lookahead));
      lookahead = bl_tokens_peek(cnt->tokens);
    }

    bl_node_t *tmp = lhs;
    lhs = bl_ast_new_node(&cnt->unit->ast, BL_NODE_BINOP, op->src_loc, op->line, op->col);
    lhs->value.binop.lhs = tmp;
    lhs->value.binop.rhs = rhs;
    lhs->value.binop.operator = op->sym;
  }

  return lhs;
}

bl_node_t *
parse_call_expr(context_t *cnt)
{
  bl_node_t *call = NULL;

  if (bl_tokens_is_seq(cnt->tokens, 2, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    bl_token_t *tok_calle = bl_tokens_consume(cnt->tokens);

    /* eat '(' */
    bl_tokens_consume(cnt->tokens);

    call = bl_ast_new_node(
      &cnt->unit->ast, BL_NODE_CALL_EXPR, tok_calle->src_loc, tok_calle->line, tok_calle->col);

    bl_ident_init(&call->value.call_expr.ident, tok_calle->value.as_string);

    /*
     * Handle callee existence, when symbol was found, store expected return
     * type into call node. When no callee was found, it can be defined later
     * or in another unit in assembly, in such case we only store unsatisfied
     * call into cache and add information about return type later.
     */
    bl_sym_tbl_t *sym_tbl = &cnt->unit->sym_tbl;
    bl_ident_t *ident = &call->value.call_expr.ident;
    bl_node_t *callee = bl_sym_tbl_get_sym_of_type(sym_tbl, ident, BL_NODE_FUNC_DECL);
    if (callee == NULL) {
      bl_sym_tbl_add_unsatisfied_expr(sym_tbl, call);
    } else {
      call->value.call_expr.callee = callee;
    }

arg:
    bl_node_call_expr_add_arg(call, parse_expr(cnt));
    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA))
      goto arg;

    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(cnt, "%s %d:%d expected "
        BL_YELLOW("')'")
        " after function call argument list", cnt->unit->filepath, tok->line, tok->col);
    }
  }

  return call;
}

bl_node_t *
parse_var_decl(context_t *cnt)
{
  bl_node_t *vdcl = NULL;

  if (bl_tokens_is_seq(cnt->tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    bl_token_t *tok_type = bl_tokens_consume(cnt->tokens);
    bl_token_t *tok_ident = bl_tokens_consume(cnt->tokens);

    if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
      /* declaration only */
      vdcl = bl_ast_new_node(
        &cnt->unit->ast, BL_NODE_VAR_DECL, tok_type->src_loc, tok_type->line, tok_type->col);

      bl_type_init(&vdcl->value.var_decl.base.type, tok_type->value.as_string);
      bl_ident_init(&vdcl->value.var_decl.base.ident, tok_ident->value.as_string);
    } else if (bl_tokens_consume_if(cnt->tokens, BL_SYM_ASIGN)) {
      /*
       * Variable is also asigned to some expression.
       */
      vdcl = bl_ast_new_node(
        &cnt->unit->ast, BL_NODE_VAR_DECL, tok_type->src_loc, tok_type->line, tok_type->col);

      bl_type_init(&vdcl->value.var_decl.base.type, tok_type->value.as_string);
      bl_ident_init(&vdcl->value.var_decl.base.ident, tok_ident->value.as_string);

      /* expected expression */
      bl_node_t *expr = parse_expr(cnt);
      if (expr) {
        vdcl->value.var_decl.expr = expr;
      } else {
        parse_error(cnt, "%s %d:%d expected expression after "
          BL_YELLOW("'='"), cnt->unit->filepath, tok_ident->line, tok_ident->col + tok_ident->len);
      }
    }
  }

  return vdcl;
}

bool
bl_parser_run(bl_builder_t *builder,
              bl_unit_t *unit)
{
  context_t cnt = {.builder = builder, .unit = unit, .tokens = &unit->tokens, .is_loop = false};

  if (setjmp(cnt.jmp_error))
    return false;

  unit->ast.root = parse_global_stmt(&cnt);

  /* TODO: move to another stage??? */
  if (!bl_sym_tbl_try_satisfy_all(&unit->sym_tbl)) {
    parse_error(&cnt, "%s unknown function detected.", unit->filepath);
  }

  return true;
}
