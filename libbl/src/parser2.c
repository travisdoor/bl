//*****************************************************************************
// bl
//
// File:   parser2.c
// Author: Martin Dorazil
// Date:   3/15/18
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
#include "stages_impl.h"
#include "common_impl.h"

#define parse_error(cnt, code, tok, format, ...)                                                   \
  {                                                                                                \
    bl_builder_error((cnt)->builder, "%s %d:%d " format, (tok)->src.file, (tok)->src.line,         \
                     (tok)->src.col, ##__VA_ARGS__);                                               \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

#define parse_error_node(cnt, code, node, format, ...)                                             \
  {                                                                                                \
    bl_builder_error((cnt)->builder, "%s %d:%d " format, (node)->src->file, (node)->src->line,     \
                     (node)->src->col, ##__VA_ARGS__);                                             \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

typedef struct
{
  bl_builder_t *builder;
  bl_unit_t *   unit;
  bl_ast_t *    ast;
  bl_tokens_t * tokens;

  /* tmps */
  jmp_buf jmp_error;
} context_t;

typedef enum {
  PATH_EXPECTED_CALL = 1,
  PATH_EXPECTED_VAR  = 2,
  PATH_EXPECTED_TYPE = 4
} path_expected_e;

static bl_node_t *
parse_fn_maybe(context_t *cnt);

static bl_node_t *
parse_struct_maybe(context_t *cnt);

static bl_node_t *
parse_enum_maybe(context_t *cnt);

static bl_node_t *
parse_module_maybe(context_t *cnt, bl_node_t *parent, bool global);

static bl_node_t *
parse_block_maybe(context_t *cnt);

static bl_node_t *
parse_type_maybe(context_t *cnt);

static bl_node_t *
parse_arg_maybe(context_t *cnt);

static void
parse_semicolon_rq(context_t *cnt);

static bl_node_t *
parse_ret_type_rq(context_t *cnt);

static bl_node_t *
parse_var_maybe(context_t *cnt);

static bl_node_t *
parse_expr_maybe(context_t *cnt);

static bl_node_t *
parse_expr_1(context_t *cnt, bl_node_t *lhs, int min_precedence);

static bl_node_t *
parse_atom_expr(context_t *cnt);

static bl_node_t *
parse_const_expr_maybe(context_t *cnt);

static bl_node_t *
parse_var_ref_maybe(context_t *cnt);

static bl_node_t *
parse_call_maybe(context_t *cnt);

static bl_node_t *
parse_nested_expr_maybe(context_t *cnt);

static bl_node_t *
parse_path_maybe(context_t *cnt, int expected_flag);

static bl_node_t *
parse_if_maybe(context_t *cnt);

static bl_node_t *
parse_loop_maybe(context_t *cnt);

static bl_node_t *
parse_block_content_maybe(context_t *cnt);

static bl_node_t *
parse_while_maybe(context_t *cnt);

static bl_node_t *
parse_break_maybe(context_t *cnt);

static bl_node_t *
parse_continue_maybe(context_t *cnt);

static bl_node_t *
parse_return_maybe(context_t *cnt);

/* impl*/
bl_node_t *
parse_return_maybe(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_RETURN);
  if (!tok_begin) {
    return NULL;
  }

  bl_node_t *expr = parse_expr_maybe(cnt);
  return bl_ast_add_stmt_return(cnt->ast, tok_begin, expr);
}

bl_node_t *
parse_loop_maybe(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LOOP);
  if (!tok_begin) {
    return NULL;
  }

  bl_node_t *true_stmt = parse_block_content_maybe(cnt);
  if (true_stmt == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, "expected loop body");
  }

  return bl_ast_add_stmt_loop(cnt->ast, tok_begin, true_stmt);
}

bl_node_t *
parse_while_maybe(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_WHILE);
  if (!tok_begin) {
    return NULL;
  }

  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_LPAREN) == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);

    parse_error(cnt, BL_ERR_MISSING_BRACKET, err_tok,
                "expected " BL_YELLOW("'('") " after while statement");
  }

  bl_node_t *test = parse_expr_maybe(cnt);
  if (test == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, err_tok, "expected expression for the while statement");
  }

  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_RPAREN) == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_MISSING_BRACKET, err_tok,
                "expected " BL_YELLOW("')'") " after while statement expression");
  }

  bl_node_t *true_stmt = parse_block_content_maybe(cnt);
  if (true_stmt == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, "expected loop body");
  }

  return bl_ast_add_stmt_while(cnt->ast, tok_begin, test, true_stmt);
}

bl_node_t *
parse_break_maybe(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_BREAK);
  if (!tok_begin) {
    return NULL;
  }

  return bl_ast_add_stmt_break(cnt->ast, tok_begin);
}

bl_node_t *
parse_continue_maybe(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_CONTINUE);
  if (!tok_begin) {
    return NULL;
  }

  return bl_ast_add_stmt_continue(cnt->ast, tok_begin);
}

bl_node_t *
parse_var_ref_maybe(context_t *cnt)
{
  bl_node_t * var_ref = NULL;
  bl_token_t *tok_id  = bl_tokens_peek(cnt->tokens);
  if (tok_id->sym == BL_SYM_IDENT) {
    bl_tokens_consume(cnt->tokens);
    var_ref = bl_ast_add_expr_var_ref(cnt->ast, tok_id, tok_id->value.str, NULL);
  }

  return var_ref;
}

bl_node_t *
parse_call_maybe(context_t *cnt)
{
  bl_node_t *call = NULL;
  if (bl_tokens_is_seq(cnt->tokens, 2, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    bl_token_t *tok_id = bl_tokens_consume(cnt->tokens);
    call               = bl_ast_add_expr_call(cnt->ast, tok_id, tok_id->value.str, NULL);

    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_LPAREN) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, "expected function parameter list");
    }

    /* parse args */
    bool rq = false;
  arg:
    if (bl_ast_call_push_arg(call, parse_expr_maybe(cnt))) {
      if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
        rq = true;
        goto arg;
      }
    } else if (rq) {
      bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err,
                  "expected function argument after comma " BL_YELLOW("','"));
    }

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok,
                  "expected end of parameter list " BL_YELLOW(
                      "')'") " or another parameter separated by comma");
    }
  }
  return call;
}

bl_node_t *
parse_const_expr_maybe(context_t *cnt)
{
  bl_node_t * const_expr = NULL;
  bl_node_t * type       = NULL;
  bl_token_t *tok        = bl_tokens_peek(cnt->tokens);

  switch (tok->sym) {
  case BL_SYM_NUM:
    bl_tokens_consume(cnt->tokens);
    type       = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_I32);
    const_expr = bl_ast_add_expr_const_unsigned(cnt->ast, tok, type, tok->value.u);
    break;

  case BL_SYM_STRING:
    bl_tokens_consume(cnt->tokens);
    type       = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_STRING);
    const_expr = bl_ast_add_expr_const_str(cnt->ast, tok, type, tok->value.str);
    break;

  case BL_SYM_FLOAT:
    bl_tokens_consume(cnt->tokens);
    type       = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_F32);
    const_expr = bl_ast_add_expr_const_double(cnt->ast, tok, type, tok->value.d);
    break;

  case BL_SYM_DOUBLE:
    bl_tokens_consume(cnt->tokens);
    type       = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_F64);
    const_expr = bl_ast_add_expr_const_double(cnt->ast, tok, type, tok->value.d);
    break;

  case BL_SYM_CHAR:
    bl_tokens_consume(cnt->tokens);
    type       = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_CHAR);
    const_expr = bl_ast_add_expr_const_char(cnt->ast, tok, type, tok->value.c);
    break;
  case BL_SYM_TRUE:
  case BL_SYM_FALSE: {
    bl_tokens_consume(cnt->tokens);
    bool val   = tok->sym == BL_SYM_TRUE;
    type       = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_BOOL);
    const_expr = bl_ast_add_expr_const_bool(cnt->ast, tok, type, val);

    break;
  }
  default:
    break;
  }

  return const_expr;
}

bl_node_t *
parse_nested_expr_maybe(context_t *cnt)
{
  bl_node_t * expr      = NULL;
  bl_token_t *tok_begin = bl_tokens_peek(cnt->tokens);

  if (tok_begin->sym == BL_SYM_LPAREN) {
    /* parse sub-expression in (...) */

    /* eat ( */
    bl_tokens_consume(cnt->tokens);
    expr = parse_expr_maybe(cnt);
    if (expr == NULL) {
      parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_begin, "expected expression.");
    }

    /* eat ) */
    bl_token_t *tok_end = bl_tokens_consume(cnt->tokens);
    if (tok_end->sym != BL_SYM_RPAREN) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_end,
                  "unterminated sub-expression, missing " BL_YELLOW("')'") ", started %d:%d",
                  tok_begin->src.line, tok_begin->src.col);
    }
  }

  return expr;
}

bl_node_t *
parse_path_maybe(context_t *cnt, int expected_flag)
{
  /*
   * We expect identificator here as entry point to another path,
   * function call, variable reference or type. This function can return
   * any of these nodes depending on source code.
   */
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_IDENT)) {
    if (bl_tokens_next_is(cnt->tokens, BL_SYM_MODULE_PATH)) {
      bl_token_t *tok_ident = bl_tokens_consume(cnt->tokens);
      bl_tokens_consume(cnt->tokens); /* eat :: */
      /* next path element */
      bl_node_t *next = parse_path_maybe(cnt, expected_flag);
      return bl_ast_add_expr_path(cnt->ast, tok_ident, tok_ident->value.str, NULL, next);
    }

    bl_node_t *curr = NULL;
    if (expected_flag & PATH_EXPECTED_CALL) {
      if ((curr = parse_call_maybe(cnt)) != NULL) {
        return curr;
      }
    }

    if (expected_flag & PATH_EXPECTED_VAR) {
      if ((curr = parse_var_ref_maybe(cnt)) != NULL) {
        return curr;
      }
    }

    if (expected_flag & PATH_EXPECTED_TYPE) {
      if ((curr = parse_type_maybe(cnt)) != NULL) {
        return curr;
      }
    }

    bl_token_t *tok_err = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_INVALID_TOKEN, tok_err,
                "invalid token found in path, expected call, variable or type");
  }

  /* not path, variable, call or type */
  return NULL;
}

bl_node_t *
parse_atom_expr(context_t *cnt)
{
  bl_node_t *expr = NULL;

  if ((expr = parse_nested_expr_maybe(cnt)))
    return expr;

  if ((expr = parse_path_maybe(cnt, PATH_EXPECTED_VAR | PATH_EXPECTED_CALL)))
    return expr;

  if ((expr = parse_const_expr_maybe(cnt)))
    return expr;

  return expr;
}

bl_node_t *
parse_expr_1(context_t *cnt, bl_node_t *lhs, int min_precedence)
{
  bl_node_t * rhs       = NULL;
  bl_token_t *lookahead = bl_tokens_peek(cnt->tokens);
  bl_token_t *op        = NULL;

  while (bl_token_prec(lookahead) >= min_precedence) {
    op = lookahead;
    bl_tokens_consume(cnt->tokens);
    rhs       = parse_atom_expr(cnt);
    lookahead = bl_tokens_peek(cnt->tokens);

    while (bl_token_prec(lookahead) > bl_token_prec(op)) {
      rhs       = parse_expr_1(cnt, rhs, bl_token_prec(lookahead));
      lookahead = bl_tokens_peek(cnt->tokens);
    }

    if (bl_token_is_binop(op)) {
      bl_node_t *tmp = lhs;
      lhs            = bl_ast_add_expr_binop(cnt->ast, op, op->sym, tmp, rhs);
    } else {
      parse_error(cnt, BL_ERR_EXPECTED_BINOP, op, "expected binary operation");
    }
  }

  return lhs;
}

bl_node_t *
parse_expr_maybe(context_t *cnt)
{
  return parse_expr_1(cnt, parse_atom_expr(cnt), 0);
}

bl_node_t *
parse_var_maybe(context_t *cnt)
{
  bl_node_t *var       = NULL;
  bl_node_t *type      = NULL;
  bl_node_t *init_expr = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_VAR)) {
    bl_token_t *tok_id = bl_tokens_consume(cnt->tokens);

    type = parse_path_maybe(cnt, PATH_EXPECTED_TYPE);
    if (type == NULL) {
      bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok_err, "expected type name after variable name");
    }

    /*
     * parse init expr when variable declaration is fallowd by assign symbol
     */
    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_ASIGN)) {
      init_expr = parse_expr_maybe(cnt);
      if (init_expr == NULL) {
        bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
        parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_err,
                    "expected expression after " BL_YELLOW("'='") " in variable declaration");
      }
    }

    var = bl_ast_add_decl_var(cnt->ast, tok_id, tok_id->value.str, type, init_expr);
  }

  return var;
}

void
parse_semicolon_rq(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_SEMICOLON) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, "missing semicolon " BL_YELLOW("';'"));
  }
}

bl_node_t *
parse_type_maybe(context_t *cnt)
{
  bl_node_t * type = NULL;
  bl_token_t *tok  = bl_tokens_consume_if(cnt->tokens, BL_SYM_IDENT);
  if (tok != NULL) {
    int found = -1;
    for (int i = 0; i < bl_nelems(bl_fund_type_strings); i++) {
      if (strcmp(bl_fund_type_strings[i], tok->value.str) == 0) {
        found = i;
        break;
      }
    }

    if (found > -1) {
      type = bl_ast_add_type_fund(cnt->ast, tok, (bl_fund_type_e)found);
    } else {
      type = bl_ast_add_type_ref(cnt->ast, tok, tok->value.str, NULL);
    }
  }

  return type;
}

bl_node_t *
parse_ret_type_rq(context_t *cnt)
{
  bl_token_t *tok  = bl_tokens_peek(cnt->tokens);
  bl_node_t * type = NULL;

  switch (tok->sym) {
  case BL_SYM_IDENT:
    type = parse_path_maybe(cnt, PATH_EXPECTED_TYPE);
    break;
  case BL_SYM_LBLOCK:
  case BL_SYM_SEMICOLON: {
    type = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_VOID);
    break;
  }
  default:
    break;
  }

  if (type == NULL)
    parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok, "expected return type");

  return type;
}

bl_node_t *
parse_if_maybe(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_IF);
  if (tok_begin == NULL) {
    return NULL;
  }

  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_LPAREN) == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);

    parse_error(cnt, BL_ERR_MISSING_BRACKET, err_tok,
                "expected " BL_YELLOW("'('") " after if statement");
  }

  bl_node_t *test = parse_expr_maybe(cnt);
  if (test == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);

    parse_error(cnt, BL_ERR_EXPECTED_EXPR, err_tok, "expected expression for the if statement");
  }

  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_RPAREN) == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);

    parse_error(cnt, BL_ERR_MISSING_BRACKET, err_tok,
                "expected " BL_YELLOW("')'") " after if statement expression");
  }

  bl_node_t *true_stmt = parse_block_content_maybe(cnt);
  if (true_stmt == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok,
                "expected statement for true result of the if expression test");
  }

  bl_node_t *false_stmt = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_ELSE)) {
    false_stmt = parse_block_content_maybe(cnt);
    if (false_stmt == NULL) {
      bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok,
                  "expected statement for false result of the if expression test");
    }
  }

  return bl_ast_add_stmt_if(cnt->ast, tok_begin, test, true_stmt, false_stmt);
}

bl_node_t *
parse_block_content_maybe(context_t *cnt)
{
  bl_node_t *stmt = NULL;

  if ((stmt = parse_block_maybe(cnt))) {
    goto done;
  }

  if ((stmt = parse_var_maybe(cnt))) {
    parse_semicolon_rq(cnt);
    goto done;
  }

  if ((stmt = parse_if_maybe(cnt))) {
    goto done;
  }

  if ((stmt = parse_loop_maybe(cnt))) {
    goto done;
  }

  if ((stmt = parse_while_maybe(cnt))) {
    goto done;
  }

  if ((stmt = parse_return_maybe(cnt))) {
    parse_semicolon_rq(cnt);
    goto done;
  }

  if ((stmt = parse_break_maybe(cnt))) {
    parse_semicolon_rq(cnt);
    goto done;
  }

  if ((stmt = parse_continue_maybe(cnt))) {
    parse_semicolon_rq(cnt);
    goto done;
  }

  if ((stmt = parse_expr_maybe(cnt))) {
    parse_semicolon_rq(cnt);
    goto done;
  }

done:
  return stmt;
}

bl_node_t *
parse_block_maybe(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBLOCK);

  if (tok_begin == NULL) {
    return NULL;
  }

  bl_node_t * block = bl_ast_add_decl_block(cnt->ast, tok_begin);
  bl_token_t *tok;
stmt:
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
    tok = bl_tokens_consume(cnt->tokens);
    // TODO: warning macro
    bl_warning("%s %d:%d extra semicolon can be removed " BL_YELLOW("';'"), cnt->unit->filepath,
               tok->src.line, tok->src.col);
    goto stmt;
  }

  /* stmts */
  if (bl_ast_block_push_node(block, parse_block_content_maybe(cnt))) {
    goto stmt;
  }

  tok = bl_tokens_consume(cnt->tokens);

  if (tok->sym != BL_SYM_RBLOCK) {
    parse_error(cnt, BL_ERR_EXPECTED_BODY_END, tok,
                "expected declaration or scope end " BL_YELLOW("'}'") ", starting %d:%d",
                tok_begin->src.line, tok_begin->src.col);
  }

  return block;
}

bl_node_t *
parse_arg_maybe(context_t *cnt)
{
  bl_node_t *arg = NULL;
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_IDENT)) {
    bl_token_t *tok  = bl_tokens_consume(cnt->tokens);
    bl_node_t * type = parse_path_maybe(cnt, PATH_EXPECTED_TYPE);

    if (type == NULL) {
      bl_token_t *tok = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, "expected argument type");
    }

    arg = bl_ast_add_decl_arg(cnt->ast, tok, tok->value.str, type);
  }

  return arg;
}

bl_node_t *
parse_fn_maybe(context_t *cnt)
{
  bl_node_t *fn = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_FN) != NULL) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected function name");
    }

    fn = bl_ast_add_decl_func(cnt->ast, tok, tok->value.str, NULL, NULL);

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_LPAREN) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, "expected function parameter list");
    }

    /* parse args */
    bool rq = false;
  arg:
    if (bl_ast_func_push_arg(fn, parse_arg_maybe(cnt))) {
      if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
        rq = true;
        goto arg;
      }
    } else if (rq) {
      bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err,
                  "expected function argument after comma " BL_YELLOW("','"));
    }

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok,
                  "expected end of parameter list " BL_YELLOW(
                      "')'") " or another parameter separated by comma");
    }

    /*
     * parse function return type definition, and use void if there is no type specified
     */
    bl_peek_decl_func(fn)->ret_type = parse_ret_type_rq(cnt);
    bl_node_t *block                = parse_block_maybe(cnt);

    if (block == NULL) {
      bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_BODY, tok_err, "expected function body " BL_YELLOW("'{'"));
    }

    bl_peek_decl_func(fn)->block = block;
  }

  return fn;
}

bl_node_t *
parse_struct_maybe(context_t *cnt)
{
  bl_node_t *strct = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_STRUCT) != NULL) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);

    if (tok->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected struct name");
    }

    strct = bl_ast_add_decl_struct(cnt->ast, tok, tok->value.str);

    // TODO
    tok = bl_tokens_consume(cnt->tokens);
    tok = bl_tokens_consume(cnt->tokens);
  }
  return strct;
}

bl_node_t *
parse_enum_maybe(context_t *cnt)
{
  bl_node_t *enm = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_ENUM) != NULL) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);

    if (tok->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected enum name");
    }
    enm = bl_ast_add_decl_enum(cnt->ast, tok, tok->value.str);

    // TODO
    tok = bl_tokens_consume(cnt->tokens);
    tok = bl_tokens_consume(cnt->tokens);
  }
  return enm;
}

bl_node_t *
parse_module_maybe(context_t *cnt, bl_node_t *parent, bool global)
{
  bl_node_t * module          = NULL;
  bl_token_t *tok_id          = NULL;
  bl_token_t *tok_begin_block = NULL;

  if (!global) {
    bl_assert(parent, "non-global module must have parent module!!!");
    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_MODULE) == NULL) {
      return NULL;
    }

    tok_id          = bl_tokens_consume(cnt->tokens);
    tok_begin_block = bl_tokens_consume(cnt->tokens);

    if (tok_id->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_id, "expected module name");
    }

    module = bl_ast_add_decl_module(cnt->ast, tok_id, tok_id->value.str);

    if (tok_begin_block->sym != BL_SYM_LBLOCK) {
      parse_error(cnt, BL_ERR_EXPECTED_BODY, tok_begin_block,
                  "expected block after module name " BL_YELLOW("'{'"));
    }
  } else {
    module = bl_ast_add_decl_module(cnt->ast, NULL, NULL);
  }

decl:
  if (bl_ast_module_push_node(module, parse_module_maybe(cnt, module, false))) {
    goto decl;
  }

  if (bl_ast_module_push_node(module, parse_fn_maybe(cnt))) {
    goto decl;
  }

  if (bl_ast_module_push_node(module, parse_struct_maybe(cnt))) {
    goto decl;
  }

  if (bl_ast_module_push_node(module, parse_enum_maybe(cnt))) {
    goto decl;
  }

  if (!global) {
    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_RBLOCK) == NULL) {
      bl_token_t *tok_err = bl_tokens_consume(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_BODY_END, tok_err,
                  "missing module block end " BL_YELLOW("'}'") ", starting " BL_YELLOW("%d:%d"),
                  tok_begin_block->src.line, tok_begin_block->src.col);
    }
  }

  return module;
}

bl_error_e
bl_parser2_run(bl_builder_t *builder, bl_unit_t *unit)
{
  context_t cnt = {.builder = builder, .unit = unit, .ast = &unit->ast, .tokens = &unit->tokens};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e)error;
  }

  unit->ast.root = parse_module_maybe(&cnt, NULL, true);
  return BL_NO_ERR;
}
