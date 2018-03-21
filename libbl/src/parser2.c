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
    bl_builder_error((cnt)->builder, "%s %d:%d " format, (tok)->file, (tok)->line, (tok)->col,     \
                     ##__VA_ARGS__);                                                               \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

typedef struct
{
  bl_builder_t *builder;
  bl_unit_t *   unit;
  bl_ast2_t *   ast;
  bl_tokens_t * tokens;

  jmp_buf jmp_error;
} context_t;

static bl_item_t *
parse_item_maybe(context_t *cnt);

static bl_module_t *
parse_module_rq(context_t *cnt, bool is_gscope);

static bl_expr_t *
parse_atom_expr(context_t *cnt);

static bl_expr_t *
parse_expr_1(context_t *cnt, bl_expr_t *lhs, int min_precedence);

static bl_func_decl_t *
parse_func_decl(context_t *cnt);

static bl_block_t *
parse_block_maybe(context_t *cnt);

static bl_struct_decl_t *
parse_struct(context_t *cnt);

static bl_enum_decl_t *
parse_enum(context_t *cnt);

static bl_arg_t *
parse_arg(context_t *cnt);

static bl_type_t *
parse_type(context_t *cnt);

static bl_type_t *
parse_ret_type(context_t *cnt);

static bl_decl_t *
parse_decl_maybe(context_t *cnt);

static bl_expr_t *
parse_expr_maybe(context_t *cnt);

static bl_const_expr_t *
parse_const_expr_maybe(context_t *cnt);

static bl_expr_t *
parse_nested_expr_maybe(context_t *cnt);

static bl_path_t *
parse_path_maybe(context_t *cnt);

static bl_call_t *
parse_call_maybe(context_t *cnt);

static bl_var_ref_t *
parse_var_ref_maybe(context_t *cnt);

static bl_stmt_t *
parse_stmt_maybe(context_t *cnt);

static void
parse_semicolon_rq(context_t *cnt);

/* impl */

bl_decl_t *
parse_decl_maybe(context_t *cnt)
{
  bl_decl_t * decl = NULL;
  bl_token_t *tok  = bl_tokens_peek(cnt->tokens);

  /*
   * Local variable declaration:
   * <var> <name> <type>;
   * <var> <name> <type> = <init_expr>;
   */
  if (tok->sym == BL_SYM_VAR) {
    /* eat var */
    bl_tokens_consume(cnt->tokens);
    decl = bl_ast2_new_node(cnt->ast, BL_NODE_DECL, tok, bl_decl_t);

    /* name */
    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected variable name");
    }

    bl_id_init(&decl->id, tok->value.as_string);

    /* type */
    tok = bl_tokens_peek(cnt->tokens);
    if (tok->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok, "expected variable type");
    }

    decl->type = parse_type(cnt);

    tok = bl_tokens_peek(cnt->tokens);
    if (tok->sym == BL_SYM_ASIGN) {
      bl_tokens_consume(cnt->tokens);
      decl->init_expr = parse_expr_maybe(cnt);
      if (decl->init_expr == NULL) {
        parse_error(cnt, BL_ERR_MISSING_SEMICOLON, tok,
                    "expected variable initialization expression after " BL_YELLOW("'='"));
      }
    } else {
      decl->init_expr = NULL;
    }
  }

  return decl;
}

bl_call_t *
parse_call_maybe(context_t *cnt)
{
  bl_call_t * call      = NULL;
  bl_token_t *tok_begin = bl_tokens_peek(cnt->tokens);

  if (bl_tokens_is_seq(cnt->tokens, 2, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    call                  = bl_ast2_new_node(cnt->ast, BL_NODE_CALL, tok_begin, bl_call_t);
    bl_token_t *callee_id = bl_tokens_consume(cnt->tokens);
    bl_id_init(&call->id, callee_id->value.as_string);

    /*
     * callee and return type must be set later during linking becouse function declaration can be
     * in different file or later in this file and it has not been parsed yet
     */

    /* constume ( */
    bl_token_t *tok_param_begin = bl_tokens_consume(cnt->tokens);

    if (bl_tokens_peek(cnt->tokens)->sym != BL_SYM_RPAREN) {
    arg:
      if (bl_ast_call_push_arg(call, parse_expr_maybe(cnt)) == NULL) {
        bl_token_t *error_tok = bl_tokens_peek(cnt->tokens);
        parse_error(cnt, BL_ERR_EXPECTED_EXPR, error_tok,
                    "expected expression in call argument list");
      } else if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
        goto arg;
      }
    }

    bl_token_t *tok_param_end = bl_tokens_consume(cnt->tokens);
    if (tok_param_end->sym != BL_SYM_RPAREN) {
      parse_error(cnt, BL_ERR_MISSING_SEMICOLON, tok_param_end,
                  "expected " BL_YELLOW("')'") " at the end of call argument list, starting %d:%d",
                  tok_param_begin->line, tok_param_begin->col);
    }
  }

  return call;
}

bl_var_ref_t *
parse_var_ref_maybe(context_t *cnt)
{
  bl_var_ref_t *var_ref = NULL;
  bl_token_t *  tok     = bl_tokens_peek(cnt->tokens);
  if (tok->sym == BL_SYM_IDENT) {
    bl_tokens_consume(cnt->tokens);
    var_ref = bl_ast2_new_node(cnt->ast, BL_NODE_VAR_REF, tok, bl_var_ref_t);
    bl_id_init(&var_ref->id, tok->value.as_string);

    /*
     * reference pointer will be set later in linker
     */
  }

  return var_ref;
}

bl_expr_t *
parse_nested_expr_maybe(context_t *cnt)
{
  bl_expr_t * expr      = NULL;
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
                  tok_begin->line, tok_begin->col);
    }
  }

  return expr;
}

bl_path_t *
parse_path_maybe(context_t *cnt)
{
  bl_path_t *path = NULL;

  if (bl_tokens_is_seq(cnt->tokens, 2, BL_SYM_IDENT, BL_SYM_MODULE_PATH)) {
    bl_token_t *tok_ident = bl_tokens_consume(cnt->tokens);
    path                  = bl_ast2_new_node(cnt->ast, BL_NODE_PATH, tok_ident, bl_path_t);
    bl_id_init(&path->id, tok_ident->value.as_string);

    bl_tokens_consume(cnt->tokens); // eat ::

    /* next path element */
    bl_path_t *next_path = parse_path_maybe(cnt);
    if (next_path != NULL) {
      path->t         = BL_PATH_PATH;
      path->next.path = next_path;
      return path;
    }

    bl_call_t *next_call = parse_call_maybe(cnt);
    if (next_call != NULL) {
      path->t         = BL_PATH_CALL;
      path->next.call = next_call;
      return path;
    }

    bl_var_ref_t *next_var = parse_var_ref_maybe(cnt);
    if (next_var != NULL) {
      path->t            = BL_PATH_VAR_REF;
      path->next.var_ref = next_var;
      return path;
    }

    bl_token_t *tok_err = bl_tokens_consume(cnt->tokens); // eat ::
    parse_error(cnt, BL_ERR_INVALID_TOKEN, tok_err,
                "invalid token found in path, expected call or variable");
  }

  return path;
}

bl_const_expr_t *
parse_const_expr_maybe(context_t *cnt)
{
  bl_const_expr_t *const_expr = NULL;
  bl_token_t *     tok        = bl_tokens_peek(cnt->tokens);

  switch (tok->sym) {
  case BL_SYM_NUM: {
    bl_tokens_consume(cnt->tokens);
    const_expr = bl_ast2_new_node(cnt->ast, BL_NODE_CONST_EXPR, tok, bl_const_expr_t);

    const_expr->type            = bl_ast2_new_node(cnt->ast, BL_NODE_TYPE, tok, bl_type_t);
    const_expr->type->t         = BL_TYPE_FUND;
    const_expr->type->type.fund = BL_FTYPE_I32;

    bl_id_init(&const_expr->type->id, "i32");
    const_expr->value.s = tok->value.as_ull;
    break;
  }

  case BL_SYM_STRING:
    bl_tokens_consume(cnt->tokens);
    const_expr = bl_ast2_new_node(cnt->ast, BL_NODE_CONST_EXPR, tok, bl_const_expr_t);

    const_expr->type            = bl_ast2_new_node(cnt->ast, BL_NODE_TYPE, tok, bl_type_t);
    const_expr->type->t         = BL_TYPE_FUND;
    const_expr->type->type.fund = BL_FTYPE_STRING;

    bl_id_init(&const_expr->type->id, "string");
    const_expr->value.str = tok->value.as_string;
    break;
  case BL_SYM_FLOAT:
  case BL_SYM_DOUBLE:
    bl_tokens_consume(cnt->tokens);
    const_expr = bl_ast2_new_node(cnt->ast, BL_NODE_CONST_EXPR, tok, bl_const_expr_t);

    const_expr->type            = bl_ast2_new_node(cnt->ast, BL_NODE_TYPE, tok, bl_type_t);
    const_expr->type->t         = BL_TYPE_FUND;
    const_expr->type->type.fund = BL_FTYPE_F64;

    bl_id_init(&const_expr->type->id, "f64");
    const_expr->value.f = tok->value.as_double;
    break;
  case BL_SYM_CHAR:
    bl_tokens_consume(cnt->tokens);
    const_expr = bl_ast2_new_node(cnt->ast, BL_NODE_CONST_EXPR, tok, bl_const_expr_t);

    const_expr->type            = bl_ast2_new_node(cnt->ast, BL_NODE_TYPE, tok, bl_type_t);
    const_expr->type->t         = BL_TYPE_FUND;
    const_expr->type->type.fund = BL_FTYPE_CHAR;

    bl_id_init(&const_expr->type->id, "char");
    const_expr->value.c = tok->value.as_char;
    break;
  case BL_SYM_TRUE:
  case BL_SYM_FALSE:
    bl_tokens_consume(cnt->tokens);
    const_expr = bl_ast2_new_node(cnt->ast, BL_NODE_CONST_EXPR, tok, bl_const_expr_t);

    const_expr->type            = bl_ast2_new_node(cnt->ast, BL_NODE_TYPE, tok, bl_type_t);
    const_expr->type->t         = BL_TYPE_FUND;
    const_expr->type->type.fund = BL_FTYPE_BOOL;

    bl_id_init(&const_expr->type->id, "bool");
    if (tok->sym == BL_SYM_TRUE)
      const_expr->value.b = true;
    else
      const_expr->value.b = false;
    break;
  default:
    break;
  }

  return const_expr;
}

bl_expr_t *
parse_atom_expr(context_t *cnt)
{
  bl_expr_t * expr = NULL;
  bl_token_t *tok  = bl_tokens_peek(cnt->tokens);

  bl_const_expr_t *const_expr = parse_const_expr_maybe(cnt);
  if (const_expr != NULL) {
    expr            = bl_ast2_new_node(cnt->ast, BL_NODE_EXPR, tok, bl_expr_t);
    expr->t         = BL_EXPR_CONST;
    expr->expr.cnst = const_expr;
    return expr;
  }

  bl_expr_t *sub_expr = parse_nested_expr_maybe(cnt);
  if (sub_expr != NULL) {
    expr              = bl_ast2_new_node(cnt->ast, BL_NODE_EXPR, tok, bl_expr_t);
    expr->t           = BL_EXPR_NESTED;
    expr->expr.nested = sub_expr;
    return expr;
  }

  bl_path_t *path = parse_path_maybe(cnt);
  if (path != NULL) {
    expr            = bl_ast2_new_node(cnt->ast, BL_NODE_EXPR, tok, bl_expr_t);
    expr->t         = BL_EXPR_PATH;
    expr->expr.path = path;
    return expr;
  }

  bl_call_t *call = parse_call_maybe(cnt);
  if (call != NULL) {
    expr            = bl_ast2_new_node(cnt->ast, BL_NODE_EXPR, tok, bl_expr_t);
    expr->t         = BL_EXPR_CALL;
    expr->expr.call = call;
    return expr;
  }

  bl_var_ref_t *var_ref = parse_var_ref_maybe(cnt);
  if (var_ref != NULL) {
    expr               = bl_ast2_new_node(cnt->ast, BL_NODE_EXPR, tok, bl_expr_t);
    expr->t            = BL_EXPR_VAR_REF;
    expr->expr.var_ref = var_ref;
    return expr;
  }

  return expr;
}

bl_expr_t *
parse_expr_1(context_t *cnt, bl_expr_t *lhs, int min_precedence)
{
  bl_expr_t * rhs       = NULL;
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

    bl_expr_t *tmp       = lhs;
    lhs                  = bl_ast2_new_node(cnt->ast, BL_NODE_EXPR, op, bl_expr_t);
    lhs->expr.binop      = bl_ast2_new_node(cnt->ast, BL_NODE_BINOP, op, bl_binop_t);
    lhs->t               = BL_EXPR_BINOP;
    lhs->expr.binop->lhs = tmp;
    lhs->expr.binop->rhs = rhs;

    if (bl_token_is_binop(op))
      lhs->expr.binop->op = op->sym;
    else {
      parse_error(cnt, BL_ERR_EXPECTED_BINOP, op, "expected binary operation");
    }
  }

  return lhs;
}

bl_expr_t *
parse_expr_maybe(context_t *cnt)
{
  return parse_expr_1(cnt, parse_atom_expr(cnt), 0);
}

bl_stmt_t *
parse_stmt_maybe(context_t *cnt)
{
  bl_stmt_t * stmt = NULL;
  bl_token_t *tok  = bl_tokens_peek(cnt->tokens);

  bl_decl_t *decl = parse_decl_maybe(cnt);
  if (decl != NULL) {
    stmt            = bl_ast2_new_node(cnt->ast, BL_NODE_STMT, tok, bl_stmt_t);
    stmt->t         = BL_STMT_DECL;
    stmt->stmt.decl = decl;
    parse_semicolon_rq(cnt);
    return stmt;
  }

  bl_expr_t *expr = parse_expr_maybe(cnt);
  if (expr != NULL) {
    stmt            = bl_ast2_new_node(cnt->ast, BL_NODE_STMT, tok, bl_stmt_t);
    stmt->t         = BL_STMT_EXPR;
    stmt->stmt.expr = expr;
    parse_semicolon_rq(cnt);
    return stmt;
  }

  bl_block_t *block = parse_block_maybe(cnt);
  if (block != NULL) {
    stmt             = bl_ast2_new_node(cnt->ast, BL_NODE_STMT, tok, bl_stmt_t);
    stmt->t          = BL_STMT_BLOCK;
    stmt->stmt.block = block;
    return stmt;
  }

  return stmt;
}

void
parse_semicolon_rq(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_SEMICOLON) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, "missing semicolon " BL_YELLOW("';'"));
  }
}

bl_type_t *
parse_type(context_t *cnt)
{
  bl_type_t * type = NULL;
  bl_token_t *tok  = bl_tokens_consume_if(cnt->tokens, BL_SYM_IDENT);
  if (tok != NULL) {
    type = bl_ast2_new_node(cnt->ast, BL_NODE_TYPE, tok, bl_type_t);
    bl_id_init(&type->id, tok->value.as_string);

    switch (type->id.hash) {
    case BL_FTYPE_VOID:
    case BL_FTYPE_I8:
    case BL_FTYPE_I32:
    case BL_FTYPE_I64:
    case BL_FTYPE_U8:
    case BL_FTYPE_U32:
    case BL_FTYPE_U64:
    case BL_FTYPE_F32:
    case BL_FTYPE_F64:
    case BL_FTYPE_CHAR:
    case BL_FTYPE_STRING:
    case BL_FTYPE_BOOL:
      type->t         = BL_TYPE_FUND;
      type->type.fund = (bl_fund_type_e)type->id.hash;
      break;
    default:
      type->t = BL_TYPE_UNKNOWN;
    }
  }

  return type;
}

bl_type_t *
parse_ret_type(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_peek(cnt->tokens);
  switch (tok->sym) {
  case BL_SYM_IDENT:
    return parse_type(cnt);
  case BL_SYM_LBLOCK:
  case BL_SYM_SEMICOLON: {
    bl_type_t *type = bl_ast2_new_node(cnt->ast, BL_NODE_TYPE, tok, bl_type_t);
    type->id        = (bl_id_t){.str = "void", .hash = BL_FTYPE_VOID};
    type->t         = BL_TYPE_FUND;
    type->type.fund = BL_FTYPE_VOID;
    return type;
  }
  default:
    parse_error(
        cnt, BL_ERR_EXPECTED_TYPE, tok,
        "expected function return type or nothing in case when function has no return type");
  }

  /* should not be reached */
  return NULL;
}

bl_struct_decl_t *
parse_struct(context_t *cnt)
{
  bl_tokens_consume(cnt->tokens); // {
  bl_tokens_consume(cnt->tokens); // }
  return bl_ast2_new_node(cnt->ast, BL_NODE_STRUCT_DECL, NULL, bl_struct_decl_t);
}

bl_enum_decl_t *
parse_enum(context_t *cnt)
{
  bl_tokens_consume(cnt->tokens); // {
  bl_tokens_consume(cnt->tokens); // }
  return bl_ast2_new_node(cnt->ast, BL_NODE_ENUM_DECL, NULL, bl_enum_decl_t);
}

bl_func_decl_t *
parse_func_decl(context_t *cnt)
{
  bl_token_t *    tok       = bl_tokens_consume(cnt->tokens);
  bl_func_decl_t *func_decl = bl_ast2_new_node(cnt->ast, BL_NODE_FUNC_DECL, tok, bl_func_decl_t);

  if (tok->sym != BL_SYM_LPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, "expected function parameter list");
  }

  /* parse args */
arg:
  if (bl_ast_func_push_arg(func_decl, parse_arg(cnt))) {
    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      goto arg;
    }
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok,
                "expected end of parameter list " BL_YELLOW(
                    "')'") " or another parameter separated by comma");
  }

  /* has return type defined? if not we use void */
  func_decl->ret = parse_ret_type(cnt);

  return func_decl;
}

bl_arg_t *
parse_arg(context_t *cnt)
{
  bl_arg_t *arg = NULL;
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_IDENT)) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    arg             = bl_ast2_new_node(cnt->ast, BL_NODE_ARG, tok, bl_arg_t);
    bl_id_init(&arg->id, tok->value.as_string);

    arg->type = parse_type(cnt);
    if (arg->type == NULL) {
      bl_token_t *tok = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, "expected argument type");
    }
  }

  return arg;
}

bl_block_t *
parse_block_maybe(context_t *cnt)
{
  bl_block_t *block = NULL;

  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBLOCK);
  if (tok_begin != NULL) {
    block = bl_ast2_new_node(cnt->ast, BL_NODE_BLOCK, tok_begin, bl_block_t);

    /* TODO: parse block body */
    while (bl_ast_block_push_stmt(block, parse_stmt_maybe(cnt))) {
    };

    bl_token_t *tok_end = bl_tokens_consume(cnt->tokens);
    if (tok_end->sym != BL_SYM_RBLOCK) {
      parse_error(
          cnt, BL_ERR_EXPECTED_BODY_END, tok_end,
          "expected end of the block " BL_YELLOW("'}'") ", statement or expression, started %d:%d",
          tok_begin->line, tok_begin->col);
    }
  }

  return block;
}

bl_item_t *
parse_item_maybe(context_t *cnt)
{
  bl_item_t * item = NULL;
  bl_token_t *tok  = bl_tokens_peek(cnt->tokens);

  switch (tok->sym) {
  case BL_SYM_FN: {
    /* eat fn */
    bl_tokens_consume(cnt->tokens);

    /* parse name */
    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected function name");
    }

    item    = bl_ast2_new_node(cnt->ast, BL_NODE_ITEM, tok, bl_item_t);
    item->t = BL_ITEM_FUNC;

    bl_id_init(&item->id, tok->value.as_string);

    item->node.func.func_decl = parse_func_decl(cnt);
    item->node.func.block     = parse_block_maybe(cnt);

    if (item->node.func.block == NULL) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, "missing function body");
    }

    break;
  }

  case BL_SYM_STRUCT: {
    bl_tokens_consume(cnt->tokens);
    tok     = bl_tokens_consume(cnt->tokens);
    item    = bl_ast2_new_node(cnt->ast, BL_NODE_ITEM, tok, bl_item_t);
    item->t = BL_ITEM_STRUCT;
    bl_id_init(&item->id, tok->value.as_string);
    item->node.struct_decl = parse_struct(cnt);
    break;
  }

  case BL_SYM_ENUM: {
    bl_tokens_consume(cnt->tokens);
    tok     = bl_tokens_consume(cnt->tokens);
    item    = bl_ast2_new_node(cnt->ast, BL_NODE_ITEM, tok, bl_item_t);
    item->t = BL_ITEM_ENUM;
    bl_id_init(&item->id, tok->value.as_string);
    item->node.enum_decl = parse_enum(cnt);
    break;
  }

  case BL_SYM_MODULE: {
    bl_tokens_consume(cnt->tokens);
    tok = bl_tokens_consume(cnt->tokens);

    if (tok->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected module name");
    }

    item    = bl_ast2_new_node(cnt->ast, BL_NODE_ITEM, tok, bl_item_t);
    item->t = BL_ITEM_MODULE;
    bl_id_init(&item->id, tok->value.as_string);
    item->node.module = parse_module_rq(cnt, false);
    break;
  }

  default:
    break;
  }

  return item;
}

bl_module_t *
parse_module_rq(context_t *cnt, bool is_gscope)
{
  bl_token_t *tok_begin = NULL;
  if (!is_gscope) {
    tok_begin = bl_tokens_consume(cnt->tokens);
    if (tok_begin->sym != BL_SYM_LBLOCK) {
      parse_error(cnt, BL_ERR_EXPECTED_BODY, tok_begin, "expected module block " BL_YELLOW("'{'"));
    }
  }

  bl_module_t *module = bl_ast2_new_node(cnt->ast, BL_NODE_MODULE, tok_begin, bl_module_t);

  /*
   * Should be extended when nested modules and named modules will
   * be implemented.
   */

  while (true) {
    bl_item_t *item = bl_ast_module_push_item(module, parse_item_maybe(cnt));
    if (item == NULL) {
      break;
    }
  };

  if (!is_gscope) {
    bl_token_t *tok_end = bl_tokens_consume(cnt->tokens);
    if (tok_end->sym != BL_SYM_RBLOCK) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_end,
                  "expected end of the module block " BL_YELLOW("'}'") ", starting: %d:%d",
                  tok_begin->line, tok_begin->col);
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

  unit->ast.root = parse_module_rq(&cnt, true);
  return BL_NO_ERR;
}
