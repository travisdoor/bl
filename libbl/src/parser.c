//************************************************************************************************
// bl
//
// File:   parser.c
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
//************************************************************************************************

#include <setjmp.h>
#include "stages_impl.h"
#include "common_impl.h"

#define parse_error(cnt, code, tok, pos, format, ...)                                              \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), &(tok)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
  }

#define parse_error_node(cnt, code, node, pos, format, ...)                                        \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), (node)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
  }

#define parse_warning(cnt, tok, pos, format, ...)                                                  \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, &(tok)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

#define parse_warning_node(cnt, node, pos, format, ...)                                            \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, (node)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

#define push(_curr, _prev)                                                                         \
  {                                                                                                \
    (*(_curr))->prev = (_prev);                                                                    \
    (_prev)          = *(_curr);                                                                   \
    (_curr)          = &(*(_curr))->next;                                                          \
  }

typedef struct
{
  bl_builder_t *builder;
  bl_unit_t *   unit;
  bl_ast_t *    ast;
  bl_tokens_t * tokens;

  /* tmps */
  bl_node_t *curr_fn;
  bool       inside_loop;
} context_t;

static void
parse_ublock_content(context_t *cnt, bl_node_t *ublock);

static bl_node_t *
parse_ident(context_t *cnt);

static bl_node_t *
parse_block(context_t *cnt);

static bl_node_t *
parse_decl_value(context_t *cnt);

static bl_node_t *
parse_type(context_t *cnt);

static bl_node_t *
parse_type_fund(context_t *cnt);

static bl_node_t *
parse_type_fn(context_t *cnt, bool named_args);

static bl_node_t *
parse_type_struct(context_t *cnt, bool named_args);

static bl_node_t *
parse_unary_expr(context_t *cnt, bl_token_t *op);

static bl_node_t *
parse_atom_expr(context_t *cnt, bl_token_t *op);

static bl_node_t *
_parse_expr(context_t *cnt, bl_node_t *lhs, int min_precedence);

static inline bl_node_t *
parse_expr(context_t *cnt);

static bl_node_t *
parse_expr_call(context_t *cnt);

static bl_node_t *
parse_value(context_t *cnt);

static bl_node_t *
parse_literal(context_t *cnt);

static bl_node_t *
parse_literal_fn(context_t *cnt);

static inline bool
parse_semicolon_rq(context_t *cnt);

static bl_node_t *
parse_stmt_return(context_t *cnt);

static bl_node_t *
parse_stmt_if(context_t *cnt);

static bl_node_t *
parse_stmt_while(context_t *cnt);
// impl

bool
parse_semicolon_rq(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_SEMICOLON);
  if (!tok) {
    tok = bl_tokens_peek_prev(cnt->tokens);
    parse_error(cnt, BL_ERR_MISSING_SEMICOLON, tok, BL_BUILDER_CUR_AFTER,
                "missing semicolon " BL_YELLOW("';'"));
    return false;
  }
  return true;
}

bl_node_t *
parse_stmt_return(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_RETURN);
  if (!tok_begin) {
    return NULL;
  }

  bl_node_t *expr = parse_expr(cnt);
  return bl_ast_stmt_return(cnt->ast, tok_begin, expr, cnt->curr_fn);
}

bl_node_t *
parse_stmt_if(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_IF);
  if (!tok_begin) return NULL;

  bl_node_t *test = parse_expr(cnt);
  if (test == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, err_tok, BL_BUILDER_CUR_WORD,
                "expected expression for the if statement");
    return bl_ast_stmt_bad(cnt->ast, err_tok);
  }

  bl_node_t *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD,
                "expected compound statement for true result of the if expression test");
    return bl_ast_stmt_bad(cnt->ast, err_tok);
  }

  bl_node_t *false_stmt = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_ELSE)) {
    false_stmt = parse_block(cnt);
    if (false_stmt == NULL) {
      bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD,
                  "expected statement for false result of the if expression test");
      return bl_ast_stmt_bad(cnt->ast, err_tok);
    }
  }

  return bl_ast_stmt_if(cnt->ast, tok_begin, test, true_stmt, false_stmt);
}

bl_node_t *
parse_stmt_while(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_WHILE);
  if (!tok_begin) {
    return NULL;
  }

  const bool prev_inside_loop = cnt->inside_loop;
  cnt->inside_loop            = true;

  bl_node_t *test = parse_expr(cnt);
  if (!test) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, err_tok, BL_BUILDER_CUR_WORD,
                "expected expression for the while statement");
    return bl_ast_stmt_bad(cnt->ast, err_tok);
  }

  bl_node_t *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD, "expected loop body");
    return bl_ast_stmt_bad(cnt->ast, err_tok);
  }

  cnt->inside_loop = prev_inside_loop;
  return bl_ast_stmt_loop(cnt->ast, tok_begin, test, true_stmt);
}

bl_node_t *
parse_literal(context_t *cnt)
{
  bl_token_t *tok  = bl_tokens_peek(cnt->tokens);
  bl_node_t * type = NULL;

  switch (tok->sym) {
  case BL_SYM_NUM:
    type = &bl_ftypes[BL_FTYPE_I32];
    break;
  case BL_SYM_CHAR:
    type = &bl_ftypes[BL_FTYPE_CHAR];
    break;
  case BL_SYM_STRING:
    type = &bl_ftypes[BL_FTYPE_STRING];
    break;
  case BL_SYM_TRUE:
    tok->value.u = true;
    type         = &bl_ftypes[BL_FTYPE_BOOL];
    break;
  case BL_SYM_FALSE:
    tok->value.u = false;
    type         = &bl_ftypes[BL_FTYPE_BOOL];
    break;
  case BL_SYM_FLOAT:
    type = &bl_ftypes[BL_FTYPE_F32];
    break;
  case BL_SYM_DOUBLE:
    type = &bl_ftypes[BL_FTYPE_F64];
    break;
  default:
    return NULL;
  }

  bl_tokens_consume(cnt->tokens);
  return bl_ast_lit(cnt->ast, tok, type);
}

bl_node_t *
parse_literal_fn(context_t *cnt)
{
  bl_token_t *tok_fn = bl_tokens_peek(cnt->tokens);
  bl_node_t * type   = parse_type_fn(cnt, true);
  if (!type) return NULL;
  bl_node_t *       fn  = bl_ast_lit_fn(cnt->ast, tok_fn, type, NULL);
  bl_node_lit_fn_t *_fn = bl_peek_lit_fn(fn);

  bl_node_t *prev_fn = cnt->curr_fn;
  cnt->curr_fn       = fn;

  _fn->block = parse_block(cnt);
  assert(_fn->block);

  cnt->curr_fn = prev_fn;
  return fn;
}

bl_node_t *
parse_expr(context_t *cnt)
{
  return _parse_expr(cnt, parse_unary_expr(cnt, NULL), 0);
}

bl_node_t *
parse_unary_expr(context_t *cnt, bl_token_t *op)
{
  // bl_node_t *expr = NULL;
  // if ((expr = parse_array_ref_maybe(cnt, op))) return expr;
  return parse_atom_expr(cnt, op);
}

bl_node_t *
parse_atom_expr(context_t *cnt, bl_token_t *op)
{
  bl_node_t *expr = NULL;
  if ((expr = parse_expr_call(cnt))) return expr;
  if ((expr = parse_literal(cnt))) return expr;
  if ((expr = parse_ident(cnt))) return expr;
  return expr;
}

bl_node_t *
_parse_expr(context_t *cnt, bl_node_t *lhs, int min_precedence)
{
  bl_node_t * rhs       = NULL;
  bl_token_t *lookahead = bl_tokens_peek(cnt->tokens);
  bl_token_t *op        = NULL;

  while (bl_token_prec(lookahead) >= min_precedence) {
    op = lookahead;
    bl_tokens_consume(cnt->tokens);
    rhs       = parse_unary_expr(cnt, op);
    lookahead = bl_tokens_peek(cnt->tokens);

    while (bl_token_prec(lookahead) > bl_token_prec(op)) {
      rhs       = _parse_expr(cnt, rhs, bl_token_prec(lookahead));
      lookahead = bl_tokens_peek(cnt->tokens);
    }

    if (bl_token_is_binop(op)) {
      bl_node_t *result_type = NULL;
      bl_node_t *tmp         = lhs;

      /* Set result type to bool for logical binary operations, this is used for type checking later
       * in the compiler pipeline. Other types are checked recursively. */
      if (bl_token_is_logic_op(op)) {
        // IDEA use ident reference instead???
        result_type = &bl_ftypes[BL_FTYPE_BOOL];
      }

      lhs = bl_ast_expr_binop(cnt->ast, op, tmp, rhs, result_type, op->sym);
    } else {
      parse_error(cnt, BL_ERR_EXPECTED_BINOP, op, BL_BUILDER_CUR_WORD, "expected binary operation");
      return bl_ast_expr_bad(cnt->ast, op);
    }
  }

  return lhs;
}

bl_node_t *
parse_ident(context_t *cnt)
{
  bl_token_t *tok_ident = bl_tokens_consume_if(cnt->tokens, BL_SYM_IDENT);
  if (!tok_ident) return NULL;
  return bl_ast_ident(cnt->ast, tok_ident, NULL);
}

bl_node_t *
parse_value(context_t *cnt)
{
  bl_node_t *value = NULL;
  if ((value = parse_type_struct(cnt, true))) return value;
  if ((value = parse_literal_fn(cnt))) return value;
  if ((value = parse_expr(cnt))) return value;
  return value;
}

bl_node_t *
parse_type(context_t *cnt)
{
  bl_node_t *type = NULL;
  if ((type = parse_type_fn(cnt, false))) return type;
  if ((type = parse_type_struct(cnt, false))) return type;
  if ((type = parse_type_fund(cnt))) return type;
  return type;
}

bl_node_t *
parse_type_fund(context_t *cnt)
{
  bl_node_t *type_ident = parse_ident(cnt);
  if (!type_ident) return NULL;

  bl_node_ident_t *_ident = bl_peek_ident(type_ident);
  uint64_t         hash;
  bl_array_foreach(bl_ftype_hashes, hash)
  {
    if (hash == _ident->hash) {
      _ident->ref = &bl_ftypes[i];
      return type_ident;
    }
  }

  return type_ident;
}

bl_node_t *
parse_type_fn(context_t *cnt, bool named_args)
{
  bl_token_t *tok_fn = bl_tokens_consume_if(cnt->tokens, BL_SYM_FN);
  if (!tok_fn) return NULL;

  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_LPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected function parameter list");
    return bl_ast_type_bad(cnt->ast, tok_fn);
  }

  /* parse arg types */
  bl_node_t * arg_types;
  bool        rq       = false;
  bl_node_t * prev     = NULL;
  bl_node_t **arg_type = &arg_types;

next:
  *arg_type = named_args ? parse_decl_value(cnt) : parse_type(cnt);
  if (*arg_type) {

    /* validate argument */
    if (bl_node_is(*arg_type, BL_NODE_DECL_VALUE)) {
      bl_node_decl_value_t *_arg_decl = bl_peek_decl_value(*arg_type);
      if (_arg_decl->value) {
        parse_error_node(cnt, BL_ERR_INVALID_ARG_TYPE, *arg_type, BL_BUILDER_CUR_WORD,
                         "function arguments cannot have value binding");
        *arg_type = bl_ast_decl_bad(cnt->ast, NULL);
      }
    }
    push(arg_type, prev);

    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                "expected type after comma ','");
    return bl_ast_type_bad(cnt->ast, tok_fn);
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected end of argument type list  ')'  or another type separated by comma");
    return bl_ast_type_bad(cnt->ast, tok_fn);
  }

  bl_node_t *ret_type = parse_type(cnt);
  if (!ret_type) ret_type = &bl_ftypes[BL_FTYPE_VOID];
  return bl_ast_type_fn(cnt->ast, tok_fn, arg_types, ret_type);
}

bl_node_t *
parse_type_struct(context_t *cnt, bool named_args)
{
  bl_token_t *tok_struct = bl_tokens_consume_if(cnt->tokens, BL_SYM_STRUCT);
  if (!tok_struct) return NULL;

  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_LBLOCK) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected struct member list");
    return bl_ast_type_bad(cnt->ast, tok_struct);
  }

  /* parse arg types */
  bl_node_t * types;
  bool        rq   = false;
  bl_node_t * prev = NULL;
  bl_node_t **type = &types;

next:
  *type = named_args ? parse_decl_value(cnt) : parse_type(cnt);
  if (*type) {
    /* validate argument */
    if (bl_node_is(*type, BL_NODE_DECL_VALUE)) {
      bl_node_decl_value_t *_member_decl = bl_peek_decl_value(*type);
      if (_member_decl->value) {
        parse_error_node(cnt, BL_ERR_INVALID_TYPE, _member_decl->value, BL_BUILDER_CUR_WORD,
                         "struct member cannot have initialization");
        *type = bl_ast_decl_bad(cnt->ast, NULL);
      }
    }
    push(type, prev);

    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                "expected member after comma ','");
    return bl_ast_type_bad(cnt->ast, tok_struct);
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RBLOCK) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected end of member list  '}'  or another memeber separated by comma");
    return bl_ast_type_bad(cnt->ast, tok_struct);
  }

  return bl_ast_type_struct(cnt->ast, tok_struct, types);
}

bl_node_t *
parse_decl_value(context_t *cnt)
{
  bl_token_t *tok_ident = bl_tokens_peek(cnt->tokens);
  if (bl_token_is_not(tok_ident, BL_SYM_IDENT)) return NULL;
  /* is value declaration? */
  bl_token_t *tok_lookehead = bl_tokens_peek_2nd(cnt->tokens);
  switch (tok_lookehead->sym) {
  case BL_SYM_IDENT:
  case BL_SYM_FN:
  case BL_SYM_STRUCT:
  case BL_SYM_ENUM:
  case BL_SYM_IMMDECL:
  case BL_SYM_MDECL:
    break;
  default:
    return NULL;
  }

  bl_node_t *ident = parse_ident(cnt);
  if (!ident) return NULL;

  bool mutable           = true;
  bl_node_t * type       = parse_type(cnt);
  bl_token_t *tok_assign = bl_tokens_consume_if(cnt->tokens, BL_SYM_MDECL);
  if (!tok_assign) tok_assign = bl_tokens_consume_if(cnt->tokens, BL_SYM_IMMDECL);

  if (!type && !tok_assign) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_INITIALIZATION, tok_err, BL_BUILDER_CUR_WORD,
                "expected binding of declaration to some value when type is not specified");
    return bl_ast_decl_bad(cnt->ast, tok_err);
  }

  bl_node_t *value = NULL;
  if (tok_assign) {
    value   = parse_value(cnt);
    mutable = bl_token_is(tok_assign, BL_SYM_MDECL);

    if (!value) {
      parse_error(cnt, BL_ERR_EXPECTED_INITIALIZATION, tok_assign, BL_BUILDER_CUR_AFTER,
                  "expected binding of declaration to some value");
      return bl_ast_decl_bad(cnt->ast, tok_assign);
    }
  }

  return bl_ast_decl(cnt->ast, tok_ident, ident, type, value, mutable);
}

bl_node_t *
parse_expr_call(context_t *cnt)
{
  if (!bl_tokens_is_seq(cnt->tokens, 2, BL_SYM_IDENT, BL_SYM_LPAREN)) return NULL;

  bl_token_t *tok_id = bl_tokens_peek(cnt->tokens);
  bl_node_t * ident  = parse_ident(cnt);
  bl_token_t *tok    = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_LPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected function parameter list");
    return bl_ast_expr_bad(cnt->ast, tok_id);
  }

  /* parse args */
  bool        rq = false;
  bl_node_t * args;
  bl_node_t * prev  = NULL;
  bl_node_t **arg   = &args;
  int         argsc = 0;
arg:
  *arg = parse_expr(cnt);
  if (*arg) {
    ++argsc;
    push(arg, prev);

    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      rq = true;
      goto arg;
    }
  } else if (rq) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                "expected function argument after comma ','");
    // return bl_ast_expr_bad(cnt->ast, tok_id);
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected end of parameter list ')' or another parameter separated by comma");
    // return bl_ast_expr_bad(cnt->ast, tok_id);
  }

  return bl_ast_expr_call(cnt->ast, tok_id, ident, args, argsc, NULL);
}

bl_node_t *
parse_block(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBLOCK);

  if (tok_begin == NULL) {
    return NULL;
  }

  bl_node_t * nodes;
  bl_token_t *tok;
  bl_node_t * prev = NULL;
  bl_node_t **node = &nodes;
next:
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
    tok = bl_tokens_consume(cnt->tokens);
    parse_warning(cnt, tok, BL_BUILDER_CUR_WORD, "extra semicolon can be removed ';'");
    goto next;
  }

  if ((*node = parse_stmt_return(cnt))) {
    push(node, prev);
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_stmt_if(cnt))) {
    push(node, prev);
    goto next;
  }

  if ((*node = parse_stmt_while(cnt))) {
    push(node, prev);
    goto next;
  }

  if ((*node = parse_decl_value(cnt))) {
    push(node, prev);
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_expr(cnt))) {
    push(node, prev);
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_block(cnt))) {
    push(node, prev);
    goto next;
  }

  tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_RBLOCK);
  if (!tok) {
    tok = bl_tokens_peek_prev(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_BODY_END, tok, BL_BUILDER_CUR_AFTER,
                "expected '}', starting %d:%d", tok_begin->src.line, tok_begin->src.col);
    return bl_ast_decl_bad(cnt->ast, tok_begin);
  }

  return bl_ast_block(cnt->ast, tok_begin, nodes);
}

void
parse_ublock_content(context_t *cnt, bl_node_t *ublock)
{
  bl_node_ublock_t *_ublock = bl_peek_ublock(ublock);
  bl_node_t *       prev    = NULL;
  bl_node_t **      node    = &_ublock->nodes;
decl:
  if ((*node = parse_decl_value(cnt))) {
    push(node, prev);
    parse_semicolon_rq(cnt);
    goto decl;
  }

  bl_token_t *tok = bl_tokens_peek(cnt->tokens);
  if (!bl_token_is(tok, BL_SYM_RBLOCK) && !bl_token_is(tok, BL_SYM_EOF)) {
    parse_error(cnt, BL_ERR_UNEXPECTED_SYMBOL, tok, BL_BUILDER_CUR_WORD,
                "unexpected symbol in module body");
  }
}

void
bl_parser_run(bl_builder_t *builder, bl_unit_t *unit)
{
  context_t cnt = {.builder     = builder,
                   .unit        = unit,
                   .ast         = &unit->ast,
                   .tokens      = &unit->tokens,
                   .curr_fn     = NULL,
                   .inside_loop = false};

  unit->ast.root = bl_ast_ublock(&unit->ast, NULL);
  parse_ublock_content(&cnt, unit->ast.root);
}
