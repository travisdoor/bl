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

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
  bl_ast_t *     ast;
  bl_tokens_t *  tokens;

  /* tmps */
  bl_node_t *curr_fn;
  bl_node_t *curr_compound;
  bool       inside_loop;
} context_t;

static void
parse_ublock_content(context_t *cnt, bl_node_t *ublock);

static int
parse_flags(context_t *cnt, int allowed);

static bl_node_t *
parse_ident(context_t *cnt);

static bl_node_t *
parse_block(context_t *cnt);

static bl_node_t *
parse_decl_value(context_t *cnt);

static bl_node_t *
parse_type(context_t *cnt);

static bl_node_t *
parse_type_fund(context_t *cnt, int ptr);

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
parse_expr_null(context_t *cnt);

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

static bl_node_t *
parse_stmt_loop(context_t *cnt);

static bl_node_t *
parse_stmt_break(context_t *cnt);

static bl_node_t *
parse_stmt_continue(context_t *cnt);

static bl_node_t *
parse_expr_sizeof(context_t *cnt);

static bl_node_t *
parse_expr_cast(context_t *cnt);

// impl

bl_node_t *
parse_expr_cast(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_CAST);
  if (!tok_begin) return NULL;

  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (!bl_token_is(tok, BL_SYM_LPAREN)) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_begin, BL_BUILDER_CUR_WORD,
                "expected '(' after cast expression");
    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok_begin);
  }

  bl_node_t *to_type = parse_type(cnt);
  if (to_type == NULL) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok_err, BL_BUILDER_CUR_WORD,
                "expected type name as cast parameter");
    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok_err);
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (!bl_token_is(tok, BL_SYM_RPAREN)) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected ')' after cast expression");
    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok);
  }

  bl_node_t *next = _parse_expr(cnt, parse_atom_expr(cnt, NULL), bl_token_prec(tok_begin, false));
  if (!next) {
    tok = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok, BL_BUILDER_CUR_WORD,
                "expected expression after cast");
    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok);
  }

  return bl_ast_expr_cast(cnt->ast, tok_begin, to_type, next);
}

bl_node_t *
parse_expr_sizeof(context_t *cnt)
{

  bl_token_t *tok_id = bl_tokens_consume_if(cnt->tokens, BL_SYM_SIZEOF);
  if (!tok_id) return NULL;

  /* eat ( */
  if (!bl_tokens_consume_if(cnt->tokens, BL_SYM_LPAREN)) {
    bl_token_t *tok_err = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_err, BL_BUILDER_CUR_WORD,
                "expected '(' after sizeof buildin");
    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok_err);
  }

  bl_node_t *in = parse_type(cnt);
  if (in == NULL) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok_err, BL_BUILDER_CUR_WORD,
                "expected type name as parameter");

    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok_err);
  }

  /* eat ) */
  if (!bl_tokens_consume_if(cnt->tokens, BL_SYM_RPAREN)) {
    bl_token_t *tok_err = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_err, BL_BUILDER_CUR_WORD,
                "expected ')' after sizeof buildin argument");
    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok_err);
  }

  return bl_ast_expr_sizeof(cnt->ast, tok_id, in, &bl_ftypes[BL_FTYPE_SIZE]);
}

bool
parse_semicolon_rq(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_SEMICOLON);
  if (!tok) {
    tok = bl_tokens_peek_prev(cnt->tokens);
    parse_error(cnt, BL_ERR_MISSING_SEMICOLON, tok, BL_BUILDER_CUR_AFTER, "missing semicolon ';'");
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
    return bl_ast_bad(cnt->ast, err_tok);
  }

  bl_node_t *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD,
                "expected compound statement for true result of the if expression test");
    return bl_ast_bad(cnt->ast, err_tok);
  }

  bl_node_t *false_stmt = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_ELSE)) {
    false_stmt = parse_stmt_if(cnt);
    if (!false_stmt) false_stmt = parse_block(cnt);
    if (false_stmt == NULL) {
      bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD,
                  "expected statement for false result of the if expression test");
      return bl_ast_bad(cnt->ast, err_tok);
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
    return bl_ast_bad(cnt->ast, err_tok);
  }

  bl_node_t *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD, "expected loop body");
    return bl_ast_bad(cnt->ast, err_tok);
  }

  cnt->inside_loop = prev_inside_loop;
  return bl_ast_stmt_loop(cnt->ast, tok_begin, test, true_stmt);
}

bl_node_t *
parse_stmt_loop(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LOOP);
  if (!tok_begin) return NULL;

  const bool prev_inside_loop = cnt->inside_loop;
  cnt->inside_loop            = true;

  bl_token_value_u value;
  value.u              = true;
  bl_node_t *test      = bl_ast_lit(cnt->ast, NULL, &bl_ftypes[BL_FTYPE_BOOL], value);
  bl_node_t *loop      = bl_ast_stmt_loop(cnt->ast, tok_begin, test, NULL);
  bl_node_t *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    bl_token_t *tok_err = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, tok_err, BL_BUILDER_CUR_WORD, "expected loop body");
    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok_err);
  }

  bl_peek_stmt_loop(loop)->true_stmt = true_stmt;
  cnt->inside_loop                   = prev_inside_loop;

  return loop;
}

bl_node_t *
parse_stmt_break(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_BREAK);
  if (!tok) return NULL;

  if (!cnt->inside_loop) {
    parse_error(cnt, BL_ERR_BREAK_OUTSIDE_LOOP, tok, BL_BUILDER_CUR_WORD,
                "break statement outside a loop");
  }
  return bl_ast_stmt_break(cnt->ast, tok);
}

bl_node_t *
parse_stmt_continue(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_CONTINUE);
  if (!tok) return NULL;

  if (!cnt->inside_loop) {
    parse_error(cnt, BL_ERR_CONTINUE_OUTSIDE_LOOP, tok, BL_BUILDER_CUR_WORD,
                "continue statement outside a loop");
  }
  return bl_ast_stmt_continue(cnt->ast, tok);
}

bl_node_t *
parse_literal(context_t *cnt)
{
  bl_token_t *tok  = bl_tokens_peek(cnt->tokens);
  bl_node_t * type = NULL;

  switch (tok->sym) {
  case BL_SYM_NUM:
    type = &bl_ftypes[BL_FTYPE_S32];
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
  return bl_ast_lit(cnt->ast, tok, type, tok->value);
}

bl_node_t *
parse_literal_fn(context_t *cnt)
{
  bl_token_t *tok_fn = bl_tokens_peek(cnt->tokens);
  if (bl_token_is_not(tok_fn, BL_SYM_FN)) return NULL;

  bl_node_t *       fn  = bl_ast_lit_fn(cnt->ast, tok_fn, NULL, NULL,
                                cnt->curr_fn ? cnt->unit->ast.root : cnt->curr_compound,
                                bl_scope_new(cnt->assembly->scope_cache, 32));
  bl_node_lit_fn_t *_fn = bl_peek_lit_fn(fn);

  bl_node_t *prev_fn       = cnt->curr_fn;
  bl_node_t *prev_compound = cnt->curr_compound;
  cnt->curr_fn             = fn;
  cnt->curr_compound       = fn;

  _fn->type = parse_type_fn(cnt, true);
  assert(_fn->type);

  /* parse block */
  _fn->block = parse_block(cnt);
  assert(_fn->block);

  cnt->curr_fn       = prev_fn;
  cnt->curr_compound = prev_compound;
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
  bl_token_t *curr_op = bl_tokens_peek(cnt->tokens);
  if (bl_token_is_unary(curr_op)) {
    bl_tokens_consume(cnt->tokens);
    bl_node_t *next = _parse_expr(cnt, parse_atom_expr(cnt, NULL), bl_token_prec(curr_op, true));

    if (next == NULL) {
      bl_token_t *err_tok = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_EXPR, err_tok, BL_BUILDER_CUR_WORD,
                  "expected expression after unary operator");
      bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
      return bl_ast_bad(cnt->ast, curr_op);
    }

    return bl_ast_expr_unary(cnt->ast, curr_op, curr_op->sym, next, NULL);
  } else {
    return parse_atom_expr(cnt, op);
  }
}

bl_node_t *
parse_atom_expr(context_t *cnt, bl_token_t *op)
{
  bl_node_t *expr = NULL;
  if ((expr = parse_expr_null(cnt))) return expr;
  if ((expr = parse_expr_sizeof(cnt))) return expr;
  if ((expr = parse_expr_cast(cnt))) return expr;
  if ((expr = parse_literal_fn(cnt))) return expr;
  if ((expr = parse_type_struct(cnt, true))) return expr;
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

  while (bl_token_prec(lookahead, false) >= min_precedence) {
    op = lookahead;
    bl_tokens_consume(cnt->tokens);
    rhs       = parse_unary_expr(cnt, op);
    lookahead = bl_tokens_peek(cnt->tokens);

    while (bl_token_prec(lookahead, false) > bl_token_prec(op, false)) {
      rhs       = _parse_expr(cnt, rhs, bl_token_prec(lookahead, false));
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
      bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
      return bl_ast_bad(cnt->ast, op);
    }
  }

  return lhs;
}

bl_node_t *
parse_ident(context_t *cnt)
{
  bl_token_t *tok_ident = bl_tokens_consume_if(cnt->tokens, BL_SYM_IDENT);
  if (!tok_ident) return NULL;

  assert(cnt->curr_compound);
  return bl_ast_ident(cnt->ast, tok_ident, NULL, cnt->curr_compound);
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
  int        ptr  = 0;

  while (bl_tokens_consume_if(cnt->tokens, BL_SYM_ASTERISK)) {
    ++ptr;
  }

  if ((type = parse_type_fn(cnt, false))) return type;
  if ((type = parse_type_struct(cnt, false))) return type;
  if ((type = parse_type_fund(cnt, ptr))) return type;
  return type;
}

bl_node_t *
parse_type_fund(context_t *cnt, int ptr)
{
  bl_node_t *type_ident = parse_ident(cnt);
  if (!type_ident) return NULL;
  assert(ptr >= 0);

  bl_node_ident_t *_ident = bl_peek_ident(type_ident);
  uint64_t         hash;
  bl_array_foreach(bl_ftype_hashes, hash)
  {
    if (hash == _ident->hash) {
      /* here we create new type instance instead of using pointer to static ftypes (fundamental
       * types written by user can be pointers */
      bl_node_t *type = bl_ast_type_fund(cnt->ast, NULL, i, ptr);
      _ident->ref     = type;
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
    return bl_ast_bad(cnt->ast, tok_fn);
  }

  /* parse arg types */
  bl_node_t * arg_types;
  bool        rq         = false;
  bl_node_t **arg_type   = &arg_types;
  int         argc_types = 0;

next:
  *arg_type = named_args ? parse_decl_value(cnt) : parse_type(cnt);
  if (*arg_type) {
    /* validate argument */
    if (bl_node_is(*arg_type, BL_NODE_DECL_VALUE)) {
      bl_node_decl_value_t *_arg_decl = bl_peek_decl_value(*arg_type);
      if (_arg_decl->value) {
        parse_error_node(cnt, BL_ERR_INVALID_ARG_TYPE, *arg_type, BL_BUILDER_CUR_WORD,
                         "function arguments cannot have value binding");
        *arg_type = bl_ast_bad(cnt->ast, NULL);
      }
    }
    arg_type = &(*arg_type)->next;
    ++argc_types;

    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                "expected type after comma ','");
    return bl_ast_bad(cnt->ast, tok_fn);
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected end of argument type list  ')'  or another type separated by comma");
    return bl_ast_bad(cnt->ast, tok_fn);
  }

  bl_node_t *ret_type = parse_type(cnt);
  if (!ret_type) ret_type = &bl_ftypes[BL_FTYPE_VOID];
  return bl_ast_type_fn(cnt->ast, tok_fn, arg_types, argc_types, ret_type);
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
    return bl_ast_bad(cnt->ast, tok_struct);
  }

  /* parse arg types */
  bl_node_t * types;
  bool        rq     = false;
  bl_node_t **type   = &types;
  int         typesc = 0;

next:
  *type = named_args ? parse_decl_value(cnt) : parse_type(cnt);
  if (*type) {
    /* validate argument */
    if (bl_node_is(*type, BL_NODE_DECL_VALUE)) {
      bl_node_decl_value_t *_member_decl = bl_peek_decl_value(*type);
      if (_member_decl->value) {
        parse_error_node(cnt, BL_ERR_INVALID_TYPE, _member_decl->value, BL_BUILDER_CUR_WORD,
                         "struct member cannot have initialization");
        *type = bl_ast_bad(cnt->ast, NULL);
      }
    }
    type = &(*type)->next;
    ++typesc;

    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                "expected member after comma ','");
    return bl_ast_bad(cnt->ast, tok_struct);
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RBLOCK) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected end of member list  '}'  or another memeber separated by comma");
    return bl_ast_bad(cnt->ast, tok_struct);
  }

  return bl_ast_type_struct(cnt->ast, tok_struct, types, typesc);
}

bl_node_t *
parse_decl_value(context_t *cnt)
{
  int         flags     = 0;
  bl_token_t *tok_ident = bl_tokens_peek(cnt->tokens);
  if (bl_token_is_not(tok_ident, BL_SYM_IDENT)) return NULL;
  /* is value declaration? */
  if (bl_token_is(bl_tokens_peek_2nd(cnt->tokens), BL_SYM_ASSIGN)) return NULL;
  bl_token_t *tok_lookehead = bl_tokens_peek_2nd(cnt->tokens);
  switch (tok_lookehead->sym) {
  case BL_SYM_IDENT:
  case BL_SYM_ASTERISK:
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

  if (bl_ast_is_buildin_type(ident) != -1) {
    parse_error_node(cnt, BL_ERR_INVALID_NAME, ident, BL_BUILDER_CUR_WORD,
                     "'%s' is reserved name of buildin type", tok_ident->value.str);
  }

  {
    int buildin = bl_ast_is_buildin(ident);
    if (buildin == BL_BUILDIN_MAIN) {
      /* main function */
      flags |= BL_FLAG_MAIN;
    }
  }

  bool mutable           = true;
  bl_node_t * type       = parse_type(cnt);
  bl_token_t *tok_assign = bl_tokens_consume_if(cnt->tokens, BL_SYM_MDECL);
  if (!tok_assign) tok_assign = bl_tokens_consume_if(cnt->tokens, BL_SYM_IMMDECL);

  if (!type && !tok_assign) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_INITIALIZATION, tok_err, BL_BUILDER_CUR_WORD,
                "expected binding of declaration to some value when type is not specified");
    return bl_ast_bad(cnt->ast, tok_err);
  }

  bl_node_t *value = NULL;
  if (tok_assign) {
    mutable = bl_token_is(tok_assign, BL_SYM_MDECL);
    flags |= parse_flags(cnt, BL_FLAG_EXTERN);

    if (!(flags & BL_FLAG_EXTERN)) {
      value = parse_value(cnt);

      if (!value) {
        parse_error(cnt, BL_ERR_EXPECTED_INITIALIZATION, tok_assign, BL_BUILDER_CUR_AFTER,
                    "expected binding of declaration to some value");
        return bl_ast_bad(cnt->ast, tok_assign);
      }
    }
  }

  if (flags & BL_FLAG_MAIN) {
    /* main function */
    if (mutable) {
      if (tok_assign) {
        parse_error(cnt, BL_ERR_INVALID_MUTABILITY, tok_assign, BL_BUILDER_CUR_WORD,
                    "'main' is expected to be immutable function");
      } else {
        parse_error_node(cnt, BL_ERR_INVALID_MUTABILITY, ident, BL_BUILDER_CUR_WORD,
                         "'main' is expected to be immutable function");
      }

      return bl_ast_bad(cnt->ast, tok_assign);
    }

    if (flags & BL_FLAG_EXTERN) {
      parse_error_node(cnt, BL_ERR_UNEXPECTED_MODIF, ident, BL_BUILDER_CUR_WORD,
                       "main function cannot be extern");
      return bl_ast_bad(cnt->ast, tok_assign);
    }
  }

  if (flags & BL_FLAG_EXTERN) {
    if (mutable) {
      parse_error(cnt, BL_ERR_INVALID_MUTABILITY, tok_assign, BL_BUILDER_CUR_WORD,
                  "extern declaration cannot be mutable");
      return bl_ast_bad(cnt->ast, tok_assign);
    }

    if (!type) {
      parse_error_node(cnt, BL_ERR_EXPECTED_TYPE, ident, BL_BUILDER_CUR_AFTER,
                       "extern declaration must have type specified");
      return bl_ast_bad(cnt->ast, tok_assign);
    }
  }

  return bl_ast_decl_value(cnt->ast, tok_ident, ident, type, value, mutable, flags);
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
    return bl_ast_bad(cnt->ast, tok_id);
  }

  /* parse args */
  bool        rq = false;
  bl_node_t * args;
  bl_node_t **arg   = &args;
  int         argsc = 0;
arg:
  *arg = parse_expr(cnt);
  if (*arg) {
    ++argsc;
    arg = &(*arg)->next;

    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      rq = true;
      goto arg;
    }
  } else if (rq) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                "expected function argument after comma ','");
    // return bl_ast_bad(cnt->ast, tok_id);
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected end of parameter list ')' or another parameter separated by comma");
    // return bl_ast_bad(cnt->ast, tok_id);
  }

  return bl_ast_expr_call(cnt->ast, tok_id, ident, args, argsc, NULL);
}

bl_node_t *
parse_expr_null(context_t *cnt)
{
  bl_token_t *tok_null = bl_tokens_consume_if(cnt->tokens, BL_SYM_NULL);
  if (!tok_null) return NULL;
  return bl_ast_expr_null(cnt->ast, tok_null, NULL);
}

int
parse_flags(context_t *cnt, int allowed)
{
  int         flags = 0;
  bl_token_t *tok;
next:
  tok = bl_tokens_peek(cnt->tokens);
  switch (tok->sym) {
  case BL_SYM_EXTERN:
    bl_tokens_consume(cnt->tokens);
    if (!(allowed & BL_FLAG_EXTERN)) {
      parse_error(cnt, BL_ERR_UNEXPECTED_MODIF, tok, BL_BUILDER_CUR_WORD, "unexpected flag");
    } else {
      flags |= BL_FLAG_EXTERN;
    }
    goto next;
  default:
    break;
  }

  return flags;
}

bl_node_t *
parse_block(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBLOCK);
  if (!tok_begin) return NULL;

  bl_node_t *           prev_compound = cnt->curr_compound;
  bl_node_t *           block  = bl_ast_decl_block(cnt->ast, tok_begin, NULL, cnt->curr_compound,
                                       bl_scope_new(cnt->assembly->scope_cache, 1024));
  bl_node_decl_block_t *_block = bl_peek_decl_block(block);
  cnt->curr_compound           = block;

  bl_token_t *tok;
  bl_node_t **node = &_block->nodes;
next:
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
    tok = bl_tokens_consume(cnt->tokens);
    parse_warning(cnt, tok, BL_BUILDER_CUR_WORD, "extra semicolon can be removed ';'");
    goto next;
  }

  parse_flags(cnt, 0);

  if ((*node = parse_stmt_return(cnt))) {
    node = &(*node)->next;
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_stmt_if(cnt))) {
    node = &(*node)->next;
    goto next;
  }

  if ((*node = parse_stmt_while(cnt))) {
    node = &(*node)->next;
    goto next;
  }

  if ((*node = parse_stmt_loop(cnt))) {
    node = &(*node)->next;
    goto next;
  }

  if ((*node = parse_stmt_break(cnt))) {
    node = &(*node)->next;
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_stmt_continue(cnt))) {
    node = &(*node)->next;
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_decl_value(cnt))) {
    node = &(*node)->next;
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_expr(cnt))) {
    node = &(*node)->next;
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_block(cnt))) {
    node = &(*node)->next;
    goto next;
  }

  tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_RBLOCK);
  if (!tok) {
    tok = bl_tokens_peek_prev(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_BODY_END, tok, BL_BUILDER_CUR_AFTER,
                "expected '}', starting %d:%d", tok_begin->src.line, tok_begin->src.col);
    cnt->curr_compound = prev_compound;
    return bl_ast_bad(cnt->ast, tok_begin);
  }

  cnt->curr_compound = prev_compound;
  return block;
}

void
parse_ublock_content(context_t *cnt, bl_node_t *ublock)
{
  cnt->curr_compound             = ublock;
  bl_node_decl_ublock_t *_ublock = bl_peek_decl_ublock(ublock);
  bl_node_t **           node    = &_ublock->nodes;
decl:
  parse_flags(cnt, 0);

  if ((*node = parse_decl_value(cnt))) {
    node = &(*node)->next;
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
bl_parser_run(bl_builder_t *builder, bl_assembly_t *assembly, bl_unit_t *unit)
{
  context_t cnt = {.builder       = builder,
                   .assembly      = assembly,
                   .unit          = unit,
                   .ast           = &unit->ast,
                   .tokens        = &unit->tokens,
                   .curr_fn       = NULL,
                   .curr_compound = NULL,
                   .inside_loop   = false};

  unit->ast.root = bl_ast_decl_ublock(&unit->ast, NULL, unit, assembly->gscope);
  parse_ublock_content(&cnt, unit->ast.root);
}
