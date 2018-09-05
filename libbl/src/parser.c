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
  node_t *curr_fn;
  node_t *curr_decl;
  node_t *curr_compound;
  bool    inside_loop;
  bool    core_loaded;
} context_t;

/* helpers */
static inline void
insert_node(node_t ***node)
{
  *node = &(**node)->next;
}

/* fw decls */
static node_t *
load_core(context_t *cnt);

static node_t *
parse_load(context_t *cnt);

static node_t *
parse_run(context_t *cnt);

static node_t *
parse_link(context_t *cnt);

static void
parse_ublock_content(context_t *cnt, node_t *ublock);

static int
parse_flags(context_t *cnt, int allowed);

static node_t *
parse_ident(context_t *cnt, int ptr);

static node_t *
parse_block(context_t *cnt);

static node_t *
parse_decl(context_t *cnt);

static node_t *
parse_arr(context_t *cnt);

static node_t *
parse_type(context_t *cnt);

static node_t *
parse_type_fund(context_t *cnt, int ptr);

static node_t *
parse_type_fn(context_t *cnt, bool named_args, int ptr);

static node_t *
parse_type_struct(context_t *cnt, bool named_args, int ptr);

static node_t *
parse_type_enum(context_t *cnt, int ptr);

static node_t *
parse_unary_expr(context_t *cnt, bl_token_t *op);

static node_t *
parse_expr_member(context_t *cnt, bl_token_t *op);

static node_t *
parse_atom_expr(context_t *cnt, bl_token_t *op);

static node_t *
_parse_expr(context_t *cnt, node_t *lhs, int min_precedence);

static inline node_t *
parse_expr(context_t *cnt);

static node_t *
parse_expr_nested(context_t *cnt);

static node_t *
parse_expr_call(context_t *cnt);

static node_t *
parse_expr_null(context_t *cnt);

static node_t *
parse_value(context_t *cnt);

static node_t *
parse_literal(context_t *cnt);

static node_t *
parse_literal_fn(context_t *cnt);

static node_t *
parse_literal_struct(context_t *cnt);

static node_t *
parse_literal_enum(context_t *cnt);

static inline bool
parse_semicolon_rq(context_t *cnt);

static node_t *
parse_stmt_return(context_t *cnt);

static node_t *
parse_stmt_if(context_t *cnt);

static node_t *
parse_stmt_while(context_t *cnt);

static node_t *
parse_stmt_loop(context_t *cnt);

static node_t *
parse_stmt_break(context_t *cnt);

static node_t *
parse_stmt_continue(context_t *cnt);

static node_t *
parse_expr_sizeof(context_t *cnt);

static node_t *
parse_expr_cast(context_t *cnt);

static node_t *
parse_expr_elem(context_t *cnt, bl_token_t *op);

// impl

node_t *
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

  node_t *to_type = parse_type(cnt);
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

  node_t *next = _parse_expr(cnt, parse_atom_expr(cnt, NULL), bl_token_prec(tok_begin, false));
  if (!next) {
    tok = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok, BL_BUILDER_CUR_WORD,
                "expected expression after cast");
    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok);
  }

  return bl_ast_expr_cast(cnt->ast, tok_begin, to_type, next);
}

node_t *
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

  node_t *in = parse_type(cnt);
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

node_t *
parse_stmt_return(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_RETURN);
  if (!tok_begin) {
    return NULL;
  }

  node_t *expr = parse_expr(cnt);
  return bl_ast_stmt_return(cnt->ast, tok_begin, expr, cnt->curr_decl);
}

node_t *
parse_stmt_if(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_IF);
  if (!tok_begin) return NULL;

  node_t *test = parse_expr(cnt);
  if (test == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, err_tok, BL_BUILDER_CUR_WORD,
                "expected expression for the if statement");
    return bl_ast_bad(cnt->ast, err_tok);
  }

  if (node_is(test, NODE_BAD)) {
    bl_tokens_consume_till(cnt->tokens, BL_SYM_LBLOCK);
  }

  node_t *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD,
                "expected compound statement for true result of the if expression test");
    return bl_ast_bad(cnt->ast, err_tok);
  }

  node_t *false_stmt = NULL;
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

node_t *
parse_stmt_while(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_WHILE);
  if (!tok_begin) {
    return NULL;
  }

  const bool prev_inside_loop = cnt->inside_loop;
  cnt->inside_loop            = true;

  node_t *test = parse_expr(cnt);
  if (!test) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, err_tok, BL_BUILDER_CUR_WORD,
                "expected expression for the while statement");
    return bl_ast_bad(cnt->ast, err_tok);
  }

  node_t *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD, "expected loop body");
    return bl_ast_bad(cnt->ast, err_tok);
  }

  cnt->inside_loop = prev_inside_loop;
  return bl_ast_stmt_loop(cnt->ast, tok_begin, test, true_stmt);
}

node_t *
parse_stmt_loop(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LOOP);
  if (!tok_begin) return NULL;

  const bool prev_inside_loop = cnt->inside_loop;
  cnt->inside_loop            = true;

  bl_token_value_u value;
  value.u           = true;
  node_t *test      = bl_ast_lit(cnt->ast, NULL, &bl_ftypes[BL_FTYPE_BOOL], value);
  node_t *loop      = bl_ast_stmt_loop(cnt->ast, tok_begin, test, NULL);
  node_t *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    bl_token_t *tok_err = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, tok_err, BL_BUILDER_CUR_WORD, "expected loop body");
    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok_err);
  }

  peek_stmt_loop(loop)->true_stmt = true_stmt;
  cnt->inside_loop                = prev_inside_loop;

  return loop;
}

node_t *
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

node_t *
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

node_t *
parse_literal(context_t *cnt)
{
  bl_token_t *tok  = bl_tokens_peek(cnt->tokens);
  node_t *    type = NULL;

  switch (tok->sym) {
  case BL_SYM_NUM: type = &bl_ftypes[BL_FTYPE_S32]; break;
  case BL_SYM_CHAR: type = &bl_ftypes[BL_FTYPE_CHAR]; break;
  case BL_SYM_STRING: type = &bl_ftypes[BL_FTYPE_STRING]; break;
  case BL_SYM_TRUE:
    tok->value.u = true;
    type         = &bl_ftypes[BL_FTYPE_BOOL];
    break;
  case BL_SYM_FALSE:
    tok->value.u = false;
    type         = &bl_ftypes[BL_FTYPE_BOOL];
    break;
  case BL_SYM_FLOAT: type = &bl_ftypes[BL_FTYPE_F32]; break;
  case BL_SYM_DOUBLE: type = &bl_ftypes[BL_FTYPE_F64]; break;
  default: return NULL;
  }

  bl_tokens_consume(cnt->tokens);
  return bl_ast_lit(cnt->ast, tok, type, tok->value);
}

node_t *
parse_literal_fn(context_t *cnt)
{
  bl_token_t *tok_fn = bl_tokens_peek(cnt->tokens);
  if (bl_token_is_not(tok_fn, BL_SYM_FN)) return NULL;

  node_t *       fn  = bl_ast_lit_fn(cnt->ast, tok_fn, NULL, NULL,
                             cnt->curr_fn ? cnt->unit->ast.root : cnt->curr_compound,
                             bl_scope_new(cnt->assembly->scope_cache, 32));
  node_lit_fn_t *_fn = peek_lit_fn(fn);

  node_t *prev_fn       = cnt->curr_fn;
  node_t *prev_compound = cnt->curr_compound;
  cnt->curr_fn          = fn;
  cnt->curr_compound    = fn;

  _fn->type = parse_type_fn(cnt, true, 0);
  assert(_fn->type);

  /* parse block */
  _fn->block = parse_block(cnt);
  assert(_fn->block);

  cnt->curr_fn       = prev_fn;
  cnt->curr_compound = prev_compound;
  return fn;
}

node_t *
parse_literal_struct(context_t *cnt)
{
  bl_token_t *tok_struct = bl_tokens_peek(cnt->tokens);
  if (bl_token_is_not(tok_struct, BL_SYM_STRUCT)) return NULL;

  node_t *prev_compound = cnt->curr_compound;

  node_t *           result      = bl_ast_lit_struct(cnt->ast, tok_struct, NULL, cnt->curr_compound,
                                     bl_scope_new(cnt->assembly->scope_cache, 64));
  node_lit_struct_t *_lit_struct = peek_lit_struct(result);

  cnt->curr_compound = result;
  _lit_struct->type  = parse_type_struct(cnt, true, 0);
  cnt->curr_compound = prev_compound;
  assert(_lit_struct->type);

  return result;
}

node_t *
parse_literal_enum(context_t *cnt)
{
  bl_token_t *tok_enum = bl_tokens_peek(cnt->tokens);
  node_t *    type     = parse_type_enum(cnt, 0);
  if (!type) return NULL;
  node_type_enum_t *_type = peek_type_enum(type);

  node_t *enm = bl_ast_lit_enum(cnt->ast, tok_enum, type, NULL, cnt->curr_compound,
                                bl_scope_new(cnt->assembly->scope_cache, 256));

  node_lit_enum_t *_enm = peek_lit_enum(enm);

  node_t *prev_compound = cnt->curr_compound;
  cnt->curr_compound    = enm;

  bl_token_t *tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBLOCK);
  if (!tok) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD, "expected enm variant list");
    return bl_ast_bad(cnt->ast, tok);
  }

  /* parse enum varinats */
  bool     rq           = false;
  node_t **variant      = &_enm->variants;
  node_t * prev_variant = NULL;

next:
  *variant = parse_decl(cnt);
  if (*variant) {
    node_decl_t *_variant = peek_decl(*variant);
    _variant->kind        = DECL_KIND_VARIANT;

    if (_variant->type) {
      parse_warning_node(
          cnt, _variant->type, BL_BUILDER_CUR_WORD,
          "explicitly written type of enum varaint declaration will be overriden by enum "
          "base type");
    }

    _variant->type = _type->base_type;
    if (!_variant->value) {
      _variant->mutable = false;

      /* implicitly infer value from previous enum varaint if there is one */
      if (prev_variant) {
        node_decl_t *_prev_variant = peek_decl(prev_variant);

        bl_token_value_u value;
        value.u          = 1;
        node_t *addition = bl_ast_lit(cnt->ast, NULL, _variant->type, value);

        _variant->value = bl_ast_expr_binop(cnt->ast, NULL, _prev_variant->value, addition,
                                            _variant->type, BL_SYM_PLUS);
      } else {
        /* first variant is allways 0 */
        bl_token_value_u value;
        value.u         = 0;
        _variant->value = bl_ast_lit(cnt->ast, NULL, _variant->type, value);
      }
    }

    prev_variant = *variant;
    variant      = &(*variant)->next;

    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    if (bl_tokens_peek_2nd(cnt->tokens)->sym == BL_SYM_RBLOCK) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                  "expected variant after comma ','");
      return bl_ast_bad(cnt->ast, tok);
    }
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RBLOCK) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected end of variant list  '}'  or another variant separated by comma");
    return bl_ast_bad(cnt->ast, tok);
  }

  cnt->curr_compound = prev_compound;
  return enm;
}

node_t *
parse_expr(context_t *cnt)
{
  return _parse_expr(cnt, parse_unary_expr(cnt, NULL), 0);
}

node_t *
parse_unary_expr(context_t *cnt, bl_token_t *op)
{
  bl_token_t *curr_op = bl_tokens_peek(cnt->tokens);
  if (bl_token_is_unary(curr_op)) {
    bl_tokens_consume(cnt->tokens);
    node_t *next = _parse_expr(cnt, parse_atom_expr(cnt, NULL), bl_token_prec(curr_op, true));

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

node_t *
parse_expr_nested(context_t *cnt)
{
  node_t *    expr      = NULL;
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LPAREN);
  if (!tok_begin) return NULL;

  expr = parse_expr(cnt);
  if (expr == NULL) {
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_begin, BL_BUILDER_CUR_WORD, "expected expression.");
  }

  /* eat ) */
  bl_token_t *tok_end = bl_tokens_consume(cnt->tokens);
  if (tok_end->sym != BL_SYM_RPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_end, BL_BUILDER_CUR_WORD,
                "unterminated sub-expression, missing " BL_YELLOW("')'") ", started %d:%d",
                tok_begin->src.line, tok_begin->src.col);
  }

  return expr;
}

node_t *
parse_expr_member(context_t *cnt, bl_token_t *op)
{
  if (!op) return NULL;
  bool is_ptr_ref = bl_token_is(op, BL_SYM_ARROW);

  if (bl_token_is_not(op, BL_SYM_DOT) && !is_ptr_ref) return NULL;

  node_t *ident = parse_ident(cnt, 0);
  if (!ident) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, op, BL_BUILDER_CUR_WORD,
                "expected structure member name");
  }

  return bl_ast_expr_member(cnt->ast, op, MEM_KIND_UNKNOWN, ident, NULL, NULL, is_ptr_ref);
}

node_t *
parse_expr_elem(context_t *cnt, bl_token_t *op)
{
  if (!op) return NULL;
  if (bl_token_is_not(op, BL_SYM_LBRACKET)) return NULL;

  node_t *index = parse_expr(cnt);
  if (index == NULL) {
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, op, BL_BUILDER_CUR_WORD,
                "expected array index expression");
  }

  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RBRACKET) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "missing bracket " BL_YELLOW("']'"));
  }

  return bl_ast_expr_elem(cnt->ast, op, NULL, NULL, index);
}

node_t *
parse_atom_expr(context_t *cnt, bl_token_t *op)
{
  node_t *expr = NULL;
  if ((expr = parse_expr_nested(cnt))) return expr;
  if ((expr = parse_expr_null(cnt))) return expr;
  if ((expr = parse_expr_sizeof(cnt))) return expr;
  if ((expr = parse_expr_cast(cnt))) return expr;
  if ((expr = parse_run(cnt))) return expr;
  if ((expr = parse_literal_fn(cnt))) return expr;
  if ((expr = parse_literal_struct(cnt))) return expr;
  if ((expr = parse_literal_enum(cnt))) return expr;
  if ((expr = parse_expr_call(cnt))) return expr;
  if ((expr = parse_expr_elem(cnt, op))) return expr;
  if ((expr = parse_literal(cnt))) return expr;
  if ((expr = parse_expr_member(cnt, op))) return expr;
  if ((expr = parse_ident(cnt, 0))) return expr;
  return expr;
}

node_t *
_parse_expr(context_t *cnt, node_t *lhs, int min_precedence)
{
  node_t *    rhs       = NULL;
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
    if (op->sym == BL_SYM_LBRACKET) {
      peek_expr_elem(rhs)->next = lhs;
      lhs                       = rhs;
    } else if (op->sym == BL_SYM_DOT || op->sym == BL_SYM_ARROW) {
      if (node_is(rhs, NODE_EXPR_CALL)) {
        /* rhs is call 'foo.pointer_to_some_fn()' */
        /* in this case we create new member access expression node and use it instead of call
         * expression, finally we put this new node into call reference */
        node_t *          call  = rhs;
        node_expr_call_t *_call = peek_expr_call(call);
        node_t *member = bl_ast_expr_member(cnt->ast, op, MEM_KIND_STRUCT, _call->ref, NULL,
                                            _call->type, bl_token_is(op, BL_SYM_ARROW));

        _call->ref                     = member;
        peek_expr_member(member)->next = lhs;
        lhs                            = call;
      } else {
        peek_expr_member(rhs)->next = lhs;
        lhs                         = rhs;
      }
    } else if (bl_token_is_binop(op)) {
      node_t *result_type = NULL;
      node_t *tmp         = lhs;

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

node_t *
parse_ident(context_t *cnt, int ptr)
{
  bl_token_t *tok_ident = bl_tokens_consume_if(cnt->tokens, BL_SYM_IDENT);
  if (!tok_ident) return NULL;

  assert(cnt->curr_compound);
  return bl_ast_ident(cnt->ast, tok_ident, NULL, cnt->curr_compound, ptr, NULL);
}

node_t *
parse_value(context_t *cnt)
{
  node_t *value = NULL;
  if ((value = parse_literal_struct(cnt))) return value;
  if ((value = parse_literal_fn(cnt))) return value;
  if ((value = parse_expr(cnt))) return value;
  return value;
}

node_t *
parse_arr(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBRACKET);
  if (!tok_begin) return NULL;

  node_t *expr = parse_expr(cnt);
  if (!expr) {
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_begin, BL_BUILDER_CUR_AFTER,
                "expected array size expression");
    return bl_ast_bad(cnt->ast, tok_begin);
  }

  bl_token_t *tok_end = bl_tokens_consume_if(cnt->tokens, BL_SYM_RBRACKET);
  if (!tok_begin) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_end, BL_BUILDER_CUR_WORD,
                "expected ']' after array size expression");
    return bl_ast_bad(cnt->ast, tok_begin);
  }

  return expr;
}

node_t *
parse_type(context_t *cnt)
{
  node_t *type = NULL;
  int     ptr  = 0;

  while (bl_tokens_consume_if(cnt->tokens, BL_SYM_ASTERISK)) {
    ++ptr;
  }

  type = parse_type_fn(cnt, false, ptr);
  if (!type) type = parse_type_struct(cnt, false, ptr);
  if (!type) type = parse_type_enum(cnt, ptr);
  if (!type) type = parse_type_fund(cnt, ptr);

  node_t *arr = parse_arr(cnt);
  if (arr) {
    bl_ast_type_set_arr(type, arr);
  }

  return type;
}

node_t *
parse_type_fund(context_t *cnt, int ptr)
{
  node_t *type_ident = parse_ident(cnt, ptr);
  if (!type_ident) return NULL;
  assert(ptr >= 0);

  return type_ident;
}

node_t *
parse_type_fn(context_t *cnt, bool named_args, int ptr)
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
  node_t * arg_types;
  bool     rq         = false;
  node_t **arg_type   = &arg_types;
  int      argc_types = 0;

next:
  *arg_type = named_args ? parse_decl(cnt) : parse_type(cnt);
  if (*arg_type) {
    /* validate argument */
    if (node_is(*arg_type, NODE_DECL)) {
      node_decl_t *_arg_decl = peek_decl(*arg_type);
      _arg_decl->kind        = DECL_KIND_ARG;
    }
    arg_type = &(*arg_type)->next;
    ++argc_types;

    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    if (bl_tokens_peek_2nd(cnt->tokens)->sym == BL_SYM_RBLOCK) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                  "expected type after comma ','");
      return bl_ast_bad(cnt->ast, tok_fn);
    }
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected end of argument type list  ')'  or another type separated by comma");
    return bl_ast_bad(cnt->ast, tok_fn);
  }

  node_t *ret_type = parse_type(cnt);
  if (!ret_type) {
    ret_type = &bl_ftypes[BL_FTYPE_VOID];
  }

  return bl_ast_type_fn(cnt->ast, tok_fn, arg_types, argc_types, ret_type, ptr);
}

node_t *
parse_type_struct(context_t *cnt, bool named_args, int ptr)
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
  node_t * types;
  bool     rq     = false;
  node_t **type   = &types;
  int      typesc = 0;

next:
  *type = named_args ? parse_decl(cnt) : parse_type(cnt);
  if (*type) {
    /* validate argument */
    if (node_is(*type, NODE_DECL)) {
      node_decl_t *_member_decl = peek_decl(*type);
      _member_decl->order       = typesc;
      _member_decl->used        = 1;
      _member_decl->kind        = DECL_KIND_MEMBER;
    }
    type = &(*type)->next;
    ++typesc;

    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    if (bl_tokens_peek_2nd(cnt->tokens)->sym == BL_SYM_RBLOCK) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                  "expected member after comma ','");
      return bl_ast_bad(cnt->ast, tok_struct);
    }
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RBLOCK) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected end of member list  '}'  or another memeber separated by comma");
    bl_tokens_consume_till(cnt->tokens, BL_SYM_SEMICOLON);
    return bl_ast_bad(cnt->ast, tok_struct);
  }

  return bl_ast_type_struct(cnt->ast, tok_struct, types, typesc, NULL, ptr);
}

node_t *
parse_type_enum(context_t *cnt, int ptr)
{
  bl_token_t *tok_enum = bl_tokens_consume_if(cnt->tokens, BL_SYM_ENUM);
  if (!tok_enum) return NULL;

  node_t *type = parse_type(cnt);
  /* implicit type s32 when enum base type has not been specified */
  if (!type) type = bl_ast_type_fund(cnt->ast, NULL, BL_FTYPE_S32, 0, NULL);

  return bl_ast_type_enum(cnt->ast, tok_enum, type, NULL, ptr);
}

node_t *
parse_decl(context_t *cnt)
{
#define RETURN_BAD                                                                                 \
  {                                                                                                \
    cnt->curr_decl = prev_decl;                                                                    \
    return bl_ast_bad(cnt->ast, tok_ident);                                                        \
  }

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
  case BL_SYM_COMMA:
  case BL_SYM_RBLOCK: break;
  default: return NULL;
  }

  node_t *ident = parse_ident(cnt, 0);
  if (!ident) return NULL;

  if (bl_ast_is_buildin_type(ident) != -1) {
    parse_error_node(cnt, BL_ERR_INVALID_NAME, ident, BL_BUILDER_CUR_WORD,
                     "'%s' is reserved name of buildin type", tok_ident->value.str);
  }

  node_t *prev_decl = cnt->curr_decl;
  node_t *decl =
      bl_ast_decl(cnt->ast, tok_ident, DECL_KIND_UNKNOWN, ident, NULL, NULL, true, 0, 0, false);
  cnt->curr_decl     = decl;
  node_decl_t *_decl = peek_decl(decl);

  {
    int buildin = bl_ast_is_buildin(ident);
    if (buildin == BL_BUILDIN_MAIN) {
      /* main function */
      _decl->flags |= FLAG_MAIN;
    }
  }

  _decl->type            = parse_type(cnt);
  bl_token_t *tok_assign = bl_tokens_consume_if(cnt->tokens, BL_SYM_MDECL);
  if (!tok_assign) tok_assign = bl_tokens_consume_if(cnt->tokens, BL_SYM_IMMDECL);

  if (tok_assign) {
    _decl->mutable = bl_token_is(tok_assign, BL_SYM_MDECL);
    _decl->flags |= parse_flags(cnt, FLAG_EXTERN);

    if (!(_decl->flags & FLAG_EXTERN)) {
      _decl->value = parse_value(cnt);

      if (!_decl->value) {
        parse_error(cnt, BL_ERR_EXPECTED_INITIALIZATION, tok_assign, BL_BUILDER_CUR_AFTER,
                    "expected binding of declaration to some value");
        RETURN_BAD;
      }
    }
  }

  if (_decl->flags & FLAG_MAIN) {
    /* main function */
    if (_decl->mutable) {
      if (tok_assign) {
        parse_error(cnt, BL_ERR_INVALID_MUTABILITY, tok_assign, BL_BUILDER_CUR_WORD,
                    "'main' is expected to be immutable function");
      } else {
        parse_error_node(cnt, BL_ERR_INVALID_MUTABILITY, ident, BL_BUILDER_CUR_WORD,
                         "'main' is expected to be immutable function");
      }

      RETURN_BAD;
    }

    if (_decl->flags & FLAG_EXTERN) {
      parse_error_node(cnt, BL_ERR_UNEXPECTED_MODIF, ident, BL_BUILDER_CUR_WORD,
                       "main function cannot be extern");
      RETURN_BAD;
    }
  }

  if (_decl->flags & FLAG_EXTERN) {
    if (_decl->mutable) {
      parse_error(cnt, BL_ERR_INVALID_MUTABILITY, tok_assign, BL_BUILDER_CUR_WORD,
                  "extern declaration cannot be mutable");
      RETURN_BAD;
    }

    if (!_decl->type) {
      parse_error_node(cnt, BL_ERR_EXPECTED_TYPE, ident, BL_BUILDER_CUR_AFTER,
                       "extern declaration must have type specified");
      RETURN_BAD;
    }
  }

  if (_decl->flags & FLAG_EXTERN) {
    _decl->kind = DECL_KIND_FN;
  } else if (_decl->value) {
    switch (node_code(_decl->value)) {
    case NODE_LIT_FN: _decl->kind = DECL_KIND_FN; break;
    case NODE_LIT_ENUM: _decl->kind = DECL_KIND_ENUM; break;
    case NODE_LIT_STRUCT: _decl->kind = DECL_KIND_STRUCT; break;
    default: _decl->kind = _decl->mutable ? DECL_KIND_FIELD : DECL_KIND_CONSTANT; break;
    }
  } else {
    _decl->kind = _decl->mutable ? DECL_KIND_FIELD : DECL_KIND_CONSTANT;
  }

  cnt->curr_decl = prev_decl;
  return decl;

#undef RETURN_BAD
}

node_t *
parse_expr_call(context_t *cnt)
{
  if (!bl_tokens_is_seq(cnt->tokens, 2, BL_SYM_IDENT, BL_SYM_LPAREN)) return NULL;

  bl_token_t *tok_id = bl_tokens_peek(cnt->tokens);
  node_t *    ident  = parse_ident(cnt, 0);
  bl_token_t *tok    = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_LPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected function parameter list");
    return bl_ast_bad(cnt->ast, tok_id);
  }

  /* parse args */
  bool     rq = false;
  node_t * args;
  node_t **arg   = &args;
  int      argsc = 0;
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
    if (bl_tokens_peek_2nd(cnt->tokens)->sym == BL_SYM_RBLOCK) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                  "expected function argument after comma ','");
      return bl_ast_bad(cnt->ast, tok_id);
    }
  }

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_RPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                "expected end of parameter list ')' or another parameter separated by comma");
    return bl_ast_bad(cnt->ast, tok_id);
  }

  return bl_ast_expr_call(cnt->ast, tok_id, ident, args, argsc, NULL, false);
}

node_t *
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
    if (!(allowed & FLAG_EXTERN)) {
      parse_error(cnt, BL_ERR_UNEXPECTED_MODIF, tok, BL_BUILDER_CUR_WORD, "unexpected flag");
    } else {
      flags |= FLAG_EXTERN;
    }
    goto next;
  default: break;
  }

  return flags;
}

node_t *
parse_load(context_t *cnt)
{
  bl_token_t *tok_id = bl_tokens_consume_if(cnt->tokens, BL_SYM_LOAD);
  if (!tok_id) return NULL;

  bl_token_t *tok_path = bl_tokens_consume(cnt->tokens);
  if (!bl_token_is(tok_path, BL_SYM_STRING)) {
    parse_error(cnt, BL_ERR_EXPECTED_STRING, tok_path, BL_BUILDER_CUR_WORD,
                "expected path string after load preprocessor directive");
  }

  const char *filepath = tok_path->value.str;

  bl_unit_t *unit = bl_unit_new_file(filepath);
  if (!bl_assembly_add_unit_unique(cnt->assembly, unit)) {
    bl_unit_delete(unit);
  }

  return bl_ast_load(cnt->ast, tok_id, filepath);
}

node_t *
parse_link(context_t *cnt)
{
  bl_token_t *tok_id = bl_tokens_consume_if(cnt->tokens, BL_SYM_LINK);
  if (!tok_id) return NULL;

  bl_token_t *tok_path = bl_tokens_consume(cnt->tokens);
  if (!bl_token_is(tok_path, BL_SYM_STRING)) {
    parse_error(cnt, BL_ERR_EXPECTED_STRING, tok_path, BL_BUILDER_CUR_WORD,
                "expected path string after link preprocessor directive");
  }

  const char *lib = tok_path->value.str;
  bl_assembly_add_link(cnt->assembly, lib);

  return bl_ast_link(cnt->ast, tok_id, lib);
}

node_t *
parse_run(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_RUN);
  if (!tok) return NULL;

  node_t *call = parse_expr(cnt);
  if (!call || node_is_not(call, NODE_EXPR_CALL)) {
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok, BL_BUILDER_CUR_AFTER,
                "expected call after '#run' directive");
    return bl_ast_bad(cnt->ast, tok);
  }

  peek_expr_call(call)->run = true;

  return call;
}

node_t *
parse_block(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBLOCK);
  if (!tok_begin) return NULL;

  node_t *      prev_compound = cnt->curr_compound;
  node_t *      block         = bl_ast_block(cnt->ast, tok_begin, NULL, cnt->curr_compound,
                               bl_scope_new(cnt->assembly->scope_cache, 1024));
  node_block_t *_block        = peek_block(block);
  cnt->curr_compound          = block;

  bl_token_t *tok;
  node_t **   node = &_block->nodes;
next:
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
    tok = bl_tokens_consume(cnt->tokens);
    parse_warning(cnt, tok, BL_BUILDER_CUR_WORD, "extra semicolon can be removed ';'");
    goto next;
  }

  parse_flags(cnt, 0);

  if ((*node = parse_stmt_return(cnt))) {
    insert_node(&node);
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_stmt_if(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_while(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_loop(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_break(cnt))) {
    insert_node(&node);
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_stmt_continue(cnt))) {
    insert_node(&node);
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_decl(cnt))) {
    insert_node(&node);
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_expr(cnt))) {
    insert_node(&node);
    if (parse_semicolon_rq(cnt)) goto next;
  }

  if ((*node = parse_block(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_load(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_link(cnt))) {
    insert_node(&node);
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
parse_ublock_content(context_t *cnt, node_t *ublock)
{
  cnt->curr_compound     = ublock;
  node_ublock_t *_ublock = peek_ublock(ublock);
  node_t **      node    = &_ublock->nodes;
next:
  parse_flags(cnt, 0);

  if ((*node = parse_decl(cnt))) {
    insert_node(&node);
    parse_semicolon_rq(cnt);
    goto next;
  }

  if (!cnt->core_loaded && (*node = load_core(cnt))) {
    insert_node(&node);
    cnt->core_loaded = true;
    goto next;
  }

  if ((*node = parse_load(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_link(cnt))) {
    insert_node(&node);
    goto next;
  }

  bl_token_t *tok = bl_tokens_peek(cnt->tokens);
  if (!bl_token_is(tok, BL_SYM_RBLOCK) && !bl_token_is(tok, BL_SYM_EOF)) {
    parse_error(cnt, BL_ERR_UNEXPECTED_SYMBOL, tok, BL_BUILDER_CUR_WORD,
                "unexpected symbol in module body");
  }
}

node_t *
load_core(context_t *cnt)
{
  bl_unit_t *unit = bl_unit_new_file(BL_CORE_SOURCE_FILE);
  if (!bl_assembly_add_unit_unique(cnt->assembly, unit)) {
    bl_unit_delete(unit);
  }

  return bl_ast_load(cnt->ast, NULL, BL_CORE_SOURCE_FILE);
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
                   .curr_decl     = NULL,
                   .curr_compound = NULL,
                   .core_loaded   = false,
                   .inside_loop   = false};

  unit->ast.root = bl_ast_ublock(&unit->ast, NULL, unit, assembly->gscope);
  parse_ublock_content(&cnt, unit->ast.root);
}
