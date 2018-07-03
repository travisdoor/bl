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
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

#define parse_error_node(cnt, code, node, pos, format, ...)                                        \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), (node)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
    longjmp((cnt)->jmp_error, (code));                                                             \
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
  bl_builder_t *builder;
  bl_unit_t *   unit;
  bl_ast_t *    ast;
  bl_tokens_t * tokens;

  /* tmps */
  jmp_buf jmp_error;
  bool    inside_loop;

  bl_node_t *curr_init_list;
  bl_node_t *curr_func;
  bl_node_t *curr_module;
} context_t;

static bl_node_t *
parse_pre_load_maybe(context_t *cnt);

static bl_node_t *
parse_pre_link_maybe(context_t *cnt);

static bl_node_t *
parse_pre_line_maybe(context_t *cnt);

static bl_node_t *
parse_pre_file_maybe(context_t *cnt);

static bl_node_t *
parse_pre_test_maybe(context_t *cnt, int modif, bl_node_t *parent);

static bl_node_t *
parse_fn_maybe(context_t *cnt, int modif, bl_node_t *parent);

static bl_node_t *
parse_using_maybe(context_t *cnt);

static bl_node_t *
parse_init_expr_maybe(context_t *cnt, bl_node_t *path);

static bl_node_t *
parse_struct_maybe(context_t *cnt, int modif);

static bl_node_t *
parse_struct_member_maybe(context_t *cnt, int order);

static bl_node_t *
parse_enum_variant_maybe(context_t *cnt, bl_node_t *parent);

static bl_node_t *
parse_enum_maybe(context_t *cnt, int modif, bl_node_t *parent);

static bl_node_t *
parse_module_maybe(context_t *cnt, bl_node_t *parent, bool global, int modif);

static void
parse_module_body(context_t *cnt, bl_node_t *module);

static bl_node_t *
parse_block_maybe(context_t *cnt, bl_node_t *parent);

static bl_node_t *
parse_type_maybe(context_t *cnt, bl_node_t *path);

static bl_node_t *
parse_array_dim_maybe(context_t *cnt);

static bl_node_t *
parse_arg_maybe(context_t *cnt);

static void
parse_semicolon_rq(context_t *cnt);

static bl_node_t *
parse_ret_type_rq(context_t *cnt);

static bl_node_t *
parse_mut_maybe(context_t *cnt, int modif);

static bl_node_t *
parse_const_maybe(context_t *cnt, int modif);

static bl_node_t *
parse_expr_maybe(context_t *cnt, bool ignore_init_list);

static bl_node_t *
parse_expr_1(context_t *cnt, bl_node_t *lhs, int min_precedence, bool ignore_init_list);

static bl_node_t *
parse_atom_expr(context_t *cnt, bl_token_t *op, bool ignore_init_list);

static bl_node_t *
parse_literal_maybe(context_t *cnt);

static bl_node_t *
parse_unary_expr_maybe(context_t *cnt);

static bl_node_t *
parse_cast_expr_maybe(context_t *cnt);

static bl_node_t *
parse_decl_ref_maybe(context_t *cnt, bl_node_t *path);

static bl_node_t *
parse_member_ref_maybe(context_t *cnt, bl_token_t *op);

static bl_node_t *
parse_array_ref_maybe(context_t *cnt, bl_token_t *op);

static bl_node_t *
parse_call_maybe(context_t *cnt, bl_node_t *path, bool run_in_compile_time);

static bl_node_t *
parse_pre_run_maybe(context_t *cnt);

static bl_node_t *
parse_nested_expr_maybe(context_t *cnt);

static bl_node_t *
parse_path_maybe(context_t *cnt);

static bl_node_t *
parse_if_maybe(context_t *cnt, bl_node_t *parent);

static bl_node_t *
parse_loop_maybe(context_t *cnt, bl_node_t *parent);

static bl_node_t *
parse_block_content_maybe(context_t *cnt, bl_node_t *parent);

static bl_node_t *
parse_while_maybe(context_t *cnt, bl_node_t *parent);

static bl_node_t *
parse_break_maybe(context_t *cnt);

static bl_node_t *
parse_continue_maybe(context_t *cnt);

static bl_node_t *
parse_return_maybe(context_t *cnt);

static bl_node_t *
parse_sizeof_maybe(context_t *cnt);

static int
parse_modifs_maybe(context_t *cnt);

/* impl*/
int
parse_modifs_maybe(context_t *cnt)
{
  int res = BL_MODIF_NONE;
modif:
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_EXTERN)) {
    res |= BL_MODIF_EXTERN;
    goto modif;
  }

  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_PUBLIC)) {
    res |= BL_MODIF_PUBLIC;
    goto modif;
  }

  return res;
}

bl_node_t *
parse_return_maybe(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_RETURN);
  if (!tok_begin) {
    return NULL;
  }

  bl_node_t *expr = parse_expr_maybe(cnt, false);
  return bl_ast_add_stmt_return(cnt->ast, tok_begin, expr, cnt->curr_func);
}

bl_node_t *
parse_loop_maybe(context_t *cnt, bl_node_t *parent)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LOOP);
  if (!tok_begin) {
    return NULL;
  }

  const bool prev_inside_loop = cnt->inside_loop;
  cnt->inside_loop            = true;
  bl_node_t *test_type        = bl_ast_add_type_fund(cnt->ast, NULL, BL_FTYPE_BOOL, false);
  bl_node_t *test             = bl_ast_add_expr_literal_bool(cnt->ast, NULL, test_type, true);
  bl_node_t *loop             = bl_ast_add_stmt_loop(cnt->ast, tok_begin, test, NULL, parent);
  bl_node_t *true_stmt        = parse_block_content_maybe(cnt, loop);
  if (true_stmt == NULL || bl_node_is_not(true_stmt, BL_DECL_BLOCK)) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD, "expected loop body");
  }

  bl_peek_stmt_loop(loop)->true_stmt = true_stmt;
  cnt->inside_loop                   = prev_inside_loop;

  return loop;
}

bl_node_t *
parse_while_maybe(context_t *cnt, bl_node_t *parent)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_WHILE);
  if (!tok_begin) {
    return NULL;
  }

  const bool prev_inside_loop = cnt->inside_loop;
  cnt->inside_loop            = true;

  bl_node_t *test = parse_expr_maybe(cnt, true);
  if (test == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, err_tok, BL_BUILDER_CUR_WORD,
                "expected expression for the while statement");
  }

  bl_node_t *loop = bl_ast_add_stmt_loop(cnt->ast, tok_begin, test, NULL, parent);

  bl_node_t *true_stmt = parse_block_content_maybe(cnt, loop);
  if (true_stmt == NULL || bl_node_is_not(true_stmt, BL_DECL_BLOCK)) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD, "expected loop body");
  }

  bl_peek_stmt_loop(loop)->true_stmt = true_stmt;
  cnt->inside_loop                   = prev_inside_loop;

  return loop;
}

bl_node_t *
parse_break_maybe(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_BREAK);
  if (!tok_begin) {
    return NULL;
  }

  if (!cnt->inside_loop) {
    parse_error(cnt, BL_ERR_BREAK_OUTSIDE_LOOP, tok_begin, BL_BUILDER_CUR_WORD,
                BL_YELLOW("'break'") " statement outside loop");
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

  if (!cnt->inside_loop) {
    parse_error(cnt, BL_ERR_CONTINUE_OUTSIDE_LOOP, tok_begin, BL_BUILDER_CUR_WORD,
                BL_YELLOW("'continue'") " statement outside loop");
  }

  return bl_ast_add_stmt_continue(cnt->ast, tok_begin);
}

bl_node_t *
parse_decl_ref_maybe(context_t *cnt, bl_node_t *path)
{
  if (!path) return NULL;

  bl_node_t * decl_ref = NULL;
  bl_token_t *tok_id   = bl_tokens_peek_prev(cnt->tokens);

  decl_ref = bl_ast_add_expr_decl_ref(cnt->ast, tok_id, NULL, path);

  return decl_ref;
}

bl_node_t *
parse_member_ref_maybe(context_t *cnt, bl_token_t *op)
{
  if (!op) return NULL;

  bl_node_t *member_ref = NULL;
  bool       is_ptr_ref = bl_token_is(op, BL_SYM_ARROW);

  if (bl_token_is(op, BL_SYM_DOT) || is_ptr_ref) {
    bl_token_t *tok_id = bl_tokens_consume(cnt->tokens);
    if (tok_id->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_id, BL_BUILDER_CUR_WORD,
                  "expected structure member name");
    }

    /* next member will be set later */
    member_ref =
        bl_ast_add_expr_member_ref(cnt->ast, tok_id, tok_id->value.str, NULL, NULL, is_ptr_ref);
  }

  return member_ref;
}

bl_node_t *
parse_array_ref_maybe(context_t *cnt, bl_token_t *op)
{
  if (!op) return NULL;

  bl_node_t *array_ref = NULL;
  if (bl_token_is(op, BL_SYM_LBRACKET)) {
    bl_node_t *index = parse_expr_maybe(cnt, true);
    if (index == NULL) {
      parse_error(cnt, BL_ERR_EXPECTED_EXPR, op, BL_BUILDER_CUR_WORD,
                  "expected array indexing expression");
    }

    array_ref = bl_ast_add_expr_array_ref(cnt->ast, op, index, NULL);

    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RBRACKET) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                  "missing bracket " BL_YELLOW("']'"));
    }
  }
  return array_ref;
}

bl_node_t *
parse_pre_line_maybe(context_t *cnt)
{
  bl_token_t *tok_line = bl_tokens_consume_if(cnt->tokens, BL_SYM_LINE);
  if (!tok_line) return NULL;

  bl_node_t *type = bl_ast_add_type_fund(cnt->ast, tok_line, BL_FTYPE_I32, 0);
  return bl_ast_add_expr_literal_signed(cnt->ast, tok_line, type, tok_line->src.line);
}

bl_node_t *
parse_pre_file_maybe(context_t *cnt)
{
  bl_token_t *tok_file = bl_tokens_consume_if(cnt->tokens, BL_SYM_FILE);
  if (!tok_file) return NULL;

  bl_node_t *type = bl_ast_add_type_fund(cnt->ast, tok_file, BL_FTYPE_STRING, 0);
  return bl_ast_add_expr_literal_str(cnt->ast, tok_file, type, tok_file->src.unit->filepath);
}

bl_node_t *
parse_pre_run_maybe(context_t *cnt)
{
  bl_token_t *tok_run = bl_tokens_consume_if(cnt->tokens, BL_SYM_RUN);
  if (!tok_run) return NULL;

  bl_node_t *path = parse_path_maybe(cnt);
  bl_node_t *call = parse_call_maybe(cnt, path, true);

  if (!call) {
    bo_unref(path);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_run, BL_BUILDER_CUR_AFTER,
                "expected method call after #run directive");
  }

  return call;
}

bl_node_t *
parse_call_maybe(context_t *cnt, bl_node_t *path, bool run_in_compile_time)
{
  if (!path) return NULL;

  bl_node_t *call = NULL;
  if (path > 0 && bl_tokens_current_is(cnt->tokens, BL_SYM_LPAREN)) {
    bl_token_t *tok_id    = bl_tokens_peek_prev(cnt->tokens);
    call                  = bl_ast_add_expr_call(cnt->ast, tok_id, NULL, path, run_in_compile_time);
    bl_expr_call_t *_call = bl_peek_expr_call(call);

    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_LPAREN) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                  "expected function parameter list");
    }

    /* parse args */
    bool        rq   = false;
    bl_node_t * prev = NULL;
    bl_node_t **arg  = &_call->args;
  arg:
    *arg = parse_expr_maybe(cnt, true);
    if (*arg) {
      _call->argsc++;
      (*arg)->prev = prev;
      prev         = *arg;
      arg          = &(*arg)->next;

      if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
        rq = true;
        goto arg;
      }
    } else if (rq) {
      bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                  "expected function argument after comma " BL_YELLOW("','"));
    }

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                  "expected end of parameter list " BL_YELLOW(
                      "')'") " or another parameter separated by comma");
    }
  }

  return call;
}

bl_node_t *
parse_literal_maybe(context_t *cnt)
{
  bl_node_t * literal = NULL;
  bl_node_t * type    = NULL;
  bl_token_t *tok     = bl_tokens_peek(cnt->tokens);

  switch (tok->sym) {
  case BL_SYM_NUM:
    bl_tokens_consume(cnt->tokens);
    type    = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_I32, 0);
    literal = bl_ast_add_expr_literal_unsigned(cnt->ast, tok, type, tok->value.u);
    break;

  case BL_SYM_STRING:
    bl_tokens_consume(cnt->tokens);
    type    = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_STRING, 0);
    literal = bl_ast_add_expr_literal_str(cnt->ast, tok, type, tok->value.str);
    break;

  case BL_SYM_FLOAT:
    bl_tokens_consume(cnt->tokens);
    type    = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_F32, 0);
    literal = bl_ast_add_expr_literal_double(cnt->ast, tok, type, tok->value.d);
    break;

  case BL_SYM_DOUBLE:
    bl_tokens_consume(cnt->tokens);
    type    = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_F64, 0);
    literal = bl_ast_add_expr_literal_double(cnt->ast, tok, type, tok->value.d);
    break;

  case BL_SYM_CHAR:
    bl_tokens_consume(cnt->tokens);
    type    = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_CHAR, 0);
    literal = bl_ast_add_expr_literal_char(cnt->ast, tok, type, tok->value.c);
    break;

  case BL_SYM_NULL:
    bl_tokens_consume(cnt->tokens);
    /* null pointer expression, type is added later during reference connection becouse null must be
     * implicitly casted */
    literal = bl_ast_add_expr_null(cnt->ast, tok, NULL);
    break;

  case BL_SYM_TRUE:
  case BL_SYM_FALSE: {
    bl_tokens_consume(cnt->tokens);
    bool val = tok->sym == BL_SYM_TRUE;
    type     = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_BOOL, 0);
    literal  = bl_ast_add_expr_literal_bool(cnt->ast, tok, type, val);

    break;
  }
  default:
    break;
  }

  return literal;
}

bl_node_t *
parse_cast_expr_maybe(context_t *cnt)
{
  bl_node_t *cast = NULL;

  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_CAST);
  if (tok_begin) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    if (!bl_token_is(tok, BL_SYM_LPAREN)) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_begin, BL_BUILDER_CUR_WORD,
                  "expected " BL_YELLOW("'('") " after cast expression");
    }

    bl_node_t *to_type = parse_type_maybe(cnt, NULL);
    if (to_type == NULL) {
      tok = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok, BL_BUILDER_CUR_WORD,
                  "expected type name as cast parameter");
    }

    tok = bl_tokens_consume(cnt->tokens);
    if (!bl_token_is(tok, BL_SYM_RPAREN)) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_begin, BL_BUILDER_CUR_WORD,
                  "expected " BL_YELLOW("')'") " after cast expression");
    }

    bl_node_t *next =
        parse_expr_1(cnt, parse_atom_expr(cnt, NULL, false), bl_token_prec(tok_begin), false);
    if (next == NULL) {
      tok = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok, BL_BUILDER_CUR_WORD,
                  "expected expression after cast");
    }

    cast = bl_ast_add_expr_cast(cnt->ast, tok_begin, to_type, next);
  }

  return cast;
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
    expr = parse_expr_maybe(cnt, false);
    if (expr == NULL) {
      parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_begin, BL_BUILDER_CUR_WORD,
                  "expected expression.");
    }

    /* eat ) */
    bl_token_t *tok_end = bl_tokens_consume(cnt->tokens);
    if (tok_end->sym != BL_SYM_RPAREN) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_end, BL_BUILDER_CUR_WORD,
                  "unterminated sub-expression, missing " BL_YELLOW("')'") ", started %d:%d",
                  tok_begin->src.line, tok_begin->src.col);
    }
  }

  return expr;
}

bl_node_t *
parse_path_maybe(context_t *cnt)
{
  bl_node_t * path          = NULL;
  bl_token_t *separator_tok = NULL;
  bl_token_t *tok           = NULL;
  bool        rq            = false;

  bl_node_t * prev      = NULL;
  bl_node_t **path_elem = &path;

next:
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_IDENT)) {
    tok = bl_tokens_consume(cnt->tokens);

    *path_elem         = bl_ast_add_path_elem(cnt->ast, tok, tok->value.str);
    (*path_elem)->prev = prev;
    prev               = *path_elem;
    path_elem          = &(*path_elem)->next;
    rq                 = false;

    separator_tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_PATH);
    if (separator_tok != NULL) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, separator_tok, BL_BUILDER_CUR_WORD,
                "expected enum or module name after path separator");
  }

  return path;
}

bl_node_t *
parse_unary_expr_maybe(context_t *cnt)
{
  bl_node_t * unary  = NULL;
  bl_token_t *tok_op = bl_tokens_peek(cnt->tokens);

  if (bl_token_is_unary(tok_op)) {
    bl_tokens_consume(cnt->tokens);
    // bl_node_t *next = parse_expr_maybe(cnt);
    bl_node_t *next =
        parse_expr_1(cnt, parse_atom_expr(cnt, NULL, false), bl_token_prec(tok_op), false);

    if (next == NULL) {
      bl_token_t *err_tok = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_EXPR, err_tok, BL_BUILDER_CUR_WORD,
                  "expected expression after unary operator");
    }

    unary = bl_ast_add_expr_unary(cnt->ast, tok_op, tok_op->sym, next);
  }

  return unary;
}

bl_node_t *
parse_atom_expr(context_t *cnt, bl_token_t *op, bool ignore_init_list)
{
  bl_node_t *expr = NULL;
  bl_node_t *path = NULL;

  if ((expr = parse_array_ref_maybe(cnt, op))) return expr;

  if ((expr = parse_member_ref_maybe(cnt, op))) return expr;

  if ((expr = parse_unary_expr_maybe(cnt))) return expr;

  if ((expr = parse_nested_expr_maybe(cnt))) return expr;

  if ((expr = parse_cast_expr_maybe(cnt))) return expr;

  if ((expr = parse_sizeof_maybe(cnt))) return expr;

  if ((expr = parse_pre_run_maybe(cnt))) return expr;

  if ((expr = parse_literal_maybe(cnt))) return expr;

  if ((expr = parse_pre_line_maybe(cnt))) return expr;

  if ((expr = parse_pre_file_maybe(cnt))) return expr;

  path = parse_path_maybe(cnt);

  if (!ignore_init_list && (expr = parse_init_expr_maybe(cnt, path))) return expr;

  if ((expr = parse_call_maybe(cnt, path, false))) return expr;

  if ((expr = parse_decl_ref_maybe(cnt, path))) return expr;

  return expr;
}

bl_node_t *
parse_expr_1(context_t *cnt, bl_node_t *lhs, int min_precedence, bool ignore_init_list)
{
  bl_node_t * rhs       = NULL;
  bl_token_t *lookahead = bl_tokens_peek(cnt->tokens);
  bl_token_t *op        = NULL;

  while (bl_token_prec(lookahead) >= min_precedence) {
    op = lookahead;
    bl_tokens_consume(cnt->tokens);
    rhs       = parse_atom_expr(cnt, op, ignore_init_list);
    lookahead = bl_tokens_peek(cnt->tokens);

    while (bl_token_prec(lookahead) > bl_token_prec(op)) {
      rhs       = parse_expr_1(cnt, rhs, bl_token_prec(lookahead), ignore_init_list);
      lookahead = bl_tokens_peek(cnt->tokens);
    }

    if (op->sym == BL_SYM_LBRACKET) {
      bl_peek_expr_array_ref(rhs)->next = lhs;
      lhs                               = rhs;
    } else if (op->sym == BL_SYM_DOT || op->sym == BL_SYM_ARROW) {
      if (!lhs && cnt->curr_init_list) {
        /* we are in initialialization list */
        lhs = bl_ast_add_expr_decl_ref(cnt->ast, op, cnt->curr_init_list, NULL);
      }

      // bl_assert(lhs, "invalid next node in member reference");
      bl_peek_expr_member_ref(rhs)->next = lhs;
      lhs                                = rhs;
    } else if (bl_token_is_binop(op)) {
      bl_node_t *result_type = NULL;
      bl_node_t *tmp         = lhs;

      /* Set result type to bool for logical binary operations, this is used for type checking later
       * in the compiler pipeline. Other types are checked recursively. */
      if (bl_token_is_logic_op(op)) {
        result_type = bl_ast_add_type_fund(cnt->ast, op, BL_FTYPE_BOOL, false);
      }

      lhs = bl_ast_add_expr_binop(cnt->ast, op, op->sym, tmp, rhs, result_type);
    } else {
      parse_error(cnt, BL_ERR_EXPECTED_BINOP, op, BL_BUILDER_CUR_WORD, "expected binary operation");
    }
  }

  return lhs;
}

bl_node_t *
parse_const_maybe(context_t *cnt, int modif)
{
  bl_node_t * type      = NULL;
  bl_node_t * init_expr = NULL;
  bl_token_t *tok_id    = NULL;

  /* TODO: check for invalid modificators */

  if (bl_tokens_current_is_not(cnt->tokens, BL_SYM_CONST)) {
    return NULL;
  }

  /* consume keyword and determinate constant variant of declaration */
  bl_tokens_consume(cnt->tokens);
  tok_id = bl_tokens_consume(cnt->tokens);
  if (!bl_token_is(tok_id, BL_SYM_IDENT)) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_id, BL_BUILDER_CUR_WORD, "expected constant name");
  }

  type = parse_type_maybe(cnt, NULL);
  if (type == NULL) {
    bl_token_t *tok_err = bl_tokens_peek_prev(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok_err, BL_BUILDER_CUR_AFTER,
                "expected type name after constant name");
  }

  bl_token_t *tok_expr = bl_tokens_consume(cnt->tokens);
  if (tok_expr == NULL) {
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_expr, BL_BUILDER_CUR_WORD,
                "expected initialization expression after constant declaration");
  }

  init_expr = parse_expr_maybe(cnt, false);
  if (init_expr == NULL) {
    bl_token_t *tok_err = bl_tokens_peek_prev(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_err, BL_BUILDER_CUR_AFTER,
                "expected initialization expression after constant declaration");
  }

  return bl_ast_add_decl_const(cnt->ast, tok_id, tok_id->value.str, type, init_expr, modif);
}

bl_node_t *
parse_expr_maybe(context_t *cnt, bool ignore_init_list)
{
  return parse_expr_1(cnt, parse_atom_expr(cnt, NULL, ignore_init_list), 0, ignore_init_list);
}

bl_node_t *
parse_mut_maybe(context_t *cnt, int modif)
{
  bl_node_t * type      = NULL;
  bl_node_t * init_expr = NULL;
  bl_token_t *tok_id    = NULL;

  /* TODO: check for invalid modificators */

  /* Constant variable can be declared without 'mut' key word at the begining, we use 'const'
   * keyword instead. This can lead to confusion later becouse 'const' is used as modifier stored in
   * modif buffer of declaration, but for now anything else than mut cannot be declared as constant.
   * Fix this later? */
  if (bl_tokens_current_is_not(cnt->tokens, BL_SYM_MUT)) {
    return NULL;
  }

  /* consume keyword and determinate constant variant of declaration */
  tok_id = bl_tokens_consume(cnt->tokens);
  tok_id = bl_tokens_consume(cnt->tokens);

  if (!bl_token_is(tok_id, BL_SYM_IDENT)) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_id, BL_BUILDER_CUR_WORD, "expected mutable name");
  }

  type = parse_type_maybe(cnt, NULL);
  /*if (type == NULL) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok_err, BL_BUILDER_CUR_WORD,
                "expected type name after mutable name");
                }*/

  /*
   * parse init expr when mutable declaration is fallowd by assign symbol
   * note: constant must have initialization
   */
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_ASSIGN)) {
    init_expr = parse_expr_maybe(cnt, false);
    if (init_expr == NULL) {
      bl_token_t *tok_err = bl_tokens_peek_prev(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_err, BL_BUILDER_CUR_AFTER,
                  "expected expression after mutable assignment");
    }

    if (init_expr == NULL) {
      bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_err, BL_BUILDER_CUR_WORD,
                  "expected initialization expression after constant declaration");
    }
  }

  return bl_ast_add_decl_mut(cnt->ast, tok_id, tok_id->value.str, type, init_expr, modif, false);
}

void
parse_semicolon_rq(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_SEMICOLON);
  if (!tok) {
    tok = bl_tokens_peek_prev(cnt->tokens);
    parse_error(cnt, BL_ERR_MISSING_SEMICOLON, tok, BL_BUILDER_CUR_AFTER,
                "missing semicolon " BL_YELLOW("';'"));
  }
}

bl_node_t *
parse_type_maybe(context_t *cnt, bl_node_t *path)
{
  bl_node_t *type   = NULL;
  int        is_ptr = 0;

  while (bl_tokens_consume_if(cnt->tokens, BL_SYM_ASTERISK)) {
    ++is_ptr;
  }

  if (!path) path = parse_path_maybe(cnt);

  bl_node_t * last_path_elem = NULL;
  bl_token_t *prev_tok       = NULL;

  if (path) {
    last_path_elem = bl_ast_path_get_last(path);
    prev_tok       = bl_tokens_peek_prev(cnt->tokens);

    bl_assert(last_path_elem, "invalid last path elem in type parsing");

    int found = -1;
    for (int i = 0; i < BL_FUND_TYPE_COUNT; ++i) {
      if (strcmp(bl_fund_type_strings[i], bl_peek_path_elem(last_path_elem)->id.str) == 0) {
        found = i;
        break;
      }
    }

    bl_node_t *expr_dim = parse_array_dim_maybe(cnt);

    if (found > -1) {
      type = bl_ast_add_type_fund(cnt->ast, prev_tok, (bl_fund_type_e)found, is_ptr);
      bl_peek_type_fund(type)->dim = expr_dim;
    } else {
      type = bl_ast_add_type_ref(cnt->ast, prev_tok, bl_peek_path_elem(last_path_elem)->id.str,
                                 NULL, path, is_ptr);
      bl_peek_type_ref(type)->dim = expr_dim;
    }
  }
  return type;
}

bl_node_t *
parse_array_dim_maybe(context_t *cnt)
{
  bl_token_t *tok_arr_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBRACKET);
  if (tok_arr_begin != NULL) {
    /* TODO: elem count expression must be evaluated */
    bl_node_t *dim = parse_expr_maybe(cnt, true);
    if (dim == NULL) {
      parse_error(cnt, BL_ERR_INVALID_EXPR, tok_arr_begin, BL_BUILDER_CUR_WORD,
                  "expected array element count after " BL_YELLOW("'['"));
    }

    bl_token_t *tok_arr_end = bl_tokens_consume_if(cnt->tokens, BL_SYM_RBRACKET);
    if (tok_arr_end == NULL) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_arr_end, BL_BUILDER_CUR_WORD,
                  "missing right bracked after array size definition " BL_YELLOW(
                      "']'") ", started here: %d:%d",
                  tok_arr_begin->src.line, tok_arr_begin->src.col);
    }

    return dim;
  }

  return NULL;
}

bl_node_t *
parse_ret_type_rq(context_t *cnt)
{
  bl_token_t *tok  = bl_tokens_peek(cnt->tokens);
  bl_node_t * type = NULL;

  switch (tok->sym) {
  case BL_SYM_ASTERISK:
  case BL_SYM_IDENT:
    type = parse_type_maybe(cnt, NULL);
    break;
  case BL_SYM_LBLOCK:
  case BL_SYM_SEMICOLON: {
    type = bl_ast_add_type_fund(cnt->ast, tok, BL_FTYPE_VOID, false);
    break;
  }
  default:
    break;
  }

  if (!type) {
    tok = bl_tokens_peek_prev(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok, BL_BUILDER_CUR_AFTER,
                "expected function return type or function body after argument list");
  }

  return type;
}

bl_node_t *
parse_sizeof_maybe(context_t *cnt)
{
  bl_node_t *szof = NULL;

  bl_token_t *tok_id = bl_tokens_peek(cnt->tokens);
  if (bl_token_is(tok_id, BL_SYM_SIZEOF)) {
    bl_tokens_consume(cnt->tokens);

    /* eat ( */
    if (!bl_tokens_consume_if(cnt->tokens, BL_SYM_LPAREN)) {
      bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
      parse_error(cnt, BL_ERR_MISSING_BRACKET, err_tok, BL_BUILDER_CUR_WORD,
                  "expected " BL_YELLOW("'('") " after sizeof buildin");
    }

    bl_node_t *type = parse_type_maybe(cnt, NULL);
    if (type == NULL) {
      bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok_err, BL_BUILDER_CUR_WORD,
                  "expected type name as parameter");
    }

    /* eat ) */
    if (!bl_tokens_consume_if(cnt->tokens, BL_SYM_RPAREN)) {
      bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
      parse_error(cnt, BL_ERR_MISSING_BRACKET, err_tok, BL_BUILDER_CUR_WORD,
                  "expected " BL_YELLOW("')'") " after sizeof buildin argument");
    }

    szof = bl_ast_add_expr_sizeof(cnt->ast, tok_id, type);
  }

  return szof;
}

bl_node_t *
parse_pre_test_maybe(context_t *cnt, int modif, bl_node_t *parent)
{
  bl_token_t *tok_test = bl_tokens_consume_if(cnt->tokens, BL_SYM_TEST);
  if (!tok_test) return NULL;

  modif |= BL_MODIF_UTEST;

  /* function declaration is expected after #test directive */
  bl_node_t *decl_func = parse_fn_maybe(cnt, modif, parent);
  if (!decl_func) {
    parse_error(cnt, BL_ERR_EXPECTED_FUNC, tok_test, BL_BUILDER_CUR_AFTER,
                "expected function declaration after #test directive");
  }
  return decl_func;
}

bl_node_t *
parse_pre_load_maybe(context_t *cnt)
{
  bl_node_t *pre_load = NULL;

  bl_token_t *tok_id = bl_tokens_peek(cnt->tokens);
  if (bl_token_is(tok_id, BL_SYM_LOAD)) {
    bl_tokens_consume(cnt->tokens);
    bl_token_t *tok_path = bl_tokens_consume(cnt->tokens);
    if (!bl_token_is(tok_path, BL_SYM_STRING)) {
      parse_error(cnt, BL_ERR_EXPECTED_STRING, tok_path, BL_BUILDER_CUR_WORD,
                  "expected path string after load preprocessor directive");
    }

    pre_load = bl_ast_add_pre_load(cnt->ast, tok_id, tok_path->value.str);
  }

  return pre_load;
}

bl_node_t *
parse_pre_link_maybe(context_t *cnt)
{
  bl_node_t *pre_link = NULL;

  bl_token_t *tok_id = bl_tokens_peek(cnt->tokens);
  if (bl_token_is(tok_id, BL_SYM_LINK)) {
    bl_tokens_consume(cnt->tokens);
    bl_token_t *tok_path = bl_tokens_consume(cnt->tokens);
    if (!bl_token_is(tok_path, BL_SYM_STRING)) {
      parse_error(cnt, BL_ERR_EXPECTED_STRING, tok_path, BL_BUILDER_CUR_WORD,
                  "expected path string after link preprocessor directive");
    }

    pre_link = bl_ast_add_pre_link(cnt->ast, tok_id, tok_path->value.str);
  }

  return pre_link;
}

bl_node_t *
parse_if_maybe(context_t *cnt, bl_node_t *parent)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_IF);
  if (tok_begin == NULL) {
    return NULL;
  }

  bl_node_t *test = parse_expr_maybe(cnt, true);
  if (test == NULL) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_EXPR, err_tok, BL_BUILDER_CUR_WORD,
                "expected expression for the if statement");
  }

  bl_node_t *if_stmt = bl_ast_add_stmt_if(cnt->ast, tok_begin, test, NULL, NULL, parent);

  bl_node_t *true_stmt = parse_block_content_maybe(cnt, if_stmt);
  if (true_stmt == NULL || bl_node_is_not(true_stmt, BL_DECL_BLOCK)) {
    bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD,
                "expected compound statement for true result of the if expression test");
  }

  bl_node_t *false_stmt = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_ELSE)) {
    false_stmt = parse_block_content_maybe(cnt, if_stmt);
    if (false_stmt == NULL ||
        (bl_node_is_not(false_stmt, BL_DECL_BLOCK) && bl_node_is_not(false_stmt, BL_STMT_IF))) {
      bl_token_t *err_tok = bl_tokens_consume(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_STMT, err_tok, BL_BUILDER_CUR_WORD,
                  "expected statement for false result of the if expression test");
    }
  }

  bl_peek_stmt_if(if_stmt)->true_stmt  = true_stmt;
  bl_peek_stmt_if(if_stmt)->false_stmt = false_stmt;

  return if_stmt;
}

bl_node_t *
parse_block_content_maybe(context_t *cnt, bl_node_t *parent)
{
  bl_node_t *stmt = NULL;

  int modif = parse_modifs_maybe(cnt);
  /*if (modif != BL_MODIF_NONE) {
    bl_token_t *err_tok = bl_tokens_peek_prev(cnt->tokens);
    parse_error(cnt, BL_ERR_UNEXPECTED_MODIF, err_tok, "unexpected modificator " BL_YELLOW("'%s'"),
                bl_sym_strings[err_tok->sym]);
                }*/

  if ((stmt = parse_block_maybe(cnt, parent))) {
    goto done;
  }

  if ((stmt = parse_mut_maybe(cnt, modif))) {
    parse_semicolon_rq(cnt);
    goto done;
  }

  if ((stmt = parse_const_maybe(cnt, modif))) {
    parse_semicolon_rq(cnt);
    goto done;
  }

  if ((stmt = parse_using_maybe(cnt))) {
    parse_semicolon_rq(cnt);
    goto done;
  }

  if ((stmt = parse_if_maybe(cnt, parent))) {
    goto done;
  }

  if ((stmt = parse_loop_maybe(cnt, parent))) {
    goto done;
  }

  if ((stmt = parse_while_maybe(cnt, parent))) {
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

  if ((stmt = parse_expr_maybe(cnt, false))) {
    parse_semicolon_rq(cnt);
    goto done;
  }

  if ((stmt = parse_fn_maybe(cnt, 0, parent))) {
    goto done;
  }

  if ((stmt = parse_struct_maybe(cnt, 0))) {
    goto done;
  }

  if ((stmt = parse_enum_maybe(cnt, 0, parent))) {
    goto done;
  }

done:
  return stmt;
}

bl_node_t *
parse_block_maybe(context_t *cnt, bl_node_t *parent)
{
  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBLOCK);

  if (tok_begin == NULL) {
    return NULL;
  }

  bl_node_t *      block  = bl_ast_add_decl_block(cnt->ast, tok_begin, parent);
  bl_decl_block_t *_block = bl_peek_decl_block(block);
  bl_token_t *     tok;
  bl_node_t *      prev = NULL;
  bl_node_t **     node = &_block->nodes;
stmt:
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
    tok = bl_tokens_consume(cnt->tokens);
    parse_warning(cnt, tok, BL_BUILDER_CUR_WORD,
                  "extra semicolon can be removed " BL_YELLOW("';'"));
    goto stmt;
  }

  /* stmts */
  *node = parse_block_content_maybe(cnt, block);
  if (*node) {
    (*node)->prev = prev;
    prev          = *node;
    node          = &(*node)->next;
    goto stmt;
  }

  tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_RBLOCK);
  if (!tok) {
    tok = bl_tokens_peek_prev(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_BODY_END, tok, BL_BUILDER_CUR_AFTER,
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
    bl_node_t * type = parse_type_maybe(cnt, NULL);

    if (type == NULL) {
      bl_token_t *tok = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD, "expected argument type");
    }

    arg = bl_ast_add_decl_arg(cnt->ast, tok, tok->value.str, type);
  }

  return arg;
}

bl_node_t *
parse_fn_maybe(context_t *cnt, int modif, bl_node_t *parent)
{
  bl_node_t *fn = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_FN) != NULL) {
    bl_token_t *tok_id = bl_tokens_consume(cnt->tokens);
    if (tok_id->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_id, BL_BUILDER_CUR_WORD, "expected function name");
    }

    fn = bl_ast_add_decl_func(cnt->ast, tok_id, tok_id->value.str, NULL, NULL, modif, parent,
                              modif & BL_MODIF_UTEST);
    bl_decl_func_t *_fn     = bl_peek_decl_func(fn);
    bl_node_t *     prev_fn = cnt->curr_func;
    cnt->curr_func          = fn;

    if (bl_ast_is_buildin(&bl_peek_decl_func(fn)->id, BL_BUILDIN_MAIN)) {
      bl_peek_decl_func(fn)->modif = BL_MODIF_EXPORT | BL_MODIF_ENTRY;
    }

    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_LPAREN) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                  "expected function parameter list");
    }

    /* parse args */
    bool        rq   = false;
    bl_node_t * prev = NULL;
    bl_node_t **arg  = &_fn->args;
  arg:
    *arg = parse_arg_maybe(cnt);
    if (*arg) {
      _fn->argsc++;
      (*arg)->prev = prev;
      prev         = *arg;
      arg          = &(*arg)->next;

      if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
        rq = true;
        goto arg;
      }
    } else if (rq) {
      bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_WORD,
                  "expected function argument after comma " BL_YELLOW("','"));
    }

    if (bl_peek_decl_func(fn)->argsc > BL_MAX_FUNC_ARG_COUNT) {
      parse_error_node(cnt, BL_ERR_INVALID_PARAM_COUNT, fn, BL_BUILDER_CUR_WORD,
                       "maximum argument count reached (%d) in declaration of " BL_YELLOW("'%s'"),
                       BL_MAX_FUNC_ARG_COUNT, _fn->id.str);
    }

    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_RPAREN) {
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, BL_BUILDER_CUR_WORD,
                  "expected end of parameter list " BL_YELLOW(
                      "')'") " or another parameter separated by comma");
    }

    /*
     * parse function return type definition, and use void if there is no type specified
     */
    _fn->ret_type    = parse_ret_type_rq(cnt);
    bl_node_t *block = parse_block_maybe(cnt, fn);

    if (modif & BL_MODIF_EXTERN) {
      if (block != NULL) {
        parse_error_node(cnt, BL_ERR_UNEXPECTED_DECL, fn, BL_BUILDER_CUR_WORD,
                         "extern function " BL_YELLOW("'%s'") " can't have body", _fn->id.str);
      }

      parse_semicolon_rq(cnt);
    } else if (block == NULL) {
      parse_error_node(cnt, BL_ERR_EXPECTED_BODY, fn, BL_BUILDER_CUR_WORD,
                       "function " BL_YELLOW("'%s'") " has no body", _fn->id.str);
    }

    _fn->block     = block;
    cnt->curr_func = prev_fn;
  }

  return fn;
}

bl_node_t *
parse_using_maybe(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_USING);
  if (tok == NULL) return NULL;

  bl_node_t *path = parse_path_maybe(cnt);
  if (path == NULL) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, BL_BUILDER_CUR_WORD,
                "expected module name or path to module after using keyword");
  }

  return bl_ast_add_stmt_using(cnt->ast, tok, path);
}

bl_node_t *
parse_init_expr_maybe(context_t *cnt, bl_node_t *path)
{
  /* Type is optional!!! */
  bl_node_t *type = NULL;

  bl_token_t *tok_begin = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBLOCK);
  if (!tok_begin) return NULL;

  /* Initialization list can have explicitly defined resulting type of initialization. */
  if (path) {
    type = parse_type_maybe(cnt, path);
    bl_assert(type, "invalid init expression type");
  }

  /* When type is not explicitly defined for init. list we need to fill them later during linking.
   */

  bl_node_t *     init  = bl_ast_add_expr_init(cnt->ast, tok_begin, type);
  bl_expr_init_t *_init = bl_peek_expr_init(init);
  bl_token_t *    tok   = NULL;

  bl_node_t *prev_init_list = cnt->curr_init_list;
  cnt->curr_init_list       = init;

  bl_node_t * prev = NULL;
  bl_node_t **expr = &_init->exprs;
  /* Loop until we reach the end of initialization list. (expressions are separated by comma) */
next_expr:
  /* eat ident */
  *expr = parse_expr_maybe(cnt, false);
  if (*expr) {
    (*expr)->prev = prev;
    prev          = *expr;
    expr          = &(*expr)->next;

    if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
      goto next_expr;
    } else if (bl_tokens_peek(cnt->tokens)->sym != BL_SYM_RBLOCK) {
      tok = bl_tokens_consume(cnt->tokens);
      parse_error(cnt, BL_ERR_MISSING_COMMA, tok, BL_BUILDER_CUR_WORD,
                  "initializer list expressions must be separated by comma " BL_YELLOW("','"));
    }
  }

  /* eat '}' */
  bl_token_t *tok_end = bl_tokens_consume(cnt->tokens);
  if (tok_end->sym != BL_SYM_RBLOCK) {
    parse_error(cnt, BL_ERR_EXPECTED_BODY_END, tok_end, BL_BUILDER_CUR_WORD,
                "expected end of the initialization block " BL_YELLOW("'}'"));
  }

  cnt->curr_init_list = prev_init_list;

  return init;
}

bl_node_t *
parse_struct_member_maybe(context_t *cnt, int order)
{
  bl_node_t *type  = NULL;
  bl_modif_e modif = parse_modifs_maybe(cnt);

  if (bl_tokens_current_is_not(cnt->tokens, BL_SYM_IDENT)) {
    return NULL;
  }
  bl_token_t *tok_id = bl_tokens_consume(cnt->tokens);

  type = parse_type_maybe(cnt, NULL);
  if (type == NULL) {
    bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
    parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok_err, BL_BUILDER_CUR_WORD,
                "expected type name after member name");
  }

  return bl_ast_add_decl_struct_member(cnt->ast, tok_id, tok_id->value.str, type, order, modif);
}

bl_node_t *
parse_enum_variant_maybe(context_t *cnt, bl_node_t *parent)
{
  if (bl_tokens_current_is_not(cnt->tokens, BL_SYM_IDENT)) {
    return NULL;
  }

  bl_token_t *tok_id     = bl_tokens_consume(cnt->tokens);
  bl_node_t * expr       = NULL;
  bl_token_t *tok_assign = bl_tokens_consume_if(cnt->tokens, BL_SYM_ASSIGN);

  if (tok_assign != NULL) {
    /* expected expression */
    expr = parse_expr_maybe(cnt, true);
    if (expr == NULL) {
      parse_error(cnt, BL_ERR_EXPECTED_EXPR, tok_assign, BL_BUILDER_CUR_WORD,
                  "expected constant expression after enum variant declaration");
    }
  }

  return bl_ast_add_decl_enum_variant(cnt->ast, tok_id, tok_id->value.str, expr, parent);
}

bl_node_t *
parse_struct_maybe(context_t *cnt, int modif)
{
  bl_node_t *strct = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_STRUCT) != NULL) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);

    if (tok->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, BL_BUILDER_CUR_WORD, "expected struct name");
    }

    bl_node_t * base     = NULL;
    bl_token_t *tok_base = bl_tokens_consume_if(cnt->tokens, BL_SYM_COLON);
    if (tok_base) {
      base = parse_type_maybe(cnt, NULL);
      if (!base) {
        parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok_base, BL_BUILDER_CUR_AFTER,
                    "expected base structure type name after" BL_YELLOW("':'"));
      }
    }

    strct                    = bl_ast_add_decl_struct(cnt->ast, tok, tok->value.str, modif, base);
    bl_decl_struct_t *_strct = bl_peek_decl_struct(strct);

    /* eat '{' */
    tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_LBLOCK);
    if (!tok) {
      tok = bl_tokens_peek_prev(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_BODY, tok, BL_BUILDER_CUR_AFTER,
                  "expected struct body " BL_YELLOW("'{'"));
    }

    int         order  = 0;
    bl_node_t * prev   = NULL;
    bl_node_t **member = &_strct->members;
  member:
    /* eat ident */
    *member = parse_struct_member_maybe(cnt, order++);
    if (*member) {
      _strct->membersc++;
      (*member)->prev = prev;
      prev            = *member;
      member          = &(*member)->next;

      if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
        goto member;
      } else if (bl_tokens_peek(cnt->tokens)->sym != BL_SYM_RBLOCK) {
        tok = bl_tokens_peek_prev(cnt->tokens);
        parse_error(
            cnt, BL_ERR_UNEXPECTED_SYMBOL, tok, BL_BUILDER_CUR_AFTER,
            "expected comma " BL_YELLOW("','") " or struct declaration end " BL_YELLOW("'}'"));
      }
    }

    /* eat '}' */
    tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_RBLOCK);
    if (!tok) {
      tok = bl_tokens_peek_prev(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_BODY_END, tok, BL_BUILDER_CUR_AFTER,
                  "expected end of struct body " BL_YELLOW("'}'"));
    }
  }

  return strct;
}

bl_node_t *
parse_enum_maybe(context_t *cnt, int modif, bl_node_t *parent)
{
  bl_node_t *enm = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_ENUM) != NULL) {
    bl_token_t *tok_id = bl_tokens_consume_if(cnt->tokens, BL_SYM_IDENT);

    if (!tok_id) {
      bl_token_t *tok_err = bl_tokens_peek_prev(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_err, BL_BUILDER_CUR_AFTER, "expected enum name");
    }

    bl_node_t *type = parse_type_maybe(cnt, NULL);

    if (type == NULL) {
      /* use i32 as default type when there is no other user specified */
      type = bl_ast_add_type_fund(cnt->ast, tok_id, BL_FTYPE_I32, false);
    }

    enm = bl_ast_add_decl_enum(cnt->ast, tok_id, tok_id->value.str, type, modif, parent);
    bl_decl_enum_t *_enm = bl_peek_decl_enum(enm);

    /* eat '{' */
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_LBLOCK) {
      parse_error(cnt, BL_ERR_EXPECTED_BODY, tok, BL_BUILDER_CUR_WORD,
                  "expected enum body " BL_YELLOW("'{'"));
    }

    bl_node_t * prev    = enm;
    bl_node_t **variant = &_enm->variants;

  variant:
    *variant = parse_enum_variant_maybe(cnt, enm);

    if (*variant) {
      (*variant)->prev = prev;
      prev             = *variant;
      variant          = &(*variant)->next;
      if (bl_tokens_consume_if(cnt->tokens, BL_SYM_COMMA)) {
        goto variant;
      } else if (bl_tokens_peek(cnt->tokens)->sym != BL_SYM_RBLOCK) {
        tok = bl_tokens_peek_prev(cnt->tokens);
        parse_error(cnt, BL_ERR_MISSING_COMMA, tok, BL_BUILDER_CUR_AFTER,
                    "enum variants must be separated by comma " BL_YELLOW("','"));
      }
    }

    if (!_enm->variants) {
      parse_error(cnt, BL_ERR_EMPTY, tok_id, BL_BUILDER_CUR_WORD,
                  "enumerator " BL_YELLOW("'%s'") " is empty", _enm->id.str);
    }

    /* eat '}' */
    tok = bl_tokens_consume_if(cnt->tokens, BL_SYM_RBLOCK);
    if (!tok) {
      tok = bl_tokens_peek_prev(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_BODY_END, tok, BL_BUILDER_CUR_AFTER,
                  "expected end of enum body " BL_YELLOW("'}'"));
    }
  }

  return enm;
}

bl_node_t *
parse_module_maybe(context_t *cnt, bl_node_t *parent, bool global, int modif)
{
  bl_node_t * module          = NULL;
  bl_token_t *tok_id          = NULL;
  bl_token_t *tok_begin_block = NULL;

  bl_assert(parent, "invalid module parent");

  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_MODULE) == NULL) {
    return NULL;
  }

  tok_id          = bl_tokens_consume(cnt->tokens);
  tok_begin_block = bl_tokens_consume(cnt->tokens);

  if (tok_id->sym != BL_SYM_IDENT) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_id, BL_BUILDER_CUR_WORD, "expected module name");
  }

  module = bl_ast_add_decl_module(cnt->ast, tok_id, tok_id->value.str, modif, parent);

  if (tok_begin_block->sym != BL_SYM_LBLOCK) {
    parse_error(cnt, BL_ERR_EXPECTED_BODY, tok_begin_block, BL_BUILDER_CUR_WORD,
                "expected block after module name " BL_YELLOW("'{'"));
  }

  parse_module_body(cnt, module);

  bl_token_t *tok_end_block = bl_tokens_consume(cnt->tokens);
  if (tok_end_block->sym != BL_SYM_RBLOCK) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok_end_block, BL_BUILDER_CUR_WORD,
                "expected block end " BL_YELLOW("'}'") " starting here: %d:%d",
                tok_begin_block->src.line, tok_begin_block->src.col);
  }

  return module;
}

void
parse_module_body(context_t *cnt, bl_node_t *module)
{
  cnt->curr_module          = module;
  int               modif   = BL_MODIF_NONE;
  bl_decl_module_t *_module = bl_peek_decl_module(module);

  bl_node_t *node;
decl:
  modif = parse_modifs_maybe(cnt);

  node = parse_module_maybe(cnt, module, false, modif);
  if (node) {
    bl_ast_insert(&_module->nodes, node);
    if (modif & BL_MODIF_EXTERN) {
      parse_error_node(cnt, BL_ERR_UNEXPECTED_MODIF, node, BL_BUILDER_CUR_WORD,
                       "module can't be declared as " BL_YELLOW("'%s'"),
                       bl_sym_strings[BL_SYM_EXTERN]);
    }
    goto decl;
  }

  node = parse_fn_maybe(cnt, modif, module);
  if (node) {
    bl_ast_insert(&_module->nodes, node);
    goto decl;
  }

  node = parse_pre_test_maybe(cnt, modif, module);
  if (node) {
    bl_ast_insert(&_module->nodes, node);
    goto decl;
  }

  node = parse_pre_load_maybe(cnt);
  if (node) {
    bl_ast_insert(&_module->nodes, node);
    goto decl;
  }

  node = parse_pre_link_maybe(cnt);
  if (node) {
    bl_ast_insert(&_module->nodes, node);
    goto decl;
  }

  node = parse_const_maybe(cnt, modif);
  if (node) {
    bl_ast_insert(&_module->nodes, node);
    parse_semicolon_rq(cnt);
    goto decl;
  }

  node = parse_using_maybe(cnt);
  if (node) {
    bl_ast_insert(&_module->nodes, node);
    parse_semicolon_rq(cnt);
    goto decl;
  }

  node = parse_struct_maybe(cnt, modif);
  if (node) {
    bl_ast_insert(&_module->nodes, node);
    if (modif & BL_MODIF_EXTERN) {
      parse_error_node(cnt, BL_ERR_UNEXPECTED_MODIF, node, BL_BUILDER_CUR_WORD,
                       "struct can't be declared as " BL_YELLOW("'%s'"),
                       bl_sym_strings[BL_SYM_EXTERN]);
    }
    goto decl;
  }

  node = parse_enum_maybe(cnt, modif, module);
  if (node) {
    bl_ast_insert(&_module->nodes, node);
    if (modif & BL_MODIF_EXTERN) {
      parse_error_node(cnt, BL_ERR_UNEXPECTED_MODIF, node, BL_BUILDER_CUR_WORD,
                       "enum can't be declared as " BL_YELLOW("'%s'"),
                       bl_sym_strings[BL_SYM_EXTERN]);
    }
    goto decl;
  }

  bl_token_t *tok = bl_tokens_peek(cnt->tokens);
  if (!bl_token_is(tok, BL_SYM_RBLOCK) && !bl_token_is(tok, BL_SYM_EOF)) {
    parse_error(cnt, BL_ERR_UNEXPECTED_SYMBOL, tok, BL_BUILDER_CUR_WORD,
                "unexpected symbol in module body");
  }
#undef iterate
}

bl_error_e
bl_parser_run(bl_builder_t *builder, bl_unit_t *unit)
{
  context_t cnt = {.builder        = builder,
                   .unit           = unit,
                   .ast            = &unit->ast,
                   .tokens         = &unit->tokens,
                   .curr_init_list = NULL,
                   .curr_func      = NULL,
                   .curr_module    = NULL,
                   .inside_loop    = false};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e)error;
  }

  /* anonymous global scope module */
  unit->ast.root = bl_ast_add_decl_module(cnt.ast, NULL, NULL, BL_MODIF_PUBLIC, NULL);
  parse_module_body(&cnt, unit->ast.root);
  return BL_NO_ERR;
}
