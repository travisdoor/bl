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
#include "stages.h"
#include "common.h"

#define FN_TEST_NAME "test@"

#define parse_error(cnt, code, tok, pos, format, ...)                                              \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_ERROR, (code), &(tok)->src, (pos), (format),           \
                ##__VA_ARGS__);                                                                    \
  }

#define parse_error_node(cnt, code, node, pos, format, ...)                                        \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_ERROR, (code), (node)->src, (pos), (format),           \
                ##__VA_ARGS__);                                                                    \
  }

#define parse_warning(cnt, tok, pos, format, ...)                                                  \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_WARNING, 0, &(tok)->src, (pos), (format),              \
                ##__VA_ARGS__);                                                                    \
  }

#define parse_warning_node(cnt, node, pos, format, ...)                                            \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_WARNING, 0, (node)->src, (pos), (format),              \
                ##__VA_ARGS__);                                                                    \
  }

#define parse_note(cnt, tok, pos, format, ...)                                                     \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_NOTE, 0, &(tok)->src, (pos), (format), ##__VA_ARGS__); \
  }

/* swap current compound with _cmp and create temporary variable with previous one */
#define push_curr_compound(_cnt, _cmp)                                                             \
  Node *_prev_cmp       = (_cnt)->curr_compound;                                                   \
  (_cnt)->curr_compound = (_cmp);

#define pop_curr_compound(_cnt) (_cnt)->curr_compound = _prev_cmp;

#define push_curr_decl(_cnt, _decl)                                                                \
  Node *const _prev_decl = (_cnt)->curr_decl;                                                      \
  (_cnt)->curr_decl      = (_decl);

#define pop_curr_decl(_cnt) (_cnt)->curr_decl = _prev_decl;

#define push_inloop(_cnt)                                                                          \
  bool _prev_inloop   = (_cnt)->inside_loop;                                                       \
  (_cnt)->inside_loop = true;

#define pop_inloop(_cnt) (_cnt)->inside_loop = _prev_inloop;

typedef struct
{
  Builder * builder;
  Assembly *assembly;
  Unit *    unit;
  Ast *     ast;
  Tokens *  tokens;

  /* tmps */
  Node *curr_decl;
  Node *curr_compound;
  bool  inside_loop;
  bool  core_loaded;
} Context;

/* helpers */
static inline void
insert_node(Node ***node)
{
  *node = &(**node)->next;
}

/* fw decls */
static Node *
load_core(Context *cnt);

static Node *
parse_load(Context *cnt);

static Node *
parse_assert(Context *cnt);

static Node *
parse_line(Context *cnt);

static Node *
parse_file(Context *cnt);

static Node *
parse_run(Context *cnt);

static Node *
parse_link(Context *cnt);

static Node *
parse_test(Context *cnt);

static void
parse_ublock_content(Context *cnt, Node *ublock);

static int
parse_flags(Context *cnt, int allowed);

static Node *
parse_ident(Context *cnt);

static Node *
parse_block(Context *cnt);

static Node *
parse_decl(Context *cnt);

static Node *
parse_type_type(Context *cnt);

static Node *
parse_type_arr(Context *cnt);

static Node *
parse_type_arr(Context *cnt);

static Node *
parse_type(Context *cnt);

static Node *
parse_type_fund(Context *cnt);

static Node *
parse_type_vargs(Context *cnt);

static Node *
parse_type_fn(Context *cnt, bool named_args);

static Node *
parse_type_struct(Context *cnt);

static Node *
parse_unary_expr(Context *cnt, Token *op);

static Node *
parse_expr_member(Context *cnt, Token *op);

static Node *
parse_atom_expr(Context *cnt, Token *op);

static Node *
_parse_expr(Context *cnt, Node *lhs, int min_precedence);

static inline Node *
parse_expr(Context *cnt);

static Node *
parse_expr_nested(Context *cnt);

static Node *
parse_expr_call(Context *cnt);

static Node *
parse_expr_null(Context *cnt);

static Node *
parse_decl_member(Context *cnt, bool type_only, int order);

static Node *
parse_decl_arg(Context *cnt);

static Node *
parse_decl_variant(Context *cnt, Node *base_type, Node *prev);

static Node *
parse_lit_cmp(Context *cnt, Node *prev);

static Node *
parse_lit(Context *cnt);

static Node *
parse_lit_fn(Context *cnt);

static Node *
parse_type_enum(Context *cnt);

static inline bool
parse_semicolon_rq(Context *cnt);

static Node *
parse_stmt_return(Context *cnt);

static Node *
parse_stmt_if(Context *cnt);

static Node *
parse_stmt_loop(Context *cnt);

static Node *
parse_stmt_break(Context *cnt);

static Node *
parse_stmt_continue(Context *cnt);

static Node *
parse_expr_sizeof(Context *cnt);

static Node *
parse_expr_typeof(Context *cnt);

static Node *
parse_expr_cast(Context *cnt);

static Node *
parse_expr_elem(Context *cnt, Token *op);

// impl
Node *
parse_expr_cast(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_CAST);
  if (!tok_begin) return NULL;

  Token *tok = tokens_consume(cnt->tokens);
  if (!token_is(tok, SYM_LPAREN)) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok_begin, BUILDER_CUR_WORD,
                "expected '(' after cast expression");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_begin, Node *);
  }

  Node *type = parse_type(cnt);
  if (type == NULL) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD,
                "expected type name as cast parameter");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  tok = tokens_consume(cnt->tokens);
  if (!token_is(tok, SYM_RPAREN)) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected ')' after cast expression");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok, Node *);
  }

  Node *next = _parse_expr(cnt, parse_atom_expr(cnt, NULL), token_prec(tok_begin, false));
  if (!next) {
    tok = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_EXPR, tok, BUILDER_CUR_WORD, "expected expression after cast");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok, Node *);
  }

  return ast_create_expr_cast(cnt->ast, tok_begin, type, next);
}

Node *
parse_test(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_TEST);
  if (!tok_begin) return NULL;

  Token *case_name = tokens_consume_if(cnt->tokens, SYM_STRING);
  if (!case_name) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "expected name of test case");
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  assert(cnt->curr_compound);
  Node *      type  = ast_create_type_fn(cnt->ast, tok_begin, NULL, 0, &ftypes[FTYPE_VOID]);
  Node *      value = ast_create_lit_fn(cnt->ast, tok_begin, type, NULL, cnt->curr_compound, NULL);
  const char *uname = builder_get_unique_name(cnt->builder, FN_TEST_NAME);
  Node *      name  = ast_create_ident(cnt->ast, case_name, uname, NULL, cnt->curr_compound);
  Node *test = ast_create_decl(cnt->ast, tok_begin, DECL_KIND_INVALID, name, NULL, value, false,
                               FLAG_TEST, false);

  NodeLitFn *_value = ast_peek_lit_fn(value);
  push_curr_decl(cnt, test);

  _value->block = parse_block(cnt);
  if (!_value->block) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_BODY, tok_err, BUILDER_CUR_WORD, "expected body of test case");
    pop_curr_decl(cnt);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  ast_peek_decl(test)->used++;

  TestCase test_case = {.fn = test, .name = case_name->value.str};
  bo_array_push_back(cnt->assembly->test_cases, test_case);

  pop_curr_decl(cnt);
  return test;
}

Node *
parse_decl_member(Context *cnt, bool type_only, int order)
{
  Token *tok_begin = tokens_peek(cnt->tokens);
  Node * name      = NULL;
  Node * type      = NULL;

  if (type_only) {
    type = parse_type(cnt);
  } else {
    name = parse_ident(cnt);
    type = parse_type(cnt);
  }

  if (!type && !name) return NULL;
  return ast_create_member(cnt->ast, tok_begin, name, type, order);
}

Node *
parse_decl_arg(Context *cnt)
{
  Token *tok_begin = tokens_peek(cnt->tokens);
  Node * name      = parse_ident(cnt);
  if (!name) return NULL;

  Node *type = parse_type(cnt);
  if (!type) {
    bl_abort("missing argument type");
  }

  return ast_create_arg(cnt->ast, tok_begin, name, type);
}

Node *
parse_decl_variant(Context *cnt, Node *base_type, Node *prev)
{
  Token *tok_begin = tokens_peek(cnt->tokens);
  Node * name      = parse_ident(cnt);
  if (!name) return NULL;

  Node * value      = NULL;
  Token *tok_assign = tokens_consume_if(cnt->tokens, SYM_IMMDECL);
  if (tok_assign) {
    value = parse_expr(cnt);
    if (!value) bl_abort("expected enum variant value");
  } else if (prev) {
    NodeVariant *_prev = ast_peek_variant(prev);

    TokenValue implval;
    implval.u      = 1;
    Node *addition = ast_create_lit(cnt->ast, NULL, base_type, implval);
    value = ast_create_expr_binop(cnt->ast, NULL, _prev->value, addition, base_type, SYM_PLUS);
  } else {
    /* first variant is allways 0 */
    TokenValue implval;
    implval.u = 0;
    value     = ast_create_lit(cnt->ast, NULL, base_type, implval);
  }

  assert(value);
  return ast_create_variant(cnt->ast, tok_begin, name, base_type, value);
}

Node *
parse_lit_cmp(Context *cnt, Node *prev)
{
  if (prev == NULL) return NULL;

  switch (ast_node_code(prev)) {
  case NODE_IDENT:
  case NODE_TYPE_STRUCT:
  case NODE_TYPE_ARR:
    break;
  default:
    return NULL;
  }

  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
  if (!tok_begin) return NULL;

  Node *      lit_cmp  = ast_create_lit_cmp(cnt->ast, tok_begin, prev, NULL, 0, cnt->curr_compound);
  NodeLitCmp *_lit_cmp = ast_peek_lit_cmp(lit_cmp);

  push_curr_compound(cnt, lit_cmp);

  /* parse lit_cmp fields */
  bool   rq     = false;
  Node **field  = &_lit_cmp->fields;
  int *  fieldc = &_lit_cmp->fieldc;

next:
  *field = parse_expr(cnt);
  if (*field) {
    field = &(*field)->next;
    ++(*fieldc);

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected field after comma ','");
      pop_curr_compound(cnt);
      return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
    }
  }

  /* eat } */
  if (!tokens_consume_if(cnt->tokens, SYM_RBLOCK)) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected end of initializer '}'");
    pop_curr_compound(cnt);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  pop_curr_compound(cnt);
  return lit_cmp;
}

Node *
parse_expr_sizeof(Context *cnt)
{
  Token *tok_id = tokens_consume_if(cnt->tokens, SYM_SIZEOF);
  if (!tok_id) return NULL;

  /* eat ( */
  if (!tokens_consume_if(cnt->tokens, SYM_LPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected '(' after sizeof buildin");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  Node *in = parse_type(cnt);
  if (!in) in = parse_expr(cnt);
  if (in == NULL) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD,
                "expected type name as parameter");

    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  /* eat ) */
  if (!tokens_consume_if(cnt->tokens, SYM_RPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected ')' after sizeof buildin argument");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  return ast_create_expr_sizeof(cnt->ast, tok_id, in, &ftypes[FTYPE_SIZE]);
}

Node *
parse_expr_typeof(Context *cnt)
{
  Token *tok_id = tokens_consume_if(cnt->tokens, SYM_TYPEOF);
  if (!tok_id) return NULL;

  /* eat ( */
  if (!tokens_consume_if(cnt->tokens, SYM_LPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected '(' after typeof buildin");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  Node *in = parse_type(cnt);
  if (!in) in = parse_expr(cnt);
  if (in == NULL) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD,
                "expected type name as parameter");

    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  /* eat ) */
  if (!tokens_consume_if(cnt->tokens, SYM_RPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected ')' after typeof buildin argument");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  return ast_create_expr_typeof(cnt->ast, tok_id, in, &ftypes[FTYPE_U32]);
}

bool
parse_semicolon_rq(Context *cnt)
{
  Token *tok = tokens_consume_if(cnt->tokens, SYM_SEMICOLON);
  if (!tok) {
    tok = tokens_peek_prev(cnt->tokens);
    parse_error(cnt, ERR_MISSING_SEMICOLON, tok, BUILDER_CUR_AFTER, "missing semicolon ';'");
    return false;
  }
  return true;
}

Node *
parse_stmt_return(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_RETURN);
  if (!tok_begin) {
    return NULL;
  }

  assert(cnt->curr_decl);
  Node *expr = parse_expr(cnt);
  return ast_create_stmt_return(cnt->ast, tok_begin, expr, cnt->curr_decl);
}

Node *
parse_stmt_if(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_IF);
  if (!tok_begin) return NULL;

  // eat '('
  if (!tokens_consume_if(cnt->tokens, SYM_LPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_begin, BUILDER_CUR_AFTER,
                "expected left parent '(' after 'if' statement");
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  Node *test = parse_expr(cnt);
  if (test == NULL) {
    Token *err_tok = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_EXPR, err_tok, BUILDER_CUR_WORD,
                "expected expression for the if statement");
    return ast_create_node(cnt->ast, NODE_BAD, err_tok, Node *);
  }

  if (ast_node_is(test, NODE_BAD)) {
    tokens_consume_till(cnt->tokens, SYM_LBLOCK);
  }

  // eat ')'
  if (!tokens_consume_if(cnt->tokens, SYM_RPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected closing parent ')' after 'if' statement expression");
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  Node *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    Token *err_tok = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_STMT, err_tok, BUILDER_CUR_WORD,
                "expected compound statement for true result of the if expression test");
    return ast_create_node(cnt->ast, NODE_BAD, err_tok, Node *);
  }

  Node *false_stmt = NULL;
  if (tokens_consume_if(cnt->tokens, SYM_ELSE)) {
    false_stmt = parse_stmt_if(cnt);
    if (!false_stmt) false_stmt = parse_block(cnt);
    if (!false_stmt) {
      Token *err_tok = tokens_consume(cnt->tokens);
      parse_error(cnt, ERR_EXPECTED_STMT, err_tok, BUILDER_CUR_WORD,
                  "expected statement for false result of the if expression test");
      return ast_create_node(cnt->ast, NODE_BAD, err_tok, Node *);
    }
  }

  return ast_create_stmt_if(cnt->ast, tok_begin, test, true_stmt, false_stmt);
}

static TokensLookaheadState
cmp_stmt_loop(Token *curr)
{
  if (token_is(curr, SYM_SEMICOLON))
    return TOK_LOOK_HIT;
  else if (token_is(curr, SYM_RPAREN))
    return TOK_LOOK_TERMINAL;
  else if (token_is(curr, SYM_LBLOCK))
    return TOK_LOOK_TERMINAL;

  return TOK_LOOK_CONTINUE;
}

Node *
parse_stmt_loop(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LOOP);
  if (!tok_begin) return NULL;

  const bool    while_true = tokens_current_is(cnt->tokens, SYM_LBLOCK);
  Node *        loop       = ast_create_stmt_loop(cnt->ast, tok_begin, NULL, NULL, NULL, NULL,
                                    scope_new(cnt->assembly->scope_cache, 8), cnt->curr_compound);
  NodeStmtLoop *_loop      = ast_peek_stmt_loop(loop);

  push_inloop(cnt);
  push_curr_compound(cnt, loop);

  if (!while_true) {
    // eat '('
    if (!while_true && !tokens_consume_if(cnt->tokens, SYM_LPAREN)) {
      Token *tok_err = tokens_consume(cnt->tokens);
      parse_error(cnt, ERR_MISSING_BRACKET, tok_begin, BUILDER_CUR_AFTER,
                  "expected left parent '(' after 'loop' statement");
      return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
    }

    if (tokens_lookahead(cnt->tokens, cmp_stmt_loop)) {
      /* for loop construct loop [init]; [condition]; [increment] {} */
      _loop->init = parse_decl(cnt);
      if (!parse_semicolon_rq(cnt)) {
        assert(false);
      }

      _loop->condition = parse_expr(cnt);
      if (!parse_semicolon_rq(cnt)) {
        assert(false);
      }

      _loop->increment = parse_expr(cnt);
    } else {
      /* while construct with optional condition */
      _loop->condition = parse_expr(cnt);
    }

    // eat ')'
    if (!tokens_consume_if(cnt->tokens, SYM_RPAREN)) {
      Token *tok_err = tokens_consume(cnt->tokens);
      parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                  "expected closing parent ')'");
      pop_inloop(cnt);
      pop_curr_compound(cnt);
      return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
    }
  }

  /* block */
  _loop->block = parse_block(cnt);
  if (!_loop->block) {
    Token *err_tok = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_BODY, err_tok, BUILDER_CUR_WORD, "expected loop body block");
    pop_inloop(cnt);
    pop_curr_compound(cnt);
    return ast_create_node(cnt->ast, NODE_BAD, err_tok, Node *);
  }

  pop_inloop(cnt);
  pop_curr_compound(cnt);
  return loop;
}

Node *
parse_stmt_break(Context *cnt)
{
  Token *tok = tokens_consume_if(cnt->tokens, SYM_BREAK);
  if (!tok) return NULL;

  if (!cnt->inside_loop) {
    parse_error(cnt, ERR_BREAK_OUTSIDE_LOOP, tok, BUILDER_CUR_WORD,
                "break statement outside a loop");
  }
  return ast_create_stmt_break(cnt->ast, tok);
}

Node *
parse_stmt_continue(Context *cnt)
{
  Token *tok = tokens_consume_if(cnt->tokens, SYM_CONTINUE);
  if (!tok) return NULL;

  if (!cnt->inside_loop) {
    parse_error(cnt, ERR_CONTINUE_OUTSIDE_LOOP, tok, BUILDER_CUR_WORD,
                "continue statement outside a loop");
  }
  return ast_create_stmt_continue(cnt->ast, tok);
}

Node *
parse_lit(Context *cnt)
{
  Token *tok  = tokens_peek(cnt->tokens);
  Node * type = NULL;

  switch (tok->sym) {
  case SYM_NUM:
    type = &ftypes[FTYPE_S32];
    break;
  case SYM_CHAR:
    type = &ftypes[FTYPE_CHAR];
    break;
  case SYM_STRING:
    type = &ftypes[FTYPE_STRING];
    break;
  case SYM_TRUE:
    tok->value.u = true;
    type         = &ftypes[FTYPE_BOOL];
    break;
  case SYM_FALSE:
    tok->value.u = false;
    type         = &ftypes[FTYPE_BOOL];
    break;
  case SYM_FLOAT:
    type = &ftypes[FTYPE_F32];
    break;
  case SYM_DOUBLE:
    type = &ftypes[FTYPE_F64];
    break;
  default:
    return NULL;
  }

  tokens_consume(cnt->tokens);
  return ast_create_lit(cnt->ast, tok, type, tok->value);
}

Node *
parse_lit_fn(Context *cnt)
{
  Token *tok_fn = tokens_peek(cnt->tokens);
  if (token_is_not(tok_fn, SYM_FN)) return NULL;

  Node *     fn  = ast_create_lit_fn(cnt->ast, tok_fn, NULL, NULL, cnt->curr_compound,
                               scope_new(cnt->assembly->scope_cache, 32));
  NodeLitFn *_fn = ast_peek_lit_fn(fn);

  push_curr_compound(cnt, fn);

  _fn->type = parse_type_fn(cnt, true);
  assert(_fn->type);

  /* parse block (block is optional function body can be external) */
  _fn->block = parse_block(cnt);

  pop_curr_compound(cnt);
  return fn;
}

Node *
parse_type_enum(Context *cnt)
{
  Token *tok_enum = tokens_consume_if(cnt->tokens, SYM_ENUM);
  if (!tok_enum) return NULL;

  Node *base_type = parse_type(cnt);
  if (!base_type) base_type = &ftypes[FTYPE_S32];

  Scope *scope = scope_new(cnt->assembly->scope_cache, 256);
  Node * enm = ast_create_type_enum(cnt->ast, tok_enum, base_type, NULL, cnt->curr_compound, scope);

  NodeTypeEnum *_enm = ast_peek_type_enum(enm);

  push_curr_compound(cnt, enm);

  Token *tok = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
  if (!tok) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "expected enm variant list");
    pop_curr_compound(cnt);
    return ast_create_node(cnt->ast, NODE_BAD, tok, Node *);
  }

  /* parse enum varinats */
  bool   rq           = false;
  Node **variant      = &_enm->variants;
  Node * prev_variant = NULL;

next:
  *variant = parse_decl_variant(cnt, base_type, prev_variant);
  if (*variant) {
    prev_variant = *variant;
    variant      = &(*variant)->next;

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected variant after comma ','");
      return ast_create_node(cnt->ast, NODE_BAD, tok, Node *);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RBLOCK) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of variant list  '}'  or another variant separated by comma");
    return ast_create_node(cnt->ast, NODE_BAD, tok, Node *);
  }

  pop_curr_compound(cnt);
  return enm;
}

Node *
parse_expr(Context *cnt)
{
  return _parse_expr(cnt, parse_unary_expr(cnt, NULL), 0);
}

Node *
parse_unary_expr(Context *cnt, Token *op)
{
  Token *curr_op = tokens_peek(cnt->tokens);
  if (token_is_unary(curr_op)) {
    tokens_consume(cnt->tokens);
    Node *next = _parse_expr(cnt, parse_atom_expr(cnt, NULL), token_prec(curr_op, true));

    if (next == NULL) {
      Token *err_tok = tokens_peek(cnt->tokens);
      parse_error(cnt, ERR_EXPECTED_EXPR, err_tok, BUILDER_CUR_WORD,
                  "expected expression after unary operator");
      tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
      return ast_create_node(cnt->ast, NODE_BAD, curr_op, Node *);
    }

    if (ast_node_is(next, NODE_BAD)) return next;

    return ast_create_expr_unary(cnt->ast, curr_op, curr_op->sym, next, NULL);
  } else {
    return parse_atom_expr(cnt, op);
  }
}

Node *
parse_expr_nested(Context *cnt)
{
  Node * expr      = NULL;
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LPAREN);
  if (!tok_begin) return NULL;

  expr = parse_expr(cnt);
  if (expr == NULL) {
    parse_error(cnt, ERR_EXPECTED_EXPR, tok_begin, BUILDER_CUR_WORD, "expected expression.");
  }

  /* eat ) */
  Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RPAREN);
  if (!tok_end) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "unterminated sub-expression, missing ')'");
    parse_note(cnt, tok_begin, BUILDER_CUR_WORD, "starting here");
    return ast_create_node(cnt->ast, NODE_BAD, tok_begin, Node *);
  }

  return expr;
}

Node *
parse_expr_member(Context *cnt, Token *op)
{
  if (!op) return NULL;
  if (token_is_not(op, SYM_DOT)) return NULL;

  Node *ident = parse_ident(cnt);
  if (!ident) {
    parse_error(cnt, ERR_EXPECTED_NAME, op, BUILDER_CUR_WORD, "expected structure member name");
  }

  return ast_create_expr_member(cnt->ast, op, MEM_KIND_INVALID, ident, NULL, NULL, false, -1);
}

Node *
parse_expr_elem(Context *cnt, Token *op)
{
  if (!op) return NULL;
  if (token_is_not(op, SYM_LBRACKET)) return NULL;

  Node *index = parse_expr(cnt);
  if (index == NULL) {
    parse_error(cnt, ERR_EXPECTED_EXPR, op, BUILDER_CUR_WORD, "expected array index expression");
  }

  Token *tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RBRACKET) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "missing bracket ']'");
  }

  return ast_create_expr_elem(cnt->ast, op, NULL, NULL, index);
}

Node *
parse_atom_expr(Context *cnt, Token *op)
{
  Node *expr = NULL;
  Node *tmp  = NULL;

  if ((expr = parse_expr_nested(cnt))) goto done;
  if ((expr = parse_expr_null(cnt))) goto done;
  if ((expr = parse_expr_sizeof(cnt))) goto done;
  if ((expr = parse_expr_typeof(cnt))) goto done;
  if ((expr = parse_expr_cast(cnt))) goto done;
  if ((expr = parse_run(cnt))) goto done;
  if ((expr = parse_lit_fn(cnt))) goto done;
  if ((expr = parse_type_struct(cnt))) goto done;
  if ((expr = parse_type_arr(cnt))) goto done;
  if ((expr = parse_type_enum(cnt))) goto done;
  if ((expr = parse_expr_call(cnt))) goto done;
  if ((expr = parse_expr_elem(cnt, op))) goto done;
  if ((expr = parse_lit(cnt))) goto done;
  if ((expr = parse_expr_member(cnt, op))) goto done;
  if ((expr = parse_ident(cnt))) goto done;
  if ((expr = parse_line(cnt))) goto done;
  if ((expr = parse_file(cnt))) goto done;

done:
  tmp = parse_lit_cmp(cnt, expr);
  return tmp ? tmp : expr;
}

Node *
_parse_expr(Context *cnt, Node *lhs, int min_precedence)
{
  Node * rhs       = NULL;
  Token *lookahead = tokens_peek(cnt->tokens);
  Token *op        = NULL;

  while (token_prec(lookahead, false) >= min_precedence) {
    op = lookahead;
    tokens_consume(cnt->tokens);
    rhs = parse_unary_expr(cnt, op);
    if (rhs && ast_node_is(rhs, NODE_BAD)) return rhs;
    lookahead = tokens_peek(cnt->tokens);

    while (token_prec(lookahead, false) > token_prec(op, false)) {
      rhs       = _parse_expr(cnt, rhs, token_prec(lookahead, false));
      lookahead = tokens_peek(cnt->tokens);
    }

    if (token_is(op, SYM_LBRACKET)) {
      ast_peek_expr_elem(rhs)->next = lhs;
      lhs                           = rhs;
    } else if (token_is(op, SYM_DOT)) {
      if (ast_node_is(rhs, NODE_EXPR_CALL)) {
        /* rhs is call 'foo.pointer_to_some_fn()' */
        /* in this case we create new member access expression node and use it instead of call
         * expression, finally we put this new node into call reference */
        Node *        call  = rhs;
        NodeExprCall *_call = ast_peek_expr_call(call);
        Node *member = ast_create_expr_member(cnt->ast, op, MEM_KIND_STRUCT, _call->ref, NULL,
                                              _call->type, false, -1);

        _call->ref                         = member;
        ast_peek_expr_member(member)->next = lhs;
        lhs                                = call;
      } else {
        ast_peek_expr_member(rhs)->next = lhs;
        lhs                             = rhs;
      }
    } else if (token_is_binop(op)) {
      Node *result_type = NULL;
      Node *tmp         = lhs;

      /* Set result type to bool for logical binary operations, this is used for type checking later
       * in the compiler pipeline. Other types are checked recursively. */
      if (token_is_logical(op)) {
        // IDEA use ident reference instead???
        result_type = &ftypes[FTYPE_BOOL];
      }

      lhs = ast_create_expr_binop(cnt->ast, op, tmp, rhs, result_type, op->sym);
    } else {
      parse_error(cnt, ERR_EXPECTED_BINOP, op, BUILDER_CUR_WORD, "expected binary operation");
      tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
      return ast_create_node(cnt->ast, NODE_BAD, op, Node *);
    }
  }

  return lhs;
}

Node *
parse_ident(Context *cnt)
{
  Token *tok_ident = tokens_consume_if(cnt->tokens, SYM_IDENT);
  if (!tok_ident) return NULL;

  assert(cnt->curr_compound);
  return ast_create_ident(cnt->ast, tok_ident, tok_ident->value.str, NULL, cnt->curr_compound);
}

Node *
parse_type_ptr(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_ASTERISK);
  if (!tok_begin) return NULL;

  Node *type = parse_type(cnt);
  assert(type);
  return ast_create_type_ptr(cnt->ast, tok_begin, type);
}

Node *
parse_type_type(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_TYPE);
  if (!tok_begin) return NULL;

  return ast_create_type_type(cnt->ast, tok_begin, NULL, NULL);
}

Node *
parse_type_arr(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBRACKET);
  if (!tok_begin) return NULL;

  Node *len = parse_expr(cnt);

  Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RBRACKET);
  if (!tok_begin) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok_end, BUILDER_CUR_WORD,
                "expected ']' after array size expression");
  }

  Node *elem_type = parse_type(cnt);
  assert(elem_type);
  return ast_create_type_arr(cnt->ast, tok_begin, elem_type, len);
}

Node *
parse_type(Context *cnt)
{
  Node *type = NULL;

  type = parse_type_ptr(cnt);
  if (!type) type = parse_type_type(cnt);
  if (!type) type = parse_type_fn(cnt, false);
  if (!type) type = parse_type_struct(cnt);
  if (!type) type = parse_type_enum(cnt);
  if (!type) type = parse_type_vargs(cnt);
  if (!type) type = parse_type_arr(cnt);
  if (!type) type = parse_type_fund(cnt);

  return type;
}

Node *
parse_type_fund(Context *cnt)
{
  Node *type_ident = parse_ident(cnt);
  if (!type_ident) return NULL;

  return type_ident;
}

Node *
parse_type_vargs(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_VARGS);
  if (!tok_begin) return NULL;

  return ast_create_type_vargs(cnt->ast, tok_begin);
}

Node *
parse_type_fn(Context *cnt, bool named_args)
{
  Token *tok_fn = tokens_consume_if(cnt->tokens, SYM_FN);
  if (!tok_fn) return NULL;

  Token *tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_LPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected function parameter list");
    return ast_create_node(cnt->ast, NODE_BAD, tok_fn, Node *);
  }

  /* parse arg types */
  Node * arg_types;
  bool   rq         = false;
  Node **arg_type   = &arg_types;
  int    argc_types = 0;

next:
  *arg_type = named_args ? parse_decl_arg(cnt) : parse_type(cnt);
  if (*arg_type) {
    arg_type = &(*arg_type)->next;
    ++argc_types;

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected type after comma ','");
      return ast_create_node(cnt->ast, NODE_BAD, tok_fn, Node *);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of argument type list  ')'  or another type separated by comma");
    return ast_create_node(cnt->ast, NODE_BAD, tok_fn, Node *);
  }

  Node *ret_type = parse_type(cnt);
  if (!ret_type) {
    ret_type = &ftypes[FTYPE_VOID];
  }

  return ast_create_type_fn(cnt->ast, tok_fn, arg_types, argc_types, ret_type);
}

Node *
parse_type_struct(Context *cnt)
{
  Token *tok_struct = tokens_consume_if(cnt->tokens, SYM_STRUCT);
  if (!tok_struct) return NULL;

  Token *tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_LBLOCK) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "expected struct member list");
    return ast_create_node(cnt->ast, NODE_BAD, tok_struct, Node *);
  }

  Scope *scope = scope_new(cnt->assembly->scope_cache, 64);
  Node * type_struct =
      ast_create_type_struct(cnt->ast, tok_struct, NULL, 0, cnt->curr_compound, scope, false);
  NodeTypeStruct *_type_struct = ast_peek_type_struct(type_struct);

  push_curr_compound(cnt, type_struct);
  /* parse arg types */
  bool   rq       = false;
  Node **member   = &_type_struct->members;
  int *  membersc = &_type_struct->membersc;

  const bool type_only = tokens_peek_2nd(cnt->tokens)->sym == SYM_COMMA ||
                         tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK;
  _type_struct->raw = type_only;
next:
  *member = parse_decl_member(cnt, type_only, *membersc);
  if (*member) {
    member = &(*member)->next;
    ++(*membersc);

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected member after comma ','");

      pop_curr_compound(cnt);
      return ast_create_node(cnt->ast, NODE_BAD, tok_struct, Node *);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RBLOCK) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of member list  '}'  or another memeber separated by comma");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    pop_curr_compound(cnt);
    return ast_create_node(cnt->ast, NODE_BAD, tok_struct, Node *);
  }

  pop_curr_compound(cnt);
  return type_struct;
}

Node *
parse_decl(Context *cnt)
{
#define RETURN_BAD                                                                                 \
  {                                                                                                \
    pop_curr_decl(cnt);                                                                            \
    return ast_create_node(cnt->ast, NODE_BAD, tok_ident, Node *);                                 \
  }

  Token *tok_ident = tokens_peek(cnt->tokens);
  if (token_is_not(tok_ident, SYM_IDENT)) return NULL;
  /* is value declaration? */
  if (token_is(tokens_peek_2nd(cnt->tokens), SYM_ASSIGN)) return NULL;
  Token *tok_lookehead = tokens_peek_2nd(cnt->tokens);
  switch (tok_lookehead->sym) {
  case SYM_IDENT:
  case SYM_ASTERISK:
  case SYM_FN:
  case SYM_STRUCT:
  case SYM_ENUM:
  case SYM_IMMDECL:
  case SYM_MDECL:
  case SYM_COMMA:
  case SYM_RBLOCK:
  case SYM_VARGS:
  case SYM_LBRACKET:
  case SYM_TYPE:
    break;
  default:
    return NULL;
  }

  Node *ident = parse_ident(cnt);
  if (!ident) return NULL;

  if (ast_is_buildin_type(ident) != -1) {
    parse_error_node(cnt, ERR_INVALID_NAME, ident, BUILDER_CUR_WORD,
                     "'%s' is reserved name of buildin type", tok_ident->value.str);
  }

  Node *decl =
      ast_create_decl(cnt->ast, tok_ident, DECL_KIND_INVALID, ident, NULL, NULL, true, 0, false);
  push_curr_decl(cnt, decl);
  NodeDecl *_decl = ast_peek_decl(decl);

  {
    int buildin = ast_is_buildin(ident);
    if (buildin == BUILDIN_MAIN) {
      /* main function */
      _decl->flags |= FLAG_MAIN;
      cnt->assembly->has_main = true;
    }
  }

  _decl->type       = parse_type(cnt);
  Token *tok_assign = tokens_consume_if(cnt->tokens, SYM_MDECL);
  if (!tok_assign) tok_assign = tokens_consume_if(cnt->tokens, SYM_IMMDECL);

  if (tok_assign) {
    _decl->value   = parse_expr(cnt);
    _decl->mutable = token_is(tok_assign, SYM_MDECL);
    _decl->flags |= parse_flags(cnt, FLAG_EXTERN);

    if (!(_decl->flags & (FLAG_EXTERN))) {
      if (!_decl->value) {
        parse_error(cnt, ERR_EXPECTED_INITIALIZATION, tok_assign, BUILDER_CUR_AFTER,
                    "expected binding of declaration to some value");
        RETURN_BAD;
      }
    }
  }

  if (_decl->flags & FLAG_MAIN) {
    /* main function */
    if (_decl->mutable) {
      if (tok_assign) {
        parse_error(cnt, ERR_INVALID_MUTABILITY, tok_assign, BUILDER_CUR_WORD,
                    "'main' is expected to be immutable function");
      } else {
        parse_error_node(cnt, ERR_INVALID_MUTABILITY, ident, BUILDER_CUR_WORD,
                         "'main' is expected to be immutable function");
      }

      RETURN_BAD;
    }

    if (_decl->flags & FLAG_EXTERN) {
      parse_error_node(cnt, ERR_UNEXPECTED_MODIF, ident, BUILDER_CUR_WORD,
                       "main function cannot be extern");
      RETURN_BAD;
    }
  }

  if (_decl->flags & FLAG_EXTERN) {
    if (_decl->mutable) {
      parse_error(cnt, ERR_INVALID_MUTABILITY, tok_assign, BUILDER_CUR_WORD,
                  "extern declaration cannot be mutable");
      RETURN_BAD;
    }
  }

  pop_curr_decl(cnt);
  return decl;

#undef RETURN_BAD
}

Node *
parse_expr_call(Context *cnt)
{
  if (!tokens_is_seq(cnt->tokens, 2, SYM_IDENT, SYM_LPAREN)) return NULL;

  Token *tok_id = tokens_peek(cnt->tokens);
  Node * ident  = parse_ident(cnt);
  Token *tok    = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_LPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected function parameter list");
    return ast_create_node(cnt->ast, NODE_BAD, tok_id, Node *);
  }

  /* parse args */
  bool   rq = false;
  Node * args;
  Node **arg   = &args;
  int    argsc = 0;
arg:
  *arg = parse_expr(cnt);
  if (*arg) {
    ++argsc;
    arg = &(*arg)->next;

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto arg;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected function argument after comma ','");
      return ast_create_node(cnt->ast, NODE_BAD, tok_id, Node *);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of parameter list ')' or another parameter separated by comma");
    return ast_create_node(cnt->ast, NODE_BAD, tok_id, Node *);
  }

  return ast_create_expr_call(cnt->ast, tok_id, ident, args, argsc, NULL, false);
}

Node *
parse_expr_null(Context *cnt)
{
  Token *tok_null = tokens_consume_if(cnt->tokens, SYM_NULL);
  if (!tok_null) return NULL;
  return ast_create_expr_null(cnt->ast, tok_null, NULL);
}

int
parse_flags(Context *cnt, int allowed)
{
#define CASE(_sym, _flag)                                                                          \
  case _sym: {                                                                                     \
    tokens_consume(cnt->tokens);                                                                   \
    if (!(allowed & _flag)) {                                                                      \
      parse_error(cnt, ERR_UNEXPECTED_MODIF, tok, BUILDER_CUR_WORD, "unexpected flag '%s'",        \
                  sym_strings[_sym]);                                                              \
    } else {                                                                                       \
      flags |= _flag;                                                                              \
    }                                                                                              \
    goto next;                                                                                     \
  }

  int    flags = 0;
  Token *tok;
next:
  tok = tokens_peek(cnt->tokens);
  switch (tok->sym) {
    CASE(SYM_EXTERN, FLAG_EXTERN)
  default:
    break;
  }

  return flags;
#undef CASE
}

Node *
parse_load(Context *cnt)
{
  Token *tok_id = tokens_consume_if(cnt->tokens, SYM_LOAD);
  if (!tok_id) return NULL;

  Token *tok_path = tokens_consume(cnt->tokens);
  if (!token_is(tok_path, SYM_STRING)) {
    parse_error(cnt, ERR_EXPECTED_STRING, tok_path, BUILDER_CUR_WORD,
                "expected path string after load preprocessor directive");
  }

  const char *filepath = tok_path->value.str;

  Unit *unit = unit_new_file(filepath, tok_path);
  if (!assembly_add_unit_unique(cnt->assembly, unit)) {
    unit_delete(unit);
  }

  return ast_create_load(cnt->ast, tok_id, filepath);
}

Node *
parse_assert(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_ASSERT);
  if (!tok_begin) return NULL;

  /* eat ( */
  if (!tokens_consume_if(cnt->tokens, SYM_LPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected '(' after assert buildin");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  Node *expr = parse_expr(cnt);
  if (expr == NULL) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD,
                "expected assert test expression");

    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  /* eat ) */
  if (!tokens_consume_if(cnt->tokens, SYM_RPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected ')' after assert buildin");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_node(cnt->ast, NODE_BAD, tok_err, Node *);
  }

  TokenValue tmp;

  Node *callee   = ast_create_ident(cnt->ast, tok_begin, "__assert", NULL, cnt->curr_compound);
  tmp.str        = tok_begin->src.unit->filepath;
  Node *arg_file = ast_create_lit(cnt->ast, NULL, &ftypes[FTYPE_STRING], tmp);

  tmp.u          = (unsigned long long int)tok_begin->src.line;
  Node *arg_line = ast_create_lit(cnt->ast, NULL, &ftypes[FTYPE_S32], tmp);

  expr->next     = arg_file;
  arg_file->next = arg_line;

  return ast_create_expr_call(cnt->ast, tok_begin, callee, expr, 3, &ftypes[FTYPE_VOID], false);
}

Node *
parse_line(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LINE);
  if (!tok_begin) return NULL;

  TokenValue value;
  value.u = (unsigned long long int)tok_begin->src.line;
  return ast_create_lit(cnt->ast, tok_begin, &ftypes[FTYPE_U32], value);
}

Node *
parse_file(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_FILE);
  if (!tok_begin) return NULL;

  TokenValue value;
  value.str = tok_begin->src.unit->filepath;
  return ast_create_lit(cnt->ast, tok_begin, &ftypes[FTYPE_STRING], value);
}

Node *
parse_link(Context *cnt)
{
  Token *tok_id = tokens_consume_if(cnt->tokens, SYM_LINK);
  if (!tok_id) return NULL;

  Token *tok_path = tokens_consume(cnt->tokens);
  if (!token_is(tok_path, SYM_STRING)) {
    parse_error(cnt, ERR_EXPECTED_STRING, tok_path, BUILDER_CUR_WORD,
                "expected path string after link preprocessor directive");
  }

  const char *lib = tok_path->value.str;
  assembly_add_link(cnt->assembly, lib);

  return ast_create_link(cnt->ast, tok_id, lib);
}

Node *
parse_run(Context *cnt)
{
  Token *tok = tokens_consume_if(cnt->tokens, SYM_RUN);
  if (!tok) return NULL;

  Node *call = parse_expr_call(cnt);
  if (!call || ast_node_is_not(call, NODE_EXPR_CALL)) {
    parse_error(cnt, ERR_EXPECTED_EXPR, tok, BUILDER_CUR_AFTER,
                "expected call after '#run' directive");
    return ast_create_node(cnt->ast, NODE_BAD, tok, Node *);
  }

  ast_peek_expr_call(call)->run = true;

  return call;
}

Node *
parse_block(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
  if (!tok_begin) return NULL;

  Node *     block  = ast_create_block(cnt->ast, tok_begin, NULL, cnt->curr_compound,
                                 scope_new(cnt->assembly->scope_cache, 1024));
  NodeBlock *_block = ast_peek_block(block);

  push_curr_compound(cnt, block);

  Token *tok;
  Node **node = &_block->nodes;
next:
  if (tokens_current_is(cnt->tokens, SYM_SEMICOLON)) {
    tok = tokens_consume(cnt->tokens);
    parse_warning(cnt, tok, BUILDER_CUR_WORD, "extra semicolon can be removed ';'");
    goto next;
  }

  parse_flags(cnt, 0);

  if ((*node = parse_stmt_return(cnt))) {
    if (!ast_node_is(*node, NODE_BAD)) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_test(cnt))) {
    if (!ast_node_is(*node, NODE_BAD)) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_if(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_loop(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_break(cnt))) {
    if (!ast_node_is(*node, NODE_BAD)) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_continue(cnt))) {
    if (!ast_node_is(*node, NODE_BAD)) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_decl(cnt))) {
    if (!ast_node_is(*node, NODE_BAD)) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_block(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_expr(cnt))) {
    switch (ast_node_code(*node)) {
    case NODE_EXPR_BINOP:
    case NODE_EXPR_CALL:
      break;
    default:
      parse_warning_node(cnt, *node, BUILDER_CUR_WORD, "unused expression");
    }

    if (!ast_node_is(*node, NODE_BAD)) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_assert(cnt))) {
    if (!ast_node_is(*node, NODE_BAD)) parse_semicolon_rq(cnt);
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

  tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
  if (!tok) {
    tok = tokens_peek_prev(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_BODY_END, tok, BUILDER_CUR_AFTER, "expected end of block '}'");
    parse_note(cnt, tok_begin, BUILDER_CUR_WORD, "block starting here");
    pop_curr_compound(cnt);
    return ast_create_node(cnt->ast, NODE_BAD, tok_begin, Node *);
  }

  pop_curr_compound(cnt);
  return block;
}

void
parse_ublock_content(Context *cnt, Node *ublock)
{
  push_curr_compound(cnt, ublock);
  NodeUBlock *_ublock = ast_peek_ublock(ublock);
  Node **     node    = &_ublock->nodes;
next:
  parse_flags(cnt, 0);

  if ((*node = parse_decl(cnt))) {
    if (ast_node_is_not(*node, NODE_BAD)) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_test(cnt))) {
    if (ast_node_is_not(*node, NODE_BAD)) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if (!(cnt->builder->flags & BUILDER_NO_API) && !cnt->core_loaded && (*node = load_core(cnt))) {
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

  Token *tok = tokens_peek(cnt->tokens);
  if (!token_is(tok, SYM_EOF)) {
    parse_error(cnt, ERR_UNEXPECTED_SYMBOL, tok, BUILDER_CUR_WORD,
                "unexpected symbol in module body '%s'", sym_strings[tok->sym]);
  }
  pop_curr_compound(cnt);
}

Node *
load_core(Context *cnt)
{
  Unit *unit = unit_new_file(CORE_SOURCE_FILE, NULL);
  if (!assembly_add_unit_unique(cnt->assembly, unit)) {
    unit_delete(unit);
  }

  return ast_create_load(cnt->ast, NULL, CORE_SOURCE_FILE);
}

void
parser_run(Builder *builder, Assembly *assembly, Unit *unit)
{
  unit->ast.root = ast_create_ublock(&unit->ast, NULL, unit, assembly->gscope);
  Context cnt    = {.builder       = builder,
                 .assembly      = assembly,
                 .unit          = unit,
                 .ast           = &unit->ast,
                 .tokens        = &unit->tokens,
                 .curr_decl     = NULL,
                 .curr_compound = unit->ast.root,
                 .core_loaded   = false,
                 .inside_loop   = false};

  parse_ublock_content(&cnt, unit->ast.root);
}
