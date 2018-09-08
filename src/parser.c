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

typedef struct
{
  Builder * builder;
  Assembly *assembly;
  Unit *    unit;
  Ast *     ast;
  Tokens *  tokens;

  /* tmps */
  Node *curr_fn;
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
parse_run(Context *cnt);

static Node *
parse_link(Context *cnt);

static void
parse_ublock_content(Context *cnt, Node *ublock);

static int
parse_flags(Context *cnt, int allowed);

static Node *
parse_ident(Context *cnt, int ptr);

static Node *
parse_block(Context *cnt);

static Node *
parse_decl(Context *cnt);

static Node *
parse_arr(Context *cnt);

static Node *
parse_type(Context *cnt);

static Node *
parse_type_fund(Context *cnt, int ptr);

static Node *
parse_type_fn(Context *cnt, bool named_args, int ptr);

static Node *
parse_type_struct(Context *cnt, bool named_args, int ptr);

static Node *
parse_type_enum(Context *cnt, int ptr);

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
parse_value(Context *cnt);

static Node *
parse_literal(Context *cnt);

static Node *
parse_literal_fn(Context *cnt);

static Node *
parse_literal_struct(Context *cnt);

static Node *
parse_literal_enum(Context *cnt);

static inline bool
parse_semicolon_rq(Context *cnt);

static Node *
parse_stmt_return(Context *cnt);

static Node *
parse_stmt_if(Context *cnt);

static Node *
parse_stmt_while(Context *cnt);

static Node *
parse_stmt_loop(Context *cnt);

static Node *
parse_stmt_break(Context *cnt);

static Node *
parse_stmt_continue(Context *cnt);

static Node *
parse_expr_sizeof(Context *cnt);

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
    return ast_bad(cnt->ast, tok_begin);
  }

  Node *to_type = parse_type(cnt);
  if (to_type == NULL) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD,
                "expected type name as cast parameter");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_bad(cnt->ast, tok_err);
  }

  tok = tokens_consume(cnt->tokens);
  if (!token_is(tok, SYM_RPAREN)) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected ')' after cast expression");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_bad(cnt->ast, tok);
  }

  Node *next = _parse_expr(cnt, parse_atom_expr(cnt, NULL), token_prec(tok_begin, false));
  if (!next) {
    tok = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_EXPR, tok, BUILDER_CUR_WORD, "expected expression after cast");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_bad(cnt->ast, tok);
  }

  return ast_expr_cast(cnt->ast, tok_begin, to_type, next);
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
    return ast_bad(cnt->ast, tok_err);
  }

  Node *in = parse_type(cnt);
  if (in == NULL) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD,
                "expected type name as parameter");

    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_bad(cnt->ast, tok_err);
  }

  /* eat ) */
  if (!tokens_consume_if(cnt->tokens, SYM_RPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected ')' after sizeof buildin argument");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_bad(cnt->ast, tok_err);
  }

  return ast_expr_sizeof(cnt->ast, tok_id, in, &ftypes[FTYPE_SIZE]);
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

  Node *expr = parse_expr(cnt);
  return ast_stmt_return(cnt->ast, tok_begin, expr, cnt->curr_decl);
}

Node *
parse_stmt_if(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_IF);
  if (!tok_begin) return NULL;

  Node *test = parse_expr(cnt);
  if (test == NULL) {
    Token *err_tok = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_EXPR, err_tok, BUILDER_CUR_WORD,
                "expected expression for the if statement");
    return ast_bad(cnt->ast, err_tok);
  }

  if (node_is(test, NODE_BAD)) {
    tokens_consume_till(cnt->tokens, SYM_LBLOCK);
  }

  Node *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    Token *err_tok = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_STMT, err_tok, BUILDER_CUR_WORD,
                "expected compound statement for true result of the if expression test");
    return ast_bad(cnt->ast, err_tok);
  }

  Node *false_stmt = NULL;
  if (tokens_consume_if(cnt->tokens, SYM_ELSE)) {
    false_stmt = parse_stmt_if(cnt);
    if (!false_stmt) false_stmt = parse_block(cnt);
    if (false_stmt == NULL) {
      Token *err_tok = tokens_consume(cnt->tokens);
      parse_error(cnt, ERR_EXPECTED_STMT, err_tok, BUILDER_CUR_WORD,
                  "expected statement for false result of the if expression test");
      return ast_bad(cnt->ast, err_tok);
    }
  }

  return ast_stmt_if(cnt->ast, tok_begin, test, true_stmt, false_stmt);
}

Node *
parse_stmt_while(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_WHILE);
  if (!tok_begin) {
    return NULL;
  }

  const bool prev_inside_loop = cnt->inside_loop;
  cnt->inside_loop            = true;

  Node *test = parse_expr(cnt);
  if (!test) {
    Token *err_tok = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_EXPR, err_tok, BUILDER_CUR_WORD,
                "expected expression for the while statement");
    return ast_bad(cnt->ast, err_tok);
  }

  Node *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    Token *err_tok = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_STMT, err_tok, BUILDER_CUR_WORD, "expected loop body");
    return ast_bad(cnt->ast, err_tok);
  }

  cnt->inside_loop = prev_inside_loop;
  return ast_stmt_loop(cnt->ast, tok_begin, test, true_stmt);
}

Node *
parse_stmt_loop(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LOOP);
  if (!tok_begin) return NULL;

  const bool prev_inside_loop = cnt->inside_loop;
  cnt->inside_loop            = true;

  TokenValue value;
  value.u         = true;
  Node *test      = ast_lit(cnt->ast, NULL, &ftypes[FTYPE_BOOL], value);
  Node *loop      = ast_stmt_loop(cnt->ast, tok_begin, test, NULL);
  Node *true_stmt = parse_block(cnt);
  if (!true_stmt) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_STMT, tok_err, BUILDER_CUR_WORD, "expected loop body");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_bad(cnt->ast, tok_err);
  }

  peek_stmt_loop(loop)->true_stmt = true_stmt;
  cnt->inside_loop                = prev_inside_loop;

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
  return ast_stmt_break(cnt->ast, tok);
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
  return ast_stmt_continue(cnt->ast, tok);
}

Node *
parse_literal(Context *cnt)
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
  return ast_lit(cnt->ast, tok, type, tok->value);
}

Node *
parse_literal_fn(Context *cnt)
{
  Token *tok_fn = tokens_peek(cnt->tokens);
  if (token_is_not(tok_fn, SYM_FN)) return NULL;

  Node *     fn  = ast_lit_fn(cnt->ast, tok_fn, NULL, NULL,
                        cnt->curr_fn ? cnt->unit->ast.root : cnt->curr_compound,
                        scope_new(cnt->assembly->scope_cache, 32));
  NodeLitFn *_fn = peek_lit_fn(fn);

  Node *prev_fn       = cnt->curr_fn;
  Node *prev_compound = cnt->curr_compound;
  cnt->curr_fn        = fn;
  cnt->curr_compound  = fn;

  _fn->type = parse_type_fn(cnt, true, 0);
  assert(_fn->type);

  /* parse block */
  _fn->block = parse_block(cnt);
  assert(_fn->block);

  cnt->curr_fn       = prev_fn;
  cnt->curr_compound = prev_compound;
  return fn;
}

Node *
parse_literal_struct(Context *cnt)
{
  Token *tok_struct = tokens_peek(cnt->tokens);
  if (token_is_not(tok_struct, SYM_STRUCT)) return NULL;

  Node *prev_compound = cnt->curr_compound;

  Node *         result      = ast_lit_struct(cnt->ast, tok_struct, NULL, cnt->curr_compound,
                                scope_new(cnt->assembly->scope_cache, 64));
  NodeLitStruct *_lit_struct = peek_lit_struct(result);

  cnt->curr_compound = result;
  _lit_struct->type  = parse_type_struct(cnt, true, 0);
  cnt->curr_compound = prev_compound;
  assert(_lit_struct->type);

  return result;
}

Node *
parse_literal_enum(Context *cnt)
{
  Token *tok_enum = tokens_peek(cnt->tokens);
  Node * type     = parse_type_enum(cnt, 0);
  if (!type) return NULL;
  NodeTypeEnum *_type = peek_type_enum(type);

  Node *enm = ast_lit_enum(cnt->ast, tok_enum, type, NULL, cnt->curr_compound,
                           scope_new(cnt->assembly->scope_cache, 256));

  NodeLitEnum *_enm = peek_lit_enum(enm);

  Node *prev_compound = cnt->curr_compound;
  cnt->curr_compound  = enm;

  Token *tok = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
  if (!tok) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "expected enm variant list");
    return ast_bad(cnt->ast, tok);
  }

  /* parse enum varinats */
  bool   rq           = false;
  Node **variant      = &_enm->variants;
  Node * prev_variant = NULL;

next:
  *variant = parse_decl(cnt);
  if (*variant) {
    NodeDecl *_variant = peek_decl(*variant);
    _variant->kind     = DECL_KIND_VARIANT;

    if (_variant->type) {
      parse_warning_node(
          cnt, _variant->type, BUILDER_CUR_WORD,
          "explicitly written type of enum varaint declaration will be overriden by enum "
          "base type");
    }

    _variant->type = _type->base_type;
    if (!_variant->value) {
      _variant->mutable = false;

      /* implicitly infer value from previous enum varaint if there is one */
      if (prev_variant) {
        NodeDecl *_prev_variant = peek_decl(prev_variant);

        TokenValue value;
        value.u        = 1;
        Node *addition = ast_lit(cnt->ast, NULL, _variant->type, value);

        _variant->value = ast_expr_binop(cnt->ast, NULL, _prev_variant->value, addition,
                                         _variant->type, SYM_PLUS);
      } else {
        /* first variant is allways 0 */
        TokenValue value;
        value.u         = 0;
        _variant->value = ast_lit(cnt->ast, NULL, _variant->type, value);
      }
    }

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
      return ast_bad(cnt->ast, tok);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RBLOCK) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of variant list  '}'  or another variant separated by comma");
    return ast_bad(cnt->ast, tok);
  }

  cnt->curr_compound = prev_compound;
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
      return ast_bad(cnt->ast, curr_op);
    }

    return ast_expr_unary(cnt->ast, curr_op, curr_op->sym, next, NULL);
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
  Token *tok_end = tokens_consume(cnt->tokens);
  if (tok_end->sym != SYM_RPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok_end, BUILDER_CUR_WORD,
                "unterminated sub-expression, missing ')', started %d:%d", tok_begin->src.line,
                tok_begin->src.col);
  }

  return expr;
}

Node *
parse_expr_member(Context *cnt, Token *op)
{
  if (!op) return NULL;
  bool is_ptr_ref = token_is(op, SYM_ARROW);

  if (token_is_not(op, SYM_DOT) && !is_ptr_ref) return NULL;

  Node *ident = parse_ident(cnt, 0);
  if (!ident) {
    parse_error(cnt, ERR_EXPECTED_NAME, op, BUILDER_CUR_WORD, "expected structure member name");
  }

  return ast_expr_member(cnt->ast, op, MEM_KIND_UNKNOWN, ident, NULL, NULL, is_ptr_ref);
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

  return ast_expr_elem(cnt->ast, op, NULL, NULL, index);
}

Node *
parse_atom_expr(Context *cnt, Token *op)
{
  Node *expr = NULL;
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

Node *
_parse_expr(Context *cnt, Node *lhs, int min_precedence)
{
  Node * rhs       = NULL;
  Token *lookahead = tokens_peek(cnt->tokens);
  Token *op        = NULL;

  while (token_prec(lookahead, false) >= min_precedence) {
    op = lookahead;
    tokens_consume(cnt->tokens);
    rhs       = parse_unary_expr(cnt, op);
    lookahead = tokens_peek(cnt->tokens);

    while (token_prec(lookahead, false) > token_prec(op, false)) {
      rhs       = _parse_expr(cnt, rhs, token_prec(lookahead, false));
      lookahead = tokens_peek(cnt->tokens);
    }
    if (op->sym == SYM_LBRACKET) {
      peek_expr_elem(rhs)->next = lhs;
      lhs                       = rhs;
    } else if (op->sym == SYM_DOT || op->sym == SYM_ARROW) {
      if (node_is(rhs, NODE_EXPR_CALL)) {
        /* rhs is call 'foo.pointer_to_some_fn()' */
        /* in this case we create new member access expression node and use it instead of call
         * expression, finally we put this new node into call reference */
        Node *        call  = rhs;
        NodeExprCall *_call = peek_expr_call(call);
        Node *member = ast_expr_member(cnt->ast, op, MEM_KIND_STRUCT, _call->ref, NULL, _call->type,
                                       token_is(op, SYM_ARROW));

        _call->ref                     = member;
        peek_expr_member(member)->next = lhs;
        lhs                            = call;
      } else {
        peek_expr_member(rhs)->next = lhs;
        lhs                         = rhs;
      }
    } else if (token_is_binop(op)) {
      Node *result_type = NULL;
      Node *tmp         = lhs;

      /* Set result type to bool for logical binary operations, this is used for type checking later
       * in the compiler pipeline. Other types are checked recursively. */
      if (token_is_logic_op(op)) {
        // IDEA use ident reference instead???
        result_type = &ftypes[FTYPE_BOOL];
      }

      lhs = ast_expr_binop(cnt->ast, op, tmp, rhs, result_type, op->sym);
    } else {
      parse_error(cnt, ERR_EXPECTED_BINOP, op, BUILDER_CUR_WORD, "expected binary operation");
      tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
      return ast_bad(cnt->ast, op);
    }
  }

  return lhs;
}

Node *
parse_ident(Context *cnt, int ptr)
{
  Token *tok_ident = tokens_consume_if(cnt->tokens, SYM_IDENT);
  if (!tok_ident) return NULL;

  assert(cnt->curr_compound);
  return ast_ident(cnt->ast, tok_ident, NULL, cnt->curr_compound, ptr, NULL);
}

Node *
parse_value(Context *cnt)
{
  Node *value = NULL;
  if ((value = parse_literal_struct(cnt))) return value;
  if ((value = parse_literal_fn(cnt))) return value;
  if ((value = parse_expr(cnt))) return value;
  return value;
}

Node *
parse_arr(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBRACKET);
  if (!tok_begin) return NULL;

  Node *expr = parse_expr(cnt);
  if (!expr) {
    parse_error(cnt, ERR_EXPECTED_EXPR, tok_begin, BUILDER_CUR_AFTER,
                "expected array size expression");
    return ast_bad(cnt->ast, tok_begin);
  }

  Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RBRACKET);
  if (!tok_begin) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok_end, BUILDER_CUR_WORD,
                "expected ']' after array size expression");
    return ast_bad(cnt->ast, tok_begin);
  }

  return expr;
}

Node *
parse_type(Context *cnt)
{
  Node *type = NULL;
  int   ptr  = 0;

  while (tokens_consume_if(cnt->tokens, SYM_ASTERISK)) {
    ++ptr;
  }

  type = parse_type_fn(cnt, false, ptr);
  if (!type) type = parse_type_struct(cnt, false, ptr);
  if (!type) type = parse_type_enum(cnt, ptr);
  if (!type) type = parse_type_fund(cnt, ptr);

  Node *arr = parse_arr(cnt);
  if (arr) {
    ast_type_set_arr(type, arr);
  }

  return type;
}

Node *
parse_type_fund(Context *cnt, int ptr)
{
  Node *type_ident = parse_ident(cnt, ptr);
  if (!type_ident) return NULL;
  assert(ptr >= 0);

  return type_ident;
}

Node *
parse_type_fn(Context *cnt, bool named_args, int ptr)
{
  Token *tok_fn = tokens_consume_if(cnt->tokens, SYM_FN);
  if (!tok_fn) return NULL;

  Token *tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_LPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected function parameter list");
    return ast_bad(cnt->ast, tok_fn);
  }

  /* parse arg types */
  Node * arg_types;
  bool   rq         = false;
  Node **arg_type   = &arg_types;
  int    argc_types = 0;

next:
  *arg_type = named_args ? parse_decl(cnt) : parse_type(cnt);
  if (*arg_type) {
    /* validate argument */
    if (node_is(*arg_type, NODE_DECL)) {
      NodeDecl *_arg_decl = peek_decl(*arg_type);
      _arg_decl->kind     = DECL_KIND_ARG;
    }
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
      return ast_bad(cnt->ast, tok_fn);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of argument type list  ')'  or another type separated by comma");
    return ast_bad(cnt->ast, tok_fn);
  }

  Node *ret_type = parse_type(cnt);
  if (!ret_type) {
    ret_type = &ftypes[FTYPE_VOID];
  }

  return ast_type_fn(cnt->ast, tok_fn, arg_types, argc_types, ret_type, ptr);
}

Node *
parse_type_struct(Context *cnt, bool named_args, int ptr)
{
  Token *tok_struct = tokens_consume_if(cnt->tokens, SYM_STRUCT);
  if (!tok_struct) return NULL;

  Token *tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_LBLOCK) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "expected struct member list");
    return ast_bad(cnt->ast, tok_struct);
  }

  /* parse arg types */
  Node * types;
  bool   rq     = false;
  Node **type   = &types;
  int    typesc = 0;

next:
  *type = named_args ? parse_decl(cnt) : parse_type(cnt);
  if (*type) {
    /* validate argument */
    if (node_is(*type, NODE_DECL)) {
      NodeDecl *_member_decl = peek_decl(*type);
      _member_decl->order    = typesc;
      _member_decl->used     = 1;
      _member_decl->kind     = DECL_KIND_MEMBER;
    }
    type = &(*type)->next;
    ++typesc;

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected member after comma ','");
      return ast_bad(cnt->ast, tok_struct);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RBLOCK) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of member list  '}'  or another memeber separated by comma");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_bad(cnt->ast, tok_struct);
  }

  return ast_type_struct(cnt->ast, tok_struct, types, typesc, NULL, ptr);
}

Node *
parse_type_enum(Context *cnt, int ptr)
{
  Token *tok_enum = tokens_consume_if(cnt->tokens, SYM_ENUM);
  if (!tok_enum) return NULL;

  Node *type = parse_type(cnt);
  /* implicit type s32 when enum base type has not been specified */
  if (!type) type = ast_type_fund(cnt->ast, NULL, FTYPE_S32, 0, NULL);

  return ast_type_enum(cnt->ast, tok_enum, type, NULL, ptr);
}

Node *
parse_decl(Context *cnt)
{
#define RETURN_BAD                                                                                 \
  {                                                                                                \
    cnt->curr_decl = prev_decl;                                                                    \
    return ast_bad(cnt->ast, tok_ident);                                                           \
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
    break;
  default:
    return NULL;
  }

  Node *ident = parse_ident(cnt, 0);
  if (!ident) return NULL;

  if (ast_is_buildin_type(ident) != -1) {
    parse_error_node(cnt, ERR_INVALID_NAME, ident, BUILDER_CUR_WORD,
                     "'%s' is reserved name of buildin type", tok_ident->value.str);
  }

  Node *prev_decl = cnt->curr_decl;
  Node *decl =
      ast_decl(cnt->ast, tok_ident, DECL_KIND_UNKNOWN, ident, NULL, NULL, true, 0, 0, false);
  cnt->curr_decl  = decl;
  NodeDecl *_decl = peek_decl(decl);

  {
    int buildin = ast_is_buildin(ident);
    if (buildin == BUILDIN_MAIN) {
      /* main function */
      _decl->flags |= FLAG_MAIN;
    }
  }

  _decl->type       = parse_type(cnt);
  Token *tok_assign = tokens_consume_if(cnt->tokens, SYM_MDECL);
  if (!tok_assign) tok_assign = tokens_consume_if(cnt->tokens, SYM_IMMDECL);

  if (tok_assign) {
    _decl->mutable = token_is(tok_assign, SYM_MDECL);
    _decl->flags |= parse_flags(cnt, FLAG_EXTERN);

    if (!(_decl->flags & FLAG_EXTERN)) {
      _decl->value = parse_value(cnt);

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

    if (!_decl->type) {
      parse_error_node(cnt, ERR_EXPECTED_TYPE, ident, BUILDER_CUR_AFTER,
                       "extern declaration must have type specified");
      RETURN_BAD;
    }
  }

  if (_decl->flags & FLAG_EXTERN) {
    _decl->kind = DECL_KIND_FN;
  } else if (_decl->value) {
    switch (node_code(_decl->value)) {
    case NODE_LIT_FN:
      _decl->kind = DECL_KIND_FN;
      break;
    case NODE_LIT_ENUM:
      _decl->kind = DECL_KIND_ENUM;
      break;
    case NODE_LIT_STRUCT:
      _decl->kind = DECL_KIND_STRUCT;
      break;
    default:
      _decl->kind = _decl->mutable ? DECL_KIND_FIELD : DECL_KIND_CONSTANT;
      break;
    }
  } else {
    _decl->kind = _decl->mutable ? DECL_KIND_FIELD : DECL_KIND_CONSTANT;
  }

  cnt->curr_decl = prev_decl;
  return decl;

#undef RETURN_BAD
}

Node *
parse_expr_call(Context *cnt)
{
  if (!tokens_is_seq(cnt->tokens, 2, SYM_IDENT, SYM_LPAREN)) return NULL;

  Token *tok_id = tokens_peek(cnt->tokens);
  Node * ident  = parse_ident(cnt, 0);
  Token *tok    = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_LPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected function parameter list");
    return ast_bad(cnt->ast, tok_id);
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
      return ast_bad(cnt->ast, tok_id);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of parameter list ')' or another parameter separated by comma");
    return ast_bad(cnt->ast, tok_id);
  }

  return ast_expr_call(cnt->ast, tok_id, ident, args, argsc, NULL, false);
}

Node *
parse_expr_null(Context *cnt)
{
  Token *tok_null = tokens_consume_if(cnt->tokens, SYM_NULL);
  if (!tok_null) return NULL;
  return ast_expr_null(cnt->ast, tok_null, NULL);
}

int
parse_flags(Context *cnt, int allowed)
{
  int    flags = 0;
  Token *tok;
next:
  tok = tokens_peek(cnt->tokens);
  switch (tok->sym) {
  case SYM_EXTERN:
    tokens_consume(cnt->tokens);
    if (!(allowed & FLAG_EXTERN)) {
      parse_error(cnt, ERR_UNEXPECTED_MODIF, tok, BUILDER_CUR_WORD, "unexpected flag");
    } else {
      flags |= FLAG_EXTERN;
    }
    goto next;
  default:
    break;
  }

  return flags;
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

  Unit *unit = unit_new_file(filepath);
  if (!assembly_add_unit_unique(cnt->assembly, unit)) {
    unit_delete(unit);
  }

  return ast_load(cnt->ast, tok_id, filepath);
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

  return ast_link(cnt->ast, tok_id, lib);
}

Node *
parse_run(Context *cnt)
{
  Token *tok = tokens_consume_if(cnt->tokens, SYM_RUN);
  if (!tok) return NULL;

  Node *call = parse_expr(cnt);
  if (!call || node_is_not(call, NODE_EXPR_CALL)) {
    parse_error(cnt, ERR_EXPECTED_EXPR, tok, BUILDER_CUR_AFTER,
                "expected call after '#run' directive");
    return ast_bad(cnt->ast, tok);
  }

  peek_expr_call(call)->run = true;

  return call;
}

Node *
parse_block(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
  if (!tok_begin) return NULL;

  Node *     prev_compound = cnt->curr_compound;
  Node *     block         = ast_block(cnt->ast, tok_begin, NULL, cnt->curr_compound,
                          scope_new(cnt->assembly->scope_cache, 1024));
  NodeBlock *_block        = peek_block(block);
  cnt->curr_compound       = block;

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

  tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
  if (!tok) {
    tok = tokens_peek_prev(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_BODY_END, tok, BUILDER_CUR_AFTER, "expected '}', starting %d:%d",
                tok_begin->src.line, tok_begin->src.col);
    cnt->curr_compound = prev_compound;
    return ast_bad(cnt->ast, tok_begin);
  }

  cnt->curr_compound = prev_compound;
  return block;
}

void
parse_ublock_content(Context *cnt, Node *ublock)
{
  cnt->curr_compound  = ublock;
  NodeUBlock *_ublock = peek_ublock(ublock);
  Node **     node    = &_ublock->nodes;
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

  Token *tok = tokens_peek(cnt->tokens);
  if (!token_is(tok, SYM_RBLOCK) && !token_is(tok, SYM_EOF)) {
    parse_error(cnt, ERR_UNEXPECTED_SYMBOL, tok, BUILDER_CUR_WORD,
                "unexpected symbol in module body");
  }
}

Node *
load_core(Context *cnt)
{
  Unit *unit = unit_new_file(CORE_SOURCE_FILE);
  if (!assembly_add_unit_unique(cnt->assembly, unit)) {
    unit_delete(unit);
  }

  return ast_load(cnt->ast, NULL, CORE_SOURCE_FILE);
}

void
parser_run(Builder *builder, Assembly *assembly, Unit *unit)
{
  Context cnt = {.builder       = builder,
                 .assembly      = assembly,
                 .unit          = unit,
                 .ast           = &unit->ast,
                 .tokens        = &unit->tokens,
                 .curr_fn       = NULL,
                 .curr_decl     = NULL,
                 .curr_compound = NULL,
                 .core_loaded   = false,
                 .inside_loop   = false};

  unit->ast.root = ast_ublock(&unit->ast, NULL, unit, assembly->gscope);
  parse_ublock_content(&cnt, unit->ast.root);
}
