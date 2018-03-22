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
  bl_ast_t *    ast;
  bl_tokens_t * tokens;

  jmp_buf jmp_error;
} context_t;

static bl_node_t *
parse_fn_rq(context_t *cnt);

static bl_node_t *
parse_struct_rq(context_t *cnt);

static bl_node_t *
parse_enum_rq(context_t *cnt);

static bl_node_t *
parse_module_rq(context_t *cnt, bool global);

static bl_node_t *
parse_block_rq(context_t *cnt);

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

/* impl*/
bl_node_t *
parse_const_expr_maybe(context_t *cnt)
{
// HACK: rewrite later
#define create(_type_, _str_type_)                                                                 \
  bl_tokens_consume(cnt->tokens);                                                                  \
  const_expr                   = bl_ast_new_node(cnt->ast, BL_NODE_EXPR, tok);                     \
  bl_peek_expr(const_expr).t   = BL_EXPR_CONST;                                                    \
  bl_node_t *type              = bl_ast_new_node(cnt->ast, BL_NODE_TYPE, tok);                     \
  bl_peek_type(type).t         = BL_TYPE_FUND;                                                     \
  bl_peek_type(type).type.fund = (_type_);                                                         \
  bl_id_init(&bl_peek_type(type).id, (_str_type_));

  bl_node_t * const_expr = NULL;
  bl_token_t *tok        = bl_tokens_peek(cnt->tokens);

  switch (tok->sym) {
  case BL_SYM_NUM: {
    create(BL_FTYPE_I32, "i32");
    bl_peek_const_expr(const_expr).value.s = tok->value.as_ull;
    break;
  }

  case BL_SYM_STRING: {
    create(BL_FTYPE_STRING, "string");
    bl_peek_const_expr(const_expr).value.str = tok->value.as_string;
    break;
  }

  case BL_SYM_FLOAT: {
    create(BL_FTYPE_F32, "f32");
    bl_peek_const_expr(const_expr).value.f = tok->value.as_float;
    break;
  }

  case BL_SYM_DOUBLE: {
    create(BL_FTYPE_F64, "f64");
    bl_peek_const_expr(const_expr).value.f = tok->value.as_double;
    break;
  }

  case BL_SYM_CHAR: {
    create(BL_FTYPE_CHAR, "char");
    bl_peek_const_expr(const_expr).value.c = tok->value.as_char;
    break;
  }
  case BL_SYM_TRUE:
  case BL_SYM_FALSE:
    create(BL_FTYPE_BOOL, "bool");

    if (tok->sym == BL_SYM_TRUE)
      bl_peek_const_expr(const_expr).value.b = true;
    else
      bl_peek_const_expr(const_expr).value.b = false;
    break;
  default:
    break;
  }

  return const_expr;
#undef create
}

bl_node_t *
parse_atom_expr(context_t *cnt)
{
  bl_node_t * expr = NULL;

  expr = parse_const_expr_maybe(cnt);

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
      bl_node_t *tmp         = lhs;
      lhs                    = bl_ast_new_node(cnt->ast, BL_NODE_EXPR, op);
      bl_peek_expr(lhs).t    = BL_EXPR_BINOP;
      bl_peek_binop(lhs).lhs = tmp;
      bl_peek_binop(lhs).rhs = rhs;
      bl_peek_binop(lhs).op  = op->sym;
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
  bl_node_t *var = NULL;
  if (bl_tokens_consume_if(cnt->tokens, BL_SYM_VAR)) {
    bl_token_t *tok_id = bl_tokens_consume(cnt->tokens);
    var                = bl_ast_new_node(cnt->ast, BL_NODE_VAR, tok_id);

    bl_id_init(&bl_peek_var(var).id, tok_id->value.as_string);

    bl_peek_var(var).type = parse_type_maybe(cnt);
    if (bl_peek_var(var).type == NULL) {
      bl_token_t *tok_err = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_EXPECTED_TYPE, tok_err, "expected type name after variable name");
    }
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
    type = bl_ast_new_node(cnt->ast, BL_NODE_TYPE, tok);
    bl_id_init(&bl_peek_type(type).id, tok->value.as_string);

    switch (bl_peek_type(type).id.hash) {
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
      bl_peek_type(type).t         = BL_TYPE_FUND;
      bl_peek_type(type).type.fund = (bl_fund_type_e)bl_peek_type(type).id.hash;
      break;
    default:
      bl_peek_type(type).t = BL_TYPE_REF;
    }
  }

  return type;
}

bl_node_t *
parse_ret_type_rq(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_peek(cnt->tokens);
  switch (tok->sym) {
  case BL_SYM_IDENT:
    return parse_type_maybe(cnt);
  case BL_SYM_LBLOCK:
  case BL_SYM_SEMICOLON: {
    bl_node_t *type              = bl_ast_new_node(cnt->ast, BL_NODE_TYPE, tok);
    bl_peek_type(type).id        = (bl_id_t){.str = "void", .hash = BL_FTYPE_VOID};
    bl_peek_type(type).t         = BL_TYPE_FUND;
    bl_peek_type(type).type.fund = BL_FTYPE_VOID;
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

bl_node_t *
parse_block_rq(context_t *cnt)
{
  bl_token_t *tok_begin = bl_tokens_consume(cnt->tokens);
  bl_node_t * block     = bl_ast_new_node(cnt->ast, BL_NODE_BLOCK, tok_begin);

  if (tok_begin->sym != BL_SYM_LBLOCK) {
    parse_error(cnt, BL_ERR_EXPECTED_BODY, tok_begin,
                "expected begin of the block " BL_YELLOW("'{'"));
  }

  bl_token_t *tok;
stmt:
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_SEMICOLON)) {
    tok = bl_tokens_consume(cnt->tokens);
    // TODO: warning macro
    bl_warning("%s %d:%d extra semicolon can be removed " BL_YELLOW("';'"), cnt->unit->filepath,
               tok->line, tok->col);
    goto stmt;
  }

  /* compound sub-statement */
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_LBLOCK)) {
    bl_ast_block_push_node(&bl_peek_block(block), parse_block_rq(cnt));
    goto stmt;
  }

  /* var decl */
  if (bl_ast_block_push_node(&bl_peek_block(block), parse_var_maybe(cnt))) {
    parse_semicolon_rq(cnt);
    goto stmt;
  }

  /* expr */
  if (bl_ast_block_push_node(&bl_peek_block(block), parse_expr_maybe(cnt))) {
    parse_semicolon_rq(cnt);
    goto stmt;
  }

  tok = bl_tokens_consume(cnt->tokens);

  if (tok->sym != BL_SYM_RBLOCK) {
    parse_error(cnt, BL_ERR_EXPECTED_BODY_END, tok,
                "expected declaration or scope end " BL_YELLOW("'}'") ", starting %d:%d",
                tok_begin->line, tok_begin->col);
  }

  return block;
}

bl_node_t *
parse_arg_maybe(context_t *cnt)
{
  bl_node_t *arg = NULL;
  if (bl_tokens_current_is(cnt->tokens, BL_SYM_IDENT)) {
    bl_token_t *tok = bl_tokens_consume(cnt->tokens);
    arg             = bl_ast_new_node(cnt->ast, BL_NODE_ARG, tok);
    bl_id_init(&bl_peek_arg(arg).id, tok->value.as_string);

    bl_peek_arg(arg).type = parse_type_maybe(cnt);

    if (bl_peek_arg(arg).type == NULL) {
      bl_token_t *tok = bl_tokens_peek(cnt->tokens);
      parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, "expected argument type");
    }
  }

  return arg;
}

bl_node_t *
parse_fn_rq(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_IDENT) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected function name");
  }

  bl_node_t *fn = bl_ast_new_node(cnt->ast, BL_NODE_FUNC, tok);
  bl_id_init(&bl_peek_func(fn).id, tok->value.as_string);

  tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_LPAREN) {
    parse_error(cnt, BL_ERR_MISSING_BRACKET, tok, "expected function parameter list");
  }

  /* parse args */
arg:
  if (bl_ast_func_push_arg(&bl_peek_func(fn), parse_arg_maybe(cnt))) {
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

  /*
   * parse function return type definition, and use void if there is no type specified
   */
  bl_peek_func(fn).ret_type = parse_ret_type_rq(cnt);
  bl_peek_func(fn).block    = parse_block_rq(cnt);

  return fn;
}

bl_node_t *
parse_struct_rq(context_t *cnt)
{
  bl_token_t *tok   = bl_tokens_consume(cnt->tokens);
  bl_node_t * strct = bl_ast_new_node(cnt->ast, BL_NODE_STRUCT, tok);

  if (tok->sym != BL_SYM_IDENT) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected struct name");
  }

  bl_id_init(&bl_peek_struct(strct).id, tok->value.as_string);

  // TODO
  tok = bl_tokens_consume(cnt->tokens);
  tok = bl_tokens_consume(cnt->tokens);

  return strct;
}

bl_node_t *
parse_enum_rq(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  bl_node_t * enm = bl_ast_new_node(cnt->ast, BL_NODE_ENUM, tok);

  if (tok->sym != BL_SYM_IDENT) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected enum name");
  }

  bl_id_init(&bl_peek_enum(enm).id, tok->value.as_string);

  // TODO
  tok = bl_tokens_consume(cnt->tokens);
  tok = bl_tokens_consume(cnt->tokens);

  return enm;
}

bl_node_t *
parse_module_rq(context_t *cnt, bool global)
{
  bl_node_t * module          = NULL;
  bl_token_t *tok_id          = NULL;
  bl_token_t *tok_begin_block = NULL;

  if (!global) {
    tok_id          = bl_tokens_consume(cnt->tokens);
    tok_begin_block = bl_tokens_consume(cnt->tokens);
    module          = bl_ast_new_node(cnt->ast, BL_NODE_MODULE, tok_id);

    if (tok_id->sym == BL_SYM_IDENT) {
      bl_id_init(&bl_peek_module(module).id, tok_id->value.as_string);
    } else {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok_id, "expected module name");
    }

    if (tok_begin_block->sym != BL_SYM_LBLOCK) {
      parse_error(cnt, BL_ERR_EXPECTED_BODY, tok_begin_block,
                  "expected block after module name " BL_YELLOW("'{'"));
    }
  } else {
    module = bl_ast_new_node(cnt->ast, BL_NODE_MODULE, NULL);
  }

  bl_token_t *tok  = NULL;
  bool        loop = true;
  while (loop) {
    tok = bl_tokens_consume(cnt->tokens);

    switch (tok->sym) {
    case BL_SYM_MODULE:
      bl_ast_module_push_node(&bl_peek_module(module), parse_module_rq(cnt, false));
      break;
    case BL_SYM_FN:
      bl_ast_module_push_node(&bl_peek_module(module), parse_fn_rq(cnt));
      break;
    case BL_SYM_STRUCT:
      bl_ast_module_push_node(&bl_peek_module(module), parse_struct_rq(cnt));
      break;
    case BL_SYM_ENUM:
      bl_ast_module_push_node(&bl_peek_module(module), parse_enum_rq(cnt));
      break;
    case BL_SYM_EOF:
      loop = false;
      break;
    case BL_SYM_RBLOCK:
      loop = false;
      break;
    default:
      parse_error(cnt, BL_ERR_INVALID_TOKEN, tok, "invalid token in module");
    }
  }

  if (!global) {
    if (tok->sym != BL_SYM_RBLOCK) {
      parse_error(cnt, BL_ERR_EXPECTED_BODY_END, tok,
                  "missing module block end " BL_YELLOW("'}'") ", starting " BL_YELLOW("%d:%d"),
                  tok_begin_block->line, tok_begin_block->col);
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
