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
  bl_unit_t *unit;
  bl_ast2_t *ast;
  bl_tokens_t *tokens;

  jmp_buf jmp_error;
} context_t;

static bl_item_t *
parse_item(context_t *cnt);

static bl_module_t *
parse_module(context_t *cnt);

static bl_func_decl_t *
parse_func_decl(context_t *cnt);

static bl_block_t *
parse_block(context_t *cnt);

static bl_struct_decl_t *
parse_struct(context_t *cnt);

static bl_enum_decl_t *
parse_enum(context_t *cnt);

static bl_arg_t *
parse_arg(context_t *cnt);

bl_struct_decl_t *
parse_struct(context_t *cnt)
{
  bl_tokens_consume(cnt->tokens); // {
  bl_tokens_consume(cnt->tokens); // }
  return NULL;
}

bl_enum_decl_t *
parse_enum(context_t *cnt)
{
  bl_tokens_consume(cnt->tokens); // {
  bl_tokens_consume(cnt->tokens); // }
  return NULL;
}

bl_func_decl_t *
parse_func_decl(context_t *cnt)
{
  bl_func_decl_t *func_decl = bl_ast2_new_node(cnt->ast, BL_NODE_FUNC_DECL, bl_func_decl_t);

  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
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
                "missing " BL_YELLOW("')'") " at the end of function parameter list");
  }

  /* has return type defined? if not we use void */
  if (bl_tokens_peek(cnt->tokens)->sym == BL_SYM_IDENT) {
    bl_tokens_consume(cnt->tokens);
  }

  return func_decl;
}

bl_arg_t *
parse_arg(context_t *cnt)
{
  bl_arg_t *arg = NULL;
  if (bl_tokens_is_seq(cnt->tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    arg = bl_ast2_new_node(cnt->ast, BL_NODE_ARG, bl_arg_t);
    // TODO: fill arg
    bl_tokens_consume(cnt->tokens);
    bl_tokens_consume(cnt->tokens);
  }

  return arg;
}

bl_block_t *
parse_block(context_t *cnt)
{
  bl_block_t *block = bl_ast2_new_node(cnt->ast, BL_NODE_BLOCK, bl_block_t);
  bl_tokens_consume(cnt->tokens); // {
  bl_tokens_consume(cnt->tokens); // }

  return block;
}

bl_item_t *
parse_item(context_t *cnt)
{
  bl_item_t *item = NULL;
  bl_token_t *tok = bl_tokens_peek(cnt->tokens);

  switch (tok->sym) {
  case BL_SYM_FN: {
    /* eat fn */
    bl_tokens_consume(cnt->tokens);

    /* parse name */
    tok = bl_tokens_consume(cnt->tokens);
    if (tok->sym != BL_SYM_IDENT) {
      parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected function name");
    }

    item    = bl_ast2_new_node(cnt->ast, BL_NODE_ITEM, bl_item_t);
    item->t = BL_ITEM_FUNC;

    bl_id_init(&item->id, tok->value.as_string);
    bl_src_init(&item->src, tok);

    item->node.func.func_decl = parse_func_decl(cnt);
    item->node.func.block     = parse_block(cnt);
    break;
  }
  case BL_SYM_STRUCT:
    bl_tokens_consume(cnt->tokens);
    tok     = bl_tokens_consume(cnt->tokens);
    item    = bl_ast2_new_node(cnt->ast, BL_NODE_ITEM, bl_item_t);
    item->t = BL_ITEM_STRUCT;
    bl_id_init(&item->id, tok->value.as_string);
    bl_src_init(&item->src, tok);
    item->node.struct_decl = parse_struct(cnt);
    break;
  case BL_SYM_ENUM:
    bl_tokens_consume(cnt->tokens);
    tok     = bl_tokens_consume(cnt->tokens);
    item    = bl_ast2_new_node(cnt->ast, BL_NODE_ITEM, bl_item_t);
    item->t = BL_ITEM_ENUM;
    bl_id_init(&item->id, tok->value.as_string);
    bl_src_init(&item->src, tok);
    item->node.enum_decl = parse_enum(cnt);
    break;
  case BL_SYM_EOF:
    break;
  default:
    bl_abort("invalid symbol %d, expected item", tok->sym);
  }

  return item;
}

bl_module_t *
parse_module(context_t *cnt)
{
  bl_module_t *module = bl_ast2_new_node(cnt->ast, BL_NODE_MODULE, bl_module_t);

  /*
   * Should be extended when nested modules and named modules will
   * be implemented.
   */

  while (bl_ast_module_push_item(module, parse_item(cnt))) {
  };

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

  unit->ast.root = parse_module(&cnt);
  return BL_NO_ERR;
}
