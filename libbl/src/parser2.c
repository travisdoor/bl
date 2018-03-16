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

bl_func_decl_t *
parse_func_decl(context_t *cnt)
{
  bl_func_decl_t *func_decl = NULL;

  bl_tokens_consume(cnt->tokens); // i32
  bl_tokens_consume(cnt->tokens); // main
  bl_tokens_consume(cnt->tokens); // (
  bl_tokens_consume(cnt->tokens); // )

  return func_decl;
}

bl_block_t *
parse_block(context_t *cnt)
{
  bl_tokens_consume(cnt->tokens); // {
  bl_tokens_consume(cnt->tokens); // }

  return NULL;
}

bl_item_t *
parse_item(context_t *cnt)
{
  bl_item_t *item = NULL;
  bl_token_t *tok = bl_tokens_peek(cnt->tokens);

  switch (tok->sym) {
  case BL_SYM_FN:
    break;
  default:
    bl_abort("invalid symbol, expected item");
  }

  if (bl_tokens_current_is(cnt->tokens, BL_SYM_IDENT)) {
    /* must be a function */
    item    = bl_ast2_new_node(cnt->ast, BL_NODE_ITEM, bl_item_t);
    item->t = BL_ITEM_FUNC;

    item->node.func.func_decl = parse_func_decl(cnt);
    item->node.func.block     = parse_block(cnt);
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
