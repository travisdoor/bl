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

bl_node_t *
parse_block_rq(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_LBLOCK) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected begin of the block " BL_YELLOW("'{'"));
  }

  bl_node_t *block = bl_ast_new_node(cnt->ast, BL_NODE_BLOCK, tok);
  bl_tokens_consume(cnt->tokens);
  return block;
}

bl_node_t *
parse_fn_rq(context_t *cnt)
{
  bl_token_t *tok = bl_tokens_consume(cnt->tokens);
  if (tok->sym != BL_SYM_IDENT) {
    parse_error(cnt, BL_ERR_EXPECTED_NAME, tok, "expected function name");
  }

  bl_node_t *fn = bl_ast_new_node(cnt->ast, BL_NODE_FUNC, tok);
  bl_id_init(&BL_FUNC(fn).id, tok->value.as_string);

  // TODO: params
  bl_tokens_consume(cnt->tokens);
  bl_tokens_consume(cnt->tokens);

  BL_FUNC(fn).block = parse_block_rq(cnt);

  return fn;
}

bl_node_t *
parse_struct_rq(context_t *cnt)
{
  bl_node_t *strct = NULL;
  return strct;
}

bl_node_t *
parse_enum_rq(context_t *cnt)
{
  bl_node_t *enm = NULL;
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
      bl_id_init(&BL_MODULE(module).id, tok_id->value.as_string);
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
      bl_ast_module_push_node(&BL_MODULE(module), parse_module_rq(cnt, false));
      break;
    case BL_SYM_FN:
      bl_ast_module_push_node(&BL_MODULE(module), parse_fn_rq(cnt));
      break;
    case BL_SYM_STRUCT:
      bl_ast_module_push_node(&BL_MODULE(module), parse_struct_rq(cnt));
      break;
    case BL_SYM_ENUM:
      bl_ast_module_push_node(&BL_MODULE(module), parse_enum_rq(cnt));
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
