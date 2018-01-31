//*****************************************************************************
// bl
//
// File:   parser.c
// Author: Martin Dorazil
// Date:   26.1.18
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

#include <stdio.h>
#include <string.h>
#include "parser.h"
#include "token.h"
#include "bldebug.h"
#include "errors.h"

/* forward decl */

static Pnode *
parse_decl(Tokens *tokens);

static Pnode *
parse_func(Tokens *tokens);

static Pnode *
parse_func_args(Tokens *tokens);

static Pnode *
parse_func_arg(Tokens *tokens);

static Pnode *
parse_exp(Tokens *tokens);

static Pnode *
parse_gscope(Tokens *tokens);

static Pnode *
parse_scope(Tokens *tokens);

static void
parse_error(Tokens    *tokens,
            bl_err e);

/* impl */
void
parse_error(Tokens    *tokens,
            bl_err e)
{
  bl_token_t *tok = bl_tokens_peek(tokens);
  fprintf(stderr, "error: %d:%d %s\n", tok->line, tok->col, bl_err_strings[e]);
  exit(0);
}

Pnode *
parse_exp(Tokens *tokens)
{
  Pnode *exp = NULL;
  if (bl_tokens_peek(tokens)->sym == BL_SYM_NUM) {
    exp = bl_pnode_new(BL_PT_EXP, bl_tokens_consume(tokens));
  }
  return exp;
}

Pnode *
parse_func(Tokens *tokens)
{
  Pnode *func = NULL;
  if (bl_tokens_is_seq(tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    switch (bl_tokens_peek_nth(tokens, 3)->sym) {
      case BL_SYM_LPAREN:
        func = bl_pnode_new(BL_PT_FUNC, NULL);
        bl_pnode_new_child(func, BL_PT_TYPE, bl_tokens_consume(tokens));
        bl_pnode_new_child(func, BL_PT_ID, bl_tokens_consume(tokens));

        bl_pnode_push(func, parse_func_args(tokens)); 
        
        if (bl_tokens_current_is_not(tokens, BL_SYM_LBLOCK))
          bl_parse_error("expected '{' block after function declaration\n");

        bl_pnode_push(func, parse_scope(tokens));
        break;
      default:
        break;
    }
  }

  return func;
}

Pnode *
parse_func_arg(Tokens *tokens)
{
  Pnode *arg = NULL;
  if (bl_tokens_is_seq(tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    arg = bl_pnode_new(BL_PT_ARG, NULL);
    bl_pnode_new_child(arg, BL_PT_TYPE, bl_tokens_consume(tokens));
    bl_pnode_new_child(arg, BL_PT_ID, bl_tokens_consume(tokens));
  } else {
    bl_parse_error("invalid argument\n");
  }
  return arg;
}

Pnode *
parse_func_args(Tokens *tokens)
{
  Pnode *args = bl_pnode_new(BL_PT_ARGS, NULL);
  // eat (
  bl_tokens_consume(tokens);

  // empty argument list
  if (bl_tokens_current_is(tokens, BL_SYM_RPAREN)) {
    bl_tokens_consume(tokens);
    return args;
  }

  while (true) {
    if (bl_tokens_current_is(tokens, BL_SYM_EOF) || 
        bl_tokens_current_is(tokens, BL_SYM_LBLOCK)) {
      bl_parse_error("missing ')' at the end of argument list\n");
    }
    
    bl_pnode_push(args, parse_func_arg(tokens));

    if (bl_tokens_current_is(tokens, BL_SYM_RPAREN))
      break;

    if (bl_tokens_consume(tokens)->sym != BL_SYM_COMMA)
      bl_parse_error("missing ',' in argument list\n");
  }
  // eat last )
  bl_tokens_consume(tokens);
  return args;
}

Pnode *
parse_decl(Tokens *tokens)
{
  Pnode *decl = NULL;
  if (bl_tokens_is_seq(tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    switch (bl_tokens_peek_nth(tokens, 3)->sym) {
      case BL_SYM_ASIGN:
        decl = bl_pnode_new(BL_PT_DECL, NULL);
        bl_pnode_new_child(decl, BL_PT_TYPE, bl_tokens_consume(tokens));
        bl_pnode_new_child(decl, BL_PT_ID, bl_tokens_consume(tokens));

        // eat =
        bl_tokens_consume(tokens);
        Pnode *exp = parse_exp(tokens);
        if (!exp)
          bl_parse_error("expected expression\n");
        bl_pnode_push(decl, exp);
        break;
      case BL_SYM_SEMICOLON:
        decl = bl_pnode_new(BL_PT_DECL, NULL);
        bl_pnode_new_child(decl, BL_PT_TYPE, bl_tokens_consume(tokens));
        bl_pnode_new_child(decl, BL_PT_ID, bl_tokens_consume(tokens));
        break;
      default:
        break;
    }
  }

  if (decl && bl_tokens_consume_if(tokens, BL_SYM_SEMICOLON) == NULL)
    parse_error(tokens, BL_ERR_MISSING_SEMICOLON);

  return decl;
}

Pnode *
parse_scope(Tokens *tokens)
{
  Pnode *scope = bl_pnode_new(BL_PT_SCOPE, NULL);
  // eat {
  bl_tokens_consume(tokens);
  while (bl_tokens_current_is_not(tokens, BL_SYM_RBLOCK)) 
  {
    if (bl_tokens_current_is(tokens, BL_SYM_EOF))
      bl_parse_error("missing '}' at the end of scope\n");
    if (bl_pnode_push(scope, parse_decl(tokens)))
      continue;
    else
      parse_error(tokens, BL_ERR_UNEXPECTED_SYMBOL);
  }

  // eat }
  bl_tokens_consume(tokens);
  return scope;
}

Pnode *
parse_gscope(Tokens *tokens)
{
  Pnode *gscope = bl_pnode_new(BL_PT_GSCOPE, NULL);

  while (bl_tokens_current_is_not(tokens, BL_SYM_EOF)) {
    if (!bl_pnode_push(gscope, parse_decl(tokens)) &&
        !bl_pnode_push(gscope, parse_func(tokens))) {
        bl_parse_error("expected declaration or function\n");
    }
  }
  return gscope;
}

/* public */

Pnode *
bl_parser_parse(Tokens *tokens)
{
  /* parse global scope of the source */
  return parse_gscope(tokens);
}
