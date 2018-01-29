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
#include <assert.h>
#include "parser.h"
#include "token.h"
#include "bldebug.h"

static Pnode *
parse_gscope(Tokens *tokens);

static Pnode *
parse_type(Tokens *tokens);

static Pnode *
parse_ident(Tokens *tokens);

static Pnode *
parse_decl(Tokens *tokens);

static Pnode *
parse_number(Tokens *tokens);

static Pnode *
maybe_type(Tokens *tokens);

/* impl */

Pnode *
parse_number(Tokens *tokens)
{
  Pnode *num = bl_pnode_new(BL_PT_EXP);
  num->tok = bl_tokens_consume(tokens);
  return num;
}

Pnode *
parse_type(Tokens *tokens)
{
  Pnode *type = bl_pnode_new(BL_PT_TYPE);
  type->tok = bl_tokens_consume(tokens);
  return type;
}

Pnode *
parse_ident(Tokens *tokens)
{
  Pnode *ident = bl_pnode_new(BL_PT_ID);
  ident->tok = bl_tokens_consume(tokens);
  return ident;
}

Pnode *
parse_decl(Tokens *tokens)
{
  Pnode *decl = bl_pnode_new(BL_PT_DECL);
  Pnode *child = parse_type(tokens);
  bo_array_push_back(decl->nodes, child);
  child = parse_ident(tokens);
  bo_array_push_back(decl->nodes, child);

  if (bl_tokens_current_is(tokens, BL_SYM_ASIGN)) {
    bl_tokens_consume(tokens);
    child = parse_number(tokens);
    bo_array_push_back(decl->nodes, child);
  }

  return decl;
}

Pnode *
maybe_type(Tokens *tokens)
{
  Pnode *child = NULL;
  if (bl_tokens_next_is(tokens, BL_SYM_IDENT)) {
    switch (bl_tokens_peek_nth(tokens, 3)->sym) {
      case BL_SYM_LPAREN:
        break;
      case BL_SYM_ASIGN:
      case BL_SYM_SEMICOLON:
        child = parse_decl(tokens);
        if (bl_tokens_consume(tokens)->sym != BL_SYM_SEMICOLON)
          bl_parse_error("missing semicolon\n");
        break;
      default:
        bl_parse_error("missing semicolon\n");
    }
  } 

  return child;
}

Pnode *
parse_gscope(Tokens *tokens)
{
  Pnode *pnode = bl_pnode_new(BL_PT_GSCOPE);
  Pnode *child = NULL;

  while (bl_tokens_next_is_not(tokens, BL_SYM_EOF)) {
    switch (bl_tokens_peek(tokens)->sym) {
      case BL_SYM_IDENT:
        child = maybe_type(tokens);
        break;
      default:
        bl_parse_error("expected type");
    }

    if (child) {
      bo_array_push_back(pnode->nodes, child);
    } else {
      bl_parse_error("expected typename\n");
    }
  }

  return pnode;
}

/* public */

Pnode *
bl_parser_parse(Tokens *tokens)
{
  /* parse global scope of the source */
  return parse_gscope(tokens);
}
