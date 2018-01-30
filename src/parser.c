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
#include "parser.h"
#include "token.h"
#include "bldebug.h"

// EXP
// BL_SYM_IDENT, BL_SYM_IDENT
// EXP, BL_SYM_EQUALS, BL_SYM_NUMBER

// DECL
// EXP, BL_SYM_SEMICOLON



/* forward decl */

static Pnode *
parse_decl(Tokens *tokens);

static Pnode *
parse_exp(Tokens *tokens);

static Pnode *
parse_gscope(Tokens *tokens);

/* impl */
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
      case BL_SYM_LPAREN:
        // parse function
        break;
      default:
        decl = bl_pnode_new(BL_PT_DECL, NULL);
        bl_pnode_new_child(decl, BL_PT_TYPE, bl_tokens_consume(tokens));
        bl_pnode_new_child(decl, BL_PT_ID, bl_tokens_consume(tokens));
        break;
    }
  }

  if (bl_tokens_consume(tokens)->sym != BL_SYM_SEMICOLON)
    bl_parse_error("missing semicolon\n");

  return decl;
}

Pnode *
parse_gscope(Tokens *tokens)
{
  Pnode *gscope = bl_pnode_new(BL_PT_GSCOPE, NULL);

  while (bl_tokens_current_is_not(tokens, BL_SYM_EOF)) {
    if (!bl_pnode_push(gscope, parse_decl(tokens))) {
      bl_parse_error("expected declaration\n");
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
