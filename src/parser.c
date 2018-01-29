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
maybe_decl(Tokens *tokens);

static Pnode *
parse_exp(Tokens *tokens);

static Pnode *
parse_type(Tokens *tokens);

static Pnode *
parse_id(Tokens *tokens);

static Pnode *
parse_decl(Tokens *tokens);


Pnode *
parse_exp(Tokens *tokens)
{
  return NULL;
}

Pnode *
parse_type(Tokens *tokens)
{
  Pnode *type = bl_pnode_new(BL_PT_TYPE);
  type->tok = bl_tokens_consume(tokens);
  return type;
}

Pnode *
parse_id(Tokens *tokens)
{
  Pnode *id = bl_pnode_new(BL_PT_ID);
  id->tok = bl_tokens_consume(tokens);
  return id;
}

Pnode *
parse_decl(Tokens *tokens)
{
  Pnode *decl = bl_pnode_new(BL_PT_DECL);
  Pnode *type = parse_type(tokens);
  Pnode *id = parse_id(tokens);
  bo_array_push_back(decl->nodes, type);
  bo_array_push_back(decl->nodes, id);
  return decl;
}

Pnode *
maybe_decl(Tokens *tokens)
{
  if (bl_tokens_peek(tokens)->sym == BL_SYM_IDENT &&
      bl_tokens_peek_2nd(tokens)->sym == BL_SYM_IDENT) {

  }

  // type
  if (bl_tokens_peek(tokens)->sym == BL_SYM_IDENT) {
    // identifier
    if (bl_tokens_peek_2nd(tokens)->sym == BL_SYM_IDENT) {
      if (bl_tokens_peek_nth(tokens, 3)->sym == BL_SYM_SEMICOLON ||
        bl_tokens_peek_nth(tokens, 3)->sym == BL_SYM_ASIGN) {
        Pnode *decl = parse_decl(tokens);
        if (bl_tokens_consume(tokens)->sym != BL_SYM_SEMICOLON)
          bl_parse_error("missing semicolon\n");
        return decl;
      }
    }
  }
  return NULL;
}

Pnode *
parse_gscope(Tokens *tokens)
{
  Pnode *pnode = bl_pnode_new(BL_PT_GSCOPE);
  Pnode *decl = maybe_decl(tokens);
  if (decl) {
    bo_array_push_back(pnode->nodes, decl);
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
