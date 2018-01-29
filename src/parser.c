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
parse_gscope(BArray *tokens);

static Pnode *
parse_exp(BArray *tokens,
          size_t *i);

static Pnode *
parse_decl(BArray *tokens,
           size_t *i);

Pnode *
parse_decl(BArray *tokens,
           size_t *i)
{
  bl_token_t *tok = NULL;
  size_t _i = *i;

  Pnode *exp = parse_exp(tokens, &_i);
  if (!exp)
    return NULL;

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_SEMICOLON) {
    bl_parse_error("missing semicolon\n");
  }

  (*i) = _i;

  Pnode *decl = bl_pnode_new(BL_PT_DECL);
  Pnode *sm = bl_pnode_new(BL_PT_SEMICLON);

  bo_array_push_back(decl->nodes, exp);
  bo_array_push_back(decl->nodes, sm);
  return decl;
}

Pnode *
parse_exp(BArray *tokens,
          size_t *i)
{
  bl_token_t *tok = NULL;
  bl_token_t *ttype = NULL;
  bl_token_t *tname = NULL;
  size_t _i = *i;

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_IDENT)
    return NULL;
  ttype = tok;

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_IDENT)
    return NULL;
  tname = tok;

  Pnode *exp = bl_pnode_new(BL_PT_EXP);
  Pnode *type = bl_pnode_new(BL_PT_TYPE);
  Pnode *name = bl_pnode_new(BL_PT_NAME);

  type->tok = ttype;
  name->tok = tname;

  bo_array_push_back(exp->nodes, type);
  bo_array_push_back(exp->nodes, name);

  (*i) = _i;

  return exp;
}

Pnode *
parse_gscope(BArray *tokens)
{
  Pnode *pnode = bl_pnode_new(BL_PT_GSCOPE);
  Pnode *child = NULL;

  size_t c = bo_array_size(tokens);
  size_t i = 0;
  while (i < c) {
    child = parse_decl(tokens, &i);

    if (child) {
      bo_array_push_back(pnode->nodes, child);
      continue;
    }

    bo_unref(pnode);
    bl_parse_error("expected attribute, method or declaration\n");
  }
  return pnode;
}

/* public */

Pnode *
bl_parser_parse(BArray *tokens)
{
  if (bo_array_size(tokens) == 0)
    return 0;

  /* parse global scope of the source */
  return parse_gscope(tokens);
}
