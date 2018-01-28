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

#define error(msg) \
  { \
    fprintf(stderr, "%s\n", (msg)); \
    abort(); \
  }

static Pnode *
parse_method(BArray *tokens,
             size_t *i);

static Pnode *
parse_scope(BArray *tokens,
            size_t *i);

static Pnode *
parse_gscope(BArray *tokens);

static Pnode *
parse_decl(BArray *tokens,
           size_t *i);

Pnode *
parse_decl(BArray *tokens,
           size_t *i)
{
  bl_token_t *tok;
  bl_token_t *type;
  bl_token_t *name;

  tok = &bo_array_at(tokens, (*i), bl_token_t);
  if (tok->sym != BL_SYM_IDENT)
    return NULL;
  type = tok;

  tok = &bo_array_at(tokens, (*i)+1, bl_token_t);
  if (tok->sym != BL_SYM_IDENT)
    return NULL;
  name = tok;

  tok = &bo_array_at(tokens, (*i)+2, bl_token_t);
  if (tok->sym != BL_SYM_SEMICOLON)
    error("missing semicolon");

  (*i) += 2;

  Pnode *pnode = bl_pnode_new(BL_PT_DECL);
  pnode->content.as_decl.type = type;
  pnode->content.as_decl.name = name;
  return pnode;
}

Pnode *
parse_scope(BArray *tokens,
            size_t *i)
{
  Pnode *pnode = bl_pnode_new(BL_PT_SCOPE);
  Pnode *child;

  bl_token_t *tok = &bo_array_at(tokens, (*i), bl_token_t);
  if (tok->sym != BL_SYM_LBLOCK)
    return NULL;

  size_t c = bo_array_size(tokens);
  while (true) {
    tok = &bo_array_at(tokens, (*i), bl_token_t);
    if (tok->sym == BL_SYM_RBLOCK)
      break;

    child = parse_method(tokens, i);
    if (!child) child = parse_decl(tokens, i);

    if (child) {
      bo_array_push_back(pnode->nodes, child);
      continue;
    }

    (*i)++;
    if (*i == c) {
      bo_unref(pnode);
      error("missing block end '}'");
    }
  }

  return pnode;
}

Pnode *
parse_method(BArray *tokens,
             size_t *i)
{
  bl_token_t *tok;
  bl_token_t *ret;
  bl_token_t *name;

  tok = &bo_array_at(tokens, (*i), bl_token_t);
  if (tok->sym != BL_SYM_IDENT)
    return NULL;
  ret = tok;

  tok = &bo_array_at(tokens, (*i)+1, bl_token_t);
  if (tok->sym != BL_SYM_IDENT)
    return NULL;
  name = tok;

  tok = &bo_array_at(tokens, (*i)+2, bl_token_t);
  if (tok->sym != BL_SYM_LPAREN)
    return NULL;

  // remove when params will be implemented
  tok = &bo_array_at(tokens, (*i)+3, bl_token_t);
  if (tok->sym != BL_SYM_RPAREN)
    return NULL;

  (*i) += 4;

  Pnode *child = parse_scope(tokens, i);
  if (child) {
    Pnode *pnode = bl_pnode_new(BL_PT_METHOD);
    pnode->content.as_method.ret  = ret;
    pnode->content.as_method.name = name;

    bo_array_push_back(pnode->nodes, child);
    return pnode;
  }

  error("expected scope");
}

Pnode *
parse_gscope(BArray *tokens)
{
  Pnode *pnode = bl_pnode_new(BL_PT_GSCOPE);
  Pnode *child = NULL;

  size_t c = bo_array_size(tokens);
  for (size_t i = 0; i < c; ++i) {
    child = parse_method(tokens, &i);
    if (!child) child = parse_decl(tokens, &i);

    if (child) {
      bo_array_push_back(pnode->nodes, child);
      continue;
    }

    bo_unref(pnode);
    error("expected method");
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
