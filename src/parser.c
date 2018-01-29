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
parse_method(BArray *tokens,
             size_t *i);

static Pnode *
parse_call(BArray *tokens,
           size_t *i);

static Pnode *
parse_scope(BArray *tokens,
            size_t *i);

static Pnode *
parse_gscope(BArray *tokens);

static Pnode *
parse_decl(BArray *tokens,
           size_t *i);

static Pnode *
parse_attribute(BArray *tokens,
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
    bl_parse_error("missing semicolon\n");

  (*i) += 3;

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
  Pnode *child = NULL;

  size_t _i = *i;
  bl_token_t *tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_LBLOCK)
    return NULL;

  size_t c = bo_array_size(tokens);
  while (_i < c) {
    tok = &bo_array_at(tokens, _i, bl_token_t);
    if (tok->sym == BL_SYM_RBLOCK)
      break;

    child = parse_call(tokens, &_i);
    if (!child) child = parse_method(tokens, &_i);
    if (!child) child = parse_decl(tokens, &_i);

    if (child) {
      bo_array_push_back(pnode->nodes, child);
      continue;
    }

    bl_parse_error("expected call, method or declaration\n");
  }

  (*i) = ++_i;
  return pnode;
}

Pnode *
parse_method(BArray *tokens,
             size_t *i)
{
  bl_token_t *tok;
  bl_token_t *ret;
  bl_token_t *name;
  bl_token_t *modif = NULL;

  size_t _i = (*i);

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym == BL_SYM_EXTERN) {
    /* extern method declaration */
    modif = tok;
  } else {
    --_i;
  }

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_IDENT)
    return NULL;
  ret = tok;

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_IDENT)
    return NULL;
  name = tok;

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_LPAREN)
    return NULL;

  // remove when params will be implemented
  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_RPAREN)
    return NULL;

  if (modif && modif->sym == BL_SYM_EXTERN) {
    tok = &bo_array_at(tokens, _i++, bl_token_t);
    if (tok->sym != BL_SYM_SEMICOLON)
      return NULL;

    (*i) = _i;

    Pnode *pnode = bl_pnode_new(BL_PT_METHOD);
    pnode->content.as_method.ret   = ret;
    pnode->content.as_method.name  = name;
    pnode->content.as_method.modif = modif;
    return pnode;
  }

  (*i) = _i;

  Pnode *child = parse_scope(tokens, i);
  if (child) {
    Pnode *pnode = bl_pnode_new(BL_PT_METHOD);
    pnode->content.as_method.ret  = ret;
    pnode->content.as_method.name = name;

    bo_array_push_back(pnode->nodes, child);
    return pnode;
  }

  bl_parse_error("expected scope\n");
}

Pnode *
parse_call(BArray *tokens,
           size_t *i)
{
  bl_token_t *tok;
  bl_token_t *name;
  bl_token_t *param;

  size_t _i = (*i);

  // name
  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_IDENT) 
    return NULL;
  name = tok;

  // (
  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_LPAREN)
    return NULL;

  // param
  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_STRING)
    return NULL;
  param = tok;

  // )
  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_RPAREN)
    return NULL;

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_SEMICOLON)
    bl_parse_error("missing semicolon\n");

  (*i) = _i;

  Pnode *pnode = bl_pnode_new(BL_PT_CALL);
  pnode->content.as_call.name = name;
  pnode->content.as_call.param1 = param;
  return pnode;
}

Pnode *
parse_attribute(BArray *tokens,
                size_t *i)
{
  /* TODO: simple implementation, improvement needed */
  bl_token_t *tok;
  bl_token_t *header;
  bl_token_t *entry_point;
  size_t _i = (*i);

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_LBRACKET)
    return NULL;

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_STRING)
    return NULL;
  header = tok;

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_COMMA)
    return NULL;

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_STRING)
    return NULL;
  entry_point = tok;

  tok = &bo_array_at(tokens, _i++, bl_token_t);
  if (tok->sym != BL_SYM_RBRACKET)
    return NULL;

  (*i) = _i;

  Pnode *child = parse_method(tokens, i);
  if (child) {
    Pnode *pnode = bl_pnode_new(BL_PT_ATTRIBUTE);
    pnode->content.as_attribute.header      = header;
    pnode->content.as_attribute.entry_point = entry_point;

    bo_array_push_back(pnode->nodes, child);
    return pnode;
  }

  bl_parse_error("expected scope\n");
}

Pnode *
parse_gscope(BArray *tokens)
{
  Pnode *pnode = bl_pnode_new(BL_PT_GSCOPE);
  Pnode *child = NULL;

  size_t c = bo_array_size(tokens);
  size_t i = 0;
  while (i < c) {
    child = parse_attribute(tokens, &i);
    if (!child) child = parse_method(tokens, &i);
    if (!child) child = parse_decl(tokens, &i);

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
