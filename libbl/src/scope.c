//************************************************************************************************
// bl
//
// File:   scope.c
// Author: Martin Dorazil
// Date:   3/14/18
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
//************************************************************************************************

#include "scope_impl.h"
#include "common_impl.h"
#include "ast_impl.h"

void
scope_cache_init(scope_cache_t **cache)
{
  *cache = bo_array_new(sizeof(scope_t *));
}

void
scope_cache_terminate(scope_cache_t *cache)
{
  scope_t *scope;
  bl_barray_foreach(cache, scope)
  {
    bo_unref(scope);
  }

  bo_unref(cache);
}

scope_t *
scope_new(scope_cache_t *cache, size_t size)
{
  scope_t *scope = bo_htbl_new(sizeof(node_t *), size);
  bo_array_push_back(cache, scope);
  return scope;
}

void
scope_delete(scope_t *scope)
{
  bo_unref(scope);
}

void
scope_insert(scope_t *scope, struct node *ident, struct node *node)
{
  assert(scope);
  const node_ident_t *_ident = peek_ident(ident);
  bo_htbl_insert(scope, _ident->hash, node);
}

node_t *
scope_get(scope_t *scope, node_t *ident)
{
  assert(scope);
  const node_ident_t *_ident = peek_ident(ident);
  if (scope_has_symbol(scope, ident)) return bo_htbl_at(scope, _ident->hash, node_t *);
  return NULL;
}

bool
scope_has_symbol(scope_t *scope, node_t *ident)
{
  assert(scope);
  const node_ident_t *_ident = peek_ident(ident);
  return bo_htbl_has_key(scope, _ident->hash);
}
