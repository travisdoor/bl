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

#include "scope.h"
#include "common.h"
#include "ast.h"

void
scope_cache_init(ScopeCache **cache)
{
  *cache = bo_array_new(sizeof(Scope *));
}

void
scope_cache_terminate(ScopeCache *cache)
{
  Scope *tmp;
  barray_foreach(cache, tmp)
  {
    bo_unref(tmp->symbols);
    bl_free(tmp);
  }

  bo_unref(cache);
}

Scope *
scope_new(ScopeCache *cache, Scope *parent, size_t size)
{
  Scope *scope = bl_malloc(sizeof(Scope));
  if (!scope) bl_abort("bad alloc");

  scope->symbols = bo_htbl_new(sizeof(Node *), size);
  scope->parent  = parent;
  bo_array_push_back(cache, scope);
  return scope;
}

void
scope_insert(Scope *scope, Node *ident, Node *node)
{
  assert(scope);
  const NodeIdent *_ident = ast_peek_ident(ident);
  bo_htbl_insert(scope->symbols, _ident->hash, node);
}

Node *
scope_lookup(Scope *scope, Node *ident, bool in_tree)
{
  assert(scope);

  while (scope) {
    NodeIdent *_ident = ast_peek_ident(ident);

    if (bo_htbl_has_key(scope->symbols, _ident->hash))
      return bo_htbl_at(scope->symbols, _ident->hash, Node *);

    if (in_tree)
      scope = scope->parent;
    else
      break;
  }

  return NULL;
}
