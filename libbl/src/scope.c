//************************************************************************************************
// Biscuit Language
//
// File:   scope.c
// Author: Martin Dorazil
// Date:   29/03/2018
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
#include "ast/ast_impl.h"

bl_scope_cache_t *
bl_scope_cache_new(void)
{
  return bo_array_new(sizeof(bl_scope_t *));
}

void
bl_scope_cache_delete(bl_scope_cache_t *cache)
{
  const size_t c     = bo_array_size(cache);
  bl_scope_t * scope = NULL;
  for (size_t i = 0; i < c; ++i) {
    scope = bo_array_at(cache, i, bl_scope_t *);
    bo_unref(scope->anonymous_syms);
    bo_unref(scope->named_syms);
    bl_free(scope);
  }

  bo_unref(cache);
}

bl_scope_t *
bl_scope_new(bl_scope_cache_t *cache)
{
  bl_scope_t *scope = bl_malloc(sizeof(bl_scope_t));

  scope->named_syms     = bo_htbl_new(sizeof(bl_node_t *), 1024);
  scope->anonymous_syms = bo_htbl_new(sizeof(bl_node_t *), 64);
  scope->magic          = 666;

  bo_array_push_back(cache, scope);
  return scope;
}

void
bl_scope_insert_node(bl_scope_t *scope, bl_node_t *node)
{
  bl_id_t *id = bl_ast_try_get_id(node);
  bl_assert(id, "invalid id");
  bo_htbl_insert(scope->named_syms, id->hash, node);
}

bl_node_t *
bl_scope_get_node(bl_scope_t *scope, bl_id_t *id)
{
  bl_assert(id, "invalid id");
  if (bo_htbl_has_key(scope->named_syms, id->hash)) {
    return bo_htbl_at(scope->named_syms, id->hash, bl_node_t *);
  }

  return NULL;
}

void
bl_scope_insert_anonymous(bl_scope_t *scope, struct bl_node *node, uint64_t key)
{
  bo_htbl_insert(scope->anonymous_syms, key, node);
}

bl_node_t *
bl_scope_get_anonymous(bl_scope_t *scope, uint64_t key)
{
  if (bo_htbl_has_key(scope->anonymous_syms, key)) {
    return bo_htbl_at(scope->anonymous_syms, key, bl_node_t *);
  }

  return NULL;
}

bl_node_t *
bl_scope_get_next_anonymous(bl_scope_t *scope, bo_iterator_t *iter)
{
  bo_iterator_t end = bo_htbl_end(scope->anonymous_syms);
  if (bo_iterator_equal(&end, iter))
    return NULL;

  bl_node_t *node = bo_htbl_iter_peek_value(scope->anonymous_syms, iter, bl_node_t *);
  bo_htbl_iter_next(scope->anonymous_syms, iter);
  return node;
}

void
bl_scope_init_iter_anonymous(bl_scope_t *scope, bo_iterator_t *iter)
{
  *iter = bo_htbl_begin(scope->anonymous_syms);
}

void
bl_scope_clear_all(bl_scope_t *scope)
{
  bo_htbl_clear(scope->named_syms);
  bo_htbl_clear(scope->anonymous_syms);
}

void
bl_scope_clear_anonymous(bl_scope_t *scope)
{
  bo_htbl_clear(scope->anonymous_syms);
}
