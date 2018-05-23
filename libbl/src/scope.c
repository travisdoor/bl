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

/*************************************************************************************************
 * Scope Cache
 *************************************************************************************************/
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
    bo_unref(scope);
  }

  bo_unref(cache);
}

/*************************************************************************************************
 * Scope
 *************************************************************************************************/

bl_scope_t *
bl_scope_new(bl_scope_cache_t *cache)
{
  bl_scope_t *scope = bo_htbl_new(sizeof(bl_node_t *), 1024);
  bo_array_push_back(cache, scope);
  return scope;
}

void
bl_scope_insert_node(bl_scope_t *scope, bl_node_t *node)
{
  bl_id_t *id = bl_ast_try_get_id(node);
  bl_assert(id, "invalid id");
  bo_htbl_insert(scope, id->hash, node);
}

bl_node_t *
bl_scope_get_node(bl_scope_t *scope, bl_id_t *id)
{
  bl_assert(id, "invalid id");
  if (bo_htbl_has_key(scope, id->hash)) {
    return bo_htbl_at(scope, id->hash, bl_node_t *);
  }

  return NULL;
}

/*************************************************************************************************
 * Linked scopes
 * note: Linked scopes is lookup hash table of scopes available for AST compound block. Every
 * compound node has one scope by default containing all symbols available in curent scope (scopes
 * can by shared between multiple modules).
 *************************************************************************************************/

void
bl_scopes_init(bl_scopes_t *scopes)
{
  scopes->scopes = bo_htbl_new(sizeof(bl_node_t *), 16);
  scopes->main   = NULL;
}

void
bl_scopes_terminate(bl_scopes_t *scopes)
{
  bo_unref(scopes->scopes);
  scopes->main = NULL;
}

void
bl_scopes_insert_node(bl_scopes_t *scopes, bl_node_t *node)
{
  bl_assert(scopes->main, "compound block has no main scope set");
  bl_scope_insert_node(scopes->main, node);
}

bl_node_t *
bl_scopes_get_node(bl_scopes_t *scopes, bl_id_t *id, bl_node_t **linked_by_out)
{
  bl_node_t *   found     = NULL;
  bl_node_t *   linked_by = NULL;
  bl_scope_t *  scope     = NULL;
  bo_iterator_t iter      = bo_htbl_begin(scopes->scopes);
  bo_iterator_t end       = bo_htbl_end(scopes->scopes);

  while (!found && !bo_iterator_equal(&iter, &end)) {
    scope     = (bl_scope_t *)bo_htbl_iter_peek_key(scopes->scopes, &iter);
    linked_by = bo_htbl_iter_peek_value(scopes->scopes, &iter, bl_node_t *);

    found = bl_scope_get_node(scope, id);
    bo_htbl_iter_next(scopes->scopes, &iter);
  }

  if (linked_by_out)
    (*linked_by_out) = linked_by;

  return found;
}

bl_node_t *
bl_scopes_get_linked_by(bl_scopes_t *scopes, bl_scope_t *scope)
{
  if (bo_htbl_has_key(scopes->scopes, (uint64_t)scope)) {
    return bo_htbl_at(scopes->scopes, (uint64_t)scope, bl_node_t *);
  }

  return NULL;
}

void
bl_scopes_include(bl_scopes_t *scopes, bl_scope_t *scope, bl_node_t *linked_by)
{
  bo_htbl_insert(scopes->scopes, (uint64_t)scope, linked_by);
}

void
bl_scopes_include_main(bl_scopes_t *scopes, bl_scope_t *scope, struct bl_node *linked_by)
{
  scopes->main = scope;
  bo_htbl_insert(scopes->scopes, (uint64_t)scope, linked_by);
}
