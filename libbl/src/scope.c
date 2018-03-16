//*****************************************************************************
// bl
//
// File:   scope.c
// Author: Martin Dorazil
// Date:   3/6/18
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

#include "scope_impl.h"
#include "common_impl.h"

#define EXPECTED_SCOPE_COUNT 32
#define EXPECTED_DECL_COUNT 2048

typedef struct
{
  BHashTable *idents;
  BHashTable *types;
} scope_t;

/* public */
void
bl_scope_init(bl_scope_t *cnt)
{
  cnt->scopes = bo_array_new(sizeof(scope_t));
  bo_array_reserve(cnt->scopes, EXPECTED_SCOPE_COUNT);

  /*
   * Push global scope by default.
   */
  bl_scope_push(cnt);
}

void
bl_scope_terminate(bl_scope_t *cnt)
{
  const size_t c = bo_array_size(cnt->scopes);
  for (size_t i = 0; i < c; i++) {
    scope_t *scope = &bo_array_at(cnt->scopes, i, scope_t);
    bo_unref(scope->idents);
    bo_unref(scope->types);
  }

  bo_unref(cnt->scopes);
}

void
bl_scope_push(bl_scope_t *cnt)
{
  // TODO: possible speed up when hash tables will be reused
  scope_t scope;

  scope.idents = bo_htbl_new(sizeof(bl_node_t *), EXPECTED_DECL_COUNT);
  scope.types  = bo_htbl_new(sizeof(bl_node_t *), EXPECTED_DECL_COUNT);
  bo_array_push_back(cnt->scopes, scope);
}

void
bl_scope_pop(bl_scope_t *cnt)
{
  const size_t c = bo_array_size(cnt->scopes);
  scope_t *scope = &bo_array_at(cnt->scopes, c - 1, scope_t);
  bo_unref(scope->idents);
  bo_unref(scope->types);

  bo_array_pop_back(cnt->scopes);
}

bl_node_t *
bl_scope_add_ident(bl_scope_t *cnt, bl_node_t *node)
{
  const size_t c = bo_array_size(cnt->scopes);
  bl_assert(c, "invalid scope cache size");

  bl_node_t *found = bl_scope_get_ident(cnt, &node->value.decl.ident);

  if (found != NULL) {
    return found;
  }

  scope_t *scope = &bo_array_at(cnt->scopes, c - 1, scope_t);
  bo_htbl_insert(scope->idents, node->value.decl.ident.hash, node);

  return NULL;
}

bl_node_t *
bl_scope_add_type(bl_scope_t *cnt, bl_node_t *node)
{
  const size_t c = bo_array_size(cnt->scopes);
  bl_assert(c, "invalid scope cache size");

  bl_node_t *found = bl_scope_get_type(cnt, &node->value.decl.type);

  if (found != NULL) {
    return found;
  }

  scope_t *scope = &bo_array_at(cnt->scopes, c - 1, scope_t);
  bo_htbl_insert(scope->types, node->value.decl.type.hash, node);

  return NULL;
}

bl_node_t *
bl_scope_get_ident(bl_scope_t *cnt, bl_ident_t *ident)
{
  const size_t c = bo_array_size(cnt->scopes);
  bl_assert(c, "invalid scope cache size");

  scope_t *scope = NULL;
  for (size_t i = c; i-- > 0;) {
    scope = &bo_array_at(cnt->scopes, i, scope_t);
    if (bo_htbl_has_key(scope->idents, ident->hash)) {
      return bo_htbl_at(scope->idents, ident->hash, bl_node_t *);
    }
  }
  return NULL;
}

bl_node_t *
bl_scope_get_type(bl_scope_t *cnt, bl_type_t *type)
{
  const size_t c = bo_array_size(cnt->scopes);
  bl_assert(c, "invalid scope cache size");

  scope_t *scope = NULL;
  for (size_t i = c; i-- > 0;) {
    scope = &bo_array_at(cnt->scopes, i, scope_t);
    if (bo_htbl_has_key(scope->types, type->hash)) {
      return bo_htbl_at(scope->types, type->hash, bl_node_t *);
    }
  }
  return NULL;
}

BHashTable *
bl_scope_get_all(bl_scope_t *cnt)
{
  const size_t c = bo_array_size(cnt->scopes);
  bl_assert(c, "invalid scope cache size");
  return bo_array_at(cnt->scopes, c - 1, BHashTable *);
}
