//*****************************************************************************
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
//*****************************************************************************

#include "scope_impl.h"
#include "common_impl.h"
#include "ast/ast2_impl.h"

bl_scope_cache_t *
bl_scope_cache_new(void)
{
  return bo_array_new_bo(bo_typeof(BHashTable), true);
}

void
bl_scope_cache_delete(bl_scope_cache_t *cache)
{
  bo_unref(cache);
}

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
