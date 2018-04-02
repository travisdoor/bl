//*****************************************************************************
// Biscuit Language
//
// File:   block_scope.c
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

#include "block_scope_impl.h"
#include "common_impl.h"

void
bl_block_scope_init(bl_block_scope_t *scope)
{
  scope->scopes = bo_array_new_bo(bo_typeof(BHashTable), true);
}

void
bl_block_scope_terminate(bl_block_scope_t *scope)
{
  bo_unref(scope->scopes);
}

void
bl_block_scope_push(bl_block_scope_t *scope)
{
  BHashTable *new_scope = bo_htbl_new(sizeof(bl_node_t *), 128);
  bo_array_push_back(scope->scopes, new_scope);
}

void
bl_block_scope_pop(bl_block_scope_t *scope)
{
  bo_array_pop_back(scope->scopes);
}

void
bl_block_scope_insert_node(bl_block_scope_t *scope, bl_node_t *node)
{
  size_t i = bo_array_size(scope->scopes);
  bl_assert(i, "invalid scope");
  BHashTable *last_scope = bo_array_at(scope->scopes, --i, BHashTable *);
  bl_assert(last_scope, "invalid last scope table");

  bl_id_t *id = bl_ast_try_get_id(node);
  bl_assert(id, "cannot get node id");
  bo_htbl_insert(last_scope, id->hash, node);
}

bl_node_t *
bl_block_scope_get_node(bl_block_scope_t *scope, bl_id_t *id)
{
  const size_t c = bo_array_size(scope->scopes);
  BHashTable * curr_scope;

  for (size_t i = c; i-- > 0;) {
    curr_scope = bo_array_at(scope->scopes, i, BHashTable *);
    bl_assert(curr_scope, "invalid current scope table");

    if (bo_htbl_has_key(curr_scope, id->hash)) {
      return bo_htbl_at(curr_scope, id->hash, bl_node_t *);
    }
  }

  return NULL;
}
