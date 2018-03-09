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

/* public */
void
bl_scope_init(bl_scope_t *cnt)
{
  cnt->scopes = bo_array_new_bo(bo_typeof(BHashTable), true);
  bo_array_reserve(cnt->scopes, EXPECTED_SCOPE_COUNT);

  /*
   * Push global scope by default.
   */
  bl_scope_push(cnt);
}

void
bl_scope_terminate(bl_scope_t *cnt)
{
  bo_unref(cnt->scopes);
}

void
bl_scope_push(bl_scope_t *cnt)
{
  // TODO: possible speed up when hash tables will be reused
  BHashTable *scope = bo_htbl_new(sizeof(bl_node_t *), EXPECTED_DECL_COUNT);
  bo_array_push_back(cnt->scopes, scope);
}

void
bl_scope_pop(bl_scope_t *cnt)
{
  bo_array_pop_back(cnt->scopes);
}

bl_node_t *
bl_scope_add_symbol(bl_scope_t *cnt,
                    bl_node_t *node,
                    uint32_t hash)
{
  const size_t c = bo_array_size(cnt->scopes);
  bl_assert(c, "invalid scope cache size");

  bl_node_t *found = bl_scope_get_symbol(cnt, hash);

  if (found != NULL) {
    return found;
  }

  BHashTable *htbl = bo_array_at(cnt->scopes, c - 1, BHashTable *);
  bo_htbl_insert(htbl, hash, node);

  return NULL;

}

bl_node_t *
bl_scope_get_symbol(bl_scope_t *cnt,
                    uint32_t hash)
{
  const size_t c = bo_array_size(cnt->scopes);
  bl_assert(c, "invalid scope cache size");

  BHashTable *htbl = NULL;
  for (size_t i = c; i-- > 0;) {
    htbl = bo_array_at(cnt->scopes, i, BHashTable *);
    if (bo_htbl_has_key(htbl, hash)) {
      return bo_htbl_at(htbl, hash, bl_node_t *);
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

void
bl_scope_clear(bl_scope_t *scope)
{
  bo_array_clear(scope->scopes);
}
