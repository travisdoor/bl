//************************************************************************************************
// Biscuit Language
//
// File:   global_using.c
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

#include "global_using_cache_impl.h"
#include "common_impl.h"

void
bl_global_using_init(bl_global_using_cache_t *cache)
{
  cache->caches = bo_array_new_bo(bo_typeof(BHashTable), true);
  cache->i      = -1;
}

void
bl_global_using_cache_terminate(bl_global_using_cache_t *cache)
{
  bo_unref(cache->caches);
}

void
bl_global_using_push(bl_global_using_cache_t *cache)
{
  cache->i++;
  if (bo_array_size(cache->caches) == cache->i) {
    BHashTable *new_cache = bo_htbl_new(sizeof(bl_node_t *), 128);
    bo_array_push_back(cache->caches, new_cache);
  }
}

void
bl_global_using_pop(bl_global_using_cache_t *cache)
{
  cache->i--;
}

void
bl_global_using_insert(bl_global_using_cache_t *cache, bl_node_t *node)
{
  BHashTable *last_cache = bo_array_at(cache->caches, cache->i, BHashTable *);
  bl_assert(last_cache, "invalid last cache table");
  bo_htbl_insert_empty(last_cache, (uint64_t)node);
}

bool
bl_global_usings_get_next(bl_global_using_cache_t *cache, bo_iterator_t *iter)
{
  return false;
}

void
bl_global_using_clear(bl_global_using_cache_t *cache)
{
  bo_array_clear(cache->caches);
  cache->i = -1;
}
