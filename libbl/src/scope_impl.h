//************************************************************************************************
// bl
//
// File:   scope_impl.h
// Author: Martin Dorazil
// Date:   15/03/2018
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

#ifndef BL_SCOPE_IMPL_H
#define BL_SCOPE_IMPL_H

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include <assert.h>

typedef BHashTable bl_scope_t;
typedef BArray     bl_scope_cache_t;

struct bl_node;

void
bl_scope_cache_init(bl_scope_cache_t *cache);

void
bl_scope_cache_terminate(bl_scope_cache_t *cache);

bl_scope_t *
bl_scope_new(bl_scope_cache_t *cache, size_t size);

void
bl_scope_insert(bl_scope_t *scope, struct bl_node *ident);

inline struct bl_node *
bl_scope_get(bl_scope_t *scope, uint64_t hash)
{
  assert(scope);
  return bo_htbl_at(scope, hash, struct bl_node *);
}

inline bool
bl_scope_has_symbol(bl_scope_t *scope, uint64_t hash)
{
  assert(scope);
  return bo_htbl_has_key(scope, hash);
}

#endif
