//************************************************************************************************
// Biscuit Language
//
// File:   global_using_cache_impl.h
// Author: Martin Dorazil
// Date:   14.2.18
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

#ifndef GLOBAL_USING_CACHE_IMPL_H_C5LHTUYK
#define GLOBAL_USING_CACHE_IMPL_H_C5LHTUYK

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include "ast/ast_impl.h"

typedef struct bl_global_using_cache
{
  BArray *caches;
} bl_global_using_cache_t;

void
bl_global_using_cache_init(bl_global_using_cache_t *cache);

void
bl_global_using_cache_terminate(bl_global_using_cache_t *cache);

void
bl_global_using_cache_push(bl_global_using_cache_t *cache);

void
bl_global_using_cache_pop(bl_global_using_cache_t *cache);

/* insert new node or return conflicted node */
void
bl_global_using_cache_insert(bl_global_using_cache_t *cache, bl_node_t *node);

bo_iterator_t
bl_global_usings_get_next(bl_global_using_cache_t *cache, bo_iterator_t iter);

bl_node_t *
bl_global_using_cache_find_node(bl_global_using_cache_t *cache, bl_id_t *id);

void
bl_global_using_cache_clear(bl_global_using_cache_t *cache);

#endif
