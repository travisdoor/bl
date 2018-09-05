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

#ifndef SCOPE_IMPL_H
#define SCOPE_IMPL_H

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include <assert.h>

typedef BHashTable scope_t;
typedef BArray     scope_cache_t;

struct node;

void
scope_cache_init(scope_cache_t **cache);

void
scope_cache_terminate(scope_cache_t *cache);

scope_t *
scope_new(scope_cache_t *cache, size_t size);

void
scope_insert(scope_t *scope, struct node *ident, struct node *node);

struct node *
scope_get(scope_t *scope, struct node *ident);

bool
scope_has_symbol(scope_t *scope, struct node *ident);

#endif
