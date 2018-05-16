//************************************************************************************************
// Biscuit Language
//
// File:   scope_impl.h
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

#ifndef BISCUIT_SCOPE_IMPL_H
#define BISCUIT_SCOPE_IMPL_H

#include <bobject/containers/htbl.h>
#include <bobject/containers/array.h>
#include "ast/id_impl.h"

/* Scope cache is used for storing unique symbols found after parsing in AST nodes. Every compound
 * node (typically block of code inside curly braces) can have multiple declarations needed by other
 * parts of user code. Scope cache is attached to this compound block and it's used for symbol
 * lookup.
 *
 * Scope can store two kind of symbols:
 * 1) named symbols - all symbols which can be found by unique name
 * 2) anonymous symbols - special kind of symbols which has no name
 */

typedef struct
{
  BHashTable *named_syms;
  BHashTable *anonymous_syms;
  int         magic;
} bl_scope_t;

typedef BArray bl_scope_cache_t;

struct bl_node;

bl_scope_cache_t *
bl_scope_cache_new(void);

void
bl_scope_cache_delete(bl_scope_cache_t *cache);

bl_scope_t *
bl_scope_new(bl_scope_cache_t *cache);

void
bl_scope_insert_node(bl_scope_t *scope, struct bl_node *node);

struct bl_node *
bl_scope_get_node(bl_scope_t *scope, bl_id_t *id);

void
bl_scope_insert_anonymous(bl_scope_t *scope, struct bl_node *node);

bool
bl_scope_has_anonymous(bl_scope_t *scope, struct bl_node *node);

struct bl_node *
bl_scope_get_next_anonymous(bl_scope_t *scope, bo_iterator_t *iter);

void
bl_scope_init_iter_anonymous(bl_scope_t *scope, bo_iterator_t *iter);

void
bl_scope_clear(bl_scope_t *scope);

#endif /* end of include guard: BISCUIT_SCOPE_IMPL_H */
