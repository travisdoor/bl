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
 */

struct bl_node;

/*************************************************************************************************
 * Scope Cache
 * note: Scope cache holds all scopes needed during compilation
 *************************************************************************************************/

typedef BArray bl_scope_cache_t;

bl_scope_cache_t *
bl_scope_cache_new(void);

void
bl_scope_cache_delete(bl_scope_cache_t *cache);

/*************************************************************************************************
 * Scope
 *************************************************************************************************/

typedef BHashTable bl_scope_t;

bl_scope_t *
bl_scope_new(bl_scope_cache_t *cache);

void
bl_scope_insert_node(bl_scope_t *scope, struct bl_node *node);

struct bl_node *
bl_scope_get_node(bl_scope_t *scope, bl_id_t *id);

/*************************************************************************************************
 * Linked scopes
 * note: Linked scopes is lookup hash table of scopes available for AST compound block. Every
 * compound node has one scope by default containing all symbols available in curent scope (scopes
 * can by shared between multiple modules). Scope of current compound is represented by pointer to
 * main scope in linked_scopes.
 *************************************************************************************************/

typedef struct
{
  /* key: scope pointer, value: pointer to node which caused linking */
  BHashTable *scopes;
  bl_scope_t *main;
} bl_linked_scopes_t;

void
bl_linked_scopes_init(bl_linked_scopes_t *scopes);

void
bl_linked_scopes_terminate(bl_linked_scopes_t *scopes);

/* Inserted into first scope by default */
void
bl_linked_scopes_insert_node(bl_linked_scopes_t *scopes, struct bl_node *node);

/* Search symbol across all scopes and return found node or NULL. Pointer to linked_by is set to
 * current compound block owner node or to using which includes scope from other compound block. */
struct bl_node *
bl_linked_scopes_get_node(bl_linked_scopes_t *scopes, bl_id_t *id, struct bl_node **linked_by_out);

struct bl_node *
bl_linked_scopes_get_linked_by(bl_linked_scopes_t *scopes, bl_scope_t *scope);

void
bl_linked_scopes_include(bl_linked_scopes_t *scopes, bl_scope_t *scope, struct bl_node *linked_by);

#endif /* end of include guard: BISCUIT_SCOPE_IMPL_H */
