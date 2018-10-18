//************************************************************************************************
// bl
//
// File:   scope.h
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

#ifndef BL_SCOPE_H
#define BL_SCOPE_H

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include <assert.h>

struct Ast;
struct Type;
struct Arena;

typedef struct ScopeEntry
{
  struct Ast *ident;
  struct Type *type;
  bool         is_buildin;
} ScopeEntry;

typedef struct Scope
{
  struct Scope *parent;
  BHashTable *  entries;
} Scope;

void
scope_arena_init(struct Arena *arena);

void
scope_entry_arena_init(struct Arena *arena);

Scope *
scope_create(struct Arena *arena, Scope *parent, size_t size);

void
scope_insert(Scope *scope, uint64_t key, ScopeEntry *entry);

ScopeEntry *
scope_lookup(Scope *scope, struct Ast *ident, bool in_tree);

ScopeEntry *
scope_create_entry(struct Arena *arena, struct Ast *ident, struct Type *type, bool is_buildin);

#endif
