//************************************************************************************************
// bl
//
// File:   scope.c
// Author: Martin Dorazil
// Date:   3/14/18
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

#include "scope.h"
#include "common.h"
#include "ast.h"
#include "arena.h"
#include "unit.h"

#define ARENA_CHUNK_COUNT 256

static void
scope_dtor(Scope *scope)
{
	assert(scope);
	bo_unref(scope->entries);
}

void
scope_arenas_init(ScopeArenas *arenas)
{
	arena_init(&arenas->scope_arena, sizeof(Scope), ARENA_CHUNK_COUNT, (ArenaElemDtor)scope_dtor);
	arena_init(&arenas->entry_arena, sizeof(ScopeEntry), ARENA_CHUNK_COUNT, NULL);
}

void
scope_arenas_terminate(ScopeArenas *arenas)
{
	arena_terminate(&arenas->scope_arena);
	arena_terminate(&arenas->entry_arena);
}

Scope *
scope_create(ScopeArenas *arenas, Scope *parent, size_t size, bool is_global)
{
	Scope *scope     = arena_alloc(&arenas->scope_arena);
	scope->entries   = bo_htbl_new(sizeof(ScopeEntry *), size);
	scope->parent    = parent;
	scope->is_global = is_global;
	return scope;
}

ScopeEntry *
scope_create_entry(ScopeArenas *arenas, ScopeEntryKind kind, ID *id, struct Ast *node,
                   bool is_buildin)
{
	ScopeEntry *entry = arena_alloc(&arenas->entry_arena);
	entry->id         = id;
	entry->kind       = kind;
	entry->node       = node;
	entry->is_buildin = is_buildin;
	return entry;
}

void
scope_insert(Scope *scope, ScopeEntry *entry)
{
	assert(scope);
	assert(entry && entry->id);
	assert(!bo_htbl_has_key(scope->entries, entry->id->hash) && "duplicate scope entry key!!!");
	entry->parent_scope = scope;
	bo_htbl_insert(scope->entries, entry->id->hash, entry);
}

ScopeEntry *
scope_lookup(Scope *scope, ID *id, bool in_tree)
{
	assert(scope && id);

	while (scope) {
		if (bo_htbl_has_key(scope->entries, id->hash))
			return bo_htbl_at(scope->entries, id->hash, ScopeEntry *);

		if (in_tree)
			scope = scope->parent;
		else
			break;
	}

	return NULL;
}
