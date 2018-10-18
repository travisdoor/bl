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
#define VERBOSE 1

static void
scope_dtor(Scope *scope)
{
  assert(scope);
  bo_unref(scope->entries);
}

void
scope_arena_init(Arena *arena)
{
  arena_init(arena, sizeof(Scope), ARENA_CHUNK_COUNT, (ArenaElemDtor)scope_dtor);
}

void
scope_entry_arena_init(struct Arena *arena)
{
  arena_init(arena, sizeof(ScopeEntry), ARENA_CHUNK_COUNT, NULL);
}

Scope *
scope_create(Arena *arena, Scope *parent, size_t size)
{
  Scope *scope   = arena_alloc(arena);
  scope->entries = bo_htbl_new(sizeof(ScopeEntry *), size);
  scope->parent  = parent;
  return scope;
}

void
scope_insert(Scope *scope, uint64_t key, ScopeEntry *entry)
{
  assert(scope);
  assert(!bo_htbl_has_key(scope->entries, key) && "duplicate scope entry key!!!");
  bo_htbl_insert(scope->entries, key, entry);

#if VERBOSE
  {
    const char *file       = entry->src ? entry->src->unit->name : "IMPLICIT";
    const int   line       = entry->src ? entry->src->line : -1;
    const int   col        = entry->src ? entry->src->col : -1;
    const char *type_name  = entry->type->name;
    const char *is_buildin = entry->is_buildin ? "true" : "false";

    bl_log("scope: ADD INTO SCOPE (%p) <src: %s:%d:%d; type: '%s'; buildin: '%s'>", scope, file,
           line, col, type_name, is_buildin);
  }
#endif
}

ScopeEntry *
scope_lookup(Scope *scope, AstIdent *ident, bool in_tree)
{
  assert(scope);

  while (scope) {
    if (bo_htbl_has_key(scope->entries, ident->hash))
      return bo_htbl_at(scope->entries, ident->hash, ScopeEntry *);

    if (in_tree)
      scope = scope->parent;
    else
      break;
  }

  return NULL;
}

ScopeEntry *
scope_create_entry(Arena *arena)
{
  return arena_alloc(arena);
}
