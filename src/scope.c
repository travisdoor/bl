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
#include "arena.h"
#include "ast.h"
#include "common.h"
#include "unit.h"

#define ARENA_CHUNK_COUNT 256

static void scope_dtor(Scope *scope)
{
    BL_ASSERT(scope);
    thtbl_terminate(&scope->entries);
}

void scope_arenas_init(ScopeArenas *arenas)
{
    arena_init(&arenas->scopes, sizeof(Scope), ARENA_CHUNK_COUNT, (ArenaElemDtor)scope_dtor);
    arena_init(&arenas->entries, sizeof(ScopeEntry), ARENA_CHUNK_COUNT, NULL);
}

void scope_arenas_terminate(ScopeArenas *arenas)
{
    arena_terminate(&arenas->scopes);
    arena_terminate(&arenas->entries);
}

Scope *
scope_create(ScopeArenas *arenas, ScopeKind kind, Scope *parent, usize size, struct Location *loc)
{
    Scope *scope    = arena_alloc(&arenas->scopes);
    scope->parent   = parent;
    scope->kind     = kind;
    scope->location = loc;

    thtbl_init(&scope->entries, sizeof(ScopeEntry *), size);

    return scope;
}

ScopeEntry *scope_create_entry(ScopeArenas *  arenas,
                               ScopeEntryKind kind,
                               ID *           id,
                               struct Ast *   node,
                               bool           is_builtin)
{
    ScopeEntry *entry = arena_alloc(&arenas->entries);
    entry->id         = id;
    entry->kind       = kind;
    entry->node       = node;
    entry->is_builtin = is_builtin;
    return entry;
}

void scope_insert(Scope *scope, ScopeEntry *entry)
{
    BL_ASSERT(scope);
    BL_ASSERT(entry && entry->id);
    BL_ASSERT(!thtbl_has_key(&scope->entries, entry->id->hash) && "duplicate scope entry key!!!");
    entry->parent_scope = scope;
    thtbl_insert(&scope->entries, entry->id->hash, entry);
}

ScopeEntry *
scope_lookup(Scope *scope, ID *id, bool in_tree, bool ignore_global, bool *out_of_fn_local_scope)
{
    BL_ASSERT(scope && id);

    while (scope) {
        if (ignore_global && scope->kind == SCOPE_GLOBAL) break;
        if (thtbl_has_key(&scope->entries, id->hash))
            return thtbl_at(ScopeEntry *, &scope->entries, id->hash);

        if (in_tree) {
            if (out_of_fn_local_scope && scope->kind == SCOPE_FN_LOCAL) {
                *out_of_fn_local_scope = true;
            }

            scope = scope->parent;
        } else {
            break;
        }
    }

    return NULL;
}

bool scope_is_subtree_of_kind(const Scope *scope, ScopeKind kind)
{
    while (scope) {
        if (scope->kind == kind) return true;
        scope = scope->parent;
    }

    return false;
}

const char *scope_kind_name(const Scope *scope)
{
    if (!scope) return "<INVALID>";
    switch (scope->kind) {
    case SCOPE_GLOBAL:
        return "Global";
    case SCOPE_PRIVATE:
        return "Private";
    case SCOPE_FN:
        return "Function";
    case SCOPE_FN_LOCAL:
        return "LocalFunction";
    case SCOPE_LEXICAL:
        return "Lexical";
    case SCOPE_TYPE_STRUCT:
        return "Struct";
    case SCOPE_TYPE_ENUM:
        return "Enum";
    }

    return "<INVALID>";
}
