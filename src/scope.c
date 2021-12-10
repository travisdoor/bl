// =================================================================================================
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
// =================================================================================================

#include "scope.h"
#include "stb_ds.h"
#if BL_PLATFORM_WIN
#include "winpthreads.h"
#else
#include <pthread.h>
#endif

#define entry_hash(id, layer) ((((u64)layer) << 32) | (u64)id)

typedef struct scope_sync_impl {
    pthread_mutex_t lock;
} scope_sync_impl;

static scope_sync_impl *sync_new(void)
{
    scope_sync_impl *impl = bmalloc(sizeof(scope_sync_impl));
    pthread_mutex_init(&impl->lock, NULL);
    return impl;
}

static void sync_delete(scope_sync_impl *impl)
{
    if (!impl) return;
    pthread_mutex_destroy(&impl->lock);
    bfree(impl);
}

static void scope_dtor(struct scope *scope)
{
    bmagic_assert(scope);
    hmfree(scope->entries);
    sync_delete(scope->sync);
}

void scope_arenas_init(struct scope_arenas *arenas)
{
    arena_init(&arenas->scopes,
               sizeof(struct scope),
               alignment_of(struct scope),
               256,
               (arena_elem_dtor_t)scope_dtor);
    arena_init(
        &arenas->entries, sizeof(struct scope_entry), alignment_of(struct scope_entry), 1024, NULL);
}

void scope_arenas_terminate(struct scope_arenas *arenas)
{
    arena_terminate(&arenas->scopes);
    arena_terminate(&arenas->entries);
}

struct scope *scope_create(struct scope_arenas *arenas,
                           enum scope_kind      kind,
                           struct scope        *parent,
                           struct location     *loc)
{
    struct scope *scope = arena_safe_alloc(&arenas->scopes);
    scope->parent       = parent;
    scope->kind         = kind;
    scope->location     = loc;

    // Global scopes must be thread safe!
    if (kind == SCOPE_GLOBAL) scope->sync = sync_new();
    bmagic_set(scope);
    return scope;
}

struct scope_entry *scope_create_entry(struct scope_arenas  *arenas,
                                       enum scope_entry_kind kind,
                                       struct id            *id,
                                       struct ast           *node,
                                       bool                  is_builtin)
{
    struct scope_entry *entry = arena_safe_alloc(&arenas->entries);
    entry->id                 = id;
    entry->kind               = kind;
    entry->node               = node;
    entry->is_builtin         = is_builtin;
    entry->ref_count          = 0;
    bmagic_set(entry);
    return entry;
}

void scope_insert(struct scope *scope, hash_t layer_index, struct scope_entry *entry)
{
    zone();
    bassert(scope);
    bassert(entry && entry->id);
    const u64 hash = entry_hash(entry->id->hash, layer_index);
    bassert(hmgeti(scope->entries, hash) == -1 && "Duplicate scope entry key!!!");
    entry->parent_scope = scope;
    hmput(scope->entries, hash, entry);
    return_zone();
}

struct scope_entry *scope_lookup(struct scope *scope,
                                 hash_t        preferred_layer,
                                 struct id    *id,
                                 bool          in_tree,
                                 bool          ignore_global,
                                 bool         *out_of_fn_local_scope)
{
    zone();
    bassert(scope && id);
    u64 hash = entry_hash(id->hash, preferred_layer);
    while (scope) {
        if (ignore_global && scope->kind == SCOPE_GLOBAL) break;
        if (!scope_is_local(scope)) {
            // Global scopes should not have layers!!!
            hash = entry_hash(id->hash, SCOPE_DEFAULT_LAYER);
        }
        const s64 i = hmgeti(scope->entries, hash);
        if (i != -1) { // found!!!
            struct scope_entry *entry = scope->entries[i].value;
            bassert(entry);
            return_zone(entry);
        }
        // Lookup in parent.
        if (in_tree) {
            if (out_of_fn_local_scope && scope->kind == SCOPE_FN_LOCAL) {
                *out_of_fn_local_scope = true;
            }
            scope = scope->parent;
        } else {
            break;
        }
    }
    return_zone(NULL);
}

void scope_dirty_clear_tree(struct scope *scope)
{
    bassert(scope);
}

void scope_lock(struct scope *scope)
{
    bassert(scope && scope->sync);
    pthread_mutex_lock(&scope->sync->lock);
}

void scope_unlock(struct scope *scope)
{
    bassert(scope && scope->sync);
    pthread_mutex_unlock(&scope->sync->lock);
}

bool scope_is_subtree_of_kind(const struct scope *scope, enum scope_kind kind)
{
    while (scope) {
        if (scope->kind == kind) return true;
        scope = scope->parent;
    }

    return false;
}

const char *scope_kind_name(const struct scope *scope)
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
    case SCOPE_NAMED:
        return "Named";
    }

    return "<INVALID>";
}

void scope_get_full_name(char **dest, struct scope *scope)
{
    bassert(dest && scope);
    sarr_t(char *, 8) tmp = SARR_ZERO;
    while (scope) {
        if (scope->name) sarrput(&tmp, (char *)scope->name);
        scope = scope->parent;
    }
    for (usize i = sarrlenu(&tmp); i-- > 0;) {
        const char *subname = sarrpeek(&tmp, i);
        if (i > 0) {
            strappend(*dest, "%s.", subname);
        } else {
            strappend(*dest, "%s", subname);
        }
    }
    sarrfree(&tmp);
}
