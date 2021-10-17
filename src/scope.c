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
    BL_MAGIC_ASSERT(scope);
    for (s64 i = 0; i < arrlen(scope->layers); ++i) {
        hmfree(scope->layers[i].entries);
    }
    arrfree(scope->layers);
    hmfree(scope->default_layer.entries);
    sync_delete(scope->sync);
}

static INLINE struct scope_layer *get_layer(struct scope *scope, s32 index)
{
    if (index == SCOPE_DEFAULT_LAYER) return &scope->default_layer;
    for (s64 i = 0; i < arrlen(scope->layers); ++i) {
        if (scope->layers[i].index == index) return &scope->layers[i];
    }
    return NULL;
}

// Only local scopes can have layers, for global ones, use only SCOPE_DEFAULT_LAYER index. Layered
// scopes are used due to polymorph function generation. We basically reuse already existing
// Ast tree for polymorphs of specified type; in this case we need new layer for every polymorph
// variant of function.
static INLINE struct scope_layer *create_layer(struct scope *scope, s32 index)
{
    bassert(index == SCOPE_DEFAULT_LAYER || scope_is_local(scope));
    bassert(get_layer(scope, index) == NULL && "Attempt to create existing layer!");
    arrput(scope->layers, ((struct scope_layer){.entries = NULL, .index = index}));
    return &arrlast(scope->layers);
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
                           u32                  expected_entry_count,
                           struct location     *loc)
{
    bassert(expected_entry_count > 0);
    struct scope *scope         = arena_alloc(&arenas->scopes);
    scope->parent               = parent;
    scope->kind                 = kind;
    scope->location             = loc;
    scope->expected_entry_count = expected_entry_count;

    // Global scopes must be thread safe!
    if (kind == SCOPE_GLOBAL) scope->sync = sync_new();
    BL_MAGIC_SET(scope);
    return scope;
}

struct scope_entry *scope_create_entry(struct scope_arenas  *arenas,
                                       enum scope_entry_kind kind,
                                       struct id            *id,
                                       struct ast           *node,
                                       bool                  is_builtin)
{
    struct scope_entry *entry = arena_alloc(&arenas->entries);
    entry->id                 = id;
    entry->kind               = kind;
    entry->node               = node;
    entry->is_builtin         = is_builtin;
    entry->ref_count          = 0;
    BL_MAGIC_SET(entry);
    return entry;
}

void scope_insert(struct scope *scope, s32 layer_index, struct scope_entry *entry)
{
    bassert(scope);
    bassert(entry && entry->id);
    bassert(layer_index >= 0);
    struct scope_layer *layer = get_layer(scope, layer_index);
    if (!layer) layer = create_layer(scope, layer_index);
    bassert(layer);
    bassert(hmgeti(layer->entries, entry->id->hash) == -1 && "Duplicate scope entry key!!!");
    entry->parent_scope = scope;
    hmput(layer->entries, entry->id->hash, entry);
}

struct scope_entry *scope_lookup(struct scope *scope,
                                 s32           preferred_layer_index,
                                 struct id    *id,
                                 bool          in_tree,
                                 bool          ignore_global,
                                 bool         *out_of_fn_local_scope)
{
    zone();
    bassert(scope && id);
    bassert(preferred_layer_index >= 0);
    while (scope) {
        if (ignore_global && scope->kind == SCOPE_GLOBAL) break;
        // Lookup in current scope first
        struct scope_layer *layer = get_layer(scope, preferred_layer_index);
        // We can implicitly switch to default layer when scope is not local.
        if (!layer && !scope_is_local(scope)) layer = get_layer(scope, SCOPE_DEFAULT_LAYER);
        if (layer) {
            const s64 i = hmgeti(layer->entries, id->hash);
            if (i != -1) {
                return_zone(layer->entries[i].value);
            }
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

void scope_get_full_name(TString *dest, struct scope *scope)
{
    bassert(dest && scope);
    TSmallArray_CharPtr buffer;
    tsa_init(&buffer);
    while (scope) {
        if (scope->name) tsa_push_CharPtr(&buffer, (char *)scope->name);
        scope = scope->parent;
    }
    for (usize i = buffer.size; i-- > 0;) {
        char *iter = buffer.data[i];
        tstring_append(dest, iter);
        if (i > 0) tstring_append_c(dest, '.');
    }
    tsa_terminate(&buffer);
}
