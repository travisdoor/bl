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
#include "arena.h"
#include "ast.h"
#include "common.h"
#include "unit.h"

#if BL_PLATFORM_WIN
#include "winpthreads.h"
#include <windows.h>
#else
#include <pthread.h>
#endif

typedef struct scope_sync_impl {
    pthread_mutex_t lock;
} scope_sync_impl;

static scope_sync_impl *sync_new(void)
{
    scope_sync_impl *impl = bl_malloc(sizeof(scope_sync_impl));
    pthread_mutex_init(&impl->lock, NULL);
    return impl;
}

static void sync_delete(scope_sync_impl *impl)
{
    if (!impl) return;
    pthread_mutex_destroy(&impl->lock);
    bl_free(impl);
}

static void layer_init(struct scope_layer *layer, s32 index, usize expected_entry_count)
{
    thtbl_init(&layer->entries, sizeof(struct scope_entry *), expected_entry_count);
    layer->index = index;
}

static void layer_terminate(struct scope_layer *layer)
{
    thtbl_terminate(&layer->entries);
}

static void scope_dtor(struct scope *scope)
{
    BL_ASSERT(scope);
    for (usize i = 0; i < scope->layers.size; ++i) {
        struct scope_layer *layer = &tarray_at(struct scope_layer, &scope->layers, i);
        layer_terminate(layer);
    }
    tarray_terminate(&scope->layers);
    sync_delete(scope->sync);
}

static INLINE THashTable *get_layer(struct scope *scope, s32 index)
{
    for (usize i = 0; i < scope->layers.size; ++i) {
        struct scope_layer *layer = &tarray_at(struct scope_layer, &scope->layers, i);
        if (layer->index == index) return &layer->entries;
    }
    return NULL;
}

// Only local scopes can have layers, for global ones, use only SCOPE_DEFAULT_LAYER index. Layered
// scopes are used due to polymorph function generation. We basicaly reuse already existing
// Ast tree for polymorphs of specified type; in this case we need new layer for every polymorph
// variant of function.
static INLINE THashTable *create_layer(struct scope *scope, s32 index)
{
    BL_ASSERT(index == SCOPE_DEFAULT_LAYER || scope_is_local(scope));
    BL_ASSERT(get_layer(scope, index) == NULL && "Attempt to create existing layer!");
    struct scope_layer *layer = tarray_push_empty(&scope->layers);
    layer_init(layer, index, scope->expected_entry_count);
    return &layer->entries;
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

struct scope *_scope_create(struct scope_arenas *arenas,
                            enum scope_kind      kind,
                            struct scope *       parent,
                            usize                size,
                            struct location *    loc,
                            const bool           safe)
{
    BL_ASSERT(size > 0);
    struct scope *scope         = arena_alloc(&arenas->scopes);
    scope->parent               = parent;
    scope->kind                 = kind;
    scope->location             = loc;
    scope->expected_entry_count = size;
    tarray_init(&scope->layers, sizeof(struct scope_layer));
    // For thread safe scope
    if (safe) scope->sync = sync_new();
    BL_MAGIC_SET(scope);
    return scope;
}

struct scope_entry *scope_create_entry(struct scope_arenas * arenas,
                                       enum scope_entry_kind kind,
                                       struct id *           id,
                                       struct ast *          node,
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
    BL_ASSERT(scope);
    BL_ASSERT(entry && entry->id);
    BL_ASSERT(layer_index >= 0);
    THashTable *entries = get_layer(scope, layer_index);
    if (!entries) entries = create_layer(scope, layer_index);
    BL_ASSERT(entries);
    BL_ASSERT(!thtbl_has_key(entries, entry->id->hash) && "Duplicate scope entry key!!!");
    entry->parent_scope = scope;
    thtbl_insert(entries, entry->id->hash, entry);
}

struct scope_entry *scope_lookup(struct scope *scope,
                                 s32           preferred_layer_index,
                                 struct id *   id,
                                 bool          in_tree,
                                 bool          ignore_global,
                                 bool *        out_of_fn_local_scope)
{
    ZONE();
    BL_ASSERT(scope && id);
    BL_ASSERT(preferred_layer_index >= 0);
    TIterator iter;
    while (scope) {
        if (ignore_global && scope->kind == SCOPE_GLOBAL) break;
        // Lookup in current scope first
        THashTable *entries = get_layer(scope, preferred_layer_index);
        // We can implicitly switch to default layer when scope is not local.
        if (!entries && !scope_is_local(scope)) entries = get_layer(scope, SCOPE_DEFAULT_LAYER);
        if (entries) {
            iter = thtbl_find(entries, id->hash);
            if (!TITERATOR_EQUAL(iter, thtbl_end(entries))) {
                RETURN_END_ZONE(thtbl_iter_peek_value(struct scope_entry *, iter));
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
    RETURN_END_ZONE(NULL);
}

void scope_dirty_clear_tree(struct scope *scope)
{
    BL_ASSERT(scope);
}

void scope_lock(struct scope *scope)
{
    BL_ASSERT(scope && scope->sync);
    pthread_mutex_lock(&scope->sync->lock);
}

void scope_unlock(struct scope *scope)
{
    BL_ASSERT(scope && scope->sync);
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
    BL_ASSERT(dest && scope);
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
