// =================================================================================================
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
// =================================================================================================

#ifndef BL_SCOPE_H
#define BL_SCOPE_H

#include "arena.h"
#include "llvm_api.h"

struct location;
struct ast;
struct mir_instr;
struct mir_type;
struct mir_fn;
struct mir_var;

struct scope_arenas {
    Arena scopes;
    Arena entries;
};

enum scope_entry_kind {
    SCOPE_ENTRY_INCOMPLETE,
    SCOPE_ENTRY_TYPE,
    SCOPE_ENTRY_VAR,
    SCOPE_ENTRY_FN,
    SCOPE_ENTRY_MEMBER,
    SCOPE_ENTRY_VARIANT,
    SCOPE_ENTRY_NAMED_SCOPE,
    SCOPE_ENTRY_VOID, // Special kind used for unnamed entries.
};

union scope_entry_data {
    struct mir_type *   type;
    struct mir_fn *     fn;
    struct mir_var *    var;
    struct mir_member * member;
    struct mir_variant *variant;
    struct scope *      scope;
};

struct scope_entry {
    ID *                   id;
    enum scope_entry_kind  kind;
    struct scope *         parent_scope;
    struct ast *           node;
    bool                   is_builtin;
    s32                    ref_count;
    union scope_entry_data data;
    BL_MAGIC_ADD
};

enum scope_kind {
    SCOPE_GLOBAL,
    SCOPE_PRIVATE,
    SCOPE_FN,
    SCOPE_FN_LOCAL,
    SCOPE_LEXICAL,
    SCOPE_TYPE_STRUCT,
    SCOPE_TYPE_ENUM,
    SCOPE_NAMED,
};

#define SCOPE_DEFAULT_LAYER 0

struct scope_layer {
    THashTable entries;
    s32        index;
};

struct scope {
    enum scope_kind kind;
    const char *    name; // optional
    struct scope *  parent;
    usize           expected_entry_count;
    TArray          layers;

    struct scope_sync_impl *sync;
    struct location *       location;
    LLVMMetadataRef         llvm_meta;
    BL_MAGIC_ADD
};

void scope_arenas_init(struct scope_arenas *arenas);
void scope_arenas_terminate(struct scope_arenas *arenas);

#define scope_create(arenas, kind, parent, size, loc)                                              \
    _scope_create(arenas, kind, parent, size, loc, false);

#define scope_create_safe(arenas, kind, parent, size, loc)                                         \
    _scope_create(arenas, kind, parent, size, loc, true);

struct scope *_scope_create(struct scope_arenas *arenas,
                            enum scope_kind      kind,
                            struct scope *       parent,
                            usize                size,
                            struct location *    loc,
                            const bool           safe);

struct scope_entry *scope_create_entry(struct scope_arenas * arenas,
                                       enum scope_entry_kind kind,
                                       ID *                  id,
                                       struct ast *          node,
                                       bool                  is_builtin);

void scope_insert(struct scope *scope, s32 layer_index, struct scope_entry *entry);
void scope_lock(struct scope *scope);
void scope_unlock(struct scope *scope);

struct scope_entry *scope_lookup(struct scope *scope,
                                 s32           preferred_layer_index,
                                 ID *          id,
                                 bool          in_tree,
                                 bool          ignore_global,
                                 bool *        out_of_fn_local_scope);

// Checks whether passed scope is of kind or is nested in scope of kind.
bool        scope_is_subtree_of_kind(const struct scope *scope, enum scope_kind kind);
const char *scope_kind_name(const struct scope *scope);
void        scope_get_full_name(TString *dest, struct scope *scope);

static INLINE bool scope_is_local(const struct scope *scope)
{
    return scope->kind != SCOPE_GLOBAL && scope->kind != SCOPE_PRIVATE &&
           scope->kind != SCOPE_NAMED;
}

static INLINE struct scope_entry *scope_entry_ref(struct scope_entry *entry)
{
    BL_MAGIC_ASSERT(entry);
    ++entry->ref_count;
    return entry;
}

#endif
