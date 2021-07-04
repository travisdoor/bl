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

struct Location;
struct Ast;
struct MirInstr;
struct MirType;
struct MirFn;
struct MirVar;

typedef struct ScopeArenas {
    Arena scopes;
    Arena entries;
} ScopeArenas;

typedef enum ScopeEntryKind {
    SCOPE_ENTRY_INCOMPLETE,
    SCOPE_ENTRY_TYPE,
    SCOPE_ENTRY_VAR,
    SCOPE_ENTRY_FN,
    SCOPE_ENTRY_MEMBER,
    SCOPE_ENTRY_VARIANT,
    SCOPE_ENTRY_NAMED_SCOPE,
    SCOPE_ENTRY_VOID, // Special kind used for unnamed entries.
} ScopeEntryKind;

typedef union ScopeEntryData {
    struct MirType *   type;
    struct MirFn *     fn;
    struct MirVar *    var;
    struct MirMember * member;
    struct MirVariant *variant;
    struct Scope *     scope;
} ScopeEntryData;

typedef struct ScopeEntry {
    ID *           id;
    ScopeEntryKind kind;
    struct Scope * parent_scope;
    struct Ast *   node;
    bool           is_builtin;
    s32            ref_count;

    ScopeEntryData data;

    BL_MAGIC_ADD
} ScopeEntry;

typedef enum ScopeKind {
    SCOPE_GLOBAL,
    SCOPE_PRIVATE,
    SCOPE_FN,
    SCOPE_FN_LOCAL,
    SCOPE_LEXICAL,
    SCOPE_TYPE_STRUCT,
    SCOPE_TYPE_ENUM,
    SCOPE_NAMED,
} ScopeKind;

#define SCOPE_DEFAULT_LAYER 0

struct ScopeLayer {
    THashTable entries;
    s32        index;
};

typedef struct Scope {
    ScopeKind     kind;
    const char *  name; // optional
    struct Scope *parent;
    usize         expected_entry_count;
    TArray        layers;

    struct ScopeSyncImpl *sync;
    struct Location *     location;
    LLVMMetadataRef       llvm_meta;
    BL_MAGIC_ADD
} Scope;

void scope_arenas_init(ScopeArenas *arenas);
void scope_arenas_terminate(ScopeArenas *arenas);

#define scope_create(arenas, kind, parent, size, loc)                                              \
    _scope_create(arenas, kind, parent, size, loc, false);

#define scope_create_safe(arenas, kind, parent, size, loc)                                         \
    _scope_create(arenas, kind, parent, size, loc, true);

Scope *_scope_create(ScopeArenas *    arenas,
                     ScopeKind        kind,
                     Scope *          parent,
                     usize            size,
                     struct Location *loc,
                     const bool       safe);

ScopeEntry *scope_create_entry(ScopeArenas *  arenas,
                               ScopeEntryKind kind,
                               ID *           id,
                               struct Ast *   node,
                               bool           is_builtin);

void scope_insert(Scope *scope, s32 layer_index, ScopeEntry *entry);
void scope_lock(Scope *scope);
void scope_unlock(Scope *scope);

ScopeEntry *scope_lookup(Scope *scope,
                         s32    preferred_layer_index,
                         ID *   id,
                         bool   in_tree,
                         bool   ignore_global,
                         bool * out_of_fn_local_scope);

// Checks whether passed scope is of kind or is nested in scope of kind.
bool        scope_is_subtree_of_kind(const Scope *scope, ScopeKind kind);
const char *scope_kind_name(const Scope *scope);
void        scope_get_full_name(TString *dest, Scope *scope);

static INLINE bool scope_is_local(const Scope *scope)
{
    return scope->kind != SCOPE_GLOBAL && scope->kind != SCOPE_PRIVATE &&
           scope->kind != SCOPE_NAMED;
}

static INLINE ScopeEntry *scope_entry_ref(ScopeEntry *entry)
{
    BL_MAGIC_ASSERT(entry);
    ++entry->ref_count;
    return entry;
}

#endif
