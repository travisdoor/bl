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
} ScopeEntryKind;

typedef union ScopeEntryData {
	struct MirType *   type;
	struct MirFn *     fn;
	struct MirVar *    var;
	struct MirMember * member;
	struct MirVariant *variant;
} ScopeEntryData;

typedef struct ScopeEntry {
	ID *           id;
	ScopeEntryKind kind;
	struct Scope * parent_scope;
	struct Ast *   node;
	bool           is_buildin;

	ScopeEntryData data;
} ScopeEntry;

typedef enum ScopeKind {
	SCOPE_GLOBAL,
	SCOPE_PRIVATE,
	SCOPE_FN,
	SCOPE_FN_LOCAL,
	SCOPE_LEXICAL,
	SCOPE_TYPE_STRUCT,
	SCOPE_TYPE_ENUM,
} ScopeKind;

typedef struct Scope {
	ScopeKind        kind;
	struct Scope *   parent;
	THashTable       entries;
	LLVMMetadataRef  llvm_di_meta; /* Optional ID data*/
	struct Location *location;     /* Optional scope start location in the source file (ex.:
	                                  function body  starting with '{'). Note: global scope has no
	                                  location data. */
} Scope;

void
scope_arenas_init(ScopeArenas *arenas);

void
scope_arenas_terminate(ScopeArenas *arenas);

Scope *
scope_create(ScopeArenas *arenas, ScopeKind kind, Scope *parent, usize size, struct Location *loc);

ScopeEntry *
scope_create_entry(ScopeArenas *  arenas,
                   ScopeEntryKind kind,
                   ID *           id,
                   struct Ast *   node,
                   bool           is_buildin);

void
scope_insert(Scope *scope, ScopeEntry *entry);

ScopeEntry *
scope_lookup(Scope *scope, ID *id, bool in_tree, bool ignore_gscope, bool *out_of_fn_local_scope);

bool
scope_is_subtree_of_kind(Scope *scope, ScopeKind kind);

#endif
