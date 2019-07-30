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
#include <assert.h>
#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>

struct Src;
struct Ast;
struct MirInstr;
struct MirType;
struct MirFn;
struct MirVar;

typedef enum ScopeEntryKind {
	SCOPE_ENTRY_INCOMPLETE,
	SCOPE_ENTRY_TYPE,
	SCOPE_ENTRY_VAR,
	SCOPE_ENTRY_FN,
	SCOPE_ENTRY_MEMBER,
	SCOPE_ENTRY_VARIANT,
} ScopeEntryKind;

typedef struct ScopeArenas {
	Arena scope_arena;
	Arena entry_arena;
} ScopeArenas;

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

typedef struct Scope {
	struct Scope *parent;
	BHashTable *  entries;
	bool          is_global;
} Scope;

void
scope_arenas_init(ScopeArenas *arenas);

void
scope_arenas_terminate(ScopeArenas *arenas);

Scope *
scope_create(ScopeArenas *arenas, Scope *parent, size_t size, bool is_global);

ScopeEntry *
scope_create_entry(ScopeArenas *  arenas,
                   ScopeEntryKind kind,
                   ID *           id,
                   struct Ast *   node,
                   bool           is_buildin);

void
scope_insert(Scope *scope, ScopeEntry *entry);

ScopeEntry *
scope_lookup(Scope *scope, ID *id, bool in_tree);

#endif
