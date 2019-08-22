//************************************************************************************************
// Biscuit Engine
//
// File:   assembly.h
// Author: Martin Dorazil
// Date:   02/03/2018
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

#ifndef BL_ASSEMBLY_BL
#define BL_ASSEMBLY_BL

#include "arena.h"
#include "scope.h"
#include "unit.h"
#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include <bobject/containers/list.h>
#include <dyncall.h>
#include <dynload.h>
#include <llvm-c/ExecutionEngine.h>

struct MirModule;
struct Builder;

typedef struct Assembly {
	BArray *    units;      /* array of all units in assembly */
	BHashTable *unit_cache; /* cache for loading only unique units */
	BHashTable *link_cache; /* all linked externals libraries passed to linker */
	BHashTable *type_table; /* type table key: type ID, value: *MirType */
	char *      name;       /* assembly name */
	Scope *     gscope;     /* global scope of the assembly */

	struct {
		Arena instr_arena;
		Arena type_arena;
		Arena var_arena;
		Arena fn_arena;
		Arena member_arena;
		Arena variant_arena;
		Arena value_arena;
		Arena array_arena;
		Arena small_array_arena;

		BArray *global_instrs; // All global instructions.
		BArray *RTTI_tmp_vars; // Temporary variables used by RTTI.
	} MIR;

	struct {
		LLVMModuleRef        module;       // LLVM Module.
		LLVMContextRef       cnt;          // LLVM Context.
		LLVMTargetDataRef    TD;           // LLVM Target data.
		LLVMTargetMachineRef TM;           // LLVM Machine.
		char *               triple;       // LLVM triple.
		LLVMMetadataRef      di_meta;      // LLVM Compile unit DI meta (optional)
		LLVMMetadataRef      di_file_meta; // LLVM dummy file for CU (optional)
	} llvm;

	/* DynCall/Lib data used for external method execution in compile time */
	struct {
		BArray *  lib_paths;
		BArray *  libs;
		DCCallVM *vm;
	} dl;
} Assembly;

typedef struct NativeLib {
	DLLib *       handle;
	struct Token *linked_from;
	const char *  user_name;
	char *        filename;
	char *        filepath;
	char *        dir;
	bool          is_internal;
} NativeLib;

Assembly *
assembly_new(struct Builder *builder, const char *name);

void
assembly_delete(Assembly *assembly);

void
assembly_add_unit(Assembly *assembly, Unit *unit);

void
assembly_add_link(Assembly *assembly, struct Token *token);

bool
assembly_add_unit_unique(Assembly *assembly, Unit *unit);

DCpointer
assembly_find_extern(Assembly *assembly, const char *symbol);

#endif
