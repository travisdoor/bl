//************************************************************************************************
// blc
//
// File:   assembly.c
// Author: Martin Dorazil
// Date:   09/02/2018
//
// Copyright 2017 Martin Dorazil
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

#include "assembly.h"
#include "blmemory.h"
#include "builder.h"
#include "mir.h"
#include "unit.h"
#include <bobject/containers/hash.h>
#include <string.h>

#define EXPECTED_GSCOPE_COUNT 4096
#define EXPECTED_UNIT_COUNT 512
#define EXPECTED_LINK_COUNT 32

static void
init_dl(Assembly *assembly)
{
	assembly->dl.libs      = bo_array_new(sizeof(NativeLib));
	assembly->dl.lib_paths = bo_array_new(sizeof(char *));

	DCCallVM *vm = dcNewCallVM(4096);
	dcMode(vm, DC_CALL_C_DEFAULT);
	assembly->dl.vm = vm;
}

static void
init_llvm(Assembly *assembly, Builder *builder)
{
	/* init LLVM */
	char *triple    = LLVMGetDefaultTargetTriple();
	char *cpu       = /*LLVMGetHostCPUName()*/ "";
	char *features  = /*LLVMGetHostCPUFeatures()*/ "";
	char *error_msg = NULL;

	msg_log("Target: %s", triple);

	LLVMTargetRef llvm_target = NULL;
	if (LLVMGetTargetFromTriple(triple, &llvm_target, &error_msg)) {
		msg_error("cannot get target with error: %s", error_msg);
		LLVMDisposeMessage(error_msg);
		abort();
	}

	LLVMCodeGenOptLevel opt_lvl = LLVMCodeGenLevelDefault;
	if (is_not_flag(builder->flags, BUILDER_DEBUG_BUILD)) opt_lvl = LLVMCodeGenLevelAggressive;

	LLVMContextRef llvm_context = LLVMContextCreate();
	LLVMModuleRef llvm_module = LLVMModuleCreateWithNameInContext(assembly->name, llvm_context);

	LLVMTargetMachineRef llvm_tm = LLVMCreateTargetMachine(
	    llvm_target, triple, cpu, features, opt_lvl, LLVMRelocDefault, LLVMCodeModelDefault);

	LLVMTargetDataRef llvm_td = LLVMCreateTargetDataLayout(llvm_tm);
	LLVMSetModuleDataLayout(llvm_module, llvm_td);
	LLVMSetTarget(llvm_module, triple);

	assembly->llvm.cnt    = llvm_context;
	assembly->llvm.module = llvm_module;
	assembly->llvm.TM     = llvm_tm;
	assembly->llvm.TD     = llvm_td;
	assembly->llvm.triple = triple;
}

static void
init_mir(Assembly *assembly)
{
	mir_arenas_init(assembly);
	assembly->MIR.global_instrs = bo_array_new(sizeof(MirInstr *));
	assembly->MIR.RTTI_tmp_vars = bo_array_new(sizeof(MirVar *));
}

static void
native_lib_terminate(NativeLib *lib)
{
	if (lib->handle) dlFreeLibrary(lib->handle);
	free(lib->filename);
	free(lib->filepath);
	free(lib->dir);
}

static void
terminate_dl(Assembly *assembly)
{
	NativeLib *lib;
	for (size_t i = 0; i < bo_array_size(assembly->dl.libs); ++i) {
		lib = &bo_array_at(assembly->dl.libs, i, NativeLib);
		native_lib_terminate(lib);
	}

	char *p;
	barray_foreach(assembly->dl.lib_paths, p) free(p);

	dcFree(assembly->dl.vm);
	bo_unref(assembly->dl.libs);
	bo_unref(assembly->dl.lib_paths);
}

static void
terminate_llvm(Assembly *assembly)
{
	LLVMDisposeModule(assembly->llvm.module);
	LLVMDisposeTargetMachine(assembly->llvm.TM);
	LLVMDisposeMessage(assembly->llvm.triple);
	LLVMDisposeTargetData(assembly->llvm.TD);
	LLVMContextDispose(assembly->llvm.cnt);
}

static void
terminate_mir(Assembly *assembly)
{
	bo_unref(assembly->MIR.global_instrs);
	bo_unref(assembly->MIR.RTTI_tmp_vars);

	mir_arenas_terminate(assembly);
}

/* public */
Assembly *
assembly_new(Builder *builder, const char *name)
{
	Assembly *assembly = bl_calloc(1, sizeof(Assembly));
	if (!assembly) bl_abort("bad alloc");
	assembly->name       = strdup(name);
	assembly->units      = bo_array_new(sizeof(Unit *));
	assembly->unit_cache = bo_htbl_new(0, EXPECTED_UNIT_COUNT);
	assembly->link_cache = bo_htbl_new(sizeof(Token *), EXPECTED_LINK_COUNT);
	assembly->type_table = bo_htbl_new(sizeof(MirType *), 8192);
	assembly->gscope     = scope_new(SCOPE_GLOBAL, NULL, EXPECTED_GSCOPE_COUNT, NULL);

	bo_array_reserve(assembly->units, EXPECTED_UNIT_COUNT);
	scope_entry_arena_init(&assembly->scope_entry_arena);

	init_dl(assembly);
	init_mir(assembly);
	init_llvm(assembly, builder);

	return assembly;
}

void
assembly_delete(Assembly *assembly)
{
	free(assembly->name);

	Unit *unit;
	barray_foreach(assembly->units, unit)
	{
		unit_delete(unit);
	}

	scope_delete(assembly->gscope);
	scope_entry_arena_terminate(&assembly->scope_entry_arena);

	bo_unref(assembly->units);
	bo_unref(assembly->unit_cache);
	bo_unref(assembly->link_cache);
	bo_unref(assembly->type_table);

	terminate_dl(assembly);
	terminate_mir(assembly);
	terminate_llvm(assembly);
	bl_free(assembly);
}

void
assembly_add_unit(Assembly *assembly, Unit *unit)
{
	bo_array_push_back(assembly->units, unit);
}

bool
assembly_add_unit_unique(Assembly *assembly, Unit *unit)
{
	uint64_t hash = 0;
	if (unit->filepath)
		hash = bo_hash_from_str(unit->filepath);
	else
		hash = bo_hash_from_str(unit->name);

	if (bo_htbl_has_key(assembly->unit_cache, hash)) return false;

	bo_htbl_insert_empty(assembly->unit_cache, hash);
	assembly_add_unit(assembly, unit);
	return true;
}

void
assembly_add_link(Assembly *assembly, Token *token)
{
	if (!token) return;

	assert(token->sym == SYM_STRING);

	uint64_t hash = bo_hash_from_str(token->value.str);
	if (bo_htbl_has_key(assembly->link_cache, hash)) return;

	bo_htbl_insert(assembly->link_cache, hash, token);
}

DCpointer
assembly_find_extern(Assembly *assembly, const char *symbol)
{
	void *       handle = NULL;
	NativeLib *  lib;
	const size_t count = bo_array_size(assembly->dl.libs);

	for (size_t i = 0; i < count; ++i) {
		lib    = &bo_array_at(assembly->dl.libs, i, NativeLib);
		handle = dlFindSymbol(lib->handle, symbol);
		if (handle) break;
	}

	return handle;
}
