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
#include "llvm_di.h"
#include "mir.h"
#include "unit.h"
#include <string.h>

#define EXPECTED_GSCOPE_COUNT 4096
#define EXPECTED_ARRAY_COUNT 256
#define EXPECTED_UNIT_COUNT 512
#define EXPECTED_LINK_COUNT 32

union _SmallArrays {
	TSmallArray_TypePtr       type;
	TSmallArray_MemberPtr     member;
	TSmallArray_VariantPtr    variant;
	TSmallArray_InstrPtr      instr;
	TSmallArray_ConstValuePtr cv;
	TSmallArray_AstPtr        ast;
	TSmallArray_ArgPtr        arg;
};

static void
tarray_dtor(TArray **arr)
{
	tarray_delete(*arr);
}

static void
small_array_dtor(TSmallArrayAny *arr)
{
	tsa_terminate(arr);
}

static void
init_dl(Assembly *assembly)
{
	tarray_init(&assembly->dl.libs, sizeof(NativeLib));
	tarray_init(&assembly->dl.lib_paths, sizeof(char *));

	DCCallVM *vm = dcNewCallVM(4096);
	dcMode(vm, DC_CALL_C_DEFAULT);
	assembly->dl.vm = vm;
}

static void
init_DI(Assembly *assembly)
{
	const char *  producer    = "blc version " BL_VERSION;
	Scope *       gscope      = assembly->gscope;
	LLVMModuleRef llvm_module = assembly->llvm.module;

	/* setup module flags for debug */
	llvm_add_module_flag_int(llvm_module,
	                         LLVMModuleFlagBehaviorWarning,
	                         "Debug Info Version",
	                         llvm_get_dwarf_version());

	/* create DI builder */
	assembly->llvm.di_builder = llvm_di_new_di_builder(llvm_module);

	/* create dummy file used as DI global scope */
	// gscope->llvm_di_meta = llvm_di_create_file(assembly->llvm.di_builder, assembly->name,
	// ".");
	gscope->llvm_di_meta = NULL;
	LLVMMetadataRef llvm_dummy_file_meta =
	    llvm_di_create_file(assembly->llvm.di_builder, assembly->name, ".");

	/* create main compile unit */
	assembly->llvm.di_meta =
	    llvm_di_create_compile_unit(assembly->llvm.di_builder, llvm_dummy_file_meta, producer);
}

static void
init_llvm(Assembly *assembly)
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
		BL_ABORT("cannot get target");
	}

	LLVMContextRef llvm_context = LLVMContextCreate();
	LLVMModuleRef llvm_module = LLVMModuleCreateWithNameInContext(assembly->name, llvm_context);

	LLVMTargetMachineRef llvm_tm = LLVMCreateTargetMachine(llvm_target,
	                                                       triple,
	                                                       cpu,
	                                                       features,
	                                                       builder.options.opt_level,
	                                                       LLVMRelocDefault,
	                                                       LLVMCodeModelDefault);

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
	mir_arenas_init(&assembly->arenas.mir);
	tarray_init(&assembly->MIR.global_instrs, sizeof(MirInstr *));
	tarray_init(&assembly->MIR.RTTI_var_queue, sizeof(MirVar *));
	thtbl_init(&assembly->MIR.RTTI_table, sizeof(MirVar *), 2048);
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
	for (usize i = 0; i < assembly->dl.libs.size; ++i) {
		lib = &tarray_at(NativeLib, &assembly->dl.libs, i);
		native_lib_terminate(lib);
	}

	char *p;
	TARRAY_FOREACH(char *, &assembly->dl.lib_paths, p) free(p);

	dcFree(assembly->dl.vm);
	tarray_terminate(&assembly->dl.libs);
	tarray_terminate(&assembly->dl.lib_paths);
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
terminate_DI(Assembly *assembly)
{
	llvm_di_delete_di_builder(assembly->llvm.di_builder);
}

static void
terminate_mir(Assembly *assembly)
{
	thtbl_terminate(&assembly->MIR.RTTI_table);
	tarray_terminate(&assembly->MIR.global_instrs);
	tarray_terminate(&assembly->MIR.RTTI_var_queue);

	mir_arenas_terminate(&assembly->arenas.mir);
}

/* public */
Assembly *
assembly_new(const char *name)
{
	Assembly *assembly = bl_calloc(1, sizeof(Assembly));
	if (!assembly) BL_ABORT("bad alloc");
	assembly->name = strdup(name);
	tarray_init(&assembly->units, sizeof(Unit *));
	thtbl_init(&assembly->unit_cache, 0, EXPECTED_UNIT_COUNT);
	thtbl_init(&assembly->link_cache, sizeof(Token *), EXPECTED_LINK_COUNT);

	scope_arenas_init(&assembly->arenas.scope);
	ast_arena_init(&assembly->arenas.ast);
	arena_init(&assembly->arenas.array,
	           sizeof(TArray *),
	           EXPECTED_ARRAY_COUNT,
	           (ArenaElemDtor)tarray_dtor);
	arena_init(&assembly->arenas.small_array,
	           sizeof(union _SmallArrays),
	           EXPECTED_ARRAY_COUNT,
	           (ArenaElemDtor)small_array_dtor);

	assembly->gscope =
	    scope_create(&assembly->arenas.scope, SCOPE_GLOBAL, NULL, EXPECTED_GSCOPE_COUNT, NULL);

	init_dl(assembly);
	init_mir(assembly);

	init_llvm(assembly);
	if (builder.options.debug_build) init_DI(assembly);

	return assembly;
}

void
assembly_delete(Assembly *assembly)
{
	free(assembly->name);

	Unit *unit;
	TARRAY_FOREACH(Unit *, &assembly->units, unit)
	{
		unit_delete(unit);
	}

	terminate_DI(assembly);

	arena_terminate(&assembly->arenas.small_array);
	arena_terminate(&assembly->arenas.array);
	ast_arena_terminate(&assembly->arenas.ast);
	scope_arenas_terminate(&assembly->arenas.scope);

	tarray_terminate(&assembly->units);
	thtbl_terminate(&assembly->unit_cache);
	thtbl_terminate(&assembly->link_cache);
	terminate_dl(assembly);
	terminate_mir(assembly);
	terminate_llvm(assembly);
	bl_free(assembly);
}

void
assembly_add_unit(Assembly *assembly, Unit *unit)
{
	tarray_push(&assembly->units, unit);
}

bool
assembly_add_unit_unique(Assembly *assembly, Unit *unit)
{
	u64 hash = 0;
	if (unit->filepath)
		hash = thash_from_str(unit->filepath);
	else
		hash = thash_from_str(unit->name);

	if (thtbl_has_key(&assembly->unit_cache, hash)) return false;

	thtbl_insert_empty(&assembly->unit_cache, hash);
	assembly_add_unit(assembly, unit);
	return true;
}

void
assembly_add_link(Assembly *assembly, Token *token)
{
	if (!token) return;

	BL_ASSERT(token->sym == SYM_STRING);

	u64 hash = thash_from_str(token->value.str);
	if (thtbl_has_key(&assembly->link_cache, hash)) return;

	thtbl_insert(&assembly->link_cache, hash, token);
}

DCpointer
assembly_find_extern(Assembly *assembly, const char *symbol)
{
	void *     handle = NULL;
	NativeLib *lib;

	for (usize i = 0; i < assembly->dl.libs.size; ++i) {
		lib    = &tarray_at(NativeLib, &assembly->dl.libs, i);
		handle = dlFindSymbol(lib->handle, symbol);
		if (handle) break;
	}

	return handle;
}
