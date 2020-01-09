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
	TSmallArray_SwitchCase    switch_case;
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
	DCCallVM *vm = dcNewCallVM(4096);
	dcMode(vm, DC_CALL_C_DEFAULT);
	assembly->dc_vm = vm;
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
	if (assembly->llvm.module) BL_ABORT("Attempt to override assembly options.");

	/* init LLVM */
	char *triple    = LLVMGetDefaultTargetTriple();
	char *cpu       = /*LLVMGetHostCPUName()*/ "";
	char *features  = /*LLVMGetHostCPUFeatures()*/ "";
	char *error_msg = NULL;

	// msg_log("Target: %s", triple);

	LLVMTargetRef llvm_target = NULL;
	if (LLVMGetTargetFromTriple(triple, &llvm_target, &error_msg)) {
		msg_error("Cannot get target with error: %s!", error_msg);
		LLVMDisposeMessage(error_msg);
		BL_ABORT("Cannot get target");
	}

	LLVMContextRef llvm_context = LLVMContextCreate();
	LLVMModuleRef llvm_module = LLVMModuleCreateWithNameInContext(assembly->name, llvm_context);

	LLVMTargetMachineRef llvm_tm =
	    LLVMCreateTargetMachine(llvm_target,
	                            triple,
	                            cpu,
	                            features,
	                            get_opt_level_for_build_mode(assembly->options.build_mode),
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
	dcFree(assembly->dc_vm);
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

	mir_arenas_terminate(&assembly->arenas.mir);
}

static void
set_default_out_dir(Assembly *assembly)
{
	char path[PATH_MAX] = {0};
	get_current_working_dir(&path, PATH_MAX);

	tstring_clear(&assembly->options.out_dir);
	tstring_append(&assembly->options.out_dir, path);
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
	tstring_init(&assembly->options.custom_linker_opt);
	tstring_init(&assembly->options.out_dir);
	tarray_init(&assembly->options.libs, sizeof(NativeLib));
	tarray_init(&assembly->options.lib_paths, sizeof(char *));
	vm_init(&assembly->vm, VM_STACK_SIZE);

	// set defaults
	assembly->options.build_mode = builder.options.build_mode;
	set_default_out_dir(assembly);

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

	NativeLib *lib;
	for (usize i = 0; i < assembly->options.libs.size; ++i) {
		lib = &tarray_at(NativeLib, &assembly->options.libs, i);
		native_lib_terminate(lib);
	}

	char *p;
	TARRAY_FOREACH(char *, &assembly->options.lib_paths, p) free(p);

	tarray_terminate(&assembly->options.libs);
	tarray_terminate(&assembly->options.lib_paths);
	tstring_terminate(&assembly->options.custom_linker_opt);
	tstring_terminate(&assembly->options.out_dir);

	vm_terminate(&assembly->vm);

	arena_terminate(&assembly->arenas.small_array);
	arena_terminate(&assembly->arenas.array);
	ast_arena_terminate(&assembly->arenas.ast);
	scope_arenas_terminate(&assembly->arenas.scope);

	tarray_terminate(&assembly->units);
	thtbl_terminate(&assembly->unit_cache);
	terminate_dl(assembly);
	terminate_mir(assembly);
	terminate_llvm(assembly);
	bl_free(assembly);
}

void
assembly_apply_options(Assembly *assembly)
{
	init_llvm(assembly);
	if (assembly->options.build_mode == BUILD_MODE_DEBUG) init_DI(assembly);
}

void
assembly_set_output_dir(Assembly *assembly, const char *dir)
{
	if (!dir) msg_error("Cannot create output directory.");

#ifdef BL_PLATFORM_WIN
	win_fix_path(dir, strlen(dir));
#endif

	if (!dir_exists(dir)) {
		if (!create_dir_tree(dir)) {
			msg_error("Cannot create output directory '%s'.", dir);
			return;
		}
	}

	char path[PATH_MAX] = {0};
	brealpath(dir, path, PATH_MAX);

	tstring_clear(&assembly->options.out_dir);
	tstring_append(&assembly->options.out_dir, path);
}

AssemblyOptions
assembly_get_default_options(void);

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
assembly_add_native_lib(Assembly *assembly, const char *lib_name, struct Token *link_token)
{
	const u64 hash = thash_from_str(lib_name);

	{ /* Search for duplicity. */
		NativeLib *lib;
		for (usize i = 0; i < assembly->options.libs.size; ++i) {
			lib = &tarray_at(NativeLib, &assembly->options.libs, i);
			if (lib->hash == hash) return;
		}
	}

	NativeLib lib   = {0};
	lib.hash        = hash;
	lib.user_name   = strdup(lib_name);
	lib.linked_from = link_token;

	tarray_push(&assembly->options.libs, lib);
}

DCpointer
assembly_find_extern(Assembly *assembly, const char *symbol)
{
	void *     handle = NULL;
	NativeLib *lib;

	for (usize i = 0; i < assembly->options.libs.size; ++i) {
		lib    = &tarray_at(NativeLib, &assembly->options.libs, i);
		handle = dlFindSymbol(lib->handle, symbol);
		if (handle) break;
	}

	return handle;
}
