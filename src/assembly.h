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
#include "mir.h"
#include "scope.h"
#include "unit.h"
#include <dyncall.h>
#include <dynload.h>

struct MirModule;
struct Builder;

typedef struct Assembly {
	struct {
		ScopeArenas scope;
		MirArenas   mir;
		Arena       ast;
		Arena       array;       /* used for all TArrays */
		Arena       small_array; /* used for all SmallArrays */
	} arenas;

	struct {
		TArray global_instrs; /* All global instructions. */

		/* Map type ids to RTTI variables. */
		THashTable RTTI_table;
	} MIR;

	struct {
		LLVMModuleRef        module;     /* LLVM Module. */
		LLVMContextRef       cnt;        /* LLVM Context. */
		LLVMTargetDataRef    TD;         /* LLVM Target data. */
		LLVMTargetMachineRef TM;         /* LLVM Machine. */
		char *               triple;     /* LLVM triple. */
		LLVMMetadataRef      di_meta;    /* LLVM Compile unit DI meta (optional) */
		LLVMDIBuilderRef     di_builder; /* LLVM debug information builder */
	} llvm;

	/* DynCall/Lib data used for external method execution in compile time */
	struct {
		TArray    lib_paths;
		TArray    libs;
		DCCallVM *vm;
	} dl;

	TArray     units;          /* array of all units in assembly */
	THashTable unit_cache;     /* cache for loading only unique units */
	char *     name;           /* assembly name */
	Scope *    gscope;         /* global scope of the assembly */
	bool       is_build_entry; /* Set for builder assembly when custom build is used. */
	MirFn *    build_entry;    /* Set for build assembly. */

	/* Builtins */
	struct BuiltinTypes {
		MirType *t_type;
		MirType *t_s8;
		MirType *t_s16;
		MirType *t_s32;
		MirType *t_s64;
		MirType *t_u8;
		MirType *t_u16;
		MirType *t_u32;
		MirType *t_u64;
		MirType *t_usize;
		MirType *t_bool;
		MirType *t_f32;
		MirType *t_f64;
		MirType *t_string;
		MirType *t_void;
		MirType *t_u8_ptr;
		MirType *t_string_ptr;
		MirType *t_string_slice;
		MirType *t_resolve_type_fn;
		MirType *t_test_case_fn;
		MirType *t_Any;
		MirType *t_Any_ptr;
		MirType *t_TypeKind;
		MirType *t_TypeInfo;
		MirType *t_TypeInfoType;
		MirType *t_TypeInfoVoid;
		MirType *t_TypeInfoInt;
		MirType *t_TypeInfoReal;
		MirType *t_TypeInfoFn;
		MirType *t_TypeInfoPtr;
		MirType *t_TypeInfoArray;
		MirType *t_TypeInfoStruct;
		MirType *t_TypeInfoEnum;
		MirType *t_TypeInfoNull;
		MirType *t_TypeInfoBool;
		MirType *t_TypeInfoString;
		MirType *t_TypeInfoStructMember;
		MirType *t_TypeInfoEnumVariant;
		MirType *t_TypeInfoFnArg;
		MirType *t_TypeInfo_ptr;
		MirType *t_TypeInfo_slice;
		MirType *t_TypeInfoStructMembers_slice;
		MirType *t_TypeInfoEnumVariants_slice;
		MirType *t_TypeInfoFnArgs_slice;

		bool is_rtti_ready;
		bool is_any_ready;
	} builtin_types;
} Assembly;

typedef struct NativeLib {
	u32           hash;
	DLLib *       handle;
	struct Token *linked_from;
	const char *  user_name;
	char *        filename;
	char *        filepath;
	char *        dir;
	bool          is_internal;
} NativeLib;

Assembly *
assembly_new(const char *name);

void
assembly_delete(Assembly *assembly);

void
assembly_add_unit(Assembly *assembly, Unit *unit);

void
assembly_add_native_lib(Assembly *assembly, const char *lib_name, struct Token *link_token);

bool
assembly_add_unit_unique(Assembly *assembly, Unit *unit);

DCpointer
assembly_find_extern(Assembly *assembly, const char *symbol);

static inline bool
assembly_has_rtti(Assembly *assembly, u64 type_id)
{
	return thtbl_has_key(&assembly->MIR.RTTI_table, type_id);
}

static inline MirVar *
assembly_get_rtti(Assembly *assembly, u64 type_id)
{
	return thtbl_at(MirVar *, &assembly->MIR.RTTI_table, type_id);
}

static inline void
assembly_add_rtti(Assembly *assembly, u64 type_id, MirVar *rtti_var)
{
	thtbl_insert(&assembly->MIR.RTTI_table, type_id, rtti_var);
}

#endif
