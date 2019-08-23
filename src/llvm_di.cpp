//************************************************************************************************
// bl
//
// File:   llvm_di.cpp
// Author: Martin Dorazil
// Date:   8/19/19
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

#include "llvm_di.h"
#include "assembly.h"
#include "mir.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/GVMaterializer.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

#define cast(T) reinterpret_cast<T>

using namespace llvm;

static Module::ModFlagBehavior
map_to_llvmModFlagBehavior(LLVMModuleFlagBehavior Behavior)
{
	switch (Behavior) {
	case LLVMModuleFlagBehaviorError:
		return Module::ModFlagBehavior::Error;
	case LLVMModuleFlagBehaviorWarning:
		return Module::ModFlagBehavior::Warning;
	case LLVMModuleFlagBehaviorRequire:
		return Module::ModFlagBehavior::Require;
	case LLVMModuleFlagBehaviorOverride:
		return Module::ModFlagBehavior::Override;
	case LLVMModuleFlagBehaviorAppend:
		return Module::ModFlagBehavior::Append;
	case LLVMModuleFlagBehaviorAppendUnique:
		return Module::ModFlagBehavior::AppendUnique;
	}
	llvm_unreachable("Unknown LLVMModuleFlagBehavior");
}

/* public */
void
llvm_add_module_flag_int(LLVMModuleRef          module_ref,
                         LLVMModuleFlagBehavior behavior,
                         const char *           key,
                         int32_t                val)
{
	auto module = cast(Module *)(module_ref);
	module->addModuleFlag(map_to_llvmModFlagBehavior(behavior), {key, strlen(key)}, val);
}

int32_t
llvm_get_dwarf_version(void)
{
	return DEBUG_METADATA_VERSION;
}

LLVMDIBuilderRef
llvm_di_new_di_builder(LLVMModuleRef module_ref)
{
	return cast(LLVMDIBuilderRef)(new DIBuilder(*cast(Module *)(module_ref)));
}

void
llvm_di_delete_di_builder(LLVMDIBuilderRef builder_ref)
{
	delete cast(DIBuilder *)(builder_ref);
}

void
llvm_di_builder_finalize(LLVMDIBuilderRef builder_ref)
{
	cast(DIBuilder *)(builder_ref)->finalize();
}

LLVMMetadataRef
llvm_di_create_compile_unit(LLVMDIBuilderRef builder_ref,
                            LLVMMetadataRef  file_ref,
                            const char *     producer)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto file    = cast(DIFile *)(file_ref);
	auto cu = builder->createCompileUnit(dwarf::DW_LANG_C99, file, producer, false, "", 1, "");

	return cast(LLVMMetadataRef)(cu);
}

LLVMMetadataRef
llvm_di_create_file(LLVMDIBuilderRef builder_ref, const char *filename, const char *dir)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto file    = builder->createFile({filename, strlen(filename)}, {dir, strlen(dir)});
	return cast(LLVMMetadataRef)(file);
}

LLVMMetadataRef
llvm_di_create_fn_fwd_decl(LLVMBuilderRef  builder_ref,
                           LLVMMetadataRef scope,
                           const char *    name,
                           const char *    linkage_name,
                           LLVMMetadataRef file,
                           unsigned        line,
                           LLVMMetadataRef type,
                           unsigned        scope_line)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto fn      = builder->createTempFunctionFwdDecl(cast(DIScope *)(scope),
                                                     {name, strlen(name)},
                                                     {linkage_name, strlen(linkage_name)},
                                                     cast(DIFile *)(file),
                                                     line,
                                                     cast(DISubroutineType *)(type),
                                                     scope_line);

	return cast(LLVMMetadataRef)(fn);
}

/*-----------------*/
