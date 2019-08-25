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
llvm_di_create_lexical_scope(LLVMDIBuilderRef builder_ref,
                             LLVMMetadataRef  scope_ref,
                             LLVMMetadataRef  file_ref,
                             unsigned         line,
                             unsigned         col)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto scope   = builder->createLexicalBlock(
            cast(DIScope *)(scope_ref), cast(DIFile *)(file_ref), line, col);

	return cast(LLVMMetadataRef)(scope);
}

LLVMMetadataRef
llvm_di_create_fn_fwd_decl(LLVMDIBuilderRef builder_ref,
                           LLVMMetadataRef  scope_ref,
                           const char *     name,
                           const char *     linkage_name,
                           LLVMMetadataRef  file_ref,
                           unsigned         line,
                           LLVMMetadataRef  type_ref,
                           unsigned         scope_line)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto fn      = builder->createTempFunctionFwdDecl(cast(DIScope *)(scope_ref),
                                                     {name, strlen(name)},
                                                     {linkage_name, strlen(linkage_name)},
                                                     cast(DIFile *)(file_ref),
                                                     line,
                                                     cast(DISubroutineType *)(type_ref),
                                                     scope_line);

	return cast(LLVMMetadataRef)(fn);
}

LLVMMetadataRef
llvm_di_create_replecable_composite_type(LLVMDIBuilderRef builder_ref,
                                         DW_TAG           tag,
                                         const char *     name,
                                         LLVMMetadataRef  scope_ref,
                                         LLVMMetadataRef  file_ref,
                                         unsigned         line)
{
	auto builder = cast(DIBuilder *)(builder_ref);

	auto type = builder->createReplaceableCompositeType(
	    tag, {name, strlen(name)}, cast(DIScope *)(scope_ref), cast(DIFile *)(file_ref), line);

	return cast(LLVMMetadataRef)(type);
}

LLVMMetadataRef
llvm_di_create_fn(LLVMDIBuilderRef builder_ref,
                  LLVMMetadataRef  scope_ref,
                  const char *     name,
                  const char *     linkage_name,
                  LLVMMetadataRef  file_ref,
                  unsigned         line,
                  LLVMMetadataRef  type_ref,
                  unsigned         scope_line)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto fn      = builder->createFunction(cast(DIScope *)(scope_ref),
                                          {name, strlen(name)},
                                          {linkage_name, strlen(linkage_name)},
                                          cast(DIFile *)(file_ref),
                                          line,
                                          cast(DISubroutineType *)(type_ref),
                                          scope_line,
                                          DINode::FlagStaticMember,
                                          DISubprogram::toSPFlags(false, true, false));

	return cast(LLVMMetadataRef)(fn);
}

LLVMMetadataRef
llvm_di_replace_temporary(LLVMDIBuilderRef builder_ref,
                          LLVMMetadataRef  temp_ref,
                          LLVMMetadataRef  replacement_ref)
{
	auto builder  = cast(DIBuilder *)(builder_ref);
	auto replaced = builder->replaceTemporary(TempDIType(cast(DIType *)(temp_ref)),
	                                          cast(DIType *)(replacement_ref));
	return cast(LLVMMetadataRef)(replaced);
}

void
llvm_di_set_current_location(LLVMBuilderRef  builder_ref,
                             unsigned        line,
                             unsigned        col,
                             LLVMMetadataRef scope_ref,
                             bool            implicit)
{
	auto builder = cast(IRBuilder<> *)(builder_ref);
	auto scope   = cast(DIScope *)(scope_ref);
	builder->SetCurrentDebugLocation(DebugLoc::get(line, col, scope, nullptr, implicit));
}

void
llvm_di_reset_current_location(LLVMBuilderRef builder_ref)
{
	auto builder = cast(IRBuilder<> *)(builder_ref);
	builder->SetCurrentDebugLocation(DebugLoc());
}

LLVMMetadataRef
llvm_di_create_basic_type(LLVMDIBuilderRef builder_ref,
                          const char *     name,
                          unsigned         size_in_bits,
                          DW_ATE_Encoding  encoding)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto type    = builder->createBasicType({name, strlen(name)}, size_in_bits, encoding);

	return cast(LLVMMetadataRef)(type);
}

LLVMMetadataRef
llvm_di_create_function_type(LLVMDIBuilderRef builder_ref,
                             LLVMMetadataRef *params,
                             unsigned         paramsc)
{
	auto builder = cast(DIBuilder *)(builder_ref);

	auto tmp  = builder->getOrCreateTypeArray({cast(Metadata **)(params), paramsc});
	auto type = builder->createSubroutineType(tmp);

	return cast(LLVMMetadataRef)(type);
}

LLVMMetadataRef
llvm_di_create_array_type(LLVMDIBuilderRef builder_ref,
                          uint64_t         size_in_bits,
                          uint32_t         align_in_bits,
                          LLVMMetadataRef  type_ref,
                          uint64_t         elem_count)
{
	auto builder = cast(DIBuilder *)(builder_ref);

	SmallVector<Metadata *, 1> subrange;
	subrange.push_back(builder->getOrCreateSubrange(0, elem_count));
	auto type = builder->createArrayType(size_in_bits,
	                                     align_in_bits,
	                                     cast(DIType *)(type_ref),
	                                     builder->getOrCreateArray(subrange));
	return cast(LLVMMetadataRef)(type);
}

LLVMMetadataRef
llvm_di_create_enum_type(LLVMDIBuilderRef builder_ref,
                         LLVMMetadataRef  scope_ref,
                         const char *     name,
                         LLVMMetadataRef  file_ref,
                         unsigned         line,
                         uint64_t         size_in_bits,
                         uint32_t         align_in_bits,
                         LLVMMetadataRef *elems,
                         size_t           elemsc,
                         LLVMMetadataRef  type_ref)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto type    = builder->createEnumerationType(
            cast(DIScope *)(scope_ref),
            {name, strlen(name)},
            cast(DIFile *)(file_ref),
            line,
            size_in_bits,
            align_in_bits,
            builder->getOrCreateArray({cast(Metadata **)(elems), elemsc}),
            cast(DIType *)(type_ref),
            "",
            true);
	return cast(LLVMMetadataRef)(type);
}

LLVMMetadataRef
llvm_di_create_enum_variant(LLVMDIBuilderRef builder_ref,
                            const char *     name,
                            uint64_t         val,
                            bool             is_unsigned)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto variant = builder->createEnumerator({name, strlen(name)}, val, is_unsigned);

	return cast(LLVMMetadataRef)(variant);
}

LLVMMetadataRef
llvm_di_create_pointer_type(LLVMDIBuilderRef builder_ref,
                            LLVMMetadataRef  pointee_type_ref,
                            uint64_t         size_in_bits,
                            uint32_t         align_in_bits,
                            const char *     name)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto type    = builder->createPointerType(cast(DIType *)(pointee_type_ref),
                                               size_in_bits,
                                               align_in_bits,
                                               None,
                                               {name, strlen(name)});

	return cast(LLVMMetadataRef)(type);
}

LLVMMetadataRef
llvm_di_create_null_type(LLVMDIBuilderRef builder_ref)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto type    = builder->createNullPtrType();
	return cast(LLVMMetadataRef)(type);
}

LLVMMetadataRef
llvm_di_create_struct_type(LLVMDIBuilderRef builder_ref,
                           LLVMMetadataRef  scope_ref,
                           const char *     name,
                           LLVMMetadataRef  file_ref,
                           unsigned         line,
                           uint64_t         size_in_bits,
                           uint32_t         align_in_bits,
                           LLVMMetadataRef *elems,
                           uint64_t         elemsc)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto type    = builder->createStructType(
            cast(DIScope *)(scope_ref),
            {name, strlen(name)},
            cast(DIFile *)(file_ref),
            line,
            size_in_bits,
            align_in_bits,
            DINode::DIFlags::FlagZero,
            nullptr,
            builder->getOrCreateArray({cast(Metadata **)(elems), elemsc}));

	return cast(LLVMMetadataRef)(type);
}

LLVMMetadataRef
llvm_di_create_member_type(LLVMDIBuilderRef builder_ref,
                           LLVMMetadataRef  scope_ref,
                           const char *     name,
                           LLVMMetadataRef  file_ref,
                           unsigned         line,
                           uint64_t         size_in_bits,
                           uint32_t         align_in_bits,
                           uint64_t         offset_in_bits,
                           LLVMMetadataRef  type_ref)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto type    = builder->createMemberType(cast(DIScope *)(scope_ref),
                                              {name, strlen(name)},
                                              cast(DIFile *)(file_ref),
                                              line,
                                              size_in_bits,
                                              align_in_bits,
                                              offset_in_bits,
                                              DINode::DIFlags::FlagZero,
                                              cast(DIType *)(type_ref));

	return cast(LLVMMetadataRef)(type);
}

void
llvm_di_set_subprogram(LLVMValueRef fn_ref, LLVMMetadataRef subprogram_ref)
{
	auto func = cast(Function *)(fn_ref);
	func->setSubprogram(cast(DISubprogram *)(subprogram_ref));
}

LLVMMetadataRef
llvm_di_create_auto_variable(LLVMDIBuilderRef builder_ref,
                             LLVMMetadataRef  scope_ref,
                             const char *     name,
                             LLVMMetadataRef  file_ref,
                             unsigned         line,
                             LLVMMetadataRef  type_ref)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto var     = builder->createAutoVariable(cast(DIScope *)(scope_ref),
                                               {name, strlen(name)},
                                               cast(DIFile *)(file_ref),
                                               line,
                                               cast(DIType *)(type_ref));
	return cast(LLVMMetadataRef)(var);
}

void
llvm_di_insert_declare(LLVMDIBuilderRef  builder_ref,
                       LLVMValueRef      storage_ref,
                       LLVMMetadataRef   var_info_ref,
                       unsigned          line,
                       unsigned          col,
                       LLVMMetadataRef   scope_ref,
                       LLVMBasicBlockRef bb_ref)
{
	auto builder = cast(DIBuilder *)(builder_ref);

	builder->insertDeclare(cast(Value *)(storage_ref),
	                       cast(DILocalVariable *)(var_info_ref),
	                       builder->createExpression(),
	                       DebugLoc::get(line, col, cast(DIScope *)(scope_ref)),
	                       cast(BasicBlock *)(bb_ref));
}
