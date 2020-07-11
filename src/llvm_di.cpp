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

_LLVM_SHUT_UP_BEGIN
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/GVMaterializer.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>
_LLVM_SHUT_UP_END

#define CAST(T) reinterpret_cast<T>

using namespace llvm;

static Module::ModFlagBehavior map_to_llvmModFlagBehavior(LLVMModuleFlagBehavior Behavior)
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
void llvm_add_module_flag_int(LLVMModuleRef          module_ref,
                              LLVMModuleFlagBehavior behavior,
                              const char *           key,
                              s32                    val)
{
	auto module = CAST(Module *)(module_ref);
	module->addModuleFlag(map_to_llvmModFlagBehavior(behavior), {key, strlen(key)}, val);
}

s32 llvm_get_dwarf_version(void)
{
	return DEBUG_METADATA_VERSION;
}

LLVMDIBuilderRef llvm_di_new_di_builder(LLVMModuleRef module_ref)
{
	return CAST(LLVMDIBuilderRef)(new DIBuilder(*CAST(Module *)(module_ref)));
}

void llvm_di_delete_di_builder(LLVMDIBuilderRef builder_ref)
{
	delete CAST(DIBuilder *)(builder_ref);
}

void llvm_di_builder_finalize(LLVMDIBuilderRef builder_ref)
{
	CAST(DIBuilder *)(builder_ref)->finalize();
}

LLVMMetadataRef llvm_di_create_compile_unit(LLVMDIBuilderRef builder_ref,
                                            LLVMMetadataRef  file_ref,
                                            const char *     producer)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto file    = CAST(DIFile *)(file_ref);
	auto cu = builder->createCompileUnit(dwarf::DW_LANG_C99, file, producer, false, "", 1, "");

	return CAST(LLVMMetadataRef)(cu);
}

LLVMMetadataRef
llvm_di_create_file(LLVMDIBuilderRef builder_ref, const char *filename, const char *dir)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto file    = builder->createFile({filename, strlen(filename)}, {dir, strlen(dir)});
	return CAST(LLVMMetadataRef)(file);
}

LLVMMetadataRef llvm_di_create_lexical_scope(LLVMDIBuilderRef builder_ref,
                                             LLVMMetadataRef  scope_ref,
                                             LLVMMetadataRef  file_ref,
                                             unsigned         line,
                                             unsigned         col)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto scope   = builder->createLexicalBlock(
            CAST(DIScope *)(scope_ref), CAST(DIFile *)(file_ref), line, col);

	return CAST(LLVMMetadataRef)(scope);
}

LLVMMetadataRef llvm_di_create_fn_fwd_decl(LLVMDIBuilderRef builder_ref,
                                           LLVMMetadataRef  scope_ref,
                                           const char *     name,
                                           const char *     linkage_name,
                                           LLVMMetadataRef  file_ref,
                                           unsigned         line,
                                           LLVMMetadataRef  type_ref,
                                           unsigned         scope_line)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto fn      = builder->createTempFunctionFwdDecl(CAST(DIScope *)(scope_ref),
                                                     {name, strlen(name)},
                                                     {linkage_name, strlen(linkage_name)},
                                                     CAST(DIFile *)(file_ref),
                                                     line,
                                                     CAST(DISubroutineType *)(type_ref),
                                                     scope_line);

	return CAST(LLVMMetadataRef)(fn);
}

LLVMMetadataRef llvm_di_create_replecable_composite_type(LLVMDIBuilderRef builder_ref,
                                                         DW_TAG           tag,
                                                         const char *     name,
                                                         LLVMMetadataRef  scope_ref,
                                                         LLVMMetadataRef  file_ref,
                                                         unsigned         line)
{
	auto builder = CAST(DIBuilder *)(builder_ref);

	auto type = builder->createReplaceableCompositeType(
	    tag, {name, strlen(name)}, CAST(DIScope *)(scope_ref), CAST(DIFile *)(file_ref), line);

	return CAST(LLVMMetadataRef)(type);
}

LLVMMetadataRef llvm_di_create_fn(LLVMDIBuilderRef builder_ref,
                                  LLVMMetadataRef  scope_ref,
                                  const char *     name,
                                  const char *     linkage_name,
                                  LLVMMetadataRef  file_ref,
                                  unsigned         line,
                                  LLVMMetadataRef  type_ref,
                                  unsigned         scope_line)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto fn      = builder->createFunction(CAST(DIScope *)(scope_ref),
                                          {name, strlen(name)},
                                          {linkage_name, strlen(linkage_name)},
                                          CAST(DIFile *)(file_ref),
                                          line,
                                          CAST(DISubroutineType *)(type_ref),
                                          scope_line,
                                          DINode::FlagPrototyped,
                                          DISubprogram::toSPFlags(false, true, false));

	return CAST(LLVMMetadataRef)(fn);
}

LLVMMetadataRef llvm_di_replace_temporary(LLVMDIBuilderRef builder_ref,
                                          LLVMMetadataRef  temp_ref,
                                          LLVMMetadataRef  replacement_ref)
{
	auto builder  = CAST(DIBuilder *)(builder_ref);
	auto replaced = builder->replaceTemporary(TempDIType(CAST(DIType *)(temp_ref)),
	                                          CAST(DIType *)(replacement_ref));
	return CAST(LLVMMetadataRef)(replaced);
}

void llvm_di_set_current_location(LLVMBuilderRef  builder_ref,
                                  unsigned        line,
                                  unsigned        col,
                                  LLVMMetadataRef scope_ref,
                                  bool            implicit)
{
	auto builder = CAST(IRBuilder<> *)(builder_ref);
	auto scope   = CAST(DIScope *)(scope_ref);
	builder->SetCurrentDebugLocation(DebugLoc::get(line, col, scope, nullptr, implicit));
}

void llvm_di_reset_current_location(LLVMBuilderRef builder_ref)
{
	auto builder = CAST(IRBuilder<> *)(builder_ref);
	builder->SetCurrentDebugLocation(DebugLoc());
}

LLVMMetadataRef llvm_di_create_basic_type(LLVMDIBuilderRef builder_ref,
                                          const char *     name,
                                          unsigned         size_in_bits,
                                          DW_ATE_Encoding  encoding)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto type    = builder->createBasicType({name, strlen(name)}, size_in_bits, encoding);

	return CAST(LLVMMetadataRef)(type);
}

LLVMMetadataRef llvm_di_create_function_type(LLVMDIBuilderRef builder_ref,
                                             LLVMMetadataRef *params,
                                             unsigned         paramsc)
{
	auto builder = CAST(DIBuilder *)(builder_ref);

	auto tmp  = builder->getOrCreateTypeArray({CAST(Metadata **)(params), paramsc});
	auto type = builder->createSubroutineType(tmp);

	return CAST(LLVMMetadataRef)(type);
}

LLVMMetadataRef llvm_di_create_array_type(LLVMDIBuilderRef builder_ref,
                                          u64              size_in_bits,
                                          u32              align_in_bits,
                                          LLVMMetadataRef  type_ref,
                                          u64              elem_count)
{
	auto builder = CAST(DIBuilder *)(builder_ref);

	SmallVector<Metadata *, 1> subrange;
	subrange.push_back(builder->getOrCreateSubrange(0, elem_count));
	auto type = builder->createArrayType(size_in_bits,
	                                     align_in_bits,
	                                     CAST(DIType *)(type_ref),
	                                     builder->getOrCreateArray(subrange));
	return CAST(LLVMMetadataRef)(type);
}

LLVMMetadataRef llvm_di_create_enum_type(LLVMDIBuilderRef builder_ref,
                                         LLVMMetadataRef  scope_ref,
                                         const char *     name,
                                         LLVMMetadataRef  file_ref,
                                         unsigned         line,
                                         u64              size_in_bits,
                                         u32              align_in_bits,
                                         LLVMMetadataRef *elems,
                                         size_t           elemsc,
                                         LLVMMetadataRef  type_ref)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto type    = builder->createEnumerationType(
            CAST(DIScope *)(scope_ref),
            {name, strlen(name)},
            CAST(DIFile *)(file_ref),
            line,
            size_in_bits,
            align_in_bits,
            builder->getOrCreateArray({CAST(Metadata **)(elems), elemsc}),
            CAST(DIType *)(type_ref),
            "",
            true);
	return CAST(LLVMMetadataRef)(type);
}

LLVMMetadataRef llvm_di_create_enum_variant(LLVMDIBuilderRef builder_ref,
                                            const char *     name,
                                            u64              val,
                                            bool             is_unsigned)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto variant = builder->createEnumerator({name, strlen(name)}, val, is_unsigned);

	return CAST(LLVMMetadataRef)(variant);
}

LLVMMetadataRef llvm_di_create_pointer_type(LLVMDIBuilderRef builder_ref,
                                            LLVMMetadataRef  pointee_type_ref,
                                            u64              size_in_bits,
                                            u32              align_in_bits,
                                            const char *     name)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto type    = builder->createPointerType(CAST(DIType *)(pointee_type_ref),
                                               size_in_bits,
                                               align_in_bits,
                                               None,
                                               {name, strlen(name)});

	return CAST(LLVMMetadataRef)(type);
}

LLVMMetadataRef llvm_di_create_null_type(LLVMDIBuilderRef builder_ref)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto type    = builder->createNullPtrType();
	return CAST(LLVMMetadataRef)(type);
}

LLVMMetadataRef llvm_di_create_struct_type(LLVMDIBuilderRef builder_ref,
                                           LLVMMetadataRef  scope_ref,
                                           const char *     name,
                                           LLVMMetadataRef  file_ref,
                                           unsigned         line,
                                           u64              size_in_bits,
                                           u32              align_in_bits,
                                           LLVMMetadataRef *elems,
                                           u64              elemsc)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto type    = builder->createStructType(
            CAST(DIScope *)(scope_ref),
            {name, strlen(name)},
            CAST(DIFile *)(file_ref),
            line,
            size_in_bits,
            align_in_bits,
            DINode::DIFlags::FlagZero,
            nullptr,
            builder->getOrCreateArray({CAST(Metadata **)(elems), elemsc}));

	return CAST(LLVMMetadataRef)(type);
}

LLVMMetadataRef llvm_di_create_union_type(LLVMDIBuilderRef builder_ref,
                                          LLVMMetadataRef  scope_ref,
                                          const char *     name,
                                          LLVMMetadataRef  file_ref,
                                          unsigned         line,
                                          u64              size_in_bits,
                                          u32              align_in_bits,
                                          LLVMMetadataRef *elems,
                                          u64              elemsc)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto type =
	    builder->createUnionType(CAST(DIScope *)(scope_ref),
	                             {name, strlen(name)},
	                             CAST(DIFile *)(file_ref),
	                             line,
	                             size_in_bits,
	                             align_in_bits,
	                             DINode::DIFlags::FlagZero,
	                             builder->getOrCreateArray({CAST(Metadata **)(elems), elemsc}));

	return CAST(LLVMMetadataRef)(type);
}

LLVMMetadataRef llvm_di_create_member_type(LLVMDIBuilderRef builder_ref,
                                           LLVMMetadataRef  scope_ref,
                                           const char *     name,
                                           LLVMMetadataRef  file_ref,
                                           unsigned         line,
                                           u64              size_in_bits,
                                           u32              align_in_bits,
                                           u64              offset_in_bits,
                                           LLVMMetadataRef  type_ref)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto type    = builder->createMemberType(CAST(DIScope *)(scope_ref),
                                              {name, strlen(name)},
                                              CAST(DIFile *)(file_ref),
                                              line,
                                              size_in_bits,
                                              align_in_bits,
                                              offset_in_bits,
                                              DINode::DIFlags::FlagZero,
                                              CAST(DIType *)(type_ref));

	return CAST(LLVMMetadataRef)(type);
}

void llvm_di_set_subprogram(LLVMValueRef fn_ref, LLVMMetadataRef subprogram_ref)
{
	auto func = CAST(Function *)(fn_ref);
	func->setSubprogram(CAST(DISubprogram *)(subprogram_ref));
}

void llvm_di_finalize_subprogram(LLVMDIBuilderRef builder_ref, LLVMMetadataRef subprogram_ref)
{
	CAST(DIBuilder *)(builder_ref)->finalizeSubprogram(CAST(DISubprogram *)(subprogram_ref));
}

LLVMMetadataRef llvm_di_create_auto_variable(LLVMDIBuilderRef builder_ref,
                                             LLVMMetadataRef  scope_ref,
                                             const char *     name,
                                             LLVMMetadataRef  file_ref,
                                             unsigned         line,
                                             LLVMMetadataRef  type_ref)
{
	auto builder = CAST(DIBuilder *)(builder_ref);
	auto var     = builder->createAutoVariable(CAST(DIScope *)(scope_ref),
                                               {name, strlen(name)},
                                               CAST(DIFile *)(file_ref),
                                               line,
                                               CAST(DIType *)(type_ref));
	return CAST(LLVMMetadataRef)(var);
}

LLVMMetadataRef llvm_di_create_global_variable(LLVMDIBuilderRef builder_ref,
                                               LLVMMetadataRef  scope_ref,
                                               const char *     name,
                                               LLVMMetadataRef  file_ref,
                                               unsigned         line,
                                               LLVMMetadataRef  type_ref)
{

	auto builder = CAST(DIBuilder *)(builder_ref);

	auto var = builder->createTempGlobalVariableFwdDecl(CAST(DIScope *)(scope_ref),
	                                                    {name, strlen(name)},
	                                                    {name, strlen(name)},
	                                                    CAST(DIFile *)(file_ref),
	                                                    line,
	                                                    CAST(DIType *)(type_ref),
	                                                    false);

	return CAST(LLVMMetadataRef)(var);
}

LLVMMetadataRef llvm_di_create_global_variable_expression(LLVMDIBuilderRef builder_ref,
                                                          LLVMMetadataRef  scope_ref,
                                                          const char *     name,
                                                          LLVMMetadataRef  file_ref,
                                                          unsigned         line,
                                                          LLVMMetadataRef  type_ref)
{
	auto builder = CAST(DIBuilder *)(builder_ref);

#if LLVM_VERSION_MAJOR >= 10
	auto var = builder->createGlobalVariableExpression(CAST(DIScope *)(scope_ref),
	                                                   {name, strlen(name)},
	                                                   {name, strlen(name)},
	                                                   CAST(DIFile *)(file_ref),
	                                                   line,
	                                                   CAST(DIType *)(type_ref),
	                                                   true,
	                                                   false);
#else
	auto var = builder->createGlobalVariableExpression(CAST(DIScope *)(scope_ref),
	                                                   {name, strlen(name)},
	                                                   {name, strlen(name)},
	                                                   CAST(DIFile *)(file_ref),
	                                                   line,
	                                                   CAST(DIType *)(type_ref),
	                                                   true,
	                                                   nullptr);
#endif

	return CAST(LLVMMetadataRef)(var);
}

void llvm_di_insert_declare(LLVMDIBuilderRef  builder_ref,
                            LLVMValueRef      storage_ref,
                            LLVMMetadataRef   var_info_ref,
                            unsigned          line,
                            unsigned          col,
                            LLVMMetadataRef   scope_ref,
                            LLVMBasicBlockRef bb_ref)
{
	auto builder = CAST(DIBuilder *)(builder_ref);

	builder->insertDeclare(CAST(Value *)(storage_ref),
	                       CAST(DILocalVariable *)(var_info_ref),
	                       builder->createExpression(),
	                       DebugLoc::get(line, col, CAST(DIScope *)(scope_ref)),
	                       CAST(BasicBlock *)(bb_ref));
}
