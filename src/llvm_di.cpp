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
#include "mir.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/GVMaterializer.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Casting.h>

//#include <llvm/IR/Instruction.h>
//#include <llvm/IR/LLVMContext.h>
//#include <llvm/IR/DebugLoc.h>
//#include <llvm/IR/Function.h>
//#include <llvm/IR/DebugInfo.h>
//#include <llvm/IR/BasicBlock.h>
//#include <llvm/IR/Constants.h>
//#include <llvm/ADT/DenseMap.h>
//#include <llvm/ADT/DenseSet.h>
//#include <llvm/ADT/None.h>
//#include <llvm/ADT/STLExtras.h>
//#include <llvm/ADT/SmallPtrSet.h>
//#include <llvm/ADT/SmallVector.h>

using namespace llvm;

template <typename DIT>
DIT *
unwrapDI(LLVMMetadataRef Ref)
{
	return (DIT *)(Ref ? unwrap<MDNode>(Ref) : nullptr);
}

static DINode::DIFlags
map_from_llvmDIFlags(LLVMDIFlags Flags)
{
	return static_cast<DINode::DIFlags>(Flags);
}

LLVMDIBuilderRef
llvm_di_new_di_builder(LLVMModuleRef module)
{
	return wrap(new DIBuilder(*unwrap(module)));
}

void
llvm_di_delete_di_builder(LLVMDIBuilderRef builder)
{
	delete unwrap(builder);
}

LLVMMetadataRef
llvm_di_create_file(LLVMDIBuilderRef builder, const char *filename, const char *dir)
{
	return wrap(unwrap(builder)->createFile(StringRef(filename, strlen(filename)),
	                                        StringRef(dir, strlen(dir))));
}

void
llvm_di_builder_finalize(LLVMDIBuilderRef builder)
{
	unwrap(builder)->finalize();
}

LLVMMetadataRef
llvm_di_create_lexical_block(LLVMDIBuilderRef builder,
                             LLVMMetadataRef  scope,
                             LLVMMetadataRef  file,
                             unsigned         line,
                             unsigned         col)
{
	return wrap(unwrap(builder)->createLexicalBlock(
	    unwrapDI<DIScope>(scope), unwrapDI<DIFile>(file), line, col));
}

void
llvm_di_set_subprogram(LLVMValueRef func, LLVMMetadataRef sp)
{
	unwrap<Function>(func)->setSubprogram(unwrap<DISubprogram>(sp));
}

LLVMMetadataRef
llvm_di_create_assembly(LLVMDIBuilderRef builder_ref, Assembly *assembly)
{
	const char *producer = "blc version " BL_VERSION;

	auto builder = unwrap(builder_ref);
	auto file    = builder->createFile({assembly->name, strlen(assembly->name)}, ".");
	auto cu = builder->createCompileUnit(dwarf::DW_LANG_C99, file, producer, false, "", 0, "");

	return wrap(cu);
}

LLVMMetadataRef
llvm_di_create_fn(LLVMDIBuilderRef builder_ref, struct MirFn *fn)
{
	auto builder  = unwrap(builder_ref);
	auto location = fn->decl_node->location;

	auto file  = unwrap<DIFile>(llvm_di_get_or_create_unit(builder_ref, location->unit));
	auto type  = unwrap<DISubroutineType>(llvm_di_get_or_create_type(builder_ref, fn->type));
	auto scope = file; // TODO

	auto di = builder->createFunction(scope,
	                                  {fn->id->str, strlen(fn->id->str)},
	                                  {fn->llvm_name, strlen(fn->llvm_name)},
	                                  file,
	                                  location->line,
	                                  type,
	                                  0);

	auto func = unwrap<Function>(fn->llvm_value);
	func->setSubprogram(di);

	return wrap(di);
}

void
llvm_di_set_instr_location(LLVMDIBuilderRef builder_ref, MirInstr *instr)
{
	assert(instr->node);
	assert(instr->node->parent_scope->llvm_meta);

	auto scope = unwrap<DIScope>(instr->node->parent_scope->llvm_meta);

	auto instruction = unwrap<Instruction>(instr->llvm_value);
	instruction->setDebugLoc(
	    DebugLoc::get(instr->node->location->line, instr->node->location->col, scope));
}

LLVMMetadataRef
llvm_di_get_or_create_unit(LLVMDIBuilderRef builder_ref, Unit *unit)
{
	if (unit->llvm_file_meta) return unit->llvm_file_meta;
	auto builder = unwrap(builder_ref);

	auto di = builder->createFile({unit->filename, strlen(unit->filename)},
	                              {unit->dirpath, strlen(unit->dirpath)});

	unit->llvm_file_meta = wrap(di);
	return unit->llvm_file_meta;
}

LLVMMetadataRef
llvm_di_get_or_create_type(LLVMDIBuilderRef builder_ref, MirType *type)
{
	if (type->llvm_meta) return type->llvm_meta;
	auto    builder = unwrap(builder_ref);
	DIType *di      = nullptr;

	auto name = type->user_id ? StringRef(type->user_id->str, strlen(type->user_id->str))
	                          : StringRef(type->id.str, strlen(type->id.str));

	switch (type->kind) {
	case MIR_TYPE_INT: {
		di = builder->createBasicType(
		    name,
		    type->size_bits,
		    type->data.integer.is_signed ? dwarf::DW_ATE_signed : dwarf::DW_ATE_unsigned);
		break;
	}

	case MIR_TYPE_REAL: {
		di = builder->createBasicType(name, type->size_bits, dwarf::DW_ATE_float);
		break;
	}

	case MIR_TYPE_BOOL: {
		di = builder->createBasicType(name, type->size_bits, dwarf::DW_ATE_boolean);
		break;
	}

	case MIR_TYPE_VOID: {
		di = builder->createBasicType(name, type->size_bits, dwarf::DW_ATE_unsigned_char);
		break;
	}

	case MIR_TYPE_NULL: {
		di = builder->createNullPtrType();
		break;
	}

	case MIR_TYPE_PTR: {
		auto pointeeType =
		    unwrapDI<DIType>(llvm_di_get_or_create_type(builder_ref, type->data.ptr.expr));

		di = builder->createPointerType(pointeeType, type->size_bits, type->alignment * 8);
		break;
	}

	case MIR_TYPE_ARRAY: {
		auto elem_type = unwrapDI<DIType>(
		    llvm_di_get_or_create_type(builder_ref, type->data.array.elem_type));

		di = builder->createArrayType(
		    type->data.array.len, type->alignment * 8, elem_type, nullptr);
		break;
	}

	case MIR_TYPE_FN: {
		SmallVector<Metadata *, 8> args;
		args.push_back(
		    unwrap(llvm_di_get_or_create_type(builder_ref, type->data.fn.ret_type)));

		auto arg_types = type->data.fn.arg_types;
		if (arg_types) {
			for (size_t i = 0; i < bo_array_size(arg_types); ++i) {
				auto arg_type = bo_array_at(arg_types, i, MirType *);
				args.push_back(
				    unwrap(llvm_di_get_or_create_type(builder_ref, arg_type)));
			}
		}

		di = builder->createSubroutineType(builder->getOrCreateTypeArray(args));
		break;
	}

	default:
		return nullptr;
	}

	type->llvm_meta = wrap(di);
	return type->llvm_meta;
}
