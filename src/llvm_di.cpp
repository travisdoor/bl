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
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

#define cast(T) reinterpret_cast<T>

using namespace llvm;

static DIScope *
get_scope(DIBuilder *builder, Scope *scope, Unit *unit = nullptr)
{
	DIScope *result      = nullptr;
	auto     builder_ref = cast(LLVMDIBuilderRef)(builder);
	switch (scope->kind) {
	case SCOPE_FN:
		result = cast(DIScope *)(scope->llvm_meta);
		break;
	case SCOPE_LEXICAL:
		result = cast(DIScope *)(llvm_di_get_or_create_lex_scope(builder_ref, scope));
		break;
	case SCOPE_GLOBAL:
	case SCOPE_PRIVATE:
		assert(unit && "Expected file unit passed for global scope.");
		result = cast(DIScope *)(llvm_di_get_or_create_unit(builder_ref, unit));
		break;

	default:
		abort();
	}

	assert(result && "Cannot get scope ID!");
	return result;
}

/* public */
LLVMDIBuilderRef
llvm_di_new_di_builder(LLVMModuleRef module)
{
	return cast(LLVMDIBuilderRef)(new DIBuilder(*unwrap(module)));
}

void
llvm_di_delete_di_builder(LLVMDIBuilderRef builder_ref)
{
	delete cast(DIBuilder *)(builder_ref);
}

void
llvm_di_builder_finalize(LLVMDIBuilderRef builder)
{
	cast(DIBuilder *)(builder)->finalize();
}

LLVMMetadataRef
llvm_di_get_or_create_assembly(LLVMDIBuilderRef builder_ref, Assembly *assembly)
{
	if (assembly->llvm.di_meta) return assembly->llvm.di_meta;
	const char *producer = "blc version " BL_VERSION;

	auto builder = cast(DIBuilder *)(builder_ref);
	auto file    = builder->createFile({assembly->name, strlen(assembly->name)}, ".");
	auto cu = builder->createCompileUnit(dwarf::DW_LANG_C99, file, producer, false, "", 1, "");

	auto module = cast(Module *)(assembly->llvm.module);
	module->addModuleFlag(
	    Module::ModFlagBehavior::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);

	assembly->llvm.di_meta      = cast(LLVMMetadataRef)(cu);
	assembly->llvm.di_file_meta = cast(LLVMMetadataRef)(file);
	return assembly->llvm.di_meta;
}

LLVMMetadataRef
llvm_di_get_or_create_lex_scope(LLVMDIBuilderRef builder_ref, Scope *scope)
{
	if (scope->llvm_meta) return scope->llvm_meta;

	auto builder = cast(DIBuilder *)(builder_ref);

	assert(scope->kind == SCOPE_LEXICAL);
	assert(scope->parent->kind != SCOPE_GLOBAL &&
	       "Cannot generate lexical block in global scope!");

	auto di_scope = get_scope(builder, scope->parent);
	auto location = scope->location;
	auto di_file  = cast(DIFile *)(location->unit->llvm_file_meta);

	auto di = builder->createLexicalBlock(di_scope, di_file, location->line, location->col);
	scope->llvm_meta = cast(LLVMMetadataRef)(di);
	return scope->llvm_meta;
}

LLVMMetadataRef
llvm_di_get_or_create_fn(LLVMDIBuilderRef builder_ref, Assembly *assembly, MirFn *fn)
{
	assert(fn->decl_node);

	if (fn->body_scope->llvm_meta) return fn->body_scope->llvm_meta;

	auto builder  = cast(DIBuilder *)(builder_ref);
	auto location = fn->decl_node->location;
	auto file     = cast(DIFile *)(llvm_di_get_or_create_unit(builder_ref, location->unit));
	auto type =
	    cast(DISubroutineType *)(llvm_di_get_or_create_type(builder_ref, assembly, fn->type));
	auto scope = get_scope(builder, fn->decl_node->parent_scope, location->unit);

	auto di = builder->createFunction(scope,
	                                  {fn->id->str, strlen(fn->id->str)},
	                                  {fn->llvm_name, strlen(fn->llvm_name)},
	                                  file,
	                                  location->line,
	                                  type,
	                                  0,
	                                  DINode::FlagStaticMember,
	                                  DISubprogram::toSPFlags(false, true, false));

	auto func = cast(Function *)(fn->llvm_value);
	func->setSubprogram(di);

	fn->body_scope->llvm_meta = cast(LLVMMetadataRef)(di);
	return fn->body_scope->llvm_meta;
}

void
llvm_di_set_current_location(LLVMDIBuilderRef builder_ref,
                             LLVMBuilderRef   ir_builder_ref,
                             MirInstr *       instr)
{
	auto builder   = cast(DIBuilder *)(builder_ref);
	auto irbuilder = cast(IRBuilder<> *)(ir_builder_ref);

	if (!instr) {
		irbuilder->SetCurrentDebugLocation(DebugLoc());
		return;
	}

	if (!instr->node) return;
	if (!instr->node->location) return;

	auto scope    = get_scope(builder, instr->node->parent_scope);
	auto location = instr->node->location;

	irbuilder->SetCurrentDebugLocation(DebugLoc::get(location->line, location->col, scope));
}

LLVMMetadataRef
llvm_di_get_or_create_unit(LLVMDIBuilderRef builder_ref, Unit *unit)
{
	if (unit->llvm_file_meta) return unit->llvm_file_meta;
	auto builder = cast(DIBuilder *)(builder_ref);

	auto di = builder->createFile({unit->filename, strlen(unit->filename)},
	                              {unit->dirpath, strlen(unit->dirpath)});

	unit->llvm_file_meta = cast(LLVMMetadataRef)(di);
	return unit->llvm_file_meta;
}

LLVMMetadataRef
llvm_di_get_of_create_var(LLVMDIBuilderRef  builder_ref,
                          Assembly *        assembly,
                          LLVMBasicBlockRef bb_ref,
                          MirVar *          var)
{
	auto builder = cast(DIBuilder *)(builder_ref);
	auto bb      = cast(BasicBlock *)(bb_ref);

	if (var->is_implicit) {
		bl_warning("Should we generate impl variable ID???");
		return nullptr;
	}

	if (var->is_in_gscope) {
		bl_warning("Missing DI for global variables.");
		return nullptr;
	}

	auto storage  = cast(Value *)(var->llvm_value);
	auto scope    = get_scope(builder, var->decl_scope);
	auto location = var->decl_node->location;
	auto file     = cast(DIFile *)(llvm_di_get_or_create_unit(builder_ref, location->unit));
	auto type =
	    cast(DIType *)(llvm_di_get_or_create_type(builder_ref, assembly, var->value.type));
	auto name = StringRef(var->id->str, strlen(var->id->str));

	DILocalVariable *di = nullptr;

	if (var->is_arg_tmp) { /* function argument tmp */
		assert(var->order > -1);
		di = builder->createParameterVariable(
		    scope, name, var->order + 1, file, location->line, type, true);
	} else {
		di = builder->createAutoVariable(scope,
		                                 name,
		                                 file,
		                                 location->line,
		                                 type,
		                                 true,
		                                 DINode::FlagZero,
		                                 var->value.type->alignment * 8);
	}

	builder->insertDeclare(storage,
	                       di,
	                       builder->createExpression(),
	                       DebugLoc::get(location->line, location->col, scope),
	                       bb);

	return cast(LLVMMetadataRef)(di);
}

LLVMMetadataRef
llvm_di_get_or_create_type(LLVMDIBuilderRef builder_ref, Assembly *assembly, MirType *type)
{
	if (type->llvm_meta) return type->llvm_meta;
	auto    builder = cast(DIBuilder *)(builder_ref);
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
		di = builder->createBasicType(name, 8, dwarf::DW_ATE_unsigned_char);
		break;
	}

	case MIR_TYPE_NULL: {
		di = builder->createNullPtrType();
		break;
	}

	case MIR_TYPE_PTR: {
		auto pointeeType = cast(DIType *)(
		    llvm_di_get_or_create_type(builder_ref, assembly, type->data.ptr.expr));

		di = builder->createPointerType(pointeeType, type->size_bits, type->alignment * 8);
		break;
	}

	case MIR_TYPE_ARRAY: {
		auto elem_type = cast(DIType *)(
		    llvm_di_get_or_create_type(builder_ref, assembly, type->data.array.elem_type));

		di = builder->createArrayType(
		    type->data.array.len, type->alignment * 8, elem_type, nullptr);
		break;
	}

	case MIR_TYPE_FN: {
		SmallVector<Metadata *, 16> args;
		args.push_back(cast(Metadata *)(
		    llvm_di_get_or_create_type(builder_ref, assembly, type->data.fn.ret_type)));

		auto arg_types = type->data.fn.arg_types;
		if (arg_types) {
			for (size_t i = 0; i < bo_array_size(arg_types); ++i) {
				auto arg_type = bo_array_at(arg_types, i, MirType *);
				args.push_back(cast(Metadata *)(
				    llvm_di_get_or_create_type(builder_ref, assembly, arg_type)));
			}
		}

		di = builder->createSubroutineType(builder->getOrCreateTypeArray(args));
		break;
	}

	case MIR_TYPE_STRUCT:
	case MIR_TYPE_SLICE:
	case MIR_TYPE_VARGS:
	case MIR_TYPE_STRING: {
		assert(assembly);
		auto bl_scope = type->data.strct.scope;
		auto td       = cast(DataLayout *)(assembly->llvm.TD);
		auto stype    = cast(StructType *)(type->llvm_type);
		auto cname    = type->user_id ? type->user_id->str : type->id.str;
		auto name     = StringRef(cname, strlen(cname));

		DIScope *scope = nullptr;
		DIFile * file  = nullptr;
		unsigned line  = 0;

		if (bl_scope) {
			assert(bl_scope->parent && "Missing parent scope of the structure.");
			scope = cast(DIScope *)(bl_scope->parent->llvm_meta);
			file  = cast(DIFile *)(bl_scope->parent->llvm_meta);
		} else {
			scope = cast(DIScope *)(assembly->llvm.di_meta);
			file  = cast(DIFile *)(assembly->llvm.di_file_meta);
		}

		auto sdi = builder->createStructType(scope,
		                                     name,
		                                     file,
		                                     line,
		                                     type->size_bits,
		                                     type->alignment * 8,
		                                     DINode::DIFlags::FlagPublic,
		                                     nullptr,
		                                     nullptr);

		SmallVector<Metadata *, 32> members;
		{ /* setup elems DI */
			const size_t memc = bo_array_size(type->data.strct.members);
			for (size_t i = 0; i < memc; ++i) {
				auto member = bo_array_at(type->data.strct.members, i, MirMember *);
				auto member_type = cast(DIType *)(
				    llvm_di_get_or_create_type(builder_ref, assembly, member->type));

				char tmp[8] = {0};
				sprintf(tmp, "m_%lu", i);

				auto mdi = builder->createMemberType(
				    sdi,
				    {tmp, strlen(tmp)},
				    file,
				    0,
				    member_type->getSizeInBits(),
				    member_type->getAlignInBits(),
				    td->getStructLayout(stype)->getElementOffsetInBits(i),
				    DINode::DIFlags::FlagPublic,
				    member_type);
				members.push_back(mdi);
			}
		}

		sdi->replaceElements(builder->getOrCreateArray(members));

		di = sdi;
		break;
	}

	default:
		di = builder->createBasicType(name, type->size_bits, dwarf::DW_ATE_unsigned_char);
	}

	type->llvm_meta = cast(LLVMMetadataRef)(di);
	return type->llvm_meta;
}
