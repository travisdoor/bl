//************************************************************************************************
// bl
//
// File:   llvm_api.cpp
// Author: Martin Dorazil
// Date:   9/21/19
//
// Copyright 2019 Martin Dorazil
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

#include "llvm_api.h"
#include <cmath>
#include <llvm/ADT/StringSwitch.h>
#include <llvm/Config/llvm-config.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Constants.h>

#define CAST(T) reinterpret_cast<T>

using namespace llvm;

#define GET_ATTR_KIND_FROM_NAME
#include "llvm/IR/Attributes.inc"

LLVMAttributeKind
llvm_get_attribute_kind(const char *name)
{
	return getAttrKindFromName({name, strlen(name)});
}

LLVMAttributeRef
llvm_create_attribute(LLVMContextRef context_ref, LLVMAttributeKind kind)
{
	return CAST(LLVMAttributeRef)(
	    Attribute::get(*CAST(LLVMContext *)(context_ref), (Attribute::AttrKind)kind)
	        .getRawPointer());
}

LLVMAttributeRef
llvm_create_attribute_int(LLVMContextRef context_ref, LLVMAttributeKind kind, s32 v)
{
	return CAST(LLVMAttributeRef)(
	    Attribute::get(*CAST(LLVMContext *)(context_ref), (Attribute::AttrKind)kind, v)
	        .getRawPointer());
}

LLVMAttributeRef
llvm_create_attribute_type(LLVMContextRef context_ref, LLVMAttributeKind kind, LLVMTypeRef v)
{
#if LLVM_VERSION_MAJOR == 10
	return CAST(LLVMAttributeRef)(Attribute::get(*CAST(LLVMContext *)(context_ref),
	                                             (Attribute::AttrKind)kind,
	                                             CAST(Type *)(v))
	                                  .getRawPointer());
#else
	return CAST(LLVMAttributeRef)(
	    Attribute::get(*CAST(LLVMContext *)(context_ref), (Attribute::AttrKind)kind)
	        .getRawPointer());
#endif
}

u32
llvm_lookup_intrinsic_id(const char *name)
{
	return Function::lookupIntrinsicID({name, strlen(name)});
}

static Intrinsic::ID
llvm_map_to_intrinsic_id(unsigned ID)
{
	assert(ID < llvm::Intrinsic::num_intrinsics && "Intrinsic ID out of range");
	return llvm::Intrinsic::ID(ID);
}

LLVMValueRef
llvm_get_intrinsic_decl(LLVMModuleRef mod_ref,
                        u32           id,
                        LLVMTypeRef * param_types_ref,
                        usize         param_types_count)
{
	ArrayRef<Type *> types(CAST(Type **)(param_types_ref), param_types_count);

	auto iid = llvm_map_to_intrinsic_id(id);
	return CAST(LLVMValueRef)(
	    llvm::Intrinsic::getDeclaration(CAST(Module *)(mod_ref), iid, types));
}

LLVMValueRef
llvm_const_string_in_context(LLVMContextRef context_ref,
                             LLVMTypeRef    t,
                             const char *   str,
                             bool           zero_terminate)
{
        size_t len = strlen(str);
        SmallVector<Constant*, 32> chars;
        for (size_t i = 0; i < len; ++i)  {
                auto c = ConstantInt::get(CAST(Type *)(t), (uint64_t)str[i]);
                chars.push_back(c);
        }

        if (zero_terminate) {
                auto c = ConstantInt::get(CAST(Type *)(t), (uint64_t)'\0');
                chars.push_back(c);
                ++len;
        }

        ArrayRef<Constant*> V(chars);
        return CAST(LLVMValueRef)(ConstantArray::get(ArrayType::get(CAST(Type *)(t), len), V));
}
