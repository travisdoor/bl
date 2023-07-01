// =================================================================================================
// bl
//
// File:   llvm_api.h
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
// =================================================================================================

#include "llvm_api.h"
#undef array
_SHUT_UP_BEGIN
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
_SHUT_UP_END

using namespace llvm;

LLVMTypeRef llvm_struct_create_named(LLVMContextRef C, str_t Name)
{
	StringRef sName(Name.ptr, (size_t)Name.len);
	return wrap(StructType::create(*unwrap(C), sName));
}

LLVMValueRef llvm_add_global(LLVMModuleRef M, LLVMTypeRef Ty, str_t Name)
{
	StringRef sName(Name.ptr, (size_t)Name.len);
	return wrap(new GlobalVariable(
	    *unwrap(M), unwrap(Ty), false, GlobalValue::ExternalLinkage, nullptr, sName));
}

LLVMValueRef llvm_add_function(LLVMModuleRef M, str_t Name, LLVMTypeRef FunctionTy)
{
	StringRef sName(Name.ptr, (size_t)Name.len);
	return wrap(Function::Create(
	    unwrap<FunctionType>(FunctionTy), GlobalValue::ExternalLinkage, sName, unwrap(M)));
}

LLVMValueRef llvm_build_alloca(LLVMBuilderRef B, LLVMTypeRef Ty, str_t Name)
{
	StringRef sName(Name.ptr, (size_t)Name.len);
	return wrap(unwrap(B)->CreateAlloca(unwrap(Ty), nullptr, sName));
}

LLVMBasicBlockRef llvm_append_basic_block_in_context(LLVMContextRef C, LLVMValueRef Fn, str_t Name)
{
	StringRef sName(Name.ptr, (size_t)Name.len);
	return wrap(BasicBlock::Create(*unwrap(C), sName, unwrap<Function>(Fn)));
}
