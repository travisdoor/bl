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
#include "llvm/IR/Attributes.h"

#define CAST(T) reinterpret_cast<T>

using namespace llvm;

LLVMAttributeRef
llvm_create_attribute(LLVMContextRef context_ref, LLVMAttributeKind kind)
{
	return CAST(LLVMAttributeRef)(
	    Attribute::get(*CAST(LLVMContext *)(context_ref), (Attribute::AttrKind)kind)
	        .getRawPointer());
}

LLVMAttributeRef
llvm_create_attribute_type(LLVMContextRef context_ref, LLVMAttributeKind kind, LLVMTypeRef type_ref)
{
	return CAST(LLVMAttributeRef)(Attribute::get(*CAST(LLVMContext *)(context_ref),
	                                             (Attribute::AttrKind)kind,
	                                             CAST(Type *)(type_ref))
	                                  .getRawPointer());
}
