//************************************************************************************************
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
//************************************************************************************************

#ifndef BL_LLVM_API_H
#define BL_LLVM_API_H

#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Linker.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>
#include <llvm-c/Transforms/Vectorize.h>
#include <llvm-c/Types.h>

#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

/* TODO: intrinsic generators */

/* Custom C wrapper for LLVM C++ API, this is kinda needed because original C API for LLVM is
 * incomplete. All used calls to original API should be replaced by this wrapper later. */

/* This def declarations must matc with 'llvm/IR/Attributes.inc'*/
typedef enum {
	LLVM_ATTRIBUTE_NONE,
#include "llvm_attributes.inc"
} LLVMAttributeKind;

LLVMAttributeRef
llvm_create_attribute(LLVMContextRef context_ref, LLVMAttributeKind kind);

LLVMAttributeRef
llvm_create_attribute_int(LLVMContextRef context_ref, LLVMAttributeKind kind, s32 v);

LLVMAttributeRef
llvm_create_attribute_type(LLVMContextRef context_ref, LLVMAttributeKind kind, LLVMTypeRef v);

#ifdef __cplusplus
}
#endif
#endif
