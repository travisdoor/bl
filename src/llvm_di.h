//************************************************************************************************
// bl
//
// File:   llvm_di.h
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

/* Note: this is custom wrapper for LLVM DI used by compiler since original C-API for DWARF
 * generation in experimental. */

#ifndef BL_LLVM_DI_H
#define BL_LLVM_DI_H

#include <llvm-c/Types.h>
#include <llvm-c/Core.h>

#ifdef __cplusplus
extern "C" {
#endif

void
llvm_add_module_flag_int(LLVMModuleRef          module_ref,
                         LLVMModuleFlagBehavior behavior,
                         const char *           key,
                         int32_t                val);

int32_t
llvm_get_dwarf_version(void);

LLVMDIBuilderRef
llvm_di_new_di_builder(LLVMModuleRef module_ref);

void
llvm_di_delete_di_builder(LLVMDIBuilderRef builder_ref);

void
llvm_di_builder_finalize(LLVMDIBuilderRef builder_ref);

LLVMMetadataRef
llvm_di_create_compile_unit(LLVMDIBuilderRef builder_ref,
                            LLVMMetadataRef  file_ref,
                            const char *     producer);

LLVMMetadataRef
llvm_di_create_file(LLVMDIBuilderRef builder_ref, const char *filename, const char *dir);

#ifdef __cplusplus
}
#endif

#endif
