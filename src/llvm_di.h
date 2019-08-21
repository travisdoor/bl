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

#ifdef __cplusplus
extern "C" {
#endif

LLVMDIBuilderRef
llvm_di_new_di_builder(LLVMModuleRef module);

void
llvm_di_delete_di_builder(LLVMDIBuilderRef builder_ref);

void
llvm_di_builder_finalize(LLVMDIBuilderRef builder_ref);

LLVMMetadataRef
llvm_di_get_or_create_assembly(LLVMDIBuilderRef builder_ref, struct Assembly *assembly);

LLVMMetadataRef
llvm_di_get_or_create_fn(LLVMDIBuilderRef builder_ref, struct Assembly *assembly, struct MirFn *fn);

LLVMMetadataRef
llvm_di_get_or_create_type(LLVMDIBuilderRef builder_ref,
                           struct Assembly *assembly,
                           struct MirType * type);

LLVMMetadataRef
llvm_di_get_or_create_unit(LLVMDIBuilderRef builder_ref, struct Unit *unit);

LLVMMetadataRef
llvm_di_get_or_create_lex_scope(LLVMDIBuilderRef builder_ref, struct Scope *scope);

LLVMMetadataRef
llvm_di_get_of_create_var(LLVMDIBuilderRef  builder_ref,
                          struct Assembly * assembly,
                          LLVMBasicBlockRef bb_ref,
                          struct MirVar *   var);

void
llvm_di_set_current_location(LLVMDIBuilderRef builder_ref,
                             LLVMBuilderRef   ir_builder_ref,
                             struct MirInstr *instr);

#ifdef __cplusplus
}
#endif

#endif
