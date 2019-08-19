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

typedef enum {
	LLVMDIFlagZero                = 0,
	LLVMDIFlagPrivate             = 1,
	LLVMDIFlagProtected           = 2,
	LLVMDIFlagPublic              = 3,
	LLVMDIFlagFwdDecl             = 1 << 2,
	LLVMDIFlagAppleBlock          = 1 << 3,
	LLVMDIFlagBlockByrefStruct    = 1 << 4,
	LLVMDIFlagVirtual             = 1 << 5,
	LLVMDIFlagArtificial          = 1 << 6,
	LLVMDIFlagExplicit            = 1 << 7,
	LLVMDIFlagPrototyped          = 1 << 8,
	LLVMDIFlagObjcClassComplete   = 1 << 9,
	LLVMDIFlagObjectPointer       = 1 << 10,
	LLVMDIFlagVector              = 1 << 11,
	LLVMDIFlagStaticMember        = 1 << 12,
	LLVMDIFlagLValueReference     = 1 << 13,
	LLVMDIFlagRValueReference     = 1 << 14,
	LLVMDIFlagReserved            = 1 << 15,
	LLVMDIFlagSingleInheritance   = 1 << 16,
	LLVMDIFlagMultipleInheritance = 2 << 16,
	LLVMDIFlagVirtualInheritance  = 3 << 16,
	LLVMDIFlagIntroducedVirtual   = 1 << 18,
	LLVMDIFlagBitField            = 1 << 19,
	LLVMDIFlagNoReturn            = 1 << 20,
	LLVMDIFlagMainSubprogram      = 1 << 21,
	LLVMDIFlagTypePassByValue     = 1 << 22,
	LLVMDIFlagTypePassByReference = 1 << 23,
	LLVMDIFlagEnumClass           = 1 << 24,
	LLVMDIFlagFixedEnum           = LLVMDIFlagEnumClass, // Deprecated.
	LLVMDIFlagThunk               = 1 << 25,
	LLVMDIFlagTrivial             = 1 << 26,
	LLVMDIFlagBigEndian           = 1 << 27,
	LLVMDIFlagLittleEndian        = 1 << 28,
	LLVMDIFlagIndirectVirtualBase = (1 << 2) | (1 << 5),
	LLVMDIFlagAccessibility       = LLVMDIFlagPrivate | LLVMDIFlagProtected | LLVMDIFlagPublic,
	LLVMDIFlagPtrToMemberRep = LLVMDIFlagSingleInheritance | LLVMDIFlagMultipleInheritance |
	                           LLVMDIFlagVirtualInheritance
} LLVMDIFlags;

LLVMDIBuilderRef
llvm_di_new_di_builder(LLVMModuleRef module);

void
llvm_di_delete_di_builder(LLVMDIBuilderRef builder);

void
llvm_di_builder_finalize(LLVMDIBuilderRef builder);

void
llvm_di_set_subprogram(LLVMValueRef func, LLVMMetadataRef sp);

void
llvm_di_set_instr_location(LLVMDIBuilderRef builder_ref, struct MirInstr *instr);

LLVMMetadataRef
llvm_di_create_assembly(LLVMDIBuilderRef builder_ref, struct Assembly *assembly);

LLVMMetadataRef
llvm_di_create_fn(LLVMDIBuilderRef builder_ref, struct MirFn *fn);

LLVMMetadataRef
llvm_di_get_or_create_type(LLVMDIBuilderRef builder_ref, struct MirType *type);

LLVMMetadataRef
llvm_di_get_or_create_unit(LLVMDIBuilderRef builder_ref, struct Unit *unit);

#ifdef __cplusplus
}
#endif

#endif
