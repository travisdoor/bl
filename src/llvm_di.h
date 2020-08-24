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

// Note: this is custom wrapper for LLVM DI used by compiler since original C-API for DWARF
// generation in experimental.

#ifndef BL_LLVM_DI_H
#define BL_LLVM_DI_H

#include "common.h"
#include "llvm_api.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    DW_ATE_adderess      = 1,
    DW_ATE_boolean       = 2,
    DW_ATE_complex_float = 3,
    DW_ATE_float         = 4,
    DW_ATE_signed        = 5,
    DW_ATE_signed_char   = 6,
    DW_ATE_unsigned      = 7,
    DW_ATE_unsigned_char = 8,
} DW_ATE_Encoding;

typedef enum {
#if LLVM_VERSION_MAJOR >= 10
#define HANDLE_DW_TAG(ID, NAME, VERSION, VENDOR, KIND) DW_TAG_##NAME = ID,
#else
#define HANDLE_DW_TAG(ID, NAME, VERSION, VENDOR) DW_TAG_##NAME = ID,
#endif
#include "llvm/BinaryFormat/Dwarf.def"
    DW_TAG_lo_user   = 0x4080,
    DW_TAG_hi_user   = 0xffff,
    DW_TAG_user_base = 0x1000 ///< Recommended base for user tags.
} DW_TAG;

void llvm_add_module_flag_int(LLVMModuleRef          module_ref,
                              LLVMModuleFlagBehavior behavior,
                              const char *           key,
                              s32                    val);
s32  llvm_get_dwarf_version(void);

LLVMDIBuilderRef llvm_di_new_di_builder(LLVMModuleRef module_ref);

void llvm_di_delete_di_builder(LLVMDIBuilderRef builder_ref);

void llvm_di_builder_finalize(LLVMDIBuilderRef builder_ref);

LLVMMetadataRef llvm_di_create_compile_unit(LLVMDIBuilderRef builder_ref,
                                            LLVMMetadataRef  file_ref,
                                            const char *     producer);

LLVMMetadataRef
llvm_di_create_file(LLVMDIBuilderRef builder_ref, const char *filename, const char *dir);

LLVMMetadataRef llvm_di_create_fn_fwd_decl(LLVMDIBuilderRef builder_ref,
                                           LLVMMetadataRef  scope_ref,
                                           const char *     name,
                                           const char *     linkage_name,
                                           LLVMMetadataRef  file_ref,
                                           unsigned         line,
                                           LLVMMetadataRef  type_ref,
                                           unsigned         scope_line);

LLVMMetadataRef llvm_di_create_lexical_scope(LLVMDIBuilderRef builder_ref,
                                             LLVMMetadataRef  scope_ref,
                                             LLVMMetadataRef  file_ref,
                                             unsigned         line,
                                             unsigned         col);
LLVMMetadataRef llvm_di_create_fn(LLVMDIBuilderRef builder_ref,
                                  LLVMMetadataRef  scope_ref,
                                  const char *     name,
                                  const char *     linkage_name,
                                  LLVMMetadataRef  file_ref,
                                  unsigned         line,
                                  LLVMMetadataRef  type_ref,
                                  unsigned         scope_line);

LLVMMetadataRef llvm_di_create_auto_variable(LLVMDIBuilderRef builder_ref,
                                             LLVMMetadataRef  scope_ref,
                                             const char *     name,
                                             LLVMMetadataRef  file_ref,
                                             unsigned         line,
                                             LLVMMetadataRef  type_ref);

LLVMMetadataRef llvm_di_create_global_variable(LLVMDIBuilderRef builder_ref,
                                               LLVMMetadataRef  scope_ref,
                                               const char *     name,
                                               LLVMMetadataRef  file_ref,
                                               unsigned         line,
                                               LLVMMetadataRef  type_ref);

LLVMMetadataRef llvm_di_create_global_variable_expression(LLVMDIBuilderRef builder_ref,
                                                          LLVMMetadataRef  scope_ref,
                                                          const char *     name,
                                                          LLVMMetadataRef  file_ref,
                                                          unsigned         line,
                                                          LLVMMetadataRef  type_ref);

LLVMMetadataRef llvm_di_replace_temporary(LLVMDIBuilderRef builder_ref,
                                          LLVMMetadataRef  temp_ref,
                                          LLVMMetadataRef  replacement_ref);

void llvm_di_set_current_location(LLVMBuilderRef  builder_ref,
                                  unsigned        line,
                                  unsigned        col,
                                  LLVMMetadataRef scope_ref,
                                  bool            implicit);

void llvm_di_reset_current_location(LLVMBuilderRef builder_ref);

LLVMMetadataRef llvm_di_create_replecable_composite_type(LLVMDIBuilderRef builder_ref,
                                                         DW_TAG           tag,
                                                         const char *     name,
                                                         LLVMMetadataRef  scope_ref,
                                                         LLVMMetadataRef  file_ref,
                                                         unsigned         line);

LLVMMetadataRef llvm_di_create_basic_type(LLVMDIBuilderRef builder_ref,
                                          const char *     name,
                                          unsigned         size_in_bits,
                                          DW_ATE_Encoding  encoding);

LLVMMetadataRef llvm_di_create_function_type(LLVMDIBuilderRef builder_ref,
                                             LLVMMetadataRef *params,
                                             unsigned         paramsc);

LLVMMetadataRef llvm_di_create_pointer_type(LLVMDIBuilderRef builder_ref,
                                            LLVMMetadataRef  pointee_type_ref,
                                            u64              size_in_bits,
                                            u32              align_in_bits,
                                            const char *     name);

LLVMMetadataRef llvm_di_create_array_type(LLVMDIBuilderRef builder_ref,
                                          u64              size_in_bits,
                                          u32              align_in_bits,
                                          LLVMMetadataRef  type_ref,
                                          u64              elem_count);

LLVMMetadataRef llvm_di_create_enum_type(LLVMDIBuilderRef builder_ref,
                                         LLVMMetadataRef  scope_ref,
                                         const char *     name,
                                         LLVMMetadataRef  file_ref,
                                         unsigned         line,
                                         u64              size_in_bits,
                                         u32              align_in_bits,
                                         LLVMMetadataRef *elems,
                                         usize            elemsc,
                                         LLVMMetadataRef  type_ref);

LLVMMetadataRef llvm_di_create_enum_variant(LLVMDIBuilderRef builder_ref,
                                            const char *     name,
                                            u64              val,
                                            bool             is_unsigned);

LLVMMetadataRef llvm_di_create_null_type(LLVMDIBuilderRef builder_ref);

LLVMMetadataRef llvm_di_create_struct_type(LLVMDIBuilderRef builder_ref,
                                           LLVMMetadataRef  scope_ref,
                                           const char *     name,
                                           LLVMMetadataRef  file_ref,
                                           unsigned         line,
                                           u64              size_in_bits,
                                           u32              align_in_bits,
                                           LLVMMetadataRef *elems,
                                           u64              elemsc);

LLVMMetadataRef llvm_di_create_union_type(LLVMDIBuilderRef builder_ref,
                                          LLVMMetadataRef  scope_ref,
                                          const char *     name,
                                          LLVMMetadataRef  file_ref,
                                          unsigned         line,
                                          u64              size_in_bits,
                                          u32              align_in_bits,
                                          LLVMMetadataRef *elems,
                                          u64              elemsc);

LLVMMetadataRef llvm_di_create_member_type(LLVMDIBuilderRef builder_ref,
                                           LLVMMetadataRef  scope_ref,
                                           const char *     name,
                                           LLVMMetadataRef  file_ref,
                                           unsigned         line,
                                           u64              size_in_bits,
                                           u32              align_in_bits,
                                           u64              offset_in_bits,
                                           LLVMMetadataRef  type_ref);

void llvm_di_set_subprogram(LLVMValueRef fn_ref, LLVMMetadataRef subprogram_ref);

void llvm_di_finalize_subprogram(LLVMDIBuilderRef builder_ref, LLVMMetadataRef subprogram_ref);

void llvm_di_insert_declare(LLVMDIBuilderRef  builder_ref,
                            LLVMValueRef      storage_ref,
                            LLVMMetadataRef   var_info_ref,
                            unsigned          line,
                            unsigned          col,
                            LLVMMetadataRef   scope_ref,
                            LLVMBasicBlockRef bb_ref);

#ifdef __cplusplus
}
#endif

#endif
