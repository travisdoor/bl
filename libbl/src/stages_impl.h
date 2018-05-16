//************************************************************************************************
// bl
//
// File:   stages_impl.h
// Author: Martin Dorazil
// Date:   02/03/2018
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

#ifndef BL_STAGES_IMPL_H
#define BL_STAGES_IMPL_H

#include "builder_impl.h"
#include "unit_impl.h"
#include "assembly_impl.h"
#include "bl/error.h"

/*
 * per unit
 */
bl_error_e
bl_file_loader_run(bl_builder_t *builder, bl_unit_t *unit);

bl_error_e
bl_lexer_run(bl_builder_t *builder, bl_unit_t *unit);

bl_error_e
bl_token_printer_run(bl_unit_t *unit);

bl_error_e
bl_parser_run(bl_builder_t *builder, bl_unit_t *unit);

bl_error_e
bl_preproc_run(bl_builder_t *builder, bl_unit_t *unit, bl_assembly_t *assembly);

/*
 * per assembly
 */
bl_error_e
bl_llvm_bc_writer_run(bl_builder_t *builder, bl_assembly_t *assembly);

bl_error_e
bl_merge_run(bl_builder_t *builder, bl_assembly_t *assembly);

bl_error_e
bl_connect_run(bl_builder_t *builder, bl_assembly_t *assembly);

bl_error_e
bl_check_run(bl_builder_t *builder, bl_assembly_t *assembly);

bl_error_e
bl_llvm_gen_run(bl_builder_t *builder, bl_assembly_t *assembly);

bl_error_e
bl_llvm_linker_run(bl_builder_t *builder, bl_assembly_t *assembly);

bl_error_e
bl_ast_printer_run(bl_assembly_t *assembly);

bl_error_e
bl_llvm_jit_exec_run(bl_builder_t *builder, bl_assembly_t *assembly);

bl_error_e
bl_llvm_native_bin_run(bl_builder_t *builder, bl_assembly_t *assembly);

bl_error_e
bl_evaluator_run(bl_builder_t *builder, bl_assembly_t *assembly);

#endif // BL_STAGES_IMPL_H
