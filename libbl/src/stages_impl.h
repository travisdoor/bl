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
void
file_loader_run(builder_t *builder, unit_t *unit);

void
lexer_run(builder_t *builder, unit_t *unit);

void
token_printer_run(unit_t *unit);

void
parser_run(builder_t *builder, assembly_t *assembly, unit_t *unit);

/*
 * per assembly
 */
void
ast_printer_run(assembly_t *assembly);

void
checker_run(builder_t *builder, assembly_t *assembly);

void
post_run(builder_t *builder, assembly_t *assembly);

void
linker_run(builder_t *builder, assembly_t *assembly);

void
bc_writer_run(builder_t *builder, assembly_t *assembly);

void
native_bin_run(builder_t *builder, assembly_t *assembly);

void
ir_run(builder_t *builder, assembly_t *assembly);

void
jit_exec_run(builder_t *builder, assembly_t *assembly);

#endif // BL_STAGES_IMPL_H
