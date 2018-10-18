//************************************************************************************************
// bl
//
// File:   stages.h
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

#ifndef BL_STAGES_H
#define BL_STAGES_H

#include "builder.h"
#include "unit.h"
#include "assembly.h"
#include "error.h"

/*
 * per unit
 */
void
file_loader_run(Builder *builder, Unit *unit);

void
lexer_run(Builder *builder, Unit *unit);

void
token_printer_run(Unit *unit);

void
parser_run(Builder *builder, Assembly *assembly, Unit *unit);

/*
 * per assembly
 */
void
ast_printer_run(Assembly *assembly);

void
checker_run(Builder *builder, Assembly *assembly);

void
ir_run(Builder *builder, Assembly *assembly);

void
post_run(Builder *builder, Assembly *assembly);

void
linker_run(Builder *builder, Assembly *assembly);

void
bc_writer_run(Builder *builder, Assembly *assembly);

void
native_bin_run(Builder *builder, Assembly *assembly);

void
jit_exec_run(Builder *builder, Assembly *assembly);

void
test_exec_run(Builder *builder, Assembly *assembly);

#endif
