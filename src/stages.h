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

#include "assembly.h"
#include "builder.h"
#include "error.h"
#include "unit.h"

void file_loader_run(Unit *unit);
void lexer_run(Unit *unit);
void token_printer_run(Unit *unit);
void parser_run(Assembly *assembly, Unit *unit);
void conf_parser_run(Unit *unit, ConfData *out_data);
void ast_printer_run(Assembly *assembly, FILE *stream);
void docs_run(Assembly *assembly);
void ir_run(Assembly *assembly);
void ir_opt_run(Assembly *assembly);
void obj_writer_run(Assembly *assembly);
void linker_run(Assembly *assembly);
void bc_writer_run(Assembly *assembly);
void native_bin_run(Assembly *assembly);
void mir_writer_run(Assembly *assembly);
s32  vm_entry_run(Assembly *assembly);
s32  vm_build_entry_run(Assembly *assembly);
s32  vm_tests_run(Assembly *assembly);

#endif
