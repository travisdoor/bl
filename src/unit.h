//************************************************************************************************
// bl
//
// File:   unit.h
// Author: Martin Dorazil
// Date:   3/1/18
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

#ifndef BL_UNIT_H
#define BL_UNIT_H

#include "ast.h"
#include "config.h"
#include "scope.h"
#include "tokens.h"

struct Token;

/* class Unit object members */
typedef struct Unit {
	u64             hash; 
	Tokens          tokens;        /* Unit tokens as lexer output. */
	Ast *           ast;           /* Abstract Syntax Tree */
	struct Scope *  private_scope; /* Unit private scope (#private). */
	char *          filename;      /* Loaded source file name. */
	char *          filepath;      /* Loaded source file name with path. */
	char *          dirpath;       /* Parent directory. */
	char *          name;          /* Unit name */
	char *          src;           /* Unit raw source data. */
	struct Token *  loaded_from;   /* Optionally set when unit is loaded from another unit. */
	LLVMMetadataRef llvm_file_meta;
} Unit;

Unit *unit_new_file(const char *filepath, struct Token *loaded_from, Unit *parent_unit);
void unit_delete(Unit *unit);
const char *unit_get_src_ln(Unit *unit, s32 line, long *len);

#endif
