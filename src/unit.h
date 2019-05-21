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
#include "tokens.h"
#include <llvm-c/Core.h>

struct Token;

/* class Unit object members */
typedef struct Unit {
	Tokens        tokens;
	Ast *         ast;
	BArray *      globals;
	char *        filepath;
	char *        dirpath;
	char *        name;
	char *        src;
	struct Token *loaded_from;
} Unit;

Unit *
unit_new_file(const char *filepath, struct Token *loaded_from, Unit *parent_unit);

void
unit_delete(Unit *unit);

const char *
unit_get_src_ln(Unit *unit, int32_t line, long *len);

#endif
