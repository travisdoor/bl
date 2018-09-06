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

#include <llvm-c/Core.h>
#include "config.h"
#include "ast.h"
#include "tokens.h"

/* class Unit object members */
typedef struct bl_unit
{
  /* output of lexer */
  tokens_t tokens;
  /* abstract syntax tree as output of parser */
  ast_t   ast;
  BArray *globals;
  /* source file name with path */
  char *filepath;
  char *name;
  /* source data */
  char *src;
} unit_t;

typedef struct bl_unit *bl_unit_ref;

bl_unit_ref
bl_unit_new_file(const char *filepath);

bl_unit_ref
bl_unit_new_str(const char *name, const char *src);

void
bl_unit_delete(bl_unit_ref unit);

const char *
bl_unit_get_src_file(bl_unit_ref unit);

const char *
bl_unit_get_src(bl_unit_ref unit);

const char *
bl_unit_get_src_ln(bl_unit_ref unit, int line, long *len);

const char *
bl_unit_get_name(bl_unit_ref unit);

#endif 
