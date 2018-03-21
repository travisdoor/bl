//*****************************************************************************
// bl
//
// File:   unit_impl.h
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
//*****************************************************************************

#ifndef BL_UNIT_IMPL_H
#define BL_UNIT_IMPL_H

#include "bl/unit.h"
#include "ast/ast2_impl.h"
#include "tokens_impl.h"

/* class Unit object members */
typedef struct bl_unit
{
  /* output of lexer */
  bl_tokens_t tokens;
  /* abstract syntax tree as output of parser */
  bl_ast2_t ast;
  BArray *globals;
  /* source file name with path */
  char *filepath;
  char *name;
  /* source data */
  char *src;

} bl_unit_t;

#endif // BL_UNIT_IMPL_H
