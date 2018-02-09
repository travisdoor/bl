//*****************************************************************************
// bl 
//
// File:   type_table.h
// Author: Martin Dorazil
// Date:   09/02/2018
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

#ifndef BISCUIT_TYPE_TABLE_H
#define BISCUIT_TYPE_TABLE_H

#include <bobject/bobject.h>

BO_BEGIN_DECLS

#define BL_TYPE_LIST \
  tp(REF, "") \
  tp(VOID, "void") \
  tp(I32, "int") \
  tp(I64, "long") \

typedef enum {
#define tp(tok, str) BL_TYPE_##tok,
  BL_TYPE_LIST
#undef tp
  BL_TYPE_COUNT
} bl_type_e;

extern BO_EXPORT char *bl_type_strings[];

extern BO_EXPORT bl_type_e
bl_strtotype(const char* str);

BO_END_DECLS

#endif /* end of include guard: BISCUIT_TYPE_TABLE_H */

