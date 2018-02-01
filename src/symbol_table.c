//*****************************************************************************
// bl 
//
// File:   symbol_table.c
// Author: Martin Dorazil
// Date:   01/02/2018
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

#include "symbol_table.h"

/* SymbolTable members */
bo_decl_members_begin(SymbolTable, BObject)
bo_end();

/* SymbolTable constructor parameters */
bo_decl_params_begin(SymbolTable)
bo_end();

bo_impl_type(SymbolTable, BObject);

/* SymbolTable class init */
void
SymbolTableKlass_init(SymbolTableKlass *klass)
{
}

/* SymbolTable constructor */
void
SymbolTable_ctor(SymbolTable *self, SymbolTableParams *p)
{
}

/* SymbolTable destructor */
void
SymbolTable_dtor(SymbolTable *self)
{
}

/* SymbolTable copy constructor */
bo_copy_result
SymbolTable_copy(SymbolTable *self, SymbolTable *other)
{
  return BO_NO_COPY;
}

/* public */
SymbolTable *
bl_symbol_table_new(void)
{
  SymbolTableParams p = {
  };
  
  return bo_new(SymbolTable, &p);
}

