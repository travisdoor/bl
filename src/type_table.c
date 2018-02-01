//*****************************************************************************
// bl 
//
// File:   type_table.c
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

#include "type_table.h"

/* TypeTable members */
bo_decl_members_begin(TypeTable, BObject)
bo_end();

/* TypeTable constructor parameters */
bo_decl_params_begin(TypeTable)
bo_end();

bo_impl_type(TypeTable, BObject);

/* TypeTable class init */
void
TypeTableKlass_init(TypeTableKlass *klass)
{
}

/* TypeTable constructor */
void
TypeTable_ctor(TypeTable *self, TypeTableParams *p)
{
}

/* TypeTable destructor */
void
TypeTable_dtor(TypeTable *self)
{
}

/* TypeTable copy constructor */
bo_copy_result
TypeTable_copy(TypeTable *self, TypeTable *other)
{
  return BO_NO_COPY;
}

/* public */
TypeTable *
bl_type_table_new(void)
{
  TypeTableParams p = {
  };
  
  return bo_new(TypeTable, &p);
}

