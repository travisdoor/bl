//*****************************************************************************
// bl
//
// File:   sym_tbl.c
// Author: Martin Dorazil
// Date:   13/02/2018
//
// Copyright 2017 Martin Dorazil
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

#include "bl/sym_tbl.h"
/* class SymTbl */
/* class SymTbl object members */
bo_decl_members_begin(SymTbl, BObject)
  /* members */
bo_end();

bo_impl_type(SymTbl, BObject);

void
SymTblKlass_init(SymTblKlass *klass)
{
}

void
SymTbl_ctor(SymTbl *self, SymTblParams *p)
{
  /* constructor */
}

void
SymTbl_dtor(SymTbl *self)
{
}

bo_copy_result
SymTbl_copy(SymTbl *self, SymTbl *other)
{
  return BO_NO_COPY;
}
/* class SymTbl end */

SymTbl *
bl_sym_tbl_new(void)
{
  return bo_new(SymTbl, NULL);
}

