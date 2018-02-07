//*****************************************************************************
// bl 
//
// File:   stage.c
// Author: Martin Dorazil
// Date:   04/02/2018
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

#include "stage_impl.h"

/* Stage constructor parameters */
bo_decl_params_begin(Stage)
bo_end();

bo_impl_type(Stage, BObject);

/* Stage class init */
void
StageKlass_init(StageKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->domain = NULL;
  bo_vtbl_cl(klass, Stage)->run = NULL;
}

/* Stage constructor */
void
Stage_ctor(Stage *self, StageParams *p)
{
}

/* Stage destructor */
void
Stage_dtor(Stage *self)
{
}

/* Stage copy constructor */
bo_copy_result
Stage_copy(Stage *self, Stage *other)
{
  return BO_NO_COPY;
}


