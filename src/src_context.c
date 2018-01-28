//*****************************************************************************
// bl 
//
// File:   src_context.c
// Author: Martin Dorazil
// Date:   28/01/2018
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

#include "src_context.h"

/* SrcContext constructor parameters */
bo_decl_params_begin(SrcContext)
bo_end();

bo_impl_type(SrcContext, BObject);

/* SrcContext class init */
void
SrcContextKlass_init(SrcContextKlass *klass)
{
}

/* SrcContext constructor */
void
SrcContext_ctor(SrcContext *self, SrcContextParams *p)
{
  self->includes = bo_string_new(256);
  self->fdecl    = bo_string_new(256);
  self->impl     = bo_string_new(1024);
}

/* SrcContext destructor */
void
SrcContext_dtor(SrcContext *self)
{
  bo_unref(self->includes);
  bo_unref(self->fdecl);
  bo_unref(self->impl);
}

/* SrcContext copy constructor */
bo_copy_result
SrcContext_copy(SrcContext *self, SrcContext *other)
{
  return BO_NO_COPY;
}

/* public */
SrcContext *
bl_src_context_new(void)
{
  return bo_new(SrcContext, NULL);
}

