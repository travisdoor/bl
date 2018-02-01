//*****************************************************************************
// bl
//
// File:   csrc.h
// Author: Martin Dorazil
// Date:   31/01/2018
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

#ifndef BL_CSRC_H
#define BL_CSRC_H

#include <bobject/bobject.h>
#include <bobject/containers/string.h>

typedef enum _bl_csrc_impl_layers
{
  BL_CSRC_IMPL_LAYER_INCLUDE = 0,
  BL_CSRC_IMPL_LAYER_TYPEDEF,
  BL_CSRC_IMPL_LAYER_DVAR,
  BL_CSRC_IMPL_LAYER_IVAR,
  BL_CSRC_IMPL_LAYER_DFUNC,
  BL_CSRC_IMPL_LAYER_IFUNC,
  BL_CSRC_IMPL_LAYER_COUNT
} bl_csrc_impl_layers;

/* class CSrc declaration */
bo_decl_type_begin(CSrc, BObject)
  /* virtuals */
bo_end();

/* class CSrc object members */
bo_decl_members_begin(CSrc, BObject)
  /* members */
  BString *impl[BL_CSRC_IMPL_LAYER_COUNT];
bo_end();

CSrc *
bl_csrc_new(void);

BString *
bl_csrc_get_impl(CSrc *self);

void
bl_csrc_merge(CSrc *self,
              CSrc *other,
              bl_csrc_impl_layers layer);

#endif //BL_CSRC_H
