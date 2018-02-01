//*****************************************************************************
// bl
//
// File:   csrc.c
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

#include "csrc.h"

/* class CSrc */

bo_impl_type(CSrc, BObject);

void
CSrcKlass_init(CSrcKlass *klass)
{
}

void
CSrc_ctor(CSrc *self,
          CSrcParams *p)
{
  for (int i = 0; i < BL_CSRC_IMPL_LAYER_COUNT; i++) {
    self->impl[i] = bo_string_new(256);
  }
}

void
CSrc_dtor(CSrc *self)
{
  for (int i = 0; i < BL_CSRC_IMPL_LAYER_COUNT; i++) {
    bo_unref(self->impl[i]);
  }
}

bo_copy_result
CSrc_copy(CSrc *self,
          CSrc *other)
{
  return BO_NO_COPY;
}

/* class CSrc end */

CSrc *
bl_csrc_new(void)
{
  return bo_new(CSrc, NULL);
}

BString *
bl_csrc_get_impl(CSrc *self)
{
  BString *impl = bo_string_new(1024);

  for (int i = 0; i < BL_CSRC_IMPL_LAYER_COUNT; i++) {
    if (bo_string_len(self->impl[i]) > 0) {
      bo_string_append_str(impl, self->impl[i]);
      if (i + 1 < BL_CSRC_IMPL_LAYER_COUNT)
        bo_string_append(impl, "\n");
    }
  }

  return impl;
}

void
bl_csrc_merge(CSrc *self,
              CSrc *other,
              bl_csrc_impl_layers layer)
{
  BString *dest = self->impl[layer];
  bo_string_append_str(dest, bl_csrc_get_impl(other));
}

