//*****************************************************************************
// bl
//
// File:   unsatisfied_impl.h
// Author: Martin Dorazil
// Date:   3/7/18
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

#ifndef BL_UNSATISFIED_IMPL_H
#define BL_UNSATISFIED_IMPL_H

#include <bobject/containers/array.h>
#include "ast/node_impl.h"

typedef struct
{
  BArray *unsatisfied;
} bl_unsatisfied_t;

void
bl_unsatisfied_init(bl_unsatisfied_t *uns);

void
bl_unsatisfied_terminate(bl_unsatisfied_t *uns);

void
bl_unsatisfied_add(bl_unsatisfied_t *uns,
                   bl_node_t *node);

int
bl_unsatisfied_get_count(bl_unsatisfied_t *uns);

bl_node_t *
bl_unsatisfied_get_node(bl_unsatisfied_t *uns,
                        int i);

#endif //BL_UNSATISFIED_IMPL_H
