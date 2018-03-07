//*****************************************************************************
// bl
//
// File:   unsatisfied.c
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

#include "unsatisfied_impl.h"

#define EXPECTED_COUNT 256

/* public */
void
bl_unsatisfied_init(bl_unsatisfied_t *uns)
{
  uns->unsatisfied = bo_array_new(sizeof(bl_node_t *));
  bo_array_reserve(uns->unsatisfied, EXPECTED_COUNT);
}

void
bl_unsatisfied_terminate(bl_unsatisfied_t *uns)
{
  bo_unref(uns->unsatisfied);
}

void
bl_unsatisfied_add(bl_unsatisfied_t *uns,
                   bl_node_t *node)

{
  bo_array_push_back(uns->unsatisfied, node);
}

int
bl_unsatisfied_get_count(bl_unsatisfied_t *uns)
{
  return (int) bo_array_size(uns->unsatisfied);
}

bl_node_t *
bl_unsatisfied_get_node(bl_unsatisfied_t *uns,
                        int i)
{
  return bo_array_at(uns->unsatisfied, (size_t) i, bl_node_t *);
}
