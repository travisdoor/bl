//*****************************************************************************
// Biscuit Object 
//
// File:   iterator.h
// Author: Martin Dorazil
// Date:   2.9.17
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

#ifndef ITERATOR_H_GBQFI6YG
#define ITERATOR_H_GBQFI6YG

#include <stdbool.h>
#include "bobject/bobject.h"

BO_BEGIN_DECLS

typedef struct bo_iterator
{
  const void *opaque;
} bo_iterator_t;

/**
 * @brief Compare two iterators.
 * @param first First iterator.
 * @param second Second iterator.
 * @return True when first is equal to second.
 */
inline static bool
bo_iterator_equal(const bo_iterator_t *first,
                  const bo_iterator_t *second)
{
  return first->opaque == second->opaque;
}

BO_END_DECLS

#endif /* end of include guard: ITERATOR_H_GBQFI6YG */
