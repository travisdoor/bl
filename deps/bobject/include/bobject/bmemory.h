//*****************************************************************************
// Biscuit Object
//
// File:   bmemory.h
// Author: Martin Dorazil
// Date:   27/10/2017
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

#ifndef BMEMORY_H_BQT6JHMT
#define BMEMORY_H_BQT6JHMT

#include <stdlib.h>
#include "bobject/utils.h"

BO_BEGIN_DECLS
/* memory configuration */

/* Malloc function type */
typedef void *(*bo_malloc_f)(size_t);

/* Calloc function type */
typedef void *(*bo_calloc_f)(size_t, size_t);

/* Free function type */
typedef void(*bo_free_f)(void *);

/* Realloc function type */
typedef void *(*bo_realloc_f)(void *, size_t);

/**
 * Structure used for setting allocator used by bobject.
 */
typedef struct _bo_mem_configuration bo_mem_configuration;
struct _bo_mem_configuration {
  /* pointer to malloc method */
  bo_malloc_f bo_malloc;

  /* pointer to calloc method */
  bo_calloc_f bo_calloc;

  /* pointer to free method */
  bo_free_f bo_free;

  /* pointer to realloc function */
  bo_realloc_f bo_realloc;
};

/**
 * Sets custom memory functions calloc and free. When custom methods
 * are not presented the default one will be used.
 * @param config Memory methods.
 */
extern BO_EXPORT void
bo_set_memory(bo_mem_configuration *config);

BO_END_DECLS

#endif /* end of include guard: BMEMORY_H_BQT6JHMT */
