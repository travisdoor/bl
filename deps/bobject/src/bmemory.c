//*****************************************************************************
// Biscuit Object
//
// File:   bmemory.c
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

#include "bmemory_impl.h"

/* set default implementations */
bo_malloc_f bo_malloc = malloc;
bo_calloc_f bo_calloc = calloc;
bo_realloc_f bo_realloc = realloc;
bo_free_f bo_free = free;

void
bo_set_memory(bo_mem_configuration *config)
{
  bo_malloc   = config->bo_malloc;
  bo_calloc   = config->bo_calloc;
  bo_free     = config->bo_free;
  bo_realloc  = config->bo_realloc;
}
