//*****************************************************************************
// tlib-c
//
// File:   array.c
// Author: Martin Dorazil
// Date:   29/9/2019
//
// Copyright 2019 Martin Dorazil
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

#include "tlib/array.h"
#include "tmemory.h"

#define ALLOC_BLOCK_SIZE 32
#define ELEM_PTR(_i) (&((u8 *)arr->data)[(_i)*arr->elem_size])

static void ensure_space(TArray *arr, usize space, bool exact)
{
    if (!space) return;
    if (arr->allocated >= space) return;
    if (!exact) {
        space = space == 1 ? ALLOC_BLOCK_SIZE : space * 2;
    }
    void *tmp = tmalloc(space * arr->elem_size);
    if (arr->size) {
        memcpy(tmp, arr->data, arr->size * arr->elem_size);
    }
    tfree(arr->data);
    arr->data      = tmp;
    arr->allocated = space;
}

/* public */

TArray *tarray_new(usize elem_size)
{
    TArray *arr = tmalloc(sizeof(TArray));
    if (!arr) TABORT("Bad alloc.");

    tarray_init(arr, elem_size);
    return arr;
}

void tarray_delete(TArray *arr)
{
    if (!arr) return;
    tarray_terminate(arr);

    tfree(arr);
}

void tarray_init(TArray *arr, usize elem_size)
{
    if (!elem_size) TABORT("Size of array element cannot be 0.");
    arr->data      = NULL;
    arr->size      = 0;
    arr->allocated = 0;
    arr->elem_size = elem_size;
}

void tarray_terminate(TArray *arr)
{
    tfree(arr->data);
    arr->data      = NULL;
    arr->allocated = 0;
    arr->size      = 0;
}

void tarray_reserve(TArray *arr, usize size)
{
    if (!size) return;
    ensure_space(arr, size, true);
}

void tarray_clear(TArray *arr)
{
    arr->size = 0;
}

void *_tarray_push(TArray *arr, void *v_ptr)
{
    ensure_space(arr, arr->size + 1, false);

    void *elem_ptr = ELEM_PTR(arr->size);
    if (v_ptr) {
        memcpy(elem_ptr, v_ptr, arr->elem_size);
    }

    arr->size += 1;
    return elem_ptr;
}

void *_tarray_at(TArray *arr, usize i)
{
    if (i > arr->size) TABORT("Array index out of the bounds.");
    return ELEM_PTR(i);
}

void tarray_pop(TArray *arr)
{
    if (arr->size > 0) arr->size--;
}

void tarray_erase(TArray *arr, usize i)
{
    if (i >= arr->size) abort();
    // single element in bo_vector or last have to be erased
    if (arr->size == 1 || i == arr->size - 1) {
        tarray_pop(arr);
    } else {
        arr->size--;
        memcpy(ELEM_PTR(i), ELEM_PTR(arr->size), arr->elem_size);
    }
}
