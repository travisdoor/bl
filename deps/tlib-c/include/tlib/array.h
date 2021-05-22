// =================================================================================================
// tlib-c
//
// File:   array.h
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
// =================================================================================================

#ifndef T_ARRAY_H
#define T_ARRAY_H

#include "tlib/common.h"

// Utility macro for iteration over an array. Expected element type must be specified as 'T', '_arr'
// is pointer to iterated array and '_it' specify name of variable used to fetch current value.
// Current index of the element is stored in implicit 'i' variable declared inside foreach
// scope.
#define TARRAY_FOREACH(T, _arr, _it)                                                               \
    if ((_arr) && (_arr)->size)                                                                    \
        for (usize i = 0; i < (_arr)->size && ((_it) = tarray_at(T, (_arr), i)); ++i)

// Push new element into the array. New reallocation can be done in case there is no space left in
// preallocated or reserved memory block.
// Returns pointer to new created element.
#define tarray_push(_arr, _v) _tarray_push((_arr), &(_v))

// Same as push but do not initialize the new element.
// Returns pointer to new created element.
#define tarray_push_empty(_arr) _tarray_push((_arr), NULL)

// Get array element at index in array size range (checked by assert).
#define tarray_at(T, _arr, _i) (*(T *)_tarray_at(_arr, (_i)))

// Return the first element in the array non-empty array (checked by assert).
#define tarray_front(T, _arr) (*(T *)_tarray_front(_arr))

// Return the last element in the array non-empty array (checked by assert).
#define tarray_back(T, _arr) (*(T *)_tarray_back(_arr))

typedef struct TArray {
    void *data;
    usize size; // Element count.
    usize allocated;
    usize elem_size;
} TArray;

// Create new array on heap. The 'elem_size' is size of one element in array greater than zero
// (checked by assert).
TAPI TArray *tarray_new(usize elem_size);

// Delete array on heap.
TAPI void tarray_delete(TArray *arr);

// Initialize array. The 'elem_size' is size of one element in array greater than zero (checked by
// assert).
TAPI void tarray_init(TArray *arr, usize elem_size);

// Terminate array.
TAPI void tarray_terminate(TArray *arr);

// Reserve allocated size for 'size' elements count.
TAPI void tarray_reserve(TArray *arr, usize count);

// Clear (remove all elements) array but keep allocations.
TAPI void tarray_clear(TArray *arr);

// Use macro instead.
TAPI void *_tarray_push(TArray *arr, void *v_ptr);

// Use macro instead.
TAPI void *_tarray_at(TArray *arr, usize i);

// Use macro instead.
TAPI void *_tarray_front(TArray *arr);

// Use macro instead.
TAPI void *_tarray_back(TArray *arr);

// Pop the last value.
TAPI void tarray_pop(TArray *arr);

// Erase value at index 'i'. The index must be in array size range (checked by assert).
TAPI void tarray_erase(TArray *arr, usize i);

#endif
