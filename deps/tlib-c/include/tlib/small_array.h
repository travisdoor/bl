// =================================================================================================*******************
// tlib-c
//
// File:   small_array.h
// Author: Martin Dorazil
// Date:   8/21/19
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
// =================================================================================================*******************

#ifndef T_SMALL_ARRAY_H
#define T_SMALL_ARRAY_H

#include "tlib/common.h"

#define TSA_FOREACH(arr, it)                                                                       \
    if ((arr)->size)                                                                               \
        for (size_t i = 0; i < (arr)->size && ((it) = (arr)->data[i]); ++i)

typedef struct TSmallArrayAny {
    void * data;
    size_t size;
    size_t allocated;
    char   tmp[1];
} TSmallArrayAny;

/*
 * Generates small array with suffix N, type T and static size S. Memory allocations are produced
 * only when count of elements is going to be greater than S value.
 */
#define TSMALL_ARRAY_TYPE(N, T, S)                                                                 \
    typedef struct TSmallArray_##N {                                                               \
        T *    data;                                                                               \
        size_t size;                                                                               \
        size_t allocated;                                                                          \
        T      tmp[S];                                                                             \
    } TSmallArray_##N;                                                                             \
                                                                                                   \
    static inline void tsa_resize_##N(TSmallArray_##N *arr, size_t desired_size)                   \
    {                                                                                              \
        if (desired_size <= S) goto SETUP;                                                         \
        if (desired_size <= arr->allocated) goto SETUP;                                            \
        if (arr->allocated == 0) {                                                                 \
            arr->data = (T *)malloc(desired_size * sizeof(T));                                     \
            if (!arr->data) {                                                                      \
                abort();                                                                           \
            }                                                                                      \
        } else {                                                                                   \
            T *tmp = arr->data;                                                                    \
            if ((arr->data = (T *)realloc(arr->data, desired_size * sizeof(T))) == NULL) {         \
                free(tmp);                                                                         \
                abort();                                                                           \
            }                                                                                      \
        }                                                                                          \
                                                                                                   \
        arr->allocated = desired_size;                                                             \
    SETUP:                                                                                         \
        arr->size = desired_size;                                                                  \
    }                                                                                              \
                                                                                                   \
    static inline void tsa_push_##N(TSmallArray_##N *arr, T v)                                     \
    {                                                                                              \
        const bool on_heap = arr->allocated;                                                       \
        if (!on_heap && arr->size == S) {                                                          \
            arr->allocated = S * 2;                                                                \
            arr->data      = (T *)malloc(sizeof(T) * arr->allocated);                              \
            if (!arr->data) {                                                                      \
                abort();                                                                           \
            }                                                                                      \
            memcpy(arr->data, arr->tmp, sizeof(T) * S);                                            \
        } else if (on_heap && arr->size == arr->allocated) {                                       \
            arr->allocated *= 2;                                                                   \
            T *tmp = arr->data;                                                                    \
            if ((arr->data = (T *)realloc(arr->data, arr->allocated * sizeof(T))) == NULL) {       \
                free(tmp);                                                                         \
                abort();                                                                           \
            }                                                                                      \
        }                                                                                          \
        arr->data[arr->size++] = v;                                                                \
    }                                                                                              \
                                                                                                   \
    static inline T tsa_pop_##N(TSmallArray_##N *arr)                                              \
    {                                                                                              \
        return arr->data[--arr->size];                                                             \
    }                                                                                              \
                                                                                                   \
    static inline T tsa_last_##N(TSmallArray_##N *arr)                                             \
    {                                                                                              \
        if (!arr->size) TABORT("Cannot get last element from empty array.");                       \
        return arr->data[arr->size - 1];                                                           \
    }

/* Initialize small array. */
#define tsa_init(Arr) _tsa_init((TSmallArrayAny *)(Arr))
static inline void _tsa_init(TSmallArrayAny *arr)
{
    arr->data      = &arr->tmp[0];
    arr->allocated = 0;
    arr->size      = 0;
}

/* Initialize terminate small array. */
#define tsa_terminate(Arr) _tsa_terminate((TSmallArrayAny *)(Arr))
static inline void _tsa_terminate(TSmallArrayAny *arr)
{
    if (arr->allocated > 0) {
        free(arr->data);
    }
    arr->size      = 0;
    arr->allocated = 0;
    arr->data      = NULL;
}

#endif
