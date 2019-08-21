//************************************************************************************************
// bl
//
// File:   small_array.h
// Author: Martin Dorazil
// Date:   8/21/19
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
//************************************************************************************************

#ifndef BL_SMALL_ARRAY_H
#define BL_SMALL_ARRAY_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SmallArrayType(N, T, S)                                                                    \
	typedef struct SmallArray_##N {                                                            \
		T      tmp[S];                                                                     \
		T *    data;                                                                       \
		size_t allocated;                                                                  \
		size_t size;                                                                       \
	} SmallArray_##N;                                                                          \
                                                                                                   \
	static inline void sa_init_##N(SmallArray_##N *arr)                                        \
	{                                                                                          \
		arr->data      = &arr->tmp[0];                                                     \
		arr->allocated = 0;                                                                \
		arr->size      = 0;                                                                \
	}                                                                                          \
                                                                                                   \
	static inline void sa_terminate_##N(SmallArray_##N *arr)                                   \
	{                                                                                          \
		if (arr->allocated > 0) {                                                          \
			free(arr->data);                                                           \
		}                                                                                  \
		arr->size      = 0;                                                                \
		arr->allocated = 0;                                                                \
		arr->data      = &arr->tmp[0];                                                     \
	}                                                                                          \
                                                                                                   \
	static inline void sa_push_##N(SmallArray_##N *arr, T v)                                   \
	{                                                                                          \
		const bool on_heap = arr->allocated;                                               \
		if (!on_heap && arr->size == S) {                                                  \
			puts("alloc");                                                             \
			arr->allocated = S * 2;                                                    \
			arr->data      = (T *)malloc(sizeof(T) * arr->allocated);                  \
			memcpy(arr->data, arr->tmp, sizeof(T) * S);                                \
		} else if (on_heap && arr->size == arr->allocated) {                               \
			puts("realloc");                                                           \
			arr->allocated *= 2;                                                       \
			arr->data = (T *)realloc(arr->data, arr->allocated * sizeof(T));           \
		}                                                                                  \
		arr->data[arr->size++] = v;                                                        \
	}

#endif
