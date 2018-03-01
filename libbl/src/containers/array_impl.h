//*****************************************************************************
// bl
//
// File:   array_impl.h
// Author: Martin Dorazil
// Date:   26/11/2017
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

#ifndef BL_ARRAY_IMPL
#define BL_ARRAY_IMPL

#include <stdlib.h>
#include <stdbool.h>

typedef void (*bl_array_data_dtor)(char *data);

typedef struct bl_array
{
  size_t size;
  size_t capacity;
  size_t data_size;
  char *data;
  bl_array_data_dtor data_dtor;
} bl_array_t;

/**
 * @brief Create new array instance.
 *
 * This verson of array can be used for non-object types.
 * @param data_size Size of one element.
 * @return New array object.
 */
bl_array_t *
bl_array_new(size_t data_size,
             bl_array_data_dtor data_dtor);

void
bl_array_delete(bl_array_t *array);

void
bl_array_init(bl_array_t *array,
              size_t data_size,
              bl_array_data_dtor data_dtor);

void
bl_array_terminate(bl_array_t *array);

/**
 * @brief Clear all elements.
 * @param array
 */
void
bl_array_clear(bl_array_t *array);

/**
 * @brief Reset array to initial state (also free all allocated memory)
 * @param array
 */
void
bl_array_reset(bl_array_t *array);

/**
 * @brief Preallocate array to defined capacity. Size of the array does not change.
 * @param array
 * @param capacity Desired capacity (element count not size).
 */
void
bl_array_reserve(bl_array_t *array,
                 size_t capacity);

/**
 * @brief Resize array to specified object count.
 * @param array
 * @param size Element count.
 */
void
bl_array_resize(bl_array_t *array,
                size_t size);

/**
 * @brief Push element at the end of array. This may cause memory reallocation.
 */
#define bl_array_push_back(array, item) \
  _bl_array_push_back(array, &(item))

void
_bl_array_push_back(bl_array_t *array,
                    void *item);

/**
 * @brief Get data on index.
 */
#define bl_array_at(array, index, type) \
  (*(type *)_bl_array_at(array, index))

void *
_bl_array_at(bl_array_t *array,
             size_t index);

/**
 * @brief Remove one element from the array end.
 * @param array
 */
void
bl_array_pop_back(bl_array_t *array);

/**
 * @brief Erase one element on index.
 * @param array
 * @param i Index to be erased.
 */
void
bl_array_erase(bl_array_t *array,
               size_t i);

#endif
