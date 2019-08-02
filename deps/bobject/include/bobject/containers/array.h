//*****************************************************************************
// Biscuit Object
//
// File:   array.h
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

#ifndef ARRAY_H_L7KXVRWJ
#define ARRAY_H_L7KXVRWJ

#include "bobject/bobject.h"
#include "bobject/containers/iterator.h"

BO_BEGIN_DECLS

/* class declaration */
bo_decl_type_begin(BArray, BObject)
bo_end();

/* class constructor parameters */
bo_decl_params_begin(BArray)
  BType type;
  size_t data_size;
  bool managed;
bo_end();

/**
 * @brief Create new array instance.
 *
 * This verson of array can be used for non-object types.
 * @param data_size Size of one element.
 * @return New array object.
 */
extern BO_EXPORT BArray *
bo_array_new(size_t data_size);

/**
 * @brief Create new array instance.
 *
 * This version can be used for storing references to objects. All objects are unreferenced
 * when managed is true.
 * @param type Object type (bo_typeof)
 * @param managed Objects are unreferenced when element has been removed or when array is
 * destroyed.
 * @return
 */
extern BO_EXPORT BArray *
bo_array_new_bo(BType type,
                bool managed);

/**
 * @brief Clear all elements.
 * @param self
 */
extern BO_EXPORT void
bo_array_clear(BArray *self);

/**
 * @brief Reset array to initial state (also free all allocated memory)
 * @param self
 */
extern BO_EXPORT void
bo_array_reset(BArray *self);

/**
 * @brief Preallocate array to defined capacity. Size of the array does not change.
 * @param self
 * @param capacity Desired capacity (element count not size).
 */
extern BO_EXPORT void
bo_array_reserve(BArray *self,
                 size_t capacity);

/**
 * @brief Resize array to specified object count.
 * @param self
 * @param size Element count.
 */
extern BO_EXPORT void
bo_array_resize(BArray *self,
                size_t size);

/**
 * @brief Push element at the end of array. This may cause memory reallocation.
 */
#define bo_array_push_back(self, item) \
  _bo_array_push_back(self, &(item))

extern BO_EXPORT void
_bo_array_push_back(BArray *self,
                    void *item);

/**
 * @brief Insert chunk of data into array.
 * @param self
 * @param index Begin.
 * @param data Data to be inserted.
 * @param count Count of elements.
 */
extern BO_EXPORT void
bo_array_insert(BArray *self,
                size_t index,
                void *data,
                size_t count);

/**
 * @brief Get data on index.
 */
#define bo_array_at(self, index, type) \
  (*(type *)_bo_array_at(self, index))

extern BO_EXPORT void *
_bo_array_at(BArray *self,
             size_t index);

/**
 * @brief Remove one element from the array end.
 * @param self
 */
extern BO_EXPORT void
bo_array_pop_back(BArray *self);

/**
 * @brief Erase one element on index.
 * @param self
 * @param i Index to be erased.
 */
extern BO_EXPORT void
bo_array_erase(BArray *self,
               size_t i);

/**
 * @brief Get iterator pointing to array begin.
 * @param self
 * @return Begin.
 */
extern BO_EXPORT bo_iterator_t
bo_array_begin(BArray *self);

/**
 * @brief Get iterator pointing to array end.
 * @param self
 * @return End.
 */
extern BO_EXPORT bo_iterator_t
bo_array_end(BArray *self);

/**
 * @brief Is array empty?
 * @param self
 * @return Return true when array is empty.
 */
extern BO_EXPORT bool
bo_array_empty(BArray *self);

/**
 * @brief Get size array (element counr)
 * @param self
 * @return Count of elements in array.
 */
extern BO_EXPORT size_t
bo_array_size(BArray *self);

/**
 * @brief Preallocated capacity of array (element count which can be inserted
 * without reallocation).
 * @param self
 * @return Capacity.
 */
extern BO_EXPORT size_t
bo_array_capacity(BArray *self);

/**
 * @brief Get stored object type.
 * @param self
 * @return Type of the object.
 */
extern BO_EXPORT BType
bo_array_type(BArray *self);

/**
 * @brief Get pointer to raw data block.
 * @param self
 * @return Data.
 */
extern BO_EXPORT bo_byte_t *
bo_array_data(BArray *self);

/**
 * @brief Get size of element in bytes.
 * @param self
 * @return Element size.
 */
extern BO_EXPORT size_t
bo_array_data_size(BArray *self);

/**
 * @brief Move iterator to the next element.
 * @param self
 * @param iter Iterator.
 */
extern BO_EXPORT void
bo_array_iter_next(BArray *self,
                   bo_iterator_t *iter);

/**
 * @brief Get value by iterator.
 * @param self
 * @param iter Iterator.
 * @return
 */
#define bo_array_iter_peek(self, iter, type) \
  (*(type *)_bo_array_iter_peek(self, iter))

extern BO_EXPORT void *
_bo_array_iter_peek(BArray *self,
                    bo_iterator_t *iter);

BO_END_DECLS

#endif /* end of include guard: ARRAY_H_L7KXVRWJ */
