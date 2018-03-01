//*****************************************************************************
// Biscuit Engine
//
// File:   array.c
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

#include "containers/array_impl.h"
#include "bl/bldebug.h"
#include "bl/blmemory.h"

#define ARRAY_DEFAULT_CAPACITY 2

#define max(x, y) (((x) > (y)) ? (x) : (y))

#if BO_DEBUG
#define validate_index(vec, i)                                                                     \
    bl_assert((i) < (vec)->size && (i) >= 0, "vector index out of range!!!");
#define validate_iter(array, iter)                                                                   \
    bl_assert((iter) != NULL && (iter)->opaque >= (void *)(array)->data &&                           \
                  (iter)->opaque <= bl_array_end(array).opaque, "invalid iterator")
#else
#define validate_index(vec, i)                                                                     \
    do {                                                                                           \
    } while (0)

#define validate_iter(vec, node)                                                                   \
    do {                                                                                           \
    } while (0)
#endif

static inline void
unref_range(bl_array_t *array,
            const char *begin,
            size_t count)
{
  const char *end = begin + array->data_size * count;
  while (begin != end) {
    bl_free(&begin);
    begin += array->data_size;
  }
}

static inline char *
item_ptr(bl_array_t *array,
         const size_t i)
{
  return array->data + i * array->data_size;
}

/* public */
bl_array_t *
bl_array_new(size_t data_size,
             bl_array_data_dtor data_dtor)
{
  bl_array_t *array = bl_malloc(sizeof(bl_array_t));
  bl_array_init(array, data_size, data_dtor);
  return array;
}

void
bl_array_delete(bl_array_t *array)
{
  bl_array_terminate(array);
  free(array);
}

void
bl_array_init(bl_array_t *array,
              size_t data_size,
              bl_array_data_dtor data_dtor)
{
  array->data_size = data_size;
  array->data_dtor = data_dtor;
}

void
bl_array_terminate(bl_array_t *array)
{
  bl_array_clear(array);
}

void
bl_array_clear(bl_array_t *array)
{
  if (array->data_dtor) {
    unref_range(array, array->data, array->size);
  }

  array->size = 0;
}

void
bl_array_reset(bl_array_t *array)
{
  bl_array_clear(array);
  array->capacity = 0;
  bl_free(array->data);
  array->data = NULL;
}

void
bl_array_reserve(bl_array_t *array,
                 size_t capacity)
{
  if (capacity == 0 || capacity <= array->capacity)
    return;

  array->data = bl_realloc(array->data, array->data_size * capacity);
  if (!array->data) bl_abort("cannot resize array to capacity: %zu", capacity);

  array->capacity = capacity;
}

void
bl_array_resize(bl_array_t *array,
                size_t size)
{
  if (array->size == size)
    return;

  if (array->size < size) {
    // expand the bl_vector
    if (size > array->capacity) {
      bl_array_reserve(array, size);
    }
    // set all new elements to default state
    memset(item_ptr(array, array->size), 0, (size - array->size) * array->data_size);
  } else if (array->data_dtor) {
    // destroy overlapping elements
    unref_range(array, item_ptr(array, size), array->size - size);
  }

  array->size = size;
}

void
_bl_array_push_back(bl_array_t *array,
                    void *item)
{
  bl_assert(item, "push back null item");

  // empty bl_vector
  if (!array->data) {
    bl_array_reserve(array, ARRAY_DEFAULT_CAPACITY);
  }
  // double if needed
  if (array->size >= array->capacity) {
    bl_array_reserve(array, array->capacity * 2);
  }

  char *new_elem = item_ptr(array, array->size);
  array->size++;

  memcpy(new_elem, item, array->data_size);
}

void
bl_array_insert(bl_array_t *array,
                size_t index,
                void *data,
                size_t count)
{
  if (count == 0)
    return;

  bl_assert(index >= 0, "invalid index");

  // elems to the end of array
  const size_t diff = array->size - index;
  const size_t needed_capacity = array->size - diff + count;

  if (array->data_dtor)
    unref_range(array, item_ptr(array, array->size - diff), diff);

  if (array->capacity < needed_capacity) {
    bl_array_reserve(array, needed_capacity);
  }

  memcpy(item_ptr(array, index), data, count * array->data_size);
  array->size = max(array->size, array->size - diff + count);
}

void *
_bl_array_at(bl_array_t *array,
             size_t index)
{
  validate_index(array, index);
  return &array->data[index * array->data_size];
}

void
bl_array_pop_back(bl_array_t *array)
{
  if (array->size == 0) {
    return;
  }

  array->size--;
  if (array->data_dtor) {
    void *ptr = item_ptr(array, array->size);
    array->data_dtor(ptr);
    bl_free(ptr);
  }
}

void
bl_array_erase(bl_array_t *array,
               size_t i)
{
  bl_assert(array->size > 0, "calling erase on empty vector");
  validate_index(array, i);

  // single element in bl_vector or last have to be erased
  if (array->size == 1 || i == array->size - 1) {
    bl_array_pop_back(array);
  } else {
    if (array->data_dtor) {
      void *ptr = item_ptr(array, array->size);
      array->data_dtor(ptr);
      bl_free(ptr);
    }

    array->size--;
    memcpy(item_ptr(array, i), item_ptr(array, array->size), array->data_size);
  }
}

