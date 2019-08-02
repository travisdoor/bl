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

#include "bobject/containers/array.h"
#include "common_impl.h"

#define ARRAY_DEFAULT_CAPACITY 2

#define max(x, y) (((x) > (y)) ? (x) : (y))

#if BO_DEBUG
#define validate_index(vec, i)                                                                     \
    bo_assert((i) < (vec)->size && (i) >= 0, "vector index out of range!!!");
#define validate_iter(self, iter)                                                                   \
    bo_assert((iter) != NULL && (iter)->opaque >= (void *)(self)->data &&                           \
                  (iter)->opaque <= bo_array_end(self).opaque, "invalid iterator")
#else
#define validate_index(vec, i)                                                                     \
    do {                                                                                           \
    } while (0)

#define validate_iter(vec, node)                                                                   \
    do {                                                                                           \
    } while (0)
#endif

/* class BArray */

/* class members */
bo_decl_members_begin(BArray, BObject)
  BType         type;
  size_t        size;
  size_t        capacity;
  size_t        data_size;
  bo_byte_t    *data;
  bool          managed;
bo_end();

bo_impl_type(BArray, BObject);

/* BArray type initialization (static constructor) */
void
BArrayKlass_init(BArrayKlass *klass)
{
}

/* BArray constructor */
void
BArray_ctor(BArray       *self,
            BArrayParams *p)
{
  bo_assert(p, "invalid params");
  self->type      = p->type;
  self->managed   = p->managed;
  self->data_size = p->data_size;
}

/* BArray destructor */
void
BArray_dtor(BArray *self)
{
  bo_array_clear(self);
  bo_free(self->data);
}

bo_copy_result
BArray_copy(BArray *self,
            BArray *other)
{
  self->type      = other->type;
  self->size      = other->size;
  self->capacity  = other->capacity;
  self->data_size = other->data_size;
  self->managed   = other->managed;

  /* copy all data and increase reference count of stored objects */
  if (self->size > 0) {
    self->data = bo_calloc(other->data_size, other->size);
    memcpy(self->data, other->data, other->data_size * other->size);
    if (other->managed) {
      for (size_t i = self->size; i--;) {
        bo_ref(bo_array_at(self, i, BObject *));
      }
    }
  }

  return BO_COPY_SUCCESS;
}

/* class BArray end */

static inline void
unref_range(BArray          *self,
            const bo_byte_t *begin,
            size_t           count)
{
  const bo_byte_t *end = begin + self->data_size * count;
  while (begin != end) {
    bo_unref(*(BObject **)begin);
    begin += self->data_size;
  }
}

static inline bo_byte_t *
item_ptr(BArray        *self,
         const size_t   i)
{
  return self->data + i * self->data_size;
}

/* public */
BArray *
bo_array_new(size_t data_size)
{
  BArrayParams p;
  p.type = NULL;
  p.managed = false;
  p.data_size = data_size;

  return bo_new(BArray, &p);
}

BArray *
bo_array_new_bo(BType type,
                bool  managed)
{
  bo_assert(type, "invalid type");
  BArrayParams p;
  p.type = type;
  p.managed = managed;
  p.data_size = sizeof(BObject *);

  return bo_new(BArray, &p);
}

void
bo_array_clear(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  if (self->managed) {
    unref_range(self, bo_array_begin(self).opaque, self->size);
  }

  self->size = 0;
}

void
bo_array_reset(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  bo_array_clear(self);
  self->capacity = 0;
  bo_free(self->data);
  self->data = NULL;
}

void
bo_array_reserve(BArray *self,
                 size_t  capacity)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  if (capacity == 0 || capacity <= self->capacity)
    return;

  self->data = bo_realloc(self->data, self->data_size * capacity);
  if (!self->data)
    bo_abort("cannot resize array to capacity: %zu", capacity);

  self->capacity = capacity;
}

void
bo_array_resize(BArray *self,
                size_t  size)
{
  if (self->size == size)
    return;

  if (self->size < size) {
    // expand the bo_vector
    if (size > self->capacity) {
      bo_array_reserve(self, size);
    }
    // set all new elements to default state
    memset(item_ptr(self, self->size), 0, (size - self->size) * self->data_size);
  } else if (self->managed) {
    // destroy overlapping elements
    unref_range(self, item_ptr(self, size), self->size - size);
  }

  self->size = size;
}

void
_bo_array_push_back(BArray *self,
                    void   *item)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  bo_assert(item, "push back null item");

  // empty bo_vector
  if (!self->data) {
    bo_array_reserve(self, ARRAY_DEFAULT_CAPACITY);
  }
  // double if needed
  if (self->size >= self->capacity) {
    bo_array_reserve(self, self->capacity * 2);
  }

  bo_byte_t *new_elem = item_ptr(self, self->size);
  self->size++;

  memcpy(new_elem, item, self->data_size);
}

void
bo_array_insert(BArray *self,
                size_t  index,
                void   *data,
                size_t  count)
{
  if (count == 0)
    return;

  bo_assert(index >= 0, "invalid index");

  // elems to the end of array
  const size_t diff = self->size - index;
  const size_t needed_capacity = self->size - diff + count;

  if (self->managed)
    unref_range(self, item_ptr(self, self->size - diff), diff);

  if (self->capacity < needed_capacity) {
    bo_array_reserve(self, needed_capacity);
  }

  memcpy(item_ptr(self, index), data, count * self->data_size);
  self->size = max(self->size, self->size - diff + count);
}

void *
_bo_array_at(BArray *self,
             size_t  index)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  validate_index(self, index);
  return &self->data[index * self->data_size];
}

void
bo_array_pop_back(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  if (bo_array_empty(self)) {
    return;
  }

  self->size--;
  if (self->managed)
    bo_unref(*(BObject **)item_ptr(self, self->size));
}

void
bo_array_erase(BArray *self,
               size_t  i)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  bo_assert(!bo_array_empty(self), "calling erase on empty vector");
  validate_index(self, i);

  // single element in bo_vector or last have to be erased
  if (self->size == 1 || i == self->size - 1) {
    bo_array_pop_back(self);
  } else {
    if (self->managed)
      bo_unref(*(BObject **)item_ptr(self, i));

    self->size--;
    memcpy(item_ptr(self, i), item_ptr(self, self->size), self->data_size);
  }
}

/**
 * @brief Move iterator to the next element.
 * @param self
 * @param iter Iterator.
 */
void
bo_array_iter_next(BArray *self,
                   bo_iterator_t *iter)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  validate_iter(self, iter);
#ifdef __GNUC__
  iter->opaque += self->data_size;
#else
  (char *)iter->opaque += self->data_size;
#endif
}

/**
 * @brief Get value by iterator.
 * @param self
 * @param iter Iterator.
 * @return
 */
void *
_bo_array_iter_peek(BArray *self,
                   bo_iterator_t *iter)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  validate_iter(self, iter);
  return (void *) iter->opaque;
}

bo_iterator_t
bo_array_begin(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  return (bo_iterator_t) {.opaque = self->data};
}

bo_iterator_t
bo_array_end(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  return (bo_iterator_t) {.opaque = self->data + self->size * self->data_size};
}

bool
bo_array_empty(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  return self->size == 0;
}

size_t
bo_array_size(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  return self->size;
}

size_t
bo_array_capacity(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  return self->capacity;
}

BType
bo_array_type(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  return self->type;
}

bo_byte_t *
bo_array_data(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  return self->data;
}

size_t
bo_array_data_size(BArray *self)
{
  bo_assert(bo_is_typeof(self, BArray), "invalid array");
  return self->data_size;
}
