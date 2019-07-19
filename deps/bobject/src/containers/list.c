//*****************************************************************************
// Biscuit Object
//
// File:   list.c
// Author: Martin Dorazil
// Date:   04/12/2017
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

#include "bobject/containers/list.h"
#include "common_impl.h"

#if BO_DEBUG
#define validate_iter(iter) \
    bo_assert((iter) != NULL && ((list_node *)((iter)->opaque))->this == ((iter)->opaque),  \
    "invalid iterator")
#else
#define validate_iter(node)                                                                        \
    do {                                                                                           \
    } while (0)
#endif

typedef struct _list_node
{
  struct _list_node *prev;
  struct _list_node *next;
#if BO_DEBUG
  struct _list_node *this;
#endif
} list_node;

/* class BList */

/* class BList constructor params */
bo_decl_params_begin(BList)
  /* constructor params goes here */
  BType  type;
  size_t data_size;
  bool   managed;
bo_end();

/* class BList object data members */
bo_decl_members_begin(BList, BObject)
  /* member data */
  BType      type;
  size_t     data_size;
  bool       managed;
  size_t     size;
  list_node *begin;
  list_node  end;
bo_end();

bo_impl_type(BList, BObject);

static inline bo_byte_t *
get_data_ptr(const void *n)
{
  return (bo_byte_t *) n + sizeof(list_node);
}

static inline size_t
get_node_size(size_t data_size)
{
  return sizeof(list_node) + data_size;
}

static list_node *
insert_node(BList     *self,
            list_node *prev,
            list_node *next,
            void      *data)
{
  list_node *node =  bo_calloc(1, get_node_size(self->data_size));
#if BO_DEBUG
  node->this = node;
#endif
  memcpy(get_data_ptr(node), data, self->data_size);

  if (prev)
    prev->next = node;
  node->prev = prev;
  if (next)
    next->prev = node;
  node->next = next;

  self->size++;

  return node;
}

/* return pointer to next node */
static list_node *
erase_node(BList     *self,
           list_node *node)
{
  list_node *ret = node->next;
  if (node->prev) {
    node->prev->next = node->next;
    node->next->prev = node->prev;
  } else {
    self->begin = node->next;
    node->next->prev = NULL;
  }

  if (self->managed)
    bo_unref(*(BObject **)get_data_ptr(node));
  bo_free(node);
  self->size--;
  return ret;
}

void
BListKlass_init(BListKlass *klass)
{
  /* init vtable here */
}

void
BList_ctor(BList *self, BListParams *p)
{
  /* constructor */
  bo_assert(p, "invalid params");
  self->type      = p->type;
  self->managed   = p->managed;
  self->data_size = p->data_size;
  self->begin     = &self->end;
}

void
BList_dtor(BList *self)
{
  /* destructor */
  bo_list_clear(self);
}

bo_copy_result
BList_copy(BList *self, BList *other)
{
  return BO_NO_COPY;
}
/* class BList end */

BList *
bo_list_new(size_t data_size)
{
  BListParams p = {
    .data_size = data_size,
    .managed   = false,
    .type      = NULL
  };

  return bo_new(BList, &p);
}

BList *
bo_list_new_bo(BType  type,
               bool   managed)
{
  BListParams p = {
    .data_size = sizeof(BObject *),
    .managed   = managed,
    .type      = type
  };

  return bo_new(BList, &p);
}

void
_bo_list_push_back(BList *self,
                   void  *data)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");

  list_node *node = insert_node(self, self->end.prev, &self->end, data);
  if (self->size == 1)
    self->begin = node;
}

void
bo_list_pop_back(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  bo_assert(!bo_list_empty(self), "pop back on empty list");
  erase_node(self, self->end.prev);
}

void
_bo_list_push_front(BList *self,
                    void  *data)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  list_node *node = NULL;

  if (self->size == 0)
    node = insert_node(self, NULL, &self->end, data);
  else
    node = insert_node(self, NULL, self->begin, data);

  self->begin = node;
}

void
bo_list_pop_front(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  bo_assert(!bo_list_empty(self), "pop back on empty list");
  erase_node(self, self->begin);
}

void
bo_list_iter_next(BList         *self,
                  bo_iterator_t *iter)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  validate_iter(iter);
  list_node *node = (list_node *) iter->opaque;
  iter->opaque = node->next;
}

void *
_bo_list_iter_peek(BList         *self,
                   bo_iterator_t *iter)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  validate_iter(iter);
  return get_data_ptr(iter->opaque);
}

bo_iterator_t
bo_list_begin(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  return (bo_iterator_t) {.opaque = self->begin};
}

bo_iterator_t
bo_list_end(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  return (bo_iterator_t) {.opaque = &self->end};
}

void*
_bo_list_front(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  bo_assert(!bo_list_empty(self), "calling front on empty list");
  return get_data_ptr(self->begin);
}

void*
_bo_list_back(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  bo_assert(!bo_list_empty(self), "calling back on empty list");
  return get_data_ptr(self->end.prev);
}

bool
bo_list_empty(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  return !self->size;
}

void
bo_list_erase(BList         *self,
              bo_iterator_t *iter)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  validate_iter(iter);
  iter->opaque = erase_node(self, (list_node *)iter->opaque);
}

void
bo_list_clear(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  bo_iterator_t iter = bo_list_begin(self);
  bo_iterator_t end  = bo_list_end(self);
  list_node *current = NULL;

  while (!bo_iterator_equal(&iter, &end))
  {
    current = (list_node *)iter.opaque;
    bo_list_iter_next(self, &iter);

    if (self->managed)
      bo_unref(*(BObject **)get_data_ptr(current));
    bo_free(current);
  }

  self->size = 0;
  self->begin = &self->end;
}

size_t
bo_list_size(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  return self->size;
}

size_t
bo_list_data_size(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  return self->data_size;
}

bool
bo_list_managed(BList *self)
{
  bo_assert(bo_is_typeof(self, BList), "invalid list");
  return self->managed;
}
