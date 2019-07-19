 //*****************************************************************************
// Biscuit Object
//
// File:   hash_table.c
// Author: Martin Dorazil
// Date:   28/11/2017
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

#include <math.h>
#include "bobject/containers/htbl.h"
#include "common_impl.h"

#define DEFAULT_EXPECTED_SIZE 64
#define MAX_LOAD_FACTOR       1
#if BO_DEBUG
#define validate_iter(iter) \
    bo_assert((iter) != NULL && ((struct node *)(iter->opaque))->this == (iter->opaque),  \
    "invalid iterator")
#else
#define validate_iter(node)                                                                        \
    do {                                                                                           \
    } while (0)
#endif

struct node
{
  struct node *next;
  struct node *prev;
#if BO_DEBUG
  struct node *this;
#endif
  uint64_t key;
};

struct bucket
{
  struct node *first;
  struct node *last;
};

/* class BHashTable */

/* class BHashTable constructor params */
bo_decl_params_begin(BHashTable)
  BType  type;
  size_t data_size;
  size_t expected_size;
  bool   managed;
bo_end();

/* class BHashTable object data members */
bo_decl_members_begin(BHashTable, BObject)
  /* member data */
  BType           type;
  size_t          bucket_count;
  size_t          data_size;
  size_t          size;
  struct node     end;
  struct node    *begin;
  struct bucket  *buckets;
  bool            managed;
bo_end();

bo_impl_type(BHashTable, BObject);

static inline int
next_prime(int num)
{
  num++;
  for (int i = 2; i < num; i++) {
    if (num % i == 0) {
      num++;
      i = 2;
    } else {
      continue;
    }
  }
  return num;
}

static inline bo_byte_t *
get_data_ptr(const void *n)
{
  return (bo_byte_t *) n + sizeof(struct node);
}

static inline size_t
get_node_size(size_t data_size)
{
  return sizeof(struct node) + data_size;
}

void
BHashTableKlass_init(BHashTableKlass *klass)
{
}

void
BHashTable_ctor(BHashTable        *self,
                BHashTableParams  *p)
{
  bo_assert(p, "invalid params");
  self->type      = p->type;
  self->data_size = p->data_size;
  self->managed   = p->managed;
  self->end       = (struct node) {
#if BO_DEBUG
    .this = &self->end,
#endif
    .key = 0,
    .prev = NULL,
    .next = NULL
  };

  self->begin          = &self->end;
  size_t expected_size = p->expected_size;

  // init buckets
  if (expected_size == 0)
    expected_size = DEFAULT_EXPECTED_SIZE;

  self->bucket_count =
    (size_t) next_prime((int) ceil((double) expected_size / (double) MAX_LOAD_FACTOR));

  self->buckets = bo_calloc(self->bucket_count, sizeof(struct bucket));
}

void
BHashTable_dtor(BHashTable *self)
{
  bo_htbl_clear(self);
  bo_free(self->buckets);
}

bo_copy_result
BHashTable_copy(BHashTable *self,
                BHashTable *other)
{
  return BO_NO_COPY;
}
/* class BHashTable end */

/* helpers */
static inline uint64_t
hash_index(BHashTable     *self,
           const uint64_t  key)
{
  return (uint64_t) (key % self->bucket_count);
}

static struct node *
create_node(BHashTable *self)
{
  struct node *new_node = bo_malloc(get_node_size(self->data_size));
  memset(new_node, 0, get_node_size(self->data_size));
#if BO_DEBUG
  new_node->this = new_node;
#endif
  return new_node;
}

static void
insert_node(struct node *prev,
            struct node *next,
            struct node *new)
{
  if (prev)
    prev->next = new;
  new->prev = prev;
  if (next)
    next->prev = new;
  new->next = next;
}

static bo_iterator_t
erase_node(BHashTable     *self,
           struct node    *node,
           struct bucket  *bucket)
{
  if (bucket->first == bucket->last)
    bucket->first = bucket->last = NULL;
  else if (node == bucket->first)
    bucket->first = node->next;
  else if (node == bucket->last)
    bucket->last = node->prev;

  if (node->prev) {
    node->prev->next = node->next;
    node->next->prev = node->prev;
  } else {
    self->begin = node->next;
    node->next->prev = NULL;
  }

  if (self->managed)
    bo_unref(*(BObject **)get_data_ptr(node));
  bo_iterator_t iter_next = (bo_iterator_t) {.opaque = node->next};
  bo_free(node);
  self->size--;
  return iter_next;
}

/* public */
BHashTable *
bo_htbl_new(size_t data_size,
            size_t expected_size)
{
  BHashTableParams p = {
    .expected_size = expected_size,
    .data_size     = data_size,
    .type          = NULL,
    .managed       = false
  };

  return bo_new(BHashTable, &p);
}

BHashTable *
bo_htbl_new_bo(BType type,
               bool managed,
               size_t expected_size)
{
  BHashTableParams p = {
    .expected_size = expected_size,
    .data_size     = sizeof(BObject *),
    .type          = type,
    .managed       = managed
  };

  return bo_new(BHashTable, &p);
}

void *
_bo_htbl_insert(BHashTable *self,
                uint64_t key,
                void *data)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  const uint64_t hash = hash_index(self, key);

  struct bucket *bucket = &self->buckets[hash];
  struct node *new_node = create_node(self);
  new_node->key = key;

  if (!bucket->first) {
    // new empty bucket
    bucket->first = new_node;
    bucket->last = new_node;
    insert_node(NULL, self->begin, new_node);
    self->begin = new_node;
  } else {
    // find conflicts
    struct node *node = bucket->first;
    while (node) {
      if (node->key == key)
        bo_abort("duplicate key: 0x%x", key);
      if (node == bucket->last)
        break;
      node = node->next;
    }

    if (self->begin == bucket->first)
      self->begin = new_node;
    insert_node(bucket->first->prev, bucket->first, new_node);
    bucket->first = new_node;
  }

  // copy user data
  if (data != NULL)
    memcpy(get_data_ptr(new_node), data, self->data_size);

  ++self->size;
  return get_data_ptr(new_node);
}

bo_iterator_t
bo_htbl_find(BHashTable *self,
             uint64_t key)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  const uint64_t hash = hash_index(self, key);
  struct bucket *bucket = &self->buckets[hash];

  struct node *node = bucket->first;
  while (node) {
    if (node->key == key)
      return (bo_iterator_t) {.opaque = node};
    if (node == bucket->last)
      break;
    node = node->next;
  }

  return (bo_iterator_t) {.opaque = &self->end};
}

void *
_bo_htbl_at(BHashTable *self,
            uint64_t key)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  bo_iterator_t iter = bo_htbl_find(self, key);
  bo_iterator_t iter_end = bo_htbl_end(self);
  if (!bo_iterator_equal(&iter, &iter_end))
    return get_data_ptr(iter.opaque);

  bo_abort("no such key %u in hash table", key);
}

void
bo_htbl_erase(BHashTable *self,
              bo_iterator_t *iter)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  validate_iter(iter);
  // bo_iterator pointing to end of table
  if (iter->opaque == &self->end) {
    *iter = (bo_iterator_t) {.opaque = &self->end};
    return;
  }

  struct node *node = (struct node *) iter->opaque;
  const uint64_t hash = hash_index(self, node->key);
  struct bucket *bucket = &self->buckets[hash];
  *iter = erase_node(self, node, bucket);
}

bo_iterator_t
bo_htbl_erase_key(BHashTable *self,
                  uint64_t key)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  const uint64_t hash = hash_index(self, key);
  struct bucket *bucket = &self->buckets[hash];

  struct node *node = bucket->first;
  while (node) {
    if (node->key == key) {
      return erase_node(self, node, bucket);
    }
    if (node == bucket->last)
      break;
    node = node->next;
  }
  bo_abort("no such key %u in hash table", key);
}

bool
bo_htbl_has_key(BHashTable *self,
                uint64_t    key)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  bo_iterator_t iter = bo_htbl_find(self, key);
  bo_iterator_t end  = bo_htbl_end(self);
  return !bo_iterator_equal(&iter, &end);
}

bo_iterator_t
bo_htbl_begin(BHashTable *self)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  return (bo_iterator_t) {.opaque = self->begin};
}

bo_iterator_t
bo_htbl_end(BHashTable *self)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  return (bo_iterator_t) {.opaque = &self->end};
}

void
bo_htbl_iter_next(BHashTable *self,
                  bo_iterator_t *iter)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  validate_iter(iter);
  struct node *node = (struct node *) iter->opaque;
  iter->opaque = node->next;
}

uint64_t
bo_htbl_iter_peek_key(BHashTable *self,
                      bo_iterator_t *iter)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  validate_iter(iter);
  struct node *node = (struct node *) iter->opaque;
  return node->key;
}

void *
_bo_htbl_iter_peek_value(BHashTable *self,
                         bo_iterator_t *iter)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  validate_iter(iter);
  struct node *node = (struct node *) iter->opaque;
  return get_data_ptr(node);
}

void
bo_htbl_clear(BHashTable *self)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");

  bo_iterator_t iter = bo_htbl_begin(self);
  bo_iterator_t iter_end = bo_htbl_end(self);
  struct node *node = NULL;
  while (!bo_iterator_equal(&iter, &iter_end)) {
    node = (struct node *) iter.opaque;

    if (self->managed)
      bo_unref(*(BObject **)get_data_ptr(node));
    bo_htbl_iter_next(self, &iter);
    bo_free(node);
  }
  memset(self->buckets, 0, self->bucket_count * sizeof(struct bucket));

  self->end = (struct node) {
#if BO_DEBUG
    .this = &self->end,
#endif
    .key = 0, .prev = NULL,
    .next = NULL
  };
  self->begin = &self->end;
  self->size = 0;
}

const size_t
bo_htbl_size(BHashTable *self)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  return self->size;
}

const size_t
bo_htbl_data_size(BHashTable *self)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  return self->data_size;
}

BType
bo_htbl_type(BHashTable *self)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  return self->type;
}

bool
bo_htbl_empty(BHashTable *self)
{
  bo_assert(bo_is_typeof(self, BHashTable), "invalid hash table");
  return self->size == 0;
}
