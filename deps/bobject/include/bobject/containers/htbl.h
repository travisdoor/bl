//*****************************************************************************
// Biscuit Object
//
// File:   hash_table.h
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

#ifndef BOBJECT_HASH_TABLE_H
#define BOBJECT_HASH_TABLE_H

#include <bobject/bobject.h>
#include <stdint.h>
#include "bobject/utils.h"
#include "bobject/containers/iterator.h"

BO_BEGIN_DECLS

/* class BHashTable declaration */
bo_decl_type_begin(BHashTable, BObject)
  /* virtual functions */
bo_end();

extern BO_EXPORT BHashTable *
bo_htbl_new(size_t data_size,
            size_t expected_size);

extern BO_EXPORT BHashTable *
bo_htbl_new_bo(BType type,
               bool managed,
               size_t expected_size);

#define bo_htbl_insert(self, key, data) \
  _bo_htbl_insert((self), (key), &(data))

#define bo_htbl_insert_empty(self, key) \
  _bo_htbl_insert((self), (key), NULL)

extern BO_EXPORT void *
_bo_htbl_insert(BHashTable *self,
                uint64_t key,
                void *data);

extern BO_EXPORT bo_iterator_t
bo_htbl_find(BHashTable *self,
             uint64_t key);

#define bo_htbl_at(self, key, type) \
  (*(type *)_bo_htbl_at((self), (key)))

extern BO_EXPORT void *
_bo_htbl_at(BHashTable *self,
            uint64_t key);

extern BO_EXPORT void
bo_htbl_erase(BHashTable *self,
              bo_iterator_t *iter);

extern BO_EXPORT bo_iterator_t
bo_htbl_erase_key(BHashTable *self,
                  uint64_t key);

extern BO_EXPORT bool
bo_htbl_has_key(BHashTable *self,
                uint64_t    key);

extern BO_EXPORT bo_iterator_t
bo_htbl_begin(BHashTable *self);

extern BO_EXPORT bo_iterator_t
bo_htbl_end(BHashTable *self);

extern BO_EXPORT void
bo_htbl_iter_next(BHashTable *self,
                  bo_iterator_t *iter);

extern BO_EXPORT uint64_t
bo_htbl_iter_peek_key(BHashTable *self,
                      bo_iterator_t *iter);

#define bo_htbl_iter_peek_value(self, iter, type) \
  (*(type *)_bo_htbl_iter_peek_value((self), (iter)))

extern BO_EXPORT void *
_bo_htbl_iter_peek_value(BHashTable *self,
                         bo_iterator_t *iter);

extern BO_EXPORT void
bo_htbl_clear(BHashTable *self);

extern BO_EXPORT size_t
bo_htbl_size(BHashTable *self);

extern BO_EXPORT size_t
bo_htbl_data_size(BHashTable *self);

extern BO_EXPORT BType
bo_htbl_type(BHashTable *self);

extern BO_EXPORT bool
bo_htbl_empty(BHashTable *self);

BO_END_DECLS

#endif //BOBJECT_HASH_TABLE_H
