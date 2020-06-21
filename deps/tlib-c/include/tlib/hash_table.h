//*****************************************************************************
// tlib-c
//
// File:   hash_table.h
// Author: Martin Dorazil
// Date:   30/9/2019
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
//*****************************************************************************

#ifndef T_HASH_TABLE_H
#define T_HASH_TABLE_H

#include "tlib/common.h"

struct THtblNode {
	struct THtblNode *next;
	struct THtblNode *prev;
#ifndef NDEBUG
	struct THtblNode *_this;
#endif
	u64 key;
};

struct THtblBucket {
	struct THtblNode *first;
	struct THtblNode *last;
};

typedef struct THashTable {
	usize               bucket_count;
	usize               data_size;
	usize               size;
	struct THtblNode    end;
	struct THtblNode *  begin;
	struct THtblBucket *buckets;
} THashTable;

#define thtbl_insert(tbl, key, data) _thtbl_insert((tbl), (key), &(data))
#define thtbl_insert_empty(tbl, key) _thtbl_insert((tbl), (key), NULL)
#define thtbl_at(T, tbl, key) (*(T*)_thtbl_at((tbl), (key)))
#define thtbl_iter_peek_value(T, iter) (*(T *)_thtbl_iter_peek_value((iter)))

#define THTBL_FOREACH(_htbl, _it)                                                                  \
	(_it) = thtbl_begin((_htbl));                                                              \
	for (TIterator end = thtbl_end((_htbl)); !TITERATOR_EQUAL((_it), end);                     \
	     thtbl_iter_next(&(_it)))

/* clang-format off */
TAPI THashTable *
thtbl_new(usize data_size, usize expected_size);

TAPI void
thtbl_init(THashTable *tbl, usize data_size, usize expected_size);

TAPI void
thtbl_terminate(THashTable *tbl);

TAPI THashTable *
thtbl_new(usize data_size, usize expected_size);

TAPI void
thtbl_delete(THashTable *tbl);

TAPI void *
_thtbl_insert(THashTable *tbl, u64 key, void *data);

TAPI TIterator
thtbl_find(THashTable *tbl, u64 key);

TAPI void *
_thtbl_at(THashTable *tbl, u64 key);

TAPI TIterator 
thtbl_erase(THashTable *tbl, TIterator iter);

TAPI TIterator
thtbl_erase_key(THashTable *tbl, u64 key);

TAPI bool
thtbl_has_key(THashTable *tbl, u64 key);

TAPI TIterator
thtbl_begin(THashTable *tbl);

TAPI TIterator
thtbl_end(THashTable *tbl);

TAPI void
thtbl_iter_next(TIterator *iter);

TAPI u64
thtbl_iter_peek_key(TIterator iter);

TAPI void *
_thtbl_iter_peek_value(TIterator iter);

TAPI void
thtbl_clear(THashTable *tbl);
/* clang-format on */

#endif
