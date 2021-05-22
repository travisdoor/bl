// =================================================================================================
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
// =================================================================================================

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
    usize               data_size; // Size of data in bytes.
    usize               size;      // Count of entries in the hash table.
    struct THtblNode    end;
    struct THtblNode *  begin;
    struct THtblBucket *buckets;
} THashTable;

// Insert new entry 'data' into the hash table identified by unique 'key'. The key must be unique
// s64 value (checked by assert).
#define thtbl_insert(tbl, key, data) _thtbl_insert((tbl), (key), &(data))

// Create empty table entry with specified 'key'.
#define thtbl_insert_empty(tbl, key) _thtbl_insert((tbl), (key), NULL)

// Return pointer to the value asociated to the 'key'. Abort is called in case there is no such key
// in the table.
#define thtbl_at(T, tbl, key) (*(T *)_thtbl_at((tbl), (key)))

// Peek pointer to the value from the iterator.
#define thtbl_iter_peek_value(T, iter) (*(T *)_thtbl_iter_peek_value((iter)))

// Utilitity macro for iteration over the hash table.
#define THTBL_FOREACH(_htbl, _it)                                                                  \
    (_it) = thtbl_begin((_htbl));                                                                  \
    for (TIterator end = thtbl_end((_htbl)); !TITERATOR_EQUAL((_it), end); thtbl_iter_next(&(_it)))

// Create new hash table on heap. The 'expected_size' represents expected count of table entries
// (operation over the table can be slower when inserted entry count is greater than expected).
TAPI THashTable *thtbl_new(usize data_size, usize expected_size);

// Release all resources used by table.
TAPI void thtbl_delete(THashTable *tbl);

// Initialize hash table. The 'expected_size' represents expected count of table entries
// (operation over the table can be slower when inserted entry count is greater than expected).
TAPI void thtbl_init(THashTable *tbl, usize data_size, usize expected_size);

// Release all resources used by table entries, but don't delete the table itself.
TAPI void thtbl_terminate(THashTable *tbl);

// Use macro instead.
TAPI void *_thtbl_insert(THashTable *tbl, u64 key, void *data);

// Find table entry specified by 'key' value and return iterator. In case no such key exist in table
// returned iterator points to 'thtbl_end' result.
TAPI TIterator thtbl_find(THashTable *tbl, u64 key);

// Use macro instead.
TAPI void *_thtbl_at(THashTable *tbl, u64 key);

// Erase table entry specified by iterator. Entry must exist.
TAPI TIterator thtbl_erase(THashTable *tbl, TIterator iter);

// Erase table entry specified by key value. Entry must exist.
TAPI TIterator thtbl_erase_key(THashTable *tbl, u64 key);

// Check whether key is in table.
TAPI bool thtbl_has_key(THashTable *tbl, u64 key);

// Return iterator pointing to the table begin.
TAPI TIterator thtbl_begin(THashTable *tbl);

// Return iterator pointing to the table end.
TAPI TIterator thtbl_end(THashTable *tbl);

// Shift iterator to the next entry.
TAPI void thtbl_iter_next(TIterator *iter);

// Peek key value from iterator.
TAPI u64 thtbl_iter_peek_key(TIterator iter);

// Use macro instead.
TAPI void *_thtbl_iter_peek_value(TIterator iter);

// Clear the table content.
TAPI void thtbl_clear(THashTable *tbl);

#endif
