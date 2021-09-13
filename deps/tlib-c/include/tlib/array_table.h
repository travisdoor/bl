// =================================================================================================
// tlib-c
//
// File:   array_table.h
// Author: Martin Dorazil
// Date:   13/9/2021
//
// Copyright 2021 Martin Dorazil
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

#ifndef T_ARRAY_TABLE_H
#define T_ARRAY_TABLE_H

#include "tlib/array.h"
#include "tlib/common.h"

typedef struct TArrayTable {
    TArray keys;
    TArray values;
} TArrayTable;

// Insert new entry 'data' into the hash table identified by unique 'key'. The key must be unique
// s64 value (checked by assert).
#define tatbl_insert(tbl, key, data) _tatbl_insert((tbl), (key), &(data))

// Create empty table entry with specified 'key'.
#define tatbl_insert_empty(tbl, key) _tatbl_insert((tbl), (key), NULL)

// Return pointer to the value asociated to the 'key'. Abort is called in case there is no such key
// in the table.
#define tatbl_at(T, tbl, key) (*(T *)_tatbl_at((tbl), (key)))

// Peek pointer to the value from the iterator.
#define tatbl_iter_peek_value(T, tbl, iter) (*(T *)_tatbl_iter_peek_value((tbl), (iter)))

// Utilitity macro for iteration over the array table.
#define TATBL_FOREACH(T, _tbl, _it_key, _it_value)                                                 \
    if ((_tbl) && tatbl_size(_tbl))                                                                \
        for (usize i = 0;                                                                          \
             (i < tatbl_size(_tbl)) && ((_it_key) = tarray_at(u64, &(_tbl)->keys, i)) &&           \
             ((_it_value) = tarray_at(T, &(_tbl)->values, i));                                     \
             ++i)

static inline usize tatbl_size(TArrayTable *tbl)
{
    assert(tbl->keys.size == tbl->values.size);
    return tbl->keys.size;
}

// Create new array table on heap. The 'expected_size' represents expected count of table entries
// (operation over the table can be slower when inserted entry count is greater than expected).
TAPI TArrayTable *tatbl_new(usize data_size, usize expected_size);

// Release all resources used by table.
TAPI void tatbl_delete(TArrayTable *tbl);

// Initialize array table. The 'expected_size' represents expected count of table entries
// (operation over the table can be slower when inserted entry count is greater than expected).
TAPI void tatbl_init(TArrayTable *tbl, usize data_size, usize expected_size);

// Release all resources used by table entries, but don't delete the table itself.
TAPI void tatbl_terminate(TArrayTable *tbl);

// Use macro instead.
TAPI void *_tatbl_insert(TArrayTable *tbl, u64 key, void *data);

// Find table entry specified by 'key' value and return iterator. In case no such key exist in table
// returned iterator points to 'tatbl_end' result.
TAPI TIterator tatbl_find(TArrayTable *tbl, u64 key);

// Check whether key is in table.
TAPI bool tatbl_has_key(TArrayTable *tbl, u64 key);

// Clear the table content.
TAPI void tatbl_clear(TArrayTable *tbl);

// Return iterator pointing to the table begin.
TAPI TIterator tatbl_begin(TArrayTable *tbl);

// Return iterator pointing to the table end.
TAPI TIterator tatbl_end(TArrayTable *tbl);

// Peek key value from iterator.
TAPI u64 tatbl_iter_peek_key(TArrayTable *tbl, TIterator iter);

// Use macro instead.
TAPI void *_tatbl_iter_peek_value(TArrayTable *tbl, TIterator iter);

#endif
