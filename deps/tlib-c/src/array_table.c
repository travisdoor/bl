// =================================================================================================
// tlib-c
//
// File:   array_table.c
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

#include "tlib/array_table.h"
#include "tmemory.h"

void tatbl_init(TArrayTable *tbl, usize data_size, usize expected_size)
{
    tarray_init(&tbl->keys, sizeof(u64));
    tarray_init(&tbl->values, data_size);
    tarray_reserve(&tbl->keys, expected_size);
    tarray_reserve(&tbl->values, expected_size);
}

void tatbl_terminate(TArrayTable *tbl)
{
    tarray_terminate(&tbl->keys);
    tarray_terminate(&tbl->values);
}

TArrayTable *tatbl_new(usize data_size, usize expected_size)
{
    TArrayTable *tbl = tmalloc(sizeof(TArrayTable));
    if (!tbl) TABORT("Bad alloc.");
    tatbl_init(tbl, data_size, expected_size);
    return tbl;
}

void tatbl_delete(TArrayTable *tbl)
{
    if (!tbl) return;
    tatbl_terminate(tbl);
    tfree(tbl);
}

void tatbl_clear(TArrayTable *tbl)
{
    tarray_clear(&tbl->keys);
    tarray_clear(&tbl->values);
}

void *_tatbl_insert(TArrayTable *tbl, u64 key, void *data)
{
    assert(!tatbl_has_key(tbl, key));
    tarray_push(&tbl->keys, key);
    return _tarray_push(&tbl->values, data);
}

TIterator tatbl_find(TArrayTable *tbl, u64 key)
{
    assert(tbl->keys.size == tbl->values.size);
    for (usize i = 0; i < tbl->keys.size; ++i) {
        const u64 other_key = tarray_at(u64, &tbl->keys, i);
        if (key == other_key) {
            return (TIterator){.index = i};
        }
    }
    return (TIterator){.index = tatbl_size(tbl)};
}

bool tatbl_has_key(TArrayTable *tbl, u64 key)
{
    return tatbl_find(tbl, key).index != tatbl_size(tbl);
}

TIterator tatbl_begin(TArrayTable *tbl)
{
    return (TIterator){.index = 0};
}

TIterator tatbl_end(TArrayTable *tbl)
{
    return (TIterator){.index = tatbl_size(tbl)};
}

u64 tatbl_iter_peek_key(TArrayTable *tbl, TIterator iter)
{
    assert(iter.index < tbl->keys.size);
    return tarray_at(u64, &tbl->keys, iter.index);
}

void *_tatbl_iter_peek_value(TArrayTable *tbl, TIterator iter)
{
    assert(iter.index < tbl->values.size);
    return _tarray_at(&tbl->values, iter.index);
}
