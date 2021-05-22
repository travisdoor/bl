// =================================================================================================
// tlib-c
//
// File:   list.h
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

#ifndef T_LIST_H
#define T_LIST_H

#include "tlib/array.h"
#include "tlib/common.h"

// Double linked list using cache for erased nodes.
//
// - Every node in the list points to previous node and next one.
// - The list is more effective than array for random position insert and erase of values.
// - Node order is kept after erase.
// - List internally use cache for erased nodes, those cached nodes can be reused during
//   insertation, so no new allocation is needed in such case.

// Insert new value at the end of the list.
#define tlist_push_back(list, data) _tlist_push_back((list), &(data))

// Insert new value at the begining of the list.
#define tlist_push_front(list, data) _tlist_push_front((list), &(data))

// Peek value represented by iterator.
#define tlist_iter_peek(T, list, iter) (*(T *)_tlist_iter_peek((list), (iter)))

// Peek first value in the list.
#define tlist_front(T, list) (*(T *)_tlist_front((list)))

// Peek last value in the list.
#define tlist_back(T, list) (*(T *)_tlist_back((list)))

// List iteration tool macro.
#define TLIST_FOREACH(list, it)                                                                    \
    (it) = tlist_begin((list));                                                                    \
    for (TIterator end = tlist_end((list)); !TITERATOR_EQUAL((it), end); tlist_iter_next(&(it)))

struct TListNode {
    struct TListNode *prev; // Previous list node.
    struct TListNode *next; // Next list node.
#ifndef NDEBUG
    struct TListNode *_this;
#endif
};

typedef struct TList {
    TArray            buf;
    usize             elem_size;
    usize             size; // Count of element in list.
    struct TListNode *begin;
    struct TListNode  end;
} TList;

// Create new list on heap. The 'elem_size' is size of one list element in bytes.
TAPI TList *tlist_new(usize elem_size);

// Delete list on the heap.
TAPI void tlist_delete(TList *list);

// Initialize list. The 'elem_size' is size of one list element in bytes.
TAPI void tlist_init(TList *list, usize elem_size);

// Release all resources used by list but do not free the list itself.
TAPI void tlist_terminate(TList *list);

// Use macro insread.
TAPI void _tlist_push_back(TList *list, void *data);

// Pop the last element from the list. List cannot be empty, checked by assert.
TAPI void tlist_pop_back(TList *list);

// Pop the first element from the list. List cannot be empty, checked by assert.
TAPI void tlist_pop_front(TList *list);

// Use macro instead.
TAPI void _tlist_push_front(TList *list, void *data);

// Erase one list element specified by iterator value.
TAPI void tlist_erase(TList *list, TIterator *iter);

// Move iterator to the next element.
TAPI void tlist_iter_next(TIterator *iter);

// Use macro instead.
TAPI void *_tlist_iter_peek(TIterator iter);

// Use macro instead.
TAPI void _tlist_pop_back(TList *list);

// Return begin iterator.
TAPI TIterator tlist_begin(TList *list);

// Return end iterator.
TAPI TIterator tlist_end(TList *list);

// Use macro instead.
TAPI void *_tlist_front(TList *list);

// Use macro instead.
TAPI void *_tlist_back(TList *list);

// Checks whether list is empty.
TAPI bool tlist_empty(TList *list);

// Clear the list.
TAPI void tlist_clear(TList *list);

#endif
