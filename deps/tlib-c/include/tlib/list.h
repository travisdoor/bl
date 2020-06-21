//*****************************************************************************
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
//*****************************************************************************

#ifndef T_LIST_H
#define T_LIST_H

#include "tlib/common.h"

#define tlist_push_back(list, data) _tlist_push_back((list), &(data))
#define tlist_push_front(list, data) _tlist_push_front((list), &(data))
#define tlist_iter_peek(T, list, iter) (*(T *)_tlist_iter_peek((list), (iter)))
#define tlist_front(T, list) (*(T *)_tlist_front((list)))
#define tlist_back(T, list) (*(T *)_tlist_back((list)))

#define TLIST_FOREACH(list, it)                                                                    \
	(it) = tlist_begin((list));                                                                \
	for (TIterator end = tlist_end((list)); !TITERATOR_EQUAL((it), end);                       \
	     tlist_iter_next(&(it)))

struct TListNode {
	struct TListNode *prev;
	struct TListNode *next;
#ifndef NDEBUG
	struct TListNode *_this;
#endif
};

typedef struct TList {
	usize             data_size;
	usize             size;
	struct TListNode *begin;
	struct TListNode  end;
} TList;

/* clang-format off */
TAPI TList *
tlist_new(usize data_size);

TAPI void
tlist_delete(TList *list);

TAPI void
tlist_init(TList *list, usize data_size);

TAPI void
tlist_terminate(TList *list);

TAPI void
_tlist_push_back(TList *list, void *data);

TAPI void
tlist_pop_back(TList *list);

TAPI void
tlist_pop_front(TList *list);

TAPI void
_tlist_push_front(TList *list, void *data);

TAPI void
tlist_erase(TList *list, TIterator *iter);

TAPI void
tlist_iter_next(TIterator *iter);

TAPI void *
_tlist_iter_peek(TIterator iter);

TAPI void
_tlist_pop_back(TList *list);

TAPI TIterator
tlist_begin(TList *list);

TAPI TIterator
tlist_end(TList *list);

TAPI void *
_tlist_front(TList *list);

TAPI void *
_tlist_back(TList *list);

TAPI bool
tlist_empty(TList *list);

TAPI void
tlist_clear(TList *list);
/* clang-format on */
#endif
