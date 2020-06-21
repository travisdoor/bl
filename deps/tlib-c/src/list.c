//*****************************************************************************
// tlib-c
//
// File:   list.c
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

#include "tlib/list.h"

#define VALIDATE_ITER(_iter)                                                                        \
	assert((_iter) != NULL && ((struct TListNode *)((_iter)->opaque))->_this == ((_iter)->opaque))

#define GET_DATA_PTR(_n) ((s8 *)(_n) + sizeof(struct TListNode))
#define GET_NODE_SIZE (sizeof(struct TListNode) + list->data_size)
#define ERROR_ON_EMPTY                                                                             \
	if (tlist_empty(list)) TABORT("List is empty.");

static struct TListNode *
insert_node(TList *list, struct TListNode *prev, struct TListNode *next, void *data)
{
	struct TListNode *node = calloc(1, GET_NODE_SIZE);
#ifndef NDEBUG
	node->_this = node;
#endif
	memcpy(GET_DATA_PTR(node), data, list->data_size);

	if (prev) prev->next = node;
	node->prev = prev;
	if (next) next->prev = node;
	node->next = next;

	list->size++;

	return node;
}

/* return pointer to next node */
static struct TListNode *
erase_node(TList *list, struct TListNode *node)
{
	struct TListNode *ret = node->next;
	if (node->prev) {
		node->prev->next = node->next;
		node->next->prev = node->prev;
	} else {
		list->begin      = node->next;
		node->next->prev = NULL;
	}

	free(node);
	list->size--;
	return ret;
}

TList *
tlist_new(usize data_size)
{
	TList *list = calloc(1, sizeof(TList));
	if (!list) TABORT("Bad alloc");

	tlist_init(list, data_size);
	return list;
}

void
tlist_delete(TList *list)
{
	if (!list) return;
	tlist_terminate(list);
	free(list);
}

void
tlist_init(TList *list, usize data_size)
{
	list->data_size = data_size;
	list->begin     = &list->end;
	list->size      = 0;
}

void
tlist_terminate(TList *list)
{
	tlist_clear(list);
	list->size  = 0;
	list->begin = &list->end;
	list->size  = 0;
}

void
_tlist_push_back(TList *list, void *data)
{
	struct TListNode *node = insert_node(list, list->end.prev, &list->end, data);
	if (list->size == 1) list->begin = node;
}

void
tlist_pop_back(TList *list)
{
	ERROR_ON_EMPTY;
	erase_node(list, list->end.prev);
}

void
_tlist_push_front(TList *list, void *data)
{
	struct TListNode *node = NULL;

	if (list->size == 0)
		node = insert_node(list, NULL, &list->end, data);
	else
		node = insert_node(list, NULL, list->begin, data);

	list->begin = node;
}

void
tlist_pop_front(TList *list)
{
	ERROR_ON_EMPTY;
	erase_node(list, list->begin);
}

void
tlist_iter_next(TIterator *iter)
{
	VALIDATE_ITER(iter);
	struct TListNode *node = (struct TListNode *)iter->opaque;
	iter->opaque           = node->next;
}

void *
_tlist_iter_peek(TIterator iter)
{
	VALIDATE_ITER(&iter);
	return GET_DATA_PTR(iter.opaque);
}

TIterator
tlist_begin(TList *list)
{
	return (TIterator){.opaque = list->begin};
}

TIterator
tlist_end(TList *list)
{
	return (TIterator){.opaque = &list->end};
}

void *
_tlist_front(TList *list)
{
	ERROR_ON_EMPTY;
	return GET_DATA_PTR(list->begin);
}

void *
_tlist_back(TList *list)
{
	ERROR_ON_EMPTY;
	return GET_DATA_PTR(list->end.prev);
}

bool
tlist_empty(TList *list)
{
	return !list->size;
}

void
tlist_erase(TList *list, TIterator *iter)
{
	VALIDATE_ITER(iter);
	iter->opaque = erase_node(list, (struct TListNode *)iter->opaque);
}

void
tlist_clear(TList *list)
{
	TIterator         iter    = tlist_begin(list);
	TIterator         end     = tlist_end(list);
	struct TListNode *current = NULL;

	while (!TITERATOR_EQUAL(iter, end)) {
		current = (struct TListNode *)iter.opaque;
		tlist_iter_next(&iter);

		free(current);
	}

	list->size  = 0;
	list->begin = &list->end;
}
