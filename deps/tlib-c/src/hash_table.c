// =================================================================================================
// tlib-c
//
// File:   hash_table.c
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

#include "tlib/hash_table.h"
#include "tmemory.h"
#include <math.h>

#define DEFAULT_EXPECTED_SIZE 64
#define MAX_LOAD_FACTOR 1
#define NODE_SIZE (sizeof(struct THtblNode) + tbl->data_size)

#define GET_DATA_PTR(_n) ((s8 *)(_n) + sizeof(struct THtblNode))
#define HASH_INDEX(_key) ((u64)((_key) % tbl->bucket_count))
#define VALIDATE_ITER(_iter)                                                                       \
    assert((_iter) != NULL && ((struct THtblNode *)((_iter)->opaque))->_this == ((_iter)->opaque))

static inline s32 next_prime(s32 num)
{
    num++;
    for (s32 i = 2; i < num; i++) {
        if (num % i == 0) {
            num++;
            i = 2;
        } else {
            continue;
        }
    }
    return num;
}

static struct THtblNode *create_node(THashTable *tbl)
{
    struct THtblNode *new_node = tmalloc(NODE_SIZE);
    memset(new_node, 0, NODE_SIZE);
#ifndef NDEBUG
    new_node->_this = new_node;
#endif
    return new_node;
}

static void insert_node(struct THtblNode *prev, struct THtblNode *next, struct THtblNode *new)
{
    if (prev) prev->next = new;
    new->prev = prev;
    if (next) next->prev = new;
    new->next = next;
}

static TIterator erase_node(THashTable *tbl, struct THtblNode *node, struct THtblBucket *bucket)
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
        tbl->begin       = node->next;
        node->next->prev = NULL;
    }

    TIterator iter_next = (TIterator){.opaque = node->next};
    tfree(node);
    tbl->size--;
    return iter_next;
}

// =================================================================================================
// public
// =================================================================================================
void thtbl_init(THashTable *tbl, usize data_size, usize expected_size)
{
    tbl->data_size = data_size;
    tbl->end       = (struct THtblNode){
#ifndef NDEBUG
        ._this = &tbl->end,
#endif
        .key  = 0,
        .prev = NULL,
        .next = NULL};

    tbl->begin = &tbl->end;

    // init buckets
    if (!expected_size) expected_size = DEFAULT_EXPECTED_SIZE;

    tbl->bucket_count = (usize)next_prime((s32)ceil((f64)expected_size / (f64)MAX_LOAD_FACTOR));
    tbl->buckets      = tmalloc(tbl->bucket_count * sizeof(struct THtblBucket));
    memset(tbl->buckets, 0, tbl->bucket_count * sizeof(struct THtblBucket));
    tbl->size = 0;
}

void thtbl_terminate(THashTable *tbl)
{
    thtbl_clear(tbl);
    tfree(tbl->buckets);
    tbl->size  = 0;
    tbl->begin = &tbl->end;
}

THashTable *thtbl_new(usize data_size, usize expected_size)
{
    THashTable *tbl = tmalloc(sizeof(THashTable));
    if (!tbl) TABORT("Bad alloc.");
    thtbl_init(tbl, data_size, expected_size);

    return tbl;
}

void thtbl_delete(THashTable *tbl)
{
    if (!tbl) return;
    thtbl_terminate(tbl);
    tfree(tbl);
}

void *_thtbl_insert(THashTable *tbl, u64 key, void *data)
{
    const u64 hash = HASH_INDEX(key);

    struct THtblBucket *bucket   = &tbl->buckets[hash];
    struct THtblNode *  new_node = create_node(tbl);
    new_node->key                = key;

    if (!bucket->first) {
        // new empty bucket
        bucket->first = new_node;
        bucket->last  = new_node;
        insert_node(NULL, tbl->begin, new_node);
        tbl->begin = new_node;
    } else {
        // find conflicts
        struct THtblNode *node = bucket->first;
        while (node) {
            assert(node->key != key && "Duplicate key.");
            if (node == bucket->last) break;
            node = node->next;
        }

        if (tbl->begin == bucket->first) tbl->begin = new_node;
        insert_node(bucket->first->prev, bucket->first, new_node);
        bucket->first = new_node;
    }

    // copy user data
    if (data) memcpy(GET_DATA_PTR(new_node), data, tbl->data_size);

    ++tbl->size;
    return GET_DATA_PTR(new_node);
}

TIterator thtbl_find(THashTable *tbl, u64 key)
{
    const u64           hash   = HASH_INDEX(key);
    struct THtblBucket *bucket = &tbl->buckets[hash];

    struct THtblNode *node = bucket->first;
    while (node) {
        if (node->key == key) return (TIterator){.opaque = node};
        if (node == bucket->last) break;
        node = node->next;
    }

    return (TIterator){.opaque = &tbl->end};
}

void *_thtbl_at(THashTable *tbl, u64 key)
{
    TIterator iter     = thtbl_find(tbl, key);
    TIterator iter_end = thtbl_end(tbl);
    if (!TITERATOR_EQUAL(iter, iter_end)) return GET_DATA_PTR(iter.opaque);
    TABORT("No such key %llu in hash table.", key);
}

TIterator thtbl_erase(THashTable *tbl, TIterator iter)
{
    VALIDATE_ITER(&iter);

    if (iter.opaque == &tbl->end) {
        return (TIterator){.opaque = &tbl->end};
    }

    struct THtblNode *  node   = (struct THtblNode *)iter.opaque;
    const u64           hash   = HASH_INDEX(node->key);
    struct THtblBucket *bucket = &tbl->buckets[hash];
    return erase_node(tbl, node, bucket);
}

TIterator thtbl_erase_key(THashTable *tbl, u64 key)
{
    const u64           hash   = HASH_INDEX(key);
    struct THtblBucket *bucket = &tbl->buckets[hash];

    struct THtblNode *node = bucket->first;
    while (node) {
        if (node->key == key) {
            return erase_node(tbl, node, bucket);
        }
        if (node == bucket->last) break;
        node = node->next;
    }
    TABORT("No such key %llu in hash table.", key);
}

bool thtbl_has_key(THashTable *tbl, u64 key)
{
    TIterator iter = thtbl_find(tbl, key);
    TIterator end  = thtbl_end(tbl);
    return !TITERATOR_EQUAL(iter, end);
}

TIterator thtbl_begin(THashTable *tbl)
{
    return (TIterator){.opaque = tbl->begin};
}

TIterator thtbl_end(THashTable *tbl)
{
    return (TIterator){.opaque = &tbl->end};
}

void thtbl_iter_next(TIterator *iter)
{
    VALIDATE_ITER(iter);
    struct THtblNode *node = (struct THtblNode *)iter->opaque;
    iter->opaque           = node->next;
}

u64 thtbl_iter_peek_key(TIterator iter)
{
    VALIDATE_ITER(&iter);
    struct THtblNode *node = (struct THtblNode *)iter.opaque;
    return node->key;
}

void *_thtbl_iter_peek_value(TIterator iter)
{
    VALIDATE_ITER(&iter);
    struct THtblNode *node = (struct THtblNode *)iter.opaque;
    return GET_DATA_PTR(node);
}

void thtbl_clear(THashTable *tbl)
{
    TIterator         iter     = thtbl_begin(tbl);
    TIterator         iter_end = thtbl_end(tbl);
    struct THtblNode *node     = NULL;
    while (!TITERATOR_EQUAL(iter, iter_end)) {
        node = (struct THtblNode *)iter.opaque;

        thtbl_iter_next(&iter);
        tfree(node);
    }
    memset(tbl->buckets, 0, tbl->bucket_count * sizeof(struct THtblBucket));

    tbl->end = (struct THtblNode){
#ifndef NDEBUG
        ._this = &tbl->end,
#endif
        .key  = 0,
        .prev = NULL,
        .next = NULL};

    tbl->begin = &tbl->end;
    tbl->size  = 0;
}
