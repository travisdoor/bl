// =================================================================================================
// bl
//
// File:   arena.c
// Author: Martin Dorazil
// Date:   3/14/18
//
// Copyright 2018 Martin Dorazil
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

#include "arena.h"
#if BL_PLATFORM_WIN
#include "winpthreads.h"
#else
#include <pthread.h>
#endif

struct arena_chunk {
    struct arena_chunk *next;
    s32                 count;
};

struct arena_sync_impl {
    pthread_mutex_t mtx;
};

static struct arena_sync_impl *sync_new(void)
{
    struct arena_sync_impl *s = bmalloc(sizeof(struct arena_sync_impl));
    pthread_mutex_init(&s->mtx, NULL);
    return s;
}

static void sync_delete(struct arena_sync_impl *s)
{
    pthread_mutex_destroy(&s->mtx);
    bfree(s);
}

static INLINE struct arena_chunk *alloc_chunk(struct arena *arena)
{
    const usize chunk_size_in_bytes =
        sizeof(struct arena_chunk) + arena->elem_size_in_bytes * arena->elems_per_chunk;
    struct arena_chunk *chunk = bmalloc(chunk_size_in_bytes);
    if (!chunk) babort("bad alloc");
    memset(chunk, 0, chunk_size_in_bytes);
    return chunk;
}

static INLINE void *get_from_chunk(struct arena *arena, struct arena_chunk *chunk, s32 i)
{
    bassert(i >= 0 && i < arena->elems_per_chunk);
    void *elem =
        (void *)((char *)chunk + sizeof(struct arena_chunk) + i * arena->elem_size_in_bytes);
    ptrdiff_t adj;
    align_ptr_up(&elem, arena->elem_alignment, &adj);
    bassert(adj < arena->elem_alignment);
    return elem;
}

static INLINE struct arena_chunk *free_chunk(struct arena *arena, struct arena_chunk *chunk)
{
    if (!chunk) return NULL;
    struct arena_chunk *next = chunk->next;
    if (arena->elem_dtor) {
        for (s32 i = 0; i < chunk->count; ++i) {
            arena->elem_dtor(get_from_chunk(arena, chunk, i));
        }
    }
    bfree(chunk);
    return next;
}

void arena_init(struct arena *    arena,
                usize             elem_size_in_bytes,
                s32               elem_alignment,
                s32               elems_per_chunk,
                arena_elem_dtor_t elem_dtor)
{
    arena->elem_size_in_bytes = elem_size_in_bytes + elem_alignment;
    arena->elems_per_chunk    = elems_per_chunk;
    arena->elem_alignment     = elem_alignment;
    arena->first_chunk        = NULL;
    arena->current_chunk      = NULL;
    arena->elem_dtor          = elem_dtor;
    arena->sync               = sync_new();
}

void arena_terminate(struct arena *arena)
{
    struct arena_chunk *chunk = arena->first_chunk;
    while (chunk) {
        chunk = free_chunk(arena, chunk);
    }
    sync_delete(arena->sync);
}

void *arena_safe_alloc(struct arena *arena)
{
    pthread_mutex_lock(&arena->sync->mtx);
    if (!arena->current_chunk) {
        arena->current_chunk = alloc_chunk(arena);
        arena->first_chunk   = arena->current_chunk;
    }

    if (arena->current_chunk->count == arena->elems_per_chunk) {
        // last chunk node
        struct arena_chunk *chunk  = alloc_chunk(arena);
        arena->current_chunk->next = chunk;
        arena->current_chunk       = chunk;
    }

    void *elem = get_from_chunk(arena, arena->current_chunk, arena->current_chunk->count++);
    bassert(is_aligned(elem, arena->elem_alignment) && "Unaligned allocation of arena element!");
    pthread_mutex_unlock(&arena->sync->mtx);
    return elem;
}
