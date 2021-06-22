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
#include <windows.h>
#else
#include <pthread.h>
#endif

typedef struct ArenaChunk {
    struct ArenaChunk *next;
    s32                count;
} ArenaChunk;

typedef struct ArenaSyncImpl {
    pthread_mutex_t mtx;
} ArenaSyncImpl;

static ArenaSyncImpl *sync_new(void)
{
    ArenaSyncImpl *s = bl_malloc(sizeof(ArenaSyncImpl));
    pthread_mutex_init(&s->mtx, NULL);
    return s;
}

static void sync_delete(ArenaSyncImpl *s)
{
    pthread_mutex_destroy(&s->mtx);
    bl_free(s);
}

static INLINE ArenaChunk *alloc_chunk(Arena *arena)
{
    const usize chunk_size_in_bytes =
        sizeof(ArenaChunk) + arena->elem_size_in_bytes * arena->elems_per_chunk;
    ArenaChunk *chunk = bl_malloc(chunk_size_in_bytes);
    if (!chunk) BL_ABORT("bad alloc");
    memset(chunk, 0, chunk_size_in_bytes);
    return chunk;
}

static INLINE void *get_from_chunk(Arena *arena, ArenaChunk *chunk, s32 i)
{
    BL_ASSERT(i >= 0 && i < arena->elems_per_chunk);
    void *    elem = (void *)((char *)chunk + sizeof(ArenaChunk) + i * arena->elem_size_in_bytes);
    ptrdiff_t adj;
    align_ptr_up(&elem, arena->elem_alignment, &adj);
    BL_ASSERT(adj < arena->elem_alignment);
    return elem;
}

static INLINE ArenaChunk *free_chunk(Arena *arena, ArenaChunk *chunk)
{
    if (!chunk) return NULL;
    ArenaChunk *next = chunk->next;
    if (arena->elem_dtor) {
        for (s32 i = 0; i < chunk->count; ++i) {
            arena->elem_dtor(get_from_chunk(arena, chunk, i));
        }
    }
    bl_free(chunk);
    return next;
}

void arena_init(Arena *       arena,
                usize         elem_size_in_bytes,
                s32           elem_alignment,
                s32           elems_per_chunk,
                ArenaElemDtor elem_dtor)
{
    arena->elem_size_in_bytes = elem_size_in_bytes + elem_alignment;
    arena->elems_per_chunk    = elems_per_chunk;
    arena->elem_alignment     = elem_alignment;
    arena->first_chunk        = NULL;
    arena->current_chunk      = NULL;
    arena->elem_dtor          = elem_dtor;
    arena->sync               = sync_new();
}

void arena_terminate(Arena *arena)
{
    ArenaChunk *chunk = arena->first_chunk;
    while (chunk) {
        chunk = free_chunk(arena, chunk);
    }
    sync_delete(arena->sync);
}

void *arena_alloc(Arena *arena)
{
    pthread_mutex_lock(&arena->sync->mtx);
    if (!arena->current_chunk) {
        arena->current_chunk = alloc_chunk(arena);
        arena->first_chunk   = arena->current_chunk;
    }

    if (arena->current_chunk->count == arena->elems_per_chunk) {
        // last chunk node
        ArenaChunk *chunk          = alloc_chunk(arena);
        arena->current_chunk->next = chunk;
        arena->current_chunk       = chunk;
    }

    void *elem = get_from_chunk(arena, arena->current_chunk, arena->current_chunk->count++);
    BL_ASSERT(is_aligned(elem, arena->elem_alignment) && "Unaligned allocation of arena element!");
    pthread_mutex_unlock(&arena->sync->mtx);
    return elem;
}
