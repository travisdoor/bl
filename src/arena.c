//************************************************************************************************
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
//************************************************************************************************

#include "arena.h"

#define MAX_ALIGNMENT 16

typedef struct ArenaChunk {
    struct ArenaChunk *next;
    s32                count;
} ArenaChunk;

static INLINE ArenaChunk *alloc_chunk(Arena *arena)
{
    const usize chunk_size_in_bytes = arena->elem_size_in_bytes * arena->elems_per_chunk;
    ArenaChunk *chunk               = bl_malloc(chunk_size_in_bytes);
    if (!chunk) BL_ABORT("bad alloc");

#ifdef BL_PLATFORM_WINDOWS
    memset(chunk, 0, chunk_size_in_bytes);
#else
    bzero(chunk, chunk_size_in_bytes);
#endif
    chunk->count = 1;
    return chunk;
}

static INLINE void *get_from_chunk(Arena *arena, ArenaChunk *chunk, s32 i)
{
    void *elem = (void *)((char *)chunk + (i * arena->elem_size_in_bytes));
    /* New node pointer in chunk must be aligned. (ALLOCATED SIZE FOR EVERY NODE MUST BE
     * sizeof(node_t) + MAX_ALIGNMENT) */
    ptrdiff_t adj;
    align_ptr_up(&elem, MAX_ALIGNMENT, &adj);
    BL_ASSERT(adj < MAX_ALIGNMENT);
    return elem;
}

static INLINE ArenaChunk *free_chunk(Arena *arena, ArenaChunk *chunk)
{
    if (!chunk) return NULL;

    ArenaChunk *next = chunk->next;
    for (s32 i = 0; i < chunk->count - 1; ++i) {
        if (arena->elem_dtor) arena->elem_dtor(get_from_chunk(arena, chunk, i + 1));
    }

    bl_free(chunk);
    return next;
}

void arena_init(Arena *       arena,
                usize         elem_size_in_bytes,
                s32           elems_per_chunk,
                ArenaElemDtor elem_dtor)
{
    arena->elem_size_in_bytes = elem_size_in_bytes + MAX_ALIGNMENT;
    arena->elems_per_chunk    = elems_per_chunk;
    arena->first_chunk        = NULL;
    arena->current_chunk      = NULL;
    arena->elem_dtor          = elem_dtor;
}

void arena_terminate(Arena *arena)
{
    ArenaChunk *chunk = arena->first_chunk;
    while (chunk) {
        chunk = free_chunk(arena, chunk);
    }
}

void *arena_alloc(Arena *arena)
{
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

    void *elem = get_from_chunk(arena, arena->current_chunk, arena->current_chunk->count);
    arena->current_chunk->count++;
    BL_ASSERT(is_aligned(elem, MAX_ALIGNMENT) && "unaligned allocation of arena element");
    return elem;
}
