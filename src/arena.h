// =================================================================================================
// bl
//
// File:   arena.h
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

#ifndef BL_ARENA_H
#define BL_ARENA_H

#include "common.h"

typedef void (*ArenaElemDtor)(void *);

struct ArenaChunk;

typedef struct Arena {
    struct ArenaChunk *   first_chunk;
    struct ArenaChunk *   current_chunk;
    usize                 elem_size_in_bytes;
    s32                   elems_per_chunk;
    ArenaElemDtor         elem_dtor;
    struct ArenaSyncImpl *sync;
} Arena;

void arena_init(Arena *       arena,
                usize         elem_size_in_bytes,
                s32           elems_per_chunk,
                ArenaElemDtor elem_dtor);

void arena_terminate(Arena *arena);

void *arena_alloc(Arena *arena);

#endif
