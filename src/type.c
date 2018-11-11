//************************************************************************************************
// bl
//
// File:   type.c
// Author: Martin Dorazil
// Date:   11/11/18
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

#include "type.h"
#include "arena.h"

#define ARENA_CHUNK_COUNT 256

void
type_arena_init(Arena *arena)
{
  arena_init(arena, sizeof(Type), ARENA_CHUNK_COUNT, NULL);
}

Type *
_type_create(struct Arena *arena, TypeKind kind)
{
  Type *tmp = arena_alloc(arena);
  tmp->kind = kind;
  return tmp;
}

static void
_type_to_str(char *buf, size_t len, Type *type)
{
#define append_buf(buf, len, str)                                                                  \
  {                                                                                                \
    const size_t filled = strlen(buf);                                                             \
    snprintf((buf) + filled, (len)-filled, "%s", str);                                             \
  }

  if (!buf) return;
  if (!type) {
    append_buf(buf, len, "?");
    return;
  }

  if (type->name) {
    append_buf(buf, len, type->name);
    return;
  }

  switch (type->kind) {
  case TYPE_INT:
    append_buf(buf, len, "integer");
    break;

  case TYPE_FN: {
    append_buf(buf, len, "fn");
    break;
  }

  case TYPE_STRUCT: {
    append_buf(buf, len, "struct");
    break;
  }

  default:
    bl_abort("unimplemented");
  }
}

void
type_to_str(char *buf, int len, Type *type)
{
  if (!buf || !len) return;
  buf[0] = '\0';
  _type_to_str(buf, len, type);
}
