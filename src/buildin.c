//************************************************************************************************
// bl
//
// File:   buildin.c
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

#include "buildin.h"
#include "ast.h"

static const char *names[BUILDIN_COUNT] = {"u8", "u16", "u32", "u64", "usize",
                                           "s8", "s16", "s32", "s64"};

static Ast nodes[BUILDIN_COUNT] = {
    // u8
    {.kind                   = AST_TYPE_INT,
     .src                    = NULL,
     .next                   = NULL,
     .type_integer.name      = "u8",
     .type_integer.bitcount  = 8,
     .type_integer.is_signed = false},

    // u16
    {.kind                   = AST_TYPE_INT,
     .src                    = NULL,
     .next                   = NULL,
     .type_integer.name      = "u16",
     .type_integer.bitcount  = 16,
     .type_integer.is_signed = false},

    // u32
    {.kind                   = AST_TYPE_INT,
     .src                    = NULL,
     .next                   = NULL,
     .type_integer.name      = "u32",
     .type_integer.bitcount  = 32,
     .type_integer.is_signed = false},

    // u64
    {.kind                   = AST_TYPE_INT,
     .src                    = NULL,
     .next                   = NULL,
     .type_integer.name      = "u64",
     .type_integer.bitcount  = 64,
     .type_integer.is_signed = false},

    // usize
    {.kind                   = AST_TYPE_INT,
     .src                    = NULL,
     .next                   = NULL,
     .type_integer.name      = "usize",
     .type_integer.bitcount  = 64,
     .type_integer.is_signed = false},

    // s8
    {.kind                   = AST_TYPE_INT,
     .src                    = NULL,
     .next                   = NULL,
     .type_integer.name      = "s8",
     .type_integer.bitcount  = 8,
     .type_integer.is_signed = true},

    // s16
    {.kind                   = AST_TYPE_INT,
     .src                    = NULL,
     .next                   = NULL,
     .type_integer.name      = "s16",
     .type_integer.bitcount  = 16,
     .type_integer.is_signed = true},

    // s32
    {.kind                   = AST_TYPE_INT,
     .src                    = NULL,
     .next                   = NULL,
     .type_integer.name      = "s32",
     .type_integer.bitcount  = 32,
     .type_integer.is_signed = true},

    // s64
    {.kind                   = AST_TYPE_INT,
     .src                    = NULL,
     .next                   = NULL,
     .type_integer.name      = "s64",
     .type_integer.bitcount  = 64,
     .type_integer.is_signed = true},
};

void
buildin_init(Buildin *buildin)
{
  assert(buildin);
  buildin->table = bo_htbl_new(sizeof(Ast *), BUILDIN_COUNT);

  for (int i = 0; i < BUILDIN_COUNT; ++i) {
    buildin->names[i]  = names[i];
    buildin->hashes[i] = bo_hash_from_str(names[i]);
    Ast *node          = &nodes[i];
    bo_htbl_insert(buildin->table, buildin->hashes[i], node);
  }
}

void
buildin_terminate(Buildin *buildin)
{
  bo_unref(buildin->table);
}

struct Ast *
buildin_get(Buildin *buildin, uint64_t hash)
{
  return bo_htbl_at(buildin->table, hash, Ast *);
}
