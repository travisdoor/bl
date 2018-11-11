//************************************************************************************************
// bl
//
// File:   mir.c
// Author: Martin Dorazil
// Date:   3/15/18
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

#include "mir.h"
#include "unit.h"
#include "common.h"
#include "builder.h"
#include "assembly.h"
#include "arena.h"

#define ARENA_CHUNK_COUNT 512

typedef struct
{
  Builder *   builder;
  Assembly *  assembly;
  Arena *     mir_instr_arena;
  Arena *     mir_block_arena;
  BHashTable *buildin_type_table;
} Context;

static const char *buildin_type_names[_BUILDIN_TYPE_COUNT] = {"s32"};

static Type entry_s32 = {
    .kind = TYPE_INT, .name = "s32", .data.integer.bitcount = 32, .data.integer.is_signed = true};

static inline MirInstr *
create_instr(Arena *arena, MirInstrKind kind)
{
  MirInstr *tmp = arena_alloc(arena);
  tmp->kind     = kind;
  return tmp;
}

static void
mir_block_dtor(MirBlock *block)
{
  bo_unref(block->instructions);
}

static inline BuildinType
is_buildin_type(Context *cnt, const uint64_t hash)
{
  if (!bo_htbl_has_key(cnt->buildin_type_table, hash)) return BUILDIN_TYPE_NONE;
  return bo_htbl_at(cnt->buildin_type_table, hash, BuildinType);
}

static MirBlock *
create_block(Context *cnt);

static MirInstr *
mir_ast(Context *cnt, Ast *node);

static void
mir_ast_ublock(Context *cnt, Ast *node);

/* impl */
MirBlock *
create_block(Context *cnt)
{
  MirBlock *tmp     = arena_alloc(cnt->mir_block_arena);
  tmp->instructions = bo_array_new(sizeof(MirInstr *));
  return tmp;
}

void
mir_ast_ublock(Context *cnt, Ast *node)
{
  Ast *tmp;
  barray_foreach(node->data.ublock.nodes, tmp)
  {
    mir_ast(cnt, tmp);
  }
}

MirInstr *
mir_ast(Context *cnt, Ast *node)
{
  if (!node) return NULL;
  switch (node->kind) {
  case AST_UBLOCK:
    mir_ast_ublock(cnt, node);
    break;
  default:
    bl_abort("invalid node");
  }

  return NULL;
}

/* public */
void
mir_instr_arena_init(Arena *arena)
{
  arena_init(arena, sizeof(MirInstr), ARENA_CHUNK_COUNT, NULL);
}

void
mir_block_arena_init(Arena *arena)
{
  arena_init(arena, sizeof(MirBlock), ARENA_CHUNK_COUNT, (ArenaElemDtor)mir_block_dtor);
}

void
mir_value_arena_init(Arena *arena)
{
  arena_init(arena, sizeof(MirValue), ARENA_CHUNK_COUNT, NULL);
}

void
mir_run(Builder *builder, Assembly *assembly)
{
  Context cnt;
  cnt.builder         = builder;
  cnt.assembly        = assembly;
  cnt.mir_instr_arena = &builder->mir_instr_arena;
  cnt.mir_block_arena = &builder->mir_block_arena;

  /* INIT BUILDINS */
  uint64_t tmp;
  cnt.buildin_type_table = bo_htbl_new(sizeof(BuildinType), _BUILDIN_TYPE_COUNT);
  for (int i = 0; i < _BUILDIN_TYPE_COUNT; ++i) {
    tmp = bo_hash_from_str(buildin_type_names[i]);
    bo_htbl_insert(cnt.buildin_type_table, tmp, i);
  }
  /* INIT BUILDINS */

  Unit *unit;

  barray_foreach(assembly->units, unit)
  {
    mir_ast(&cnt, unit->ast);
  }

  bo_unref(cnt.buildin_type_table);
}
