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
#include "mir_printer.h"

#define ARENA_CHUNK_COUNT 512

typedef struct
{
  MirModule   module;
  Builder *   builder;
  Assembly *  assembly;
  MirArenas * arenas;
  BHashTable *buildin_type_table;
  MirBlock *  curr_block;
  bool        verbose;
} Context;

static const char *buildin_type_names[_BUILDIN_TYPE_COUNT] = {"s32"};

static MirType entry_type = {.kind = MIR_TYPE_TYPE, .name = "type"};

static MirType entry_s32 = {.kind                   = MIR_TYPE_INT,
                            .name                   = "s32",
                            .data.integer.bitcount  = 32,
                            .data.integer.is_signed = true};

static void
block_dtor(MirBlock *block)
{
  bo_unref(block->instructions);
}

static inline MirType *
create_type(Context *cnt, MirTypeKind kind)
{
  MirType *tmp = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind    = kind;
  return tmp;
}

static inline MirBlock *
add_block(Context *cnt, const char *name)
{
  MirBlock *tmp     = arena_alloc(&cnt->arenas->block_arena);
  tmp->name         = name;
  tmp->instructions = bo_array_new(sizeof(MirInstr *));
  cnt->curr_block   = tmp;

  /*
  assert(cnt->curr_fn);
  bo_array_push_back(cnt->curr_fn->blocks, tmp);*/
  return tmp;
}

static inline void
init_module(MirModule *module, const char *name)
{
  module->name = name;
  /*module->fns     = bo_array_new(sizeof(MirFn *));
    module->globals = bo_array_new(sizeof(MirVar *));*/
}

static inline void
terminate_module(MirModule *module)
{
  bo_unref(module->fns);
  bo_unref(module->globals);
}

/* instructions */
static inline BuildinType
is_buildin_type(Context *cnt, const uint64_t hash)
{
  if (!bo_htbl_has_key(cnt->buildin_type_table, hash)) return BUILDIN_TYPE_NONE;
  return bo_htbl_at(cnt->buildin_type_table, hash, BuildinType);
}

static inline MirType *
get_buildin(BuildinType id)
{
  switch (id) {
  case BUILDIN_TYPE_S32:
    return &entry_s32;
  default:
    bl_abort("invalid buildin type");
  }
}

static void
mir_ast(Context *cnt, Ast *node);

void
mir_ast(Context *cnt, Ast *node)
{}

/* public */
void
mir_arenas_init(MirArenas *arenas)
{
  arena_init(&arenas->block_arena, sizeof(MirBlock), ARENA_CHUNK_COUNT, (ArenaElemDtor)block_dtor);
  arena_init(&arenas->instr_arena, sizeof(MirInstr), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->type_arena, sizeof(MirType), ARENA_CHUNK_COUNT, NULL);
}

void
mir_arenas_terminate(MirArenas *arenas)
{
  arena_terminate(&arenas->block_arena);
  arena_terminate(&arenas->instr_arena);
  arena_terminate(&arenas->type_arena);
}

static void
_type_to_str(char *buf, size_t len, MirType *type)
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
  case MIR_TYPE_INT:
    append_buf(buf, len, "integer");
    break;

  case MIR_TYPE_FN: {
    append_buf(buf, len, "fn");
    break;
  }

  default:
    bl_abort("unimplemented");
  }
}

void
mir_type_to_str(char *buf, int len, MirType *type)
{
  if (!buf || !len) return;
  buf[0] = '\0';
  _type_to_str(buf, len, type);
}

void
mir_run(Builder *builder, Assembly *assembly)
{
  Context cnt;
  cnt.builder  = builder;
  cnt.assembly = assembly;
  cnt.arenas   = &builder->mir_arenas;
  cnt.verbose  = builder->flags & BUILDER_VERBOSE;

  /* INIT BUILDINS */
  uint64_t tmp;
  cnt.buildin_type_table = bo_htbl_new(sizeof(BuildinType), _BUILDIN_TYPE_COUNT);
  for (int i = 0; i < _BUILDIN_TYPE_COUNT; ++i) {
    tmp = bo_hash_from_str(buildin_type_names[i]);
    bo_htbl_insert(cnt.buildin_type_table, tmp, i);
  }
  /* INIT BUILDINS */

  init_module(&cnt.module, "main");

  Unit *unit;

  barray_foreach(assembly->units, unit)
  {
    mir_ast(&cnt, unit->ast);
  }

  if (cnt.verbose) mir_printer_module(&cnt.module);

  bo_unref(cnt.buildin_type_table);
  terminate_module(&cnt.module);
}
