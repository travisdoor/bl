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
  MirFn *     curr_fn;
  MirBlock *  curr_block;
  bool        verbose;
} Context;

static MirValue entry_no_value = {.kind = MIR_VALUE_NO, .type = NULL};

static const char *buildin_type_names[_BUILDIN_TYPE_COUNT] = {"s32"};

static MirType entry_type = {.kind = MIR_TYPE_TYPE, .name = "type"};

static MirType entry_s32 = {.kind                   = MIR_TYPE_INT,
                            .name                   = "s32",
                            .data.integer.bitcount  = 32,
                            .data.integer.is_signed = true};

static void
fn_dtor(MirFn *fn)
{
  bo_unref(fn->blocks);
}

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

static inline MirInstr *
create_instr(Context *cnt, MirInstrKind kind)
{
  MirInstr *tmp = arena_alloc(&cnt->arenas->instr_arena);
  tmp->kind     = kind;
  assert(cnt->curr_block);
  bo_array_push_back(cnt->curr_block->instructions, tmp);
  return tmp;
}

static inline MirValue *
create_value(Context *cnt, MirValueKind kind)
{
  MirValue *tmp = arena_alloc(&cnt->arenas->value_arena);
  tmp->kind     = kind;
  return tmp;
}

static inline MirType *
create_type_fn(Context *cnt, MirType args[], unsigned argc)
{
  MirType *tmp = create_type(cnt, MIR_TYPE_FN);
  return tmp;
}

static inline MirValue *
add_fn(Context *cnt, MirType *type, const char *name)
{
  MirValue *value = create_value(cnt, MIR_VALUE_FN);
  value->type     = type;

  MirFn *tmp  = arena_alloc(&cnt->arenas->fn_arena);
  tmp->name   = name;
  tmp->value  = value;
  tmp->blocks = bo_array_new(sizeof(MirBlock *));

  cnt->curr_fn = tmp;
  bo_array_push_back(cnt->module.fns, tmp);

  return tmp->value;
}

static inline MirValue *
add_global_variable(Context *cnt, const char *name)
{
  MirVar *tmp = arena_alloc(&cnt->arenas->var_arena);
  tmp->name   = name;
  tmp->value  = create_value(cnt, MIR_VALUE_INVALID);

  bo_array_push_back(cnt->module.globals, tmp);
  return tmp->value;
}

static inline MirBlock *
add_block(Context *cnt, const char *name)
{
  MirBlock *tmp     = arena_alloc(&cnt->arenas->block_arena);
  tmp->name         = name;
  tmp->instructions = bo_array_new(sizeof(MirInstr *));
  cnt->curr_block   = tmp;
  assert(cnt->curr_fn);
  bo_array_push_back(cnt->curr_fn->blocks, tmp);
  return tmp;
}

static inline void
init_module(MirModule *module, const char *name)
{
  module->name    = name;
  module->fns     = bo_array_new(sizeof(MirFn *));
  module->globals = bo_array_new(sizeof(MirVar *));
}

static inline void
terminate_module(MirModule *module)
{
  bo_unref(module->fns);
  bo_unref(module->globals);
}

/* instructions */
static inline void
add_ret(Context *cnt, MirValue *value)
{
  MirInstr *tmp       = create_instr(cnt, MIR_INSTR_RET);
  tmp->data.ret.value = value;
  tmp->value          = &entry_no_value;
}

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

static MirValue *
mir_ast(Context *cnt, Ast *node);

static MirValue *
mir_ast_ublock(Context *cnt, Ast *node);

static MirValue *
mir_ast_decl_entity(Context *cnt, Ast *node);

static MirValue *
mir_ast_expr_lit_int(Context *cnt, Ast *node);

/* impl */
MirValue *
mir_ast_ublock(Context *cnt, Ast *node)
{
  Ast *tmp;
  barray_foreach(node->data.ublock.nodes, tmp)
  {
    mir_ast(cnt, tmp);
  }

  return NULL;
}

MirValue *
mir_ast_expr_lit_int(Context *cnt, Ast *node)
{
  MirValue *tmp       = create_value(cnt, MIR_VALUE_INT);
  tmp->data.int_value = node->data.expr_integer.val;
  tmp->type           = &entry_s32;
  return tmp;
}

MirValue *
mir_ast_decl_entity(Context *cnt, Ast *node)
{
  MirValue *var = add_global_variable(cnt, node->data.decl.name->data.ident.str);

  if (node->data.decl.type) {
    Ast *ast_type_ref = node->data.decl.type;
    assert(ast_is_type(ast_type_ref));
    assert(ast_type_ref->kind == AST_TYPE_REF);

    Ast *ident = ast_type_ref->data.type_ref.ident;
    assert(ident);
    BuildinType id = is_buildin_type(cnt, ident->data.ident.hash);
    assert(id != BUILDIN_TYPE_NONE);

    var->type = get_buildin(id);
  }

  Ast *ast_expr = node->data.decl_entity.value;
  if (ast_expr) {
    MirValue *initializer = mir_ast(cnt, ast_expr);
    assert(initializer);

    /* TODO: cmp types of initializer and variable */
    var->data.int_value = initializer->data.int_value;
    var->has_data       = true;
  }

  assert(var->type);
  return var;
}

MirValue *
mir_ast(Context *cnt, Ast *node)
{
  if (!node) return NULL;
  switch (node->kind) {
  case AST_UBLOCK:
    return mir_ast_ublock(cnt, node);
  case AST_DECL_ENTITY:
    return mir_ast_decl_entity(cnt, node);
  case AST_EXPR_LIT_INT:
    return mir_ast_expr_lit_int(cnt, node);
  default:
    bl_abort("invalid node");
  }

  return NULL;
}

/* public */
void
mir_arenas_init(MirArenas *arenas)
{
  arena_init(&arenas->block_arena, sizeof(MirBlock), ARENA_CHUNK_COUNT, (ArenaElemDtor)block_dtor);
  arena_init(&arenas->fn_arena, sizeof(MirFn), ARENA_CHUNK_COUNT, (ArenaElemDtor)fn_dtor);
  arena_init(&arenas->instr_arena, sizeof(MirInstr), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->value_arena, sizeof(MirValue), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->var_arena, sizeof(MirVar), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->type_arena, sizeof(MirType), ARENA_CHUNK_COUNT, NULL);
}

void
mir_arenas_terminate(MirArenas *arenas)
{
  arena_terminate(&arenas->block_arena);
  arena_terminate(&arenas->fn_arena);
  arena_terminate(&arenas->instr_arena);
  arena_terminate(&arenas->value_arena);
  arena_terminate(&arenas->var_arena);
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
  cnt.curr_fn  = NULL;

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
