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

/* TODO: this is temporary solution, we need some kind of fast allocator for different instructions
 * with different size (we allocate pool where every element has size of biggest instruction -> we
 * are wastig memory) */
union _MirInstr
{
  MirInstrDeclVar  var;
  MirInstrConstInt const_int;
  MirInstrLoad     load;
  MirInstrStore    store;
  MirInstrRet      ret;
};

typedef struct
{
  Builder *   builder;
  Assembly *  assembly;
  MirArenas * arenas;
  BHashTable *buildin_type_table;

  BArray *  execs;
  MirExec * curr_exec;
  MirBlock *curr_block;
  bool      verbose;
} Context;

static const char *buildin_type_names[_BUILDIN_TYPE_COUNT] = {"s32"};

static MirType entry_type = {.kind = MIR_TYPE_TYPE, .name = "type"};

static MirType entry_s32 = {.kind                   = MIR_TYPE_INT,
                            .name                   = "s32",
                            .data.integer.bitcount  = 32,
                            .data.integer.is_signed = true};

static MirType entry_void = {.kind = MIR_TYPE_VOID, .name = "void"};

static void
block_dtor(MirBlock *block)
{
  bo_unref(block->instructions);
}

static void
exec_dtor(MirExec *exec)
{
  bo_unref(exec->blocks);
}

static inline unsigned
get_id(Context *cnt)
{
  assert(cnt->curr_exec);
  return cnt->curr_exec->id_counter++;
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

#define add_instr(_cnt, _kind, _type, _t) ((_t)_add_instr((_cnt), (_kind), (_type)))

static MirInstr *
_add_instr(Context *cnt, MirInstrKind kind, MirType *type)
{
  MirInstr *tmp = arena_alloc(&cnt->arenas->instr_arena);
  tmp->kind     = kind;
  tmp->id       = get_id(cnt);
  tmp->type     = type;

  assert(cnt->curr_block);
  bo_array_push_back(cnt->curr_block->instructions, tmp);

  return tmp;
}

static MirType *
create_type(Context *cnt, MirTypeKind kind)
{
  MirType *tmp = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind    = kind;
  return tmp;
}

static MirExec *
add_exec(Context *cnt)
{
  MirExec *tmp = arena_alloc(&cnt->arenas->exec_arena);
  tmp->blocks  = bo_array_new(sizeof(MirBlock *));

  bo_array_push_back(cnt->execs, tmp);
  cnt->curr_exec = tmp;
  return tmp;
}

static MirBlock *
add_block(Context *cnt, const char *name)
{
  MirBlock *tmp     = arena_alloc(&cnt->arenas->block_arena);
  tmp->name         = name;
  tmp->instructions = bo_array_new(sizeof(MirInstr *));
  cnt->curr_block   = tmp;

  assert(cnt->curr_exec);
  bo_array_push_back(cnt->curr_exec->blocks, tmp);
  return tmp;
}

/* instructions */
static MirInstr *
add_instr_decl_var(Context *cnt, MirType *type)
{
  MirInstrDeclVar *tmp = add_instr(cnt, MIR_INSTR_DECL_VAR, type, MirInstrDeclVar *);
  return &tmp->base;
}

static MirInstr *
add_instr_const_int(Context *cnt, uint64_t val)
{
  MirInstrConstInt *tmp = add_instr(cnt, MIR_INSTR_CONST_INT, &entry_s32, MirInstrConstInt *);
  tmp->value            = val;
  return &tmp->base;
}

static MirInstr *
add_instr_ret(Context *cnt, MirInstr *value)
{
  MirType *type = NULL;
  if (value) {
    assert(value->type);
    type = value->type;
  } else {
    type = &entry_void;
  }

  MirInstrRet *tmp = add_instr(cnt, MIR_INSTR_RET, type, MirInstrRet *);
  tmp->value       = value;
  return &tmp->base;
}

static MirInstr *
mir_ast(Context *cnt, Ast *node);

static MirInstr *
mir_ast_ublock(Context *cnt, Ast *ublock);

static MirInstr *
mir_ast_decl_entity(Context *cnt, Ast *entity);

static MirInstr *
mir_ast_expr_lit_int(Context *cnt, Ast *expr);

MirInstr *
mir_ast_ublock(Context *cnt, Ast *ublock)
{
  Ast *tmp;
  barray_foreach(ublock->data.ublock.nodes, tmp) mir_ast(cnt, tmp);
  return NULL;
}

MirInstr *
mir_ast_expr_lit_int(Context *cnt, Ast *expr)
{
  return add_instr_const_int(cnt, expr->data.expr_integer.val);
}

MirInstr *
mir_ast_decl_entity(Context *cnt, Ast *entity)
{
  add_exec(cnt);
  add_block(cnt, "entry");

  MirType * type = &entry_s32;
  MirInstr *var  = add_instr_decl_var(cnt, type);
  MirInstr *init = mir_ast(cnt, entity->data.decl_entity.value);
  MirInstr *ret  = add_instr_ret(cnt, NULL);

  return var;
}

MirInstr *
mir_ast(Context *cnt, Ast *node)
{
  if (!node) return NULL;
  switch (node->kind) {
  case AST_UBLOCK:
    mir_ast_ublock(cnt, node);
    break;
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
  arena_init(&arenas->instr_arena, sizeof(union _MirInstr), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->type_arena, sizeof(MirType), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->exec_arena, sizeof(MirExec), ARENA_CHUNK_COUNT, (ArenaElemDtor)exec_dtor);
}

void
mir_arenas_terminate(MirArenas *arenas)
{
  arena_terminate(&arenas->block_arena);
  arena_terminate(&arenas->instr_arena);
  arena_terminate(&arenas->type_arena);
  arena_terminate(&arenas->exec_arena);
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
  memset(&cnt, 0, sizeof(Context));
  cnt.builder  = builder;
  cnt.assembly = assembly;
  cnt.arenas   = &builder->mir_arenas;
  cnt.verbose  = builder->flags & BUILDER_VERBOSE;
  cnt.execs    = bo_array_new(sizeof(MirExec *));

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

  if (cnt.verbose) {
    MirExec *tmp;
    barray_foreach(cnt.execs, tmp)
    {
      mir_printer_exec(tmp);
    }
  }

  // if (cnt.verbose) mir_printer_module(&cnt.module);

  bo_unref(cnt.buildin_type_table);
  bo_unref(cnt.execs);
}
