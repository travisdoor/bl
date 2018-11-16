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

/* Pipeline:
 *   - generate first pass from AST with unknown types
 *   - generate implicit type resolvers
 *   - analyze and execute type resolvers
 *   - analyze rest of code and also execute compile time executables
 */

/* TODO: this is temporary solution, we need some kind of fast allocator for different instructions
 * with different size (we allocate pool where every element has size of biggest instruction -> we
 * are wastig memory) */
union _MirInstr
{
  MirInstrDeclVar      var;
  MirInstrConst        cnst;
  MirInstrLoad         load;
  MirInstrStore        store;
  MirInstrRet          ret;
  MirInstrBinop        binop;
  MirInstrValidateType validate_type;
};

typedef struct
{
  Builder *  builder;
  Assembly * assembly;
  MirArenas *arenas;

  BArray *    functions;
  BArray *    needed_types;
  BHashTable *type_resolvers;

  struct
  {
    BHashTable *table;

    MirType *entry_type;
    MirType *entry_s32;
    MirType *entry_void;
    MirType *entry_resolve_type_fn;
  } buildin_types;

  struct
  {
    MirExec * exec;
    MirBlock *block;
    unsigned  id_counter;
  } cursor;

  LLVMContextRef    llvm_cnt;
  LLVMModuleRef     llvm_module;
  LLVMTargetDataRef llvm_td;

  bool verbose;
} Context;

static const char *buildin_type_names[_BUILDIN_TYPE_COUNT] = {"s32"};

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

static void
type_dtor(MirType *type)
{
  switch (type->kind) {
  case MIR_TYPE_FN:
    bo_unref(type->data.fn.arg_types);
    break;
  default:
    break;
  }
}

static inline BuildinType
is_buildin_type(Context *cnt, const uint64_t hash)
{
  if (!bo_htbl_has_key(cnt->buildin_types.table, hash)) return BUILDIN_TYPE_NONE;
  return bo_htbl_at(cnt->buildin_types.table, hash, BuildinType);
}

static inline MirType *
get_buildin(Context *cnt, BuildinType id)
{
  switch (id) {
  case BUILDIN_TYPE_S32:
    return cnt->buildin_types.entry_s32;
  default:
    bl_abort("invalid buildin type");
  }
}

/* FW decls */
static void
schedule_type_resolve(Context *cnt, Ast *node);

static void
create_type_resolvers(Context *cnt);

static MirInstr *
ast(Context *cnt, Ast *node);

static void
ast_ublock(Context *cnt, Ast *ublock);

static void
ast_block(Context *cnt, Ast *block);

static void
ast_stmt_return(Context *cnt, Ast *ret);

static MirInstr *
ast_decl_entity(Context *cnt, Ast *entity);

static MirInstr *
ast_type_ref(Context *cnt, Ast *type_ref);

static MirInstr *
ast_expr_lit_int(Context *cnt, Ast *expr);

static MirInstr *
ast_expr_lit_fn(Context *cnt, Ast *lit_fn);

static MirInstr *
ast_expr_binop(Context *cnt, Ast *binop);

static LLVMTypeRef
to_llvm_type(Context *cnt, MirType *type, size_t *out_size);

/* analyze */
static void
analyze_instr_const(Context *cnt, MirInstrConst *cnst);

static void
analyze_instr_validate_type(Context *cnt, MirInstrValidateType *vld);

static void
analyze_instr_ret(Context *cnt, MirInstrRet *ret);

static void
analyze_instr(Context *cnt, MirInstr *instr);

static void
analyze_entry_block(Context *cnt, MirBlock *entry);

static void
analyze_fn(Context *cnt, MirFn *fn);

static void
analyze_type_resolvers(Context *cnt);

/* impl */

static MirType *
create_type_meta(Context *cnt, Ast *node)
{
  assert(node);
  MirType *tmp        = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind           = MIR_TYPE_META;
  tmp->name           = "?";
  tmp->data.meta.node = node;
  return tmp;
}

static MirType *
create_type_type(Context *cnt, MirType *spec)
{
  MirType *tmp        = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind           = MIR_TYPE_TYPE;
  tmp->name           = "type";
  tmp->data.type.spec = spec;
  return tmp;
}

static MirType *
create_type_void(Context *cnt)
{
  MirType *tmp = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind    = MIR_TYPE_VOID;
  tmp->name    = "void";
  return tmp;
}

static MirType *
create_type_int(Context *cnt, const char *name, int bitcount, bool is_signed)
{
  assert(bitcount > 0);
  MirType *tmp                = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind                   = MIR_TYPE_INT;
  tmp->name                   = name;
  tmp->data.integer.bitcount  = bitcount;
  tmp->data.integer.is_signed = is_signed;

  return tmp;
}

static MirType *
create_type_ptr(Context *cnt, MirType *src_type)
{
  MirType *tmp       = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind          = MIR_TYPE_PTR;
  tmp->data.ptr.next = src_type;

  return tmp;
}

static MirType *
create_type_fn(Context *cnt, MirType *ret_type, BArray *arg_types)
{
  assert(arg_types && ret_type);
  MirType *tmp           = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind              = MIR_TYPE_FN;
  tmp->data.fn.arg_types = arg_types;
  tmp->data.fn.ret_type  = ret_type;

  return tmp;
}

static MirVar *
create_var(Context *cnt, MirType *type, Ast *name)
{
  assert(name);
  MirVar *tmp = arena_alloc(&cnt->arenas->var_arena);
  tmp->name   = name;
  tmp->type   = type;
  return tmp;
}

static MirExec *
create_exec(Context *cnt)
{
  MirExec *tmp = arena_alloc(&cnt->arenas->exec_arena);
  tmp->blocks  = bo_array_new(sizeof(MirBlock *));
  return tmp;
}

static MirFn *
add_fn(Context *cnt, MirType *type, Ast *name)
{
  MirFn *tmp = arena_alloc(&cnt->arenas->fn_arena);
  tmp->name  = name;
  tmp->type  = type;
  tmp->exec  = create_exec(cnt);

  cnt->cursor.block      = NULL;
  cnt->cursor.exec       = tmp->exec;
  cnt->cursor.id_counter = 0;
  bo_array_push_back(cnt->functions, tmp);
  return tmp;
}

static MirBlock *
add_block(Context *cnt, const char *name)
{
  MirBlock *tmp     = arena_alloc(&cnt->arenas->block_arena);
  tmp->name         = name;
  tmp->instructions = bo_array_new(sizeof(MirInstr *));
  cnt->cursor.block = tmp;

  assert(cnt->cursor.exec);
  bo_array_push_back(cnt->cursor.exec->blocks, tmp);
  if (!cnt->cursor.exec->entry_block) cnt->cursor.exec->entry_block = tmp;
  return tmp;
}

/* instructions */
#define add_instr(_cnt, _kind, _node, _type, _comptime, _t)                                        \
  ((_t)_add_instr((_cnt), (_kind), (_node), (_type), (_comptime)))

static MirInstr *
_add_instr(Context *cnt, MirInstrKind kind, Ast *node, MirType *type, bool comptime)
{
  assert(type);
  MirInstr *tmp = arena_alloc(&cnt->arenas->instr_arena);
  tmp->kind     = kind;
  tmp->node     = node;
  tmp->id       = cnt->cursor.id_counter++;
  tmp->type     = type;
  tmp->comptime = comptime;

  assert(cnt->cursor.block);
  bo_array_push_back(cnt->cursor.block->instructions, tmp);

  return tmp;
}

static void
push_instr(Context *cnt, MirInstr *instr)
{
  assert(instr);
  assert(cnt->cursor.block);
  bo_array_push_back(cnt->cursor.block->instructions, instr);
}

static MirInstr *
add_instr_decl_var(Context *cnt, Ast* node, MirType *type, Ast *name)
{
  MirVar *var = create_var(cnt, type, name);

  MirType *        type_ptr = create_type_ptr(cnt, type);
  MirInstrDeclVar *tmp = add_instr(cnt, MIR_INSTR_DECL_VAR, node, type_ptr, false, MirInstrDeclVar *);
  tmp->var             = var;
  return &tmp->base;
}

static MirInstr *
add_instr_const_int(Context *cnt, Ast *node, uint64_t val)
{
  MirType *      type = cnt->buildin_types.entry_s32;
  MirInstrConst *tmp  = add_instr(cnt, MIR_INSTR_CONST, node, type, true, MirInstrConst *);
  tmp->int_value      = val;
  return &tmp->base;
}

static MirInstr *
add_instr_const_type(Context *cnt, Ast *node, MirType *type)
{
  MirType *      type_type = create_type_type(cnt, type);
  MirInstrConst *tmp       = add_instr(cnt, MIR_INSTR_CONST, node, type_type, true, MirInstrConst *);
  tmp->type_value          = type;
  return &tmp->base;
}

static MirInstr *
add_instr_ret(Context *cnt, Ast* node, MirInstr *value)
{
  MirType *type = NULL;
  if (value) {
    assert(value->type);
    type = value->type;
    ++value->ref_count;
  } else {
    type = cnt->buildin_types.entry_void;
  }

  MirInstrRet *tmp = add_instr(cnt, MIR_INSTR_RET, node, type, false, MirInstrRet *);
  tmp->value       = value;

  /* add terminate current block and for return statement terminate also executable unit */
  assert(cnt->cursor.block);
  assert(cnt->cursor.exec);

  /* when current block is already terminated we produce unrecheable code replacement later during
   * analyze */
  if (!cnt->cursor.block->terminal) cnt->cursor.block->terminal = &tmp->base;
  if (!cnt->cursor.exec->ret) cnt->cursor.exec->ret = &tmp->base;
  ++tmp->base.ref_count;

  return &tmp->base;
}

static MirInstr *
add_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest)
{
  assert(src && dest);
  ++src->ref_count;
  ++dest->ref_count;
  MirInstrStore *tmp = add_instr(cnt, MIR_INSTR_STORE, node, dest->type, false, MirInstrStore *);
  tmp->src           = src;
  tmp->dest          = dest;
  return &tmp->base;
}

static MirInstr *
add_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op)
{
  assert(lhs && rhs);
  ++lhs->ref_count;
  ++rhs->ref_count;
  MirType *      type = lhs->type;
  MirInstrBinop *tmp =
    add_instr(cnt, MIR_INSTR_BINOP, node, type, lhs->comptime && rhs->comptime, MirInstrBinop *);
  tmp->lhs = lhs;
  tmp->rhs = rhs;
  tmp->op  = op;
  return &tmp->base;
}

static MirInstr *
add_instr_validate_type(Context *cnt, MirInstr *src)
{
  assert(src);
  ++src->ref_count;
  MirInstrValidateType *tmp = add_instr(cnt, MIR_INSTR_VALIDATE_TYPE, NULL, cnt->buildin_types.entry_void,
                                        true, MirInstrValidateType *);
  tmp->src                  = src;
  return &tmp->base;
}

/* LLVM */
LLVMTypeRef
to_llvm_type(Context *cnt, MirType *type, size_t *out_size)
{
  if (!type) return NULL;
  LLVMTypeRef result = NULL;

  switch (type->kind) {
  case MIR_TYPE_TYPE:
  case MIR_TYPE_META:
    bl_abort("unable to convert to llvm type");

  case MIR_TYPE_VOID: {
    if (out_size) *out_size = 0;
    result = LLVMVoidTypeInContext(cnt->llvm_cnt);
    break;
  }

  case MIR_TYPE_INT: {
    result = LLVMIntTypeInContext(cnt->llvm_cnt, type->data.integer.bitcount);
    if (out_size) *out_size = LLVMSizeOfTypeInBits(cnt->llvm_td, result);
    break;
  }

  case MIR_TYPE_PTR: {
    MirType *tmp = type->data.ptr.next;
    assert(tmp);
    assert(tmp->llvm_type);
    result = LLVMPointerType(tmp->llvm_type, 0);
    if (out_size) *out_size = LLVMSizeOfTypeInBits(cnt->llvm_td, result);
    break;
  }

  case MIR_TYPE_FN: {
    MirType *tmp_ret  = type->data.fn.ret_type;
    BArray * tmp_args = type->data.fn.arg_types;
    assert(tmp_ret && tmp_ret->llvm_type && tmp_args);
    const size_t cargs = bo_array_size(tmp_args);

    LLVMTypeRef *tmp_args_llvm = bl_malloc(cargs * sizeof(LLVMTypeRef));
    if (!tmp_args_llvm) bl_abort("bad alloc");

    MirType *tmp_arg;
    barray_foreach(tmp_args, tmp_arg)
    {
      assert(tmp_arg->llvm_type);
      tmp_args_llvm[i] = tmp_arg->llvm_type;
    }

    result = LLVMFunctionType(tmp_ret->llvm_type, tmp_args_llvm, cargs, false);
    if (out_size) *out_size = 0;
    bl_free(tmp_args_llvm);
    break;
  }

  default:
    bl_abort("unimplemented");
  }

  return result;
}

void
schedule_type_resolve(Context *cnt, Ast *node)
{
  assert(node);
  bo_array_push_back(cnt->needed_types, node);
}

void
create_type_resolvers(Context *cnt)
{
  MirFn *resolver_fn;
  Ast *  tmp;
  barray_foreach(cnt->needed_types, tmp)
  {
    resolver_fn = add_fn(cnt, cnt->buildin_types.entry_resolve_type_fn, NULL);
    add_block(cnt, "entry");
    MirInstr *result = ast(cnt, tmp);
    assert(result);
    add_instr_validate_type(cnt, result);
    add_instr_ret(cnt, NULL, result);

    bo_htbl_insert(cnt->type_resolvers, (uint64_t)tmp, resolver_fn);
  }
}

/* analyze */
void
analyze_instr_const(Context *cnt, MirInstrConst *cnst)
{
  /* TODO: set llvm_type here!!! */
  /* TODO: set llvm_type here!!! */
  /* TODO: set llvm_type here!!! */

  if (cnst->base.ref_count) {
    /* unused instruction */
    push_instr(cnt, &cnst->base);
  }
}

void
analyze_instr_validate_type(Context *cnt, MirInstrValidateType *vld)
{
  assert(vld->src && vld->src->type);
  if (vld->src->type->kind == MIR_TYPE_TYPE) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_TYPE, vld->src->node->src, BUILDER_CUR_WORD,
                "expected type");
  }
}

void
analyze_instr_ret(Context *cnt, MirInstrRet *ret)
{
  push_instr(cnt, &ret->base);
  if (!cnt->cursor.block->terminal) cnt->cursor.block->terminal = &ret->base;
  if (!cnt->cursor.exec->ret) cnt->cursor.exec->ret = &ret->base;
}

void
analyze_instr(Context *cnt, MirInstr *instr)
{
  if (!instr) return;

  switch (instr->kind) {
  case MIR_INSTR_CONST:
    analyze_instr_const(cnt, (MirInstrConst *)instr);
    break;
  case MIR_INSTR_RET:
    analyze_instr_ret(cnt, (MirInstrRet *)instr);
    break;
  case MIR_INSTR_VALIDATE_TYPE:
    analyze_instr_validate_type(cnt, (MirInstrValidateType *)instr);
    break;
  default:
    bl_abort("missing analyze for %d", instr->kind);
  }
}

void
analyze_entry_block(Context *cnt, MirBlock *entry)
{
  assert(entry);
  assert(entry->instructions);
  add_block(cnt, "entry");

  MirInstr *tmp;
  barray_foreach(entry->instructions, tmp) analyze_instr(cnt, tmp);
}

void
analyze_fn(Context *cnt, MirFn *fn)
{
  assert(fn);
  assert(fn->exec);
  assert(fn->exec->blocks);
  assert(fn->exec->ret);

  cnt->cursor.exec = fn->exec_analyzed = create_exec(cnt);

  analyze_entry_block(cnt, fn->exec->entry_block);
}

void
analyze_type_resolvers(Context *cnt)
{
  MirFn *       tmp;
  bo_iterator_t it;
  bhtbl_foreach(cnt->type_resolvers, it)
  {
    tmp = bo_htbl_iter_peek_value(cnt->type_resolvers, &it, MirFn *);
    assert(tmp);
    analyze_fn(cnt, tmp);
  }
}

/* MIR building */
void
ast_ublock(Context *cnt, Ast *ublock)
{
  Ast *tmp;
  barray_foreach(ublock->data.ublock.nodes, tmp) ast(cnt, tmp);
}

void
ast_block(Context *cnt, Ast *block)
{
  Ast *tmp;
  barray_foreach(block->data.block.nodes, tmp) ast(cnt, tmp);
}

void
ast_stmt_return(Context *cnt, Ast *ret)
{
  MirInstr *value = ast(cnt, ret->data.stmt_return.expr);
  add_instr_ret(cnt, ret, value);
}

MirInstr *
ast_expr_lit_int(Context *cnt, Ast *expr)
{
  return add_instr_const_int(cnt, expr, expr->data.expr_integer.val);
}

MirInstr *
ast_expr_lit_fn(Context *cnt, Ast *lit_fn)
{
  Ast *block = lit_fn->data.expr_fn.block;
  assert(block);

  add_block(cnt, "entry");
  ast(cnt, block);

  return NULL;
}

MirInstr *
ast_expr_binop(Context *cnt, Ast *binop)
{
  Ast *ast_lhs = binop->data.expr_binop.lhs;
  Ast *ast_rhs = binop->data.expr_binop.rhs;
  assert(ast_lhs && ast_rhs);

  MirInstr *lhs = ast(cnt, ast_lhs);
  MirInstr *rhs = ast(cnt, ast_rhs);
  assert(lhs && rhs);

  return add_instr_binop(cnt, binop, lhs, rhs, binop->data.expr_binop.kind);
}

MirInstr *
ast_decl_entity(Context *cnt, Ast *entity)
{
  Ast *ast_name  = entity->data.decl.name;
  Ast *ast_type  = entity->data.decl.type;
  Ast *ast_value = entity->data.decl_entity.value;

  assert(ast_value);

  if (ast_value->kind == AST_EXPR_LIT_FN) {
    /* TODO: type resolving!!! */
    BArray * args = bo_array_new(sizeof(MirType *));
    MirType *type = create_type_fn(cnt, cnt->buildin_types.entry_void, args);
    MirFn *  fn   = add_fn(cnt, type, ast_name);

    /* TODO comptime = true -> test only */
    ast(cnt, ast_value);
  } else {
    MirType *type = NULL;
    if (ast_type) {
      schedule_type_resolve(cnt, ast_type);
      type = create_type_meta(cnt, ast_type);
    }

    MirInstr *decl = add_instr_decl_var(cnt, ast_name, type, ast_name);
    MirInstr *init = ast(cnt, ast_value);
    return add_instr_store(cnt, ast_value, init, decl);
  }

  return NULL;
}

MirInstr *
ast_type_ref(Context *cnt, Ast *type_ref)
{
  MirInstr *result  = NULL;
  Ast *     ast_ref = type_ref->data.type_ref.ident;
  assert(ast_ref);

  BuildinType id = is_buildin_type(cnt, ast_ref->data.ident.hash);
  if (id != BUILDIN_TYPE_NONE) {
    /* buildin primitive !!! */
    result = add_instr_const_type(cnt, ast_ref, cnt->buildin_types.entry_s32);
  }

  assert(result);
  return result;
}

MirInstr *
ast(Context *cnt, Ast *node)
{
  if (!node) return NULL;
  switch (node->kind) {
  case AST_UBLOCK:
    ast_ublock(cnt, node);
    break;
  case AST_BLOCK:
    ast_block(cnt, node);
    break;
  case AST_STMT_RETURN:
    ast_stmt_return(cnt, node);
    break;
  case AST_DECL_ENTITY:
    return ast_decl_entity(cnt, node);
  case AST_TYPE_REF:
    return ast_type_ref(cnt, node);
  case AST_EXPR_LIT_INT:
    return ast_expr_lit_int(cnt, node);
  case AST_EXPR_LIT_FN:
    return ast_expr_lit_fn(cnt, node);
  case AST_EXPR_BINOP:
    return ast_expr_binop(cnt, node);
  default:
    bl_abort("invalid node %s", ast_get_name(node));
  }

  return NULL;
}

/* public */
void
mir_arenas_init(MirArenas *arenas)
{
  arena_init(&arenas->block_arena, sizeof(MirBlock), ARENA_CHUNK_COUNT, (ArenaElemDtor)block_dtor);
  arena_init(&arenas->instr_arena, sizeof(union _MirInstr), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->type_arena, sizeof(MirType), ARENA_CHUNK_COUNT, (ArenaElemDtor)type_dtor);
  arena_init(&arenas->exec_arena, sizeof(MirExec), ARENA_CHUNK_COUNT, (ArenaElemDtor)exec_dtor);
  arena_init(&arenas->var_arena, sizeof(MirVar), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->fn_arena, sizeof(MirFn), ARENA_CHUNK_COUNT, NULL);
}

void
mir_arenas_terminate(MirArenas *arenas)
{
  arena_terminate(&arenas->block_arena);
  arena_terminate(&arenas->instr_arena);
  arena_terminate(&arenas->type_arena);
  arena_terminate(&arenas->exec_arena);
  arena_terminate(&arenas->var_arena);
  arena_terminate(&arenas->fn_arena);
}

static void
_type_to_str(char *buf, size_t len, MirType *type)
{
#define append_buf(buf, len, str)                                                                  \
  {                                                                                                \
    const size_t filled = strlen(buf);                                                             \
    snprintf((buf) + filled, (len)-filled, "%s", str);                                             \
  }

  assert(type);
  if (!buf) return;

  if (type->name) {
    append_buf(buf, len, type->name);
    return;
  }

  switch (type->kind) {
  case MIR_TYPE_INT:
    append_buf(buf, len, "integer");
    break;

  case MIR_TYPE_FN: {
    append_buf(buf, len, "fn(");

    MirType *tmp;
    BArray * args = type->data.fn.arg_types;
    assert(args);
    barray_foreach(args, tmp)
    {
      _type_to_str(buf, len, tmp);
      if (i < bo_array_size(args)) append_buf(buf, len, ", ");
    }

    append_buf(buf, len, ")");
    break;
  }

  case MIR_TYPE_PTR: {
    append_buf(buf, len, "*");
    _type_to_str(buf, len, type->data.ptr.next);
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
  cnt.builder        = builder;
  cnt.assembly       = assembly;
  cnt.arenas         = &builder->mir_arenas;
  cnt.verbose        = builder->flags & BUILDER_VERBOSE;
  cnt.functions      = bo_array_new(sizeof(MirFn *));
  cnt.needed_types   = bo_array_new(sizeof(Ast *));
  cnt.type_resolvers = bo_htbl_new(sizeof(MirFn *), 4096);
  cnt.llvm_cnt       = LLVMContextCreate();
  cnt.llvm_module    = LLVMModuleCreateWithNameInContext(assembly->name, cnt.llvm_cnt);
  cnt.llvm_td        = LLVMGetModuleDataLayout(cnt.llvm_module);

  /* INIT BUILDINS */
  uint64_t tmp;
  cnt.buildin_types.table = bo_htbl_new(sizeof(BuildinType), _BUILDIN_TYPE_COUNT);
  for (int i = 0; i < _BUILDIN_TYPE_COUNT; ++i) {
    tmp = bo_hash_from_str(buildin_type_names[i]);
    bo_htbl_insert(cnt.buildin_types.table, tmp, i);
  }

  cnt.buildin_types.entry_type = create_type_type(&cnt, NULL);
  cnt.buildin_types.entry_void = create_type_void(&cnt);
  cnt.buildin_types.entry_s32 =
      create_type_int(&cnt, buildin_type_names[BUILDIN_TYPE_S32], 32, true);

  BArray *args = bo_array_new(sizeof(MirType *));
  cnt.buildin_types.entry_resolve_type_fn =
      create_type_fn(&cnt, cnt.buildin_types.entry_type, args);
  /* INIT BUILDINS */

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    ast(&cnt, unit->ast);
  }

  create_type_resolvers(&cnt);
  analyze_type_resolvers(&cnt);

  if (cnt.verbose) {
    MirFn *tmp;

    barray_foreach(cnt.functions, tmp)
    {
      mir_printer_fn(tmp);
    }
  }

  bo_unref(cnt.buildin_types.table);
  bo_unref(cnt.functions);
  bo_unref(cnt.type_resolvers);
  bo_unref(cnt.needed_types);
  /* TODO: pass context to the next stages */
  LLVMContextDispose(cnt.llvm_cnt);
}
