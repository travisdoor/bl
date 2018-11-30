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
  MirInstrTypeFn       type_fn;
  MirInstrFnProto      fn_proto;
  MirInstrDeclRef      decl_ref;
  MirInstrCall         call;
  MirInstrUnreachable  unreachable;
  MirInstrAddrOf       addr_of;
  MirInstrTryInfer     try_infer;
  MirInstrCondBr       cond_br;
};

#define push_curr_dependent(_cnt, _dep)                                                            \
  MirInstr *const _prev_dep = (_cnt)->curr_dependent;                                              \
  (_cnt)->curr_dependent    = (_dep);

#define pop_curr_dependent(_cnt) (_cnt)->curr_dependent = _prev_dep;

typedef enum
{
  BUILDIN_TYPE_NONE = -1,

  BUILDIN_TYPE_S8,
  BUILDIN_TYPE_S16,
  BUILDIN_TYPE_S32,
  BUILDIN_TYPE_S64,
  BUILDIN_TYPE_U8,
  BUILDIN_TYPE_U16,
  BUILDIN_TYPE_U32,
  BUILDIN_TYPE_U64,
  BUILDIN_TYPE_USIZE,
  BUILDIN_TYPE_BOOL,

  _BUILDIN_TYPE_COUNT,
} BuildinType;

typedef enum
{
  PASS_GENERATE,
  PASS_ANALYZE
} Pass;

typedef struct
{
  MirValue * exec_call_result;
  Builder *  builder;
  Assembly * assembly;
  MirArenas *arenas;
  BList *    analyze_stack;
  MirInstr * curr_dependent;

  struct
  {
    BHashTable *table;

    MirType *entry_type;
    MirType *entry_s8;
    MirType *entry_s16;
    MirType *entry_s32;
    MirType *entry_s64;
    MirType *entry_u8;
    MirType *entry_u16;
    MirType *entry_u32;
    MirType *entry_u64;
    MirType *entry_usize;
    MirType *entry_bool;
    MirType *entry_void;
    MirType *entry_resolve_type_fn;
  } buildin_types;

  struct
  {
    MirBlock *block;
  } cursor;

  LLVMContextRef    llvm_cnt;
  LLVMModuleRef     llvm_module;
  LLVMTargetDataRef llvm_td;

  MirInstr *entry_fn;
  bool      verbose;
  Pass      pass;
} Context;

static const char *entry_fn_name                           = "main";
static const char *buildin_type_names[_BUILDIN_TYPE_COUNT] = {"s8",  "s16", "s32", "s64",   "u8",
                                                              "u16", "u32", "u64", "usize", "bool"};
static uint64_t    entry_fn_hash                           = 0;

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
instr_dtor(MirInstr *instr)
{
  bo_unref(instr->deps);
  switch (instr->kind) {
  case MIR_INSTR_TYPE_FN:
    bo_unref(((MirInstrTypeFn *)instr)->arg_types);
    break;
  default:
    break;
  }
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

/* FW decls */
static void
init_buildins(Context *cnt);

static void
execute_entry_fn(Context *cnt);

static bool
type_cmp(MirType *first, MirType *second);

static const char *
instr_name(MirInstr *instr);

/* ctors */
static MirType *
create_type_type(Context *cnt);

static MirType *
create_type_void(Context *cnt);

static MirType *
create_type_bool(Context *cnt);

static MirType *
create_type_int(Context *cnt, const char *name, int bitcount, bool is_signed);

static MirType *
create_type_ptr(Context *cnt, MirType *src_type);

static MirType *
create_type_fn(Context *cnt, MirType *ret_type, BArray *arg_types);

static MirVar *
create_var(Context *cnt, Ast *name);

static MirExec *
create_exec(Context *cnt, MirFn *owner_fn);

static MirFn *
create_fn(Context *cnt);

static MirBlock *
append_block(Context *cnt, MirInstr *fn, const char *name);

/* instructions */
static void
push_into_curr_block(Context *cnt, MirInstr *instr);

#define create_instr(_cnt, _kind, _node, _t) ((_t)_create_instr((_cnt), (_kind), (_node)))

static MirInstr *
_create_instr(Context *cnt, MirInstrKind kind, Ast *node);

static MirInstr *
create_instr_call_type_resolve(Context *cnt, MirInstr *resolver_fn, Ast *type);

static MirInstr *
create_instr_fn_proto(Context *cnt, MirInstr *type, MirInstr *user_type, Ast *name);

static MirInstr *
add_instr_addr_of(Context *cnt, Ast *node, MirInstr *target);

static MirInstr *
add_instr_cond_br(Context *cnt, Ast *node, MirInstr *cond, MirBlock *then_block,
                  MirBlock *else_block);

static MirInstr *
add_instr_load(Context *cnt, Ast *node, MirInstr *src);

static MirInstr *
add_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, BArray *arg_types);

static MirInstr *
add_instr_try_infer(Context *cnt, Ast *node, MirInstr *expr, MirInstr *decl);

static MirInstr *
add_instr_fn_proto(Context *cnt, MirInstr *type, MirInstr *user_type, Ast *name);

static MirInstr *
add_instr_decl_ref(Context *cnt, Ast *node);

static MirInstr *
add_instr_call(Context *cnt, Ast *node, MirInstr *callee, BArray *args);

static MirInstr *
add_instr_decl_var(Context *cnt, MirInstr *type, Ast *name);

static MirInstr *
add_instr_const_int(Context *cnt, Ast *node, uint64_t val);

static MirInstr *
add_instr_const_bool(Context *cnt, Ast *node, bool val);

static MirInstr *
add_instr_const_type(Context *cnt, Ast *node, MirType *type);

static MirInstr *
add_instr_ret(Context *cnt, Ast *node, MirInstr *value);

static MirInstr *
add_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest);

static MirInstr *
add_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op);

static MirInstr *
add_instr_validate_type(Context *cnt, MirInstr *src);

/* ast */
static MirInstr *
ast_create_type_resolver_call(Context *cnt, Ast *type);

static MirInstr *
ast(Context *cnt, Ast *node);

static void
ast_ublock(Context *cnt, Ast *ublock);

static void
ast_stmt_if(Context *cnt, Ast *stmt_if);

static void
ast_block(Context *cnt, Ast *block);

static void
ast_stmt_return(Context *cnt, Ast *ret);

static MirInstr *
ast_decl_entity(Context *cnt, Ast *entity);

static MirInstr *
ast_type_ref(Context *cnt, Ast *type_ref);

static MirInstr *
ast_type_fn(Context *cnt, Ast *type_fn);

static MirInstr *
ast_expr_ref(Context *cnt, Ast *ref);

static MirInstr *
ast_expr_lit_int(Context *cnt, Ast *expr);

static MirInstr *
ast_expr_lit_bool(Context *cnt, Ast *expr);

static MirInstr *
ast_expr_lit_fn(Context *cnt, Ast *lit_fn);

static MirInstr *
ast_expr_binop(Context *cnt, Ast *binop);

static LLVMTypeRef
to_llvm_type(Context *cnt, MirType *type, size_t *out_size);

/* analyze */
static bool
analyze_is_satisfied(Context *cnt, MirInstr *instr, bool strict_only);

static bool
analyze_instr(Context *cnt, MirInstr *instr);

static bool
analyze_instr_ret(Context *cnt, MirInstrRet *ret);

static bool
analyze_instr_load(Context *cnt, MirInstrLoad *load);

static bool
analyze_instr_store(Context *cnt, MirInstrStore *store);

static bool
analyze_instr_addr_of(Context *cnt, MirInstrAddrOf *addrof);

static bool
analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);

static bool
analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn);

static bool
analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *var);

static bool
analyze_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static bool
analyze_instr_const(Context *cnt, MirInstrConst *cnst);

static bool
analyze_instr_validate_type(Context *cnt, MirInstrValidateType *validate);

static bool
analyze_instr_try_infer(Context *cnt, MirInstrTryInfer *infer);

static bool
analyze_instr_call(Context *cnt, MirInstrCall *call);

static bool
analyze_instr_binop(Context *cnt, MirInstrBinop *binop);

static void
analyze(Context *cnt);

/* execute */
static MirValue *
exec_instr(Context *cnt, MirInstr *instr);

static MirValue *
exec_instr_const(Context *cnt, MirInstrConst *cnst);

static MirValue *
exec_instr_load(Context *cnt, MirInstrLoad *load);

static MirValue *
exec_instr_addr_of(Context *cnt, MirInstrAddrOf *addrof);

static MirValue *
exec_instr_store(Context *cnt, MirInstrStore *store);

static MirValue *
exec_instr_binop(Context *cnt, MirInstrBinop *binop);

static MirValue *
exec_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static MirValue *
exec_instr_call(Context *cnt, MirInstrCall *call);

static MirValue *
exec_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);

static MirValue *
exec_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn);

static MirValue *
exec_instr_ret(Context *cnt, MirInstrRet *ret);

static MirValue *
exec_instr_decl_var(Context *cnt, MirInstrDeclVar *var);

static inline bool
is_pointer_type(MirType *type)
{
  assert(type);
  return type->kind == MIR_TYPE_PTR;
}

static inline bool
is_entry_fn(Ast *ident)
{
  if (!ident) return false;
  assert(ident->kind == AST_IDENT);
  return ident->data.ident.hash == entry_fn_hash;
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
  case BUILDIN_TYPE_S8:
    return cnt->buildin_types.entry_s8;
  case BUILDIN_TYPE_S16:
    return cnt->buildin_types.entry_s16;
  case BUILDIN_TYPE_S32:
    return cnt->buildin_types.entry_s32;
  case BUILDIN_TYPE_S64:
    return cnt->buildin_types.entry_s64;
  case BUILDIN_TYPE_U8:
    return cnt->buildin_types.entry_u8;
  case BUILDIN_TYPE_U16:
    return cnt->buildin_types.entry_u16;
  case BUILDIN_TYPE_U32:
    return cnt->buildin_types.entry_u32;
  case BUILDIN_TYPE_U64:
    return cnt->buildin_types.entry_u64;
  case BUILDIN_TYPE_USIZE:
    return cnt->buildin_types.entry_usize;
  case BUILDIN_TYPE_BOOL:
    return cnt->buildin_types.entry_bool;
  default:
    bl_abort("invalid buildin type");
  }
}

static inline void
set_cursor_block(Context *cnt, MirBlock *block)
{
  if (!block) return;
  cnt->cursor.block = block;
}

static inline MirBlock *
get_current_block(Context *cnt)
{
  return cnt->cursor.block;
}

static inline MirFn *
get_current_fn(Context *cnt)
{
  return cnt->cursor.block ? cnt->cursor.block->owner_exec->owner_fn : NULL;
}

static inline void
error_no_impl_cast(Context *cnt, MirType *from, MirType *to, Ast *loc)
{
  assert(from && to);

  char tmp_from[256];
  char tmp_to[256];
  mir_type_to_str(tmp_from, 256, from);
  mir_type_to_str(tmp_to, 256, to);

  builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, loc->src, BUILDER_CUR_WORD,
              "no implicit cast for type '%s' and '%s'", tmp_from, tmp_to);
}

static inline void
add_dep_uq(MirInstr *dependent, MirInstr *dependency, MirDepKind kind)
{
  assert(dependent && dependency);
  const uint64_t hash = (uint64_t)dependency;

  if (!dependent->deps) {
    dependent->deps = bo_htbl_new(sizeof(MirDep), 128);
  } else {
    if (bo_htbl_has_key(dependent->deps, hash)) {
      MirDep *dep = &bo_htbl_at(dependent->deps, hash, MirDep);
      if (dep->kind == MIR_DEP_LAX) dep->kind = kind;
      return;
    }
  }

  MirDep dep = {.kind = kind, .dep = dependency};
  bo_htbl_insert(dependent->deps, hash, dep);
}

static inline MirInstr *
add_instr_load_if_needed(Context *cnt, MirInstr *src)
{
  if (!src) return src;
  switch (src->kind) {
  case MIR_INSTR_CONST:
  case MIR_INSTR_BINOP:
    return src;
  default:
    break;
  }

  return add_instr_load(cnt, NULL, src);
}

static inline void
print_deps(MirInstr *instr)
{
  if (!instr->deps) return;
  fprintf(stdout, "Analyze dependencies: \n");

  bo_iterator_t it;
  MirDep *      dep;
  bhtbl_foreach(instr->deps, it)
  {
    dep = &bo_htbl_iter_peek_value(instr->deps, &it, MirDep);
    fprintf(stdout, "  %%%u: %s\n", dep->dep->id, dep->kind == MIR_DEP_STRICT ? "STRICT" : "LAX");
  }
}

/* impl */
MirType *
create_type_type(Context *cnt)
{
  MirType *tmp   = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind      = MIR_TYPE_TYPE;
  tmp->name      = "type";
  tmp->llvm_type = to_llvm_type(cnt, tmp, &tmp->size);
  return tmp;
}

MirType *
create_type_void(Context *cnt)
{
  MirType *tmp   = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind      = MIR_TYPE_VOID;
  tmp->name      = "void";
  tmp->llvm_type = to_llvm_type(cnt, tmp, &tmp->size);
  return tmp;
}

MirType *
create_type_bool(Context *cnt)
{
  MirType *tmp   = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind      = MIR_TYPE_BOOL;
  tmp->name      = "bool";
  tmp->llvm_type = to_llvm_type(cnt, tmp, &tmp->size);
  return tmp;
}

MirType *
create_type_int(Context *cnt, const char *name, int bitcount, bool is_signed)
{
  assert(bitcount > 0);
  MirType *tmp                = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind                   = MIR_TYPE_INT;
  tmp->name                   = name;
  tmp->data.integer.bitcount  = bitcount;
  tmp->data.integer.is_signed = is_signed;
  tmp->llvm_type              = to_llvm_type(cnt, tmp, &tmp->size);

  return tmp;
}

MirType *
create_type_ptr(Context *cnt, MirType *src_type)
{
  MirType *tmp       = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind          = MIR_TYPE_PTR;
  tmp->data.ptr.next = src_type;
  tmp->llvm_type     = to_llvm_type(cnt, tmp, &tmp->size);

  return tmp;
}

MirType *
create_type_fn(Context *cnt, MirType *ret_type, BArray *arg_types)
{
  MirType *tmp           = arena_alloc(&cnt->arenas->type_arena);
  tmp->kind              = MIR_TYPE_FN;
  tmp->data.fn.arg_types = arg_types;
  tmp->data.fn.ret_type  = ret_type ? ret_type : cnt->buildin_types.entry_void;
  tmp->llvm_type         = to_llvm_type(cnt, tmp, &tmp->size);

  return tmp;
}

MirVar *
create_var(Context *cnt, Ast *name)
{
  assert(name);
  MirVar *tmp = arena_alloc(&cnt->arenas->var_arena);
  tmp->name   = name;
  return tmp;
}

MirExec *
create_exec(Context *cnt, MirFn *owner_fn)
{
  MirExec *tmp  = arena_alloc(&cnt->arenas->exec_arena);
  tmp->blocks   = bo_array_new(sizeof(MirBlock *));
  tmp->owner_fn = owner_fn;
  return tmp;
}

MirFn *
create_fn(Context *cnt)
{
  MirFn *tmp         = arena_alloc(&cnt->arenas->fn_arena);
  tmp->exec          = create_exec(cnt, tmp);
  tmp->exec_analyzed = create_exec(cnt, tmp);
  return tmp;
}

MirBlock *
append_block(Context *cnt, MirInstr *fn, const char *name)
{
  assert(fn && name);
  MirBlock *tmp     = arena_alloc(&cnt->arenas->block_arena);
  tmp->name         = name;
  tmp->instructions = bo_array_new(sizeof(MirInstr *));

  assert(fn->kind == MIR_INSTR_FN_PROTO);
  MirFn *v_fn = fn->value.data.v_fn;
  if (!v_fn) fn->value.data.v_fn = v_fn = create_fn(cnt);

  tmp->owner_exec = cnt->pass == PASS_GENERATE ? v_fn->exec : v_fn->exec_analyzed;
  if (!tmp->owner_exec->entry_block) tmp->owner_exec->entry_block = tmp;
  tmp->id = bo_array_size(tmp->owner_exec->blocks);
  bo_array_push_back(tmp->owner_exec->blocks, tmp);
  return tmp;
}

/* instructions */
void
push_into_curr_block(Context *cnt, MirInstr *instr)
{
  assert(instr);
  assert(cnt->cursor.block);

  instr->id          = bo_array_size(cnt->cursor.block->instructions);
  instr->owner_block = cnt->cursor.block;

  bo_array_push_back(cnt->cursor.block->instructions, instr);
}

#define create_instr(_cnt, _kind, _node, _t) ((_t)_create_instr((_cnt), (_kind), (_node)))

MirInstr *
_create_instr(Context *cnt, MirInstrKind kind, Ast *node)
{
  MirInstr *tmp = arena_alloc(&cnt->arenas->instr_arena);
  tmp->kind     = kind;
  tmp->node     = node;
  tmp->id       = 0;
  return tmp;
}

MirInstr *
create_instr_call_type_resolve(Context *cnt, MirInstr *resolver_fn, Ast *type)
{
  assert(resolver_fn && resolver_fn->kind == MIR_INSTR_FN_PROTO);
  MirInstrCall *tmp  = create_instr(cnt, MIR_INSTR_CALL, type, MirInstrCall *);
  tmp->base.id       = 0;
  tmp->base.comptime = true;
  tmp->callee        = resolver_fn;
  ++resolver_fn->ref_count;
  return &tmp->base;
}

MirInstr *
create_instr_fn_proto(Context *cnt, MirInstr *type, MirInstr *user_type, Ast *name)
{
  MirInstrFnProto *tmp = create_instr(cnt, MIR_INSTR_FN_PROTO, name, MirInstrFnProto *);
  tmp->base.id         = bo_list_size(cnt->analyze_stack);
  tmp->type            = type;
  tmp->user_type       = user_type;

  return &tmp->base;
}

MirInstr *
add_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, BArray *arg_types)
{
  MirInstrTypeFn *tmp  = create_instr(cnt, MIR_INSTR_TYPE_FN, node, MirInstrTypeFn *);
  tmp->base.value.type = cnt->buildin_types.entry_type;
  tmp->base.comptime   = true;
  tmp->ret_type        = ret_type;
  tmp->arg_types       = arg_types;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_cond_br(Context *cnt, Ast *node, MirInstr *cond, MirBlock *then_block,
                  MirBlock *else_block)
{
  assert(cond && then_block && else_block);
  ++cond->ref_count;
  MirInstrCondBr *tmp = create_instr(cnt, MIR_INSTR_COND_BR, node, MirInstrCondBr *);
  tmp->cond           = cond;
  tmp->then_block     = then_block;
  tmp->else_block     = else_block;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_load(Context *cnt, Ast *node, MirInstr *src)
{
  ++src->ref_count;
  MirInstrLoad *tmp = create_instr(cnt, MIR_INSTR_LOAD, node, MirInstrLoad *);
  tmp->src          = src;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_addr_of(Context *cnt, Ast *node, MirInstr *target)
{
  ++target->ref_count;
  MirInstrAddrOf *tmp = create_instr(cnt, MIR_INSTR_ADDR_OF, node, MirInstrAddrOf *);
  tmp->target         = target;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_fn_proto(Context *cnt, MirInstr *type, MirInstr *user_type, Ast *name)
{
  MirInstr *tmp = create_instr_fn_proto(cnt, type, user_type, name);
  bo_list_push_back(cnt->analyze_stack, tmp);
  return tmp;
}

MirInstr *
add_instr_decl_ref(Context *cnt, Ast *node)
{
  MirInstrDeclRef *tmp = create_instr(cnt, MIR_INSTR_DECL_REF, node, MirInstrDeclRef *);
  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_call(Context *cnt, Ast *node, MirInstr *callee, BArray *args)
{
  assert(callee && callee->kind == MIR_INSTR_FN_PROTO);
  MirInstrCall *tmp = create_instr(cnt, MIR_INSTR_CALL, node, MirInstrCall *);
  tmp->args         = args;
  tmp->callee       = callee;
  ++callee->ref_count;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_decl_var(Context *cnt, MirInstr *type, Ast *name)
{
  if (type) ++type->ref_count;
  MirInstrDeclVar *tmp = create_instr(cnt, MIR_INSTR_DECL_VAR, name, MirInstrDeclVar *);
  tmp->type            = type;

  MirVar *var = create_var(cnt, name);
  tmp->var    = var;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_const_int(Context *cnt, Ast *node, uint64_t val)
{
  MirInstr *tmp         = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime         = true;
  tmp->value.type       = cnt->buildin_types.entry_s32;
  tmp->value.data.v_int = val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
add_instr_const_bool(Context *cnt, Ast *node, bool val)
{
  MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime          = true;
  tmp->value.type        = cnt->buildin_types.entry_bool;
  tmp->value.data.v_bool = val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
add_instr_const_type(Context *cnt, Ast *node, MirType *type)
{
  MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime          = true;
  tmp->value.type        = cnt->buildin_types.entry_type;
  tmp->value.data.v_type = type;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
add_instr_ret(Context *cnt, Ast *node, MirInstr *value)
{
  if (value) {
    ++value->ref_count;
  }

  MirInstrRet *tmp     = create_instr(cnt, MIR_INSTR_RET, node, MirInstrRet *);
  tmp->value           = value;
  tmp->base.value.type = cnt->buildin_types.entry_void;

  /* add terminate current block and for return statement terminate also executable unit */
  assert(cnt->cursor.block);
  assert(cnt->cursor.block->owner_exec);

  /* when current block is already terminated we produce unrecheable code replacement later during
   * analyze */
  if (!cnt->cursor.block->terminal) cnt->cursor.block->terminal = &tmp->base;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest)
{
  assert(src && dest);
  ++src->ref_count;
  ++dest->ref_count;
  MirInstrStore *tmp = create_instr(cnt, MIR_INSTR_STORE, node, MirInstrStore *);
  tmp->src           = src;
  tmp->dest          = dest;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_try_infer(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest)
{
  assert(src && dest);
  ++src->ref_count;
  ++dest->ref_count;
  MirInstrTryInfer *tmp = create_instr(cnt, MIR_INSTR_TRY_INFER, node, MirInstrTryInfer *);
  tmp->src              = src;
  tmp->dest             = dest;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op)
{
  assert(lhs && rhs);
  ++lhs->ref_count;
  ++rhs->ref_count;
  MirInstrBinop *tmp = create_instr(cnt, MIR_INSTR_BINOP, node, MirInstrBinop *);
  tmp->lhs           = lhs;
  tmp->rhs           = rhs;
  tmp->op            = op;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
add_instr_validate_type(Context *cnt, MirInstr *src)
{
  assert(src);
  ++src->ref_count;
  MirInstrValidateType *tmp =
      create_instr(cnt, MIR_INSTR_VALIDATE_TYPE, NULL, MirInstrValidateType *);
  tmp->src             = src;
  tmp->base.value.type = cnt->buildin_types.entry_void;

  push_into_curr_block(cnt, &tmp->base);
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

  case MIR_TYPE_BOOL: {
    result = LLVMIntTypeInContext(cnt->llvm_cnt, 1);
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
    MirType *    tmp_ret   = type->data.fn.ret_type;
    BArray *     tmp_args  = type->data.fn.arg_types;
    const size_t cargs     = tmp_args ? bo_array_size(tmp_args) : 0;
    LLVMTypeRef *llvm_args = NULL;
    LLVMTypeRef  llvm_ret  = NULL;

    if (tmp_args) {
      llvm_args = bl_malloc(cargs * sizeof(LLVMTypeRef));
      if (!llvm_args) bl_abort("bad alloc");

      MirType *tmp_arg;
      barray_foreach(tmp_args, tmp_arg)
      {
        assert(tmp_arg->llvm_type);
        llvm_args[i] = tmp_arg->llvm_type;
      }
    }

    llvm_ret = tmp_ret ? tmp_ret->llvm_type : LLVMVoidTypeInContext(cnt->llvm_cnt);
    assert(llvm_ret);

    result = LLVMFunctionType(llvm_ret, llvm_args, cargs, false);
    if (out_size) *out_size = 0;
    bl_free(llvm_args);
    break;
  }

  default:
    bl_abort("unimplemented");
  }

  return result;
}

bool
type_cmp(MirType *first, MirType *second)
{
  assert(first && second);
  if (first->kind != second->kind) return false;

  switch (first->kind) {
  case MIR_TYPE_INT: {
    return first->data.integer.bitcount == second->data.integer.bitcount &&
           first->data.integer.is_signed == second->data.integer.is_signed;
  }

  case MIR_TYPE_FN: {
    if (!type_cmp(first->data.fn.ret_type, second->data.fn.ret_type)) return false;
    BArray *     fargs = first->data.fn.arg_types;
    BArray *     sargs = second->data.fn.arg_types;
    const size_t fargc = fargs ? bo_array_size(fargs) : 0;
    const size_t sargc = sargs ? bo_array_size(sargs) : 0;

    if (fargc != sargc) return false;

    MirType *ftmp, *stmp;
    if (fargc) {
      barray_foreach(fargs, ftmp)
      {
        stmp = bo_array_at(sargs, i, MirType *);
        assert(stmp && ftmp);
        if (!type_cmp(ftmp, stmp)) return false;
      }
    }

    return true;
  }

  case MIR_TYPE_VOID:
  case MIR_TYPE_TYPE:
  case MIR_TYPE_BOOL:
    return true;

  default:
    break;
  }

#if BL_DEBUG
  char tmp_first[256];
  char tmp_second[256];
  mir_type_to_str(tmp_first, 256, first);
  mir_type_to_str(tmp_second, 256, second);
  msg_warning("missing type comparation for types %s and %s!!!", tmp_first, tmp_second);
#endif

  return false;
}

/* analyze */
bool
analyze_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
  Ast *ast_ident = ref->base.node;
  assert(ref->base.node && ref->base.node->kind == AST_IDENT);

  Scope *scope = ast_ident->data.ident.scope;
  assert(scope);
  ScopeEntry *scope_entry = scope_lookup(scope, ast_ident->data.ident.hash, true);
  if (!scope_entry) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNKNOWN_SYMBOL, ast_ident->src,
                BUILDER_CUR_WORD, "unknown symbol");
    return false;
  }

  assert(scope_entry->instr);
  assert(scope_entry->instr->analyzed);
  scope_entry->instr->ref_count++;

  assert(scope_entry->instr->value.type);
  ref->base.value = scope_entry->instr->value;

  /* create pointer to referenced data */
  assert(scope_entry->instr->value.type);
  ref->base.value.type       = create_type_ptr(cnt, scope_entry->instr->value.type);
  ref->base.value.data.v_ptr = &scope_entry->instr->value;

  if (ref->base.ref_count) push_into_curr_block(cnt, &ref->base);
  return true;
}

bool
analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
  MirBlock *prev_block = NULL;

  /* resolve type */
  if (!fn_proto->base.value.type) {
    assert(fn_proto->type && fn_proto->type->kind == MIR_INSTR_CALL);
    analyze_instr(cnt, fn_proto->type);
    MirValue *type_val = exec_instr(cnt, fn_proto->type);
    assert(type_val->type && type_val->type->kind == MIR_TYPE_TYPE);

    if (fn_proto->user_type) {
      assert(fn_proto->user_type->kind == MIR_INSTR_CALL);
      analyze_instr(cnt, fn_proto->user_type);
      MirValue *user_type_val = exec_instr(cnt, fn_proto->user_type);
      assert(user_type_val->type && user_type_val->type->kind == MIR_TYPE_TYPE);

      if (!type_cmp(type_val->data.v_type, user_type_val->data.v_type)) {
        error_no_impl_cast(cnt, type_val->data.v_type, user_type_val->data.v_type,
                           fn_proto->user_type->node);
      }
    }

    fn_proto->base.value.type = type_val->data.v_type;
  }

  assert(fn_proto->base.value.type && "function has no valid type");
  assert(fn_proto->base.value.data.v_fn);
  fn_proto->base.value.data.v_fn->type = fn_proto->base.value.type;

  prev_block            = get_current_block(cnt);
  MirBlock *entry_block = append_block(cnt, &fn_proto->base, "entry");
  set_cursor_block(cnt, entry_block);

  MirExec *exec = fn_proto->base.value.data.v_fn->exec;
  assert(exec);

  if (cnt->verbose) {
    print_deps(&fn_proto->base);
    mir_print_instr(&fn_proto->base, false);
  }
  /* iterate over entry block of executable */
  MirInstr *tmp;
  barray_foreach(exec->entry_block->instructions, tmp)
  {
    if (!analyze_instr(cnt, tmp)) {
      break;
    }
  }

  if (cnt->verbose) {
    mir_print_instr(&fn_proto->base, true);
    fprintf(stdout,
            "--------------------------------------------------------------------------------\n");
  }

  assert(prev_block);
  set_cursor_block(cnt, prev_block);

  return true;
}

bool
analyze_instr_load(Context *cnt, MirInstrLoad *load)
{
  MirInstr *src = load->src;
  assert(src);
  assert(is_pointer_type(src->value.type) && "expected pointer");

  MirValue *deref       = src->value.data.v_ptr;
  load->base.value.type = deref->type;

  push_into_curr_block(cnt, &load->base);
  return true;
}

bool
analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn)
{
  assert(type_fn->base.value.type);

  /* TODO analyze args */
  if (type_fn->arg_types) bl_abort("unimplemented");

  if (type_fn->ret_type) {
    assert(type_fn->ret_type->analyzed);
  }

  if (type_fn->base.ref_count) push_into_curr_block(cnt, &type_fn->base);
  return true;
}

bool
analyze_instr_binop(Context *cnt, MirInstrBinop *binop)
{
  MirInstr *lhs = binop->lhs;
  MirInstr *rhs = binop->rhs;
  assert(lhs && rhs);
  assert(lhs->analyzed);
  assert(rhs->analyzed);

  if (!type_cmp(lhs->value.type, rhs->value.type)) {
    error_no_impl_cast(cnt, lhs->value.type, rhs->value.type, binop->base.node);
  }

  MirType *type = lhs->value.type;
  assert(type);
  binop->base.value.type = type;

  if (binop->base.ref_count) push_into_curr_block(cnt, &binop->base);
  return true;
}

bool
analyze_instr_addr_of(Context *cnt, MirInstrAddrOf *addrof)
{
  assert(addrof->target);
  assert(addrof->target->analyzed);

  MirType *type = addrof->target->value.type;
  assert(type);

  addrof->base.value.type       = create_type_ptr(cnt, type);
  addrof->base.value.data.v_ptr = &addrof->target->value;
  if (addrof->base.ref_count) push_into_curr_block(cnt, &addrof->base);
  return true;
}

bool
analyze_instr_const(Context *cnt, MirInstrConst *cnst)
{
  assert(cnst->base.value.type);
  if (cnst->base.ref_count) push_into_curr_block(cnt, &cnst->base);
  return true;
}

bool
analyze_instr_validate_type(Context *cnt, MirInstrValidateType *validate)
{
  MirInstr *src = validate->src;
  assert(src);

  if (!type_cmp(src->value.type, cnt->buildin_types.entry_type)) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, src->node->src, BUILDER_CUR_WORD,
                "expected type");
  }

  assert(src->value.data.v_type);
  src->ref_count--;
  return true;
}

bool
analyze_instr_try_infer(Context *cnt, MirInstrTryInfer *infer)
{
  MirInstr *src  = infer->src;
  MirInstr *dest = infer->dest;
  assert(src && dest);
  assert(src->analyzed && dest->analyzed);

  src->ref_count--;
  dest->ref_count--;

  if (dest->value.type) return true;

  dest->value.type = src->value.type;
  assert(dest->value.type);
  return true;
}

bool
analyze_instr_ret(Context *cnt, MirInstrRet *ret)
{
  /* compare return value with current function type */
  MirBlock *block = get_current_block(cnt);
  assert(block);
  if (!block->terminal) block->terminal = &ret->base;

  MirInstr *value = ret->value;
  if (value) {
    assert(value->analyzed);
  }

  MirType *fn_type = get_current_fn(cnt)->type;
  assert(fn_type);
  assert(fn_type->kind == MIR_TYPE_FN);

  const bool expected_ret_value =
      !type_cmp(fn_type->data.fn.ret_type, cnt->buildin_types.entry_void);

  if (value) {
    if (!expected_ret_value) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_EXPR, ret->value->node->src,
                  BUILDER_CUR_WORD, "unexpected return value");
    } else if (!type_cmp(value->value.type, fn_type->data.fn.ret_type)) {
      error_no_impl_cast(cnt, value->value.type, fn_type->data.fn.ret_type, ret->value->node);
    }
  } else if (expected_ret_value) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_EXPR, ret->base.node->src,
                BUILDER_CUR_AFTER, "expected return value");
  }

  push_into_curr_block(cnt, &ret->base);
  return true;
}

bool
analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *var)
{
  if (var->type) {
    analyze_instr(cnt, var->type);
    MirValue *resolved_type_value = exec_instr(cnt, var->type);
    assert(resolved_type_value && resolved_type_value->type->kind == MIR_TYPE_TYPE);

    var->base.value.type = resolved_type_value->data.v_type;
  }

  if (var->base.ref_count == 0) {
    builder_msg(cnt->builder, BUILDER_MSG_WARNING, 0, var->base.node->src, BUILDER_CUR_WORD,
                "unused declaration");
  }

  if (var->base.ref_count) push_into_curr_block(cnt, &var->base);
  return true;
}

bool
analyze_instr_call(Context *cnt, MirInstrCall *call)
{
  /* TODO */
  return true;
}

bool
analyze_instr_store(Context *cnt, MirInstrStore *store)
{
  MirInstr *src  = store->src;
  MirInstr *dest = store->dest;
  assert(src && dest);
  assert(src->analyzed && dest->analyzed);

  assert(is_pointer_type(dest->value.type) && "store expect destination to be a pointer");

  MirValue *deref_dest = dest->value.data.v_ptr;
  assert(deref_dest && "reference does not point to any declaration");

  if (!type_cmp(src->value.type, deref_dest->type)) {
    error_no_impl_cast(cnt, src->value.type, deref_dest->type, src->node);
  }

  /* store implicitly yields void value */
  store->base.value.type = cnt->buildin_types.entry_void;

  push_into_curr_block(cnt, &store->base);
  return true;
}

bool
analyze_instr(Context *cnt, MirInstr *instr)
{
  if (!instr) return NULL;

  /* skip already analyzed instructions */
  if (instr->analyzed) return instr;
  bool state = false;

  switch (instr->kind) {
  case MIR_INSTR_FN_PROTO:
    state = analyze_instr_fn_proto(cnt, (MirInstrFnProto *)instr);
    break;
  case MIR_INSTR_DECL_VAR:
    state = analyze_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
    break;
  case MIR_INSTR_CALL:
    state = analyze_instr_call(cnt, (MirInstrCall *)instr);
    break;
  case MIR_INSTR_CONST:
    state = analyze_instr_const(cnt, (MirInstrConst *)instr);
    break;
  case MIR_INSTR_RET:
    state = analyze_instr_ret(cnt, (MirInstrRet *)instr);
    break;
  case MIR_INSTR_STORE:
    state = analyze_instr_store(cnt, (MirInstrStore *)instr);
    break;
  case MIR_INSTR_DECL_REF:
    state = analyze_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
    break;
  case MIR_INSTR_BINOP:
    state = analyze_instr_binop(cnt, (MirInstrBinop *)instr);
    break;
  case MIR_INSTR_TYPE_FN:
    state = analyze_instr_type_fn(cnt, (MirInstrTypeFn *)instr);
    break;
  case MIR_INSTR_ADDR_OF:
    state = analyze_instr_addr_of(cnt, (MirInstrAddrOf *)instr);
    break;
  case MIR_INSTR_VALIDATE_TYPE:
    state = analyze_instr_validate_type(cnt, (MirInstrValidateType *)instr);
    break;
  case MIR_INSTR_TRY_INFER:
    state = analyze_instr_try_infer(cnt, (MirInstrTryInfer *)instr);
    break;
  case MIR_INSTR_LOAD:
    state = analyze_instr_load(cnt, (MirInstrLoad *)instr);
    break;
  default:
    msg_warning("missing analyze for %s", instr_name(instr));
    return false;
  }

  instr->analyzed = true;
  return state;
}

bool
analyze_is_satisfied(Context *cnt, MirInstr *instr, bool strict_only)
{
  assert(instr);
  BHashTable *deps = instr->deps;
  if (!deps) return true;

  bo_iterator_t iter;
  MirDep *      dep;
  bhtbl_foreach(deps, iter)
  {
    dep = &bo_htbl_iter_peek_value(deps, &iter, MirDep);

    // PERFORMANCE: is there some better solution than check whole tree???
    const bool check_tree = (bool)(strict_only ? dep->kind & MIR_DEP_STRICT : true);
    if (check_tree) {
      if (!dep->dep->analyzed) {
        return false;
      } else if (!analyze_is_satisfied(cnt, dep->dep, false)) {
        return false;
      }
    }
  }

  return true;
}

void
analyze(Context *cnt)
{
  BList *   stack = cnt->analyze_stack;
  MirInstr *instr;

  while (!bo_list_empty(stack)) {
    instr = bo_list_front(stack, MirInstr *);
    bo_list_pop_front(stack);
    assert(instr);

    if (analyze_is_satisfied(cnt, instr, true)) {
      analyze_instr(cnt, instr);
    } else {
      bo_list_push_back(stack, instr);
    }
  }
}

/* executing */
MirValue *
exec_instr(Context *cnt, MirInstr *instr)
{
  if (!instr) return NULL;
  assert(instr->analyzed);

  switch (instr->kind) {
  case MIR_INSTR_CONST:
    return exec_instr_const(cnt, (MirInstrConst *)instr);
  case MIR_INSTR_BINOP:
    return exec_instr_binop(cnt, (MirInstrBinop *)instr);
  case MIR_INSTR_CALL:
    return exec_instr_call(cnt, (MirInstrCall *)instr);
  case MIR_INSTR_RET:
    return exec_instr_ret(cnt, (MirInstrRet *)instr);
  case MIR_INSTR_TYPE_FN:
    return exec_instr_type_fn(cnt, (MirInstrTypeFn *)instr);
  case MIR_INSTR_FN_PROTO:
    return exec_instr_fn_proto(cnt, (MirInstrFnProto *)instr);
  case MIR_INSTR_DECL_VAR:
    return exec_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
  case MIR_INSTR_STORE:
    return exec_instr_store(cnt, (MirInstrStore *)instr);
  case MIR_INSTR_DECL_REF:
    return exec_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
  case MIR_INSTR_ADDR_OF:
    return exec_instr_addr_of(cnt, (MirInstrAddrOf *)instr);
  case MIR_INSTR_LOAD:
    return exec_instr_load(cnt, (MirInstrLoad *)instr);

  default:
    bl_abort("missing execution for instruction: %s", instr_name(instr));
  }

  return NULL;
}

MirValue *
exec_instr_addr_of(Context *cnt, MirInstrAddrOf *addrof)
{
  assert(is_pointer_type(addrof->base.value.type));
  return &addrof->base.value;
}

MirValue *
exec_instr_decl_var(Context *cnt, MirInstrDeclVar *var)
{
  assert(var->base.value.type);
  return &var->base.value;
}

MirValue *
exec_instr_load(Context *cnt, MirInstrLoad *load)
{
  assert(load->src);
  assert(is_pointer_type(load->src->value.type));
  load->base.value = *load->src->value.data.v_ptr;
  return &load->base.value;
}

MirValue *
exec_instr_store(Context *cnt, MirInstrStore *store)
{
  MirInstr *dest = store->dest;
  MirInstr *src  = store->src;
  assert(dest && src);
  assert(is_pointer_type(dest->value.type));

  MirValue *deref_dest = dest->value.data.v_ptr;
  assert(deref_dest);

  /* copy value do destination */
  *deref_dest = src->value;
  return &store->base.value;
}

MirValue *
exec_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
  assert(ref->base.value.type);
  return &ref->base.value;
}

MirValue *
exec_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn)
{
  MirType *ret_type = NULL;
  if (type_fn->ret_type) {
    ret_type = type_fn->ret_type->value.data.v_type;
    assert(ret_type);
  }

  BArray *arg_types = NULL;
  if (type_fn->arg_types) {
    bl_abort("unimplemented");
  }

  type_fn->base.value.data.v_type = create_type_fn(cnt, ret_type, arg_types);
  return &type_fn->base.value;
}

MirValue *
exec_instr_const(Context *cnt, MirInstrConst *cnst)
{
  assert(cnst->base.value.type);
  return &cnst->base.value;
}

MirValue *
exec_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
  MirFn *fn = fn_proto->base.value.data.v_fn;
  assert(fn && "currently external functions cannot be executed in compile time");
  MirExec *exec = fn->exec_analyzed;
  assert(exec);

  cnt->exec_call_result = NULL;
  /* iterate over entry block of executable */
  MirInstr *tmp;
  barray_foreach(exec->entry_block->instructions, tmp)
  {
    exec_instr(cnt, tmp);
  }

  return cnt->exec_call_result;
}

MirValue *
exec_instr_call(Context *cnt, MirInstrCall *call)
{
  assert(call->base.comptime);
  assert(call->callee && call->callee->kind == MIR_INSTR_FN_PROTO);
  MirValue *result = exec_instr_fn_proto(cnt, (MirInstrFnProto *)call->callee);
  assert(result);

  /* copy function execution return value into call instruction */
  call->base.value = *result;
  return &call->base.value;
}

MirValue *
exec_instr_ret(Context *cnt, MirInstrRet *ret)
{
  cnt->exec_call_result = &ret->value->value;
  return &ret->value->value;
}

static void
exec_math_add(Context *cnt, MirInstr *lhs, MirInstr *rhs, MirInstr *dest)
{
  MirValue *v_lhs  = &lhs->value;
  MirValue *v_rhs  = &rhs->value;
  MirValue *v_dest = &dest->value;

  assert(v_lhs && v_rhs && v_dest);
  v_dest->data.v_int = v_lhs->data.v_int + v_rhs->data.v_int;
}

static void
exec_math_sub(Context *cnt, MirInstr *lhs, MirInstr *rhs, MirInstr *dest)
{
  MirValue *v_lhs  = &lhs->value;
  MirValue *v_rhs  = &rhs->value;
  MirValue *v_dest = &dest->value;

  assert(v_lhs && v_rhs && v_dest);
  v_dest->data.v_int = v_lhs->data.v_int - v_rhs->data.v_int;
}

static void
exec_math_mul(Context *cnt, MirInstr *lhs, MirInstr *rhs, MirInstr *dest)
{
  MirValue *v_lhs  = &lhs->value;
  MirValue *v_rhs  = &rhs->value;
  MirValue *v_dest = &dest->value;

  assert(v_lhs && v_rhs && v_dest);
  v_dest->data.v_int = v_lhs->data.v_int * v_rhs->data.v_int;
}

static void
exec_math_div(Context *cnt, MirInstr *lhs, MirInstr *rhs, MirInstr *dest)
{
  MirValue *v_lhs  = &lhs->value;
  MirValue *v_rhs  = &rhs->value;
  MirValue *v_dest = &dest->value;

  assert(v_lhs && v_rhs && v_dest);
  if (v_rhs->data.v_int == 0) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_DIV_BY_ZERO, rhs->node->src, BUILDER_CUR_WORD,
                "division by zero");

    v_dest->data.v_int = 0;
    return;
  }

  v_dest->data.v_int = v_lhs->data.v_int / v_rhs->data.v_int;
}

MirValue *
exec_instr_binop(Context *cnt, MirInstrBinop *binop)
{
  assert(binop->base.value.type);
  switch (binop->op) {
  case BINOP_ADD:
    exec_math_add(cnt, binop->lhs, binop->rhs, &binop->base);
    break;
  case BINOP_SUB:
    exec_math_sub(cnt, binop->lhs, binop->rhs, &binop->base);
    break;
  case BINOP_MUL:
    exec_math_mul(cnt, binop->lhs, binop->rhs, &binop->base);
    break;
  case BINOP_DIV:
    exec_math_div(cnt, binop->lhs, binop->rhs, &binop->base);
    break;
  default:
    bl_abort("unimplemented");
  }

  return &binop->base.value;
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
ast_stmt_if(Context *cnt, Ast *stmt_if)
{
  bl_abort("unimplemented");
}

void
ast_stmt_return(Context *cnt, Ast *ret)
{
  MirInstr *value = ast(cnt, ret->data.stmt_return.expr);
  value           = add_instr_load_if_needed(cnt, value);
  add_instr_ret(cnt, ret, value);
}

MirInstr *
ast_expr_lit_int(Context *cnt, Ast *expr)
{
  return add_instr_const_int(cnt, expr, expr->data.expr_integer.val);
}

MirInstr *
ast_expr_lit_bool(Context *cnt, Ast *expr)
{
  return add_instr_const_bool(cnt, expr, expr->data.expr_boolean.val);
}

MirInstr *
ast_expr_ref(Context *cnt, Ast *ref)
{
  Ast *ident = ref->data.expr_ref.ident;
  assert(ident);
  return add_instr_decl_ref(cnt, ident);
}

MirInstr *
ast_expr_lit_fn(Context *cnt, Ast *lit_fn)
{
  /* creates function prototype */
  Ast *ast_block   = lit_fn->data.expr_fn.block;
  Ast *ast_fn_type = lit_fn->data.expr_fn.type;

  /* TODO: external function has no body!!! */
  assert(ast_block);
  MirInstrFnProto *fn_proto = (MirInstrFnProto *)add_instr_fn_proto(cnt, NULL, NULL, lit_fn);
  push_curr_dependent(cnt, &fn_proto->base);

  fn_proto->type = ast_create_type_resolver_call(cnt, ast_fn_type);
  assert(fn_proto->type);

  MirBlock *prev_block  = get_current_block(cnt);
  MirBlock *entry_block = append_block(cnt, &fn_proto->base, "entry");
  set_cursor_block(cnt, entry_block);

  /* generate body instructions */
  ast(cnt, ast_block);

  pop_curr_dependent(cnt);
  set_cursor_block(cnt, prev_block);
  return &fn_proto->base;
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

  const BinopKind op = binop->data.expr_binop.kind;
  if (ast_binop_is_assign(op)) {
    switch (op) {
    case BINOP_ASSIGN: {
      return add_instr_store(cnt, binop, rhs, lhs);
    }
    default:
      bl_abort("unimplemented");
    }
  } else {
    lhs = add_instr_load_if_needed(cnt, lhs);
    rhs = add_instr_load_if_needed(cnt, rhs);
    return add_instr_binop(cnt, binop, lhs, rhs, op);
  }
}

MirInstr *
ast_decl_entity(Context *cnt, Ast *entity)
{
  Ast *ast_name  = entity->data.decl.name;
  Ast *ast_type  = entity->data.decl.type;
  Ast *ast_value = entity->data.decl_entity.value;

  /* Prepare scope entry created in parser */
  assert(ast_name->kind == AST_IDENT);
  Scope *scope = ast_name->data.ident.scope;
  assert(scope);
  ScopeEntry *scope_entry = scope_lookup(scope, ast_name->data.ident.hash, true);
  assert(scope_entry && "declaration has no scope entry");

  MirInstr *value = ast(cnt, ast_value);

  if (value && value->kind == MIR_INSTR_FN_PROTO) {
    value->node        = ast_name;
    scope_entry->instr = value;

    if (ast_type) {
      push_curr_dependent(cnt, value);
      ((MirInstrFnProto *)value)->user_type = ast_create_type_resolver_call(cnt, ast_type);
      pop_curr_dependent(cnt);
    }

    if (is_entry_fn(ast_name)) {
      assert(!cnt->entry_fn);
      cnt->entry_fn = value;
    }
  } else {
    MirInstr *type     = ast_type ? ast_create_type_resolver_call(cnt, ast_type) : NULL;
    MirInstr *decl     = add_instr_decl_var(cnt, type, ast_name);
    scope_entry->instr = decl;

    if (value) {
      if (!type) add_instr_try_infer(cnt, NULL, value, decl);
      MirInstr *decl_ref = add_instr_addr_of(cnt, NULL, decl);
      return add_instr_store(cnt, ast_value, value, decl_ref);
    }

    if (is_entry_fn(ast_name)) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_FUNC, ast_name->src,
                  BUILDER_CUR_WORD, "'main' is expected to be a function");
    }
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
    result = add_instr_const_type(cnt, ast_ref, get_buildin(cnt, id));
  }

  assert(result && "unimplemented type ref resolving");
  add_instr_validate_type(cnt, result);
  return result;
}

MirInstr *
ast_type_fn(Context *cnt, Ast *type_fn)
{
  Ast *   ast_ret_type  = type_fn->data.type_fn.ret_type;
  BArray *ast_arg_types = type_fn->data.type_fn.args;

  /* return type */
  MirInstr *ret_type = ast_ret_type ? ast(cnt, ast_ret_type) : NULL;

  BArray *arg_types = NULL;
  if (ast_arg_types && bo_array_size(ast_arg_types)) {
    const size_t c = bo_array_size(ast_arg_types);
    arg_types      = bo_array_new(sizeof(MirInstr *));
    bo_array_reserve(arg_types, c);

    Ast *     ast_arg_type;
    MirInstr *arg_type;
    for (size_t i = 0; i < c; ++i) {
      ast_arg_type = bo_array_at(ast_arg_types, i, Ast *);
      arg_type     = ast(cnt, ast_arg_type);
      bo_array_push_back(arg_types, arg_type);
    }
  }

  return add_instr_type_fn(cnt, type_fn, ret_type, arg_types);
}

MirInstr *
ast_create_type_resolver_call(Context *cnt, Ast *type)
{
  if (!type) return NULL;
  MirBlock *prev_block = get_current_block(cnt);
  MirInstr *fn         = add_instr_fn_proto(cnt, NULL, NULL, NULL);
  fn->value.type       = cnt->buildin_types.entry_resolve_type_fn;

  MirBlock *entry = append_block(cnt, fn, "entry");
  set_cursor_block(cnt, entry);

  MirInstr *result = ast(cnt, type);
  add_instr_ret(cnt, NULL, result);

  set_cursor_block(cnt, prev_block);
  add_dep_uq(cnt->curr_dependent, fn, MIR_DEP_STRICT);
  return create_instr_call_type_resolve(cnt, fn, type);
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
  case AST_STMT_IF:
    ast_stmt_if(cnt, node);
    break;
  case AST_DECL_ENTITY:
    return ast_decl_entity(cnt, node);
  case AST_TYPE_REF:
    return ast_type_ref(cnt, node);
  case AST_TYPE_FN:
    return ast_type_fn(cnt, node);
  case AST_EXPR_LIT_INT:
    return ast_expr_lit_int(cnt, node);
  case AST_EXPR_LIT_BOOL:
    return ast_expr_lit_bool(cnt, node);
  case AST_EXPR_LIT_FN:
    return ast_expr_lit_fn(cnt, node);
  case AST_EXPR_BINOP:
    return ast_expr_binop(cnt, node);
  case AST_EXPR_REF:
    return ast_expr_ref(cnt, node);
  default:
    bl_abort("invalid node %s", ast_get_name(node));
  }

  return NULL;
}

const char *
instr_name(MirInstr *instr)
{
  assert(instr);
  switch (instr->kind) {
  case MIR_INSTR_INVALID:
    return "InstrInvalid";
  case MIR_INSTR_DECL_VAR:
    return "InstrDeclVar";
  case MIR_INSTR_CONST:
    return "InstrConst";
  case MIR_INSTR_LOAD:
    return "InstrLoad";
  case MIR_INSTR_STORE:
    return "InstrStore";
  case MIR_INSTR_BINOP:
    return "InstrBinop";
  case MIR_INSTR_RET:
    return "InstrRet";
  case MIR_INSTR_VALIDATE_TYPE:
    return "InstrValidateType";
  case MIR_INSTR_FN_PROTO:
    return "InstrFnProto";
  case MIR_INSTR_CALL:
    return "InstrCall";
  case MIR_INSTR_DECL_REF:
    return "InstrDeclRef";
  case MIR_INSTR_UNREACHABLE:
    return "InstrUnreachable";
  case MIR_INSTR_TYPE_FN:
    return "InstrTypeFn";
  case MIR_INSTR_ADDR_OF:
    return "InstrAddrOf";
  case MIR_INSTR_TRY_INFER:
    return "InstrTryInfer";
  case MIR_INSTR_COND_BR:
    return "InstrCondBr";
  }

  return "UNKNOWN";
}

/* public */
void
mir_arenas_init(MirArenas *arenas)
{
  arena_init(&arenas->block_arena, sizeof(MirBlock), ARENA_CHUNK_COUNT, (ArenaElemDtor)block_dtor);
  arena_init(&arenas->instr_arena, sizeof(union _MirInstr), ARENA_CHUNK_COUNT,
             (ArenaElemDtor)instr_dtor);
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
    append_buf(buf, len, "fn(");

    MirType *tmp;
    BArray * args = type->data.fn.arg_types;
    if (args) {
      barray_foreach(args, tmp)
      {
        _type_to_str(buf, len, tmp);
        if (i < bo_array_size(args)) append_buf(buf, len, ", ");
      }
    }

    append_buf(buf, len, ") ");

    _type_to_str(buf, len, type->data.fn.ret_type);
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
execute_entry_fn(Context *cnt)
{
  if (!cnt->entry_fn) {
    msg_error("assembly '%s' has no entry function!", cnt->assembly->name);
    return;
  }

  MirValue result;
  result = *exec_instr(cnt, cnt->entry_fn);
  msg_log("execution finished with state: %llu", result.data.v_int);
}

void
init_buildins(Context *cnt)
{
  uint64_t tmp;
  cnt->buildin_types.table = bo_htbl_new(sizeof(BuildinType), _BUILDIN_TYPE_COUNT);
  for (int i = 0; i < _BUILDIN_TYPE_COUNT; ++i) {
    tmp = bo_hash_from_str(buildin_type_names[i]);
    bo_htbl_insert(cnt->buildin_types.table, tmp, i);
  }

  cnt->buildin_types.entry_type = create_type_type(cnt);
  cnt->buildin_types.entry_void = create_type_void(cnt);

  cnt->buildin_types.entry_s8 = create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_S8], 8, true);
  cnt->buildin_types.entry_s16 =
      create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_S16], 16, true);
  cnt->buildin_types.entry_s32 =
      create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_S32], 32, true);
  cnt->buildin_types.entry_s64 =
      create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_S64], 64, true);

  cnt->buildin_types.entry_u8 = create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_U8], 8, false);
  cnt->buildin_types.entry_u16 =
      create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_U16], 16, false);
  cnt->buildin_types.entry_u32 =
      create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_U32], 32, false);
  cnt->buildin_types.entry_usize =
      create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_USIZE], 64, false);

  cnt->buildin_types.entry_bool = create_type_bool(cnt);

  cnt->buildin_types.entry_resolve_type_fn =
      create_type_fn(cnt, cnt->buildin_types.entry_type, NULL);
}

void
mir_run(Builder *builder, Assembly *assembly)
{
  Context cnt;
  memset(&cnt, 0, sizeof(Context));
  cnt.builder       = builder;
  cnt.assembly      = assembly;
  cnt.arenas        = &builder->mir_arenas;
  cnt.verbose       = builder->flags & BUILDER_VERBOSE;
  cnt.analyze_stack = bo_list_new(sizeof(MirInstr *));
  cnt.llvm_cnt      = LLVMContextCreate();
  cnt.llvm_module   = LLVMModuleCreateWithNameInContext(assembly->name, cnt.llvm_cnt);
  cnt.llvm_td       = LLVMGetModuleDataLayout(cnt.llvm_module);
  cnt.pass          = PASS_GENERATE;

  init_buildins(&cnt);

  entry_fn_hash = bo_hash_from_str(entry_fn_name);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    ast(&cnt, unit->ast);
  }

  if (!builder->errorc) {
    cnt.pass = PASS_ANALYZE;
    analyze(&cnt);
  }

  if (!builder->errorc && builder->flags & BUILDER_RUN) {
    msg_log("executing 'main' in compile time...");
    execute_entry_fn(&cnt);
  }

  bo_unref(cnt.buildin_types.table);
  bo_unref(cnt.analyze_stack);
  /* TODO: pass context to the next stages */
  LLVMContextDispose(cnt.llvm_cnt);
}
