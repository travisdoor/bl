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
#define TEST_CASE_FN_NAME "__test"
#define RESOLVE_TYPE_FN_NAME "__type"
#define IMPL_FN_NAME "__impl_"
#define DEFAULT_EXEC_FRAME_STACK_SIZE 2097152 // 2MB
#define DEFAULT_EXEC_CALL_STACK_NESTING 10000

union _MirInstr
{
  MirInstrBlock        block;
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
  MirInstrTryInfer     try_infer;
  MirInstrCondBr       cond_br;
  MirInstrBr           br;
  MirInstrUnop         unop;
  MirInstrArg          arg;
  MirInstrElemPtr      elem_ptr;
  MirInstrTypeArray    type_array;
};

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

typedef struct
{
  Builder *  builder;
  Assembly * assembly;
  MirModule *module;
  BArray *   analyze_stack;
  BArray *   test_cases;

  struct
  {
    char * frame_stack;
    char * frame_stack_ptr;
    size_t frame_stack_allocated;
    size_t frame_stack_size;

    BArray *call_stack;
    bool    aborted;
  } exec;

  /* DynCall/Lib data used for external method execution in compile time */
  struct
  {
    DLLib *   lib;
    DCCallVM *vm;
  } dl;

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
    MirType *entry_test_case_fn;
  } buildin_types;

  MirInstrBlock *current_block;
  MirInstrBlock *break_block;
  MirInstrBlock *continue_block;

  MirFn *entry_fn;
  bool   verbose_pre, verbose_post;
} Context;

static const char *entry_fn_name                           = "main";
static const char *buildin_type_names[_BUILDIN_TYPE_COUNT] = {"s8",  "s16", "s32", "s64",   "u8",
                                                              "u16", "u32", "u64", "usize", "bool"};
static uint64_t    entry_fn_hash                           = 0;

static void
fn_dtor(MirFn *fn)
{
  bo_unref(fn->arg_slots);
}

static void
instr_dtor(MirInstr *instr)
{
  switch (instr->kind) {
  case MIR_INSTR_TYPE_FN:
    bo_unref(((MirInstrTypeFn *)instr)->arg_types);
    break;
  case MIR_INSTR_CALL:
    bo_unref(((MirInstrCall *)instr)->args);
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

static int
init_dl(Context *cnt);

static void
init_frame_stack(Context *cnt, size_t size);

static void
terminate_frame_stack(Context *cnt);

static void
terminate_dl(Context *cnt);

static void
execute_entry_fn(Context *cnt);

static void
execute_test_cases(Context *cnt);

static bool
type_cmp(MirType *first, MirType *second);

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

static MirType *
create_type_array(Context *cnt, MirType *elem_type, size_t len);

static MirVar *
create_var(Context *cnt, const char *name, MirType *type);

static MirFn *
create_fn(Context *cnt, Ast *node, const char *name, BArray *arg_slots, bool is_external,
          bool is_test_case);

static MirInstrBlock *
append_block(Context *cnt, MirFn *fn, const char *name);

/* instructions */
static void
push_into_curr_block(Context *cnt, MirInstr *instr);

static void
erase_from_curr_block(Context *cnt, MirInstr *instr);

#define create_instr(_cnt, _kind, _node, _t) ((_t)_create_instr((_cnt), (_kind), (_node)))

static MirInstr *
_create_instr(Context *cnt, MirInstrKind kind, Ast *node);

static MirInstr *
create_instr_call_type_resolve(Context *cnt, MirInstr *resolver_fn, Ast *type);

static MirInstr *
create_instr_fn_proto(Context *cnt, Ast *node, MirInstr *type, MirInstr *user_type);

// static MirInstr *
// append_instr_addr_of(Context *cnt, Ast *node, MirInstr *target);

static MirInstr *
append_instr_arg(Context *cnt, Ast *node, unsigned i);

static MirInstr *
append_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index);

static MirInstr *
append_instr_cond_br(Context *cnt, Ast *node, MirInstr *cond, MirInstrBlock *then_block,
                     MirInstrBlock *else_block);

static MirInstr *
append_instr_br(Context *cnt, Ast *node, MirInstrBlock *then_block);

static MirInstr *
append_instr_load(Context *cnt, Ast *node, MirInstr *src);

static MirInstr *
append_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, BArray *arg_types);

static MirInstr *
append_instr_type_array(Context *cnt, Ast *node, MirInstr *elem_type, MirInstr *len);

static MirInstr *
append_instr_try_infer(Context *cnt, Ast *node, MirInstr *expr, MirInstr *dest);

static MirInstr *
append_instr_fn_proto(Context *cnt, Ast *node, MirInstr *type, MirInstr *user_type);

static MirInstr *
append_instr_decl_ref(Context *cnt, Ast *node, MirInstr *decl);

static MirInstr *
append_instr_call(Context *cnt, Ast *node, MirInstr *callee, BArray *args);

static MirInstr *
create_instr_decl_var(Context *cnt, MirInstr *type, Ast *name);

static MirInstr *
append_instr_decl_var(Context *cnt, MirInstr *type, Ast *name);

static MirInstr *
append_instr_const_int(Context *cnt, Ast *node, uint64_t val);

static MirInstr *
append_instr_const_bool(Context *cnt, Ast *node, bool val);

static MirInstr *
append_instr_const_type(Context *cnt, Ast *node, MirType *type);

static MirInstr *
append_instr_const_string(Context *cnt, Ast *node, const char *str);

static MirInstr *
append_instr_ret(Context *cnt, Ast *node, MirInstr *value);

static MirInstr *
append_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest);

static MirInstr *
append_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op);

static MirInstr *
append_instr_unop(Context *cnt, Ast *node, MirInstr *instr, UnopKind op);

static MirInstr *
append_instr_validate_type(Context *cnt, MirInstr *src);

static MirInstr *
append_instr_unrecheable(Context *cnt, Ast *node);

/* ast */
static MirInstr *
ast_create_type_resolver_call(Context *cnt, Ast *type);

static MirInstr *
ast(Context *cnt, Ast *node);

static void
ast_ublock(Context *cnt, Ast *ublock);

static void
ast_test_case(Context *cnt, Ast *test);

static void
ast_unrecheable(Context *cnt, Ast *unr);

static void
ast_block(Context *cnt, Ast *block);

static void
ast_stmt_if(Context *cnt, Ast *stmt_if);

static void
ast_stmt_return(Context *cnt, Ast *ret);

static void
ast_stmt_loop(Context *cnt, Ast *loop);

static void
ast_stmt_break(Context *cnt, Ast *br);

static void
ast_stmt_continue(Context *cnt, Ast *cont);

static MirInstr *
ast_decl_entity(Context *cnt, Ast *entity);

static MirInstr *
ast_decl_arg(Context *cnt, Ast *arg);

static MirInstr *
ast_type_ref(Context *cnt, Ast *type_ref);

static MirInstr *
ast_type_fn(Context *cnt, Ast *type_fn);

static MirInstr *
ast_type_arr(Context *cnt, Ast *type_arr);

static MirInstr *
ast_expr_ref(Context *cnt, Ast *ref);

static MirInstr *
ast_expr_call(Context *cnt, Ast *call);

static MirInstr *
ast_expr_elem(Context *cnt, Ast *elem);

static MirInstr *
ast_expr_lit_int(Context *cnt, Ast *expr);

static MirInstr *
ast_expr_lit_bool(Context *cnt, Ast *expr);

static MirInstr *
ast_expr_lit_fn(Context *cnt, Ast *lit_fn);

static MirInstr *
ast_expr_lit_string(Context *cnt, Ast *lit_string);

static MirInstr *
ast_expr_binop(Context *cnt, Ast *binop);

static MirInstr *
ast_expr_unary(Context *cnt, Ast *unop);

static LLVMTypeRef
to_llvm_type(Context *cnt, MirType *type, size_t *out_size, unsigned *out_alignment);

/* analyze */
static bool
analyze_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static bool
analyze_instr_block(Context *cnt, MirInstrBlock *block, bool comptime);

static bool
analyze_instr(Context *cnt, MirInstr *instr, bool comptime);

static bool
analyze_instr_ret(Context *cnt, MirInstrRet *ret);

static bool
analyze_instr_arg(Context *cnt, MirInstrArg *arg);

static bool
analyze_instr_unop(Context *cnt, MirInstrUnop *unop);

static bool
analyze_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static bool
analyze_instr_cond_br(Context *cnt, MirInstrCondBr *br);

static bool
analyze_instr_br(Context *cnt, MirInstrBr *br);

static bool
analyze_instr_load(Context *cnt, MirInstrLoad *load);

static bool
analyze_instr_store(Context *cnt, MirInstrStore *store);

static bool
analyze_instr_addr_of(Context *cnt, MirInstrAddrOf *addrof);

static bool
analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto, bool comptime);

static bool
analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn);

static bool
analyze_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr);

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
analyze_instr_call(Context *cnt, MirInstrCall *call, bool comptime);

static bool
analyze_instr_binop(Context *cnt, MirInstrBinop *binop);

static void
analyze(Context *cnt);

/* execute */
static MirValue *
exec_instr(Context *cnt, MirInstr *instr);

static MirValue *
exec_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static MirValue *
exec_instr_br(Context *cnt, MirInstrBr *br);

static MirValue *
exec_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static MirValue *
exec_instr_arg(Context *cnt, MirInstrArg *arg);

static MirValue *
exec_instr_cond_br(Context *cnt, MirInstrCondBr *br);

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
exec_instr_unop(Context *cnt, MirInstrUnop *unop);

static MirValue *
exec_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static MirValue *
exec_instr_call(Context *cnt, MirInstrCall *call);

static MirValue *
exec_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn);

static MirValue *
exec_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr);

static MirValue *
exec_instr_ret(Context *cnt, MirInstrRet *ret);

static MirValue *
exec_instr_decl_var(Context *cnt, MirInstrDeclVar *var);

static bool
exec_fn(Context *cnt, MirFn *fn, BArray *args, MirValue *out_value);

/* zero max nesting = unlimited nesting */
static void
exec_print_call_stack(Context *cnt, size_t max_nesting);

static inline const char *
gen_uq_name(Context *cnt, const char *prefix)
{
  static int ui = 0;
  BString *  s  = builder_create_cached_str(cnt->builder);

  bo_string_append(s, prefix);
  char ui_str[21];
  sprintf(ui_str, "%i", ui++);
  bo_string_append(s, ui_str);
  return bo_string_get(s);
}

static inline size_t
sizeof_type_in_bits(MirType *type)
{
  return type->size;
}

static inline size_t
store_sizeof_type_in_bytes(MirType *type)
{
  size_t size = type->size;
  if (size % 8) size = size + (8 - size % 8);
  size = size / 8;
  return size;
}

static inline bool
is_pointer_type(MirType *type)
{
  assert(type);
  return type->kind == MIR_TYPE_PTR;
}

static inline bool
is_array_type(MirType *type)
{
  assert(type);
  return type->kind == MIR_TYPE_ARRAY;
}

static inline void
exec_copy_value(MirValue *dest, MirValue *src)
{
  assert(dest->is_stack_allocated);
  MirFrameStackPtr *dest_ptr = dest->data.v_stack_ptr;
  MirFrameStackPtr *src_ptr =
      src->is_stack_allocated ? src->data.v_stack_ptr : (MirFrameStackPtr *)&src->data;

  assert(dest_ptr && src_ptr);
  const size_t size = store_sizeof_type_in_bytes(dest->type);

  /* copy data from src to destination */
  memcpy(dest_ptr, src_ptr, size);
}

static inline int64_t
exec_read_int64(MirValue *value)
{
  if (!value->is_stack_allocated) return value->data.v_int;
  assert(value->data.v_stack_ptr);

  const size_t size = store_sizeof_type_in_bytes(value->type);
  assert(size <= sizeof(int64_t));

  int64_t tmp = 0;

  MirFrameStackPtr *dest_ptr = (void *)&tmp;
  MirFrameStackPtr *src_ptr  = value->data.v_stack_ptr;
  memcpy(dest_ptr, src_ptr, size);

  return tmp;
}

static inline void
exec_call_stack_push(Context *cnt, MirInstr *instr)
{
  assert(instr);
  bo_array_push_back(cnt->exec.call_stack, instr);
  if (bo_array_size(cnt->exec.call_stack) > DEFAULT_EXEC_CALL_STACK_NESTING) {
    msg_error("Maximum call stack nesting exceeded!!!");
    exec_print_call_stack(cnt, 10);
    abort();
  }
}

static inline MirInstr **
exec_call_stack_get(Context *cnt)
{
  const size_t c = bo_array_size(cnt->exec.call_stack);
  if (c == 0) return NULL;
  return &bo_array_at(cnt->exec.call_stack, c - 1, MirInstr *);
}

static inline void
exec_call_stack_pop(Context *cnt)
{
  const size_t c = bo_array_size(cnt->exec.call_stack);
  if (c == 0) return;
  bo_array_pop_back(cnt->exec.call_stack);
}

/* allocate memory on frame stack, size is in bits!!! */
static inline MirFrameStackPtr
exec_frame_stack_alloc(Context *cnt, MirValue *val)
{
  assert(val && val->type);
  MirType *    type = val->type;
  const size_t size = store_sizeof_type_in_bytes(type);
  assert(size && "trying to allocate 0 bits on stack");

  // bl_log("allocate %u bytes on stack", size);
  cnt->exec.frame_stack_allocated += size;
  if (cnt->exec.frame_stack_allocated > cnt->exec.frame_stack_size) {
    msg_error("Stack overflow!!!");
    exec_print_call_stack(cnt, 10);
    abort();
  }

  char *mem = cnt->exec.frame_stack_ptr;
  cnt->exec.frame_stack_ptr += size;

  val->is_stack_allocated = true;
  val->data.v_stack_ptr   = mem;
  return mem;
}

static inline MirFrameStackPtr
exec_frame_stack_get_ptr(Context *cnt)
{
  return cnt->exec.frame_stack_ptr;
}

static inline void
exec_frame_stack_rollback(Context *cnt, MirFrameStackPtr ptr)
{
  if (cnt->exec.frame_stack_ptr < (char *)ptr) bl_abort("frame stack corrupted!");
  const ptrdiff_t freed_bytes = cnt->exec.frame_stack_ptr - (char *)ptr;
  // bl_log("stack rollback to %p, free: %u bytes", ptr, freed_bytes);
  assert(ptr);
  cnt->exec.frame_stack_allocated -= freed_bytes;
  cnt->exec.frame_stack_ptr = ptr;
}

static inline void
terminate_block(MirInstrBlock *block, MirInstr *terminator)
{
  assert(block);
  if (block->terminal) bl_abort("basic block already terminated!");
  block->terminal = terminator;
}

static inline bool
is_block_terminated(MirInstrBlock *block)
{
  return block->terminal;
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

static inline bool
get_block_terminator(MirInstrBlock *block)
{
  return block->terminal;
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
set_cursor_block(Context *cnt, MirInstrBlock *block)
{
  if (!block) return;
  cnt->current_block = block;
}

static inline MirInstrBlock *
get_current_block(Context *cnt)
{
  return cnt->current_block;
}

static inline MirFn *
get_current_fn(Context *cnt)
{
  return cnt->current_block ? cnt->current_block->owner_fn : NULL;
}

static inline void
error_types(Context *cnt, MirType *from, MirType *to, Ast *loc, const char *msg)
{
  assert(from && to);
  if (!msg) msg = "no implicit cast for type '%s' and '%s'";

  char tmp_from[256];
  char tmp_to[256];
  mir_type_to_str(tmp_from, 256, from);
  mir_type_to_str(tmp_to, 256, to);

  builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, loc->src, BUILDER_CUR_WORD, msg,
              tmp_from, tmp_to);
}

static inline MirInstr *
append_instr_load_if_needed(Context *cnt, MirInstr *src)
{
  if (!src) return src;
  switch (src->kind) {
  case MIR_INSTR_CONST:
  case MIR_INSTR_BINOP:
  case MIR_INSTR_UNOP:
  case MIR_INSTR_CALL:
    return src;
  default:
    break;
  }

  return append_instr_load(cnt, NULL, src);
}

static void
unref_instr(MirInstr *instr)
{
  if (!instr) return;
  --instr->ref_count;
  switch (instr->kind) {
  case MIR_INSTR_CALL:
    ((MirInstrCall *)instr)->callee->ref_count--;
    break;
  case MIR_INSTR_VALIDATE_TYPE:
    ((MirInstrValidateType *)instr)->src->ref_count--;
    break;
  case MIR_INSTR_TRY_INFER:
    ((MirInstrTryInfer *)instr)->src->ref_count--;
    ((MirInstrTryInfer *)instr)->dest->ref_count--;
    break;
  case MIR_INSTR_CONST:
  case MIR_INSTR_FN_PROTO:
  case MIR_INSTR_DECL_VAR:
    break;
  default:
    bl_abort("unimplemented for %s", mir_instr_name(instr));
  }
}

static inline void
ref_instr(MirInstr *instr)
{
  if (!instr) return;
  ++instr->ref_count;
}

/* impl */
MirType *
create_type_type(Context *cnt)
{
  MirType *tmp   = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind      = MIR_TYPE_TYPE;
  tmp->name      = "type";
  tmp->llvm_type = to_llvm_type(cnt, tmp, NULL, NULL);
  return tmp;
}

MirType *
create_type_void(Context *cnt)
{
  MirType *tmp   = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind      = MIR_TYPE_VOID;
  tmp->name      = "void";
  tmp->llvm_type = to_llvm_type(cnt, tmp, &tmp->size, &tmp->alignment);
  return tmp;
}

MirType *
create_type_bool(Context *cnt)
{
  MirType *tmp   = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind      = MIR_TYPE_BOOL;
  tmp->name      = "bool";
  tmp->llvm_type = to_llvm_type(cnt, tmp, &tmp->size, &tmp->alignment);
  return tmp;
}

MirType *
create_type_int(Context *cnt, const char *name, int bitcount, bool is_signed)
{
  assert(bitcount > 0);
  MirType *tmp                = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind                   = MIR_TYPE_INT;
  tmp->name                   = name;
  tmp->data.integer.bitcount  = bitcount;
  tmp->data.integer.is_signed = is_signed;
  tmp->llvm_type              = to_llvm_type(cnt, tmp, &tmp->size, &tmp->alignment);

  return tmp;
}

MirType *
create_type_ptr(Context *cnt, MirType *src_type)
{
  MirType *tmp       = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind          = MIR_TYPE_PTR;
  tmp->data.ptr.next = src_type;
  tmp->llvm_type     = to_llvm_type(cnt, tmp, &tmp->size, &tmp->alignment);

  return tmp;
}

MirType *
create_type_fn(Context *cnt, MirType *ret_type, BArray *arg_types)
{
  MirType *tmp           = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind              = MIR_TYPE_FN;
  tmp->data.fn.arg_types = arg_types;
  tmp->data.fn.ret_type  = ret_type ? ret_type : cnt->buildin_types.entry_void;
  tmp->llvm_type         = to_llvm_type(cnt, tmp, &tmp->size, &tmp->alignment);

  return tmp;
}

MirType *
create_type_array(Context *cnt, MirType *elem_type, size_t len)
{
  MirType *tmp              = arena_alloc(&cnt->module->arenas.type_arena);
  tmp->kind                 = MIR_TYPE_ARRAY;
  tmp->data.array.elem_type = elem_type;
  tmp->data.array.len       = len;
  tmp->llvm_type            = to_llvm_type(cnt, tmp, &tmp->size, &tmp->alignment);

  return tmp;
}

MirVar *
create_var(Context *cnt, const char *name, MirType *type)
{
  assert(name);
  MirVar *tmp     = arena_alloc(&cnt->module->arenas.var_arena);
  tmp->name       = name;
  tmp->value.type = type;
  return tmp;
}

MirFn *
create_fn(Context *cnt, Ast *node, const char *name, BArray *arg_slots, bool is_external,
          bool is_test_case)
{
  MirFn *tmp        = arena_alloc(&cnt->module->arenas.fn_arena);
  tmp->name         = name;
  tmp->arg_slots    = arg_slots;
  tmp->is_external  = is_external;
  tmp->is_test_case = is_test_case;
  tmp->node         = node;
  return tmp;
}

/* instructions */
void
push_into_curr_block(Context *cnt, MirInstr *instr)
{
  assert(instr);
  MirInstrBlock *block = get_current_block(cnt);
  MirFn *        fn    = get_current_fn(cnt);
  assert(block);

  instr->id          = fn->instr_count++;
  instr->owner_block = block;
  instr->prev        = block->last_instr;

  if (!block->entry_instr) block->entry_instr = instr;
  if (instr->prev) instr->prev->next = instr;
  block->last_instr = instr;
}

void
erase_from_curr_block(Context *cnt, MirInstr *instr)
{
  assert(instr);
  MirInstrBlock *block = get_current_block(cnt);
  assert(block);

  unref_instr(instr);

  if (block->entry_instr == instr) block->entry_instr = instr->next;
  if (instr->prev) instr->prev->next = instr->next;
  if (instr->next) instr->next->prev = instr->prev;

  instr->prev = NULL;
  instr->next = NULL;
}

MirInstr *
_create_instr(Context *cnt, MirInstrKind kind, Ast *node)
{
  MirInstr *tmp = arena_alloc(&cnt->module->arenas.instr_arena);
  tmp->kind     = kind;
  tmp->node     = node;
  tmp->id       = 0;

#if BL_DEBUG
  static uint64_t counter = 0;
  tmp->_serial            = counter++;
#endif

  return tmp;
}

MirInstrBlock *
append_block(Context *cnt, MirFn *fn, const char *name)
{
  assert(fn && name);
  MirInstrBlock *tmp = create_instr(cnt, MIR_INSTR_BLOCK, NULL, MirInstrBlock *);
  tmp->name          = name;
  tmp->owner_fn      = fn;
  tmp->base.id       = fn->block_count++;

  if (!fn->first_block) {
    fn->first_block = tmp;

    /* first block is referenced everytime!!! */
    ref_instr(&tmp->base);
  }

  tmp->base.prev = &fn->last_block->base;
  if (fn->last_block) fn->last_block->base.next = &tmp->base;
  fn->last_block = tmp;

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
  ref_instr(resolver_fn);
  return &tmp->base;
}

MirInstr *
append_instr_type_fn(Context *cnt, Ast *node, MirInstr *ret_type, BArray *arg_types)
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
append_instr_type_array(Context *cnt, Ast *node, MirInstr *elem_type, MirInstr *len)
{
  MirInstrTypeArray *tmp = create_instr(cnt, MIR_INSTR_TYPE_ARRAY, node, MirInstrTypeArray *);
  tmp->base.value.type   = cnt->buildin_types.entry_type;
  tmp->base.comptime     = true;
  tmp->elem_type         = elem_type;
  tmp->len               = len;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_arg(Context *cnt, Ast *node, unsigned i)
{
  MirInstrArg *tmp = create_instr(cnt, MIR_INSTR_ARG, node, MirInstrArg *);
  tmp->i           = i;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_cond_br(Context *cnt, Ast *node, MirInstr *cond, MirInstrBlock *then_block,
                     MirInstrBlock *else_block)
{
  assert(cond && then_block && else_block);
  ref_instr(cond);
  ref_instr(&then_block->base);
  ref_instr(&else_block->base);
  MirInstrCondBr *tmp  = create_instr(cnt, MIR_INSTR_COND_BR, node, MirInstrCondBr *);
  tmp->base.value.type = cnt->buildin_types.entry_void;
  tmp->cond            = cond;
  tmp->then_block      = then_block;
  tmp->else_block      = else_block;

  MirInstrBlock *block = get_current_block(cnt);
  terminate_block(block, &tmp->base);

  push_into_curr_block(cnt, &tmp->base);
  ref_instr(&tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_br(Context *cnt, Ast *node, MirInstrBlock *then_block)
{
  assert(then_block);
  ref_instr(&then_block->base);
  MirInstrBr *tmp      = create_instr(cnt, MIR_INSTR_BR, node, MirInstrBr *);
  tmp->base.value.type = cnt->buildin_types.entry_void;
  tmp->then_block      = then_block;
  ref_instr(&tmp->base);

  MirInstrBlock *block = get_current_block(cnt);
  terminate_block(block, &tmp->base);

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_elem_ptr(Context *cnt, Ast *node, MirInstr *arr_ptr, MirInstr *index)
{
  assert(arr_ptr && index);
  ref_instr(arr_ptr);
  ref_instr(index);
  MirInstrElemPtr *tmp = create_instr(cnt, MIR_INSTR_ELEM_PTR, node, MirInstrElemPtr *);
  tmp->arr_ptr         = arr_ptr;
  tmp->index           = index;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_load(Context *cnt, Ast *node, MirInstr *src)
{
  ref_instr(src);
  MirInstrLoad *tmp = create_instr(cnt, MIR_INSTR_LOAD, node, MirInstrLoad *);
  tmp->src          = src;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

/*MirInstr *
append_instr_addr_of(Context *cnt, Ast *node, MirInstr *target)
{
  ref_instr(target);
  MirInstrAddrOf *tmp = create_instr(cnt, MIR_INSTR_ADDR_OF, node, MirInstrAddrOf *);
  tmp->target         = target;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
  }*/

MirInstr *
append_instr_unrecheable(Context *cnt, Ast *node)
{
  MirInstrUnreachable *tmp = create_instr(cnt, MIR_INSTR_UNREACHABLE, node, MirInstrUnreachable *);
  tmp->base.value.type     = cnt->buildin_types.entry_void;
  push_into_curr_block(cnt, &tmp->base);
  ref_instr(&tmp->base);
  return &tmp->base;
}

MirInstr *
create_instr_fn_proto(Context *cnt, Ast *node, MirInstr *type, MirInstr *user_type)
{
  MirInstrFnProto *tmp = create_instr(cnt, MIR_INSTR_FN_PROTO, node, MirInstrFnProto *);
  tmp->base.id         = (int)bo_array_size(cnt->analyze_stack);
  tmp->type            = type;
  tmp->user_type       = user_type;
  return &tmp->base;
}

MirInstr *
append_instr_fn_proto(Context *cnt, Ast *node, MirInstr *type, MirInstr *user_type)
{
  MirInstr *tmp = create_instr_fn_proto(cnt, node, type, user_type);
  bo_array_push_back(cnt->analyze_stack, tmp);
  return tmp;
}

MirInstr *
append_instr_decl_ref(Context *cnt, Ast *node, MirInstr *decl)
{
  MirInstrDeclRef *tmp = create_instr(cnt, MIR_INSTR_DECL_REF, node, MirInstrDeclRef *);
  ref_instr(decl);
  tmp->decl = decl;
  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_call(Context *cnt, Ast *node, MirInstr *callee, BArray *args)
{
  assert(callee);
  MirInstrCall *tmp = create_instr(cnt, MIR_INSTR_CALL, node, MirInstrCall *);
  tmp->args         = args;
  tmp->callee       = callee;
  ref_instr(callee);

  /* every call must have ref_count = 1 at least */
  ref_instr(&tmp->base);

  /* reference all arguments */
  if (args) {
    MirInstr *instr;
    barray_foreach(args, instr) ref_instr(instr);
  }

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
create_instr_decl_var(Context *cnt, MirInstr *type, Ast *name)
{
  if (type) ref_instr(type);
  MirInstrDeclVar *tmp = create_instr(cnt, MIR_INSTR_DECL_VAR, name, MirInstrDeclVar *);
  tmp->type            = type;
  tmp->var             = create_var(cnt, name->data.ident.str, NULL); /* type is filled later */
  tmp->base.value.data.v_ptr = &tmp->var->value;
  return &tmp->base;
}

MirInstr *
append_instr_decl_var(Context *cnt, MirInstr *type, Ast *name)
{
  MirInstr *tmp = create_instr_decl_var(cnt, type, name);
  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_int(Context *cnt, Ast *node, uint64_t val)
{
  MirInstr *tmp         = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime         = true;
  tmp->value.type       = cnt->buildin_types.entry_s32;
  tmp->value.data.v_int = (long long int)val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_bool(Context *cnt, Ast *node, bool val)
{
  MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime          = true;
  tmp->value.type        = cnt->buildin_types.entry_bool;
  tmp->value.data.v_bool = val;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_type(Context *cnt, Ast *node, MirType *type)
{
  MirInstr *tmp          = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime          = true;
  tmp->value.type        = cnt->buildin_types.entry_type;
  tmp->value.data.v_type = type;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_const_string(Context *cnt, Ast *node, const char *str)
{
  MirInstr *tmp         = create_instr(cnt, MIR_INSTR_CONST, node, MirInstr *);
  tmp->comptime         = true;
  tmp->value.type       = create_type_array(cnt, cnt->buildin_types.entry_u8, strlen(str));
  tmp->value.data.v_str = str;

  push_into_curr_block(cnt, tmp);
  return tmp;
}

MirInstr *
append_instr_ret(Context *cnt, Ast *node, MirInstr *value)
{
  if (value) ref_instr(value);

  MirInstrRet *tmp     = create_instr(cnt, MIR_INSTR_RET, node, MirInstrRet *);
  tmp->value           = value;
  tmp->base.value.type = cnt->buildin_types.entry_void;
  ref_instr(&tmp->base);

  MirInstrBlock *block = get_current_block(cnt);
  terminate_block(block, &tmp->base);

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_store(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest)
{
  assert(src && dest);
  ref_instr(src);
  ref_instr(dest);
  MirInstrStore *tmp = create_instr(cnt, MIR_INSTR_STORE, node, MirInstrStore *);
  tmp->src           = src;
  tmp->dest          = dest;
  ref_instr(&tmp->base);

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_try_infer(Context *cnt, Ast *node, MirInstr *src, MirInstr *dest)
{
  assert(src && dest);
  ref_instr(src);
  ref_instr(dest);
  MirInstrTryInfer *tmp = create_instr(cnt, MIR_INSTR_TRY_INFER, node, MirInstrTryInfer *);
  tmp->src              = src;
  tmp->dest             = dest;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_binop(Context *cnt, Ast *node, MirInstr *lhs, MirInstr *rhs, BinopKind op)
{
  assert(lhs && rhs);
  ref_instr(lhs);
  ref_instr(rhs);
  MirInstrBinop *tmp = create_instr(cnt, MIR_INSTR_BINOP, node, MirInstrBinop *);
  tmp->lhs           = lhs;
  tmp->rhs           = rhs;
  tmp->op            = op;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_unop(Context *cnt, Ast *node, MirInstr *instr, UnopKind op)
{
  assert(instr);
  ref_instr(instr);
  MirInstrUnop *tmp = create_instr(cnt, MIR_INSTR_UNOP, node, MirInstrUnop *);
  tmp->instr        = instr;
  tmp->op           = op;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

MirInstr *
append_instr_validate_type(Context *cnt, MirInstr *src)
{
  assert(src);
  ref_instr(src);
  MirInstrValidateType *tmp =
      create_instr(cnt, MIR_INSTR_VALIDATE_TYPE, NULL, MirInstrValidateType *);
  tmp->src             = src;
  tmp->base.value.type = cnt->buildin_types.entry_void;

  push_into_curr_block(cnt, &tmp->base);
  return &tmp->base;
}

/* LLVM */
LLVMTypeRef
to_llvm_type(Context *cnt, MirType *type, size_t *out_size, unsigned *out_alignment)
{
  if (!type) return NULL;
  LLVMTypeRef result = NULL;

  switch (type->kind) {
  case MIR_TYPE_TYPE:
  case MIR_TYPE_VOID: {
    if (out_size) *out_size = 0;
    if (out_alignment) *out_alignment = 0;
    result = LLVMVoidTypeInContext(cnt->module->llvm_cnt);
    break;
  }

  case MIR_TYPE_INT: {
    result = LLVMIntTypeInContext(cnt->module->llvm_cnt, (unsigned int)type->data.integer.bitcount);
    if (out_size) *out_size = LLVMSizeOfTypeInBits(cnt->module->llvm_td, result);
    if (out_alignment) *out_alignment = LLVMABIAlignmentOfType(cnt->module->llvm_td, result);
    break;
  }

  case MIR_TYPE_BOOL: {
    result = LLVMIntTypeInContext(cnt->module->llvm_cnt, 1);
    if (out_size) *out_size = LLVMSizeOfTypeInBits(cnt->module->llvm_td, result);
    if (out_alignment) *out_alignment = LLVMABIAlignmentOfType(cnt->module->llvm_td, result);
    break;
  }

  case MIR_TYPE_PTR: {
    MirType *tmp = type->data.ptr.next;
    assert(tmp);
    assert(tmp->llvm_type);
    result = LLVMPointerType(tmp->llvm_type, 0);
    if (out_size) *out_size = LLVMSizeOfTypeInBits(cnt->module->llvm_td, result);
    if (out_alignment) *out_alignment = LLVMABIAlignmentOfType(cnt->module->llvm_td, result);
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

    llvm_ret = tmp_ret ? tmp_ret->llvm_type : LLVMVoidTypeInContext(cnt->module->llvm_cnt);
    assert(llvm_ret);

    result = LLVMFunctionType(llvm_ret, llvm_args, (unsigned int)cargs, false);
    if (out_size) *out_size = 0;
    if (out_alignment) *out_alignment = 0;
    bl_free(llvm_args);
    break;
  }

  case MIR_TYPE_ARRAY: {
    LLVMTypeRef llvm_elem_type = type->data.array.elem_type->llvm_type;
    assert(llvm_elem_type);
    const unsigned int len = (const unsigned int)type->data.array.len;

    result = LLVMArrayType(llvm_elem_type, len);
    if (out_size) *out_size = LLVMSizeOfTypeInBits(cnt->module->llvm_td, result);
    if (out_alignment) *out_alignment = LLVMABIAlignmentOfType(cnt->module->llvm_td, result);
    break;
  }

  default:
    bl_unimplemented;
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
analyze_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
  assert(elem_ptr->index);

  MirInstr *arr_ptr = elem_ptr->arr_ptr;
  assert(arr_ptr);
  assert(arr_ptr->value.type);

  assert(is_pointer_type(arr_ptr->value.type));
  MirType *arr_type = arr_ptr->value.type->data.ptr.next;
  assert(arr_type);

  if (arr_type->kind != MIR_TYPE_ARRAY) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, arr_ptr->node->src,
                BUILDER_CUR_WORD, "expected array type");
    return false;
  }

  /* setup ElemPtr instruction value type */
  MirType *elem_type = arr_type->data.array.elem_type;
  assert(elem_type);
  elem_ptr->tmp_value.is_stack_allocated = true;
  elem_ptr->tmp_value.type               = elem_type;
  elem_ptr->base.value.type              = create_type_ptr(cnt, elem_type);

  return true;
}

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

  ref->base.value = scope_entry->instr->value;
  ref->decl       = scope_entry->instr;
  ref_instr(scope_entry->instr);
  return true;
}

bool
analyze_instr_arg(Context *cnt, MirInstrArg *arg)
{
  MirFn *fn = arg->base.owner_block->owner_fn;
  assert(fn);

  BArray *arg_types = fn->type->data.fn.arg_types;
  assert(arg_types && "trying to reference type of argument in function without arguments");

  assert(arg->i < bo_array_size(arg_types));
  arg->base.value.type = bo_array_at(arg_types, arg->i, MirType *);
  assert(arg->base.value.type);

  return true;
}

bool
analyze_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
  /* nothing to do :( */
  return true;
}

bool
analyze_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto, bool comptime)
{
  MirInstrBlock *prev_block = NULL;

  /* resolve type */
  if (!fn_proto->base.value.type) {
    assert(fn_proto->type && fn_proto->type->kind == MIR_INSTR_CALL);
    analyze_instr(cnt, fn_proto->type, true);
    MirValue *type_val = exec_instr(cnt, fn_proto->type);
    unref_instr(fn_proto->type);
    assert(type_val->type && type_val->type->kind == MIR_TYPE_TYPE);

    if (fn_proto->user_type) {
      assert(fn_proto->user_type->kind == MIR_INSTR_CALL);
      analyze_instr(cnt, fn_proto->user_type, true);
      MirValue *user_type_val = exec_instr(cnt, fn_proto->user_type);
      unref_instr(fn_proto->user_type);
      assert(user_type_val->type && user_type_val->type->kind == MIR_TYPE_TYPE);

      if (!type_cmp(type_val->data.v_type, user_type_val->data.v_type)) {
        error_types(cnt, type_val->data.v_type, user_type_val->data.v_type,
                    fn_proto->user_type->node, NULL);
      }
    }

    assert(type_val->data.v_type->kind == MIR_TYPE_FN);
    fn_proto->base.value.type = type_val->data.v_type;
  }

  MirValue *value = &fn_proto->base.value;

  assert(value->type && "function has no valid type");
  assert(value->data.v_fn);
  value->data.v_fn->type = fn_proto->base.value.type;

  MirFn *fn = fn_proto->base.value.data.v_fn;
  assert(fn);

  /* implicit functions has no name -> generate one */
  if (!fn->name) {
    fn->name = gen_uq_name(cnt, IMPL_FN_NAME);
  }

  if (fn->is_external) {
    /* lookup external function exec handle */
    assert(fn->name);
    void *handle = dlFindSymbol(cnt->dl.lib, fn->name);
    assert(handle);

    if (!handle) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNKNOWN_SYMBOL, fn_proto->base.node->src,
                  BUILDER_CUR_WORD, "external symbol '%s' not found", fn->name);
    }

    fn->extern_entry = handle;
  } else {
    prev_block         = get_current_block(cnt);
    MirInstrBlock *tmp = fn->first_block;

    while (tmp) {
      if (comptime)
        analyze_instr_block(cnt, tmp, comptime);
      else {
        /* TODO: first_block can be used as linked list, only first one must be pushed into
         * analyze_stack
         */
        bo_array_push_back(cnt->analyze_stack, tmp);
      }

      tmp = (MirInstrBlock *)tmp->base.next;
    }

    assert(prev_block);
    set_cursor_block(cnt, prev_block);
  }

  /* push function into globals of the mir module */
  bo_array_push_back(cnt->module->globals, fn_proto);
  return true;
}

bool
analyze_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
  assert(br->cond && br->then_block && br->else_block);
  assert(br->cond->analyzed);

  MirType *cond_type = br->cond->value.type;
  assert(cond_type);

  if (!type_cmp(cond_type, cnt->buildin_types.entry_bool)) {
    error_types(cnt, cond_type, cnt->buildin_types.entry_bool, br->cond->node, NULL);
    return false;
  }

  return true;
}

bool
analyze_instr_br(Context *cnt, MirInstrBr *br)
{
  assert(br->then_block);
  return true;
}

bool
analyze_instr_load(Context *cnt, MirInstrLoad *load)
{
  MirInstr *src = load->src;
  assert(src);
  assert(is_pointer_type(src->value.type) && "expected pointer");

  MirType *type = src->value.type->data.ptr.next;
  assert(type);
  load->base.value.type = type;

  return true;
}

bool
analyze_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn)
{
  assert(type_fn->base.value.type);
  assert(type_fn->ret_type ? type_fn->ret_type->analyzed : true);

  return true;
}

bool
analyze_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr)
{
  assert(type_arr->base.value.type);
  assert(type_arr->elem_type->analyzed);

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

  const bool is_logic = ast_binop_is_logic(binop->op);

  if (!type_cmp(lhs->value.type, rhs->value.type)) {
    error_types(cnt, lhs->value.type, rhs->value.type, binop->base.node, NULL);
  } else {
    const MirTypeKind lhs_kind = lhs->value.type->kind;
    const MirTypeKind rhs_kind = rhs->value.type->kind;
    const bool lhs_valid = (lhs_kind == MIR_TYPE_INT) || (lhs_kind == MIR_TYPE_BOOL && is_logic);
    const bool rhs_valid = (rhs_kind == MIR_TYPE_INT) || (rhs_kind == MIR_TYPE_BOOL && is_logic);

    if (!(lhs_valid && rhs_valid)) {
      error_types(cnt, lhs->value.type, rhs->value.type, binop->base.node,
                  "invalid operation for %s type");
    }
  }

  MirType *type = ast_binop_is_logic(binop->op) ? cnt->buildin_types.entry_bool : lhs->value.type;
  assert(type);
  binop->base.value.type = type;

  return true;
}

bool
analyze_instr_unop(Context *cnt, MirInstrUnop *unop)
{
  assert(unop->instr && unop->instr->analyzed);
  MirType *type = unop->instr->value.type;
  assert(type);
  unop->base.value.type = type;
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
  return true;
}

bool
analyze_instr_const(Context *cnt, MirInstrConst *cnst)
{
  assert(cnst->base.value.type);
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

  erase_from_curr_block(cnt, &validate->base);
  return true;
}

bool
analyze_instr_try_infer(Context *cnt, MirInstrTryInfer *infer)
{
  MirInstr *src  = infer->src;
  MirInstr *dest = infer->dest;
  assert(src && dest);
  assert(src->analyzed && dest->analyzed);

  assert(dest->kind == MIR_INSTR_DECL_VAR);
  if (dest->value.type) return true;

  /* set type to decl var and variable */
  dest->value.type = create_type_ptr(cnt, src->value.type);
  MirVar *var      = ((MirInstrDeclVar *)dest)->var;
  assert(var);
  var->value.type = src->value.type;

  assert(dest->value.type);
  erase_from_curr_block(cnt, &infer->base);
  return true;
}

bool
analyze_instr_ret(Context *cnt, MirInstrRet *ret)
{
  /* compare return value with current function type */
  MirInstrBlock *block = get_current_block(cnt);
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
      error_types(cnt, value->value.type, fn_type->data.fn.ret_type, ret->value->node, NULL);
    }
  } else if (expected_ret_value) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_EXPR, ret->base.node->src,
                BUILDER_CUR_AFTER, "expected return value");
  }

  return true;
}

bool
analyze_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
  if (decl->type) {
    analyze_instr(cnt, decl->type, true);
    MirValue *resolved_type_value = exec_instr(cnt, decl->type);
    unref_instr(decl->type);
    assert(resolved_type_value && resolved_type_value->type->kind == MIR_TYPE_TYPE);
    MirType *resolved_type = resolved_type_value->data.v_type;
    assert(resolved_type);

    decl->base.value.type = create_type_ptr(cnt, resolved_type);
    MirVar *var           = decl->var;
    assert(var);
    var->value.type = resolved_type;
  }

  /* TODO: reference can be created later during analyze pass */
  /* TODO: reference can be created later during analyze pass */
  /* TODO: reference can be created later during analyze pass */
  /* TODO: reference can be created later during analyze pass */
  /*
  if (decl->base.ref_count == 0) {
    builder_msg(cnt->builder, BUILDER_MSG_WARNING, 0, decl->base.node->src, BUILDER_CUR_WORD,
                "unused declaration");
  }
  */

  return true;
}

bool
analyze_instr_call(Context *cnt, MirInstrCall *call, bool comptime)
{
  assert(call->callee);
  analyze_instr(cnt, call->callee, call->base.comptime || comptime);

  MirType *type = call->callee->value.type;
  assert(type && "invalid type of called object");

  if (is_pointer_type(type)) {
    /* we want to make calls also via pointer to functions so in such case we need to resolve
     * pointed function */
    type = type->data.ptr.next;
  }

  if (type->kind != MIR_TYPE_FN) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_FUNC, call->callee->node->src,
                BUILDER_CUR_WORD, "expected a function name");
    return false;
  }

  MirType *result_type = type->data.fn.ret_type;
  assert(result_type && "invalid type of call result");
  call->base.value.type = result_type;

  /* validate arguments */
  const size_t callee_argc = type->data.fn.arg_types ? bo_array_size(type->data.fn.arg_types) : 0;
  const size_t call_argc   = call->args ? bo_array_size(call->args) : 0;
  if (callee_argc != call_argc) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_ARG_COUNT, call->base.node->src,
                BUILDER_CUR_WORD, "expected %u %s, but called with %u", callee_argc,
                callee_argc == 1 ? "argument" : "arguments", call_argc);
    return false;
  }

  /* validate argument types */
  if (call_argc) {
    MirInstr *call_arg;
    MirType * callee_arg_type;
    barray_foreach(call->args, call_arg)
    {
      callee_arg_type = bo_array_at(type->data.fn.arg_types, i, MirType *);
      if (!type_cmp(call_arg->value.type, callee_arg_type)) {
        error_types(cnt, call_arg->value.type, callee_arg_type, call_arg->node, NULL);
      }
    }
  }

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

  MirType *dest_type = dest->value.type->data.ptr.next;
  assert(dest_type && "store destination has invalid base type");

  if (!type_cmp(src->value.type, dest_type)) {
    error_types(cnt, src->value.type, dest_type, src->node, NULL);
  }

  /* store implicitly yields void value */
  store->base.value.type = cnt->buildin_types.entry_void;

  return true;
}

bool
analyze_instr_block(Context *cnt, MirInstrBlock *block, bool comptime)
{
  assert(block);
  set_cursor_block(cnt, block);

  /* append implicit return for void functions or generate error when last block is not terminated
   */
  if (!is_block_terminated(block)) {
    MirFn *fn = block->owner_fn;
    assert(fn);

    if (fn->type->data.fn.ret_type->kind == MIR_TYPE_VOID) {
      append_instr_ret(cnt, NULL, NULL);
    } else {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_MISSING_RETURN, fn->node->src,
                  BUILDER_CUR_WORD, "not every path inside function return value");
    }
  }

  /* iterate over entry block of executable */
  MirInstr *instr = block->entry_instr;
  MirInstr *next  = NULL;

  while (instr) {
    next = instr->next;
    if (!analyze_instr(cnt, instr, comptime)) return false;
    instr = next;
  }

  return true;
}

bool
analyze_instr(Context *cnt, MirInstr *instr, bool comptime)
{
  if (!instr) return NULL;

  /* skip already analyzed instructions */
  if (instr->analyzed) return instr;
  instr->analyzed = true;
  bool state;

  switch (instr->kind) {
  case MIR_INSTR_BLOCK:
    state = analyze_instr_block(cnt, (MirInstrBlock *)instr, comptime);
    break;
  case MIR_INSTR_FN_PROTO:
    state = analyze_instr_fn_proto(cnt, (MirInstrFnProto *)instr, comptime);
    break;
  case MIR_INSTR_DECL_VAR:
    state = analyze_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
    break;
  case MIR_INSTR_CALL:
    state = analyze_instr_call(cnt, (MirInstrCall *)instr, comptime);
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
  case MIR_INSTR_TYPE_ARRAY:
    state = analyze_instr_type_array(cnt, (MirInstrTypeArray *)instr);
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
  case MIR_INSTR_BR:
    state = analyze_instr_br(cnt, (MirInstrBr *)instr);
    break;
  case MIR_INSTR_COND_BR:
    state = analyze_instr_cond_br(cnt, (MirInstrCondBr *)instr);
    break;
  case MIR_INSTR_UNOP:
    state = analyze_instr_unop(cnt, (MirInstrUnop *)instr);
    break;
  case MIR_INSTR_UNREACHABLE:
    state = analyze_instr_unreachable(cnt, (MirInstrUnreachable *)instr);
    break;
  case MIR_INSTR_ARG:
    state = analyze_instr_arg(cnt, (MirInstrArg *)instr);
    break;
  case MIR_INSTR_ELEM_PTR:
    state = analyze_instr_elem_ptr(cnt, (MirInstrElemPtr *)instr);
    break;
  default:
    msg_warning("missing analyze for %s", mir_instr_name(instr));
    return false;
  }

  return state;
}

void
analyze(Context *cnt)
{
  BArray *  stack = cnt->analyze_stack;
  MirInstr *instr;

  if (cnt->verbose_pre) {
    barray_foreach(stack, instr)
    {
      /* Print verbose information before analyze */
      if (instr->kind == MIR_INSTR_FN_PROTO) mir_print_instr(instr, stdout);
    }
  }

  barray_foreach(stack, instr)
  {
    assert(instr);
    analyze_instr(cnt, instr, false);
  }

  if (cnt->verbose_post) {
    barray_foreach(stack, instr)
    {
      /* Print verbose information before analyze */
      if (instr->kind == MIR_INSTR_FN_PROTO) mir_print_instr(instr, stdout);
    }
  }
}

/* executing */
void
exec_print_call_stack(Context *cnt, size_t max_nesting)
{
  MirInstr *instr;
  size_t    n = 0;
  for (size_t i = bo_array_size(cnt->exec.call_stack); i-- > 0; ++n) {
    if (max_nesting && n == max_nesting) {
      msg_note("continue...");
      break;
    }

    instr = bo_array_at(cnt->exec.call_stack, i, MirInstr *);
    builder_msg(cnt->builder, BUILDER_MSG_LOG, 0, instr->node->src, BUILDER_CUR_WORD, "");
  }
}

MirValue *
exec_instr(Context *cnt, MirInstr *instr)
{
  if (!instr) return NULL;
  if (!instr->analyzed) {
#if BL_DEBUG
    bl_abort("instruction %s (%llu) has not been analyzed!", mir_instr_name(instr), instr->_serial);
#else
    bl_abort("instruction %s has not been analyzed!", mir_instr_name(instr));
#endif
  }

#if 0
  /* step-by-step execution */
  {
    mir_print_instr(instr);
    getchar();
  }
#endif

  switch (instr->kind) {
  case MIR_INSTR_CONST:
    return exec_instr_const(cnt, (MirInstrConst *)instr);
  case MIR_INSTR_BINOP:
    return exec_instr_binop(cnt, (MirInstrBinop *)instr);
  case MIR_INSTR_UNOP:
    return exec_instr_unop(cnt, (MirInstrUnop *)instr);
  case MIR_INSTR_CALL:
    return exec_instr_call(cnt, (MirInstrCall *)instr);
  case MIR_INSTR_RET:
    return exec_instr_ret(cnt, (MirInstrRet *)instr);
  case MIR_INSTR_TYPE_FN:
    return exec_instr_type_fn(cnt, (MirInstrTypeFn *)instr);
  case MIR_INSTR_TYPE_ARRAY:
    return exec_instr_type_array(cnt, (MirInstrTypeArray *)instr);
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
  case MIR_INSTR_BR:
    return exec_instr_br(cnt, (MirInstrBr *)instr);
  case MIR_INSTR_COND_BR:
    return exec_instr_cond_br(cnt, (MirInstrCondBr *)instr);
  case MIR_INSTR_UNREACHABLE:
    return exec_instr_unreachable(cnt, (MirInstrUnreachable *)instr);
  case MIR_INSTR_ARG:
    return exec_instr_arg(cnt, (MirInstrArg *)instr);
  case MIR_INSTR_ELEM_PTR:
    return exec_instr_elem_ptr(cnt, (MirInstrElemPtr *)instr);

  default:
    bl_abort("missing execution for instruction: %s", mir_instr_name(instr));
  }

  return NULL;
}

MirValue *
exec_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
  assert(elem_ptr->arr_ptr && elem_ptr->index);
  assert(is_pointer_type(elem_ptr->arr_ptr->value.type));

  MirValue *arr_value = elem_ptr->arr_ptr->value.data.v_ptr;
  assert(is_array_type(arr_value->type));
  assert(arr_value->is_stack_allocated);

  // const size_t elem_size = store_sizeof_type_in_bytes(arr_value->type->data.array.elem_type);
  MirValue *elem_value = &elem_ptr->base.value;
  /* TODO: shift pointer by index */
  elem_ptr->tmp_value.data = arr_value->data;
  elem_value->data.v_ptr   = &elem_ptr->tmp_value;

  return elem_value;
}

MirValue *
exec_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
  msg_error("execution reached unreachable code");
  exec_print_call_stack(cnt, 0);
  cnt->exec.aborted = true;
  return NULL;
}

MirValue *
exec_instr_br(Context *cnt, MirInstrBr *br)
{
  assert(br->then_block);
  *exec_call_stack_get(cnt) = br->then_block->entry_instr;
  return &br->base.value;
}

MirValue *
exec_instr_arg(Context *cnt, MirInstrArg *arg)
{
  MirFn *fn = arg->base.owner_block->owner_fn;
  assert(fn);

  BArray *arg_slots = fn->arg_slots;
  assert(arg_slots);
  assert(arg->i < bo_array_size(arg_slots));

  MirValue *val                      = bo_array_at(arg_slots, arg->i, MirValue *);
  arg->base.value.is_stack_allocated = val->is_stack_allocated;
  arg->base.value.data               = val->data;
  return &arg->base.value;
}

MirValue *
exec_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
  assert(br->cond);
  MirValue *cond = &br->cond->value;
  assert(cond->type);

  if (exec_read_int64(cond)) {
    *exec_call_stack_get(cnt) = br->then_block->entry_instr;
  } else {
    *exec_call_stack_get(cnt) = br->else_block->entry_instr;
  }

  return &br->base.value;
}

MirValue *
exec_instr_addr_of(Context *cnt, MirInstrAddrOf *addrof)
{
  assert(is_pointer_type(addrof->base.value.type));
  return &addrof->base.value;
}

MirValue *
exec_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
  assert(decl->base.value.type);

  MirVar *var = decl->var;
  assert(var);

  /* allocate memory for variable on frame stack */
  exec_frame_stack_alloc(cnt, &var->value);

  return &decl->base.value;
}

MirValue *
exec_instr_load(Context *cnt, MirInstrLoad *load)
{
  assert(load->src);
  assert(is_pointer_type(load->src->value.type));

  assert(!load->src->value.is_stack_allocated);
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
  assert(deref_dest && "invalid pointer reference");

  /* copy from source to destination */
  exec_copy_value(deref_dest, &src->value);

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
    arg_types = bo_array_new(sizeof(MirType *));
    bo_array_reserve(arg_types, bo_array_size(type_fn->arg_types));

    MirInstr *arg_type;
    MirType * tmp;
    barray_foreach(type_fn->arg_types, arg_type)
    {
      tmp = arg_type->value.data.v_type;
      bo_array_push_back(arg_types, tmp);
    }
  }

  type_fn->base.value.data.v_type = create_type_fn(cnt, ret_type, arg_types);
  return &type_fn->base.value;
}

MirValue *
exec_instr_type_array(Context *cnt, MirInstrTypeArray *type_arr)
{
  MirType *elem_type = type_arr->elem_type->value.data.v_type;
  size_t   len       = type_arr->len->value.data.v_int;

  type_arr->base.value.data.v_type = create_type_array(cnt, elem_type, len);
  return &type_arr->base.value;
}

MirValue *
exec_instr_const(Context *cnt, MirInstrConst *cnst)
{
  assert(cnst->base.value.type);
  return &cnst->base.value;
}

static inline void
exec_fn_push_dc_arg(Context *cnt, MirValue *val)
{
  MirType *type = val->type;
  assert(type);

  switch (type->kind) {
  case MIR_TYPE_INT: {
    switch (type->data.integer.bitcount) {
    case 64:
      dcArgLongLong(cnt->dl.vm, val->data.v_int);
      break;
    case 32:
      dcArgInt(cnt->dl.vm, (DCint)val->data.v_int);
      break;
    case 16:
      dcArgShort(cnt->dl.vm, (DCshort)val->data.v_int);
      break;
    case 8:
      dcArgChar(cnt->dl.vm, (DCchar)val->data.v_int);
      break;
    default:
      bl_abort("unsupported external call integer argument type");
    }
    break;
  }

  default:
    bl_abort("unsupported external call argument type");
  }
}

bool
exec_fn(Context *cnt, MirFn *fn, BArray *args, MirValue *out_value)
{
  assert(fn);
  MirType *ret_type = fn->type->data.fn.ret_type;
  assert(ret_type);
  const bool does_return_value = ret_type->kind != MIR_TYPE_VOID;

  /* Store previous frame stack location pointer, this can be used later for rollback. This pointer
   */
  MirFrameStackPtr frame_ptr = exec_frame_stack_get_ptr(cnt);

  /* store return frame pointer */
  fn->exec_ret_value = out_value;

  if (fn->is_external) {
    assert(fn->extern_entry);
    dcMode(cnt->dl.vm, DC_CALL_C_DEFAULT);
    dcReset(cnt->dl.vm);

    if (args) {
      MirInstr *arg;
      barray_foreach(args, arg)
      {
        exec_fn_push_dc_arg(cnt, &arg->value);
      }
    }

    switch (ret_type->kind) {
    case MIR_TYPE_INT:
      switch (ret_type->data.integer.bitcount) {
      case sizeof(char) * 8:
        out_value->data.v_int = dcCallChar(cnt->dl.vm, fn->extern_entry);
        break;
      case sizeof(short) * 8:
        out_value->data.v_int = dcCallShort(cnt->dl.vm, fn->extern_entry);
        break;
      case sizeof(int) * 8:
        out_value->data.v_int = dcCallInt(cnt->dl.vm, fn->extern_entry);
        break;
      case sizeof(long long) * 8:
        out_value->data.v_int = dcCallLongLong(cnt->dl.vm, fn->extern_entry);
        break;
      default:
        bl_abort("unsupported integer size for external call result");
      }
      break;

    default:
      bl_abort("unsupported external call return type");
    }
  } else {
    /* copy arguments into arg slots of the executed function */
    if (args) {
      MirInstr *arg;
      MirValue *arg_val;
      barray_foreach(args, arg)
      {
        arg_val = &arg->value;
        bo_array_push_back(fn->arg_slots, arg_val);
      }
    }

    if (!fn->first_block->entry_instr) return does_return_value;
    exec_call_stack_push(cnt, fn->first_block->entry_instr);

    /* iterate over entry block of executable */
    MirInstr *instr, *prev;
    while (true) {
      instr = *exec_call_stack_get(cnt);
      prev  = instr;
      if (!instr || cnt->exec.aborted) break;

      exec_instr(cnt, instr);

      /* stack head can be changed by br instructions */
      if (*exec_call_stack_get(cnt) == prev) *exec_call_stack_get(cnt) = instr->next;
    }

    /* cleanup */
    exec_call_stack_pop(cnt);
    if (fn->arg_slots) bo_array_clear(fn->arg_slots);
  }

  /* do frame stack rollback */
  exec_frame_stack_rollback(cnt, frame_ptr);
  return does_return_value;
}

MirValue *
exec_instr_call(Context *cnt, MirInstrCall *call)
{
  assert(call->callee && call->base.value.type);
  MirValue *callee_val = &call->callee->value;
  assert(callee_val->type && callee_val->type->kind == MIR_TYPE_FN);

  MirFn *fn = callee_val->data.v_fn;
  exec_fn(cnt, fn, call->args, &call->base.value);
  return &call->base.value;
}

MirValue *
exec_instr_ret(Context *cnt, MirInstrRet *ret)
{
  MirFn *fn = ret->base.owner_block->owner_fn;
  assert(fn);

  /* return from function with void return type */
  if (!ret->value) return NULL;
  MirValue *val = &ret->value->value;

  /* set fn execution resulting instruction */
  *fn->exec_ret_value = *val;
  return &ret->base.value;
}

/* INT MATH */
static inline void
exec_math_add(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  dest->data.v_int = exec_read_int64(lhs) + exec_read_int64(rhs);
}

static inline void
exec_math_sub(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  dest->data.v_int = exec_read_int64(lhs) - exec_read_int64(rhs);
}

static inline void
exec_math_mul(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  dest->data.v_int = exec_read_int64(lhs) * exec_read_int64(rhs);
}

static inline void
exec_math_div(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest, Src *rhs_src)
{
  if (exec_read_int64(rhs) == 0) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_DIV_BY_ZERO, rhs_src, BUILDER_CUR_WORD,
                "division by zero");

    dest->data.v_int = 0;
    return;
  }

  dest->data.v_int = exec_read_int64(lhs) / exec_read_int64(rhs);
}

static inline void
exec_math_eq(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  assert(dest->type->kind == MIR_TYPE_BOOL);
  dest->data.v_bool = exec_read_int64(lhs) == exec_read_int64(rhs);
}

static inline void
exec_math_neq(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  assert(dest->type->kind == MIR_TYPE_BOOL);
  dest->data.v_bool = exec_read_int64(lhs) != exec_read_int64(rhs);
}

static inline void
exec_math_less(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  assert(dest->type->kind == MIR_TYPE_BOOL);
  dest->data.v_bool = exec_read_int64(lhs) < exec_read_int64(rhs);
}

static inline void
exec_math_less_eq(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  assert(dest->type->kind == MIR_TYPE_BOOL);
  dest->data.v_bool = exec_read_int64(lhs) <= exec_read_int64(rhs);
}

static inline void
exec_math_greater(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  assert(dest->type->kind == MIR_TYPE_BOOL);
  dest->data.v_bool = exec_read_int64(lhs) > exec_read_int64(rhs);
}

static inline void
exec_math_greater_eq(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  assert(dest->type->kind == MIR_TYPE_BOOL);
  dest->data.v_bool = exec_read_int64(lhs) >= exec_read_int64(rhs);
}

static inline void
exec_math_logic_and(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  assert(dest->type->kind == MIR_TYPE_BOOL);
  dest->data.v_bool = exec_read_int64(lhs) && exec_read_int64(rhs);
}

static inline void
exec_math_logic_or(Context *cnt, MirValue *lhs, MirValue *rhs, MirValue *dest)
{
  assert(dest->type->kind == MIR_TYPE_BOOL);
  dest->data.v_bool = exec_read_int64(lhs) || exec_read_int64(rhs);
}

MirValue *
exec_instr_binop(Context *cnt, MirInstrBinop *binop)
{
  MirValue *lhs    = &binop->lhs->value;
  MirValue *rhs    = &binop->rhs->value;
  MirValue *result = &binop->base.value;

  assert(binop->base.value.type);
  /* sometimes valus are allocated on stack */

  switch (binop->op) {
  case BINOP_ADD:
    exec_math_add(cnt, lhs, rhs, result);
    break;
  case BINOP_SUB:
    exec_math_sub(cnt, lhs, rhs, result);
    break;
  case BINOP_MUL:
    exec_math_mul(cnt, lhs, rhs, result);
    break;
  case BINOP_DIV:
    exec_math_div(cnt, lhs, rhs, result, binop->rhs->node->src);
    break;
  case BINOP_EQ:
    exec_math_eq(cnt, lhs, rhs, result);
    break;
  case BINOP_NEQ:
    exec_math_neq(cnt, lhs, rhs, result);
    break;
  case BINOP_LESS:
    exec_math_less(cnt, lhs, rhs, result);
    break;
  case BINOP_LESS_EQ:
    exec_math_less_eq(cnt, lhs, rhs, result);
    break;
  case BINOP_GREATER:
    exec_math_greater(cnt, lhs, rhs, result);
    break;
  case BINOP_GREATER_EQ:
    exec_math_greater_eq(cnt, lhs, rhs, result);
    break;
  case BINOP_LOGIC_AND:
    exec_math_logic_and(cnt, lhs, rhs, result);
    break;
  case BINOP_LOGIC_OR:
    exec_math_logic_or(cnt, lhs, rhs, result);
    break;
  default:
    bl_unimplemented;
  }

  return result;
}

MirValue *
exec_instr_unop(Context *cnt, MirInstrUnop *unop)
{
  assert(unop->base.value.type);
  switch (unop->op) {
  case UNOP_NEG:
    unop->base.value.data.v_int = unop->instr->value.data.v_int * -1;
    break;
  case UNOP_POS:
    unop->base.value.data.v_int = unop->instr->value.data.v_int;
    break;
  case UNOP_NOT:
    unop->base.value.data.v_int = !unop->instr->value.data.v_int;
    break;
  default:
    bl_unimplemented;
  }

  return &unop->base.value;
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
ast_test_case(Context *cnt, Ast *test)
{
  /* build test function */
  Ast *ast_block = test->data.test_case.block;
  assert(ast_block);

  MirInstrFnProto *fn_proto = (MirInstrFnProto *)append_instr_fn_proto(cnt, test, NULL, NULL);
  if (cnt->builder->flags & BUILDER_FORCE_TEST_LLVM) ref_instr(&fn_proto->base);

  fn_proto->base.value.type = cnt->buildin_types.entry_test_case_fn;

  MirInstrBlock *prev_block = get_current_block(cnt);
  MirFn *        fn =
      create_fn(cnt, test, TEST_CASE_FN_NAME, NULL, false, true); /* TODO: based on user flag!!! */
  fn->node = test;

  assert(test->data.test_case.desc);
  fn->test_case_desc             = test->data.test_case.desc;
  fn_proto->base.value.data.v_fn = fn;

  bo_array_push_back(cnt->test_cases, fn);

  append_block(cnt, fn_proto->base.value.data.v_fn, "init");
  MirInstrBlock *entry_block = append_block(cnt, fn_proto->base.value.data.v_fn, "entry");
  set_cursor_block(cnt, entry_block);

  /* generate body instructions */
  ast(cnt, ast_block);

  /* terminate initialization block */
  set_cursor_block(cnt, fn->first_block);
  append_instr_br(cnt, NULL, entry_block);

  set_cursor_block(cnt, prev_block);
}

void
ast_unrecheable(Context *cnt, Ast *unr)
{
  append_instr_unrecheable(cnt, unr);
}

void
ast_stmt_if(Context *cnt, Ast *stmt_if)
{
  Ast *ast_cond = stmt_if->data.stmt_if.test;
  Ast *ast_then = stmt_if->data.stmt_if.true_stmt;
  Ast *ast_else = stmt_if->data.stmt_if.false_stmt;
  assert(ast_cond && ast_then);

  MirFn *fn = get_current_fn(cnt);
  assert(fn);

  MirInstrBlock *tmp_block  = NULL;
  MirInstrBlock *then_block = append_block(cnt, fn, "if_then");
  MirInstrBlock *else_block = append_block(cnt, fn, "if_else");
  MirInstrBlock *cont_block = append_block(cnt, fn, "if_cont");

  MirInstr *cond = ast(cnt, ast_cond);
  cond           = append_instr_load_if_needed(cnt, cond);
  append_instr_cond_br(cnt, stmt_if, cond, then_block, else_block);

  /* then block */
  set_cursor_block(cnt, then_block);
  ast(cnt, ast_then);

  tmp_block = get_current_block(cnt);
  if (!get_block_terminator(tmp_block)) {
    /* block has not been terminated -> add terminator */
    append_instr_br(cnt, NULL, cont_block);
  }

  /* else if */
  if (ast_else) {
    set_cursor_block(cnt, else_block);
    ast(cnt, ast_else);

    tmp_block = get_current_block(cnt);
    if (!is_block_terminated(tmp_block)) append_instr_br(cnt, NULL, cont_block);
  }

  if (!is_block_terminated(else_block)) {
    /* block has not been terminated -> add terminator */
    set_cursor_block(cnt, else_block);
    append_instr_br(cnt, NULL, cont_block);
  }

  set_cursor_block(cnt, cont_block);
}

void
ast_stmt_loop(Context *cnt, Ast *loop)
{
  Ast *ast_block     = loop->data.stmt_loop.block;
  Ast *ast_cond      = loop->data.stmt_loop.condition;
  Ast *ast_increment = loop->data.stmt_loop.increment;
  Ast *ast_init      = loop->data.stmt_loop.init;
  assert(ast_block);

  MirFn *fn = get_current_fn(cnt);
  assert(fn);

  /* prepare all blocks */
  MirInstrBlock *tmp_block       = NULL;
  MirInstrBlock *increment_block = ast_increment ? append_block(cnt, fn, "loop_increment") : NULL;
  MirInstrBlock *decide_block    = append_block(cnt, fn, "loop_decide");
  MirInstrBlock *body_block      = append_block(cnt, fn, "loop_body");
  MirInstrBlock *cont_block      = append_block(cnt, fn, "loop_continue");

  MirInstrBlock *prev_break_block    = cnt->break_block;
  MirInstrBlock *prev_continue_block = cnt->continue_block;
  cnt->break_block                   = cont_block;
  cnt->continue_block                = ast_increment ? increment_block : decide_block;

  /* generate initialization if there is one */
  if (ast_init) {
    ast(cnt, ast_init);
  }

  /* decide block */
  append_instr_br(cnt, NULL, decide_block);
  set_cursor_block(cnt, decide_block);

  MirInstr *cond = ast_cond ? append_instr_load_if_needed(cnt, ast(cnt, ast_cond))
                            : append_instr_const_bool(cnt, NULL, true);

  append_instr_cond_br(cnt, ast_cond, cond, body_block, cont_block);

  /* loop body */
  set_cursor_block(cnt, body_block);
  ast(cnt, ast_block);

  tmp_block = get_current_block(cnt);
  if (!is_block_terminated(tmp_block)) {
    append_instr_br(cnt, NULL, ast_increment ? increment_block : decide_block);
  }

  /* increment if there is one */
  if (ast_increment) {
    set_cursor_block(cnt, increment_block);
    ast(cnt, ast_increment);
    append_instr_br(cnt, NULL, decide_block);
  }

  cnt->break_block    = prev_break_block;
  cnt->continue_block = prev_continue_block;
  set_cursor_block(cnt, cont_block);
}

void
ast_stmt_break(Context *cnt, Ast *br)
{
  assert(cnt->break_block && "break statement outside the loop");
  append_instr_br(cnt, br, cnt->break_block);
}

void
ast_stmt_continue(Context *cnt, Ast *cont)
{
  assert(cnt->continue_block && "break statement outside the loop");
  append_instr_br(cnt, cont, cnt->continue_block);
}

void
ast_stmt_return(Context *cnt, Ast *ret)
{
  MirInstr *value = ast(cnt, ret->data.stmt_return.expr);
  value           = append_instr_load_if_needed(cnt, value);
  append_instr_ret(cnt, ret, value);
}

MirInstr *
ast_expr_lit_int(Context *cnt, Ast *expr)
{
  return append_instr_const_int(cnt, expr, expr->data.expr_integer.val);
}

MirInstr *
ast_expr_lit_bool(Context *cnt, Ast *expr)
{
  return append_instr_const_bool(cnt, expr, expr->data.expr_boolean.val);
}

MirInstr *
ast_expr_call(Context *cnt, Ast *call)
{
  Ast *   ast_callee = call->data.expr_call.ref;
  BArray *ast_args   = call->data.expr_call.args;
  assert(ast_callee);

  MirInstr *callee = ast(cnt, ast_callee);
  BArray *  args   = NULL;

  if (ast_args) {
    args = bo_array_new(sizeof(MirInstr *));
    bo_array_reserve(args, bo_array_size(ast_args));
    MirInstr *arg;
    Ast *     ast_arg;
    barray_foreach(ast_args, ast_arg)
    {
      arg = ast(cnt, ast_arg);
      arg = append_instr_load_if_needed(cnt, arg);
      bo_array_push_back(args, arg);
    }
  }

  return append_instr_call(cnt, call, callee, args);
}

MirInstr *
ast_expr_ref(Context *cnt, Ast *ref)
{
  Ast *ident = ref->data.expr_ref.ident;
  assert(ident);
  /* referenced declaration will be resolved later during analyze pass */
  return append_instr_decl_ref(cnt, ident, NULL);
}

MirInstr *
ast_expr_elem(Context *cnt, Ast *elem)
{
  Ast *ast_arr   = elem->data.expr_elem.next;
  Ast *ast_index = elem->data.expr_elem.index;
  assert(ast_arr && ast_index);

  MirInstr *arr_ptr = ast(cnt, ast_arr);
  MirInstr *index   = ast(cnt, ast_index);

  return append_instr_elem_ptr(cnt, elem, arr_ptr, index);
}

MirInstr *
ast_expr_lit_fn(Context *cnt, Ast *lit_fn)
{
  /* creates function prototype */
  Ast *ast_block   = lit_fn->data.expr_fn.block;
  Ast *ast_fn_type = lit_fn->data.expr_fn.type;

  MirInstrFnProto *fn_proto = (MirInstrFnProto *)append_instr_fn_proto(cnt, lit_fn, NULL, NULL);

  fn_proto->type = ast_create_type_resolver_call(cnt, ast_fn_type);
  assert(fn_proto->type);

  MirInstrBlock *prev_block = get_current_block(cnt);
  MirFn *        fn =
      create_fn(cnt, lit_fn, NULL, NULL, !ast_block, false); /* TODO: based on user flag!!! */
  fn_proto->base.value.data.v_fn = fn;

  /* function body */

  /* external functions has no body */
  if (fn->is_external) return &fn_proto->base;

  /* create block for initialization locals and arguments */
  MirInstrBlock *init_block = append_block(cnt, fn_proto->base.value.data.v_fn, "init");
  set_cursor_block(cnt, init_block);

  /* create arg slots for the function */
  {
    BArray *ast_args  = ast_fn_type->data.type_fn.args;
    BArray *arg_slots = NULL;
    if (ast_args) {
      arg_slots = bo_array_new(sizeof(MirValue *));
      bo_array_reserve(arg_slots, bo_array_size(ast_args));

      Ast *ast_arg;
      Ast *ast_arg_name;
      barray_foreach(ast_args, ast_arg)
      {
        assert(ast_arg->kind == AST_DECL_ARG);
        ast_arg_name = ast_arg->data.decl.name;
        assert(ast_arg_name);

        /* create tmp declaration for arg variable */
        MirInstr *var = append_instr_decl_var(cnt, NULL, ast_arg_name);
        MirInstr *arg = append_instr_arg(cnt, NULL, i);
        append_instr_try_infer(cnt, NULL, arg, var);
        append_instr_store(cnt, NULL, arg, var);

        /* registrate argument into scope */
        Scope *scope = ast_arg_name->data.ident.scope;
        assert(scope);
        ScopeEntry *scope_entry = scope_lookup(scope, ast_arg_name->data.ident.hash, true);
        assert(scope_entry && "declaration has no scope entry");
        scope_entry->instr = var;
      }
    }
    fn->arg_slots = arg_slots;
  }

  MirInstrBlock *entry_block = append_block(cnt, fn_proto->base.value.data.v_fn, "entry");
  set_cursor_block(cnt, entry_block);

  /* generate body instructions */
  ast(cnt, ast_block);

  /* terminate initialization block */
  set_cursor_block(cnt, fn->first_block);
  append_instr_br(cnt, NULL, entry_block);

  set_cursor_block(cnt, prev_block);
  return &fn_proto->base;
}

MirInstr *
ast_expr_lit_string(Context *cnt, Ast *lit_string)
{
  const char *cstr = lit_string->data.expr_string.val;
  assert(cstr);
  return append_instr_const_string(cnt, lit_string, cstr);
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

  rhs = append_instr_load_if_needed(cnt, rhs);

  const BinopKind op = binop->data.expr_binop.kind;
  if (ast_binop_is_assign(op)) {
    switch (op) {

    case BINOP_ASSIGN: {
      return append_instr_store(cnt, binop, rhs, lhs);
    }

    case BINOP_ADD_ASSIGN: {
      MirInstr *lhs_val = append_instr_load_if_needed(cnt, lhs);
      MirInstr *rhs_val = append_instr_load_if_needed(cnt, rhs);
      MirInstr *add_tmp = append_instr_binop(cnt, binop, lhs_val, rhs_val, BINOP_ADD);
      return append_instr_store(cnt, binop, add_tmp, lhs);
    }

    case BINOP_SUB_ASSIGN: {
      MirInstr *lhs_val = append_instr_load_if_needed(cnt, lhs);
      MirInstr *rhs_val = append_instr_load_if_needed(cnt, rhs);
      MirInstr *add_tmp = append_instr_binop(cnt, binop, lhs_val, rhs_val, BINOP_SUB);
      return append_instr_store(cnt, binop, add_tmp, lhs);
    }

    case BINOP_MUL_ASSIGN: {
      MirInstr *lhs_val = append_instr_load_if_needed(cnt, lhs);
      MirInstr *rhs_val = append_instr_load_if_needed(cnt, rhs);
      MirInstr *add_tmp = append_instr_binop(cnt, binop, lhs_val, rhs_val, BINOP_MUL);
      return append_instr_store(cnt, binop, add_tmp, lhs);
    }

    case BINOP_DIV_ASSIGN: {
      MirInstr *lhs_val = append_instr_load_if_needed(cnt, lhs);
      MirInstr *rhs_val = append_instr_load_if_needed(cnt, rhs);
      MirInstr *add_tmp = append_instr_binop(cnt, binop, lhs_val, rhs_val, BINOP_DIV);
      return append_instr_store(cnt, binop, add_tmp, lhs);
    }

    case BINOP_MOD_ASSIGN: {
      MirInstr *lhs_val = append_instr_load_if_needed(cnt, lhs);
      MirInstr *rhs_val = append_instr_load_if_needed(cnt, rhs);
      MirInstr *add_tmp = append_instr_binop(cnt, binop, lhs_val, rhs_val, BINOP_MOD);
      return append_instr_store(cnt, binop, add_tmp, lhs);
    }

    default:
      bl_unimplemented;
    }
  } else {
    lhs = append_instr_load_if_needed(cnt, lhs);
    return append_instr_binop(cnt, binop, lhs, rhs, op);
  }
}

MirInstr *
ast_expr_unary(Context *cnt, Ast *unop)
{
  Ast *ast_next = unop->data.expr_unary.next;
  assert(ast_next);

  MirInstr *next = ast(cnt, ast_next);
  assert(next);

  next = append_instr_load_if_needed(cnt, next);
  return append_instr_unop(cnt, unop, next, unop->data.expr_unary.kind);
}

MirInstr *
ast_decl_entity(Context *cnt, Ast *entity)
{
  MirInstr *result    = NULL;
  Ast *     ast_name  = entity->data.decl.name;
  Ast *     ast_type  = entity->data.decl.type;
  Ast *     ast_value = entity->data.decl_entity.value;

  /* Prepare scope entry created in parser */
  assert(ast_name->kind == AST_IDENT);
  Scope *scope = ast_name->data.ident.scope;
  assert(scope);
  ScopeEntry *scope_entry = scope_lookup(scope, ast_name->data.ident.hash, true);
  assert(scope_entry && "declaration has no scope entry");

  MirInstr *value = ast(cnt, ast_value);

  if (value && value->kind == MIR_INSTR_FN_PROTO) {
    value->value.data.v_fn->name = ast_name->data.ident.str;
    value->node                  = ast_name;
    scope_entry->instr           = value;

    if (ast_type) {
      ((MirInstrFnProto *)value)->user_type = ast_create_type_resolver_call(cnt, ast_type);
    }

    /* check main */
    if (is_entry_fn(ast_name)) {
      assert(!cnt->entry_fn);
      cnt->entry_fn = value->value.data.v_fn;
      ref_instr(value);
    }
  } else {
    MirInstr *type = ast_type ? ast_create_type_resolver_call(cnt, ast_type) : NULL;

    MirInstrBlock *prev_block = get_current_block(cnt);
    MirFn *        fn         = get_current_fn(cnt);
    set_cursor_block(cnt, fn->first_block);
    MirInstr *decl = append_instr_decl_var(cnt, type, ast_name);
    set_cursor_block(cnt, prev_block);
    scope_entry->instr = decl;

    /* initialize value */
    if (value) {
      if (!type) append_instr_try_infer(cnt, NULL, value, decl);
      result = append_instr_store(cnt, ast_value, value, decl);
    }

    if (is_entry_fn(ast_name)) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_FUNC, ast_name->src,
                  BUILDER_CUR_WORD, "'main' is expected to be a function");
    }
  }

  return result;
}

MirInstr *
ast_decl_arg(Context *cnt, Ast *arg)
{
  Ast *ast_type = arg->data.decl.type;

  assert(ast_type);
  MirInstr *type = ast(cnt, ast_type);
  return type;
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
    result = append_instr_const_type(cnt, ast_ref, get_buildin(cnt, id));
  }

  assert(result && "unimplemented type ref resolving");
  append_instr_validate_type(cnt, result);
  return result;
}

MirInstr *
ast_type_fn(Context *cnt, Ast *type_fn)
{
  Ast *   ast_ret_type  = type_fn->data.type_fn.ret_type;
  BArray *ast_arg_types = type_fn->data.type_fn.args;

  /* return type */
  MirInstr *ret_type = NULL;
  if (ast_ret_type) {
    ret_type = ast(cnt, ast_ret_type);
    ref_instr(ret_type);
  }

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
      ref_instr(arg_type);
      bo_array_push_back(arg_types, arg_type);
    }
  }

  return append_instr_type_fn(cnt, type_fn, ret_type, arg_types);
}

MirInstr *
ast_type_arr(Context *cnt, Ast *type_arr)
{
  Ast *ast_elem_type = type_arr->data.type_arr.elem_type;
  Ast *ast_len       = type_arr->data.type_arr.len;
  assert(ast_elem_type && ast_len);

  MirInstr *len       = ast(cnt, ast_len);
  MirInstr *elem_type = ast(cnt, ast_elem_type);
  return append_instr_type_array(cnt, type_arr, elem_type, len);
}

MirInstr *
ast_create_type_resolver_call(Context *cnt, Ast *type)
{
  if (!type) return NULL;
  MirInstrBlock *prev_block = get_current_block(cnt);
  MirInstr *     fn         = create_instr_fn_proto(cnt, NULL, NULL, NULL);
  fn->value.type            = cnt->buildin_types.entry_resolve_type_fn;
  fn->value.data.v_fn       = create_fn(cnt, NULL, RESOLVE_TYPE_FN_NAME, NULL, false, false);

  MirInstrBlock *entry = append_block(cnt, fn->value.data.v_fn, "entry");
  set_cursor_block(cnt, entry);

  MirInstr *result = ast(cnt, type);
  append_instr_ret(cnt, NULL, result);

  set_cursor_block(cnt, prev_block);
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
  case AST_TEST_CASE:
    ast_test_case(cnt, node);
    break;
  case AST_UNREACHABLE:
    ast_unrecheable(cnt, node);
    break;
  case AST_STMT_RETURN:
    ast_stmt_return(cnt, node);
    break;
  case AST_STMT_LOOP:
    ast_stmt_loop(cnt, node);
    break;
  case AST_STMT_BREAK:
    ast_stmt_break(cnt, node);
    break;
  case AST_STMT_CONTINUE:
    ast_stmt_continue(cnt, node);
    break;
  case AST_STMT_IF:
    ast_stmt_if(cnt, node);
    break;
  case AST_DECL_ENTITY:
    return ast_decl_entity(cnt, node);
  case AST_DECL_ARG:
    return ast_decl_arg(cnt, node);
  case AST_TYPE_REF:
    return ast_type_ref(cnt, node);
  case AST_TYPE_FN:
    return ast_type_fn(cnt, node);
  case AST_TYPE_ARR:
    return ast_type_arr(cnt, node);
  case AST_EXPR_LIT_INT:
    return ast_expr_lit_int(cnt, node);
  case AST_EXPR_LIT_BOOL:
    return ast_expr_lit_bool(cnt, node);
  case AST_EXPR_LIT_FN:
    return ast_expr_lit_fn(cnt, node);
  case AST_EXPR_LIT_STRING:
    return ast_expr_lit_string(cnt, node);
  case AST_EXPR_BINOP:
    return ast_expr_binop(cnt, node);
  case AST_EXPR_UNARY:
    return ast_expr_unary(cnt, node);
  case AST_EXPR_REF:
    return ast_expr_ref(cnt, node);
  case AST_EXPR_CALL:
    return ast_expr_call(cnt, node);
  case AST_EXPR_ELEM:
    return ast_expr_elem(cnt, node);

  case AST_LOAD:
    break;
  default:
    bl_abort("invalid node %s", ast_get_name(node));
  }

  return NULL;
}

const char *
mir_instr_name(MirInstr *instr)
{
  assert(instr);
  switch (instr->kind) {
  case MIR_INSTR_INVALID:
    return "InstrInvalid";
  case MIR_INSTR_BLOCK:
    return "InstrBlock";
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
  case MIR_INSTR_TYPE_ARRAY:
    return "InstrTypeArray";
  case MIR_INSTR_ADDR_OF:
    return "InstrAddrOf";
  case MIR_INSTR_TRY_INFER:
    return "InstrTryInfer";
  case MIR_INSTR_COND_BR:
    return "InstrCondBr";
  case MIR_INSTR_BR:
    return "InstrBr";
  case MIR_INSTR_UNOP:
    return "InstrUnop";
  case MIR_INSTR_ARG:
    return "InstrArg";
  case MIR_INSTR_ELEM_PTR:
    return "InstrElemPtr";
  }

  return "UNKNOWN";
}

static void
arenas_init(struct MirArenas *arenas)
{
  arena_init(&arenas->instr_arena, sizeof(union _MirInstr), ARENA_CHUNK_COUNT,
             (ArenaElemDtor)instr_dtor);
  arena_init(&arenas->type_arena, sizeof(MirType), ARENA_CHUNK_COUNT, (ArenaElemDtor)type_dtor);
  arena_init(&arenas->var_arena, sizeof(MirVar), ARENA_CHUNK_COUNT, NULL);
  arena_init(&arenas->fn_arena, sizeof(MirFn), ARENA_CHUNK_COUNT, (ArenaElemDtor)fn_dtor);
}

static void
arenas_terminate(struct MirArenas *arenas)
{
  arena_terminate(&arenas->instr_arena);
  arena_terminate(&arenas->type_arena);
  arena_terminate(&arenas->var_arena);
  arena_terminate(&arenas->fn_arena);
}

/* public */
static void
_type_to_str(char *buf, int len, MirType *type)
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
        if (i < bo_array_size(args) - 1) append_buf(buf, len, ", ");
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

  case MIR_TYPE_ARRAY: {
    char str[35];
    sprintf(str, "[%lu]", type->data.array.len);
    append_buf(buf, len, str);

    _type_to_str(buf, len, type->data.array.elem_type);
    break;
  }

  default:
    bl_unimplemented;
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
  msg_log("\nexecuting 'main' in compile time...");
  if (!cnt->entry_fn) {
    msg_error("assembly '%s' has no entry function!", cnt->assembly->name);
    return;
  }

  /* tmp return value storage */
  MirValue result;
  if (exec_fn(cnt, cnt->entry_fn, NULL, &result)) {
    int64_t tmp = exec_read_int64(&result);
    msg_log("execution finished with state: %llu\n", tmp);
  } else {
    msg_log("execution finished\n");
  }
}

void
execute_test_cases(Context *cnt)
{
  msg_log("\nexecuting test cases...");

  const size_t c      = bo_array_size(cnt->test_cases);
  int          failed = 0;
  MirFn *      test_fn;
  int          line;
  const char * file;

  barray_foreach(cnt->test_cases, test_fn)
  {
    cnt->exec.aborted = false;
    assert(test_fn->is_test_case);
    exec_fn(cnt, test_fn, NULL, NULL);

    line = test_fn->node ? test_fn->node->src->line : -1;
    file = test_fn->node ? test_fn->node->src->unit->filepath : "?";

    msg_log("[ %s ] (%lu/%lu) %s:%d '%s'", cnt->exec.aborted ? RED("FAILED") : GREEN("PASSED"),
            i + 1, c, file, line, test_fn->test_case_desc);

    if (cnt->exec.aborted) ++failed;
  }

  msg_log("testing done, %d of %zu failed\n", failed, c);
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
  cnt->buildin_types.entry_u64 =
      create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_U64], 64, false);
  cnt->buildin_types.entry_usize =
      create_type_int(cnt, buildin_type_names[BUILDIN_TYPE_USIZE], 64, false);

  cnt->buildin_types.entry_bool = create_type_bool(cnt);

  cnt->buildin_types.entry_resolve_type_fn =
      create_type_fn(cnt, cnt->buildin_types.entry_type, NULL);

  cnt->buildin_types.entry_test_case_fn = create_type_fn(cnt, cnt->buildin_types.entry_void, NULL);
}

void
init_frame_stack(Context *cnt, size_t size)
{
  if (size == 0) bl_abort("invalid frame stack size");

  char *stack = bl_malloc(sizeof(char) * size);
  if (!stack) bl_abort("bad alloc");

  cnt->exec.frame_stack           = stack;
  cnt->exec.frame_stack_ptr       = stack;
  cnt->exec.frame_stack_allocated = 0;
  cnt->exec.frame_stack_size      = size;
}

void
terminate_frame_stack(Context *cnt)
{
  bl_free(cnt->exec.frame_stack);
}

int
init_dl(Context *cnt)
{
  /* load only current executed workspace */
  DLLib *lib = dlLoadLibrary(NULL);
  if (!lib) {
    msg_error("unable to load library");
    return ERR_LIB_NOT_FOUND;
  }

  DCCallVM *vm = dcNewCallVM(4096);
  dcMode(vm, DC_CALL_C_DEFAULT);
  cnt->dl.lib = lib;
  cnt->dl.vm  = vm;
  return NO_ERR;
}

void
terminate_dl(Context *cnt)
{
  dcFree(cnt->dl.vm);
  dlFreeLibrary(cnt->dl.lib);
}

MirModule *
mir_new_module(const char *name)
{
  MirModule *tmp = bl_malloc(sizeof(MirModule));
  if (!tmp) bl_abort("bad alloc");

  arenas_init(&tmp->arenas);

  tmp->globals     = bo_array_new(sizeof(MirInstr *));
  tmp->llvm_cnt    = LLVMContextCreate();
  tmp->llvm_module = LLVMModuleCreateWithNameInContext(name, tmp->llvm_cnt);
  tmp->llvm_td     = LLVMGetModuleDataLayout(tmp->llvm_module);
  return tmp;
}

void
mir_delete_module(MirModule *module)
{
  if (!module) return;
  bo_unref(module->globals);

  arenas_terminate(&module->arenas);

  LLVMDisposeModule(module->llvm_module);

  bl_free(module);
}

void
mir_run(Builder *builder, Assembly *assembly)
{
  Context cnt;
  memset(&cnt, 0, sizeof(Context));
  cnt.builder         = builder;
  cnt.assembly        = assembly;
  cnt.module          = assembly->mir_module;
  cnt.verbose_pre     = (bool)(builder->flags & BUILDER_VERBOSE_MIR_PRE);
  cnt.verbose_post    = (bool)(builder->flags & BUILDER_VERBOSE_MIR_POST);
  cnt.analyze_stack   = bo_array_new(sizeof(MirInstr *));
  cnt.exec.call_stack = bo_array_new(sizeof(MirInstr *));
  cnt.test_cases      = bo_array_new(sizeof(MirFn *));

  init_frame_stack(&cnt, DEFAULT_EXEC_FRAME_STACK_SIZE);
  init_buildins(&cnt);

  entry_fn_hash = bo_hash_from_str(entry_fn_name);
  bo_array_reserve(cnt.analyze_stack, 1024);

  int error = init_dl(&cnt);
  if (error != NO_ERR) return;

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    ast(&cnt, unit->ast);
  }

  if (!builder->errorc) {
    analyze(&cnt);
  }

  if (!builder->errorc && builder->flags & BUILDER_RUN_TESTS) execute_test_cases(&cnt);
  if (!builder->errorc && builder->flags & BUILDER_RUN) execute_entry_fn(&cnt);

  bo_unref(cnt.buildin_types.table);
  bo_unref(cnt.analyze_stack);
  bo_unref(cnt.exec.call_stack);
  bo_unref(cnt.test_cases);

  terminate_dl(&cnt);
  terminate_frame_stack(&cnt);
}
