//************************************************************************************************
// bl
//
// File:   ir.c
// Author: Martin Dorazil
// Date:   12/7/18
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

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Linker.h>
#include "stages_impl.h"
#include "common_impl.h"
#include "ast_impl.h"

#if BL_DEBUG
#define gname(s) s
#else
#define gname(s) ""
#endif

#define VERBOSE 1
#define PRINT_IR 1

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;

  BHashTable *llvm_values;
  bool        is_gscope;

  LLVMModuleRef  llvm_module;
  LLVMBuilderRef llvm_builder;
  LLVMContextRef llvm_cnt;
  BHashTable *   llvm_modules;

  /* JIT execution engine for #run called methods */
  LLVMExecutionEngineRef llvm_jit;
  BHashTable *           jit_linked;
  bl_node_t *            main_tmp;

  LLVMBasicBlockRef break_block;
  LLVMBasicBlockRef continue_block;
  LLVMBasicBlockRef fn_init_block;
  LLVMBasicBlockRef fn_ret_block;
  LLVMBasicBlockRef fn_entry_block;
  LLVMValueRef      fn_ret_val;
} context_t;

static void
generate(context_t *cnt);

static void
generate_decl(context_t *cnt, bl_node_t *decl);

static LLVMModuleRef
link(context_t *cnt, bl_node_t *entry);

/* resursive version */
static LLVMModuleRef
_link(context_t *cnt, bl_node_t *entry);

static LLVMExecutionEngineRef
create_jit(LLVMContextRef llvm_cnt);

static void
link_into_jit(context_t *cnt, bl_node_t *fn);

/* check if declaration has all it's dependencies already generated in LLVM Modules, by
 * 'strict_only' tag we can check onlu strict dependencies caused by '#run' directive */
static bool
is_satisfied(context_t *cnt, bl_node_t *decl, bool strict_only);

static LLVMGenericValueRef
run(context_t *cnt, bl_node_t *fn);

static LLVMValueRef
ir_decl(context_t *cnt, bl_node_t *decl);

static LLVMValueRef
ir_fn_get(context_t *cnt, bl_node_t *fn);

static LLVMValueRef
ir_global_get(context_t *cnt, bl_node_t *global);

static LLVMValueRef
ir_decl_fn(context_t *cnt, bl_node_t *decl);

static LLVMValueRef
ir_decl_mut(context_t *cnt, bl_node_t *decl);

static LLVMValueRef
ir_decl_immut(context_t *cnt, bl_node_t *decl);

static void
ir_block(context_t *cnt, bl_node_t *block);

static LLVMValueRef
ir_expr(context_t *cnt, bl_node_t *expr);

static LLVMValueRef
ir_lit(context_t *cnt, bl_node_t *lit);

static LLVMValueRef
ir_expr_binop(context_t *cnt, bl_node_t *binop);

static LLVMValueRef
ir_expr_unary(context_t *cnt, bl_node_t *unary);

static LLVMValueRef
ir_expr_call(context_t *cnt, bl_node_t *call);

static LLVMValueRef
ir_expr_call_rt(context_t *cnt, bl_node_t *call);

static LLVMValueRef
ir_expr_call_ct(context_t *cnt, bl_node_t *call);

static LLVMValueRef
ir_expr_member(context_t *cnt, bl_node_t *member);

static LLVMValueRef
ir_expr_elem(context_t *cnt, bl_node_t *elem);

static LLVMValueRef
ir_expr_null(context_t *cnt, bl_node_t *nl);

static LLVMValueRef
ir_ident(context_t *cnt, bl_node_t *ident);

static LLVMValueRef
ir_expr_cast(context_t *cnt, bl_node_t *cast);

static inline LLVMValueRef
ir_expr_sizeof(context_t *cnt, bl_node_t *szof);

static void
ir_stmt_if(context_t *cnt, bl_node_t *stmt_if);

static void
ir_stmt_return(context_t *cnt, bl_node_t *stmt_return);

static void
ir_stmt_loop(context_t *cnt, bl_node_t *loop);

static inline void
ir_stmt_break(context_t *cnt, bl_node_t *brk);

static inline void
ir_stmt_continue(context_t *cnt, bl_node_t *cont);

static LLVMTypeRef
to_llvm_type(context_t *cnt, bl_node_t *type);

// impl
static void
ir_validate(LLVMModuleRef module)
{
  char *error;
  if (LLVMVerifyModule(module, LLVMReturnStatusAction, &error)) {
    char *str = LLVMPrintModuleToString(module);
    bl_abort("module not verified with error: %s\n%s", error, str);
  }
}

static inline void
llvm_values_insert(context_t *cnt, bl_node_t *node, void *val)
{
  bo_htbl_insert(cnt->llvm_values, (uint64_t)node, val);
}

static inline void *
llvm_values_get(context_t *cnt, bl_node_t *node)
{
  if (bo_htbl_has_key(cnt->llvm_values, (uint64_t)node))
    return bo_htbl_at(cnt->llvm_values, (uint64_t)node, void *);
  return NULL;
}

static inline void
llvm_values_reset(context_t *cnt)
{
  bo_htbl_clear(cnt->llvm_values);
}

static inline bool
is_terminated(context_t *cnt)
{
  return (!cnt->is_gscope && LLVMGetInsertBlock((cnt)->llvm_builder) &&
          LLVMGetBasicBlockTerminator(LLVMGetInsertBlock((cnt)->llvm_builder)) != NULL);
}

static inline bool
should_load(bl_node_t *node, LLVMValueRef llvm_value)
{
  if ((bl_node_is(node, BL_NODE_EXPR_UNARY) && bl_peek_expr_unary(node)->op == BL_SYM_ASTERISK))
    return true;

  if (bl_node_is(node, BL_NODE_EXPR_MEMBER) &&
      bl_peek_expr_member(node)->kind == BL_MEM_KIND_STRUCT)
    return true;

  if (bl_node_is(node, BL_NODE_EXPR_ELEM)) return true;
  if (LLVMIsAAllocaInst(llvm_value) || LLVMIsAGlobalVariable(llvm_value)) return true;

  return false;
}

LLVMTypeRef
to_llvm_type(context_t *cnt, bl_node_t *type)
{
  assert(type);
  LLVMTypeRef result = NULL;
  bl_node_t * arr    = bl_ast_type_get_arr(type);
  int         ptr    = bl_ast_type_get_ptr(type);

  switch (bl_node_code(type)) {
  case BL_NODE_TYPE_FUND: {
    bl_node_type_fund_t *_type = bl_peek_type_fund(type);
    switch (_type->code) {
    case BL_FTYPE_VOID: result = LLVMVoidTypeInContext(cnt->llvm_cnt); break;
    case BL_FTYPE_CHAR:
    case BL_FTYPE_S8:
    case BL_FTYPE_U8: result = LLVMInt8TypeInContext(cnt->llvm_cnt); break;
    case BL_FTYPE_S16:
    case BL_FTYPE_U16: result = LLVMInt16TypeInContext(cnt->llvm_cnt); break;
    case BL_FTYPE_S32:
    case BL_FTYPE_U32: result = LLVMInt32TypeInContext(cnt->llvm_cnt); break;
    case BL_FTYPE_S64:
    case BL_FTYPE_U64: result = LLVMInt64TypeInContext(cnt->llvm_cnt); break;
    case BL_FTYPE_F32: result = LLVMFloatTypeInContext(cnt->llvm_cnt); break;
    case BL_FTYPE_F64: result = LLVMDoubleTypeInContext(cnt->llvm_cnt); break;
    case BL_FTYPE_STRING: result = LLVMPointerType(LLVMInt8TypeInContext(cnt->llvm_cnt), 0); break;
    case BL_FTYPE_BOOL: result = LLVMInt1TypeInContext(cnt->llvm_cnt); break;
    case BL_FTYPE_SIZE:
      /* TODO: use target setup later */
      if (sizeof(size_t) == 4) {
        result = LLVMInt32TypeInContext(cnt->llvm_cnt);
      } else if (sizeof(size_t) == 8) {
        result = LLVMInt64TypeInContext(cnt->llvm_cnt);
      } else {
        bl_abort("unsupported architecture");
      }
      break;
    default: bl_abort("unknown fundamenetal type %s", bl_node_name(type));
    }
    break;
  }

  case BL_NODE_TYPE_FN: {
    /* args */
    bl_node_type_fn_t *_fn_type = bl_peek_type_fn(type);

    LLVMTypeRef  llvm_ret       = to_llvm_type(cnt, bl_ast_get_type(_fn_type->ret_type));
    LLVMTypeRef *llvm_arg_types = bl_malloc(sizeof(LLVMTypeRef) * _fn_type->argc_types);

    bl_node_t *arg;
    bl_node_t *tmp_type;
    unsigned   i = 0;
    bl_node_foreach(_fn_type->arg_types, arg)
    {
      tmp_type            = bl_ast_get_type(arg);
      llvm_arg_types[i++] = to_llvm_type(cnt, tmp_type);
    }

    result = LLVMFunctionType(llvm_ret, llvm_arg_types, i, false);
    bl_free(llvm_arg_types);
    break;
  }

  case BL_NODE_TYPE_STRUCT: {
    bl_node_type_struct_t *_struct_type = bl_peek_type_struct(type);

    LLVMTypeRef *llvm_member_types = bl_malloc(sizeof(LLVMTypeRef) * _struct_type->typesc);

    bl_node_t *member;
    bl_node_t *tmp_type;
    unsigned   i = 0;

    bl_node_foreach(_struct_type->types, member)
    {
      tmp_type               = bl_ast_get_type(member);
      llvm_member_types[i++] = to_llvm_type(cnt, tmp_type);
    }

    if (_struct_type->base_decl) {
      result = llvm_values_get(cnt, _struct_type->base_decl);
      if (!result) {
        bl_node_decl_t *_base_decl = bl_peek_decl(_struct_type->base_decl);
        assert(_base_decl->value);
        const char *name = bl_peek_ident(_base_decl->name)->str;
        assert(name);

        /* create new one named structure */
        result = LLVMStructCreateNamed(cnt->llvm_cnt, name);
        llvm_values_insert(cnt, _struct_type->base_decl, result);

        LLVMStructSetBody(result, llvm_member_types, i, false);
      }
    } else {
      /* anonymous structure type */
      result = LLVMStructTypeInContext(cnt->llvm_cnt, llvm_member_types, i, false);
    }
    bl_free(llvm_member_types);
    break;
  }

  case BL_NODE_TYPE_ENUM: {
    bl_node_type_enum_t *_enum_type = bl_peek_type_enum(type);
    assert(_enum_type->base_type);
    result = to_llvm_type(cnt, _enum_type->base_type);
    break;
  }

  default: bl_abort("invalid node type %s", bl_node_name(type));
  }

  if (ptr) {
    if (LLVMGetTypeKind(result) == LLVMVoidTypeKind) {
      result = LLVMInt8TypeInContext(cnt->llvm_cnt);
    }
    result = LLVMPointerType(result, 0);
  }

  if (arr) {
    assert(bl_node_is(arr, BL_NODE_LIT));
    result = LLVMArrayType(result, bl_peek_lit(arr)->value.u);
  }

  return result;
}

LLVMValueRef
ir_lit(context_t *cnt, bl_node_t *lit)
{
  LLVMValueRef   result = NULL;
  bl_node_lit_t *_lit   = bl_peek_lit(lit);

#define PEEK_ULL _lit->value.u
#define PEEK_REAL _lit->value.d
#define PEEK_STR _lit->value.str
#define PEEK_CHAR (unsigned long long int)_lit->value.c

  switch (bl_peek_type_fund(_lit->type)->code) {
  case BL_FTYPE_S8:
    result = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case BL_FTYPE_S16:
    result = LLVMConstInt(LLVMInt16TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case BL_FTYPE_S32:
    result = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case BL_FTYPE_S64:
    result = LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case BL_FTYPE_U8:
    result = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case BL_FTYPE_U16:
    result = LLVMConstInt(LLVMInt16TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case BL_FTYPE_U32:
    result = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case BL_FTYPE_U64:
  case BL_FTYPE_SIZE:
    result = LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case BL_FTYPE_F32:
    result = LLVMConstReal(LLVMFloatTypeInContext(cnt->llvm_cnt), PEEK_REAL);
    break;
  case BL_FTYPE_F64:
    result = LLVMConstReal(LLVMDoubleTypeInContext(cnt->llvm_cnt), PEEK_REAL);
    break;
  case BL_FTYPE_BOOL:
    result = LLVMConstInt(LLVMInt1TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case BL_FTYPE_STRING:
    // TODO
    result = LLVMBuildGlobalStringPtr(cnt->llvm_builder, PEEK_STR, "str");
    // result = get_or_create_const_string(cnt, _lit->value.str);
    break;
  case BL_FTYPE_CHAR:
    result = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), PEEK_CHAR, false);
    break;
  default: bl_abort("invalid constant type %s", bl_node_name(lit));
  }

#undef PEEK_ULL
#undef PEEK_REAL
#undef PEEK_STR
#undef PEEK_CHAR

  return result;
}

LLVMValueRef
ir_expr_call(context_t *cnt, bl_node_t *call)
{
  assert(call);
  /* run in compile time or in runtime */
  if (bl_peek_expr_call(call)->run) return ir_expr_call_ct(cnt, call);
  return ir_expr_call_rt(cnt, call);
}

LLVMValueRef
ir_expr_call_rt(context_t *cnt, bl_node_t *call)
{
  LLVMValueRef         result        = NULL;
  bl_node_expr_call_t *_call         = bl_peek_expr_call(call);
  bl_node_ident_t *    _callee_ident = bl_peek_ident(
      bl_node_is(_call->ref, BL_NODE_IDENT) ? _call->ref : bl_peek_expr_member(_call->ref)->ident);

  LLVMValueRef llvm_fn = NULL;
  if (bl_peek_decl(_callee_ident->ref)->mutable) {
    if (bl_node_is(_call->ref, BL_NODE_EXPR_MEMBER)) {
      llvm_fn = ir_expr_member(cnt, _call->ref);
    } else if (bl_node_is(_call->ref, BL_NODE_EXPR_ELEM)) {
      llvm_fn = ir_expr_elem(cnt, _call->ref);
    } else {
      llvm_fn = llvm_values_get(cnt, _callee_ident->ref);
    }
    llvm_fn = LLVMBuildLoad(cnt->llvm_builder, llvm_fn, gname("tmp"));
  } else {
    llvm_fn = ir_fn_get(cnt, _callee_ident->ref);
  }

  assert(llvm_fn);

  LLVMValueRef *llvm_args = bl_malloc(sizeof(LLVMValueRef) * _call->argsc);

  bl_node_t *arg;
  int        i = 0;
  bl_node_foreach(_call->args, arg)
  {
    llvm_args[i] = ir_expr(cnt, arg);
    assert(llvm_args[i] && "invalid call argument");

    if (should_load(arg, llvm_args[i]))
      llvm_args[i] = LLVMBuildLoad(cnt->llvm_builder, llvm_args[i], gname("tmp"));

    ++i;
  }

  result = LLVMBuildCall(cnt->llvm_builder, llvm_fn, llvm_args, (unsigned int)_call->argsc, "");
  bl_free(llvm_args);
  return result;
}

LLVMValueRef
ir_expr_call_ct(context_t *cnt, bl_node_t *call)
{
  bl_node_expr_call_t *_call = bl_peek_expr_call(call);
  // TODO: handle return values
  run(cnt, _call->ref);
  return NULL;
}

LLVMValueRef
ir_expr_member(context_t *cnt, bl_node_t *member)
{
  bl_node_expr_member_t *_member      = bl_peek_expr_member(member);
  bl_node_ident_t *      _ident       = bl_peek_ident(_member->ident);
  bl_node_decl_t *       _decl_member = bl_peek_decl(_ident->ref);

  LLVMValueRef result = NULL;

  if (_member->kind == BL_MEM_KIND_STRUCT) {
    result = ir_expr(cnt, _member->next);
    assert(result);
    if (_member->ptr_ref && should_load(member, result))
      result = LLVMBuildLoad(cnt->llvm_builder, result, gname("tmp"));
    result = LLVMBuildStructGEP(cnt->llvm_builder, result, (unsigned int)_decl_member->order,
                                gname(_ident->str));
  } else if (_member->kind == BL_MEM_KIND_ENUM) {
    assert(!_member->ptr_ref);
    assert(_decl_member->value);
    result = ir_expr(cnt, _decl_member->value);
  } else {
    bl_abort("unknown member kind");
  }
  assert(result);
  return result;
}

LLVMValueRef
ir_expr_elem(context_t *cnt, bl_node_t *elem)
{
  bl_node_expr_elem_t *_elem = bl_peek_expr_elem(elem);
  LLVMValueRef         ptr   = ir_expr(cnt, _elem->next);

  assert(_elem->index && "invalid array element index");
  LLVMValueRef index = ir_expr(cnt, _elem->index);

  if (should_load(_elem->index, index))
    index = LLVMBuildLoad(cnt->llvm_builder, index, gname("tmp"));

  LLVMValueRef indices[2];
  indices[0] = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, false);
  indices[1] = index;

  return LLVMBuildGEP(cnt->llvm_builder, ptr, indices, BL_ARRAY_SIZE(indices), "");
}

LLVMValueRef
ir_expr_cast(context_t *cnt, bl_node_t *cast)
{
  bl_node_expr_cast_t *_cast     = bl_peek_expr_cast(cast);
  LLVMTypeRef          dest_type = to_llvm_type(cnt, _cast->type);
  LLVMValueRef         next      = ir_expr(cnt, _cast->next);

  if (should_load(_cast->next, next)) {
    next = LLVMBuildLoad(cnt->llvm_builder, next, gname("tmp"));
  }

  LLVMTypeKind src_kind  = LLVMGetTypeKind(LLVMTypeOf(next));
  LLVMTypeKind dest_kind = LLVMGetTypeKind(dest_type);

  LLVMTargetDataRef  data_layout = LLVMGetModuleDataLayout(cnt->llvm_module);
  unsigned long long src_size    = LLVMSizeOfTypeInBits(data_layout, LLVMTypeOf(next));
  unsigned long long dest_size   = LLVMSizeOfTypeInBits(data_layout, dest_type);

  LLVMOpcode op = src_size > dest_size ? LLVMTrunc : LLVMSExt;

  switch (dest_kind) {

  case LLVMPointerTypeKind: {
    switch (src_kind) {
    case LLVMPointerTypeKind: op = LLVMBitCast; break;
    case LLVMIntegerTypeKind: op = LLVMIntToPtr; break;
    default: break;
    }
    break;
  }

  case LLVMIntegerTypeKind: {
    switch (src_kind) {
    case LLVMFloatTypeKind:
    case LLVMDoubleTypeKind: op = LLVMFPToSI; break;
    case LLVMPointerTypeKind: op = LLVMPtrToInt; break;
    default: break;
    }
    break;
  }

  case LLVMFloatTypeKind:
  case LLVMDoubleTypeKind: {
    switch (src_kind) {
    case LLVMIntegerTypeKind: op = LLVMSIToFP; break;
    case LLVMFloatTypeKind:
    case LLVMDoubleTypeKind: {
      op = LLVMFPExt;
      break;
    }
    default: break;
    }
    break;
  }

  default: bl_abort("invalid cast combination");
  }

  return LLVMBuildCast(cnt->llvm_builder, op, next, dest_type, gname("tmp"));
}

LLVMValueRef
ir_ident(context_t *cnt, bl_node_t *ident)
{
  LLVMValueRef     result = NULL;
  bl_node_ident_t *_ident = bl_peek_ident(ident);
  assert(_ident->ref);

  bl_node_t *ref = bl_ast_unroll_ident(_ident->ref);
  assert(bl_node_is(ref, BL_NODE_DECL));
  bl_node_decl_t *_ref = bl_peek_decl(ref);

  switch (_ref->kind) {
  case BL_DECL_KIND_FIELD:
    if (_ref->in_gscope)
      result = ir_global_get(cnt, _ident->ref);
    else
      result = llvm_values_get(cnt, _ident->ref);
    break;

  case BL_DECL_KIND_FN: result = ir_fn_get(cnt, bl_ast_unroll_ident(ident)); break;

  case BL_DECL_KIND_STRUCT: bl_log("here"); break;

  case BL_DECL_KIND_ARG: result = llvm_values_get(cnt, _ident->ref); break;

  case BL_DECL_KIND_CONSTANT: result = ir_expr(cnt, _ref->value); break;

  case BL_DECL_KIND_MEMBER:
  case BL_DECL_KIND_VARIANT:
  case BL_DECL_KIND_ENUM:
  case BL_DECL_KIND_TYPE: bl_abort("unimplemented");
  case BL_DECL_KIND_UNKNOWN: bl_abort("unknown declaration kind");
  }

  assert(result);
  return result;
}

LLVMValueRef
ir_expr_binop(context_t *cnt, bl_node_t *binop)
{

  bl_node_expr_binop_t *_binop = bl_peek_expr_binop(binop);
  LLVMValueRef          lhs    = ir_expr(cnt, _binop->lhs);
  LLVMValueRef          rhs    = ir_expr(cnt, _binop->rhs);

  if (_binop->op == BL_SYM_ASSIGN) {
    /* special case for dereferencing on the right side, we need to perform additional load
     * because we use pointer to data not real data. */
    if (should_load(_binop->rhs, rhs)) {
      rhs = LLVMBuildLoad(cnt->llvm_builder, rhs, gname("tmp"));
    }
    LLVMBuildStore(cnt->llvm_builder, rhs, lhs);
    return lhs;
  }

  if (should_load(_binop->lhs, lhs)) lhs = LLVMBuildLoad(cnt->llvm_builder, lhs, gname("tmp"));
  if (should_load(_binop->rhs, rhs)) rhs = LLVMBuildLoad(cnt->llvm_builder, rhs, gname("tmp"));

  LLVMTypeKind lhs_kind = LLVMGetTypeKind(LLVMTypeOf(lhs));
  // LLVMTypeKind rhs_kind = LLVMGetTypeKind(LLVMTypeOf(rhs));

  // assert(lhs_kind == rhs_kind);
  bool float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

  switch (_binop->op) {
  case BL_SYM_PLUS:
    if (float_kind) return LLVMBuildFAdd(cnt->llvm_builder, lhs, rhs, gname("tmp"));

    return LLVMBuildAdd(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_MINUS:
    if (float_kind) return LLVMBuildFSub(cnt->llvm_builder, lhs, rhs, gname("tmp"));
    return LLVMBuildSub(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_ASTERISK:
    if (float_kind) return LLVMBuildFMul(cnt->llvm_builder, lhs, rhs, gname("tmp"));
    return LLVMBuildMul(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_SLASH:
    if (float_kind) return LLVMBuildFDiv(cnt->llvm_builder, lhs, rhs, gname("tmp"));
    return LLVMBuildSDiv(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_MODULO: return LLVMBuildSRem(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_EQ:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOEQ, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntEQ, lhs, rhs, gname("tmp"));

  case BL_SYM_NEQ:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealONE, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntNE, lhs, rhs, gname("tmp"));

  case BL_SYM_GREATER:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGT, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGT, lhs, rhs, gname("tmp"));

  case BL_SYM_LESS:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLT, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLT, lhs, rhs, gname("tmp"));

  case BL_SYM_GREATER_EQ:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGE, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGE, lhs, rhs, gname("tmp"));

  case BL_SYM_LESS_EQ:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLE, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLE, lhs, rhs, gname("tmp"));

  case BL_SYM_LOGIC_AND: return LLVMBuildAnd(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_LOGIC_OR: return LLVMBuildOr(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  default: bl_abort("unknown binop");
  }
}

LLVMValueRef
ir_expr_unary(context_t *cnt, bl_node_t *unary)
{
  bl_node_expr_unary_t *_unary = bl_peek_expr_unary(unary);
  assert(_unary->next);
  LLVMValueRef next_val  = ir_expr(cnt, _unary->next);
  LLVMTypeRef  next_type = LLVMTypeOf(next_val);

  switch (_unary->op) {
  case BL_SYM_MINUS:
  case BL_SYM_PLUS: {
    if (should_load(_unary->next, next_val)) {
      next_val  = LLVMBuildLoad(cnt->llvm_builder, next_val, gname("tmp"));
      next_type = LLVMTypeOf(next_val);
    }

    /* TODO use BL_KIND */
    LLVMTypeKind next_type_kind = LLVMGetTypeKind(next_type);

    int mult = 1;
    switch (_unary->op) {
    case BL_SYM_MINUS: mult = -1; break;
    case BL_SYM_PLUS: mult = 1; break;
    default: bl_abort("invalid unary operation %s", bl_sym_strings[_unary->op]);
    }

    if (next_type_kind == LLVMFloatTypeKind || next_type_kind == LLVMDoubleTypeKind) {
      LLVMValueRef cnst = LLVMConstReal(next_type, (double)mult);
      return LLVMBuildFMul(cnt->llvm_builder, cnst, next_val, "");
    }

    LLVMValueRef cnst = LLVMConstInt(next_type, (unsigned long long int)mult, false);
    return LLVMBuildMul(cnt->llvm_builder, cnst, next_val, "");
  }

  case BL_SYM_NOT: {
    if (should_load(_unary->next, next_val)) {
      next_val = LLVMBuildLoad(cnt->llvm_builder, next_val, gname("tmp"));
    }

    next_val = LLVMBuildNot(cnt->llvm_builder, next_val, gname("tmp"));
    return next_val;
  }

  case BL_SYM_AND: {
    /* unary operation is getting address of something "&foo" */
    LLVMValueRef indices[1];
    indices[0] = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, false);
    return LLVMBuildGEP(cnt->llvm_builder, next_val, indices, BL_ARRAY_SIZE(indices), gname("tmp"));
  }

  case BL_SYM_ASTERISK: {
    next_val = LLVMBuildLoad(cnt->llvm_builder, next_val, gname("tmp"));
    return next_val;
  }

  default: bl_abort("invalid unary operation %s", bl_sym_strings[_unary->op]);
  }
}

LLVMValueRef
ir_expr_sizeof(context_t *cnt, bl_node_t *szof)
{
  assert(szof);
  return LLVMSizeOf(to_llvm_type(cnt, bl_peek_expr_sizeof(szof)->in));
}

LLVMValueRef
ir_expr_null(context_t *cnt, bl_node_t *nl)
{
  bl_node_expr_null_t *_null = bl_peek_expr_null(nl);
  assert(_null->type);
  LLVMTypeRef type = to_llvm_type(cnt, _null->type);
  return LLVMConstPointerNull(type);
}

LLVMValueRef
ir_expr(context_t *cnt, bl_node_t *expr)
{
  assert(expr);
  if (is_terminated(cnt)) return NULL;
  LLVMValueRef result = NULL;
  switch (bl_node_code(expr)) {
  case BL_NODE_EXPR_BINOP: result = ir_expr_binop(cnt, expr); break;
  case BL_NODE_EXPR_CALL: result = ir_expr_call(cnt, expr); break;
  case BL_NODE_EXPR_MEMBER: result = ir_expr_member(cnt, expr); break;
  case BL_NODE_EXPR_ELEM: result = ir_expr_elem(cnt, expr); break;
  case BL_NODE_EXPR_CAST: result = ir_expr_cast(cnt, expr); break;
  case BL_NODE_EXPR_UNARY: result = ir_expr_unary(cnt, expr); break;
  case BL_NODE_LIT: result = ir_lit(cnt, expr); break;
  case BL_NODE_EXPR_SIZEOF: result = ir_expr_sizeof(cnt, expr); break;
  case BL_NODE_EXPR_NULL: result = ir_expr_null(cnt, expr); break;
  case BL_NODE_IDENT: result = ir_ident(cnt, expr); break;
  default: break;
  }

  return result;
}

void
ir_block(context_t *cnt, bl_node_t *block)
{
  if (is_terminated(cnt)) return;
  bool prev_is_gscope = cnt->is_gscope;
  cnt->is_gscope      = false;

  bl_node_block_t *_block = bl_peek_block(block);
  bl_node_t *      stmt;

  bl_node_foreach(_block->nodes, stmt)
  {
    if (ir_expr(cnt, stmt)) continue;

    switch (bl_node_code(stmt)) {
    case BL_NODE_DECL: ir_decl(cnt, stmt); break;
    case BL_NODE_STMT_IF: ir_stmt_if(cnt, stmt); break;
    case BL_NODE_STMT_LOOP: ir_stmt_loop(cnt, stmt); break;
    case BL_NODE_STMT_BREAK: ir_stmt_break(cnt, stmt); break;
    case BL_NODE_STMT_CONTINUE: ir_stmt_continue(cnt, stmt); break;
    case BL_NODE_STMT_RETURN: ir_stmt_return(cnt, stmt); break;
    case BL_NODE_BLOCK: ir_block(cnt, stmt); break;
    default: break;
    }
  }

  cnt->is_gscope = prev_is_gscope;
}

LLVMValueRef
ir_decl_mut(context_t *cnt, bl_node_t *decl)
{
  LLVMValueRef    result    = NULL;
  bl_node_decl_t *_decl     = bl_peek_decl(decl);
  LLVMTypeRef     llvm_type = to_llvm_type(cnt, _decl->type);
  assert(llvm_type);

  if (cnt->is_gscope) {
    result = ir_global_get(cnt, decl);
    assert(result);
    LLVMValueRef init = ir_expr(cnt, _decl->value);
    assert(init);
    LLVMSetInitializer(result, init);
  } else {
    LLVMBasicBlockRef prev_block = LLVMGetInsertBlock(cnt->llvm_builder);
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);
    result = LLVMBuildAlloca(cnt->llvm_builder, llvm_type, gname(bl_peek_ident(_decl->name)->str));
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, prev_block);
    llvm_values_insert(cnt, decl, result);

    if (!_decl->value) return result;
    // result = LLVMBuildLoad(cnt->llvm_builder, result, gname("tmp"));
    LLVMBuildStore(cnt->llvm_builder, ir_expr(cnt, _decl->value), result);
  }
  return result;
}

LLVMValueRef
ir_decl_immut(context_t *cnt, bl_node_t *decl)
{
  bl_node_decl_t *_decl = bl_peek_decl(decl);
  assert(_decl->value);
  LLVMValueRef result = ir_expr(cnt, _decl->value);
  llvm_values_insert(cnt, decl, result);
  return result;
}

LLVMValueRef
ir_fn_get(context_t *cnt, bl_node_t *fn)
{
  bl_node_decl_t *_fn     = bl_peek_decl(fn);
  const char *    fn_name = bl_peek_ident(_fn->name)->str;

  LLVMValueRef result = LLVMGetNamedFunction(cnt->llvm_module, fn_name);
  if (!result) {

    LLVMTypeRef llvm_type = to_llvm_type(cnt, _fn->type);
    result                = LLVMAddFunction(cnt->llvm_module, fn_name, llvm_type);
  }

  return result;
}

LLVMValueRef
ir_global_get(context_t *cnt, bl_node_t *global)
{
  bl_node_decl_t *_global = bl_peek_decl(global);
  const char *    g_name  = bl_peek_ident(_global->name)->str;

  LLVMValueRef result = LLVMGetNamedGlobal(cnt->llvm_module, g_name);
  if (!result) {
    LLVMTypeRef llvm_type = to_llvm_type(cnt, _global->type);
    result                = LLVMAddGlobal(cnt->llvm_module, llvm_type, g_name);
  }

  return result;
}

LLVMValueRef
ir_decl_fn(context_t *cnt, bl_node_t *decl)
{
  /* local functions will be generated in separate module */
  if (!cnt->is_gscope) return NULL;

  bl_node_decl_t *_decl = bl_peek_decl(decl);

  LLVMValueRef      result = ir_fn_get(cnt, decl);
  bl_node_lit_fn_t *_fn    = bl_peek_lit_fn(_decl->value);

  {
    assert(_decl->value);
    cnt->fn_init_block  = LLVMAppendBasicBlock(result, gname("init"));
    cnt->fn_entry_block = LLVMAppendBasicBlock(result, gname("entry"));
    cnt->fn_ret_block   = LLVMAppendBasicBlock(result, gname("exit"));

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);

    bl_node_type_fn_t *_fn_type = bl_peek_type_fn(_fn->type);
    /*
     * Create named references to function parameters so they
     * can be called by name in function body.
     */

    bl_node_t *arg;
    int        i = 0;
    bl_node_foreach(_fn_type->arg_types, arg)
    {
      const char * name  = bl_peek_ident(bl_peek_decl(arg)->name)->str;
      LLVMValueRef p     = LLVMGetParam(result, (unsigned int)i++);
      LLVMValueRef p_tmp = LLVMBuildAlloca(cnt->llvm_builder, LLVMTypeOf(p), gname(name));
      LLVMBuildStore(cnt->llvm_builder, p, p_tmp);

      llvm_values_insert(cnt, arg, p_tmp);
    }

    /*
     * Prepare return value.
     */
    LLVMTypeRef llvm_ret_type = to_llvm_type(cnt, bl_ast_get_type(_fn_type->ret_type));
    if (llvm_ret_type != LLVMVoidTypeInContext(cnt->llvm_cnt)) {
      cnt->fn_ret_val = LLVMBuildAlloca(cnt->llvm_builder, llvm_ret_type, gname("ret"));
    } else {
      cnt->fn_ret_val = NULL;
    }

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_entry_block);
  }

  /* generate function body */
  ir_block(cnt, _fn->block);

  {
    LLVMBasicBlockRef curr_block = LLVMGetInsertBlock(cnt->llvm_builder);

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);
    LLVMBuildBr(cnt->llvm_builder, cnt->fn_entry_block);

    if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
      LLVMPositionBuilderAtEnd(cnt->llvm_builder, curr_block);
      LLVMBuildBr(cnt->llvm_builder, cnt->fn_ret_block);
    }

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_ret_block);
    if (cnt->fn_ret_val) {
      cnt->fn_ret_val = LLVMBuildLoad(cnt->llvm_builder, cnt->fn_ret_val, gname("tmp"));
      LLVMBuildRet(cnt->llvm_builder, cnt->fn_ret_val);
    } else {
      LLVMBuildRetVoid(cnt->llvm_builder);
    }
  }

  /* reset tmps */
  cnt->fn_entry_block = NULL;
  cnt->fn_init_block  = NULL;
  cnt->fn_ret_block   = NULL;
  cnt->fn_ret_val     = NULL;
  cnt->break_block    = NULL;
  cnt->continue_block = NULL;

  return result;
}

LLVMValueRef
ir_decl(context_t *cnt, bl_node_t *decl)
{
  if (is_terminated(cnt)) return NULL;
  bl_node_decl_t *_decl = bl_peek_decl(decl);
  assert(_decl->type);
  assert(_decl->name);

  switch (_decl->kind) {
  case BL_DECL_KIND_FIELD: return ir_decl_mut(cnt, decl);
  case BL_DECL_KIND_FN: return ir_decl_fn(cnt, decl);
  case BL_DECL_KIND_MEMBER: return ir_decl_mut(cnt, decl);
  case BL_DECL_KIND_ARG: return ir_decl_mut(cnt, decl);
  case BL_DECL_KIND_CONSTANT: return ir_decl_immut(cnt, decl);
  case BL_DECL_KIND_STRUCT:
  case BL_DECL_KIND_ENUM:
  case BL_DECL_KIND_VARIANT:
  case BL_DECL_KIND_TYPE: break;
  default: bl_abort("unknown declaration kind");
  }

  return NULL;
}

void
ir_stmt_if(context_t *cnt, bl_node_t *stmt_if)
{
  if (is_terminated(cnt)) return;

  bl_node_stmt_if_t *_stmt_if     = bl_peek_stmt_if(stmt_if);
  LLVMBasicBlockRef  insert_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMValueRef       parent       = LLVMGetBasicBlockParent(insert_block);
  assert(LLVMIsAFunction(parent));

  LLVMBasicBlockRef if_then = LLVMAppendBasicBlock(parent, gname("if_then"));
  LLVMBasicBlockRef if_else = LLVMAppendBasicBlock(parent, gname("if_else"));
  LLVMBasicBlockRef if_cont = LLVMAppendBasicBlock(parent, gname("if_cont"));
  LLVMValueRef      expr    = ir_expr(cnt, _stmt_if->test);

  if (should_load(_stmt_if->test, expr))
    expr = LLVMBuildLoad(cnt->llvm_builder, expr, gname("tmp"));

  expr =
      LLVMBuildIntCast(cnt->llvm_builder, expr, LLVMInt1TypeInContext(cnt->llvm_cnt), gname("tmp"));

  /*
   * If condition break generation.
   */
  LLVMBuildCondBr(cnt->llvm_builder, expr, if_then, if_else);

  if (_stmt_if->false_stmt == NULL) {
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_else);
    LLVMBuildBr(cnt->llvm_builder, if_cont);
  }

  /* then block */
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_then);
  ir_block(cnt, _stmt_if->true_stmt);

  LLVMBasicBlockRef curr_block = LLVMGetInsertBlock(cnt->llvm_builder);
  if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
    LLVMBuildBr(cnt->llvm_builder, if_cont);
  }

  /* else if */
  if (_stmt_if->false_stmt != NULL) {
    /* else */
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_else);
    if (bl_node_is(_stmt_if->false_stmt, BL_NODE_STMT_IF))
      ir_stmt_if(cnt, _stmt_if->false_stmt);
    else
      ir_block(cnt, _stmt_if->false_stmt);

    curr_block = LLVMGetInsertBlock(cnt->llvm_builder);
    if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
      LLVMBuildBr(cnt->llvm_builder, if_cont);
    }
  }

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_cont);
}

void
ir_stmt_return(context_t *cnt, bl_node_t *stmt_return)
{
  if (is_terminated(cnt)) return;
  bl_node_stmt_return_t *_ret = bl_peek_stmt_return(stmt_return);
  if (!_ret->expr) {
    LLVMBuildBr(cnt->llvm_builder, cnt->fn_ret_block);
    return;
  }

  LLVMValueRef val = ir_expr(cnt, _ret->expr);

  if (should_load(_ret->expr, val)) val = LLVMBuildLoad(cnt->llvm_builder, val, gname("tmp"));

  assert(cnt->fn_ret_val);
  assert(cnt->fn_ret_block);
  LLVMBuildStore(cnt->llvm_builder, val, cnt->fn_ret_val);
  LLVMBuildBr(cnt->llvm_builder, cnt->fn_ret_block);
}

void
ir_stmt_loop(context_t *cnt, bl_node_t *loop)
{
  if (is_terminated(cnt)) return;
  bl_node_stmt_loop_t *_loop        = bl_peek_stmt_loop(loop);
  LLVMBasicBlockRef    insert_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMValueRef         parent       = LLVMGetBasicBlockParent(insert_block);
  assert(LLVMIsAFunction(parent));

  LLVMBasicBlockRef loop_decide         = LLVMAppendBasicBlock(parent, gname("loop_decide"));
  LLVMBasicBlockRef loop_block          = LLVMAppendBasicBlock(parent, gname("loop"));
  LLVMBasicBlockRef loop_cont           = LLVMAppendBasicBlock(parent, gname("loop_cont"));
  LLVMValueRef      expr                = NULL;
  LLVMBasicBlockRef prev_break_block    = cnt->break_block;
  LLVMBasicBlockRef prev_continue_block = cnt->continue_block;
  cnt->break_block                      = loop_cont;
  cnt->continue_block                   = loop_decide;

  LLVMBuildBr(cnt->llvm_builder, loop_decide);
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_decide);

  if (_loop->test) {
    expr = ir_expr(cnt, _loop->test);

    if (should_load(_loop->test, expr)) expr = LLVMBuildLoad(cnt->llvm_builder, expr, gname("tmp"));
  } else {
    expr = LLVMConstInt(LLVMInt1TypeInContext(cnt->llvm_cnt), true, false);
  }

  LLVMBuildCondBr(cnt->llvm_builder, expr, loop_block, loop_cont);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_block);
  ir_block(cnt, _loop->true_stmt);

  LLVMBasicBlockRef curr_block = LLVMGetInsertBlock(cnt->llvm_builder);
  if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
    LLVMBuildBr(cnt->llvm_builder, loop_decide);
  }

  cnt->break_block    = prev_break_block;
  cnt->continue_block = prev_continue_block;
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_cont);
}

void
ir_stmt_break(context_t *cnt, bl_node_t *brk)
{
  if (is_terminated(cnt)) return;
  LLVMBuildBr(cnt->llvm_builder, cnt->break_block);
}

void
ir_stmt_continue(context_t *cnt, bl_node_t *cont)
{
  if (is_terminated(cnt)) return;
  LLVMBuildBr(cnt->llvm_builder, cnt->continue_block);
}

LLVMExecutionEngineRef
create_jit(LLVMContextRef llvm_cnt)
{
  LLVMModuleRef          module = LLVMModuleCreateWithNameInContext("run", llvm_cnt);
  LLVMExecutionEngineRef jit;
  char *                 llvm_error = NULL;

  if (LLVMCreateJITCompilerForModule(&jit, module, 3, &llvm_error) != 0)
    bl_abort("failed to create execution engine for compile-time module with error %s", llvm_error);

  return jit;
}

LLVMGenericValueRef
run(context_t *cnt, bl_node_t *fn)
{
  bl_node_t *decl = bl_ast_unroll_ident(fn);
  link_into_jit(cnt, decl);
  LLVMValueRef        llvm_fn;
  LLVMGenericValueRef result;

  bl_node_decl_t *_decl     = bl_peek_decl(decl);
  const char *    decl_name = bl_peek_ident(_decl->name)->str;
  assert(decl_name);

  if (!LLVMFindFunction(cnt->llvm_jit, decl_name, &llvm_fn)) {
    result = LLVMRunFunction(cnt->llvm_jit, llvm_fn, 0, NULL);
  } else {
    bl_abort("unknown function %s", decl_name);
  }

  return result;
}

void
generate(context_t *cnt)
{
  BList *         queue = cnt->assembly->ir_queue;
  bl_node_t *     decl;
  bl_node_decl_t *_decl;

  while (!bo_list_empty(queue)) {
    decl = bo_list_front(queue, bl_node_t *);
    bo_list_pop_front(queue);

    _decl = bl_peek_decl(decl);
    if (_decl->flags & BL_FLAG_EXTERN) continue;

    if (is_satisfied(cnt, decl, true)) {
      generate_decl(cnt, decl);

      if (_decl->flags & BL_FLAG_MAIN) {
        assert(!cnt->main_tmp);
        cnt->main_tmp = decl;
      }
    } else {
      /* declaration is waiting for it's dependencies and need to be processed later */
      bo_list_push_back(queue, decl);
#if VERBOSE
      bl_log(BL_RED("defered: '%s'"), bl_peek_ident(bl_peek_decl(decl)->name)->str);
#endif
    }
  }
}

void
generate_decl(context_t *cnt, bl_node_t *decl)
{
  assert(decl);
  bl_node_decl_t *_decl = bl_peek_decl(decl);
#if VERBOSE
  bl_log(BL_GREEN("generate: '%s'"), bl_peek_ident(_decl->name)->str);
#endif
  cnt->llvm_module =
      LLVMModuleCreateWithNameInContext(bl_peek_ident(_decl->name)->str, cnt->llvm_cnt);
  ir_decl(cnt, decl);
  bo_htbl_insert(cnt->llvm_modules, (uint64_t)decl, cnt->llvm_module);
  llvm_values_reset(cnt);
}

LLVMModuleRef
link(context_t *cnt, bl_node_t *entry)
{
  if (!entry) return NULL;
  LLVMModuleRef dest_module = _link(cnt, entry);

#if PRINT_IR
  {
    char *str = LLVMPrintModuleToString(dest_module);
    bl_log("\n--------------------------------------------------------------------------------"
           "\n%s"
           "\n--------------------------------------------------------------------------------",
           str);
    LLVMDisposeMessage(str);
  }
#endif
  return dest_module;
}

LLVMModuleRef
_link(context_t *cnt, bl_node_t *entry)
{
  bl_node_decl_t *_entry = bl_peek_decl(entry);

  if (!bo_htbl_has_key(cnt->llvm_modules, (uint64_t)entry)) return NULL;

  LLVMModuleRef dest_module = bo_htbl_at(cnt->llvm_modules, (uint64_t)entry, LLVMModuleRef);
  assert(dest_module);
  if (!_entry->deps) return dest_module;

  bl_dependency_t dep;
  bo_iterator_t   it;
  bl_bhtbl_foreach(_entry->deps, it)
  {
    dep = bo_htbl_iter_peek_value(_entry->deps, &it, bl_dependency_t);

    /* link all lax dependencies */
    if (dep.type & BL_DEP_LAX) {
      /* must be linked */
      LLVMModuleRef src_module = _link(cnt, dep.node);

      if (src_module) {
        if (LLVMLinkModules2(dest_module, src_module)) bl_abort("unable to link modules");

        bo_htbl_erase_key(cnt->llvm_modules, (uint64_t)dep.node);
      }
    }
  }

  return dest_module;
}

static void
link_into_jit(context_t *cnt, bl_node_t *fn)
{
  assert(fn);
  if (bo_htbl_has_key(cnt->jit_linked, (uint64_t)fn)) return;

  bl_node_decl_t *_fn = bl_peek_decl(fn);
  if (!bo_htbl_has_key(cnt->llvm_modules, (uint64_t)fn))
    bl_abort("Missing llvm module for '%s', this is usualy caused by #run directive.",
             bl_peek_ident(_fn->name)->str);

  LLVMModuleRef module = bo_htbl_at(cnt->llvm_modules, (uint64_t)fn, LLVMModuleRef);
  module               = LLVMCloneModule(module);
  LLVMAddModule(cnt->llvm_jit, module);
  bo_htbl_insert_empty(cnt->jit_linked, (uint64_t)fn);

  if (!_fn->deps) return;

  bo_iterator_t   iter;
  bl_dependency_t dep;
  bl_bhtbl_foreach(_fn->deps, iter)
  {
    dep = bo_htbl_iter_peek_value(_fn->deps, &iter, bl_dependency_t);

    if (dep.type & BL_DEP_LAX) {
      link_into_jit(cnt, dep.node);
    }
  }
}

static inline bool
generated(context_t *cnt, bl_node_t *decl)
{
  return bo_htbl_has_key(cnt->llvm_modules, (uint64_t)decl);
}

bool
is_satisfied(context_t *cnt, bl_node_t *decl, bool strict_only)
{
  BHashTable *deps = bl_peek_decl(decl)->deps;
  if (!deps) return true;

  bo_iterator_t   iter;
  bl_dependency_t dep;
  bl_bhtbl_foreach(deps, iter)
  {
    dep = bo_htbl_iter_peek_value(deps, &iter, bl_dependency_t);

    // PERFORMANCE: is there some better solution than check whole tree???
    if (dep.type & BL_DEP_STRICT || !strict_only) {
      if (!generated(cnt, dep.node)) {
        return false;
      } else {
        return is_satisfied(cnt, dep.node, false);
      }
    }
  }

  return true;
}

void
bl_ir_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  assert(!bo_list_empty(assembly->ir_queue) && "nothig to generate");
  context_t cnt;
  memset(&cnt, 0, sizeof(cnt));
  cnt.builder        = builder;
  cnt.assembly       = assembly;
  cnt.is_gscope      = true;
  cnt.jit_linked     = bo_htbl_new(0, bo_list_size(assembly->ir_queue));
  cnt.llvm_cnt       = LLVMContextCreate();
  cnt.llvm_builder   = LLVMCreateBuilderInContext(cnt.llvm_cnt);
  cnt.llvm_modules   = bo_htbl_new(sizeof(LLVMModuleRef), bo_list_size(assembly->ir_queue));
  cnt.llvm_values    = bo_htbl_new(sizeof(LLVMValueRef), 256);
  cnt.llvm_jit       = create_jit(cnt.llvm_cnt);

  generate(&cnt);

  assert(cnt.main_tmp);
  assembly->llvm_module = link(&cnt, cnt.main_tmp);
  bo_htbl_erase_key(cnt.llvm_modules, (uint64_t)cnt.main_tmp);

  // assembly->llvm_module = LLVMModuleCreateWithNameInContext("main", cnt.llvm_cnt);
  ir_validate(assembly->llvm_module);

  assembly->llvm_cnt = cnt.llvm_cnt;
  assembly->llvm_jit = cnt.llvm_jit;
  bo_unref(cnt.llvm_modules);
  bo_unref(cnt.llvm_values);
  bo_unref(cnt.jit_linked);
}
