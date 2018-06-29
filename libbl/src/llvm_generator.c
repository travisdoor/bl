//************************************************************************************************
// blc
//
// File:   llvm_generator.c
// Author: Martin Dorazil
// Date:   14.2.18
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

#include <setjmp.h>
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Linker.h>
#include <bobject/containers/hash.h>

#include "common_impl.h"
#include "stages_impl.h"
#include "visitor_impl.h"

#define push_value_cscope(ptr, llvm_value_ref)                                                     \
  (bo_htbl_insert(cnt->cscope, (uint64_t)(ptr), (llvm_value_ref)))

#define push_value_gscope(ptr, llvm_value_ref)                                                     \
  (bo_htbl_insert(cnt->gscope, (uint64_t)(ptr), (llvm_value_ref)))

#define get_value_cscope(ptr) (bo_htbl_at(cnt->cscope, (uint64_t)(ptr), LLVMValueRef))

#define get_value_gscope(ptr) (bo_htbl_at(cnt->gscope, (uint64_t)(ptr), LLVMValueRef))

#define is_in_gscope(ptr) (bo_htbl_has_key(cnt->gscope, (uint64_t)ptr))

#define is_in_cscope(ptr) (bo_htbl_has_key(cnt->cscope, (uint64_t)ptr))

#define reset_cscope() (bo_htbl_clear(cnt->cscope))
#define reset_gscope() (bo_htbl_clear(cnt->gscope))

#define skip_if_terminated(cnt)                                                                    \
  if (LLVMGetInsertBlock((cnt)->llvm_builder) &&                                                   \
      LLVMGetBasicBlockTerminator(LLVMGetInsertBlock((cnt)->llvm_builder)) != NULL)                \
    return;

#define is_deref(node)                                                                             \
  ((bl_node_is((node), BL_EXPR_UNARY) && bl_peek_expr_unary((node))->op == BL_SYM_ASTERISK))

#if BL_DEBUG
#define gname(s) s
#else
#define gname(s) ""
#endif

typedef struct
{
  /* main builder */
  bl_builder_t *builder;

  /* current assembly */
  bl_assembly_t *assembly;

  /* main llvm module */
  LLVMModuleRef llvm_mod;

  /* llvm IR code builder */
  LLVMBuilderRef llvm_builder;

  /* llvm context */
  LLVMContextRef llvm_cnt;

  /* mapping ast node pointers to LLVMValues for symbols in global scope */
  BHashTable *gscope;

  /* mapping ast node pointers to LLVMValues for symbols in current function scope, functions can be
   * later located by node pointer */
  BHashTable *cscope;

  BHashTable *llvm_modules;

  /* temporary blocks */
  LLVMBasicBlockRef func_init_block;
  LLVMBasicBlockRef func_ret_block;
  LLVMBasicBlockRef func_entry_block;
  LLVMBasicBlockRef continue_block;
  LLVMBasicBlockRef break_block;

  /* LLVMValueRef to current return tmp */
  LLVMValueRef ret_value;

  LLVMExecutionEngineRef llvm_jit;
  BHashTable *           jit_linked;

  bl_node_t *tmp_main;
} context_t;

static inline context_t *
peek_cnt(bl_visitor_t *visitor)
{
  return (context_t *)visitor->context;
}

static LLVMValueRef
gen_init(context_t *cnt, bl_node_t *init);

static LLVMModuleRef
link(context_t *cnt, bl_node_t *entry);

static inline int
gen_func_args(context_t *cnt, bl_decl_func_t *func, LLVMTypeRef *out);

static LLVMValueRef
gen_func(context_t *cnt, bl_node_t *func);

static LLVMValueRef
gen_call(context_t *cnt, bl_node_t *call);

static inline int
gen_call_args(context_t *cnt, bl_node_t *call, LLVMValueRef *out);

static inline LLVMValueRef
get_or_create_const_string(context_t *cnt, const char *str);

static LLVMValueRef
gen_binop(context_t *cnt, bl_node_t *binop);

static inline LLVMValueRef
gen_member_ref(context_t *cnt, bl_node_t *member_ref);

static inline LLVMValueRef
gen_array_ref(context_t *cnt, bl_node_t *array_ref);

/* static LLVMValueRef */
/* gen_array_ref_1(context_t *cnt, bl_node_t *array, size_t *dim_mult, BArray **dims, */
/*                 size_t *dims_iter, size_t *iter); */

static LLVMValueRef
gen_expr(context_t *cnt, bl_node_t *expr);

static inline LLVMValueRef
gen_null(context_t *cnt, bl_node_t *nl);

static LLVMValueRef
gen_unary_expr(context_t *cnt, bl_node_t *expr);

static LLVMTypeRef
gen_struct(context_t *cnt, bl_node_t *strct);

static inline LLVMTypeRef
gen_enum(context_t *cnt, bl_node_t *enm);

static LLVMTypeRef
to_llvm_type(context_t *cnt, bl_node_t *type);

static inline bool
should_load(bl_node_t *node)
{
  return bl_node_is(node, BL_EXPR_ARRAY_REF) || bl_node_is(node, BL_EXPR_MEMBER_REF) ||
         is_deref(node);
}

static LLVMGenericValueRef
run_fn(context_t *cnt, bl_node_t *callee);

/*
 * convert known type to LLVM type representation
 */
LLVMTypeRef
to_llvm_type(context_t *cnt, bl_node_t *type)
{
  LLVMTypeRef llvm_type = NULL;
  bl_node_t * size_expr = NULL;
  bool        is_ptr    = false;
  if (bl_node_is(type, BL_TYPE_FUND)) {
    bl_type_fund_t *_type = bl_peek_type_fund(type);
    is_ptr                = _type->is_ptr;
    switch (_type->type) {
    case BL_FTYPE_VOID:
      llvm_type = LLVMVoidTypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_CHAR:
    case BL_FTYPE_I8:
    case BL_FTYPE_U8:
      llvm_type = LLVMInt8TypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_I16:
    case BL_FTYPE_U16:
      llvm_type = LLVMInt16TypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_I32:
    case BL_FTYPE_U32:
      llvm_type = LLVMInt32TypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_I64:
    case BL_FTYPE_U64:
      llvm_type = LLVMInt64TypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_F32:
      llvm_type = LLVMFloatTypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_F64:
      llvm_type = LLVMDoubleTypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_STRING:
      llvm_type = LLVMPointerType(LLVMInt8TypeInContext(cnt->llvm_cnt), 0);
      break;
    case BL_FTYPE_BOOL:
      llvm_type = LLVMInt1TypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_SIZE:
      /* TODO: use target setup later */
      if (sizeof(size_t) == 4) {
        llvm_type = LLVMInt32TypeInContext(cnt->llvm_cnt);
      } else if (sizeof(size_t) == 8) {
        llvm_type = LLVMInt64TypeInContext(cnt->llvm_cnt);
      } else {
        bl_abort("unsupported architecture");
      }
      break;
    default:
      bl_abort("unknown fundamenetal type");
    }

    size_expr = _type->dim;
  } else if (bl_node_is(type, BL_TYPE_REF)) {
    /* here we solve custom user defined types like structures and enumerators which are described
     * by reference to definition node */
    bl_type_ref_t *_type = bl_peek_type_ref(type);
    bl_assert(_type, "invalid type reference");
    is_ptr = _type->is_ptr;
    switch (bl_node_code(_type->ref)) {
    case BL_DECL_STRUCT:
      llvm_type = gen_struct(cnt, _type->ref);
      break;
    case BL_DECL_ENUM:
      llvm_type = gen_enum(cnt, _type->ref);
      break;
    default:
      bl_abort("invalid reference type");
    }

    size_expr = _type->dim;
  }

  /* type is array */
  if (size_expr) {
    bl_assert(bl_node_is(size_expr, BL_EXPR_CALL), "expected call");
    bl_node_t *         callee       = bl_peek_expr_call(size_expr)->ref;
    LLVMGenericValueRef size_generic = run_fn(cnt, callee);
    size_t              size         = LLVMGenericValueToInt(size_generic, false);
    llvm_type                        = LLVMArrayType(llvm_type, size);
  }

  if (is_ptr) {
    llvm_type = LLVMPointerType(llvm_type, 0);
  }

  return llvm_type;
}

LLVMValueRef
gen_init(context_t *cnt, bl_node_t *init)
{
  bl_expr_init_t *_init = bl_peek_expr_init(init);

  bl_assert(_init->type, "invalid type for initialization list");

  LLVMBasicBlockRef prev_block = LLVMGetInsertBlock(cnt->llvm_builder);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_init_block);
  LLVMValueRef result =
      LLVMBuildAlloca(cnt->llvm_builder, to_llvm_type(cnt, _init->type), gname("tmp"));
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, prev_block);
  push_value_cscope(init, result);

  bl_node_t *expr = _init->exprs;
  while (expr) {
    gen_expr(cnt, expr);
    expr = expr->next;
  }

  return result;
}

LLVMTypeRef
gen_struct(context_t *cnt, bl_node_t *strct)
{
  bl_decl_struct_t *_strct = bl_peek_decl_struct(strct);
  LLVMTypeRef       type   = NULL;

  /* structure can be already generated -> check cache */
  if (is_in_gscope(strct)) {
    type = (LLVMTypeRef)get_value_gscope(strct);
    return type;
  }

  type = LLVMStructCreateNamed(cnt->llvm_cnt, _strct->id.str);
  push_value_gscope(strct, type);

  LLVMTypeRef *members = bl_malloc(sizeof(LLVMTypeRef) * _strct->membersc);
  bl_node_t *  member  = _strct->members;
  int          i       = 0;

  while (member && i < _strct->membersc) {
    members[i++] = to_llvm_type(cnt, bl_peek_decl_struct_member(member)->type);
    member       = member->next;
  }

  LLVMStructSetBody(type, members, (unsigned int)i, false);
  bl_free(members);

  return type;
}

LLVMTypeRef
gen_enum(context_t *cnt, bl_node_t *enm)
{
  bl_decl_enum_t *_enm = bl_peek_decl_enum(enm);
  return to_llvm_type(cnt, _enm->type);
}

LLVMValueRef
gen_cast(context_t *cnt, bl_node_t *cast)
{
  bl_expr_cast_t *_cast     = bl_peek_expr_cast(cast);
  LLVMTypeRef     dest_type = to_llvm_type(cnt, _cast->type);
  LLVMValueRef    next      = gen_expr(cnt, _cast->next);

  if (should_load(_cast->next) || LLVMIsAAllocaInst(next)) {
    next = LLVMBuildLoad(cnt->llvm_builder, next, gname("tmp"));
  }

  LLVMTypeKind src_kind  = LLVMGetTypeKind(LLVMTypeOf(next));
  LLVMTypeKind dest_kind = LLVMGetTypeKind(dest_type);

  LLVMOpcode op = LLVMSExt;

  // bl_log("from %d to %d", src_kind, dest_kind);
  switch (dest_kind) {

  case LLVMPointerTypeKind: {
    switch (src_kind) {
    case LLVMPointerTypeKind:
      op = LLVMBitCast;
      break;
    case LLVMIntegerTypeKind:
      op = LLVMIntToPtr;
      break;
    default:
      break;
    }
    break;
  }

  case LLVMIntegerTypeKind: {
    switch (src_kind) {
    case LLVMFloatTypeKind:
    case LLVMDoubleTypeKind:
      op = LLVMFPToSI;
      break;
    case LLVMPointerTypeKind:
      op = LLVMPtrToInt;
      break;
    default:
      break;
    }
    break;
  }

  case LLVMFloatTypeKind:
  case LLVMDoubleTypeKind: {
    switch (src_kind) {
    case LLVMIntegerTypeKind:
      op = LLVMSIToFP;
      // bl_log("si -> fp");
      break;
    case LLVMFloatTypeKind:
    case LLVMDoubleTypeKind: {
      op = LLVMFPExt;
      break;
    }
    default:
      break;
    }
    break;
  }

  default:
    bl_abort("invalid cast combination");
  }

  return LLVMBuildCast(cnt->llvm_builder, op, next, dest_type, gname("tmp"));
}

int
gen_func_args(context_t *cnt, bl_decl_func_t *func, LLVMTypeRef *out)
{
  int        out_i = 0;
  bl_node_t *arg   = func->args;
  while (arg) {
    *out = to_llvm_type(cnt, bl_peek_decl_arg(arg)->type);

    out++;
    out_i++;
    arg = arg->next;
  }

  return out_i;
}

/*
 * generate function declaration (forward or direct)
 */
LLVMValueRef
gen_func(context_t *cnt, bl_node_t *func)
{
  bl_decl_func_t *_func     = bl_peek_decl_func(func);
  LLVMValueRef    llvm_func = NULL;

  /* find extern functions by name in current module */
  if (_func->modif & BL_MODIF_EXTERN) {
    llvm_func = LLVMGetNamedFunction(cnt->llvm_mod, _func->id.str);
  } else if (is_in_gscope(func)) {
    llvm_func = get_value_gscope(func);
  }

  /* args */
  LLVMTypeRef param_types[BL_MAX_FUNC_ARG_COUNT] = {0};
  const int   pc                                 = gen_func_args(cnt, _func, param_types);

  if (llvm_func == NULL) {
    const char *uname = NULL;
    if (!(_func->modif & BL_MODIF_EXTERN) && !(_func->modif & BL_MODIF_EXPORT))
      uname = _func->uname;
    else
      uname = _func->id.str;

    LLVMTypeRef ret      = to_llvm_type(cnt, _func->ret_type);
    LLVMTypeRef ret_type = LLVMFunctionType(ret, param_types, (unsigned int)pc, false);
    llvm_func            = LLVMAddFunction(cnt->llvm_mod, uname, ret_type);

    if (!(_func->modif & BL_MODIF_EXTERN))
      push_value_gscope(func, llvm_func);
  }

  return llvm_func;
}

int
gen_call_args(context_t *cnt, bl_node_t *call, LLVMValueRef *out)
{
  int             out_i = 0;
  bl_expr_call_t *_call = bl_peek_expr_call(call);

  bl_node_t *expr = _call->args;
  while (expr) {
    LLVMValueRef val = gen_expr(cnt, expr);

    if (should_load(expr) || LLVMIsAAllocaInst(val)) {
      *out = LLVMBuildLoad(cnt->llvm_builder, val, gname("tmp"));
    } else {
      *out = val;
    }

    out++;
    out_i++;
    expr = expr->next;
  }

  return out_i;
}

static void
link_into_jit(context_t *cnt, bl_node_t *fn)
{
  if (bo_htbl_has_key(cnt->jit_linked, (uint64_t)fn))
    return;

  bl_decl_func_t *_fn = bl_peek_decl_func(fn);
  bl_assert(bo_htbl_has_key(cnt->llvm_modules, (uint64_t)fn),
            "function %s has no llvm module generated yet!!!", _fn->id.str) LLVMModuleRef module =
      bo_htbl_at(cnt->llvm_modules, (uint64_t)fn, LLVMModuleRef);
  bl_assert("invalid llvm module for function %s", _fn->id.str);

  module = LLVMCloneModule(module);
  LLVMAddModule(cnt->llvm_jit, module);
  bo_htbl_insert_empty(cnt->jit_linked, (uint64_t)fn);

  if (!_fn->deps)
    return;

  bo_iterator_t    iter = bo_list_begin(_fn->deps);
  bo_iterator_t    end  = bo_list_end(_fn->deps);
  bl_dependency_t *dep;
  while (!bo_iterator_equal(&iter, &end)) {
    dep = &bo_list_iter_peek(_fn->deps, &iter, bl_dependency_t);
    bo_list_iter_next(_fn->deps, &iter);

    if (dep->type & BL_DEP_LAX) {
      link_into_jit(cnt, dep->node);
    }
  }
}

static LLVMExecutionEngineRef
create_jit(context_t *cnt)
{
  LLVMModuleRef          module = LLVMModuleCreateWithNameInContext("run", cnt->llvm_cnt);
  LLVMExecutionEngineRef jit;
  char *                 llvm_error = NULL;
  if (LLVMCreateJITCompilerForModule(&jit, module, 3, &llvm_error) != 0)
    bl_abort("failed to create execution engine for compile-time module with error %s", llvm_error);

  return jit;
}

LLVMGenericValueRef
run_fn(context_t *cnt, bl_node_t *callee)
{
  link_into_jit(cnt, callee);
  LLVMValueRef        fn;
  LLVMGenericValueRef result;
  if (!LLVMFindFunction(cnt->llvm_jit, bl_peek_decl_func(callee)->uname, &fn)) {
    result = LLVMRunFunction(cnt->llvm_jit, fn, 0, NULL);
  } else {
    bl_abort("unknown function");
  }

  return result;
}

/*
 * generate method call and return value of method return
 */
LLVMValueRef
gen_call(context_t *cnt, bl_node_t *call)
{
  bl_expr_call_t *_call = bl_peek_expr_call(call);

  if (_call->run_in_compile_time) {
    bl_decl_func_t *    _callee = bl_peek_decl_func(_call->ref);
    LLVMGenericValueRef tmp     = run_fn(cnt, _call->ref);

    LLVMTypeRef    ret_type      = to_llvm_type(cnt, _callee->ret_type);
    bl_type_kind_e ret_type_kind = bl_ast_type_get_kind(_callee->ret_type);
    LLVMValueRef   result        = NULL;

    switch (ret_type_kind) {
    case BL_VOID_KIND:
      result = NULL;
      break;
    case BL_SINT_KIND:
      result = LLVMConstInt(ret_type, LLVMGenericValueToInt(tmp, true), true);
      break;
    case BL_UINT_KIND:
    case BL_SIZE_KIND:
    case BL_BOOL_KIND:
    case BL_CHAR_KIND:
      result = LLVMConstInt(ret_type, LLVMGenericValueToInt(tmp, false), false);
      break;
    case BL_REAL_KIND:
      result = LLVMConstReal(ret_type, LLVMGenericValueToFloat(ret_type, tmp));
      break;
    default:
      bl_abort("unsupported type of run result");
    }

    LLVMDisposeGenericValue(tmp);
    return result;
  } else {
    LLVMValueRef fn                          = gen_func(cnt, _call->ref);
    LLVMValueRef argv[BL_MAX_FUNC_ARG_COUNT] = {0};
    int          argc                        = gen_call_args(cnt, call, argv);
    return LLVMBuildCall(cnt->llvm_builder, fn, argv, argc, "");
  }
}

LLVMValueRef
get_or_create_const_string(context_t *cnt, const char *str)
{
  uint32_t  hash    = bo_hash_from_str(str);
  const int max_len = 32;
  char      name_tmp[max_len];
  snprintf(&name_tmp[0], max_len, "str_%u", hash);

  LLVMValueRef s = LLVMGetNamedGlobal(cnt->llvm_mod, name_tmp);
  if (!s)
    s = LLVMBuildGlobalString(cnt->llvm_builder, str, name_tmp);

  s = LLVMConstPointerCast(s, LLVMPointerType(LLVMInt8TypeInContext(cnt->llvm_cnt), 0));
  return s;
}

/*
 * generate unary expressions like +, -, &, *, ++, --, ...
 */
LLVMValueRef
gen_unary_expr(context_t *cnt, bl_node_t *expr)
{
  bl_expr_unary_t *_unary = bl_peek_expr_unary(expr);
  bl_assert(_unary->next, "invalid unary expression, next is NULL");
  LLVMValueRef next_val  = gen_expr(cnt, _unary->next);
  LLVMTypeRef  next_type = LLVMTypeOf(next_val);

  if (_unary->op == BL_SYM_MINUS || _unary->op == BL_SYM_PLUS) {
    if (should_load(_unary->next) || LLVMIsAAllocaInst(next_val)) {
      next_val  = LLVMBuildLoad(cnt->llvm_builder, next_val, gname("tmp"));
      next_type = LLVMTypeOf(next_val);
    }

    /* TODO use BL_KIND */
    LLVMTypeKind next_type_kind = LLVMGetTypeKind(next_type);

    int mult = 1;
    switch (_unary->op) {
    case BL_SYM_MINUS:
      mult = -1;
      break;
    case BL_SYM_PLUS:
      mult = 1;
      break;
    default:
      bl_abort("invalid unary operation %s", bl_sym_strings[_unary->op]);
    }

    if (next_type_kind == LLVMFloatTypeKind || next_type_kind == LLVMDoubleTypeKind) {
      LLVMValueRef cnst = LLVMConstReal(next_type, (double)mult);
      return LLVMBuildFMul(cnt->llvm_builder, cnst, next_val, "");
    }

    LLVMValueRef cnst = LLVMConstInt(next_type, mult, false);
    return LLVMBuildMul(cnt->llvm_builder, cnst, next_val, "");
  } else if (_unary->op == BL_SYM_AND) {
    /* unary operation is getting address of something "&foo" */
    LLVMValueRef indices[1];
    indices[0] = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, false);
    return LLVMBuildGEP(cnt->llvm_builder, next_val, indices, BL_ARRAY_SIZE(indices), gname("tmp"));
  } else if (_unary->op == BL_SYM_ASTERISK) {
    next_val = LLVMBuildLoad(cnt->llvm_builder, next_val, gname("tmp"));
    return next_val;
  } else {
    bl_abort("invalid unary operation %s", bl_sym_strings[_unary->op]);
  }
}

LLVMValueRef
gen_expr(context_t *cnt, bl_node_t *expr)
{
  LLVMValueRef val;
  switch (bl_node_code(expr)) {
  case BL_EXPR_LITERAL: {

    bl_expr_literal_t *cnst = bl_peek_expr_literal(expr);
    switch (bl_peek_type_fund(cnst->type)->type) {
    case BL_FTYPE_I8:
      val = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.s, true);
      break;
    case BL_FTYPE_I16:
      val = LLVMConstInt(LLVMInt16TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.s, true);
      break;
    case BL_FTYPE_I32:
      val = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.s, true);
      break;
    case BL_FTYPE_I64:
      val = LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.s, true);
      break;
    case BL_FTYPE_U8:
      val = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.s, false);
      break;
    case BL_FTYPE_U16:
      val = LLVMConstInt(LLVMInt16TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.s, false);
      break;
    case BL_FTYPE_U32:
      val = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.s, false);
      break;
    case BL_FTYPE_U64:
    case BL_FTYPE_SIZE:
      val = LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.s, false);
      break;
    case BL_FTYPE_F32:
      val = LLVMConstReal(LLVMFloatTypeInContext(cnt->llvm_cnt), cnst->value.f);
      break;
    case BL_FTYPE_F64:
      val = LLVMConstReal(LLVMDoubleTypeInContext(cnt->llvm_cnt), cnst->value.f);
      break;
    case BL_FTYPE_BOOL:
      val = LLVMConstInt(LLVMInt1TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.b, false);
      break;
    case BL_FTYPE_STRING:
      val = get_or_create_const_string(cnt, cnst->value.str);
      break;
    case BL_FTYPE_CHAR:
      val = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.c, false);
      break;
    default:
      bl_abort("invalid constant type %s", bl_node_name(expr));
    }
    break;
  }

  case BL_EXPR_MEMBER_REF: {
    val = gen_member_ref(cnt, expr);
    break;
  }

  case BL_EXPR_ARRAY_REF: {
    val = gen_array_ref(cnt, expr);
    break;
  }

  case BL_EXPR_SIZEOF: {
    bl_expr_sizeof_t *_sizeof = bl_peek_expr_sizeof(expr);
    val                       = LLVMSizeOf(to_llvm_type(cnt, _sizeof->des_type));
    break;
  }

  case BL_EXPR_UNARY: {
    val = gen_unary_expr(cnt, expr);
    break;
  }

  case BL_EXPR_NULL: {
    val = gen_null(cnt, expr);
    break;
  }

  case BL_EXPR_INIT: {
    val = gen_init(cnt, expr);
    break;
  }

  case BL_EXPR_DECL_REF: {
    bl_node_t *ref = bl_peek_expr_decl_ref(expr)->ref;

    switch (bl_node_code(ref)) {

    case BL_EXPR_INIT:
    case BL_DECL_MUT:
    case BL_DECL_ARG: {
      val = get_value_cscope(ref);
      break;
    }

    case BL_DECL_CONST: {
      bl_decl_const_t *_const = bl_peek_decl_const(ref);
      val                     = gen_expr(cnt, _const->init_expr);
      break;
    }

    case BL_DECL_ENUM_VARIANT: {
      bl_decl_enum_variant_t *variant = bl_peek_decl_enum_variant(ref);
      val                             = gen_expr(cnt, variant->expr);
      break;
    }

    default:
      bl_abort("cannot generate reference to %s", bl_node_name(ref));
    }

    bl_assert(val, "unknown symbol");
    break;
  }

  case BL_EXPR_CALL: {
    val = gen_call(cnt, expr);
    break;
  }

  case BL_EXPR_BINOP: {
    val = gen_binop(cnt, expr);
    break;
  }

  case BL_EXPR_CAST: {
    val = gen_cast(cnt, expr);
    break;
  }

  default:
    bl_abort("unknown expression type");
  }

  return val;
}

LLVMValueRef
gen_null(context_t *cnt, bl_node_t *nl)
{
  bl_expr_null_t *_null = bl_peek_expr_null(nl);
  bl_assert(_null->type, "invalid null type %s:%d:%d", nl->src->unit->filepath, nl->src->line,
            nl->src->col);
  LLVMTypeRef type = to_llvm_type(cnt, _null->type);
  return LLVMConstPointerNull(type);
}

LLVMValueRef
gen_binop(context_t *cnt, bl_node_t *binop)
{

  bl_expr_binop_t *_binop = bl_peek_expr_binop(binop);
  LLVMValueRef     lhs    = gen_expr(cnt, _binop->lhs);
  LLVMValueRef     rhs    = gen_expr(cnt, _binop->rhs);

  if (_binop->op == BL_SYM_ASSIGN) {
    /* special case for dereferencing on the right side, we need to perform additional load
     * because we use pointer to data not real data. */
    if (should_load(_binop->rhs) || LLVMIsAAllocaInst(rhs)) {
      rhs = LLVMBuildLoad(cnt->llvm_builder, rhs, gname("tmp"));
    }
    LLVMBuildStore(cnt->llvm_builder, rhs, lhs);
    return NULL;
  }

  if (should_load(_binop->lhs) || LLVMIsAAllocaInst(lhs))
    lhs = LLVMBuildLoad(cnt->llvm_builder, lhs, gname("tmp"));
  if (should_load(_binop->rhs) || LLVMIsAAllocaInst(rhs))
    rhs = LLVMBuildLoad(cnt->llvm_builder, rhs, gname("tmp"));

  LLVMTypeKind lhs_kind = LLVMGetTypeKind(LLVMTypeOf(lhs));
  LLVMTypeKind rhs_kind = LLVMGetTypeKind(LLVMTypeOf(rhs));

  bl_assert(lhs_kind == rhs_kind, "both operands of binary operation must be same type");
  bool float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

  switch (_binop->op) {
  case BL_SYM_PLUS:
    if (float_kind)
      return LLVMBuildFAdd(cnt->llvm_builder, lhs, rhs, gname("tmp"));

    return LLVMBuildAdd(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_MINUS:
    if (float_kind)
      return LLVMBuildFSub(cnt->llvm_builder, lhs, rhs, gname("tmp"));
    return LLVMBuildSub(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_ASTERISK:
    if (float_kind)
      return LLVMBuildFMul(cnt->llvm_builder, lhs, rhs, gname("tmp"));
    return LLVMBuildMul(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_SLASH:
    if (float_kind)
      return LLVMBuildFDiv(cnt->llvm_builder, lhs, rhs, gname("tmp"));
    return LLVMBuildSDiv(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_MODULO:
    return LLVMBuildSRem(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_EQ:
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOEQ, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntEQ, lhs, rhs, gname("tmp"));

  case BL_SYM_NEQ:
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealONE, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntNE, lhs, rhs, gname("tmp"));

  case BL_SYM_GREATER:
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGT, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGT, lhs, rhs, gname("tmp"));

  case BL_SYM_LESS:
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLT, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLT, lhs, rhs, gname("tmp"));

  case BL_SYM_GREATER_EQ:
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGE, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGE, lhs, rhs, gname("tmp"));

  case BL_SYM_LESS_EQ:
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLE, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLE, lhs, rhs, gname("tmp"));

  case BL_SYM_LOGIC_AND:
    return LLVMBuildAnd(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case BL_SYM_LOGIC_OR:
    return LLVMBuildOr(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  default:
    bl_abort("unknown binop");
  }

  return NULL;
}

LLVMValueRef
gen_member_ref(context_t *cnt, bl_node_t *member_ref)
{
  LLVMValueRef ptr = NULL;

  bl_expr_member_ref_t *   _member_ref = bl_peek_expr_member_ref(member_ref);
  bl_decl_struct_member_t *_member     = bl_peek_decl_struct_member(_member_ref->ref);
  ptr                                  = gen_expr(cnt, _member_ref->next);

  if (_member_ref->is_ptr_ref)
    ptr = LLVMBuildLoad(cnt->llvm_builder, ptr, gname("tmp"));

  ptr = LLVMBuildStructGEP(cnt->llvm_builder, ptr, (unsigned int)_member->order,
                           gname(_member->id.str));
  return ptr;
}

LLVMValueRef
gen_array_ref(context_t *cnt, bl_node_t *array_ref)
{
  LLVMValueRef         ptr        = NULL;
  bl_expr_array_ref_t *_array_ref = bl_peek_expr_array_ref(array_ref);
  ptr                             = gen_expr(cnt, _array_ref->next);

  LLVMValueRef index = gen_expr(cnt, _array_ref->index);

  if (should_load(_array_ref->index) || LLVMIsAAllocaInst(index))
    index = LLVMBuildLoad(cnt->llvm_builder, index, gname("tmp"));

  LLVMValueRef indices[2];
  indices[0] = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, false);
  indices[1] = index;

  ptr = LLVMBuildGEP(cnt->llvm_builder, ptr, indices, BL_ARRAY_SIZE(indices), "");

  return ptr;
}

/*************************************************************************************************
 * generate visitors
 *************************************************************************************************/

static void
visit_block(bl_visitor_t *visitor, bl_node_t **block)
{
  context_t *      cnt    = peek_cnt(visitor);
  bl_decl_block_t *_block = bl_peek_decl_block(*block);
  bl_assert(_block->parent, "block has no parent");

  if (bl_node_is(_block->parent, BL_DECL_FUNC)) {
    LLVMValueRef    llvm_func = get_value_gscope(_block->parent);
    bl_decl_func_t *func      = bl_peek_decl_func(_block->parent);
    bl_assert(llvm_func, "cannot find llvm function representation");

    cnt->func_init_block  = LLVMAppendBasicBlock(llvm_func, gname("init"));
    cnt->func_entry_block = LLVMAppendBasicBlock(llvm_func, gname("entry"));
    cnt->func_ret_block   = LLVMAppendBasicBlock(llvm_func, gname("exit"));

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_init_block);

    /*
     * Create named references to function parameters so they
     * can be called by name in function body. This is valid only
     * when this compound statement is function body.
     */
    bl_node_t *arg = func->args;
    int        i   = 0;
    while (arg) {
      LLVMValueRef p = LLVMGetParam(llvm_func, i++);
      LLVMValueRef p_tmp =
          LLVMBuildAlloca(cnt->llvm_builder, LLVMTypeOf(p), gname(bl_peek_decl_arg(arg)->id.str));
      LLVMBuildStore(cnt->llvm_builder, p, p_tmp);

      push_value_cscope(arg, p_tmp);
      arg = arg->next;
    }

    /*
     * Prepare return value.
     */
    LLVMTypeRef ret_type = to_llvm_type(cnt, func->ret_type);
    if (ret_type != LLVMVoidTypeInContext(cnt->llvm_cnt)) {
      cnt->ret_value = LLVMBuildAlloca(cnt->llvm_builder, ret_type, gname("ret"));
    } else {
      cnt->ret_value = NULL;
    }

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_entry_block);
  } else {
    skip_if_terminated(cnt);
  };

  bl_visitor_walk_block(visitor, block);

  if (bl_node_is(_block->parent, BL_DECL_FUNC)) {
    LLVMBasicBlockRef curr_block = LLVMGetInsertBlock(cnt->llvm_builder);

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_init_block);
    LLVMBuildBr(cnt->llvm_builder, cnt->func_entry_block);

    if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
      LLVMPositionBuilderAtEnd(cnt->llvm_builder, curr_block);
      LLVMBuildBr(cnt->llvm_builder, cnt->func_ret_block);
    }

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_ret_block);
    if (cnt->ret_value) {
      cnt->ret_value = LLVMBuildLoad(cnt->llvm_builder, cnt->ret_value, gname("tmp"));
      LLVMBuildRet(cnt->llvm_builder, cnt->ret_value);
    } else {
      LLVMBuildRetVoid(cnt->llvm_builder);
    }
  }
}

static void
visit_mut(bl_visitor_t *visitor, bl_node_t **mut)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  bl_decl_mut_t *   _mut       = bl_peek_decl_mut(*mut);
  LLVMBasicBlockRef prev_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMTypeRef       t          = to_llvm_type(cnt, _mut->type);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_init_block);
  LLVMValueRef llvm_mut = LLVMBuildAlloca(cnt->llvm_builder, t, gname(_mut->id.str));
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, prev_block);
  push_value_cscope(*mut, llvm_mut);

  if (_mut->init_expr) {
    LLVMValueRef init = gen_expr(cnt, _mut->init_expr);
    if (should_load(_mut->init_expr) || LLVMIsAAllocaInst(init))
      init = LLVMBuildLoad(cnt->llvm_builder, init, gname("tmp"));
    LLVMBuildStore(cnt->llvm_builder, init, llvm_mut);
  }
}

static void
visit_expr(bl_visitor_t *visitor, bl_node_t **expr)
{
  skip_if_terminated(peek_cnt(visitor));
  gen_expr(peek_cnt(visitor), *expr);
}

static void
visit_return(bl_visitor_t *visitor, bl_node_t **ret)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  bl_stmt_return_t *_ret = bl_peek_stmt_return(*ret);
  if (!_ret->expr) {
    return;
  }

  LLVMValueRef val = gen_expr(cnt, _ret->expr);

  if (should_load(_ret->expr) || LLVMIsAAllocaInst(val))
    val = LLVMBuildLoad(cnt->llvm_builder, val, gname("tmp"));

  LLVMBuildStore(cnt->llvm_builder, val, cnt->ret_value);
  LLVMBuildBr(cnt->llvm_builder, cnt->func_ret_block);
}

static void
visit_if(bl_visitor_t *visitor, bl_node_t **if_stmt)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);

  bl_stmt_if_t *    _if_stmt     = bl_peek_stmt_if(*if_stmt);
  LLVMBasicBlockRef insert_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMValueRef      parent       = LLVMGetBasicBlockParent(insert_block);
  bl_assert(LLVMIsAFunction(parent), "invalid parent");

  LLVMBasicBlockRef if_then = LLVMAppendBasicBlock(parent, gname("if_then"));
  LLVMBasicBlockRef if_else = LLVMAppendBasicBlock(parent, gname("if_else"));
  LLVMBasicBlockRef if_cont = LLVMAppendBasicBlock(parent, gname("if_cont"));
  LLVMValueRef      expr    = gen_expr(cnt, _if_stmt->test);

  if (should_load(_if_stmt->test) || LLVMIsAAllocaInst(expr))
    expr = LLVMBuildLoad(cnt->llvm_builder, expr, gname("tmp"));

  expr =
      LLVMBuildIntCast(cnt->llvm_builder, expr, LLVMInt1TypeInContext(cnt->llvm_cnt), gname("tmp"));

  /*
   * If condition break generation.
   */
  LLVMBuildCondBr(cnt->llvm_builder, expr, if_then, if_else);

  if (_if_stmt->false_stmt == NULL) {
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_else);
    LLVMBuildBr(cnt->llvm_builder, if_cont);
  }

  /* then block */
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_then);
  bl_visitor_walk_if_true(visitor, if_stmt);

  LLVMBasicBlockRef curr_block = LLVMGetInsertBlock(cnt->llvm_builder);
  if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
    LLVMBuildBr(cnt->llvm_builder, if_cont);
  }

  /* else if */
  if (_if_stmt->false_stmt != NULL) {
    /* else */
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_else);
    bl_visitor_walk_if_false(visitor, if_stmt);

    curr_block = LLVMGetInsertBlock(cnt->llvm_builder);
    if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
      LLVMBuildBr(cnt->llvm_builder, if_cont);
    }
  }

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_cont);
}

static void
visit_loop(bl_visitor_t *visitor, bl_node_t **loop)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  bl_stmt_loop_t *  _loop        = bl_peek_stmt_loop(*loop);
  LLVMBasicBlockRef insert_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMValueRef      parent       = LLVMGetBasicBlockParent(insert_block);
  bl_assert(LLVMIsAFunction(parent), "invalid parent");

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
    expr = gen_expr(cnt, _loop->test);

    if (should_load(_loop->test) || LLVMIsAAllocaInst(expr))
      expr = LLVMBuildLoad(cnt->llvm_builder, expr, gname("tmp"));
  } else {
    expr = LLVMConstInt(LLVMInt1TypeInContext(cnt->llvm_cnt), true, false);
  }

  LLVMBuildCondBr(cnt->llvm_builder, expr, loop_block, loop_cont);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_block);
  bl_visitor_walk_loop_body(visitor, loop);

  LLVMBasicBlockRef curr_block = LLVMGetInsertBlock(cnt->llvm_builder);
  if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
    LLVMBuildBr(cnt->llvm_builder, loop_decide);
  }

  cnt->break_block    = prev_break_block;
  cnt->continue_block = prev_continue_block;
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_cont);
}

static void
visit_break(bl_visitor_t *visitor, bl_node_t **brk)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  LLVMBuildBr(cnt->llvm_builder, cnt->break_block);
}

static void
visit_continue(bl_visitor_t *visitor, bl_node_t **cont)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  LLVMBuildBr(cnt->llvm_builder, cnt->continue_block);
}

/*************************************************************************************************
 * main entry functions
 *************************************************************************************************/
#if 0
static void
validate(LLVMModuleRef module)
{
  char *error;
  if (LLVMVerifyModule(module, LLVMReturnStatusAction, &error)) {
    char *str = LLVMPrintModuleToString(module);
    bl_abort("module not verified with error: %s\n%s", error, str);
  }
}
#endif

static inline bool
has_satysfied_deps(context_t *cnt, bl_node_t *fn)
{
  BList *deps = bl_ast_get_deps(fn);
  if (!deps)
    return true;

  bo_iterator_t    iter = bo_list_begin(deps);
  bo_iterator_t    end  = bo_list_end(deps);
  bl_dependency_t *dep;
  while (!bo_iterator_equal(&iter, &end)) {
    dep = &bo_list_iter_peek(deps, &iter, bl_dependency_t);

    if (bl_node_is(dep->node, BL_DECL_STRUCT)) {
      if (!has_satysfied_deps(cnt, dep->node))
	return false;
    } else if (dep->type & BL_DEP_STRICT &&
               !bo_htbl_has_key(cnt->llvm_modules, (uint64_t)dep->node)) {
      return false;
    }

    bo_list_iter_next(deps, &iter);
  }

  return true;
}

static void
generate(bl_visitor_t *visitor)
{
  context_t *     cnt   = peek_cnt(visitor);
  BList *         queue = cnt->assembly->func_queue;
  bl_node_t *     fn;
  bl_decl_func_t *_fn;
  while (!bo_list_empty(queue)) {
    fn  = bo_list_front(queue, bl_node_t *);
    _fn = bl_peek_decl_func(fn);
    bo_list_pop_front(queue);

    if (has_satysfied_deps(cnt, fn)) {
      cnt->llvm_mod =
          LLVMModuleCreateWithNameInContext(bl_peek_decl_func(fn)->id.str, cnt->llvm_cnt);

      LLVMValueRef llvm_func = gen_func(cnt, fn);

      bl_visitor_walk_func(visitor, &fn);
      reset_cscope();
      reset_gscope();

      //#define PRINT_IR
#ifdef PRINT_IR
      {
        char *str = LLVMPrintModuleToString(cnt->llvm_mod);
        bl_log("\n--------------------------------------------------------------------------------"
               "\n%s"
               "\n--------------------------------------------------------------------------------",
               str);
        LLVMDisposeMessage(str);
      }
#endif
#undef PRINT_IR

      bo_htbl_insert(cnt->llvm_modules, (uint64_t)fn, cnt->llvm_mod);

      if (_fn->modif & BL_MODIF_ENTRY) {
        cnt->assembly->llvm_main_func = llvm_func;
        cnt->tmp_main                 = fn;
      }

      if (_fn->modif & BL_MODIF_UTEST) {
	bo_array_push_back(cnt->assembly->utest_methods, fn);
      }
    } else {
      bo_list_push_back(queue, fn);
    }
  }
}

static LLVMModuleRef
_link(context_t *cnt, bl_node_t *entry)
{
  bl_decl_func_t *_entry = bl_peek_decl_func(entry);

  if (!bo_htbl_has_key(cnt->llvm_modules, (uint64_t)entry))
    return NULL;

  LLVMModuleRef dest_module = bo_htbl_at(cnt->llvm_modules, (uint64_t)entry, LLVMModuleRef);
  bl_assert("invalid llvm module for function %s", _entry->id.str);
  if (!_entry->deps)
    return dest_module;

  bo_iterator_t    iter = bo_list_begin(_entry->deps);
  bo_iterator_t    end  = bo_list_end(_entry->deps);
  bl_dependency_t *dep;
  while (!bo_iterator_equal(&iter, &end)) {
    dep = &bo_list_iter_peek(_entry->deps, &iter, bl_dependency_t);
    bo_list_iter_next(_entry->deps, &iter);

    /* link all lax dependencies */
    if (dep->type & BL_DEP_LAX) {
      /* must be linked */
      LLVMModuleRef src_module = _link(cnt, dep->node);

      if (src_module) {
        if (LLVMLinkModules2(dest_module, src_module))
          bl_abort("unable to link modules");

        bo_htbl_erase_key(cnt->llvm_modules, (uint64_t)dep->node);
      }
    }
  }

  return dest_module;
}

LLVMModuleRef
link(context_t *cnt, bl_node_t *entry)
{
  if (!entry)
    return NULL;

  LLVMModuleRef dest_module = _link(cnt, entry);

  //#define PRINT_IR
#ifdef PRINT_IR
  {
    char *str = LLVMPrintModuleToString(dest_module);
    bl_log("\n--------------------------------------------------------------------------------"
           "\n%s"
           "\n--------------------------------------------------------------------------------",
           str);
    LLVMDisposeMessage(str);
  }
#endif
#undef PRINT_IR
  return dest_module;
}

bl_error_e
bl_llvm_gen_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  /* context initialization */
  context_t cnt    = {0};
  cnt.builder      = builder;
  cnt.assembly     = assembly;
  cnt.llvm_cnt     = LLVMContextCreate();
  cnt.llvm_builder = LLVMCreateBuilderInContext(cnt.llvm_cnt);
  cnt.cscope       = bo_htbl_new(sizeof(LLVMValueRef), 256);
  cnt.gscope       = bo_htbl_new(sizeof(LLVMValueRef), 2048);
  cnt.llvm_modules = bo_htbl_new(sizeof(LLVMModuleRef), bo_list_size(assembly->func_queue));
  cnt.jit_linked   = bo_htbl_new(0, bo_list_size(assembly->func_queue));
  cnt.llvm_jit     = create_jit(&cnt);
  cnt.tmp_main     = NULL;

  /* prepare visitor */
  bl_visitor_t visitor;
  bl_visitor_init(&visitor, &cnt);

  bl_visitor_add(&visitor, visit_mut, BL_VISIT_MUT);
  bl_visitor_add(&visitor, visit_expr, BL_VISIT_EXPR);
  bl_visitor_add(&visitor, visit_block, BL_VISIT_BLOCK);
  bl_visitor_add(&visitor, visit_return, BL_VISIT_RETURN);
  bl_visitor_add(&visitor, visit_if, BL_VISIT_IF);
  bl_visitor_add(&visitor, visit_loop, BL_VISIT_LOOP);
  bl_visitor_add(&visitor, visit_break, BL_VISIT_BREAK);
  bl_visitor_add(&visitor, visit_continue, BL_VISIT_CONTINUE);

  generate(&visitor);

  if (cnt.tmp_main) {
    assembly->llvm_module = link(&cnt, cnt.tmp_main);
    bo_htbl_erase_key(cnt.llvm_modules, (uint64_t)cnt.tmp_main);
  }

  /* link all utests */
  const size_t uc = bo_array_size(assembly->utest_methods);
  bl_node_t *utest;
  for (size_t i = 0; i < uc; ++i) {
    utest = bo_array_at(assembly->utest_methods, i, bl_node_t *);
    link_into_jit(&cnt, utest);
  }

  /* context destruction */
  LLVMDisposeBuilder(cnt.llvm_builder);
  bo_unref(cnt.gscope);
  bo_unref(cnt.cscope);
  bo_unref(cnt.llvm_modules);
  bo_unref(cnt.jit_linked);

  assembly->llvm_cnt = cnt.llvm_cnt;
  assembly->llvm_jit = cnt.llvm_jit;

  return BL_NO_ERR;
}
