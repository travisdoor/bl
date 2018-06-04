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
#include <bobject/containers/hash.h>

#include "common_impl.h"
#include "stages_impl.h"
#include "visitor_impl.h"

#define peek_cnt(visitor) ((context_t *)(visitor)->context)
#define push_value_cscope(ptr, llvm_value_ref)                                                     \
  (bo_htbl_insert(cnt->cscope, (uint64_t)(ptr), (llvm_value_ref)))
#define push_value_gscope(ptr, llvm_value_ref)                                                     \
  (bo_htbl_insert(cnt->gscope, (uint64_t)(ptr), (llvm_value_ref)))
#define get_value_cscope(ptr) (bo_htbl_at(cnt->cscope, (uint64_t)(ptr), LLVMValueRef))
#define get_value_gscope(ptr) (bo_htbl_at(cnt->gscope, (uint64_t)(ptr), LLVMValueRef))
#define is_in_gscope(ptr) (bo_htbl_has_key(cnt->gscope, (uint64_t)ptr))
#define is_in_cscope(ptr) (bo_htbl_has_key(cnt->cscope, (uint64_t)ptr))
#define reset_cscope() (bo_htbl_clear(cnt->cscope))
#define skip_if_terminated(cnt)                                                                    \
  if (LLVMGetBasicBlockTerminator(LLVMGetInsertBlock((cnt)->llvm_builder)) != NULL)                \
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
  bl_builder_t * builder;
  bl_assembly_t *assembly;

  /* hash set of unique pointers to ast nodes of the exported functions */
  BHashTable *gen_stack;
  BHashTable *gen_stack_extern;

  LLVMModuleRef  mod;
  LLVMBuilderRef llvm_builder;
  LLVMContextRef llvm_cnt;

  /* tmps */
  /* mapping ast node pointers to LLVMValues for symbols in global scope */
  BHashTable *gscope;

  /* mapping ast node pointers to LLVMValues for symbols in current function scope */
  BHashTable *cscope;

  /* constant string cache */
  BHashTable *const_strings;

  /* LLVM blocks */
  LLVMBasicBlockRef func_init_block;
  LLVMBasicBlockRef func_ret_block;
  LLVMBasicBlockRef func_entry_block;

  /* used for loop generation (statements break and continue) */
  LLVMBasicBlockRef continue_block;
  LLVMBasicBlockRef break_block;

  /* LLVMValueRef to current return tmp */
  LLVMValueRef ret_value;
} context_t;

static LLVMValueRef
gen_init(context_t *cnt, bl_node_t *init);

static int
gen_func_args(context_t *cnt, bl_decl_func_t *func, LLVMTypeRef *out);

static LLVMValueRef
gen_func(context_t *cnt, bl_node_t *func);

static LLVMValueRef
gen_call(context_t *cnt, bl_node_t *call);

static int
gen_call_args(context_t *cnt, bl_node_t *call, LLVMValueRef *out);

static LLVMValueRef
get_or_create_const_string(context_t *cnt, const char *str);

static LLVMValueRef
gen_binop(context_t *cnt, bl_node_t *binop);

static LLVMValueRef
gen_member_ref(context_t *cnt, bl_node_t *member_ref);

static LLVMValueRef
gen_array_ref(context_t *cnt, bl_node_t *array_ref);

/* static LLVMValueRef */
/* gen_array_ref_1(context_t *cnt, bl_node_t *array, size_t *dim_mult, BArray **dims, */
/*                 size_t *dims_iter, size_t *iter); */

#if 0
static LLVMValueRef
gen_default(context_t *cnt, bl_node_t *type);
#endif

static LLVMValueRef
gen_expr(context_t *cnt, bl_node_t *expr);

static LLVMValueRef
gen_null(context_t *cnt, bl_node_t *nl);

static LLVMValueRef
gen_unary_expr(context_t *cnt, bl_node_t *expr);

static LLVMTypeRef
gen_struct(context_t *cnt, bl_node_t *strct);

static LLVMTypeRef
gen_enum(context_t *cnt, bl_node_t *enm);

static LLVMTypeRef
to_llvm_type(context_t *cnt, bl_node_t *type);

static inline bool
should_load(bl_node_t *node)
{
  return bl_node_is(node, BL_EXPR_ARRAY_REF) || bl_node_is(node, BL_EXPR_MEMBER_REF) ||
         is_deref(node);
}

/*
 * convert known type to LLVM type representation
 */
LLVMTypeRef
to_llvm_type(context_t *cnt, bl_node_t *type)
{
  LLVMTypeRef llvm_type = NULL;
  size_t      size      = 0;
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

    size = bl_ast_type_fund_dim_total_size(_type);
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

    size = bl_ast_type_ref_dim_total_size(_type);
  }

  if (is_ptr) {
    llvm_type = LLVMPointerType(llvm_type, 0);
  }

  /* type is array */
  if (size) {
    llvm_type = LLVMArrayType(llvm_type, size);
  }

  return llvm_type;
}

LLVMValueRef
gen_init(context_t *cnt, bl_node_t *init)
{
  bl_expr_init_t *_init = bl_peek_expr_init(init);

  bl_assert(_init->type, "invalid type for initialization list");
  LLVMValueRef    result =
      LLVMBuildAlloca(cnt->llvm_builder, to_llvm_type(cnt, _init->type), gname("tmp"));
  push_value_cscope(init, result);

  const size_t c    = bl_ast_init_expr_count(_init);
  bl_node_t *  expr = NULL;

  for (size_t i = 0; i < c; ++i) {
    expr = bl_ast_init_get_expr(_init, i);
    gen_expr(cnt, expr);
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

  const size_t c       = bl_ast_struct_member_count(_strct);
  LLVMTypeRef *members = bl_malloc(sizeof(LLVMTypeRef) * c);
  bl_node_t *  member;

  for (size_t i = 0; i < c; ++i) {
    member     = bl_ast_struct_get_member(_strct, i);
    members[i] = to_llvm_type(cnt, bl_peek_decl_struct_member(member)->type);
  }

  LLVMStructSetBody(type, members, (unsigned int)c, false);
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
  LLVMTypeRef     dest_type = to_llvm_type(cnt, _cast->to_type);
  LLVMValueRef    next      = gen_expr(cnt, _cast->next);

  if (should_load(_cast->next) || LLVMIsAAllocaInst(next)) {
    next = LLVMBuildLoad(cnt->llvm_builder, next, gname("tmp"));
  }

  LLVMTypeKind src_kind  = LLVMGetTypeKind(LLVMTypeOf(next));
  LLVMTypeKind dest_kind = LLVMGetTypeKind(dest_type);

  LLVMOpcode op = LLVMZExt;

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
  int          out_i = 0;
  const size_t c     = bl_ast_func_arg_count(func);

  /* no args */
  if (c == 0) {
    return 0;
  }

  bl_node_t *arg = NULL;
  for (size_t i = 0; i < c; ++i) {
    arg  = bl_ast_func_get_arg(func, i);
    *out = to_llvm_type(cnt, bl_peek_decl_arg(arg)->type);

    out++;
    out_i++;
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
    llvm_func = LLVMGetNamedFunction(cnt->mod, _func->id.str);
  } else if (is_in_gscope(func)) {
    llvm_func = get_value_gscope(func);
  }

  /* args */
  LLVMTypeRef param_types[BL_MAX_FUNC_ARG_COUNT] = {0};
  const int   pc                                 = gen_func_args(cnt, _func, param_types);

  if (llvm_func == NULL) {
    char name_tmp[BL_MAX_FUNC_NAME_LEN];
    if (!(_func->modif & BL_MODIF_EXTERN) && !(_func->modif & BL_MODIF_EXPORT))
      snprintf(&name_tmp[0], BL_MAX_FUNC_NAME_LEN, "bl_%s", _func->id.str);
    else
      snprintf(&name_tmp[0], BL_MAX_FUNC_NAME_LEN, "%s", _func->id.str);

    LLVMTypeRef ret      = to_llvm_type(cnt, _func->ret_type);
    LLVMTypeRef ret_type = LLVMFunctionType(ret, param_types, (unsigned int)pc, false);
    llvm_func            = LLVMAddFunction(cnt->mod, &name_tmp[0], ret_type);

    if (!(_func->modif & BL_MODIF_EXTERN))
      push_value_gscope(func, llvm_func);

    if (!bo_htbl_has_key(cnt->gen_stack, (uint64_t)func)) {
      bo_htbl_insert_empty(cnt->gen_stack, (uint64_t)func);
    }
  }

  return llvm_func;
}

int
gen_call_args(context_t *cnt, bl_node_t *call, LLVMValueRef *out)
{
  int             out_i = 0;
  bl_expr_call_t *_call = bl_peek_expr_call(call);
  const int       c     = bl_ast_call_arg_count(_call);

  /* no args */
  if (c == 0) {
    return 0;
  }

  bl_node_t *expr = NULL;
  for (int i = 0; i < c; ++i) {
    expr             = bl_ast_call_get_arg(_call, i);
    LLVMValueRef val = gen_expr(cnt, expr);

    if (should_load(expr) || LLVMIsAAllocaInst(val)) {
      *out = LLVMBuildLoad(cnt->llvm_builder, val, gname("tmp"));
    } else {
      *out = val;
    }

    out++;
    out_i++;
  }

  return out_i;
}

/*
 * generate method call and return value of method return
 */
LLVMValueRef
gen_call(context_t *cnt, bl_node_t *call)
{
  bl_expr_call_t *_call                       = bl_peek_expr_call(call);
  LLVMValueRef    fn                          = gen_func(cnt, _call->ref);
  LLVMValueRef    args[BL_MAX_FUNC_ARG_COUNT] = {0};
  int             argc                        = gen_call_args(cnt, call, args);

  return LLVMBuildCall(cnt->llvm_builder, fn, args, argc, "");
}

LLVMValueRef
get_or_create_const_string(context_t *cnt, const char *str)
{
  uint32_t hash = bo_hash_from_str(str);
  if (bo_htbl_has_key(cnt->const_strings, hash))
    return bo_htbl_at(cnt->const_strings, hash, LLVMValueRef);

  LLVMValueRef s = LLVMBuildGlobalString(cnt->llvm_builder, str, "str");

  s = LLVMConstPointerCast(s, LLVMPointerType(LLVMInt8TypeInContext(cnt->llvm_cnt), 0));
  bo_htbl_insert(cnt->const_strings, hash, s);
  return s;
}

#if 0
/*
 * Generate default value for known type.
 * For string we create global string array with line terminator.
 */
static LLVMValueRef
gen_default(context_t *cnt, bl_node_t *type)
{
  LLVMTypeRef llvm_type = to_llvm_type(cnt, type);
  if (bl_node_code(type) == BL_TYPE_FUND) {
    bl_type_fund_t *_type = bl_peek_type_fund(type);

    if (_type->is_ptr)
      return LLVMConstPointerNull(llvm_type);

    /* skip arrays */
    if (_type->dims)
      return NULL;

    switch (_type->type) {
    case BL_FTYPE_CHAR:
    case BL_FTYPE_I8:
    case BL_FTYPE_I16:
    case BL_FTYPE_I32:
    case BL_FTYPE_I64:
    case BL_FTYPE_U8:
    case BL_FTYPE_U16:
    case BL_FTYPE_U32:
    case BL_FTYPE_U64:
    case BL_FTYPE_BOOL:
      return LLVMConstInt(llvm_type, 0, false);
    case BL_FTYPE_F32:
    case BL_FTYPE_F64:
      return LLVMConstReal(llvm_type, 0);
    case BL_FTYPE_STRING: {
      return get_or_create_const_string(cnt, "\0");
    }
    default:
      return NULL;
    }
  } else if (bl_node_code(type) == BL_TYPE_REF) {
    if (bl_peek_type_ref(type)->is_ptr)
      return LLVMConstPointerNull(llvm_type);

    /* skip arrays */
    if (bl_peek_type_ref(type)->dims)
      return NULL;

    bl_node_t *ref = bl_peek_type_ref(type)->ref;

    switch (bl_node_code(ref)) {
    case BL_DECL_ENUM: {
      bl_decl_enum_t *enm = bl_peek_decl_enum(ref);

      if (bl_ast_enum_get_count(enm) == 0)
        return NULL;

      bl_node_t *def_variant = bl_ast_enum_get_variant(enm, 0);
      bl_assert(bl_peek_decl_enum_variant(def_variant)->expr,
                "every enum varaint must have constant initializer");
      return gen_expr(cnt, bl_peek_decl_enum_variant(def_variant)->expr);
    }

    default:
      // bl_warning("LLVM cannot generate default value for node type: %s", bl_node_name(ref));
      return NULL;
    }
  }

  return NULL;
}
#endif

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
  case BL_EXPR_CONST: {

    bl_expr_const_t *cnst = bl_peek_expr_const(expr);
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
    val                       = LLVMSizeOf(to_llvm_type(cnt, _sizeof->type));
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
    case BL_DECL_VAR:
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
  bl_assert(_null->type, "invalid null type %s:%d:%d", nl->src->file, nl->src->line, nl->src->col);
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

/*
 * generate all functions in gen_stack
 */
static void
generate(bl_visitor_t *visitor)
{
  context_t *   cnt  = peek_cnt(visitor);
  bl_node_t *   node = NULL;
  bo_iterator_t begin;

  while (bo_htbl_size(cnt->gen_stack_extern) > 0) {
    begin = bo_htbl_begin(cnt->gen_stack_extern);
    node  = bo_htbl_iter_peek_value(cnt->gen_stack, &begin, bl_node_t *);

    gen_func(cnt, node);
    bo_htbl_erase(cnt->gen_stack_extern, &begin);
  }

  /* generate all exported functions and functions used inside */
  while (bo_htbl_size(cnt->gen_stack) > 0) {
    begin = bo_htbl_begin(cnt->gen_stack);
    node  = (bl_node_t *)bo_htbl_iter_peek_key(cnt->gen_stack, &begin);

    gen_func(cnt, node);
    bl_visitor_walk_func(visitor, node);
    reset_cscope();
    bo_htbl_erase(cnt->gen_stack, &begin);
  }
}

static void
visit_block(bl_visitor_t *visitor, bl_node_t *block)
{
  context_t *      cnt    = peek_cnt(visitor);
  bl_decl_block_t *_block = bl_peek_decl_block(block);
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
    const size_t pc = bl_ast_func_arg_count(func);
    for (int i = 0; i < pc; ++i) {
      bl_node_t *arg = bl_ast_func_get_arg(func, i);

      LLVMValueRef p = LLVMGetParam(llvm_func, i);
      LLVMValueRef p_tmp =
          LLVMBuildAlloca(cnt->llvm_builder, LLVMTypeOf(p), gname(bl_peek_decl_arg(arg)->id.str));
      LLVMBuildStore(cnt->llvm_builder, p, p_tmp);

      push_value_cscope(arg, p_tmp);
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
visit_var(bl_visitor_t *visitor, bl_node_t *var)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  bl_decl_var_t *   _var       = bl_peek_decl_var(var);
  LLVMBasicBlockRef prev_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMTypeRef       t          = to_llvm_type(cnt, _var->type);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_init_block);
  LLVMValueRef llvm_var = LLVMBuildAlloca(cnt->llvm_builder, t, gname(_var->id.str));
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, prev_block);
  push_value_cscope(var, llvm_var);

  /* TODO: remove when default initialization of arrays will be implemented */
  if (!_var->init_expr) {
    bl_warning("missing initialization of variable");
    return;
  }

  bl_assert(_var->init_expr, "invalid init expression for variable declaration");
  LLVMValueRef init = gen_expr(cnt, _var->init_expr);
  if (init) {
    if (should_load(_var->init_expr) || LLVMIsAAllocaInst(init))
      init = LLVMBuildLoad(cnt->llvm_builder, init, gname("tmp"));
    LLVMBuildStore(cnt->llvm_builder, init, llvm_var);
  }
}

static void
visit_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  skip_if_terminated(peek_cnt(visitor));
  gen_expr(peek_cnt(visitor), expr);
}

static void
visit_return(bl_visitor_t *visitor, bl_node_t *ret)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  bl_stmt_return_t *_ret = bl_peek_stmt_return(ret);
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
visit_if(bl_visitor_t *visitor, bl_node_t *if_stmt)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);

  bl_stmt_if_t *    _if_stmt     = bl_peek_stmt_if(if_stmt);
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
visit_loop(bl_visitor_t *visitor, bl_node_t *loop)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  bl_stmt_loop_t *  _loop        = bl_peek_stmt_loop(loop);
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
visit_break(bl_visitor_t *visitor, bl_node_t *brk)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  LLVMBuildBr(cnt->llvm_builder, cnt->break_block);
}

static void
visit_continue(bl_visitor_t *visitor, bl_node_t *cont)
{
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  LLVMBuildBr(cnt->llvm_builder, cnt->continue_block);
}
/*************************************************************************************************
 * top level visitors
 * here we decide which functions should be generated
 *************************************************************************************************/
static void
visit_func(bl_visitor_t *visitor, bl_node_t *func)
{
  context_t *     cnt   = peek_cnt(visitor);
  bl_decl_func_t *_func = bl_peek_decl_func(func);
  if (_func->modif & BL_MODIF_EXPORT) {
    /* generate exported functions */
    bo_htbl_insert_empty(cnt->gen_stack, (uint64_t)func);
  } else if (_func->modif & BL_MODIF_EXTERN) {
    if (!bo_htbl_has_key(cnt->gen_stack_extern, _func->id.hash))
      bo_htbl_insert(cnt->gen_stack_extern, _func->id.hash, func);
  }
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/
bl_error_e
bl_llvm_gen_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  bl_unit_t *unit = NULL;
  size_t     c    = bo_array_size(assembly->units);

  /* context initialization */
  context_t cnt        = {0};
  cnt.builder          = builder;
  cnt.llvm_cnt         = LLVMContextCreate();
  cnt.mod              = LLVMModuleCreateWithNameInContext(assembly->name, cnt.llvm_cnt);
  cnt.llvm_builder     = LLVMCreateBuilderInContext(cnt.llvm_cnt);
  cnt.gscope           = bo_htbl_new(sizeof(LLVMValueRef), 2048);
  cnt.cscope           = bo_htbl_new(sizeof(LLVMValueRef), 256);
  cnt.const_strings    = bo_htbl_new(sizeof(LLVMValueRef), 256);
  cnt.gen_stack_extern = bo_htbl_new(sizeof(bl_node_t), 2048);
  cnt.gen_stack        = bo_htbl_new(0, 2048);

  bl_visitor_t top_visitor;
  bl_visitor_t gen_visitor;

  bl_visitor_init(&top_visitor, &cnt);
  bl_visitor_init(&gen_visitor, &cnt);

  bl_visitor_add(&top_visitor, visit_func, BL_VISIT_FUNC);

  bl_visitor_add(&gen_visitor, visit_var, BL_VISIT_VAR);
  bl_visitor_add(&gen_visitor, visit_expr, BL_VISIT_EXPR);
  bl_visitor_add(&gen_visitor, visit_block, BL_VISIT_BLOCK);
  bl_visitor_add(&gen_visitor, visit_return, BL_VISIT_RETURN);
  bl_visitor_add(&gen_visitor, visit_if, BL_VISIT_IF);
  bl_visitor_add(&gen_visitor, visit_loop, BL_VISIT_LOOP);
  bl_visitor_add(&gen_visitor, visit_break, BL_VISIT_BREAK);
  bl_visitor_add(&gen_visitor, visit_continue, BL_VISIT_CONTINUE);

  for (int i = 0; i < c; ++i) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&top_visitor, unit->ast.root);
  }

  generate(&gen_visitor);

#ifdef BL_DEBUG
  char *error;
  if (LLVMVerifyModule(cnt.mod, LLVMReturnStatusAction, &error)) {
    char *str = LLVMPrintModuleToString(cnt.mod);
    bl_abort("module not verified with error: %s\n%s", error, str);
  }
#endif

  /* context destruction */
  LLVMDisposeBuilder(cnt.llvm_builder);
  bo_unref(cnt.gscope);
  bo_unref(cnt.cscope);
  bo_unref(cnt.const_strings);
  bo_unref(cnt.gen_stack);
  bo_unref(cnt.gen_stack_extern);

  assembly->llvm_module = cnt.mod;
  assembly->llvm_cnt    = cnt.llvm_cnt;

  return BL_NO_ERR;
}
