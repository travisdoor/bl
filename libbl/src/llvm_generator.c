//*****************************************************************************
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
//*****************************************************************************

#include <setjmp.h>
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <bobject/containers/hash.h>

#include "common_impl.h"
#include "stages_impl.h"
#include "ast/visitor_impl.h"

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

#define DEBUG_NAMES 1

#if DEBUG_NAMES
#define gname(s) s
#else
#define gname(s) ""
#endif

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  BHashTable *   gen_stack;

  LLVMModuleRef  mod;
  LLVMBuilderRef llvm_builder;
  LLVMContextRef llvm_cnt;

  /* tmps */
  BHashTable *      gscope;
  BHashTable *      cscope;
  BHashTable *      const_strings;
  LLVMBasicBlockRef func_init_block;
  LLVMBasicBlockRef func_ret_block;
  LLVMBasicBlockRef func_entry_block;
  LLVMValueRef      ret_value;
} context_t;

static int
gen_func_args(context_t *cnt, bl_decl_func_t *func, LLVMTypeRef *out);

static LLVMValueRef
gen_func(context_t *cnt, bl_node_t *func);

static LLVMValueRef
gen_call(context_t *cnt, bl_node_t *call);

static LLVMValueRef
get_or_create_const_string(context_t *cnt, const char *str);

static LLVMValueRef
gen_binop(context_t *cnt, bl_node_t *binop);

static LLVMValueRef
gen_default(context_t *cnt, bl_node_t *type);

static int
gen_call_args(context_t *cnt, bl_node_t *call, LLVMValueRef *out);

static LLVMValueRef
gen_expr(context_t *cnt, bl_node_t *expr);

static LLVMTypeRef
to_llvm_type(context_t *cnt, bl_node_t *type);

/*
 * convert known type to LLVM type representation
 */
LLVMTypeRef
to_llvm_type(context_t *cnt, bl_node_t *type)
{
  if (bl_node_code(type) == BL_TYPE_FUND) {
    bl_type_fund_t *_type = bl_peek_type_fund(type);
    switch (_type->type) {
    case BL_FTYPE_VOID:
      return LLVMVoidTypeInContext(cnt->llvm_cnt);
    case BL_FTYPE_CHAR:
    case BL_FTYPE_I8:
    case BL_FTYPE_U8:
      return LLVMInt8TypeInContext(cnt->llvm_cnt);
    case BL_FTYPE_I32:
    case BL_FTYPE_U32:
      return LLVMInt32TypeInContext(cnt->llvm_cnt);
    case BL_FTYPE_I64:
    case BL_FTYPE_PTR:
    case BL_FTYPE_U64:
      return LLVMInt64TypeInContext(cnt->llvm_cnt);
    case BL_FTYPE_F32:
      return LLVMFloatTypeInContext(cnt->llvm_cnt);
    case BL_FTYPE_F64:
      return LLVMDoubleTypeInContext(cnt->llvm_cnt);
    case BL_FTYPE_STRING:
      return LLVMPointerType(LLVMInt8TypeInContext(cnt->llvm_cnt), 0);
    case BL_FTYPE_BOOL:
      return LLVMInt1TypeInContext(cnt->llvm_cnt);
    default:
      bl_abort("invalide type");
    }
  }

  bl_abort("invalid type");
}

int
gen_func_args(context_t *cnt, bl_decl_func_t *func, LLVMTypeRef *out)
{
  int       out_i = 0;
  const int c     = bl_ast_func_arg_count(func);

  /* no args */
  if (c == 0) {
    return 0;
  }

  bl_node_t *arg = NULL;
  for (int i = 0; i < c; i++) {
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
  if (is_in_gscope(func)) {
    llvm_func = get_value_gscope(func);
  }

  /* args */
  LLVMTypeRef param_types[BL_MAX_FUNC_ARG_COUNT] = {0};
  const int   pc                                 = gen_func_args(cnt, _func, param_types);

  if (llvm_func == NULL) {
    LLVMTypeRef ret      = to_llvm_type(cnt, _func->ret_type);
    LLVMTypeRef ret_type = LLVMFunctionType(ret, param_types, (unsigned int)pc, false);
    llvm_func            = LLVMAddFunction(cnt->mod, _func->id.str, ret_type);

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
  int       out_i = 0;
  const int c     = bl_ast_call_arg_count(call);

  /* no args */
  if (c == 0) {
    return 0;
  }

  bl_node_t *expr = NULL;
  for (int i = 0; i < c; i++) {
    expr             = bl_ast_call_get_arg(call, i);
    LLVMValueRef val = gen_expr(cnt, expr);
    if (LLVMIsAAllocaInst(val)) {
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

/*
 * Generate default value for known type.
 * For string we create global string array with line terminator.
 */
static LLVMValueRef
gen_default(context_t *cnt, bl_node_t *type)
{
  if (bl_node_code(type) == BL_TYPE_FUND) {
    switch (bl_peek_type_fund(type)->type) {
    case BL_FTYPE_CHAR:
      return LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), 0, true);
    case BL_FTYPE_I8:
      return LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), 0, true);
    case BL_FTYPE_I32:
      return LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, true);
    case BL_FTYPE_I64:
      return LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt), 0, true);
    case BL_FTYPE_U8:
      return LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), 0, false);
    case BL_FTYPE_U32:
      return LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, false);
    case BL_FTYPE_PTR:
    case BL_FTYPE_U64:
      return LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt), 0, false);
    case BL_FTYPE_F32:
      return LLVMConstReal(LLVMFloatTypeInContext(cnt->llvm_cnt), 0);
    case BL_FTYPE_F64:
      return LLVMConstReal(LLVMDoubleTypeInContext(cnt->llvm_cnt), 0);
    case BL_FTYPE_BOOL:
      return LLVMConstInt(LLVMInt1TypeInContext(cnt->llvm_cnt), 0, false);
    case BL_FTYPE_STRING: {
      return get_or_create_const_string(cnt, "\0");
    }
    default:
      return NULL;
    }
  }
  bl_abort("unimplemented default value setting for non-fundamental types");
}

LLVMValueRef
gen_expr(context_t *cnt, bl_node_t *expr)
{
  LLVMValueRef val;
  switch (bl_node_code(expr)) {
  case BL_EXPR_CONST: {

    bl_expr_const_t *cnst = bl_peek_expr_const(expr);
    switch (bl_peek_type_fund(cnst->type)->type) {
    case BL_FTYPE_I32:
      val = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt),
                         (unsigned long long int)cnst->value.s, true);
      break;
    case BL_FTYPE_F32:
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
      bl_abort("invalid constant type");
    }
    break;
  }

  case BL_EXPR_VAR_REF: {
    val = get_value_cscope(bl_peek_expr_var_ref(expr)->ref);
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

  default:
    bl_abort("unknown expression type");
  }

  return val;
}

LLVMValueRef
gen_binop(context_t *cnt, bl_node_t *binop)
{
  bl_expr_binop_t *_binop = bl_peek_expr_binop(binop);
  LLVMValueRef     lhs    = gen_expr(cnt, _binop->lhs);
  LLVMValueRef     rhs    = gen_expr(cnt, _binop->rhs);

  if (_binop->op == BL_SYM_ASIGN) {
    if (LLVMIsAAllocaInst(rhs))
      rhs = LLVMBuildLoad(cnt->llvm_builder, rhs, gname("tmp"));

    LLVMBuildStore(cnt->llvm_builder, rhs, lhs);
    return NULL;
  }

  if (LLVMIsAAllocaInst(lhs))
    lhs = LLVMBuildLoad(cnt->llvm_builder, lhs, gname("tmp"));

  if (LLVMIsAAllocaInst(rhs))
    rhs = LLVMBuildLoad(cnt->llvm_builder, rhs, gname("tmp"));

  switch (_binop->op) {
  case BL_SYM_PLUS:
    return LLVMBuildAdd(cnt->llvm_builder, lhs, rhs, gname("tmp"));
  case BL_SYM_MINUS:
    return LLVMBuildSub(cnt->llvm_builder, lhs, rhs, gname("tmp"));
  case BL_SYM_ASTERISK:
    return LLVMBuildMul(cnt->llvm_builder, lhs, rhs, gname("tmp"));
  case BL_SYM_SLASH:
    return LLVMBuildFDiv(cnt->llvm_builder, lhs, rhs, gname("tmp"));
  case BL_SYM_EQ:
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntEQ, lhs, rhs, gname("tmp"));
  case BL_SYM_NEQ:
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntNE, lhs, rhs, gname("tmp"));
  case BL_SYM_GREATER:
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGT, lhs, rhs, gname("tmp"));
  case BL_SYM_LESS:
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLT, lhs, rhs, gname("tmp"));
  case BL_SYM_GREATER_EQ:
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGE, lhs, rhs, gname("tmp"));
  case BL_SYM_LESS_EQ:
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
    for (int i = 0; i < pc; i++) {
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

  /*
   * Generate expression if there is one or use default value instead.
   */
  LLVMValueRef def = NULL;
  if (_var->init_expr) {
    def = gen_expr(cnt, _var->init_expr);
  } else {
    def = gen_default(cnt, _var->type);
  }

  // TODO: can't generate default values for struct members
  if (def) {
    if (LLVMIsAAllocaInst(def)) {
      def = LLVMBuildLoad(cnt->llvm_builder, def, gname("tmp"));
    }

    LLVMBuildStore(cnt->llvm_builder, def, llvm_var);
  }

  push_value_cscope(var, llvm_var);
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

  if (LLVMIsAAllocaInst(val))
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

  if (LLVMIsAAllocaInst(expr))
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
    // TODO: terminated
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
  /*
  bl_log("loop");
  context_t *cnt = peek_cnt(visitor);
  skip_if_terminated(cnt);
  bl_stmt_loop_t *  _loop        = bl_peek_stmt_loop(loop);
  LLVMBasicBlockRef insert_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMValueRef      parent       = LLVMGetBasicBlockParent(insert_block);
  bl_assert(LLVMIsAFunction(parent), "invalid parent");

  bool terminated = false;

  LLVMBasicBlockRef loop_decide = LLVMAppendBasicBlock(parent, gname("loop_decide"));
  LLVMBasicBlockRef loop_block  = LLVMAppendBasicBlock(parent, gname("loop"));
  LLVMBasicBlockRef loop_cont   = LLVMAppendBasicBlock(parent, gname("loop_cont"));
  LLVMValueRef      expr        = NULL;

  LLVMBuildBr(cnt->llvm_builder, loop_decide);
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_decide);

  if (_loop->test) {
    expr = gen_expr(cnt, loop_stmt->expr);

    if (LLVMIsAAllocaInst(expr))
      expr = LLVMBuildLoad(cnt->llvm_builder, expr, gname("tmp"));
  } else {
    expr = LLVMConstInt(LLVMInt1TypeInContext(cnt->llvm_cnt), true, false);
  }

  LLVMBuildCondBr(cnt->llvm_builder, expr, loop, loop_cont);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop);
  bl_llvm_bl_cnt_push_block(&cnt->block_context);
  terminated = gen_cmp_stmt(cnt, loop_stmt->cmp_stmt, loop_cont, loop_decide);
  bl_llvm_bl_cnt_pop_block(&cnt->block_context);

  if (!terminated) {
    LLVMBuildBr(cnt->llvm_builder, loop_decide);
  }

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_cont);
  */
}

/*************************************************************************************************
 * top level visitors
 * here we decide which functions should be generated
 *************************************************************************************************/
static void
visit_func(bl_visitor_t *visitor, bl_node_t *func)
{
  context_t *cnt = peek_cnt(visitor);
  if (bl_peek_decl_func(func)->modif & BL_MODIF_EXPORT) {
    /* generate exported functions */
    bo_htbl_insert_empty(cnt->gen_stack, (uint64_t)func);
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
  context_t cnt     = {0};
  cnt.builder       = builder;
  cnt.llvm_cnt      = LLVMContextCreate();
  cnt.mod           = LLVMModuleCreateWithNameInContext(assembly->name, cnt.llvm_cnt);
  cnt.llvm_builder  = LLVMCreateBuilderInContext(cnt.llvm_cnt);
  cnt.gscope        = bo_htbl_new(sizeof(LLVMValueRef), 2048);
  cnt.cscope        = bo_htbl_new(sizeof(LLVMValueRef), 256);
  cnt.const_strings = bo_htbl_new(sizeof(LLVMValueRef), 256);
  cnt.gen_stack     = bo_htbl_new(0, 2048);

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

  for (int i = 0; i < c; i++) {
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

  /*char *str = LLVMPrintModuleToString(cnt.mod);*/
  /*bl_log("\n%s", str);*/
  /*LLVMDisposeMessage(str);*/

  /* context destruction */
  LLVMDisposeBuilder(cnt.llvm_builder);
  bo_unref(cnt.gscope);
  bo_unref(cnt.cscope);
  bo_unref(cnt.const_strings);
  bo_unref(cnt.gen_stack);

  assembly->llvm_module = cnt.mod;
  assembly->llvm_cnt    = cnt.llvm_cnt;

  return BL_NO_ERR;
}
