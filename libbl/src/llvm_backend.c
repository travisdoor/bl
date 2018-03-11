//*****************************************************************************
// blc
//
// File:   llvm_backend.c
// Author: Martin Dorazil
// Date:   6.2.18
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
#include <string.h>
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>

#include <bobject/containers/htbl.h>
#include <bobject/containers/hash.h>

#include "stages_impl.h"
#include "bl/bllimits.h"
#include "llvm_bl_cnt_impl.h"
#include "common_impl.h"

/* class context_t */

#define DEBUG_NAMES 1

#define gen_error(cnt, code, format, ...) \
  { \
    bl_builder_error((cnt)->builder, (format), ##__VA_ARGS__); \
    cnt_terminate((cnt)); \
    longjmp((cnt)->jmp_error, (code)); \
  }

#if DEBUG_NAMES
#define gname(s) s
#else
#define gname(s) ""
#endif

typedef struct
{
  bl_builder_t *builder;
  LLVMModuleRef mod;
  LLVMBuilderRef llvm_builder;
  LLVMContextRef llvm_cnt;
  jmp_buf jmp_error;

  /* tmps */
  bl_llvm_bl_cnt_t block_context;
  LLVMValueRef ret_value;
  LLVMBasicBlockRef func_init_block;
  LLVMBasicBlockRef func_ret_block;
  BHashTable *const_strings;
  BHashTable *structs;
  char *error;
  char *error_src;
} context_t;

static void
cnt_init(bl_builder_t *builder,
         bl_assembly_t *assembly,
         context_t *cnt);

static void
cnt_terminate(context_t *cnt);

static LLVMTypeRef
to_llvm_type(context_t *cnt,
             bl_type_t *t);

static void
gen_unit(context_t *cnt,
         bl_unit_t *unit);

static LLVMValueRef
get_or_create_const_string(context_t *cnt,
                           const char *str);

static LLVMValueRef
gen_default(context_t *cnt,
            bl_type_t *t);

static int
gen_func_params(context_t *cnt,
                bl_node_t *node,
                LLVMTypeRef *out,
                bool forward);

static LLVMValueRef
gen_func_decl(context_t *cnt,
              bl_node_t *node,
              bool forward);

static LLVMValueRef
gen_expr(context_t *cnt,
         bl_node_t *expr);

static LLVMValueRef
gen_binop(context_t *cnt,
          bl_node_t *node);

static LLVMValueRef
gen_member_access(context_t *cnt,
                  bl_node_t *node);

static bool
gen_cmp_stmt(context_t *cnt,
             bl_node_t *stmt,
             LLVMBasicBlockRef break_block,
             LLVMBasicBlockRef cont_block);

static void
gen_if_stmt(context_t *cnt,
            bl_node_t *node,
            LLVMBasicBlockRef break_block,
            LLVMBasicBlockRef cont_block);

static void
gen_loop_stmt(context_t *cnt,
              bl_node_t *node);

static void
gen_ret(context_t *cnt,
        bl_node_t *node);

static void
gen_break_stmt(context_t *cnt,
               bl_node_t *node,
               LLVMBasicBlockRef break_block);

static void
gen_continue_stmt(context_t *cnt,
                  bl_node_t *node,
                  LLVMBasicBlockRef cont_block);

static void
gen_var_decl(context_t *cnt,
             bl_node_t *node);

static LLVMValueRef
gen_call_expr(context_t *cnt,
              bl_node_t *call);

static int
gen_call_args(context_t *cnt,
              bl_node_t *call,
              LLVMValueRef *out);

static LLVMTypeRef
gen_struct_decl(context_t *cnt,
                bl_node_t *node);

static void
gen_gstmt(context_t *cnt,
          bl_node_t *gstmt);

/*
 * Convert known type to LLVM type representation
 */
LLVMTypeRef
to_llvm_type(context_t *cnt,
             bl_type_t *t)
{
  switch (t->hash) {
    case BL_TYPE_VOID:
      return LLVMVoidTypeInContext(cnt->llvm_cnt);
    case BL_TYPE_CHAR:
    case BL_TYPE_I8:
    case BL_TYPE_U8:
      return LLVMInt8TypeInContext(cnt->llvm_cnt);
    case BL_TYPE_I32:
    case BL_TYPE_U32:
      return LLVMInt32TypeInContext(cnt->llvm_cnt);
    case BL_TYPE_I64:
    case BL_TYPE_PTR:
    case BL_TYPE_U64:
      return LLVMInt64TypeInContext(cnt->llvm_cnt);
    case BL_TYPE_F32:
      return LLVMFloatTypeInContext(cnt->llvm_cnt);
    case BL_TYPE_F64:
      return LLVMDoubleTypeInContext(cnt->llvm_cnt);
    case BL_TYPE_STRING:
      return LLVMPointerType(LLVMInt8TypeInContext(cnt->llvm_cnt), 0);
    case BL_TYPE_BOOL:
      return LLVMInt1TypeInContext(cnt->llvm_cnt);
    default: {
      /* type is not fundamental and must be user defined */
      bl_assert(t->custom_type,
                "user defined type reference not specified for non-fundamental type");

      switch (t->custom_type->type) {
        case BL_NODE_STRUCT_DECL:
          return gen_struct_decl(cnt, t->custom_type);
        case BL_NODE_ENUM_DECL: {
          return to_llvm_type(cnt, &t->custom_type->value.decl.type);
        }
        default: bl_abort("invalid custom type reference");
      }
    }
  }
}

LLVMValueRef
get_or_create_const_string(context_t *cnt,
                           const char *str)
{
  uint32_t hash = bo_hash_from_str(str);
  if (bo_htbl_has_key(cnt->const_strings, hash))
    return bo_htbl_at(cnt->const_strings, hash, LLVMValueRef);

  LLVMValueRef s = LLVMBuildGlobalString(
    cnt->llvm_builder, str, "str");

  s = LLVMConstPointerCast(s, LLVMPointerType(LLVMInt8TypeInContext(cnt->llvm_cnt), 0));
  bo_htbl_insert(cnt->const_strings, hash, s);
  return s;
}

/*
 * Generate default value for known type.
 * For string we create global string array with line terminator.
 */
static LLVMValueRef
gen_default(context_t *cnt,
            bl_type_t *t)
{
  switch (t->hash) {
    case BL_TYPE_CHAR:
      return LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), 0, true);
    case BL_TYPE_I8:
      return LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), 0, true);
    case BL_TYPE_I32:
      return LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, true);
    case BL_TYPE_I64:
      return LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt), 0, true);
    case BL_TYPE_U8:
      return LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), 0, false);
    case BL_TYPE_U32:
      return LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, false);
    case BL_TYPE_PTR:
    case BL_TYPE_U64:
      return LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt), 0, false);
    case BL_TYPE_F32:
      return LLVMConstReal(LLVMFloatTypeInContext(cnt->llvm_cnt), 0);
    case BL_TYPE_F64:
      return LLVMConstReal(LLVMDoubleTypeInContext(cnt->llvm_cnt), 0);
    case BL_TYPE_BOOL:
      return LLVMConstInt(LLVMInt1TypeInContext(cnt->llvm_cnt), 0, false);
    case BL_TYPE_STRING: {
      return get_or_create_const_string(cnt, "\0");
    }
    default:
      return NULL;
  }
}

/*
 * Fill array for parameters of function and return count.
 * When forward is false we expect function body after function declaration
 * and all parameters are cached for future use in function body.
 */
static int
gen_func_params(context_t *cnt,
                bl_node_t *node,
                LLVMTypeRef *out,
                bool forward)
{
  int out_i = 0;
  const int c = bl_node_func_decl_get_param_count(node);

  /* no params */
  if (c == 0) {
    return 0;
  }

  bl_node_t *param = NULL;
  for (int i = 0; i < c; i++) {
    param = bl_node_func_decl_get_param(node, i);
    *out = to_llvm_type(cnt, &param->value.param_var_decl.base.type);

    out++;
    out_i++;
  }

  return out_i;
}

LLVMValueRef
gen_expr(context_t *cnt,
         bl_node_t *expr)
{
  LLVMValueRef val;
  bl_node_const_expr_t *const_expr = NULL;
  switch (expr->type) {
    case BL_NODE_CONST_EXPR:
      const_expr = &expr->value.const_expr;
      switch (const_expr->type) {
        case BL_CONST_INT:
          val = LLVMConstInt(
            LLVMInt32TypeInContext(cnt->llvm_cnt),
            (unsigned long long int) const_expr->value.as_ulong,
            true);
          break;
        case BL_CONST_DOUBLE:
          val = LLVMConstReal(LLVMDoubleTypeInContext(cnt->llvm_cnt), const_expr->value.as_double);
          break;
        case BL_CONST_FLOAT:
          val = LLVMConstReal(
            LLVMFloatTypeInContext(cnt->llvm_cnt), const_expr->value.as_float);
          break;
        case BL_CONST_BOOL:
          val = LLVMConstInt(
            LLVMInt1TypeInContext(cnt->llvm_cnt),
            (unsigned long long int) const_expr->value.as_bool,
            false);
          break;
        case BL_CONST_STRING:
          val = get_or_create_const_string(cnt, const_expr->value.as_string);
          break;
        case BL_CONST_CHAR:
          val = LLVMConstInt(
            LLVMInt8TypeInContext(cnt->llvm_cnt),
            (unsigned long long int) const_expr->value.as_char,
            false);
          break;
        default: bl_abort("invalid constant type");
      }
      break;
    case BL_NODE_DECL_REF_EXPR: {
      val = bl_llvm_bl_cnt_get(&cnt->block_context, &expr->value.decl_ref_expr.ident);
      bl_assert(val, "unknown symbol");
      break;
    }
    case BL_NODE_CALL_EXPR:
      val = gen_call_expr(cnt, expr);
      break;
    case BL_NODE_BINOP:
      val = gen_binop(cnt, expr);
      break;
    case BL_NODE_MEMBER_EXPR:
      val = gen_member_access(cnt, expr);
      break;
    default: bl_abort("unknown expression type");
  }

  return val;
}

LLVMValueRef
gen_member_access(context_t *cnt,
                  bl_node_t *node)
{
  LLVMValueRef ptr;
  switch (node->type) {
    case BL_NODE_MEMBER_EXPR: {
      bl_node_t *member = node->value.member_expr.member;
      ptr = gen_member_access(cnt, node->value.member_expr.next);
      bl_assert(ptr, "invalid reference ptr");

      ptr = LLVMBuildStructGEP(
        cnt->llvm_builder,
        ptr,
        (unsigned int) member->value.var_decl.order,
        gname(node->value.member_expr.ident.name));
      break;
    }
    case BL_NODE_DECL_REF_EXPR: {
      ptr = bl_llvm_bl_cnt_get(&cnt->block_context, &node->value.decl_ref_expr.ident);
      bl_assert(ptr, "invalid reference ptr");
      break;
    }
    default: bl_abort("invalid expression");
  }

  return ptr;
}

LLVMValueRef
gen_binop(context_t *cnt,
          bl_node_t *node)
{
  bl_node_binop_t *binop = &node->value.binop;
  LLVMValueRef lhs = gen_expr(cnt, binop->lhs);
  LLVMValueRef rhs = gen_expr(cnt, binop->rhs);

  if (binop->operator == BL_SYM_ASIGN) {
    if (LLVMIsAAllocaInst(rhs))
      rhs = LLVMBuildLoad(cnt->llvm_builder, rhs, gname("tmp"));

    LLVMBuildStore(cnt->llvm_builder, rhs, lhs);
    return NULL;
  }

  if (LLVMIsAAllocaInst(lhs) || binop->lhs->type == BL_NODE_MEMBER_EXPR)
    lhs = LLVMBuildLoad(cnt->llvm_builder, lhs, gname("tmp"));

  if (LLVMIsAAllocaInst(rhs) || binop->rhs->type == BL_NODE_MEMBER_EXPR)
    rhs = LLVMBuildLoad(cnt->llvm_builder, rhs, gname("tmp"));

  switch (binop->operator) {
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
    default: bl_abort("unknown binop");
  }

  return NULL;
}

void
gen_ret(context_t *cnt,
        bl_node_t *node)
{
  bl_node_return_stmt_t *rnode = &node->value.return_stmt;
  if (!rnode->expr) {
    return;
  }

  LLVMValueRef val = gen_expr(cnt, rnode->expr);

  if (LLVMIsAAllocaInst(val))
    val = LLVMBuildLoad(cnt->llvm_builder, val, gname("tmp"));

  LLVMBuildStore(cnt->llvm_builder, val, cnt->ret_value);
  LLVMBuildBr(cnt->llvm_builder, cnt->func_ret_block);
}

void
gen_break_stmt(context_t *cnt,
               bl_node_t *node,
               LLVMBasicBlockRef break_block)
{
  LLVMBuildBr(cnt->llvm_builder, break_block);
}

void
gen_continue_stmt(context_t *cnt,
                  bl_node_t *node,
                  LLVMBasicBlockRef cont_block)
{
  LLVMBuildBr(cnt->llvm_builder, cont_block);
}

void
gen_var_decl(context_t *cnt,
             bl_node_t *node)
{
  bl_node_var_decl_t *vdcl = &node->value.var_decl;
  LLVMBasicBlockRef prev_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMTypeRef t = to_llvm_type(cnt, &vdcl->base.type);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_init_block);
  LLVMValueRef var = LLVMBuildAlloca(cnt->llvm_builder, t, gname(vdcl->base.ident.name));
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, prev_block);

  /*
   * Generate expression if there is one or use default value instead.
   */
  LLVMValueRef def = NULL;
  if (vdcl->expr) {
    def = gen_expr(cnt, vdcl->expr);
  } else {
    def = gen_default(cnt, &vdcl->base.type);
  }

  // TODO: can't generate default values for struct members
  if (def) {
    if (LLVMIsAAllocaInst(def)) {
      def = LLVMBuildLoad(cnt->llvm_builder, def, gname("tmp"));
    }

    LLVMBuildStore(cnt->llvm_builder, def, var);
  }
  bl_llvm_bl_cnt_add(&cnt->block_context, var, &vdcl->base.ident);
}

int
gen_call_args(context_t *cnt,
              bl_node_t *call,
              LLVMValueRef *out)
{
  int out_i = 0;
  const int c = bl_node_call_expr_get_arg_count(call);

  /* no args */
  if (c == 0) {
    return 0;
  }

  bl_node_t *expr = NULL;
  for (int i = 0; i < c; i++) {
    expr = bl_node_call_expr_get_arg(call, i);
    LLVMValueRef val = gen_expr(cnt, expr);

    if (LLVMIsAAllocaInst(val) || expr->type == BL_NODE_MEMBER_EXPR) {
      *out = LLVMBuildLoad(cnt->llvm_builder, val, gname("tmp"));
      if (LLVMGetAllocatedType(val) == LLVMFloatTypeInContext(cnt->llvm_cnt)) {
        *out = LLVMBuildCast(
          cnt->llvm_builder,
          LLVMFPExt,
          *out,
          LLVMDoubleTypeInContext(cnt->llvm_cnt),
          gname("cast"));
      }
    } else {
      *out = val;
    }

    out++;
    out_i++;
  }

  return out_i;
}

LLVMValueRef
gen_call_expr(context_t *cnt,
              bl_node_t *call)
{
  bl_node_t *callee = call->value.call_expr.callee;

  LLVMValueRef fn = LLVMGetNamedFunction(cnt->mod, callee->value.func_decl.base.ident.name);
  /*
   * Create forward declaration when function has not been created yet.
   */
  if (fn == NULL) {
    fn = gen_func_decl(cnt, callee, true);
  }

  /* args */
  LLVMValueRef args[BL_MAX_FUNC_PARAM_COUNT] = {0};
  int argc = gen_call_args(cnt, call, args);

  /* TODO: return value passed from build method */
  LLVMValueRef ret = LLVMBuildCall(cnt->llvm_builder, fn, args, argc, "");
  return ret;
}

void
gen_if_stmt(context_t *cnt,
            bl_node_t *node,
            LLVMBasicBlockRef break_block,
            LLVMBasicBlockRef cont_block)
{
  bl_node_if_stmt_t *if_stmt = &node->value.if_stmt;
  LLVMBasicBlockRef insert_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMValueRef parent = LLVMGetBasicBlockParent(insert_block);
  bl_assert(LLVMIsAFunction(parent), "invalid parent");

  bool terminated = false;

  LLVMBasicBlockRef if_then = LLVMAppendBasicBlock(parent, gname("if_then"));
  LLVMBasicBlockRef if_else = LLVMAppendBasicBlock(parent, gname("if_else"));
  LLVMBasicBlockRef if_cont = LLVMAppendBasicBlock(parent, gname("if_cont"));
  LLVMValueRef expr = gen_expr(cnt, if_stmt->expr);

  if (LLVMIsAAllocaInst(expr))
    expr = LLVMBuildLoad(cnt->llvm_builder, expr, gname("tmp"));
  expr =
    LLVMBuildIntCast(cnt->llvm_builder, expr, LLVMInt1TypeInContext(cnt->llvm_cnt), gname("tmp"));

  /*
   * If condition break generation.
   */
  LLVMBuildCondBr(cnt->llvm_builder, expr, if_then, if_else);

  if (if_stmt->else_stmt == NULL && if_stmt->else_if_stmt == NULL) {
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_else);
    LLVMBuildBr(cnt->llvm_builder, if_cont);
  }

  /* then block */
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_then);
  bl_llvm_bl_cnt_push_block(&cnt->block_context);
  terminated = gen_cmp_stmt(cnt, if_stmt->then_stmt, break_block, cont_block);
  bl_llvm_bl_cnt_pop_block(&cnt->block_context);

  if (!terminated) {
    LLVMBuildBr(cnt->llvm_builder, if_cont);
  }

  /* else if */
  if (if_stmt->else_if_stmt != NULL) {
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_else);
    gen_if_stmt(cnt, if_stmt->else_if_stmt, break_block, cont_block);
    LLVMBuildBr(cnt->llvm_builder, if_cont);
  } else if (if_stmt->else_stmt != NULL) {
    /* else */
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_else);
    bl_llvm_bl_cnt_push_block(&cnt->block_context);
    // TODO: terminated
    terminated = gen_cmp_stmt(cnt, if_stmt->else_stmt, break_block, cont_block);
    bl_llvm_bl_cnt_pop_block(&cnt->block_context);

    if (!terminated) {
      LLVMBuildBr(cnt->llvm_builder, if_cont);
    }
  }

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_cont);
}

void
gen_loop_stmt(context_t *cnt,
              bl_node_t *node)
{
  bl_node_loop_stmt_t *loop_stmt = &node->value.loop_stmt;
  LLVMBasicBlockRef insert_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMValueRef parent = LLVMGetBasicBlockParent(insert_block);
  bl_assert(LLVMIsAFunction(parent), "invalid parent");

  bool terminated = false;

  LLVMBasicBlockRef loop_decide = LLVMAppendBasicBlock(parent, gname("loop_decide"));
  LLVMBasicBlockRef loop = LLVMAppendBasicBlock(parent, gname("loop"));
  LLVMBasicBlockRef loop_cont = LLVMAppendBasicBlock(parent, gname("loop_cont"));
  LLVMValueRef expr = NULL;

  /* break into loop_init */
  LLVMBuildBr(cnt->llvm_builder, loop_decide);
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_decide);

  if (loop_stmt->expr) {
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

  /* break go back to loop */
  if (!terminated) {
    LLVMBuildBr(cnt->llvm_builder, loop_decide);
  }

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_cont);
}

LLVMValueRef
gen_func_decl(context_t *cnt,
              bl_node_t *node,
              bool forward)
{
  bl_node_func_decl_t *fnode = &node->value.func_decl;
  /* params */
  LLVMTypeRef param_types[BL_MAX_FUNC_PARAM_COUNT] = {0};

  const int pc = gen_func_params(cnt, node, param_types, forward);
  bl_ident_t *id = &fnode->base.ident;
  bl_type_t *type = &fnode->base.type;

  LLVMValueRef func = LLVMGetNamedFunction(cnt->mod, id->name);
  if (func == NULL) {
    LLVMTypeRef ret = to_llvm_type(cnt, type);
    LLVMTypeRef ret_type = LLVMFunctionType(ret, param_types, (unsigned int) pc, false);
    func = LLVMAddFunction(cnt->mod, id->name, ret_type);
  }

  /* Function body */
  if (!forward) {
    if (fnode->cmp_stmt) {
      cnt->func_init_block = LLVMAppendBasicBlock(func, gname("init"));
      LLVMBasicBlockRef entry_block = LLVMAppendBasicBlock(func, gname("entry"));
      cnt->func_ret_block = LLVMAppendBasicBlock(func, gname("exit"));

      LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_init_block);

      bl_llvm_bl_cnt_push_block(&cnt->block_context);

      /*
       * Create named references to function parameters so they
       * can be called by name in function body. This is valid only
       * when this compound statement is function body.
       */
      for (int i = 0; i < pc; i++) {
        bl_node_t *param = bl_node_func_decl_get_param(node, i);
        bl_ident_t *ident = &param->value.param_var_decl.base.ident;

        LLVMValueRef p = LLVMGetParam(func, i);
        LLVMValueRef p_tmp = LLVMBuildAlloca(
          cnt->llvm_builder, LLVMTypeOf(p), gname(ident->name));
        LLVMBuildStore(cnt->llvm_builder, p, p_tmp);
        bl_llvm_bl_cnt_add(&cnt->block_context, p_tmp, ident);
      }

      /*
       * Prepare return value.
       */
      LLVMTypeRef ret_type = to_llvm_type(cnt, &fnode->base.type);
      if (ret_type != LLVMVoidTypeInContext(cnt->llvm_cnt)) {
        cnt->ret_value = LLVMBuildAlloca(cnt->llvm_builder, ret_type, gname("ret"));
      } else {
        cnt->ret_value = NULL;
      }

      LLVMPositionBuilderAtEnd(cnt->llvm_builder, entry_block);
      gen_cmp_stmt(cnt, fnode->cmp_stmt, NULL, NULL);

      LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_init_block);
      LLVMBuildBr(cnt->llvm_builder, entry_block);
      LLVMPositionBuilderAtEnd(cnt->llvm_builder, entry_block);

      if (LLVMGetBasicBlockTerminator(entry_block) == NULL) {
        LLVMPositionBuilderAtEnd(cnt->llvm_builder, entry_block);
        LLVMBuildBr(cnt->llvm_builder, cnt->func_ret_block);
      }

      LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->func_ret_block);
      if (cnt->ret_value) {
        cnt->ret_value = LLVMBuildLoad(cnt->llvm_builder, cnt->ret_value, gname("tmp"));
        LLVMBuildRet(cnt->llvm_builder, cnt->ret_value);
      } else {
        LLVMBuildRetVoid(cnt->llvm_builder);
      }

      bl_llvm_bl_cnt_pop_block(&cnt->block_context);
    }
  }

  return func;
}

/*
 * Generate compound statement in basic block.
 */
bool
gen_cmp_stmt(context_t *cnt,
             bl_node_t *stmt,
             LLVMBasicBlockRef break_block,
             LLVMBasicBlockRef cont_block)
{
  bl_assert(stmt->type == BL_NODE_CMP_STMT, "invalid node");
  LLVMBasicBlockRef block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, block);
  bool terminated = false;

  bl_node_t *child = NULL;
  const int c = bl_node_cmp_stmt_get_children_count(stmt);
  for (int i = 0; i < c; i++) {
    child = bl_node_cmp_stmt_get_child(stmt, i);
    switch (child->type) {
      case BL_NODE_RETURN_STMT:
        gen_ret(cnt, child);
        return true;
      case BL_NODE_VAR_DECL:
        gen_var_decl(cnt, child);
        break;
      case BL_NODE_BINOP:
        gen_binop(cnt, child);
        break;
      case BL_NODE_CALL_EXPR:
        gen_expr(cnt, child);
        break;
      case BL_NODE_IF_STMT:
        gen_if_stmt(cnt, child, break_block, cont_block);
        break;
      case BL_NODE_LOOP_STMT:
        gen_loop_stmt(cnt, child);
        break;
      case BL_NODE_BREAK_STMT:
        gen_break_stmt(cnt, child, break_block);
        return true;
      case BL_NODE_CONTINUE_STMT:
        gen_continue_stmt(cnt, child, cont_block);
        return true;
      case BL_NODE_CMP_STMT:
        bl_llvm_bl_cnt_push_block(&cnt->block_context);
        terminated = gen_cmp_stmt(cnt, child, break_block, cont_block);
        bl_llvm_bl_cnt_pop_block(&cnt->block_context);

        if (terminated) {
          return terminated;
        }

        break;
      case BL_NODE_DECL_REF_EXPR:
        /* only decl reference without any expression, this will be ignored for now */
        break;
      default: bl_warning("unimplemented statement in function scope");
    }
  }

  return false;
}

LLVMTypeRef
gen_struct_decl(context_t *cnt,
                bl_node_t *node)
{
  bl_node_decl_t *strct_decl = &node->value.decl;

  /*
   * Struct can be cached when variable of this struct type
   * was declared before struct body.
   */
  LLVMTypeRef type = NULL;
  if (bo_htbl_has_key(cnt->structs, strct_decl->type.hash)) {
    type = bo_htbl_at(cnt->structs, strct_decl->type.hash, LLVMTypeRef);
    return type;
  } else {
    type = LLVMStructCreateNamed(cnt->llvm_cnt, strct_decl->type.name);
    bo_htbl_insert(cnt->structs, strct_decl->type.hash, type);
  }

  const int c = bl_node_struct_decl_get_member_count(node);
  LLVMTypeRef *members = bl_malloc(sizeof(LLVMTypeRef) * c);

  bl_node_t *member;

  for (int i = 0; i < c; i++) {
    member = bl_node_struct_decl_get_member(node, i);
    members[i] = to_llvm_type(cnt, &member->value.decl.type);
  }

  LLVMStructSetBody(type, members, (unsigned int) c, false);
  bl_free(members);
  return type;
}

void
gen_gstmt(context_t *cnt,
          bl_node_t *gstmt)
{
  bl_node_t *child = NULL;
  const int c = bl_node_glob_stmt_get_children_count(gstmt);

  for (size_t i = 0; i < c; i++) {
    child = bl_node_glob_stmt_get_child(gstmt, i);
    switch (child->type) {
      case BL_NODE_FUNC_DECL:
        gen_func_decl(cnt, child, false);
        break;
      case BL_NODE_VAR_DECL:
      case BL_NODE_STRUCT_DECL:
      case BL_NODE_ENUM_DECL:
        break;
      default: bl_abort("invalid node in global scope");
    }
  }
}

void
gen_unit(context_t *cnt,
         bl_unit_t *unit)
{
  bl_assert(unit->ast.root, "invalid ast root in unit");
  switch (unit->ast.root->type) {
    case BL_NODE_GLOBAL_STMT:
      gen_gstmt(cnt, unit->ast.root);
      break;
    default: bl_abort("invalid node on llvm generator input");
  }
}

void
cnt_init(bl_builder_t *builder,
         bl_assembly_t *assembly,
         context_t *cnt)
{
  cnt->builder = builder;
  cnt->llvm_cnt = LLVMContextCreate();
  cnt->mod = LLVMModuleCreateWithNameInContext(assembly->name, cnt->llvm_cnt);
  cnt->llvm_builder = LLVMCreateBuilderInContext(cnt->llvm_cnt);
  cnt->const_strings = bo_htbl_new(sizeof(LLVMValueRef), 1024);
  cnt->structs = bo_htbl_new(sizeof(LLVMTypeRef), 1024);

  bl_llvm_bl_cnt_init(&cnt->block_context);
}

void
cnt_terminate(context_t *cnt)
{
  bl_llvm_bl_cnt_terminate(&cnt->block_context);
  LLVMDisposeBuilder(cnt->llvm_builder);
  LLVMDisposeModule(cnt->mod);
  LLVMDisposeMessage(cnt->error);
  LLVMDisposeMessage(cnt->error_src);
  LLVMContextDispose(cnt->llvm_cnt);
  bo_unref(cnt->const_strings);
  bo_unref(cnt->structs);
}

bl_error_e
bl_llvm_backend_run(bl_builder_t *builder,
                    bl_assembly_t *assembly)
{
  context_t cnt = {0};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e) error;
  }

  cnt_init(builder, assembly, &cnt);

  const int c = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    gen_unit(&cnt, unit);
  }

  if (LLVMVerifyModule(cnt.mod, LLVMReturnStatusAction, &cnt.error)) {
    cnt.error_src = LLVMPrintModuleToString(cnt.mod);
    gen_error(&cnt,
              BL_ERR_NOT_VERIFIED,
              "not verified with error %s\n%s",
              cnt.error,
              cnt.error_src);
  }

  assembly->llvm_module = cnt.mod;
  assembly->llvm_cnt = cnt.llvm_cnt;
  cnt.mod = NULL;
  cnt.llvm_cnt = NULL;
  cnt_terminate(&cnt);

  return BL_NO_ERR;
}
