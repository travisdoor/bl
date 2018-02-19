//*****************************************************************************
// bl
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
#include <llvm-c/Support.h>
#include <bobject/containers/htbl.h>
#include <bobject/containers/hash.h>

#include "bl/llvm_backend.h"
#include "bl/pipeline/stage.h"
#include "bl/bldebug.h"
#include "bl/bllimits.h"
#include "bl/unit.h"
#include "ast/ast_impl.h"
#include "llvm_bl_cnt_impl.h"

/* class LlvmBackend */

#define VERIFY 1
#define gen_error(cnt, format, ...) \
  { \
    bl_actor_error((Actor *)(cnt)->unit, (format), ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, 1); \
  }

typedef struct _context_t
{
  Unit *unit;
  LLVMModuleRef mod;
  LLVMBuilderRef builder;
  jmp_buf jmp_error;

  /* tmps */
  LlvmBlockContext *block_context;
  BHashTable *const_strings;
} context_t;

static bool
run(LlvmBackend *self,
    Unit *unit);

static void
init_cnt(context_t *cnt,
         Unit *unit,
         LLVMModuleRef module,
         jmp_buf jmp_error);

static void
destroy_cnt(context_t *cnt);

static LLVMTypeRef
to_llvm_type(Type *t);

static LLVMValueRef
get_or_create_const_string(context_t *cnt,
                           const char *str);

static LLVMValueRef
gen_default(context_t *cnt,
            Type *t);

static int
gen_func_params(context_t *cnt,
                NodeFuncDecl *node,
                LLVMTypeRef *out,
                bool forward);

static LLVMValueRef
gen_func(context_t *cnt,
         NodeFuncDecl *fnode,
         bool forward);

static LLVMValueRef
gen_expr(context_t *cnt,
         NodeExpr *expr);

static LLVMValueRef
gen_binop(context_t *cnt,
          NodeBinop *binop);

static void
gen_cmp_stmt(context_t *cnt,
             NodeStmt *stmt,
             NodeFuncDecl *fnode);

static void
gen_if_stmt(context_t *cnt,
            NodeIfStmt *ifstmt);

static void
gen_ret(context_t *cnt,
        NodeReturnStmt *node);

static void
gen_var_decl(context_t *cnt,
             NodeVarDecl *vdcl);

static LLVMValueRef
gen_call(context_t *cnt,
         NodeCall *call);

static int
gen_call_args(context_t *cnt,
              NodeCall *call,
              LLVMValueRef *out);

static void
gen_gstmt(context_t *cnt,
          NodeGlobalStmt *gstmt);


/* class LlvmBackend constructor params */
bo_decl_params_with_base_begin(LlvmBackend, Stage)
  /* constructor params */
bo_end();

/* class LlvmBackend object members */
bo_decl_members_begin(LlvmBackend, Stage)
  /* members */
bo_end();

bo_impl_type(LlvmBackend, Stage);

void
LlvmBackendKlass_init(LlvmBackendKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run =
    (bool (*)(Stage *,
              Actor *)) run;
}

void
LlvmBackend_ctor(LlvmBackend *self,
                 LlvmBackendParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Stage, p);
}

void
LlvmBackend_dtor(LlvmBackend *self)
{
}

bo_copy_result
LlvmBackend_copy(LlvmBackend *self,
                 LlvmBackend *other)
{
  return BO_NO_COPY;
}

/* class LlvmBackend end */

void
init_cnt(context_t *cnt,
         Unit *unit,
         LLVMModuleRef module,
         jmp_buf jmp_error)
{
  cnt->builder = LLVMCreateBuilder();
  cnt->unit = unit;
  cnt->mod = module;
  cnt->block_context = bl_llvm_block_context_new();
  cnt->const_strings = bo_htbl_new(sizeof(LLVMValueRef), 256);
}

void
destroy_cnt(context_t *cnt)
{
  bo_unref(cnt->const_strings);
  bo_unref(cnt->block_context);
  LLVMDisposeBuilder(cnt->builder);
}

bool
run(LlvmBackend *self,
    Unit *unit)
{
  context_t cnt = {0};
  jmp_buf jmp_error;
  LLVMModuleRef mod;

  if (setjmp(cnt.jmp_error)) {
    destroy_cnt(&cnt);
    LLVMDisposeModule(mod);
    return false;
  }

  mod = LLVMModuleCreateWithName(bl_unit_get_name(unit));
  init_cnt(&cnt, unit, mod, jmp_error);

  Node *root = bl_ast_get_root(bl_unit_get_ast(unit));
  switch (root->type) {
    case BL_NODE_GLOBAL_STMT:
      gen_gstmt(&cnt, (NodeGlobalStmt *) root);
      break;
    default: gen_error(&cnt, "invalid node on llvm generator input");
  }

#if VERIFY
  char *error = NULL;
  if (LLVMVerifyModule(cnt.mod, LLVMReturnStatusAction, &error)) {
    bl_actor_error((Actor *) unit, "not verified with error %s", error);
    LLVMDisposeMessage(error);
    LLVMDisposeModule(mod);
    destroy_cnt(&cnt);
    return false;
  }
#endif

  bl_unit_set_llvm_module(unit, cnt.mod);

  destroy_cnt(&cnt);
  return true;
}

/*
 * Convert known type to LLVM type representation
 * TODO: dont use strings here!!!
 */
LLVMTypeRef
to_llvm_type(Type *t)
{
  switch (bl_type_get(t)) {
    case BL_TYPE_VOID:
      return LLVMVoidType();
    case BL_TYPE_I32:
      return LLVMInt32Type();
    case BL_TYPE_I64:
      return LLVMInt64Type();
    case BL_TYPE_STRING:
      return LLVMPointerType(LLVMInt8Type(), 0);
    case BL_TYPE_CHAR:
      return LLVMInt8Type();
    case BL_TYPE_BOOL:
      return LLVMInt1Type();
    default:
      return NULL;
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
    cnt->builder, str, "str");

  s = LLVMConstPointerCast(s, LLVMPointerType(LLVMInt8Type(), 0));
  bo_htbl_insert(cnt->const_strings, hash, s);
  return s;
}

/*
 * Generate default value for known type.
 * For string we create global string array with line terminator.
 */
static LLVMValueRef
gen_default(context_t *cnt,
            Type *t)
{
  switch (bl_type_get(t)) {
    case BL_TYPE_CHAR:
      return LLVMConstInt(LLVMInt8Type(), 0, false);
    case BL_TYPE_I32:
      return LLVMConstInt(LLVMInt32Type(), 0, false);
    case BL_TYPE_I64:
      return LLVMConstInt(LLVMInt64Type(), 0, false);
    case BL_TYPE_BOOL:
      return LLVMConstInt(LLVMInt1Type(), 0, false);
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
                NodeFuncDecl *node,
                LLVMTypeRef *out,
                bool forward)
{
  int out_i = 0;
  const int c = bl_node_func_decl_get_param_count(node);

  /* no params */
  if (c == 0) {
    return 0;
  }

  NodeParamVarDecl *param = NULL;
  for (int i = 0; i < c; i++) {
    param = bl_node_func_decl_get_param(node, i);
    *out = to_llvm_type(bl_node_decl_get_type((NodeDecl *) param));

    out++;
    out_i++;
  }

  return out_i;
}

LLVMValueRef
gen_expr(context_t *cnt,
         NodeExpr *expr)
{
  bl_node_e nt = bl_node_get_type((Node *) expr);
  LLVMValueRef val;
  switch (nt) {
    case BL_NODE_CONST:
      switch (bl_node_const_get_type((NodeConst *) expr)) {
        case BL_CONST_INT:
          val = LLVMConstInt(
            LLVMInt32Type(),
            (unsigned long long int) bl_node_const_get_int((NodeConst *) expr),
            true);
          break;
        case BL_CONST_BOOL:
          val = LLVMConstInt(
            LLVMInt1Type(),
            (unsigned long long int) bl_node_const_get_bool((NodeConst *) expr),
            false);
          break;
        case BL_CONST_STRING:
          val = get_or_create_const_string(
            cnt, bl_node_const_get_str((NodeConst *) expr));
          break;
        default: bl_abort("invalid constant type");
      }
      break;
    case BL_NODE_DECL_REF: {
      val = bl_llvm_block_context_get(
        cnt->block_context, bl_node_decl_ref_get_ident((NodeDeclRef *) expr));
      bl_assert(val, "unknown symbol");
      break;
    }
    case BL_NODE_CALL:
      val = gen_call(cnt, (NodeCall *) expr);
      break;
    case BL_NODE_BINOP:
      val = gen_binop(cnt, (NodeBinop *) expr);
      break;
    default: bl_abort("unknown expression type");
  }

  return val;
}

LLVMValueRef
gen_binop(context_t *cnt,
          NodeBinop *binop)
{
  LLVMValueRef lvalue = gen_expr(cnt, bl_node_binop_get_lvalue(binop));
  LLVMValueRef rvalue = gen_expr(cnt, bl_node_binop_get_rvalue(binop));

  if (LLVMIsAAllocaInst(rvalue))
    rvalue = LLVMBuildLoad(cnt->builder, rvalue, "tmp");

  switch (bl_node_binop_get_op(binop)) {
    case BL_SYM_ASIGN:
      LLVMBuildStore(cnt->builder, rvalue, lvalue);
      return NULL;
    case BL_SYM_PLUS:
      if (LLVMIsAAllocaInst(lvalue))
        lvalue = LLVMBuildLoad(cnt->builder, lvalue, "tmp");
      return LLVMBuildAdd(cnt->builder, lvalue, rvalue, "tmp");
    case BL_SYM_MINUS:
      if (LLVMIsAAllocaInst(lvalue))
        lvalue = LLVMBuildLoad(cnt->builder, lvalue, "tmp");
      return LLVMBuildSub(cnt->builder, lvalue, rvalue, "tmp");
    case BL_SYM_ASTERISK:
      if (LLVMIsAAllocaInst(lvalue))
        lvalue = LLVMBuildLoad(cnt->builder, lvalue, "tmp");
      return LLVMBuildMul(cnt->builder, lvalue, rvalue, "tmp");
    case BL_SYM_SLASH:
      if (LLVMIsAAllocaInst(lvalue))
        lvalue = LLVMBuildLoad(cnt->builder, lvalue, "tmp");
      return LLVMBuildFDiv(cnt->builder, lvalue, rvalue, "tmp");
    case BL_SYM_EQ:
      if (LLVMIsAAllocaInst(lvalue))
        lvalue = LLVMBuildLoad(cnt->builder, lvalue, "tmp");
      return LLVMBuildICmp(cnt->builder, LLVMIntEQ, lvalue, rvalue, "tmp");
    case BL_SYM_NEQ:
      if (LLVMIsAAllocaInst(lvalue))
        lvalue = LLVMBuildLoad(cnt->builder, lvalue, "tmp");
      return LLVMBuildICmp(cnt->builder, LLVMIntNE, lvalue, rvalue, "tmp");
    case BL_SYM_GREATER:
      if (LLVMIsAAllocaInst(lvalue))
        lvalue = LLVMBuildLoad(cnt->builder, lvalue, "tmp");
      return LLVMBuildICmp(cnt->builder, LLVMIntSGT, lvalue, rvalue, "tmp");
    case BL_SYM_LESS:
      if (LLVMIsAAllocaInst(lvalue))
        lvalue = LLVMBuildLoad(cnt->builder, lvalue, "tmp");
      return LLVMBuildICmp(cnt->builder, LLVMIntSLT, lvalue, rvalue, "tmp");
    case BL_SYM_GREATER_EQ:
      if (LLVMIsAAllocaInst(lvalue))
        lvalue = LLVMBuildLoad(cnt->builder, lvalue, "tmp");
      return LLVMBuildICmp(cnt->builder, LLVMIntSGE, lvalue, rvalue, "tmp");
    case BL_SYM_LESS_EQ:
      if (LLVMIsAAllocaInst(lvalue))
        lvalue = LLVMBuildLoad(cnt->builder, lvalue, "tmp");
      return LLVMBuildICmp(cnt->builder, LLVMIntSLE, lvalue, rvalue, "tmp");
    default: bl_abort("unknown binop");
  }

  return NULL;
}

void
gen_ret(context_t *cnt,
        NodeReturnStmt *node)
{
  NodeExpr *expr = bl_node_return_stmt_get_expr(node);
  if (!expr) {
    LLVMBuildRetVoid(cnt->builder);
    return;
  }

  LLVMValueRef val = gen_expr(cnt, expr);

  if (LLVMIsAAllocaInst(val))
    val = LLVMBuildLoad(cnt->builder, val, "tmp");
  LLVMBuildRet(cnt->builder, val);
}

void
gen_var_decl(context_t *cnt,
             NodeVarDecl *vdcl)
{
  LLVMTypeRef t = to_llvm_type(bl_node_decl_get_type((NodeDecl *) vdcl));

  Ident *id = bl_node_decl_get_ident((NodeDecl *) vdcl);
  LLVMValueRef var = LLVMBuildAlloca(cnt->builder, t, bl_ident_get_name(id));

  /*
   * Generate expression if there is one or use default value instead.
   */
  LLVMValueRef def = NULL;
  NodeExpr *expr = bl_node_var_decl_get_expr(vdcl);
  if (expr) {
    def = gen_expr(cnt, expr);
  } else {
    def = gen_default(cnt, bl_node_decl_get_type((NodeDecl *) vdcl));
  }

  if (LLVMIsAAllocaInst(def)) {
    def = LLVMBuildLoad(cnt->builder, def, "tmp");
  }

  LLVMBuildStore(cnt->builder, def, var);
  bl_llvm_block_context_add(cnt->block_context, var, id);
}

int
gen_call_args(context_t *cnt,
              NodeCall *call,
              LLVMValueRef *out)
{
  int out_i = 0;
  const int c = bl_node_call_get_arg_count(call);

  /* no args */
  if (c == 0) {
    return 0;
  }

  NodeExpr *expr = NULL;
  for (int i = 0; i < c; i++) {
    expr = bl_node_call_get_arg(call, i);
    LLVMValueRef val = gen_expr(cnt, expr);

    if (LLVMIsAAllocaInst(val))
      *out = LLVMBuildLoad(cnt->builder, val, "tmp");
    else
      *out = val;

    out++;
    out_i++;
  }

  return out_i;
}

LLVMValueRef
gen_call(context_t *cnt,
         NodeCall *call)
{
  NodeFuncDecl *callee = bl_node_call_get_callee(call);

  LLVMValueRef
    fn =
    LLVMGetNamedFunction(cnt->mod, bl_ident_get_name(bl_node_decl_get_ident((NodeDecl *) callee)));

  /*
   * Create forward declaration when function has not been created yet.
   */
  if (fn == NULL) {
    fn = gen_func(cnt, callee, true);
  }

  /* args */
  LLVMValueRef args[BL_MAX_FUNC_PARAM_COUNT] = {0};
  int argc = gen_call_args(cnt, call, args);

  /* TODO: return value passed from build method */

  LLVMValueRef ret = LLVMBuildCall(cnt->builder, fn, args, argc, "");
  return ret;
}

LLVMValueRef
gen_func(context_t *cnt,
         NodeFuncDecl *fnode,
         bool forward)
{
  /* params */
  LLVMTypeRef param_types[BL_MAX_FUNC_PARAM_COUNT] = {0};

  int pc = gen_func_params(cnt, fnode, param_types, forward);
  Ident *id = bl_node_decl_get_ident((NodeDecl *) fnode);

  LLVMValueRef func = LLVMGetNamedFunction(cnt->mod, bl_ident_get_name(id));
  if (func == NULL) {
    LLVMTypeRef ret = to_llvm_type(bl_node_decl_get_type((NodeDecl *) fnode));
    LLVMTypeRef ret_type = LLVMFunctionType(ret, param_types, (unsigned int) pc, false);
    func = LLVMAddFunction(cnt->mod, bl_ident_get_name(id), ret_type);
  }

  if (!forward) {
    NodeStmt *stmt = bl_node_func_decl_get_stmt(fnode);
    if (stmt) {
      LLVMBasicBlockRef block = LLVMAppendBasicBlock(func, "entry");
      LLVMPositionBuilderAtEnd(cnt->builder, block);
      gen_cmp_stmt(cnt, stmt, fnode);
    }
  }

  return func;
}

void
gen_if_stmt(context_t *cnt,
            NodeIfStmt *ifstmt)
{
  LLVMBasicBlockRef insert_block = LLVMGetInsertBlock(cnt->builder);
  LLVMValueRef parent = LLVMGetBasicBlockParent(insert_block);

  LLVMBasicBlockRef ifthen = LLVMAppendBasicBlock(parent, "then");
  LLVMBasicBlockRef ifelse = LLVMAppendBasicBlock(parent, "else");
  LLVMBasicBlockRef ifcont = LLVMAppendBasicBlock(parent, "cont");
  LLVMValueRef expr = gen_expr(cnt, bl_node_if_stmt_get_cond(ifstmt));

  if (LLVMIsAAllocaInst(expr))
    expr = LLVMBuildLoad(cnt->builder, expr, "tmp");
  expr = LLVMBuildIntCast(cnt->builder, expr, LLVMInt1Type(), "tmp");

  /*
   * If condition break generation.
   */
  LLVMBuildCondBr(cnt->builder, expr, ifthen, ifelse);

  LLVMPositionBuilderAtEnd(cnt->builder, ifthen);
  gen_cmp_stmt(cnt, bl_node_if_stmt_get_stmt(ifstmt), NULL);
  LLVMBuildBr(cnt->builder, ifcont);

  LLVMPositionBuilderAtEnd(cnt->builder, ifelse);
  LLVMBuildBr(cnt->builder, ifcont);

  LLVMPositionBuilderAtEnd(cnt->builder, ifcont);
}

void
gen_cmp_stmt(context_t *cnt,
             NodeStmt *stmt,
             NodeFuncDecl *fnode)
{
  bool return_presented = false;
  bl_llvm_block_context_push_block(cnt->block_context);
  LLVMBasicBlockRef insert_block = LLVMGetInsertBlock(cnt->builder);
  LLVMPositionBuilderAtEnd(cnt->builder, insert_block);

  /*
   * Create named references to function parameters so they
   * can be called by name in function body. This is valid only
   * when this compound statement is function body.
   */
  if (fnode) {
    LLVMValueRef parent = LLVMGetBasicBlockParent(insert_block);
    bl_assert(LLVMIsAFunction(parent), "invalid parent, must be function");
    const int pc = bl_node_func_decl_get_param_count(fnode);
    for (int i = 0; i < pc; i++) {
      NodeParamVarDecl *param = bl_node_func_decl_get_param(fnode, i);
      Ident *ident = bl_node_decl_get_ident((NodeDecl *) param);

      LLVMValueRef p = LLVMGetParam(parent, i);
      LLVMValueRef p_tmp = LLVMBuildAlloca(cnt->builder, LLVMTypeOf(p), bl_ident_get_name(ident));
      LLVMBuildStore(cnt->builder, p, p_tmp);
      bl_llvm_block_context_add(cnt->block_context, p_tmp, ident);
    }
  }

  Node *child = NULL;
  const int c = bl_node_stmt_child_get_count(stmt);
  for (int i = 0; i < c; i++) {
    child = bl_node_stmt_get_child(stmt, i);
    switch (child->type) {
      case BL_NODE_RETURN_STMT:
        return_presented = true;
        gen_ret(cnt, (NodeReturnStmt *) child);
        goto done;
      case BL_NODE_VAR_DECL:
        gen_var_decl(cnt, (NodeVarDecl *) child);
        break;
      case BL_NODE_BINOP:
        gen_binop(cnt, (NodeBinop *) child);
        break;
      case BL_NODE_CALL:
        gen_expr(cnt, (NodeExpr *) child);
        break;
      case BL_NODE_IF_STMT:
        gen_if_stmt(cnt, (NodeIfStmt *) child);
        break;
      case BL_NODE_DECL_REF:
        /* only decl reference without any expression, this will be ignored for now */
        break;
      default: bl_warning("unimplemented statement in function scope");
    }
  }

done:
  if (!return_presented && fnode) {
    LLVMBuildRetVoid(cnt->builder);
  }

  bl_llvm_block_context_pop_block(cnt->block_context);
}

void
gen_gstmt(context_t *cnt,
          NodeGlobalStmt *gstmt)
{
  Node *child = NULL;
  const int c = bl_node_global_stmt_get_child_count(gstmt);
  for (size_t i = 0; i < c; i++) {
    child = bl_node_global_stmt_get_child(gstmt, i);
    switch (child->type) {
      case BL_NODE_FUNC_DECL:
        gen_func(cnt, (NodeFuncDecl *) child, false);
        break;
      default: gen_error(cnt, "invalid node in global scope");
    }
  }
}

LlvmBackend *
bl_llvm_backend_new(bl_compile_group_e group)
{
  LlvmBackendParams p = {.base.group = group};

  return bo_new(LlvmBackend, &p);
}

