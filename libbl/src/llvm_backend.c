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
#include <llvm-c/BitWriter.h>

#include "bl/llvm_backend.h"
#include "bl/pipeline/stage.h"
#include "bl/bldebug.h"
#include "bl/bllimits.h"
#include "bl/unit.h"
#include "ast/ast_impl.h"

/* class LlvmBackend */
#define NAME_EMPTY_STRING "empty_string"
#define NAME_CONST_STRING "str_tmp"

#define VERIFY 1
#define gen_error(cnt, format, ...) \
  { \
    bl_actor_error((Actor *)(cnt)->unit, ("(llvm_backend) "format), ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, 1); \
  }

typedef struct _context_t
{
  Unit *unit;
  LLVMModuleRef mod;
  LLVMBuilderRef builder;
  jmp_buf jmp_error;

  /* tmps */
  LLVMValueRef empty_string_tmp;
} context_t;

static bool
run(LlvmBackend *self,
    Unit *unit);

static LLVMTypeRef
to_llvm_type(Type *t);

static LLVMValueRef
gen_default(context_t *cnt,
            Type *t);

static int
gen_func_params(context_t *cnt,
                NodeFuncDecl *node,
                LLVMTypeRef *out);

static void
gen_func(context_t *cnt,
         NodeFuncDecl *node);

static LLVMValueRef
gen_epr(context_t *cnt,
        NodeExpr *expr);

static void
gen_stmt(context_t *cnt,
         LLVMValueRef func,
         NodeStmt *stmt);

static void
gen_ret(context_t *cnt,
        NodeReturnStmt *node);

static void
gen_var_decl(context_t *cnt,
             NodeVarDecl *vdcl);

static void
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

bool
run(LlvmBackend *self,
    Unit *unit)
{
  context_t cnt = {0};
  if (setjmp(cnt.jmp_error))
    return false;

  Node *root = bl_ast_get_root(bl_unit_get_ast(unit));

  /* TODO: solve only one unit for now */
  LLVMModuleRef mod = LLVMModuleCreateWithName(bl_unit_get_name(unit));

  cnt.builder = LLVMCreateBuilder();
  cnt.unit = unit;
  cnt.mod = mod;

  switch (root->type) {
    case BL_NODE_GLOBAL_STMT:
      gen_gstmt(&cnt, (NodeGlobalStmt *) root);
      break;
    default: gen_error(&cnt, "invalid node on llvm generator input");
  }

#if VERIFY
  char *error = NULL;
  if (LLVMVerifyModule(mod, LLVMReturnStatusAction, &error)) {
    bl_actor_error((Actor *) unit, "(llvm_backend) not verified with error %s", error);
    LLVMDisposeMessage(error);
    LLVMDisposeModule(mod);
    return false;
  }
#endif

  char *export_file = malloc(sizeof(char) * (strlen(bl_unit_get_src_file(unit)) + 4));
  strcpy(export_file, bl_unit_get_src_file(unit));
  strcat(export_file, ".bc");
  if (LLVMWriteBitcodeToFile(mod, export_file) != 0) {
    free(export_file);
    LLVMDisposeModule(mod);
    gen_error(&cnt, "error writing bitcode to file, skipping");
  }
  free(export_file);

  LLVMDisposeModule(mod);
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
    case BL_TYPE_BOOL:
      return LLVMInt8Type();
    default:
      return NULL;
  }
}

/*
 * Generate default value for known type.
 * For string we create global string array with line terminator.
 * TODO: dont use strings here!!!
 */
static LLVMValueRef
gen_default(context_t *cnt,
            Type *t)
{
  switch (bl_type_get(t)) {
    case BL_TYPE_BOOL:
    case BL_TYPE_CHAR:
      return LLVMConstInt(LLVMInt8Type(), 0, false);
    case BL_TYPE_I32:
      return LLVMConstInt(LLVMInt32Type(), 0, false);
    case BL_TYPE_I64:
      return LLVMConstInt(LLVMInt64Type(), 0, false);
    case BL_TYPE_STRING: {
      if (cnt->empty_string_tmp == NULL)
        cnt->empty_string_tmp = LLVMBuildGlobalString(cnt->builder, "\0", NAME_EMPTY_STRING);
      return LLVMConstPointerCast(cnt->empty_string_tmp, LLVMPointerType(LLVMInt8Type(), 0));
    }
    default:
      return NULL;
  }
}

/*
 * Fill array for parameters of function and return count.
 */
static int
gen_func_params(context_t *cnt,
                NodeFuncDecl *node,
                LLVMTypeRef *out)
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
gen_epr(context_t *cnt,
        NodeExpr *expr)
{
  bl_node_e nt = bl_node_get_type((Node *) expr);
  switch (nt) {
    case BL_NODE_INT_CONST:
      return LLVMConstInt(
        LLVMInt32Type(), bl_node_int_const_get_num((NodeIntConst *) expr), true);
    case BL_NODE_STRING_CONST: {
      /*
       * For constant string we generate constant global array and return pointer
       * to this array.
       */
      LLVMValueRef str = LLVMBuildGlobalString(
        cnt->builder, bl_node_string_const_get_str((NodeStringConst *) expr), NAME_CONST_STRING);
      return LLVMConstPointerCast(str, LLVMPointerType(LLVMInt8Type(), 0));
    }
    default: bl_abort("unknown expression type");
  }
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

  LLVMValueRef tmp = gen_epr(cnt, expr);
  LLVMBuildRet(cnt->builder, tmp);
}

void
gen_var_decl(context_t *cnt,
             NodeVarDecl *vdcl)
{
  LLVMTypeRef t = to_llvm_type(bl_node_decl_get_type((NodeDecl *) vdcl));
  LLVMValueRef var = LLVMBuildAlloca(cnt->builder, t, bl_node_decl_get_ident((NodeDecl *) vdcl));

  /*
   * Generate expression if there is one or use default value instead.
   */
  LLVMValueRef def = NULL;
  NodeExpr *expr = bl_node_var_decl_get_expr(vdcl);
  if (expr) {
    def = LLVMConstIntCast(gen_epr(cnt, expr), t, false);
  } else {
    def = gen_default(cnt, bl_node_decl_get_type((NodeDecl *) vdcl));
  }

  LLVMBuildStore(cnt->builder, def, var);
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
    *out = gen_epr(cnt, expr);
    out++;
    out_i++;
  }

  return out_i;
}

void
gen_call(context_t *cnt,
         NodeCall *call)
{
  LLVMValueRef fn = LLVMGetNamedFunction(cnt->mod, bl_node_call_get_calle(call));
  bl_assert(fn, "invalid function");

  /* args */
  LLVMValueRef args[BL_MAX_FUNC_PARAM_COUNT] = {0};
  int argc = gen_call_args(cnt, call, args);

  /* TODO: return value passed from build method */
  LLVMBuildCall(cnt->builder, fn, args, argc, "");
}

void
gen_func(context_t *cnt,
         NodeFuncDecl *node)
{
  /* params */
  LLVMTypeRef param_types[BL_MAX_FUNC_PARAM_COUNT] = {0};

  int pc = gen_func_params(cnt, node, param_types);
  LLVMTypeRef ret = to_llvm_type(bl_node_decl_get_type((NodeDecl *) node));
  LLVMTypeRef ret_type = LLVMFunctionType(ret, param_types, (unsigned int) pc, false);
  LLVMValueRef
    func = LLVMAddFunction(cnt->mod, bl_node_decl_get_ident((NodeDecl *) node), ret_type);

  NodeStmt *stmt = bl_node_func_decl_get_stmt(node);
  if (stmt)
    gen_stmt(cnt, func, stmt);
}

void
gen_stmt(context_t *cnt,
         LLVMValueRef func,
         NodeStmt *stmt)
{
  bool return_presented = false;
  LLVMBasicBlockRef entry = LLVMAppendBasicBlock(func, "entry");
  LLVMPositionBuilderAtEnd(cnt->builder, entry);

  Node *child = NULL;
  const int c = bl_node_stmt_child_get_count(stmt);
  for (int i = 0; i < c; i++) {
    child = bl_node_stmt_get_child(stmt, i);
    switch (child->type) {
      case BL_NODE_RETURN_STMT:
        return_presented = true;
        gen_ret(cnt, (NodeReturnStmt *) child);
        return;
      case BL_NODE_VAR_DECL:
        gen_var_decl(cnt, (NodeVarDecl *) child);
        break;
      case BL_NODE_CALL:
        gen_call(cnt, (NodeCall *) child);
        break;
      default: gen_error(cnt, "invalid stmt in function scope");
    }
  }

  if (!return_presented) {
    LLVMBuildRetVoid(cnt->builder);
  }
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
        gen_func(cnt, (NodeFuncDecl *) child);
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

