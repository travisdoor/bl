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
#include "bl/type_table.h"
#include "bl/bllimits.h"
#include "unit_impl.h"
#include "ast/ast_impl.h"

/* class LlvmBackend */
#define NAME_EMPTY_STRING "empty_string"

#define VERIFY 1
#define gen_error(cnt, format, ...) \
  { \
    bl_actor_error((Actor *)(cnt)->unit, ("(llvm_backend) "format), ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, 1); \
  }

typedef struct _context_t
{
  Unit          *unit;
  LLVMModuleRef  mod;
  LLVMBuilderRef builder;
  jmp_buf        jmp_error;

  /* tmps */
  LLVMValueRef   empty_string_tmp;
} context_t;

static bool
run(LlvmBackend *self,
    Unit        *unit);

static LLVMTypeRef
to_type(const char *t);

static LLVMValueRef
gen_default(context_t      *cnt,
            const char     *t);

static int  
gen_func_params(context_t    *cnt,
                NodeFuncDecl *node,
                LLVMTypeRef  *out);

static void
gen_func(context_t    *cnt,
         NodeFuncDecl *node);

static LLVMValueRef 
gen_epr(context_t    *cnt,
        NodeExpr     *node);

static void
gen_stmt(context_t    *cnt,
         LLVMValueRef   func,
         NodeStmt      *stmt);

static void
gen_ret(context_t      *cnt,
        NodeReturnStmt *node);

static void
gen_var_decl(context_t   *cnt,
             NodeVarDecl *vdcl);

static void
gen_gstmt(context_t      *cnt,
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
  bo_vtbl_cl(klass, Stage)->run 
    = (bool (*)(Stage*, Actor *)) run;
}

void
LlvmBackend_ctor(LlvmBackend *self, LlvmBackendParams *p)
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
LlvmBackend_copy(LlvmBackend *self, LlvmBackend *other)
{
  return BO_NO_COPY;
}
/* class LlvmBackend end */

bool
run(LlvmBackend *self,
    Unit        *unit)
{
  context_t cnt = {0};
  if (setjmp(cnt.jmp_error))
    return false;

  Node *root = bl_ast_get_root(unit->ast);

  /* TODO: solve only one unit for now */
  LLVMModuleRef mod = LLVMModuleCreateWithName(unit->name);

  cnt.builder = LLVMCreateBuilder();
  cnt.unit = unit;
  cnt.mod = mod;

  switch (root->type) {
    case BL_NODE_GLOBAL_STMT:
      gen_gstmt(&cnt, (NodeGlobalStmt *)root);
      break;
    default:
      gen_error(&cnt, "invalid node on llvm generator input");
  }

#if VERIFY
  char *error = NULL;
  if (LLVMVerifyModule(mod, LLVMReturnStatusAction, &error)) {
    bl_actor_error((Actor *)unit, "(llvm_backend) not verified with error %s", error);
    LLVMDisposeMessage(error);
    LLVMDisposeModule(mod);
    return false;
  }
#endif

  char *export_file = malloc(sizeof(char) * (strlen(unit->filepath) + 4));
  strcpy(export_file, unit->filepath);
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
to_type(const char *t)
{
  bl_type_e type = bl_strtotype(t);
  switch (type) {
    case BL_TYPE_VOID:
      return LLVMVoidType();
    case BL_TYPE_I32:
      return LLVMInt32Type();
    case BL_TYPE_I64:
      return LLVMInt64Type();
    case BL_TYPE_STRING:
      return LLVMPointerType(LLVMInt8Type(), 0);
    case BL_TYPE_BYTE:
      return LLVMInt8Type();
    case BL_TYPE_PTR:
      return LLVMPointerType(LLVMInt8Type(), 0);
    case BL_TYPE_REF:
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
gen_default(context_t      *cnt,
            const char     *t)
{
  bl_type_e type = bl_strtotype(t);
  switch (type) {
    case BL_TYPE_I32:
      return LLVMConstInt(LLVMInt32Type(), 0, false);
    case BL_TYPE_I64:
      return LLVMConstInt(LLVMInt64Type(), 0, false);
    case BL_TYPE_STRING: {
      if (cnt->empty_string_tmp == NULL)
        cnt->empty_string_tmp = LLVMBuildGlobalString(cnt->builder, "\0", NAME_EMPTY_STRING);
      return LLVMConstPointerCast(cnt->empty_string_tmp, LLVMPointerType(LLVMInt8Type(), 0));
    }
    case BL_TYPE_REF:
    default:
      return NULL;
  }
}

/*
 * Fill array for parameters of function and return count.
 */ 
static int  
gen_func_params(context_t    *cnt,
                NodeFuncDecl *node,
                LLVMTypeRef  *out)
{
  int out_i = 0;
  const int c = bl_node_func_decl_param_count(node);

  /* no params */
  if (c == 0) {
      return 0;
  }

  NodeParamVarDecl *param = NULL;
  for (int i = 0; i < c; i++) {
    param = bl_node_func_decl_param(node, i);
    *out = to_type(bl_node_param_var_decl_type(param));
    out++;
    out_i++;
  }

  return out_i;
}

LLVMValueRef
gen_epr(context_t    *cnt,
        NodeExpr     *node)
{
  return LLVMConstInt(LLVMInt32Type(), (unsigned long long int) bl_node_expr_num(node), true);
}

void
gen_ret(context_t      *cnt,
        NodeReturnStmt *node)
{
  NodeExpr *expr = bl_node_return_stmt_expr(node);
  if (!expr) {
    LLVMBuildRetVoid(cnt->builder);
    return;
  }
    
  LLVMValueRef tmp = gen_epr(cnt, expr);
  LLVMBuildRet(cnt->builder, tmp);
}

void
gen_var_decl(context_t   *cnt,
             NodeVarDecl *vdcl)
{
  LLVMTypeRef t = to_type(bl_node_var_decl_type(vdcl));
  LLVMValueRef var = LLVMBuildAlloca(cnt->builder, t, bl_node_var_decl_ident(vdcl));
  LLVMValueRef def = gen_default(cnt, bl_node_var_decl_type(vdcl));
  LLVMBuildStore(cnt->builder, def, var);
}

void
gen_func(context_t    *cnt,
         NodeFuncDecl *node)
{
  /* params */
  LLVMTypeRef param_types[BL_MAX_FUNC_PARAM_COUNT] = {0};

  int pc = gen_func_params(cnt, node, param_types);
  LLVMTypeRef ret = to_type(bl_node_func_decl_type(node));
  LLVMTypeRef ret_type = LLVMFunctionType(ret, param_types, (unsigned int) pc, false);
  LLVMValueRef func = LLVMAddFunction(cnt->mod, bl_node_func_decl_ident(node), ret_type);

  NodeStmt *stmt = bl_node_func_decl_get_stmt(node);
  if (stmt)
    gen_stmt(cnt, func, stmt);
}

void
gen_stmt(context_t     *cnt,
         LLVMValueRef   func,
         NodeStmt      *stmt)
{
  bool return_presented = false;
  LLVMBasicBlockRef entry = LLVMAppendBasicBlock(func, "entry");
  LLVMPositionBuilderAtEnd(cnt->builder, entry);

  Node *child = NULL;
  const int c = bl_node_stmt_child_count(stmt);
  for (int i = 0; i < c; i++) {
    child = bl_node_stmt_child(stmt, i);
    switch (child->type) {
      case BL_NODE_RETURN_STMT:
        return_presented = true;
        gen_ret(cnt, (NodeReturnStmt *)child);
        return;
      case BL_NODE_VAR_DECL:
        gen_var_decl(cnt, (NodeVarDecl *)child);
        break;
      default:
        gen_error(cnt, "invalid stmt in function scope");
    }
  }

  if (!return_presented) {
    LLVMBuildRetVoid(cnt->builder);
  }
}

void
gen_gstmt(context_t      *cnt,
          NodeGlobalStmt *gstmt)
{
  Node *child = NULL;
  const int c = bl_node_global_stmt_child_count(gstmt);
  for (size_t i = 0; i < c; i++) {
    child = bl_node_global_stmt_child(gstmt, i);
    switch (child->type) {
      case BL_NODE_FUNC_DECL:
        gen_func(cnt, (NodeFuncDecl *)child);
        break;
      default:
        gen_error(cnt, "invalid node in global scope");
    }
  }
}

LlvmBackend *
bl_llvm_backend_new(bl_compile_group_e group)
{
  LlvmBackendParams p = {
    .base.group = group
  };

  return bo_new(LlvmBackend, &p);
}

