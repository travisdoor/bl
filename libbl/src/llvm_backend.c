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

#define gen_error(format, ...) \
  { \
    bl_actor_error((Actor *)unit, ("(llvm_backend) "format), ##__VA_ARGS__); \
    longjmp(jmp_error, 1); \
  } 

static bool
run(LlvmBackend *self,
    Unit        *unit);

static LLVMTypeRef
to_type(const char *t);

static int  
gen_func_params(Unit       *unit,
                NodeFuncDecl *node,
                LLVMTypeRef  *out,
                jmp_buf       jmp_error);

static void
gen_func(Unit       *unit,
         LLVMModuleRef mod,
         NodeFuncDecl *node, 
         jmp_buf       jmp_error);

static LLVMValueRef 
gen_epr(Unit         *unit,
        LLVMModuleRef mod,
        NodeExpr     *node, 
        jmp_buf       jmp_error);

static void
gen_stmt(Unit         *unit,
        LLVMModuleRef  mod,
        LLVMValueRef   func,
        NodeStmt      *node, 
        jmp_buf        jmp_error);

static void
gen_ret(Unit           *unit,
        LLVMModuleRef   mod,
        LLVMBuilderRef  builder,
        NodeReturnStmt *node, 
        jmp_buf         jmp_error);

static void
gen_gstmt(Unit         *unit,
          LLVMModuleRef   mod,
          NodeGlobalStmt *node, 
          jmp_buf         jmp_error);
         

/* class LlvmBackend constructor params */
bo_decl_params_with_base_begin(LlvmBackend, Stage)
  /* constructor params */
bo_end();

/* class LlvmBackend object members */
bo_decl_members_begin(LlvmBackend, Stage)
  /* members */
  /*Module *tmp_mod;*/
  /*jmp_buf tmp_jmp_error;*/
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
  jmp_buf jmp_error;
  if (setjmp(jmp_error))
    return false;

  Node *root = bl_ast_get_root(unit->ast);

  /* TODO: solve only one unit for now */
  LLVMModuleRef mod = LLVMModuleCreateWithName(unit->name);
  
  switch (root->type) {
    case BL_NODE_GLOBAL_STMT:
      gen_gstmt(unit, mod, (NodeGlobalStmt *)root, jmp_error);
      break;
    default:
      gen_error("invalid node on llvm generator input");
  }

  /*
  char *error = NULL;
  if (LLVMVerifyModule(mod, LLVMReturnStatusAction, &error)) {
    bl_actor_error((Actor *)unit, "(llvm_backend) not verified with error %s", error);
    LLVMDisposeMessage(error);
    LLVMDisposeModule(mod);
    return false;
  }
  */

  char *export_file = malloc(sizeof(char) * (strlen(unit->filepath) + 4));
  strcpy(export_file, unit->filepath);
  strcat(export_file, ".bc");
  if (LLVMWriteBitcodeToFile(mod, export_file) != 0) {
    free(export_file);
    LLVMDisposeModule(mod);
    gen_error("error writing bitcode to file, skipping");
  }
  free(export_file);
  
  LLVMDisposeModule(mod);
  return true;
}

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
    case BL_TYPE_REF:
    default:
      return NULL;
  }
}

/*
 * Fill array for parameters of function and return count.
 */ 
static int  
gen_func_params(Unit       *unit,
                NodeFuncDecl *node,
                LLVMTypeRef  *out,
                jmp_buf       jmp_error)
{
  int out_i = 0;
  const size_t c = bo_array_size(bo_members(node, Node)->nodes);

  /* no params -> use void (scope always presented)*/ 
  if (c == 1) {
      *out = to_type("void");
      return 1;
  }

  Node *child = NULL;
  for (int i = 0; i < c; i++) {
    child = bo_array_at(bo_members(node, Node)->nodes, i, Node *);
    if (child->type == BL_NODE_PARAM_VAR_DECL) {
      *out = to_type(bo_members(child, NodeParamVarDecl)->type);
      out++;
      out_i++;
    }
  }

  return out_i;
}

LLVMValueRef
gen_epr(Unit         *unit,
        LLVMModuleRef mod,
        NodeExpr     *node, 
        jmp_buf       jmp_error)
{
  return LLVMConstInt(LLVMInt32Type(), node->num, true); 
}

void
gen_ret(Unit           *unit,
        LLVMModuleRef   mod,
        LLVMBuilderRef  builder,
        NodeReturnStmt *node, 
        jmp_buf         jmp_error)
{
  NodeExpr *expr = bl_node_return_stmt_get_expr(node);
  if (!expr) {
    LLVMBuildRetVoid(builder);
    return;
  }
    
  LLVMValueRef tmp = gen_epr(unit, mod, expr, jmp_error);
  LLVMBuildRet(builder, tmp);
}

void
gen_func(Unit         *unit,
         LLVMModuleRef mod,
         NodeFuncDecl *node,
         jmp_buf       jmp_error)
{
  /* params */
  LLVMTypeRef param_types[BL_MAX_FUNC_PARAM_COUNT];

  int pc = gen_func_params(unit, node, param_types, jmp_error);
  LLVMTypeRef ret = to_type(node->type);
  LLVMTypeRef ret_type = LLVMFunctionType(ret, param_types, (unsigned int) pc, 0);
  LLVMValueRef func = LLVMAddFunction(mod, node->ident, ret_type);

  NodeStmt *stmt = bl_node_func_decl_get_stmt(node);
  gen_stmt(unit, mod, func, stmt, jmp_error);
}

void
gen_stmt(Unit         *unit,
         LLVMModuleRef  mod,
         LLVMValueRef   func,
         NodeStmt      *node, 
         jmp_buf        jmp_error)
{
  LLVMBasicBlockRef entry = LLVMAppendBasicBlock(func, "entry");

  LLVMBuilderRef builder = LLVMCreateBuilder();
  LLVMPositionBuilderAtEnd(builder, entry);

  Node *child = NULL;
  const size_t c = bo_array_size(bo_members(node, Node)->nodes);
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(bo_members(node, Node)->nodes, i, Node *);
    switch (child->type) {
      case BL_NODE_RETURN_STMT:
        gen_ret(unit, mod, builder, (NodeReturnStmt *)child, jmp_error);
        break;
      default:
        gen_error("invalid node in function scope");
    }
  }
}

void
gen_gstmt(Unit           *unit,
          LLVMModuleRef   mod,
          NodeGlobalStmt *node, 
          jmp_buf         jmp_error)
{
  Node *child = NULL;
  const size_t c = bo_array_size(bo_members(node, Node)->nodes);
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(bo_members(node, Node)->nodes, i, Node *);
    switch (child->type) {
      case BL_NODE_FUNC_DECL:
        gen_func(unit, mod, (NodeFuncDecl *)child, jmp_error);
        break;
      default:
        gen_error("invalid node in global scope");
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

