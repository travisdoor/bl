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
         LLVMModuleRef llvm_mod,
         NodeFuncDecl *node, 
         jmp_buf       jmp_error);

static void
gen_gstmt(Unit         *unit,
          LLVMModuleRef   llvm_mod,
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
  p->base.group = BL_CGROUP_ANALYZE;
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

  bl_log("llvm here");

  Node *root = bl_ast_get_root(unit->ast);

  /* TODO: solve only one unit for now */
  LLVMModuleRef llvm_mod = LLVMModuleCreateWithName(unit->name);
  

  switch (root->type) {
    case BL_NODE_GLOBAL_STMT:
      gen_gstmt(unit, llvm_mod, (NodeGlobalStmt *)root, jmp_error);
      break;
    default:
      gen_error("invalid node on llvm generator input");
  }

  char *error = NULL;
  if (LLVMVerifyModule(llvm_mod, LLVMReturnStatusAction, &error)) {
    bl_actor_error((Actor *)unit, "(llvm_backend) not verified with error %s", error);
    LLVMDisposeMessage(error);
    LLVMDisposeModule(llvm_mod);
    return false;
  }

  char *export_file = malloc(sizeof(char) * (strlen(unit->filepath) + 4));
  strcpy(export_file, unit->filepath);
  strcat(export_file, ".bc");
  if (LLVMWriteBitcodeToFile(llvm_mod, export_file) != 0) {
    free(export_file);
    LLVMDisposeModule(llvm_mod);
    gen_error("error writing bitcode to file, skipping");
  }
  free(export_file);
  
  LLVMDisposeModule(llvm_mod);
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

void
gen_func(Unit         *unit,
         LLVMModuleRef llvm_mod,
         NodeFuncDecl *node,
         jmp_buf       jmp_error)
{
  /* params */
  LLVMTypeRef param_types[BL_MAX_FUNC_PARAM_COUNT];

  int pc = gen_func_params(unit, node, param_types, jmp_error);
  LLVMTypeRef ret = to_type(node->type);
  LLVMTypeRef ret_type = LLVMFunctionType(ret, param_types, (unsigned int) pc, 0);
  LLVMValueRef func = LLVMAddFunction(llvm_mod, node->ident, ret_type);

  LLVMBasicBlockRef entry = LLVMAppendBasicBlock(func, "entry");

  LLVMBuilderRef builder = LLVMCreateBuilder();
  LLVMPositionBuilderAtEnd(builder, entry);

  LLVMValueRef tmp = LLVMBuildAdd(builder, LLVMGetParam(func, 0), LLVMGetParam(func, 1), "tmp");
  LLVMBuildRet(builder, tmp);
}

void
gen_gstmt(Unit           *unit,
          LLVMModuleRef   llvm_mod,
          NodeGlobalStmt *node, 
          jmp_buf         jmp_error)
{
  Node *child = NULL;
  const size_t c = bo_array_size(bo_members(node, Node)->nodes);
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(bo_members(node, Node)->nodes, i, Node *);
    switch (child->type) {
      case BL_NODE_FUNC_DECL:
        gen_func(unit, llvm_mod, (NodeFuncDecl *)child, jmp_error);
        break;
      default:
        gen_error("invalid node in global scope");
    }
  }
}

LlvmBackend *
bl_llvm_backend_new(void)
{
  LlvmBackendParams p = {
  };

  return bo_new(LlvmBackend, &p);
}

