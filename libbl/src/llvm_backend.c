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
#include "bl/module.h"
#include "bl/pipeline/stage.h"
#include "bl/bldebug.h"
#include "domains_impl.h"
#include "module_impl.h"
#include "unit_impl.h"
#include "ast/ast_impl.h"

/* class LlvmBackend */

#define gen_error(format, ...) \
  { \
    bl_actor_error((Actor *)mod, ("(llvm_backend) "format), ##__VA_ARGS__); \
    longjmp(jmp_error, 1); \
  } 

static bool
run(LlvmBackend *self,
    Module      *mod);

static int
domain(LlvmBackend *self);

static void
gen_func(Module       *mod,
         LLVMModuleRef llvm_mod,
         NodeFuncDecl *node, 
         jmp_buf       jmp_error);

static void
gen_gstmt(Module         *mod,
          LLVMModuleRef   llvm_mod,
          NodeGlobalStmt *node, 
          jmp_buf         jmp_error);
         

/* class LlvmBackend constructor params */
bo_decl_params_begin(LlvmBackend)
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
  bo_vtbl_cl(klass, Stage)->domain
    = (int (*)(Stage*)) domain;
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
    Module      *mod)
{
  jmp_buf jmp_error;
  if (setjmp(jmp_error))
    return false;

  if (bo_array_size(mod->base.actors) == 0)
    return true;

  Unit *u = bo_array_at(mod->base.actors, 0, Unit *);
  Node *root = bl_ast_get_root(u->ast);

  /* TODO: solve only one unit for now */
  LLVMModuleRef llvm_mod = LLVMModuleCreateWithName(mod->name);
  

  switch (root->type) {
    case BL_NODE_GLOBAL_STMT:
      gen_gstmt(mod, llvm_mod, (NodeGlobalStmt *)root, jmp_error);
      break;
    default:
      gen_error("invalid node on llvm generator input");
  }

  char *error = NULL;
  LLVMVerifyModule(llvm_mod, LLVMAbortProcessAction, &error);
  
  if (strlen(error)) {
    bl_actor_error((Actor *)mod, "(llvm_backend) not verified with error %s", error);
    LLVMDisposeMessage(error);
    LLVMDisposeModule(llvm_mod);
    return false;
  }

  char *export_file = malloc(sizeof(char) * (strlen(u->filepath) + 4));
  strcpy(export_file, u->filepath);
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

int
domain(LlvmBackend *self)
{
  return BL_DOMAIN_MODULE;
}

void
gen_func(Module       *mod,
         LLVMModuleRef llvm_mod,
         NodeFuncDecl *node,
         jmp_buf       jmp_error)
{
  LLVMTypeRef param_types[] = { LLVMInt32Type(), LLVMInt32Type() };
  LLVMTypeRef ret_type = LLVMFunctionType(LLVMInt32Type(), param_types, 2, 0);
  LLVMValueRef sum = LLVMAddFunction(llvm_mod, "sum", ret_type);

  LLVMBasicBlockRef entry = LLVMAppendBasicBlock(sum, "entry");

  LLVMBuilderRef builder = LLVMCreateBuilder();
  LLVMPositionBuilderAtEnd(builder, entry);

  LLVMValueRef tmp = LLVMBuildAdd(builder, LLVMGetParam(sum, 0), LLVMGetParam(sum, 1), "tmp");
  LLVMBuildRet(builder, tmp);
}

void
gen_gstmt(Module         *mod,
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
        gen_func(mod, llvm_mod, (NodeFuncDecl *)child, jmp_error);
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

