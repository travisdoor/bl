//************************************************************************************************
// bl
//
// File:   ir.c
// Author: Martin Dorazil
// Date:   12/7/18
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

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Linker.h>
#include <llvm-c/DebugInfo.h>
#include "stages.h"
#include "common.h"

#define LOG_TAG BLUE("IR")

#if BL_DEBUG
#define gname(s) s
#define JIT_OPT_LEVEL 0
#else
#define gname(s) ""
#define JIT_OPT_LEVEL 3
#endif

typedef struct
{
  Builder *      builder;
  Assembly *     assembly;
  BHashTable *   llvm_modules;
  LLVMContextRef llvm_cnt;
  LLVMBuilderRef llvm_builder;
  LLVMModuleRef  llvm_module; /* current generated llvm module */

  /* current fn tmps */
  LLVMBasicBlockRef break_block;
  LLVMBasicBlockRef continue_block;
  LLVMBasicBlockRef fn_init_block;
  LLVMBasicBlockRef fn_ret_block;
  LLVMBasicBlockRef fn_entry_block;
  LLVMValueRef      fn_ret_val;

  bool verbose;
} Context;

static inline bool
generated(Context *cnt, AstDecl *decl)
{
  return bo_htbl_has_key(cnt->llvm_modules, (uint64_t)decl);
}

#if BL_DEBUG
static void
validate(LLVMModuleRef module);
#endif

static LLVMValueRef
fn_get(Context *cnt, AstDecl *fn);

static LLVMTypeRef
to_llvm_type(Context *cnt, AstType *type);

/* check if declaration has all it's dependencies already generated in LLVM Modules, by
 * 'strict_only' tag we can check onlu strict dependencies caused by '#run' directive */
static bool
is_satisfied(Context *cnt, AstDecl *decl, bool strict_only);

static LLVMValueRef
ir_node(Context *cnt, Ast *node);

static LLVMValueRef
ir_expr(Context *cnt, AstExpr *expr);

static LLVMValueRef
ir_decl(Context *cnt, AstDecl *decl);

static LLVMValueRef
ir_decl_fn(Context *cnt, AstDecl *decl_fn);

/* impl */
#if BL_DEBUG
void
validate(LLVMModuleRef module)
{
  char *error = NULL;
  if (LLVMVerifyModule(module, LLVMReturnStatusAction, &error)) {
    char *str = LLVMPrintModuleToString(module);
    bl_abort("module not verified with error: %s\n%s", error, str);
  }
  LLVMDisposeMessage(error);
}
#endif

LLVMTypeRef
to_llvm_type(Context *cnt, AstType *type)
{
  LLVMTypeRef result = NULL;
  return result;
}

LLVMValueRef
fn_get(Context *cnt, AstDecl *fn)
{
  const char *fn_name = fn->name->str;
  assert(fn_name);

  LLVMValueRef result = LLVMGetNamedFunction(cnt->llvm_module, fn_name);
  if (!result) {
    LLVMTypeRef llvm_type = to_llvm_type(cnt, fn->type);

    result = LLVMAddFunction(cnt->llvm_module, fn_name, llvm_type);
  }

  assert(result);
  return result;
}

LLVMValueRef
ir_node(Context *cnt, Ast *node)
{
  assert(node);
  switch (ast_kind(node)) {
  case AST_DECL:
    return ir_decl(cnt, (AstDecl *)node);
  case AST_EXPR:
    return ir_expr(cnt, (AstExpr *)node);

  default:
    bl_abort("missing ir generation for %s", ast_get_name(node));
  }

  return NULL;
}

LLVMValueRef
ir_expr(Context *cnt, AstExpr *expr)
{
  assert(expr);
  return NULL;
}

LLVMValueRef
ir_decl(Context *cnt, AstDecl *decl)
{
  switch (decl->kind) {
  case DECL_KIND_FN:
    return ir_decl_fn(cnt, decl);
  case DECL_KIND_FIELD:
    break;
  default:
    bl_abort("invalid declaration");
  }

  return NULL;
}

LLVMValueRef
ir_decl_fn(Context *cnt, AstDecl *decl_fn)
{
  fn_get(cnt, decl_fn);
  return NULL;
}

bool
is_satisfied(Context *cnt, AstDecl *decl, bool strict_only)
{
  assert(decl);
  BHashTable *deps = decl->deps;
  if (!deps) return true;

  bo_iterator_t iter;
  Dependency    dep;
  bhtbl_foreach(deps, iter)
  {
    dep = bo_htbl_iter_peek_value(deps, &iter, Dependency);

    // PERFORMANCE: is there some better solution than check whole tree???
    bool check_tree = (bool)(strict_only ? dep.type & DEP_STRICT : true);
    if (check_tree) {
      if (!generated(cnt, dep.decl)) {
        return false;
      } else if (!is_satisfied(cnt, dep.decl, false)) {
        return false;
      }
    }
  }

  return true;
}

void
generate(Context *cnt)
{
  BList *  queue = cnt->assembly->ir_queue;
  AstDecl *decl;

  while (!bo_list_empty(queue)) {
    decl = bo_list_front(queue, AstDecl *);
    bo_list_pop_front(queue);

    assert(decl->flags == 0 && "invalid flags");

    if (is_satisfied(cnt, decl, true)) {
      if (cnt->verbose) msg_log(LOG_TAG ": generate: '%s'", decl->name->str);

      /* prepare llvm module for currently generated declaration */
      cnt->llvm_module = LLVMModuleCreateWithNameInContext(decl->name->str, cnt->llvm_cnt);

      ir_node(cnt, (Ast *)decl);

#if BL_DEBUG
      validate(cnt->llvm_module);
#endif
      bo_htbl_insert(cnt->llvm_modules, (uint64_t)decl, cnt->llvm_module);
    } else {
      /* declaration is waiting for it's dependencies and need to be processed later */
      bo_list_push_back(queue, decl);
    }
  }
}

/* public */
void
ir_run(Builder *builder, Assembly *assembly)
{
  if (!bo_list_size(assembly->ir_queue)) {
    msg_warning("nothing to generate!");
    return;
  }

  Context cnt;
  memset(&cnt, 0, sizeof(Context));

  cnt.verbose      = builder->flags & BUILDER_VERBOSE;
  cnt.builder      = builder;
  cnt.llvm_cnt     = LLVMContextCreate();
  cnt.llvm_builder = LLVMCreateBuilderInContext(cnt.llvm_cnt);
  cnt.assembly     = assembly;
  cnt.llvm_modules = bo_htbl_new(sizeof(LLVMModuleRef), bo_list_size(assembly->ir_queue));

  generate(&cnt);

  /* cleanup */
  bo_unref(cnt.llvm_modules);
  LLVMDisposeBuilder(cnt.llvm_builder);
}
