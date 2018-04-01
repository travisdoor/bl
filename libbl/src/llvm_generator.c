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

#include "common_impl.h"
#include "stages_impl.h"
#include "ast/visitor_impl.h"

#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define DEBUG_NAMES 0

#if DEBUG_NAMES
#define gname(s) s
#else
#define gname(s) ""
#endif

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;

  LLVMModuleRef  mod;
  LLVMBuilderRef llvm_builder;
  LLVMContextRef llvm_cnt;

  /* tmps */
  BHashTable *      func_decls;
  LLVMBasicBlockRef func_init_block;
  LLVMBasicBlockRef func_ret_block;
  LLVMBasicBlockRef func_entry_block;
} context_t;

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
    default: {
      bl_abort("invalide type");
    }
    }
  }

  bl_abort("invalid type");
}

static int
gen_func_params(context_t *cnt, bl_decl_func_t *func, LLVMTypeRef *out)
{
  int       out_i = 0;
  const int c     = bl_ast_func_arg_count(func);

  /* no params */
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
static void
gen_func(context_t *cnt, bl_node_t *func, bool forward)
{
  bl_decl_func_t *_func     = bl_peek_decl_func(func);
  LLVMValueRef    llvm_func = NULL;
  if (bo_htbl_has_key(cnt->func_decls, (uint64_t)func)) {
    llvm_func = bo_htbl_at(cnt->func_decls, (uint64_t)func, LLVMValueRef);
  }

  /* params */
  LLVMTypeRef param_types[BL_MAX_FUNC_ARG_COUNT] = {0};
  const int   pc                                 = gen_func_params(cnt, _func, param_types);

  if (llvm_func == NULL) {
    LLVMTypeRef ret      = to_llvm_type(cnt, _func->ret_type);
    LLVMTypeRef ret_type = LLVMFunctionType(ret, param_types, (unsigned int)pc, false);
    llvm_func            = LLVMAddFunction(cnt->mod, _func->id.str, ret_type);

    bo_htbl_insert(cnt->func_decls, (uint64_t)func, llvm_func);
  }

  if (!forward && _func->block) {
    cnt->func_init_block          = LLVMAppendBasicBlock(llvm_func, gname("init"));
    LLVMBasicBlockRef entry_block = LLVMAppendBasicBlock(llvm_func, gname("entry"));
    cnt->func_ret_block           = LLVMAppendBasicBlock(llvm_func, gname("exit"));
  }
}

/* visitors */
/**************************************************************************************************/
static void
visit_func(bl_visitor_t *visitor, bl_node_t *func)
{
  bl_log("generate function: %p", func);
  gen_func(peek_cnt(visitor), func, false);
  bl_visitor_walk_func(visitor, func);
}

static void
visit_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  context_t *cnt = peek_cnt(visitor);
  switch (bl_node_code(expr)) {
  case BL_EXPR_CALL:
    bl_log("call to:           %p", bl_peek_expr_call(expr)->ref);
    gen_func(cnt, bl_peek_expr_call(expr)->ref, true);
    break;
  default:
    break;
  }
}

/* main entry function */
/**************************************************************************************************/
bl_error_e
bl_llvm_gen_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  bl_unit_t *unit = NULL;
  size_t     c    = bo_array_size(assembly->units);

  /* context initialization */
  context_t cnt;
  cnt.builder      = builder;
  cnt.llvm_cnt     = LLVMContextCreate();
  cnt.mod          = LLVMModuleCreateWithNameInContext(assembly->name, cnt.llvm_cnt);
  cnt.llvm_builder = LLVMCreateBuilderInContext(cnt.llvm_cnt);
  cnt.func_decls   = bo_htbl_new(sizeof(LLVMValueRef), 2048);

  bl_visitor_t visitor_gen;
  bl_visitor_init(&visitor_gen, &cnt);
  bl_visitor_add(&visitor_gen, visit_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_gen, visit_expr, BL_VISIT_EXPR);

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor_gen, unit->ast.root);
  }

#ifdef BL_DEBUG
  char *error;
  if (LLVMVerifyModule(cnt.mod, LLVMReturnStatusAction, &error)) {
    char *str = LLVMPrintModuleToString(cnt.mod);
    bl_abort("module not verified with error: %s\n%s", error, str);
  }
#endif

  char *str = LLVMPrintModuleToString(cnt.mod);
  bl_log("\n%s", str);
  LLVMDisposeMessage(str);

  /* context destruction */
  LLVMDisposeBuilder(cnt.llvm_builder);
  LLVMDisposeModule(cnt.mod);
  LLVMContextDispose(cnt.llvm_cnt);
  bo_unref(cnt.func_decls);

  return BL_NO_ERR;
}
