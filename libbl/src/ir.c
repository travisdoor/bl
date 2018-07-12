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
#include "stages_impl.h"
#include "common_impl.h"
#include "ast_impl.h"

#if BL_DEBUG
#define gname(s) s
#else
#define gname(s) ""
#endif

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;

  LLVMModuleRef  llvm_module;
  LLVMBuilderRef llvm_builder;
  LLVMContextRef llvm_cnt;
  BHashTable *   llvm_modules;

  LLVMBasicBlockRef fn_init_block;
  LLVMBasicBlockRef fn_ret_block;
  LLVMBasicBlockRef fn_entry_block;
  LLVMValueRef      fn_ret_val;
} context_t;

static void
generate(context_t *cnt);

static LLVMValueRef
ir_decl(context_t *cnt, bl_node_t *decl);

static LLVMValueRef
ir_fn_decl_or_get(context_t *cnt, bl_node_t *fn);

static LLVMValueRef
ir_fn(context_t *cnt, bl_node_t *decl);

static LLVMValueRef
ir_mut(context_t *cnt, bl_node_t *decl);

static void
ir_block(context_t *cnt, bl_node_t *block);

static LLVMValueRef
ir_expr(context_t *cnt, bl_node_t *expr);

static LLVMValueRef
ir_lit(context_t *cnt, bl_node_t *lit);

static LLVMValueRef
ir_call(context_t *cnt, bl_node_t *call);

static LLVMTypeRef
to_llvm_type(context_t *cnt, bl_node_t *type);

// impl

LLVMTypeRef
to_llvm_type(context_t *cnt, bl_node_t *type)
{
  assert(type);
  LLVMTypeRef result = NULL;

  switch (bl_node_code(type)) {
  case BL_NODE_TYPE_FUND: {
    bl_node_type_fund_t *_type = bl_peek_type_fund(type);
    switch (_type->code) {
    case BL_FTYPE_VOID:
      result = LLVMVoidTypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_CHAR:
    case BL_FTYPE_S8:
    case BL_FTYPE_U8:
      result = LLVMInt8TypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_S16:
    case BL_FTYPE_U16:
      result = LLVMInt16TypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_S32:
    case BL_FTYPE_U32:
      result = LLVMInt32TypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_S64:
    case BL_FTYPE_U64:
      result = LLVMInt64TypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_F32:
      result = LLVMFloatTypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_F64:
      result = LLVMDoubleTypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_STRING:
      result = LLVMPointerType(LLVMInt8TypeInContext(cnt->llvm_cnt), 0);
      break;
    case BL_FTYPE_BOOL:
      result = LLVMInt1TypeInContext(cnt->llvm_cnt);
      break;
    case BL_FTYPE_SIZE:
      /* TODO: use target setup later */
      if (sizeof(size_t) == 4) {
        result = LLVMInt32TypeInContext(cnt->llvm_cnt);
      } else if (sizeof(size_t) == 8) {
        result = LLVMInt64TypeInContext(cnt->llvm_cnt);
      } else {
        bl_abort("unsupported architecture");
      }
      break;
    default:
      bl_abort("unknown fundamenetal type %s", bl_node_name(type));
    }
    break;
  }

  case BL_NODE_TYPE_FN: {
    /* args */
    bl_node_type_fn_t *_fn_type = bl_peek_type_fn(type);

    LLVMTypeRef  llvm_ret       = to_llvm_type(cnt, bl_ast_get_type(_fn_type->ret_type));
    LLVMTypeRef *llvm_arg_types = bl_malloc(sizeof(LLVMTypeRef) * _fn_type->argc_types);

    bl_node_t *arg;
    bl_node_t *tmp_type;
    int        i = 0;
    bl_node_foreach(_fn_type->arg_types, arg)
    {
      tmp_type            = bl_ast_get_type(arg);
      llvm_arg_types[i++] = to_llvm_type(cnt, tmp_type);
    }

    result = LLVMFunctionType(llvm_ret, llvm_arg_types, (unsigned int)i, false);
    bl_free(llvm_arg_types);
    break;
  }

  case BL_NODE_TYPE_STRUCT: {
    break;
  }

  default:
    bl_abort("invalid node type %s", bl_node_name(type));
  }

  return result;
}

LLVMValueRef
ir_lit(context_t *cnt, bl_node_t *lit)
{
  LLVMValueRef   result = NULL;
  bl_node_lit_t *_lit   = bl_peek_lit(lit);

#define PEEK_ULL _lit->token->value.u
#define PEEK_REAL _lit->token->value.d
#define PEEK_STR _lit->token->value.str
#define PEEK_CHAR (unsigned long long int)_lit->token->value.c

  switch (bl_peek_type_fund(_lit->type)->code) {
  case BL_FTYPE_S8:
    result = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case BL_FTYPE_S16:
    result = LLVMConstInt(LLVMInt16TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case BL_FTYPE_S32:
    result = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case BL_FTYPE_S64:
    result = LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case BL_FTYPE_U8:
    result = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case BL_FTYPE_U16:
    result = LLVMConstInt(LLVMInt16TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case BL_FTYPE_U32:
    result = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case BL_FTYPE_U64:
  case BL_FTYPE_SIZE:
    result = LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case BL_FTYPE_F32:
    result = LLVMConstReal(LLVMFloatTypeInContext(cnt->llvm_cnt), PEEK_REAL);
    break;
  case BL_FTYPE_F64:
    result = LLVMConstReal(LLVMDoubleTypeInContext(cnt->llvm_cnt), PEEK_REAL);
    break;
  case BL_FTYPE_BOOL:
    result = LLVMConstInt(LLVMInt1TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case BL_FTYPE_STRING:
    // TODO
    result = LLVMBuildGlobalStringPtr(cnt->llvm_builder, PEEK_STR, "str");
    // result = get_or_create_const_string(cnt, _lit->value.str);
    break;
  case BL_FTYPE_CHAR:
    result = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), PEEK_CHAR, false);
    break;
  default:
    bl_abort("invalid constant type %s", bl_node_name(lit));
  }

#undef PEEK_ULL
#undef PEEK_REAL
#undef PEEK_STR
#undef PEEK_CHAR

  return result;
}

LLVMValueRef
ir_call(context_t *cnt, bl_node_t *call)
{
  LLVMValueRef         result = NULL;
  bl_node_expr_call_t *_call  = bl_peek_expr_call(call);
  LLVMValueRef         fn     = ir_fn_decl_or_get(cnt, bl_peek_ident(_call->ident)->ref);
  assert(fn);

  LLVMValueRef *llvm_args = bl_malloc(sizeof(LLVMValueRef) * _call->argsc);

  bl_node_t *arg;
  int        i = 0;
  bl_node_foreach(_call->args, arg)
  {
    llvm_args[i] = ir_expr(cnt, arg);
    assert(llvm_args[i]);

    if (LLVMIsAAllocaInst(llvm_args[i]))
      llvm_args[i] = LLVMBuildLoad(cnt->llvm_builder, llvm_args[i], gname("tmp"));

    ++i;
  }

  result = LLVMBuildCall(cnt->llvm_builder, fn, llvm_args, _call->argsc, "");
  bl_free(llvm_args);
  return result;
}

LLVMValueRef
ir_expr(context_t *cnt, bl_node_t *expr)
{
  LLVMValueRef result = NULL;
  switch (bl_node_code(expr)) {
  case BL_NODE_EXPR_CALL:
    result = ir_call(cnt, expr);
    break;
  case BL_NODE_LIT:
    result = ir_lit(cnt, expr);
    break;
  default:
    break;
  }

  return result;
}

void
ir_block(context_t *cnt, bl_node_t *block)
{
  bl_node_decl_block_t *_block = bl_peek_decl_block(block);
  bl_node_t *           stmt;

  bl_node_foreach(_block->nodes, stmt)
  {
    ir_expr(cnt, stmt);
  }
}

LLVMValueRef
ir_mut(context_t *cnt, bl_node_t *decl)
{
  LLVMValueRef          result    = NULL;
  bl_node_decl_value_t *_decl     = bl_peek_decl_value(decl);
  LLVMTypeRef           llvm_type = to_llvm_type(cnt, _decl->type);

  LLVMBuildAlloca(cnt->llvm_builder, llvm_type, bl_peek_ident(_decl->name)->str);

  return result;
}

LLVMValueRef
ir_fn_decl_or_get(context_t *cnt, bl_node_t *fn)
{
  LLVMValueRef          result  = NULL;
  bl_node_decl_value_t *_fn     = bl_peek_decl_value(fn);
  const char *          fn_name = bl_peek_ident(_fn->name)->str;

  result = LLVMGetNamedFunction(cnt->llvm_module, fn_name);
  if (!result) {

    LLVMTypeRef llvm_type = to_llvm_type(cnt, _fn->type);
    result                = LLVMAddFunction(cnt->llvm_module, fn_name, llvm_type);
  }

  return result;
}

LLVMValueRef
ir_fn(context_t *cnt, bl_node_t *decl)
{
  LLVMValueRef          result = ir_fn_decl_or_get(cnt, decl);
  bl_node_decl_value_t *_decl  = bl_peek_decl_value(decl);
  bl_node_lit_fn_t *    _fn    = bl_peek_lit_fn(_decl->value);

  {
    assert(_decl->value);
    cnt->fn_init_block  = LLVMAppendBasicBlock(result, gname("init"));
    cnt->fn_entry_block = LLVMAppendBasicBlock(result, gname("entry"));
    cnt->fn_ret_block   = LLVMAppendBasicBlock(result, gname("exit"));

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);

    bl_node_type_fn_t *_fn_type = bl_peek_type_fn(_fn->type);
    /*
     * Create named references to function parameters so they
     * can be called by name in function body.
     */
    bl_node_t *arg;
    bl_node_foreach(_fn_type->arg_types, arg)
    {
      ir_decl(cnt, arg);
    }

    /*
     * Prepare return value.
     */
    LLVMTypeRef llvm_ret_type = to_llvm_type(cnt, bl_ast_get_type(_fn_type->ret_type));
    if (llvm_ret_type != LLVMVoidTypeInContext(cnt->llvm_cnt)) {
      cnt->fn_ret_val = LLVMBuildAlloca(cnt->llvm_builder, llvm_ret_type, gname("ret"));
    } else {
      cnt->fn_ret_val = NULL;
    }

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_entry_block);
  }

  /* generate function body */
  ir_block(cnt, _fn->block);

  {
    LLVMBasicBlockRef curr_block = LLVMGetInsertBlock(cnt->llvm_builder);

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);
    LLVMBuildBr(cnt->llvm_builder, cnt->fn_entry_block);

    if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
      LLVMPositionBuilderAtEnd(cnt->llvm_builder, curr_block);
      LLVMBuildBr(cnt->llvm_builder, cnt->fn_ret_block);
    }

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_ret_block);
    if (cnt->fn_ret_val) {
      cnt->fn_ret_val = LLVMBuildLoad(cnt->llvm_builder, cnt->fn_ret_val, gname("tmp"));
      LLVMBuildRet(cnt->llvm_builder, cnt->fn_ret_val);
    } else {
      LLVMBuildRetVoid(cnt->llvm_builder);
    }
  }

  return result;
}

LLVMValueRef
ir_decl(context_t *cnt, bl_node_t *decl)
{
  bl_node_decl_value_t *_decl = bl_peek_decl_value(decl);
  assert(_decl->type);

  if (_decl->mutable) {
    switch (bl_node_code(_decl->type)) {
    case BL_NODE_TYPE_FUND:
    case BL_NODE_TYPE_STRUCT:
      return ir_mut(cnt, decl);
    case BL_NODE_TYPE_FN:
      break;
    default:
      bl_abort("invalid type");
    }
  } else {
    switch (bl_node_code(_decl->type)) {
    case BL_NODE_TYPE_FN:
      return ir_fn(cnt, decl);
    default:
      bl_abort("invalid type");
    }
  }

  return NULL;
}

void
generate(context_t *cnt)
{
  BList *               queue = cnt->assembly->ir_queue;
  bl_node_t *           decl;
  bl_node_decl_value_t *_decl;

  while (!bo_list_empty(queue)) {
    decl = bo_list_front(queue, bl_node_t *);
    bo_list_pop_front(queue);

    _decl = bl_peek_decl_value(decl);
    // if (!_decl->used) continue;

    cnt->llvm_module =
        LLVMModuleCreateWithNameInContext(bl_peek_ident(_decl->name)->str, cnt->llvm_cnt);
    ir_decl(cnt, decl);
    bo_htbl_insert(cnt->llvm_modules, (uint64_t)decl, cnt->llvm_module);
  }
}

void
bl_ir_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt;
  cnt.builder        = builder;
  cnt.assembly       = assembly;
  cnt.fn_entry_block = NULL;
  cnt.fn_init_block  = NULL;
  cnt.fn_ret_block   = NULL;
  cnt.fn_ret_val     = NULL;
  cnt.llvm_cnt       = LLVMContextCreate();
  cnt.llvm_builder   = LLVMCreateBuilderInContext(cnt.llvm_cnt);
  cnt.llvm_modules   = bo_htbl_new(sizeof(LLVMModuleRef), bo_list_size(assembly->ir_queue));
  cnt.llvm_module    = NULL;

  generate(&cnt);

  /* temporary link all generated modules until we will have dependency stuff */
  assembly->llvm_module = LLVMModuleCreateWithNameInContext("main", cnt.llvm_cnt);
  ;

  LLVMModuleRef llvm_module;
  bo_iterator_t it;
  bl_bhtbl_foreach(cnt.llvm_modules, it)
  {
    llvm_module = bo_htbl_iter_peek_value(cnt.llvm_modules, &it, LLVMModuleRef);
    assert(llvm_module);

#define PRINT_IR
#ifdef PRINT_IR
    {
      char *str = LLVMPrintModuleToString(llvm_module);
      bl_log("\n--------------------------------------------------------------------------------"
             "\n%s"
             "\n--------------------------------------------------------------------------------",
             str);
      LLVMDisposeMessage(str);
    }
#endif
#undef PRINT_IR

    LLVMLinkModules2(assembly->llvm_module, llvm_module);
  }

#define PRINT_IR
#ifdef PRINT_IR
  {
    char *str = LLVMPrintModuleToString(assembly->llvm_module);
    bl_log("\n--------------------------------------------------------------------------------"
           "\n%s"
           "\n--------------------------------------------------------------------------------",
           str);
    LLVMDisposeMessage(str);
  }
#endif
#undef PRINT_IR

  assembly->llvm_cnt = cnt.llvm_cnt;
}
