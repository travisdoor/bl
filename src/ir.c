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
#define EXPECTED_TYPE_COUNT 4096
#define EXPECTED_VALUE_COUNT 256

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
  BHashTable *   llvm_values;
  BHashTable *   llvm_modules;
  BHashTable *   llvm_types;
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

  /* buildins */
  LLVMTypeRef       llvm_entry_void;

  bool verbose;
} Context;

/* Generate declaration in global scope */
static inline bool
generated(Context *cnt, AstDecl *decl)
{
  return bo_htbl_has_key(cnt->llvm_modules, (uint64_t)decl);
}

static inline void
llvm_values_insert(Context *cnt, Ast *node, void *val)
{
  bo_htbl_insert(cnt->llvm_values, (uint64_t)node, val);
}

static inline void *
llvm_values_get(Context *cnt, Ast *node)
{
  if (bo_htbl_has_key(cnt->llvm_values, (uint64_t)node))
    return bo_htbl_at(cnt->llvm_values, (uint64_t)node, void *);
  return NULL;
}

static inline void
llvm_values_reset(Context *cnt)
{
  bo_htbl_clear(cnt->llvm_values);
}

/* generate LLVM Load instruction if rvalue can be converted to lvalue, otherwise return unchanged
 * value */
static inline LLVMValueRef
ltor_if_needed(Context *cnt, AstExpr *expr, LLVMValueRef val)
{
  const AdrMode m = ast_get_adrmode(expr);
  if (m == ADR_MODE_IMMUT || ast_get_adrmode(expr) == ADR_MODE_MUT)
    return LLVMBuildLoad(cnt->llvm_builder, val, gname("tmp"));
  return val;
}

#if BL_DEBUG
static void
validate(LLVMModuleRef module);

static void
print_llvm_module(LLVMModuleRef module);
#endif

static LLVMValueRef
fn_get(Context *cnt, AstDecl *fn);

static LLVMValueRef
global_get(Context *cnt, AstDecl *global);

static LLVMTypeRef
to_llvm_type(Context *cnt, AstType *type);

/* check if declaration has all it's dependencies already generated in LLVM Modules, by
 * 'strict_only' tag we can check onlu strict dependencies caused by '#run' directive */
static bool
is_satisfied(Context *cnt, AstDecl *decl, bool strict_only);

static LLVMValueRef
ir_node(Context *cnt, Ast *node);

static LLVMValueRef
ir_block(Context *cnt, AstBlock *block);

static LLVMValueRef
ir_decl(Context *cnt, AstDecl *decl);

static LLVMValueRef
ir_decl_fn(Context *cnt, AstDecl *decl_fn);

static LLVMValueRef
ir_decl_field(Context *cnt, AstDecl *decl_field);

static LLVMValueRef
ir_expr(Context *cnt, AstExpr *expr);

static LLVMValueRef
ir_expr_lit_fn(Context *cnt, AstExprLitFn *lit_fn);

static LLVMValueRef
ir_expr_lit_int(Context *cnt, AstExprLitInt *lit_int);

static LLVMValueRef
ir_expr_binop(Context *cnt, AstExprBinop *binop);

static LLVMValueRef
ir_expr_unary(Context *cnt, AstExprUnary *unary);

static LLVMValueRef
ir_expr_ref(Context *cnt, AstExprRef *ref);

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

void
print_llvm_module(LLVMModuleRef module)
{
  char *str = LLVMPrintModuleToString(module);
  msg_log(LOG_TAG ":\n%s\n", str);
  LLVMDisposeMessage(str);
}
#endif

LLVMTypeRef
to_llvm_type(Context *cnt, AstType *type)
{
  assert(type);
  if (bo_htbl_has_key(cnt->llvm_types, (uint64_t)type)) {
    return bo_htbl_at(cnt->llvm_types, (uint64_t)type, LLVMTypeRef);
  }

  LLVMTypeRef result = NULL;

  switch (type->kind) {
  case AST_TYPE_INT: {
    result = LLVMIntTypeInContext(cnt->llvm_cnt, type->integer.bitcount);
    bo_htbl_insert(cnt->llvm_types, (uint64_t)type, result);
    break;
  }

  case AST_TYPE_FN: {
    /* args */
    LLVMTypeRef llvm_ret_type = NULL;

    if (!type->fn.ret_type)
      llvm_ret_type = cnt->llvm_entry_void;
    else
      to_llvm_type(cnt, type->fn.ret_type);

    LLVMTypeRef *llvm_arg_types = bl_malloc(sizeof(LLVMTypeRef) * type->fn.argc);
    if (!llvm_arg_types) bl_abort("bad alloc");

    Ast *    tmp;
    AstArg * arg;
    unsigned i = 0;
    node_foreach(type->fn.args, tmp)
    {
      assert(tmp->kind == AST_ARG);
      arg                 = (AstArg *)tmp;
      llvm_arg_types[i++] = to_llvm_type(cnt, arg->type);
    }

    result = LLVMFunctionType(llvm_ret_type, llvm_arg_types, i, false);
    bl_free(llvm_arg_types);
    bo_htbl_insert(cnt->llvm_types, (uint64_t)type, result);
    break;
  }

  default:
    bl_abort("invalid type");
  }

  assert(result);
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
global_get(Context *cnt, AstDecl *global)
{
  const char *g_name = global->name->str;
  assert(g_name);

  LLVMValueRef result = LLVMGetNamedGlobal(cnt->llvm_module, g_name);
  if (!result) {
    assert(global->type);
    LLVMTypeRef llvm_type = to_llvm_type(cnt, global->type);
    result                = LLVMAddGlobal(cnt->llvm_module, llvm_type, g_name);
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
  case AST_BLOCK:
    return ir_block(cnt, (AstBlock *)node);
  case AST_EXPR:
    return ir_expr(cnt, (AstExpr *)node);

  default:
    bl_abort("missing ir generation for %s", ast_get_name(node));
  }

  return NULL;
}

LLVMValueRef
ir_decl(Context *cnt, AstDecl *decl)
{
  switch (decl->kind) {
  case DECL_KIND_FN:
    return ir_decl_fn(cnt, decl);
  case DECL_KIND_FIELD:
    return ir_decl_field(cnt, decl);
  default:
    bl_abort("invalid declaration");
  }

  return NULL;
}

/* OTHER */
LLVMValueRef
ir_block(Context *cnt, AstBlock *block)
{
  Ast *node;
  node_foreach(block->nodes, node) ir_node(cnt, node);
  return NULL;
}

/* DECLARATIONS */
LLVMValueRef
ir_decl_fn(Context *cnt, AstDecl *decl_fn)
{
  /* local functions will be generated in separate module */
  if (!decl_fn->in_gscope) return NULL;
  assert(decl_fn->value);
  assert(decl_fn->type);

  LLVMValueRef result = fn_get(cnt, decl_fn);

  {
    cnt->fn_init_block  = LLVMAppendBasicBlock(result, gname("init"));
    cnt->fn_entry_block = LLVMAppendBasicBlock(result, gname("entry"));
    cnt->fn_ret_block   = LLVMAppendBasicBlock(result, gname("exit"));

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);

    assert(decl_fn->type->kind == AST_TYPE_FN);
    AstTypeFn *type_fn = (AstTypeFn *)decl_fn->type;

    /*
     * Create named references to function parameters so they
     * can be called by name in function body.
     */

    Ast *   tmp;
    AstArg *arg;
    int     i = 0;
    node_foreach(type_fn->args, tmp)
    {
      assert(tmp->kind == AST_ARG);
      arg                = (AstArg *)tmp;
      const char * name  = arg->name->str;
      LLVMValueRef p     = LLVMGetParam(result, (unsigned int)i++);
      LLVMValueRef p_tmp = LLVMBuildAlloca(cnt->llvm_builder, LLVMTypeOf(p), gname(name));
      LLVMBuildStore(cnt->llvm_builder, p, p_tmp);
      llvm_values_insert(cnt, tmp, p);
    }

    /*
     * Prepare return value.
     */
    if (type_fn->ret_type) {
      LLVMTypeRef llvm_ret_type = to_llvm_type(cnt, type_fn->ret_type);
      cnt->fn_ret_val           = LLVMBuildAlloca(cnt->llvm_builder, llvm_ret_type, gname("ret"));
    } else {
      cnt->fn_ret_val = NULL;
    }

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_entry_block);
  }

  /* Generate function body */
  assert(decl_fn->value->kind == AST_EXPR_LIT_FN);
  ir_node(cnt, (Ast *)decl_fn->value);

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

  /* reset tmps */
  cnt->fn_entry_block = NULL;
  cnt->fn_init_block  = NULL;
  cnt->fn_ret_block   = NULL;
  cnt->fn_ret_val     = NULL;
  cnt->break_block    = NULL;
  cnt->continue_block = NULL;

  return NULL;
}

LLVMValueRef
ir_decl_field(Context *cnt, AstDecl *decl_field)
{
  LLVMValueRef result = NULL;
  assert(decl_field->type && decl_field->name);
  LLVMTypeRef llvm_type = to_llvm_type(cnt, decl_field->type);

  if (decl_field->in_gscope) {
    /* declaration in global scope */
    result            = global_get(cnt, decl_field);
    LLVMValueRef init = ir_node(cnt, (Ast *)decl_field->value);
    assert(init);

    LLVMSetInitializer(result, init);
  } else {
    LLVMBasicBlockRef prev_block = LLVMGetInsertBlock(cnt->llvm_builder);
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);
    result = LLVMBuildAlloca(cnt->llvm_builder, llvm_type, gname(decl_field->name->str));
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, prev_block);

    llvm_values_insert(cnt, (Ast *)decl_field, result);

    if (!decl_field->value) return result;

    LLVMValueRef init = ir_node(cnt, (Ast *)decl_field->value);
    init              = ltor_if_needed(cnt, decl_field->value, init);

    LLVMBuildStore(cnt->llvm_builder, init, result);
  }
  return result;
}

/* EXPRESSIONS */
LLVMValueRef
ir_expr(Context *cnt, AstExpr *expr)
{
  assert(expr);
  switch (expr->kind) {
  case AST_EXPR_UNARY:
    return ir_expr_unary(cnt, (AstExprUnary *)expr);
  case AST_EXPR_BINOP:
    return ir_expr_binop(cnt, (AstExprBinop *)expr);
  case AST_EXPR_LIT_FN:
    return ir_expr_lit_fn(cnt, (AstExprLitFn *)expr);
  case AST_EXPR_LIT_INT:
    return ir_expr_lit_int(cnt, (AstExprLitInt *)expr);
  case AST_EXPR_REF:
    return ir_expr_ref(cnt, (AstExprRef *)expr);
  default:
    bl_abort("missing ir generation for %s", ast_get_name((Ast *)expr));
  }
  return NULL;
}

LLVMValueRef
ir_expr_ref(Context *cnt, AstExprRef *ref)
{
  assert(ref->ref);

  LLVMValueRef result = llvm_values_get(cnt, ref->ref);
  assert(result);

  return result;
}

LLVMValueRef
ir_expr_lit_fn(Context *cnt, AstExprLitFn *lit_fn)
{
  assert(lit_fn->block);
  return ir_node(cnt, (Ast *)lit_fn->block);
}

LLVMValueRef
ir_expr_lit_int(Context *cnt, AstExprLitInt *lit_int)
{
  LLVMValueRef result    = NULL;
  AstTypeInt * type      = (AstTypeInt *)ast_get_type((AstExpr *)lit_int);
  LLVMTypeRef  llvm_type = to_llvm_type(cnt, (AstType *)type);

  result = LLVMConstInt(llvm_type, lit_int->i, type->is_signed);
  assert(result);

  return result;
}

LLVMValueRef
ir_expr_unary(Context *cnt, AstExprUnary *unary)
{
  assert(unary->next);
  LLVMValueRef result = ir_node(cnt, (Ast *)unary->next);

  switch (unary->kind) {
  case UNOP_NEG:
  case UNOP_POS: {
    bl_abort("unimplemented");
  }

  case UNOP_NOT: {
    bl_abort("unimplemented");
  }

  case UNOP_ADR: {
    bl_abort("unimplemented");
  }

  case UNOP_DEREF: {
    result = LLVMBuildLoad(cnt->llvm_builder, result, gname("tmp"));
    break;
  }

  default:
    bl_abort("invalid unary operation kind");
  }

  return result;
}

LLVMValueRef
ir_expr_binop(Context *cnt, AstExprBinop *binop)
{
  LLVMValueRef llvm_lhs = ir_node(cnt, (Ast *)binop->lhs);
  LLVMValueRef llvm_rhs = ir_node(cnt, (Ast *)binop->rhs);
  assert(llvm_lhs && llvm_rhs);

  LLVMTypeKind lhs_kind   = LLVMGetTypeKind(LLVMTypeOf(llvm_lhs));
  bool         float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

  /* Assignments */
  switch (binop->kind) {
  case BINOP_ASSIGN: {
    llvm_rhs = ltor_if_needed(cnt, binop->rhs, llvm_rhs);
    LLVMBuildStore(cnt->llvm_builder, llvm_rhs, llvm_lhs);
    return llvm_lhs;
  }

  case BINOP_ADD_ASSIGN: {
    bl_abort("unimplemented");
  }

  case BINOP_SUB_ASSIGN: {
    bl_abort("unimplemented");
  }

  case BINOP_MUL_ASSIGN: {
    bl_abort("unimplemented");
  }

  case BINOP_DIV_ASSIGN: {
    bl_abort("unimplemented");
  }

  case BINOP_MOD_ASSIGN: {
    bl_abort("unimplemented");
  }

  case BINOP_ADD: {
    if (float_kind) return LLVMBuildFAdd(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
    return LLVMBuildAdd(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_SUB: {
    if (float_kind) return LLVMBuildFSub(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
    return LLVMBuildSub(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_MUL: {
    if (float_kind) return LLVMBuildFMul(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
    return LLVMBuildMul(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_DIV: {
    if (float_kind) return LLVMBuildFDiv(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
    return LLVMBuildSDiv(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_MOD: {
    return LLVMBuildSRem(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_EQ: {
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOEQ, llvm_lhs, llvm_rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntEQ, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_NEQ: {
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealONE, llvm_lhs, llvm_rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntNE, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_GREATER: {
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGT, llvm_lhs, llvm_rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGT, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_LESS: {
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLT, llvm_lhs, llvm_rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLT, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_GREATER_EQ: {
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGE, llvm_lhs, llvm_rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGE, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_LESS_EQ: {
    if (float_kind)
      return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLE, llvm_lhs, llvm_rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLE, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_LOGIC_AND: {
    return LLVMBuildAnd(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  case BINOP_LOGIC_OR: {
    return LLVMBuildOr(cnt->llvm_builder, llvm_lhs, llvm_rhs, gname("tmp"));
  }

  default:
    bl_abort("unknown binop");
  }
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
      print_llvm_module(cnt->llvm_module);
      validate(cnt->llvm_module);
#endif
      llvm_values_reset(cnt);
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

  cnt.verbose         = builder->flags & BUILDER_VERBOSE;
  cnt.builder         = builder;
  cnt.llvm_cnt        = LLVMContextCreate();
  cnt.llvm_builder    = LLVMCreateBuilderInContext(cnt.llvm_cnt);
  cnt.assembly        = assembly;
  cnt.llvm_modules    = bo_htbl_new(sizeof(LLVMModuleRef), bo_list_size(assembly->ir_queue));
  cnt.llvm_types      = bo_htbl_new(sizeof(LLVMTypeRef), EXPECTED_TYPE_COUNT);
  cnt.llvm_values     = bo_htbl_new(sizeof(LLVMValueRef), EXPECTED_VALUE_COUNT);
  cnt.llvm_entry_void = LLVMVoidTypeInContext(cnt.llvm_cnt);

  generate(&cnt);

  /* cleanup */
  bo_unref(cnt.llvm_modules);
  bo_unref(cnt.llvm_types);
  bo_unref(cnt.llvm_values);
  LLVMDisposeBuilder(cnt.llvm_builder);
}
