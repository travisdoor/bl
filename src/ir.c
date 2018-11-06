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

  bool verbose;
} Context;

/* Generate declaration in global scope */
static inline bool
generated(Context *cnt, AstDeclEntity *decl)
{
  return bo_htbl_has_key(cnt->llvm_modules, (uint64_t)decl);
}

static inline void
llvm_values_insert(Context *cnt, AstDecl *node, void *val)
{
  bo_htbl_insert(cnt->llvm_values, (uint64_t)node, val);
}

static inline void *
llvm_values_get(Context *cnt, AstDecl *node)
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
  const AdrMode m = expr->adr_mode;
  if (m == ADR_MODE_IMMUT || m == ADR_MODE_MUT)
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
fn_get(Context *cnt, AstDeclEntity *fn);

static LLVMValueRef
global_get(Context *cnt, AstDeclEntity *global);

static LLVMTypeRef
to_llvm_type(Context *cnt, AstType *type);

static LLVMModuleRef
link(Context *cnt, AstDeclEntity *entry);

/* resursive version */
static LLVMModuleRef
_link(Context *cnt, AstDeclEntity *entry);

/* check if declaration has all it's dependencies already generated in LLVM Modules, by
 * 'strict_only' tag we can check onlu strict dependencies caused by '#run' directive */
static bool
is_satisfied(Context *cnt, AstDeclEntity *decl, bool strict_only);

static void
ir_node(Context *cnt, Ast *node);

static void
ir_block(Context *cnt, AstBlock *block);

static void
ir_decl(Context *cnt, AstDecl *decl);

static void
ir_decl_entity(Context *cnt, AstDeclEntity *entity);

static void
ir_decl_fn(Context *cnt, AstDeclEntity *decl_fn);

static void
ir_decl_field(Context *cnt, AstDeclEntity *decl_field);

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

static LLVMValueRef
ir_expr_call(Context *cnt, AstExprCall *call);

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
    result = LLVMIntTypeInContext(cnt->llvm_cnt, (unsigned int)((AstTypeInt *)type)->bitcount);
    bo_htbl_insert(cnt->llvm_types, (uint64_t)type, result);
    break;
  }

  case AST_TYPE_VOID: {
    result = LLVMVoidTypeInContext(cnt->llvm_cnt);
    bo_htbl_insert(cnt->llvm_types, (uint64_t)type, result);
    break;
  }

  case AST_TYPE_FN: {
    /* args */
    AstTypeFn *  fn             = (AstTypeFn *)type;
    LLVMTypeRef  llvm_ret_type  = to_llvm_type(cnt, fn->ret_type);
    const int    argc           = bo_array_size(fn->args);
    LLVMTypeRef *llvm_arg_types = bl_malloc(sizeof(LLVMTypeRef) * argc);
    if (!llvm_arg_types) bl_abort("bad alloc");

    Ast *    arg;
    barray_foreach(fn->args, arg)
    {
      assert(arg->kind == AST_DECL);
      llvm_arg_types[i] = to_llvm_type(cnt, ((AstDecl *)arg)->type);
    }

    result = LLVMFunctionType(llvm_ret_type, llvm_arg_types, argc, false);
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
fn_get(Context *cnt, AstDeclEntity *fn)
{
  const char *fn_name = fn->base.name->str;
  assert(fn_name);

  LLVMValueRef result = LLVMGetNamedFunction(cnt->llvm_module, fn_name);
  if (!result) {
    LLVMTypeRef llvm_type = to_llvm_type(cnt, fn->base.type);

    result = LLVMAddFunction(cnt->llvm_module, fn_name, llvm_type);
  }

  assert(result);
  return result;
}

LLVMValueRef
global_get(Context *cnt, AstDeclEntity *global)
{
  AstDecl *   base   = (AstDecl *)global;
  const char *g_name = base->name->str;
  assert(g_name);

  LLVMValueRef result = LLVMGetNamedGlobal(cnt->llvm_module, g_name);
  if (!result) {
    assert(base->type);
    LLVMTypeRef llvm_type = to_llvm_type(cnt, base->type);
    result                = LLVMAddGlobal(cnt->llvm_module, llvm_type, g_name);
  }

  assert(result);
  return result;
}

LLVMModuleRef
link(Context *cnt, AstDeclEntity *entry)
{
  if (!entry) return NULL;
  LLVMModuleRef dest_module = _link(cnt, entry);
  return dest_module;
}

LLVMModuleRef
_link(Context *cnt, AstDeclEntity *entry)
{
  if (!bo_htbl_has_key(cnt->llvm_modules, (uint64_t)entry)) return NULL;

  LLVMModuleRef dest_module = bo_htbl_at(cnt->llvm_modules, (uint64_t)entry, LLVMModuleRef);
  assert(dest_module);
  if (!entry->deps) return dest_module;

  Dependency    dep;
  bo_iterator_t it;
  bhtbl_foreach(entry->deps, it)
  {
    dep = bo_htbl_iter_peek_value(entry->deps, &it, Dependency);

    /* link all lax dependencies */
    if (dep.type & DEP_LAX) {
      /* must be linked */
      LLVMModuleRef src_module = _link(cnt, dep.decl);

      if (src_module) {
        if (LLVMLinkModules2(dest_module, src_module)) bl_abort("unable to link modules");

        bo_htbl_erase_key(cnt->llvm_modules, (uint64_t)dep.decl);
      }
    }
  }

  return dest_module;
}

void
ir_node(Context *cnt, Ast *node)
{
  assert(node);
  switch (node->kind) {
  case AST_DECL:
    ir_decl(cnt, (AstDecl *)node);
    break;
  case AST_BLOCK:
    ir_block(cnt, (AstBlock *)node);
    break;
  case AST_EXPR:
    ir_expr(cnt, (AstExpr *)node);
    break;

  default:
    bl_abort("missing ir generation for %s", ast_get_name(node));
  }
}

void
ir_decl(Context *cnt, AstDecl *decl)
{
  switch (decl->kind) {
  case AST_DECL_ENTITY:
    ir_decl_entity(cnt, (AstDeclEntity *)decl);
    break;
  default:
    bl_abort("invalid declaration");
  }
}

void
ir_decl_entity(Context *cnt, AstDeclEntity *entity)
{
  switch (entity->kind) {
  case DECL_ENTITY_FN:
    ir_decl_fn(cnt, entity);
    break;
  case DECL_ENTITY_FIELD:
    ir_decl_field(cnt, entity);
    break;
  default:
    bl_abort("invalid declaration");
  }
}

/* OTHER */
void
ir_block(Context *cnt, AstBlock *block)
{
  Ast *node;
  barray_foreach(block->nodes, node) ir_node(cnt, node);
}

/* DECLARATIONS */
void
ir_decl_fn(Context *cnt, AstDeclEntity *decl_fn)
{
  /* local functions will be generated in separate module */
  if (!decl_fn->in_gscope) return;
  assert(decl_fn->value);
  assert(decl_fn->base.type);

  LLVMValueRef result = fn_get(cnt, decl_fn);

  {
    cnt->fn_init_block  = LLVMAppendBasicBlock(result, gname("init"));
    cnt->fn_entry_block = LLVMAppendBasicBlock(result, gname("entry"));
    cnt->fn_ret_block   = LLVMAppendBasicBlock(result, gname("exit"));

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);

    assert(decl_fn->base.type->kind == AST_TYPE_FN);
    AstTypeFn *type_fn = (AstTypeFn *)decl_fn->base.type;

    /*
     * Create named references to function parameters so they
     * can be called by name in function body.
     */

    AstDeclArg *tmp;
    barray_foreach(type_fn->args, tmp)
    {
      const char * name  = tmp->base.name->str;
      LLVMValueRef p     = LLVMGetParam(result, (unsigned int)i++);
      LLVMValueRef p_tmp = LLVMBuildAlloca(cnt->llvm_builder, LLVMTypeOf(p), gname(name));
      LLVMBuildStore(cnt->llvm_builder, p, p_tmp);
      llvm_values_insert(cnt, &tmp->base, p);
    }

    /*
     * Prepare return value.
     */
    assert(type_fn->ret_type);
    if (type_fn->ret_type->kind != AST_TYPE_VOID) {
      LLVMTypeRef llvm_ret_type = to_llvm_type(cnt, type_fn->ret_type);
      cnt->fn_ret_val           = LLVMBuildAlloca(cnt->llvm_builder, llvm_ret_type, gname("ret"));
    } else {
      cnt->fn_ret_val = NULL;
    }

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_entry_block);
  }

  /* Generate function body */
  assert(decl_fn->value->kind == AST_EXPR_LIT_FN);
  ir_expr(cnt, decl_fn->value);

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
}

void
ir_decl_field(Context *cnt, AstDeclEntity *decl_field)
{
  LLVMValueRef result = NULL;
  assert(((AstDecl *)decl_field)->type && ((AstDecl *)decl_field)->name);

  AstDecl *   base      = (AstDecl *)decl_field;
  LLVMTypeRef llvm_type = to_llvm_type(cnt, base->type);

  if (decl_field->in_gscope) {
    /* declaration in global scope */
    result            = global_get(cnt, decl_field);
    LLVMValueRef init = ir_expr(cnt, decl_field->value);
    assert(init);

    LLVMSetInitializer(result, init);
  } else {
    LLVMBasicBlockRef prev_block = LLVMGetInsertBlock(cnt->llvm_builder);
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);
    result =
        LLVMBuildAlloca(cnt->llvm_builder, llvm_type, gname(((AstDecl *)decl_field)->name->str));
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, prev_block);

    llvm_values_insert(cnt, &decl_field->base, result);

    if (!decl_field->value) return;

    LLVMValueRef init = ir_expr(cnt, decl_field->value);
    init              = ltor_if_needed(cnt, decl_field->value, init);

    LLVMBuildStore(cnt->llvm_builder, init, result);
  }
}

/* EXPRESSIONS */
LLVMValueRef
ir_expr(Context *cnt, AstExpr *expr)
{
  assert(expr && ((Ast *)expr)->kind == AST_EXPR);
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
  case AST_EXPR_CALL:
    return ir_expr_call(cnt, (AstExprCall *)expr);
  default:
    bl_abort("missing ir generation for %s", ast_get_name((Ast *)expr));
  }
  return NULL;
}

LLVMValueRef
ir_expr_ref(Context *cnt, AstExprRef *ref)
{
  assert(ref->ref);
  LLVMValueRef result = NULL;

  switch (ref->ref->kind) {
  case AST_DECL_ARG:
    result = llvm_values_get(cnt, ref->ref);
    break;

  case AST_DECL_ENTITY: {
    AstDeclEntity *entity = (AstDeclEntity *)ref->ref;
    switch (entity->kind) {
    case DECL_ENTITY_FIELD:
      result = llvm_values_get(cnt, ref->ref);
      break;
    case DECL_ENTITY_FN:
      result = fn_get(cnt, entity);
      break;
    default:
      bl_abort("bad declaration");
    }
    break;
  }

  case AST_DECL_MEMBER:
    bl_abort("unimplemented");
    break;

  case AST_DECL_VARIANT:
    bl_abort("unimplemented");
    break;

  case AST_DECL_BAD:
    bl_abort("bad declaration");
  }

  assert(result);
  return result;
}

LLVMValueRef
ir_expr_call(Context *cnt, AstExprCall *call)
{
  LLVMValueRef result  = NULL;
  LLVMValueRef llvm_fn = ir_expr(cnt, call->ref);

  assert(llvm_fn);

  const int     argc      = bo_array_size(call->args);
  LLVMValueRef *llvm_args = bl_malloc(sizeof(LLVMValueRef) * argc);
  if (!llvm_args) bl_abort("bad alloc");

  Ast *arg;
  barray_foreach(call->args, arg)
  {
    assert(arg->kind == AST_EXPR);
    llvm_args[i] = ir_expr(cnt, (AstExpr *)arg);
    assert(llvm_args[i] && "invalid call argument");

    llvm_args[i] = ltor_if_needed(cnt, (AstExpr *)arg, llvm_args[i]);
  }

  result = LLVMBuildCall(cnt->llvm_builder, llvm_fn, llvm_args, (unsigned int)argc, "");
  bl_free(llvm_args);
  return result;
}

LLVMValueRef
ir_expr_lit_fn(Context *cnt, AstExprLitFn *lit_fn)
{
  assert(lit_fn->block);
  ir_node(cnt, lit_fn->block);
  return NULL;
}

LLVMValueRef
ir_expr_lit_int(Context *cnt, AstExprLitInt *lit_int)
{
  LLVMValueRef result    = NULL;
  AstTypeInt * type      = (AstTypeInt *)lit_int->base.type;
  LLVMTypeRef  llvm_type = to_llvm_type(cnt, (AstType *)type);

  result = LLVMConstInt(llvm_type, lit_int->i, type->is_signed);
  assert(result);

  return result;
}

LLVMValueRef
ir_expr_unary(Context *cnt, AstExprUnary *unary)
{
  assert(unary->next);
  LLVMValueRef result = ir_expr(cnt, unary->next);

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
  LLVMValueRef llvm_lhs = ir_expr(cnt, binop->lhs);
  LLVMValueRef llvm_rhs = ir_expr(cnt, binop->rhs);
  assert(llvm_lhs && llvm_rhs);

  /* generate load if needed l/rvalue conversion is needed! */
  llvm_rhs                = ltor_if_needed(cnt, binop->rhs, llvm_rhs);
  LLVMTypeKind lhs_kind   = LLVMGetTypeKind(LLVMTypeOf(llvm_lhs));
  bool         float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

  /* Assignments */
  switch (binop->kind) {
  case BINOP_ASSIGN: {
    LLVMBuildStore(cnt->llvm_builder, llvm_rhs, llvm_lhs);
    return llvm_lhs;
  }

  case BINOP_ADD_ASSIGN: {
    LLVMValueRef value = ltor_if_needed(cnt, binop->lhs, llvm_lhs);
    if (float_kind)
      value = LLVMBuildFAdd(cnt->llvm_builder, value, llvm_rhs, gname("tmp"));
    else
      value = LLVMBuildAdd(cnt->llvm_builder, value, llvm_rhs, gname("tmp"));
    return LLVMBuildStore(cnt->llvm_builder, value, llvm_lhs);
  }

  case BINOP_SUB_ASSIGN: {
    LLVMValueRef value = ltor_if_needed(cnt, binop->lhs, llvm_lhs);
    if (float_kind)
      value = LLVMBuildFSub(cnt->llvm_builder, value, llvm_rhs, gname("tmp"));
    else
      value = LLVMBuildSub(cnt->llvm_builder, value, llvm_rhs, gname("tmp"));
    return LLVMBuildStore(cnt->llvm_builder, value, llvm_lhs);
  }

  case BINOP_MUL_ASSIGN: {
    LLVMValueRef value = ltor_if_needed(cnt, binop->lhs, llvm_lhs);
    if (float_kind)
      value = LLVMBuildFMul(cnt->llvm_builder, value, llvm_rhs, gname("tmp"));
    else
      value = LLVMBuildMul(cnt->llvm_builder, value, llvm_rhs, gname("tmp"));
    return LLVMBuildStore(cnt->llvm_builder, value, llvm_lhs);
  }

  case BINOP_DIV_ASSIGN: {
    LLVMValueRef value = ltor_if_needed(cnt, binop->lhs, llvm_lhs);
    if (float_kind)
      value = LLVMBuildFDiv(cnt->llvm_builder, value, llvm_rhs, gname("tmp"));
    else
      value = LLVMBuildSDiv(cnt->llvm_builder, value, llvm_rhs, gname("tmp"));
    return LLVMBuildStore(cnt->llvm_builder, value, llvm_lhs);
  }

  case BINOP_MOD_ASSIGN: {
    LLVMValueRef value = ltor_if_needed(cnt, binop->lhs, llvm_lhs);
    value              = LLVMBuildSRem(cnt->llvm_builder, value, llvm_rhs, gname("tmp"));
    return LLVMBuildStore(cnt->llvm_builder, value, llvm_lhs);
  }
  default:
    break;
  }

  /* generate load if needed l/rvalue conversion is needed! */
  llvm_lhs   = ltor_if_needed(cnt, binop->lhs, llvm_lhs);
  lhs_kind   = LLVMGetTypeKind(LLVMTypeOf(llvm_lhs));
  float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

  /* Other binary operations */
  switch (binop->kind) {
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
    break;
  }

  bl_abort("unknown binop");
}

bool
is_satisfied(Context *cnt, AstDeclEntity *decl, bool strict_only)
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
  BList *        queue = cnt->assembly->ir_queue;
  AstDeclEntity *entity;

  while (!bo_list_empty(queue)) {
    entity = bo_list_front(queue, AstDeclEntity *);
    bo_list_pop_front(queue);

    assert(entity->flags == 0 && "invalid flags");

    if (is_satisfied(cnt, entity, true)) {
      if (cnt->verbose) msg_log(LOG_TAG ": generate: '%s'", entity->base.name->str);

      /* prepare llvm module for currently generated declaration */
      cnt->llvm_module = LLVMModuleCreateWithNameInContext(entity->base.name->str, cnt->llvm_cnt);

      ir_node(cnt, (Ast *)entity);

#if BL_DEBUG
      if (cnt->verbose) print_llvm_module(cnt->llvm_module);
      validate(cnt->llvm_module);
#endif
      llvm_values_reset(cnt);
      bo_htbl_insert(cnt->llvm_modules, (uint64_t)entity, cnt->llvm_module);
    } else {
      /* declaration is waiting for it's dependencies and need to be processed later */
      bo_list_push_back(queue, entity);
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

  cnt.verbose      = (bool)(builder->flags & BUILDER_VERBOSE);
  cnt.builder      = builder;
  cnt.llvm_cnt     = LLVMContextCreate();
  cnt.llvm_builder = LLVMCreateBuilderInContext(cnt.llvm_cnt);
  cnt.assembly     = assembly;
  cnt.llvm_modules = bo_htbl_new(sizeof(LLVMModuleRef), bo_list_size(assembly->ir_queue));
  cnt.llvm_types   = bo_htbl_new(sizeof(LLVMTypeRef), EXPECTED_TYPE_COUNT);
  cnt.llvm_values  = bo_htbl_new(sizeof(LLVMValueRef), EXPECTED_VALUE_COUNT);

  generate(&cnt);

  /* link runtime (main as entry) */
  if (assembly->entry_node) {
    assembly->llvm_module = link(&cnt, assembly->entry_node);
    bo_htbl_erase_key(cnt.llvm_modules, (uint64_t)assembly->entry_node);

#if BL_DEBUG
    if (cnt.verbose) print_llvm_module(assembly->llvm_module);
#endif
  }

  /* cleanup */
  bo_unref(cnt.llvm_modules);
  bo_unref(cnt.llvm_types);
  bo_unref(cnt.llvm_values);
  LLVMDisposeBuilder(cnt.llvm_builder);
}
