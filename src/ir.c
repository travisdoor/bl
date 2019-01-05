//************************************************************************************************
// bl
//
// File:   ir.c
// Author: Martin Dorazil
// Date:   12/9/18
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
#include "mir.h"
#include "unit.h"
#include "common.h"
#include "builder.h"
#include "assembly.h"

typedef struct
{
  Builder * builder;
  Assembly *assembly;

  LLVMContextRef    llvm_cnt;
  LLVMModuleRef     llvm_module;
  LLVMTargetDataRef llvm_td;
  LLVMBuilderRef    llvm_builder;
} Context;

static void
gen_instr(Context *cnt, MirInstr *instr);

static void
gen_instr_binop(Context *cnt, MirInstrBinop *binop);

static void
gen_instr_store(Context *cnt, MirInstrStore *store);

static void
gen_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);

static void
gen_instr_block(Context *cnt, MirInstrBlock *block);

static void
gen_instr_br(Context *cnt, MirInstrBr *br);

static void
gen_instr_cond_br(Context *cnt, MirInstrCondBr *br);

static void
gen_instr_ret(Context *cnt, MirInstrRet *ret);

static void
gen_instr_decl_var(Context *cnt, MirInstrDeclVar *decl);

static void
gen_instr_const(Context *cnt, MirInstrConst *cnst);

static void
gen_instr_load(Context *cnt, MirInstrLoad *load);

static void
gen_instr_call(Context *cnt, MirInstrCall *call);

static void
gen_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static inline void
gen_fn_proto(Context *cnt, MirFn *fn)
{
  assert(fn);

  fn->llvm_value = LLVMGetNamedFunction(cnt->llvm_module, fn->name);
  if (!fn->llvm_value) {
    fn->llvm_value = LLVMAddFunction(cnt->llvm_module, fn->name, fn->type->llvm_type);
  }
}

static inline LLVMBasicBlockRef
gen_basic_block(Context *cnt, MirInstrBlock *block)
{
  if (!block) return NULL;
  LLVMBasicBlockRef llvm_block = NULL;
  if (!block->base.llvm_value) {
    llvm_block =
        LLVMAppendBasicBlockInContext(cnt->llvm_cnt, block->owner_fn->llvm_value, block->name);
    block->base.llvm_value = LLVMBasicBlockAsValue(llvm_block);
  } else {
    llvm_block = LLVMValueAsBasicBlock(block->base.llvm_value);
  }

  return llvm_block;
}

void
gen_instr_load(Context *cnt, MirInstrLoad *load)
{
  LLVMValueRef llvm_src = load->src->llvm_value;
  assert(llvm_src);
  load->base.llvm_value = LLVMBuildLoad(cnt->llvm_builder, llvm_src, "");
}

void
gen_instr_const(Context *cnt, MirInstrConst *cnst)
{
  MirValue *value = &cnst->base.value;
  MirType * type  = value->type;
  assert(type);
  LLVMTypeRef llvm_type = type->llvm_type;
  assert(llvm_type);

  switch (type->kind) {
  case MIR_TYPE_INT:
    cnst->base.llvm_value =
        LLVMConstInt(llvm_type, value->data.v_uint, type->data.integer.is_signed);
    break;
  case MIR_TYPE_BOOL:
    cnst->base.llvm_value = LLVMConstInt(llvm_type, value->data.v_uint, false);
    break;
  default:
    bl_unimplemented;
  }
}

void
gen_instr_store(Context *cnt, MirInstrStore *store)
{
  LLVMValueRef val = store->src->llvm_value;
  LLVMValueRef ptr = store->dest->llvm_value;
  assert(val && ptr);
  store->base.llvm_value = LLVMBuildStore(cnt->llvm_builder, val, ptr);
}

void
gen_instr_binop(Context *cnt, MirInstrBinop *binop)
{
  LLVMValueRef lhs = binop->lhs->llvm_value;
  LLVMValueRef rhs = binop->rhs->llvm_value;
  assert(lhs && rhs);

  LLVMTypeKind lhs_kind   = LLVMGetTypeKind(LLVMTypeOf(lhs));
  const bool   float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

  switch (binop->op) {
  case BINOP_ADD:
    if (float_kind)
      binop->base.llvm_value = LLVMBuildFAdd(cnt->llvm_builder, lhs, rhs, "");
    else
      binop->base.llvm_value = LLVMBuildAdd(cnt->llvm_builder, lhs, rhs, "");
    break;

  case BINOP_SUB:
    if (float_kind)
      binop->base.llvm_value = LLVMBuildFSub(cnt->llvm_builder, lhs, rhs, "");
    else
      binop->base.llvm_value = LLVMBuildSub(cnt->llvm_builder, lhs, rhs, "");
    break;

  case BINOP_MUL:
    if (float_kind)
      binop->base.llvm_value = LLVMBuildFMul(cnt->llvm_builder, lhs, rhs, "");
    else
      binop->base.llvm_value = LLVMBuildMul(cnt->llvm_builder, lhs, rhs, "");
    break;

  case BINOP_DIV:
    if (float_kind)
      binop->base.llvm_value = LLVMBuildFDiv(cnt->llvm_builder, lhs, rhs, "");
    else
      binop->base.llvm_value = LLVMBuildSDiv(cnt->llvm_builder, lhs, rhs, "");
    break;

  case BINOP_MOD:
    binop->base.llvm_value = LLVMBuildSRem(cnt->llvm_builder, lhs, rhs, "");
    break;

  case BINOP_EQ:
    if (float_kind)
      binop->base.llvm_value = LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOEQ, lhs, rhs, "");
    else
      binop->base.llvm_value = LLVMBuildICmp(cnt->llvm_builder, LLVMIntEQ, lhs, rhs, "");
    break;

  case BINOP_NEQ:
    if (float_kind)
      binop->base.llvm_value = LLVMBuildFCmp(cnt->llvm_builder, LLVMRealONE, lhs, rhs, "");
    else
      binop->base.llvm_value = LLVMBuildICmp(cnt->llvm_builder, LLVMIntNE, lhs, rhs, "");
    break;

  case BINOP_GREATER:
    if (float_kind)
      binop->base.llvm_value = LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGT, lhs, rhs, "");
    else
      binop->base.llvm_value = LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGT, lhs, rhs, "");
    break;

  case BINOP_LESS:
    if (float_kind)
      binop->base.llvm_value = LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLT, lhs, rhs, "");
    else
      binop->base.llvm_value = LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLT, lhs, rhs, "");
    break;

  case BINOP_GREATER_EQ:
    if (float_kind)
      binop->base.llvm_value = LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGE, lhs, rhs, "");
    else
      binop->base.llvm_value = LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGE, lhs, rhs, "");
    break;

  case BINOP_LESS_EQ:
    if (float_kind)
      binop->base.llvm_value = LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLE, lhs, rhs, "");
    else
      binop->base.llvm_value = LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLE, lhs, rhs, "");
    break;

  case BINOP_LOGIC_AND:
    binop->base.llvm_value = LLVMBuildAnd(cnt->llvm_builder, lhs, rhs, "");
    break;

  case BINOP_LOGIC_OR:
    binop->base.llvm_value = LLVMBuildOr(cnt->llvm_builder, lhs, rhs, "");
    break;

  default:
    bl_unimplemented;
  }
}

void
gen_instr_call(Context *cnt, MirInstrCall *call)
{
  MirInstr *callee = call->callee;
  assert(callee);
  assert(callee->value.type && callee->value.type->kind == MIR_TYPE_FN);

  MirFn *fn = callee->value.data.v_fn;
  assert(fn);

  LLVMValueRef  llvm_fn   = fn->llvm_value;
  const size_t  llvm_argc = call->args ? bo_array_size(call->args) : 0;
  LLVMValueRef *llvm_args = NULL;

  if (llvm_argc) {
    llvm_args = bl_malloc(sizeof(LLVMValueRef) * llvm_argc);
    if (!llvm_args) bl_abort("bad alloc");

    MirInstr *arg;

    barray_foreach(call->args, arg)
    {
      assert(arg->llvm_value && "function argument has no LLVM value");
      llvm_args[i] = arg->llvm_value;
    }
  }

  assert(llvm_fn);
  call->base.llvm_value =
      LLVMBuildCall(cnt->llvm_builder, llvm_fn, llvm_args, (unsigned int)llvm_argc, "");
  bl_free(llvm_args);
}

void
gen_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
  const bool is_fn = ref->base.value.type->kind == MIR_TYPE_FN;

  if (is_fn) {
    /* generate or get function prototype */
    MirFn *fn = ref->base.value.data.v_fn;
    gen_fn_proto(cnt, fn);
  }
}

void
gen_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
  MirVar *var = decl->var;
  assert(var);

  const char *name      = var->name;
  LLVMTypeRef llvm_type = var->value.type->llvm_type;
  assert(llvm_type && name);

  decl->base.llvm_value = LLVMBuildAlloca(cnt->llvm_builder, llvm_type, name);
}

void
gen_instr_ret(Context *cnt, MirInstrRet *ret)
{
  LLVMValueRef llvm_ret;
  if (ret->value) {
    bl_unimplemented;
  } else {
    llvm_ret = LLVMBuildRetVoid(cnt->llvm_builder);
  }

  ret->base.llvm_value = llvm_ret;
}

void
gen_instr_br(Context *cnt, MirInstrBr *br)
{
  MirInstrBlock *then_block = br->then_block;
  assert(then_block);

  LLVMBasicBlockRef llvm_then_block = gen_basic_block(cnt, then_block);
  assert(llvm_then_block);
  br->base.llvm_value = LLVMBuildBr(cnt->llvm_builder, llvm_then_block);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_then_block);
}

void
gen_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
  MirInstr *cond       = br->cond;
  MirInstrBlock *then_block = br->then_block;
  MirInstrBlock *else_block = br->else_block;
  assert(cond && then_block);

  LLVMValueRef llvm_cond       = cond->llvm_value;
  LLVMBasicBlockRef llvm_then_block = gen_basic_block(cnt, then_block);
  LLVMBasicBlockRef llvm_else_block = gen_basic_block(cnt, else_block);

  br->base.llvm_value =
      LLVMBuildCondBr(cnt->llvm_builder, llvm_cond, llvm_then_block, llvm_else_block);
}

void
gen_instr_block(Context *cnt, MirInstrBlock *block)
{
  assert(block->owner_fn->llvm_value);
  LLVMBasicBlockRef llvm_block = gen_basic_block(cnt, block);
  assert(llvm_block);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_block);
  MirInstr *instr = block->entry_instr;
  while (instr) {
    gen_instr(cnt, instr);
    instr = instr->next;
  }
}

void
gen_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
  MirFn *fn = fn_proto->base.value.data.v_fn;
  gen_fn_proto(cnt, fn);

  if (!fn->is_external) {
    MirInstr *block = (MirInstr *)fn->first_block;

    while (block) {
      gen_instr(cnt, block);
      block = block->next;
    }
  }
}

void
gen_instr(Context *cnt, MirInstr *instr)
{
  if (!instr || !instr->ref_count) return;

  switch (instr->kind) {
  case MIR_INSTR_BINOP:
    gen_instr_binop(cnt, (MirInstrBinop *)instr);
    break;
  case MIR_INSTR_FN_PROTO:
    gen_instr_fn_proto(cnt, (MirInstrFnProto *)instr);
    break;
  case MIR_INSTR_BLOCK:
    gen_instr_block(cnt, (MirInstrBlock *)instr);
    break;
  case MIR_INSTR_BR:
    gen_instr_br(cnt, (MirInstrBr *)instr);
    break;
  case MIR_INSTR_COND_BR:
    gen_instr_cond_br(cnt, (MirInstrCondBr *)instr);
    break;
  case MIR_INSTR_RET:
    gen_instr_ret(cnt, (MirInstrRet *)instr);
    break;
  case MIR_INSTR_DECL_VAR:
    gen_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
    break;
  case MIR_INSTR_CONST:
    gen_instr_const(cnt, (MirInstrConst *)instr);
    break;
  case MIR_INSTR_LOAD:
    gen_instr_load(cnt, (MirInstrLoad *)instr);
    break;
  case MIR_INSTR_STORE:
    gen_instr_store(cnt, (MirInstrStore *)instr);
    break;
  case MIR_INSTR_CALL:
    gen_instr_call(cnt, (MirInstrCall *)instr);
    break;
  case MIR_INSTR_DECL_REF:
    gen_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
    break;

  default:
    bl_warning("unimplemented LLVM generation for %s", mir_instr_name(instr));
    break;
  }
}

/* public */
void
ir_run(Builder *builder, Assembly *assembly)
{
  Context cnt;
  memset(&cnt, 0, sizeof(Context));
  cnt.builder      = builder;
  cnt.assembly     = assembly;
  cnt.llvm_cnt     = assembly->mir_module->llvm_cnt;
  cnt.llvm_module  = assembly->mir_module->llvm_module;
  cnt.llvm_td      = assembly->mir_module->llvm_td;
  cnt.llvm_builder = LLVMCreateBuilderInContext(assembly->mir_module->llvm_cnt);

  MirInstr *ginstr;
  barray_foreach(assembly->mir_module->globals, ginstr)
  {
    gen_instr(&cnt, ginstr);
  }

#if BL_DEBUG
  char *error = NULL;
  if (LLVMVerifyModule(cnt.llvm_module, LLVMReturnStatusAction, &error)) {
    msg_error("LLVM module not verified with error: %s", error);
  }
  LLVMDisposeMessage(error);
#endif

  LLVMDisposeBuilder(cnt.llvm_builder);
}
