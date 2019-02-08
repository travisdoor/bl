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

#define LLVM_TRAP_FN "llvm.debugtrap"
#if BL_DEBUG
#define NAMED_VARS true
#else
#define NAMED_VARS false
#endif

typedef struct
{
  Builder * builder;
  Assembly *assembly;

  LLVMContextRef    llvm_cnt;
  LLVMModuleRef     llvm_module;
  LLVMTargetDataRef llvm_td;
  LLVMBuilderRef    llvm_builder;

  LLVMValueRef llvm_trap_fn;
} Context;

static void
gen_instr(Context *cnt, MirInstr *instr);

static void
gen_instr_binop(Context *cnt, MirInstrBinop *binop);

static void
gen_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static void
gen_instr_cast(Context *cnt, MirInstrCast *cast);

static void
gen_instr_addrof(Context *cnt, MirInstrAddrOf *addrof);

static void
gen_instr_unop(Context *cnt, MirInstrUnop *unop);

static void
gen_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static void
gen_instr_store(Context *cnt, MirInstrStore *store);

static void
gen_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);

static void
gen_instr_block(Context *cnt, MirInstrBlock *block);

static void
gen_instr_br(Context *cnt, MirInstrBr *br);

static void
gen_instr_arg(Context *cnt, MirInstrArg *arg);

static void
gen_instr_cond_br(Context *cnt, MirInstrCondBr *br);

static void
gen_instr_ret(Context *cnt, MirInstrRet *ret);

static void
gen_instr_decl_var(Context *cnt, MirInstrDeclVar *decl);

static void
gen_instr_load(Context *cnt, MirInstrLoad *load);

static void
gen_instr_call(Context *cnt, MirInstrCall *call);

static void
gen_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static void
gen_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr);

static void
gen_as_const(Context *cnt, MirInstr *instr);

static void
gen_allocas(Context *cnt, MirFn *fn);

static inline LLVMValueRef
gen_fn_proto(Context *cnt, MirFn *fn)
{
  assert(fn);

  fn->llvm_value = LLVMGetNamedFunction(cnt->llvm_module, fn->llvm_name);
  if (!fn->llvm_value) {
    fn->llvm_value = LLVMAddFunction(cnt->llvm_module, fn->llvm_name, fn->type->llvm_type);
  }

  return fn->llvm_value;
}

static inline LLVMValueRef
fetch_value(Context *cnt, MirInstr *instr)
{
  LLVMValueRef value = NULL;
  if (instr->comptime && !instr->llvm_value) gen_as_const(cnt, instr);

  value = instr->llvm_value;
  assert(value);
  return value;
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
gen_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
  ScopeEntry *entry = ref->scope_entry;
  assert(entry);

  switch (entry->kind) {
  case SCOPE_ENTRY_VAR: {
    ref->base.llvm_value = entry->data.var->llvm_value;
    break;
  }
  case SCOPE_ENTRY_FN: {
    ref->base.llvm_value = entry->data.fn->llvm_value;
    break;
  }
  default:
    bl_unimplemented;
  }

  assert(ref->base.llvm_value);
}

void
gen_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
  unr->base.llvm_value = LLVMBuildCall(cnt->llvm_builder, cnt->llvm_trap_fn, NULL, 0, "");
}

void
gen_instr_cast(Context *cnt, MirInstrCast *cast)
{
  LLVMValueRef llvm_src       = cast->next->llvm_value;
  LLVMTypeRef  llvm_dest_type = cast->base.const_value.type->llvm_type;
  LLVMOpcode   llvm_op;
  assert(llvm_src && llvm_dest_type);

  switch (cast->op) {
  case MIR_CAST_BITCAST:
    llvm_op = LLVMBitCast;
    break;

  case MIR_CAST_SEXT:
    llvm_op = LLVMSExt;
    break;

  case MIR_CAST_ZEXT:
    llvm_op = LLVMZExt;
    break;

  case MIR_CAST_TRUNC:
    llvm_op = LLVMTrunc;
    break;

  case MIR_CAST_FPTOSI:
    llvm_op = LLVMFPToSI;
    break;

  case MIR_CAST_FPTOUI:
    llvm_op = LLVMFPToUI;
    break;

  case MIR_CAST_PTRTOINT:
    llvm_op = LLVMPtrToInt;
    break;

  case MIR_CAST_INTTOPTR:
    llvm_op = LLVMIntToPtr;
    break;

  default:
    bl_abort("invalid cast type");
  }

  cast->base.llvm_value = LLVMBuildCast(cnt->llvm_builder, llvm_op, llvm_src, llvm_dest_type, "");
}

void
gen_instr_addrof(Context *cnt, MirInstrAddrOf *addrof)
{
  addrof->base.llvm_value = addrof->src->llvm_value;
  assert(addrof->base.llvm_value);
}

void
gen_instr_arg(Context *cnt, MirInstrArg *arg)
{
  MirFn *fn = arg->base.owner_block->owner_fn;
  assert(fn);
  LLVMValueRef llvm_fn = fn->llvm_value;
  assert(llvm_fn);

  arg->base.llvm_value = LLVMGetParam(llvm_fn, arg->i);
}

void
gen_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
  LLVMValueRef llvm_arr_ptr = fetch_value(cnt, elem_ptr->arr_ptr);
  LLVMValueRef llvm_index   = fetch_value(cnt, elem_ptr->index);
  assert(llvm_arr_ptr && llvm_index);

  LLVMValueRef llvm_indices[2];
  llvm_indices[0] = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, false);
  llvm_indices[1] = llvm_index;

  elem_ptr->base.llvm_value =
      LLVMBuildGEP(cnt->llvm_builder, llvm_arr_ptr, llvm_indices, ARRAY_SIZE(llvm_indices), "");
}

void
gen_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
  assert(member_ptr->scope_entry->kind == SCOPE_ENTRY_MEMBER);
  MirMember *member = member_ptr->scope_entry->data.member;
  assert(member);

  LLVMValueRef       llvm_target_ptr = fetch_value(cnt, member_ptr->target_ptr);
  const unsigned int index = (const unsigned int)member_ptr->scope_entry->data.member->index;
  assert(llvm_target_ptr);

  member_ptr->base.llvm_value = LLVMBuildStructGEP(cnt->llvm_builder, llvm_target_ptr, index, "");
  assert(member_ptr->base.llvm_value);
}

void
gen_instr_load(Context *cnt, MirInstrLoad *load)
{
  assert(load->base.const_value.type && "invalid type of load instruction");
  LLVMValueRef   llvm_src  = fetch_value(cnt, load->src);
  const unsigned alignment = load->base.const_value.type->alignment;
  assert(llvm_src);
  load->base.llvm_value = LLVMBuildLoad(cnt->llvm_builder, llvm_src, "");
  LLVMSetAlignment(load->base.llvm_value, alignment);
}

void
gen_as_const(Context *cnt, MirInstr *instr)
{
  assert(instr->comptime && "only compile time known instructions can act like a constants");
  MirConstValue *value = &instr->const_value;
  MirType *      type  = value->type;
  assert(type);
  LLVMTypeRef llvm_type = type->llvm_type;
  assert(llvm_type);

  switch (type->kind) {
  case MIR_TYPE_INT: {
    instr->llvm_value = LLVMConstInt(llvm_type, value->data.v_uint, type->data.integer.is_signed);
    break;
  }

  case MIR_TYPE_REAL: {
    const size_t size = type->store_size_bytes;

    if (size == sizeof(float)) { // float
      instr->llvm_value = LLVMConstReal(llvm_type, value->data.v_float);
    } else if (size == sizeof(double)) { // double
      instr->llvm_value = LLVMConstReal(llvm_type, value->data.v_double);
    } else {
      bl_abort("invalid floating point type");
    }
    break;
  }

  case MIR_TYPE_BOOL:
    instr->llvm_value = LLVMConstInt(llvm_type, value->data.v_uint, false);
    break;

  case MIR_TYPE_NULL:
    assert(value->data.v_void_ptr == NULL);
    instr->llvm_value = LLVMConstNull(llvm_type);
    break;

  case MIR_TYPE_PTR:
    bl_abort("invalid constant type");
    break;

  case MIR_TYPE_ARRAY: {
    const size_t len = type->data.array.len;
    assert(len && "zero sized array");

    switch (value->kind) {
    case MIR_CV_STRING: {
      assert(value->data.v_str);
      instr->llvm_value = LLVMConstStringInContext(cnt->llvm_cnt, value->data.v_str, len, true);
      break;
    }

    default:
      bl_unimplemented;
    }
    break;
  }
  default:
    bl_unimplemented;
  }
}

void
gen_instr_store(Context *cnt, MirInstrStore *store)
{
  LLVMValueRef   val       = fetch_value(cnt, store->src);
  LLVMValueRef   ptr       = fetch_value(cnt, store->dest);
  const unsigned alignment = store->src->const_value.type->alignment;
  assert(val && ptr);
  store->base.llvm_value = LLVMBuildStore(cnt->llvm_builder, val, ptr);
  LLVMSetAlignment(store->base.llvm_value, alignment);
}

void
gen_instr_unop(Context *cnt, MirInstrUnop *unop)
{
  LLVMValueRef llvm_val = fetch_value(cnt, unop->instr);
  assert(llvm_val);

  switch (unop->op) {
  case UNOP_NOT: {
    unop->base.llvm_value = LLVMBuildNot(cnt->llvm_builder, llvm_val, "");
    break;
  }

  case UNOP_NEG: {
    unop->base.llvm_value = LLVMBuildNeg(cnt->llvm_builder, llvm_val, "");
    break;
  }

  case UNOP_POS: {
    unop->base.llvm_value = llvm_val;
    break;
  }

  default:
    bl_unimplemented;
  }
}

void
gen_instr_binop(Context *cnt, MirInstrBinop *binop)
{
  LLVMValueRef lhs = fetch_value(cnt, binop->lhs);
  LLVMValueRef rhs = fetch_value(cnt, binop->rhs);
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
  assert(callee->const_value.type && callee->const_value.type->kind == MIR_TYPE_FN);

  LLVMValueRef  llvm_fn   = gen_fn_proto(cnt, callee->const_value.data.v_fn);
  const size_t  llvm_argc = call->args ? bo_array_size(call->args) : 0;
  LLVMValueRef *llvm_args = NULL;

  if (llvm_argc) {
    llvm_args = bl_malloc(sizeof(LLVMValueRef) * llvm_argc);
    if (!llvm_args) bl_abort("bad alloc");

    MirInstr *arg;

    barray_foreach(call->args, arg)
    {
      llvm_args[i] = fetch_value(cnt, arg);
    }
  }

  assert(llvm_fn);
  call->base.llvm_value =
      LLVMBuildCall(cnt->llvm_builder, llvm_fn, llvm_args, (unsigned int)llvm_argc, "");
  bl_free(llvm_args);
}

void
gen_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
  MirVar *var = decl->var;
  if (!var->gen_llvm) return;
  assert(var);
  assert(var->llvm_value);

  if (decl->init) {
    LLVMValueRef llvm_init = fetch_value(cnt, decl->init);
    assert(llvm_init);
    LLVMBuildStore(cnt->llvm_builder, llvm_init, var->llvm_value);
  }
}

void
gen_instr_ret(Context *cnt, MirInstrRet *ret)
{
  LLVMValueRef llvm_ret;
  if (ret->value) {
    LLVMValueRef llvm_ret_value = fetch_value(cnt, ret->value);
    assert(llvm_ret_value);
    llvm_ret = LLVMBuildRet(cnt->llvm_builder, llvm_ret_value);
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
  MirInstr *     cond       = br->cond;
  MirInstrBlock *then_block = br->then_block;
  MirInstrBlock *else_block = br->else_block;
  assert(cond && then_block);

  LLVMValueRef      llvm_cond       = fetch_value(cnt, cond);
  LLVMBasicBlockRef llvm_then_block = gen_basic_block(cnt, then_block);
  LLVMBasicBlockRef llvm_else_block = gen_basic_block(cnt, else_block);

  br->base.llvm_value =
      LLVMBuildCondBr(cnt->llvm_builder, llvm_cond, llvm_then_block, llvm_else_block);
}

void
gen_instr_block(Context *cnt, MirInstrBlock *block)
{
  MirFn *fn = block->owner_fn;
  assert(fn->llvm_value);
  LLVMBasicBlockRef llvm_block = gen_basic_block(cnt, block);
  assert(llvm_block);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_block);

  /* gen allocas fist in entry block!!! */
  if (fn->first_block == block) {
    gen_allocas(cnt, fn);
  }

  MirInstr *instr = block->entry_instr;
  while (instr) {
    gen_instr(cnt, instr);
    instr = instr->next;
  }
}

void
gen_allocas(Context *cnt, MirFn *fn)
{
  assert(fn);

  // LLVMBasicBlockRef bb = LLVMGetEntryBasicBlock(fn->llvm_value);
  // assert(bb && "invalid insert block for allocas!!!");
  // LLVMPositionBuilderAtEnd(cnt->llvm_builder, bb);

  const char *var_name;
  LLVMTypeRef var_type;
  unsigned    var_alignment;
  MirVar *    var;

  barray_foreach(fn->variables, var)
  {
    assert(var);
#if NAMED_VARS
    var_name = var->id->str;
#else
    var_name = "";
#endif

    var_type      = var->alloc_type->llvm_type;
    var_alignment = (unsigned int)var->alloc_type->alignment;
    assert(var_type);

    var->llvm_value = LLVMBuildAlloca(cnt->llvm_builder, var_type, var_name);
    LLVMSetAlignment(var->llvm_value, var_alignment);
  }
}

void
gen_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
  MirFn *fn = fn_proto->base.const_value.data.v_fn;
  /* unused function */
  if (fn->ref_count == 0) return;
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
  case MIR_INSTR_DECL_REF:
    gen_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
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
  case MIR_INSTR_ARG:
    gen_instr_arg(cnt, (MirInstrArg *)instr);
    break;
  case MIR_INSTR_UNOP:
    gen_instr_unop(cnt, (MirInstrUnop *)instr);
    break;
  case MIR_INSTR_UNREACHABLE:
    gen_instr_unreachable(cnt, (MirInstrUnreachable *)instr);
    break;
  case MIR_INSTR_MEMBER_PTR:
    gen_instr_member_ptr(cnt, (MirInstrMemberPtr *)instr);
    break;
  case MIR_INSTR_ELEM_PTR:
    gen_instr_elem_ptr(cnt, (MirInstrElemPtr *)instr);
    break;
  case MIR_INSTR_ADDROF:
    gen_instr_addrof(cnt, (MirInstrAddrOf *)instr);
    break;
  case MIR_INSTR_CAST:
    gen_instr_cast(cnt, (MirInstrCast *)instr);
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

  {
    LLVMTypeRef llvm_void_type    = LLVMVoidTypeInContext(cnt.llvm_cnt);
    LLVMTypeRef llvm_trap_fn_type = LLVMFunctionType(llvm_void_type, NULL, 0, false);
    cnt.llvm_trap_fn = LLVMAddFunction(cnt.llvm_module, LLVM_TRAP_FN, llvm_trap_fn_type);
  }

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
