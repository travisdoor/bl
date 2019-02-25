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
#define LLVM_MEMSET_FN "llvm.memset.p0i8.i64"
#define LLVM_MEMCPY_FN "llvm.memcpy.p0i8.p0i8.i64"

#if BL_DEBUG
#define NAMED_VARS true
#else
#define NAMED_VARS false
#endif

#if NAMED_VARS
#define get_name(str) str
#else
#define get_name(str) ""
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
  LLVMValueRef llvm_memset_fn;
  LLVMValueRef llvm_memcpy_fn;
  LLVMValueRef llvm_const_i64;

  LLVMTypeRef llvm_void_type;
  LLVMTypeRef llvm_i1_type;
  LLVMTypeRef llvm_i8_type;
  LLVMTypeRef llvm_i32_type;
  LLVMTypeRef llvm_i64_type;
  LLVMTypeRef llvm_i8_ptr_type;
} Context;

static inline LLVMValueRef
create_trap_fn(Context *cnt)
{
  LLVMTypeRef llvm_fn_type = LLVMFunctionType(cnt->llvm_void_type, NULL, 0, false);
  return LLVMAddFunction(cnt->llvm_module, LLVM_TRAP_FN, llvm_fn_type);
}

static inline LLVMValueRef
create_memset_fn(Context *cnt)
{
  LLVMTypeRef *llvm_args = bl_malloc(sizeof(LLVMTypeRef) * 4);
  llvm_args[0]           = cnt->llvm_i8_ptr_type; // dest
  llvm_args[1]           = cnt->llvm_i8_type;     // value
  llvm_args[2]           = cnt->llvm_i64_type;    // size
  llvm_args[3]           = cnt->llvm_i1_type;     // volatile

  LLVMTypeRef  llvm_fn_type = LLVMFunctionType(cnt->llvm_void_type, llvm_args, 4, false);
  LLVMValueRef llvm_fn      = LLVMAddFunction(cnt->llvm_module, LLVM_MEMSET_FN, llvm_fn_type);
  bl_free(llvm_args);
  return llvm_fn;
}

static inline LLVMValueRef
create_memcpy_fn(Context *cnt)
{
  LLVMTypeRef llvm_args[4];
  llvm_args[0] = cnt->llvm_i8_ptr_type; // dest
  llvm_args[1] = cnt->llvm_i8_ptr_type; // src
  llvm_args[2] = cnt->llvm_i64_type;    // size
  llvm_args[3] = cnt->llvm_i1_type;     // volatile

  LLVMTypeRef  llvm_fn_type = LLVMFunctionType(cnt->llvm_void_type, llvm_args, 4, false);
  LLVMValueRef llvm_fn      = LLVMAddFunction(cnt->llvm_module, LLVM_MEMCPY_FN, llvm_fn_type);
  return llvm_fn;
}

static inline LLVMValueRef
build_call_memset_0(Context *cnt, LLVMValueRef llvm_dest_ptr, LLVMValueRef llvm_size)
{
  LLVMValueRef llvm_args[4];
  llvm_args[0] = LLVMBuildBitCast(cnt->llvm_builder, llvm_dest_ptr, cnt->llvm_i8_ptr_type, "");
  llvm_args[1] = LLVMConstInt(cnt->llvm_i8_type, 0, false);
  llvm_args[2] = llvm_size;
  llvm_args[3] = LLVMConstInt(cnt->llvm_i1_type, 0, false);

  LLVMValueRef llvm_result =
      LLVMBuildCall(cnt->llvm_builder, cnt->llvm_memset_fn, llvm_args, 4, "");

  return llvm_result;
}

static inline LLVMValueRef
build_call_memcpy(Context *cnt, LLVMValueRef llvm_dest_ptr, LLVMValueRef llvm_src_ptr,
                  LLVMValueRef llvm_size)
{
  LLVMValueRef llvm_args[4];
  llvm_args[0] = LLVMBuildBitCast(cnt->llvm_builder, llvm_dest_ptr, cnt->llvm_i8_ptr_type, "");
  llvm_args[1] = LLVMBuildBitCast(cnt->llvm_builder, llvm_src_ptr, cnt->llvm_i8_ptr_type, "");
  llvm_args[2] = llvm_size;
  llvm_args[3] = LLVMConstInt(cnt->llvm_i1_type, 0, false);

  LLVMValueRef llvm_result =
      LLVMBuildCall(cnt->llvm_builder, cnt->llvm_memcpy_fn, llvm_args, 4, "");

  return llvm_result;
}

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

static LLVMValueRef
gen_as_const(Context *cnt, MirConstValue *value);

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
gen_global_var_proto(Context *cnt, MirVar *var)
{
  assert(var);
  if (var->llvm_value) return var->llvm_value;

  LLVMTypeRef llvm_type = var->alloc_type->llvm_type;
  var->llvm_value       = LLVMAddGlobal(cnt->llvm_module, llvm_type, var->llvm_name);

  LLVMSetGlobalConstant(var->llvm_value, !var->is_mutable);

  /* Linkage should be later set by user. */
  LLVMSetLinkage(var->llvm_value, LLVMInternalLinkage);
  return var->llvm_value;
}

static inline LLVMValueRef
fetch_value(Context *cnt, MirInstr *instr)
{
  LLVMValueRef value = NULL;
  if (instr->comptime && !instr->llvm_value) {
    instr->llvm_value = gen_as_const(cnt, &instr->const_value);
  }

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
    MirVar *var = entry->data.var;
    if (var->is_in_gscope)
      ref->base.llvm_value = gen_global_var_proto(cnt, var);
    else
      ref->base.llvm_value = var->llvm_value;
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

  if (elem_ptr->target_is_slice) {
    /* special case for slices */
    llvm_arr_ptr = LLVMBuildStructGEP(cnt->llvm_builder, llvm_arr_ptr, 1, "");
    llvm_arr_ptr = LLVMBuildLoad(cnt->llvm_builder, llvm_arr_ptr, "");
    assert(llvm_arr_ptr);

    LLVMValueRef llvm_indices[1];
    llvm_indices[0] = llvm_index;

    elem_ptr->base.llvm_value = LLVMBuildInBoundsGEP(cnt->llvm_builder, llvm_arr_ptr, llvm_indices,
                                                     ARRAY_SIZE(llvm_indices), "");

    return;
  }

  LLVMValueRef llvm_indices[2];
  llvm_indices[0] = cnt->llvm_const_i64;
  llvm_indices[1] = llvm_index;

  elem_ptr->base.llvm_value =
      LLVMBuildGEP(cnt->llvm_builder, llvm_arr_ptr, llvm_indices, ARRAY_SIZE(llvm_indices), "");
}

void
gen_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
  LLVMValueRef llvm_target_ptr = fetch_value(cnt, member_ptr->target_ptr);
  assert(llvm_target_ptr);

  if (member_ptr->builtin_id == MIR_BUILTIN_NONE) {
    assert(member_ptr->scope_entry->kind == SCOPE_ENTRY_MEMBER);
    MirMember *member = member_ptr->scope_entry->data.member;
    assert(member);

    const unsigned int index = (const unsigned int)member_ptr->scope_entry->data.member->index;

    member_ptr->base.llvm_value = LLVMBuildStructGEP(cnt->llvm_builder, llvm_target_ptr, index, "");
    assert(member_ptr->base.llvm_value);
  } else {
    /* builtin member */

    MirType *target_type = mir_deref_type(member_ptr->target_ptr->const_value.type);

    /* Valid only for slice types, we generate direct replacement for arrays. */
    assert(mir_is_slice_type(target_type));

    if (member_ptr->builtin_id == MIR_BUILTIN_ARR_LEN) {
      /* .len */
      member_ptr->base.llvm_value = LLVMBuildStructGEP(cnt->llvm_builder, llvm_target_ptr, 0, "");
    } else if (member_ptr->builtin_id == MIR_BUILTIN_ARR_PTR) {
      /* .ptr*/
      member_ptr->base.llvm_value = LLVMBuildStructGEP(cnt->llvm_builder, llvm_target_ptr, 1, "");
    }
  }
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

LLVMValueRef
gen_as_const(Context *cnt, MirConstValue *value)
{
  MirType *type = value->type;
  assert(type);
  LLVMTypeRef llvm_type = type->llvm_type;
  assert(llvm_type);

  switch (type->kind) {
  case MIR_TYPE_INT: {
    return LLVMConstInt(llvm_type, value->data.v_u64, type->data.integer.is_signed);
  }

  case MIR_TYPE_REAL: {
    const size_t size = type->store_size_bytes;

    if (size == sizeof(float)) { // float
      return LLVMConstReal(llvm_type, value->data.v_f32);
    } else if (size == sizeof(double)) { // double
      return LLVMConstReal(llvm_type, value->data.v_f64);
    }
    bl_abort("invalid floating point type");
  }

  case MIR_TYPE_BOOL:
    return LLVMConstInt(llvm_type, value->data.v_s32, false);

  case MIR_TYPE_NULL:
    assert(value->data.v_void_ptr == NULL);
    return LLVMConstNull(llvm_type);

  case MIR_TYPE_PTR:
    bl_abort("invalid constant type");

  case MIR_TYPE_ARRAY: {
    const size_t len            = type->data.array.len;
    LLVMTypeRef  llvm_elem_type = type->data.array.elem_type->llvm_type;
    assert(len && llvm_elem_type);

    BArray *       elems = value->data.v_array.elems;
    MirConstValue *elem;

    assert(elems);
    assert(len == bo_array_size(elems));
    LLVMValueRef *llvm_elems = bl_malloc(sizeof(LLVMValueRef) * len);

    for (size_t i = 0; i < len; ++i) {
      elem          = bo_array_at(elems, i, MirConstValue *);
      llvm_elems[i] = gen_as_const(cnt, elem);
    }

    LLVMValueRef result = LLVMConstArray(llvm_elem_type, llvm_elems, len);
    bl_free(llvm_elems);
    return result;
  }

  case MIR_TYPE_STRUCT: {
    LLVMValueRef result = NULL;
    if (mir_is_slice_type(type)) {
      /* TODO: We generate representation only constant string slices, this need to be improved
       * later. */
      /* TODO: We generate representation only constant string slices, this need to be improved
       * later. */
      /* TODO: We generate representation only constant string slices, this need to be improved
       * later. */
      /* TODO: We generate representation only constant string slices, this need to be improved
       * later. */
      BArray *members = value->data.v_struct.members;
      assert(members);
      assert(bo_array_size(members) == 2 && "not slice string?");

      MirConstValue *len_value = bo_array_at(members, 0, MirConstValue *);
      MirConstValue *str_value = bo_array_at(members, 1, MirConstValue *);
      assert(len_value && str_value);

      const uint64_t len = len_value->data.v_u64;
      const char *   str = str_value->data.v_str;
      assert(str);

      LLVMValueRef *const_vals = bl_malloc(sizeof(LLVMValueRef));
      const_vals[0]            = LLVMConstInt(len_value->type->llvm_type, len, false);
      const_vals[1]            = LLVMBuildGlobalStringPtr(cnt->llvm_builder, str, get_name("str"));
      LLVMSetLinkage(const_vals[1], LLVMInternalLinkage);

      result = LLVMConstNamedStruct(llvm_type, const_vals, 2);

      bl_free(const_vals);
    } else {
      BArray *       members = value->data.v_struct.members;
      const size_t   memc    = bo_array_size(members);
      MirConstValue *member;
      LLVMValueRef * llvm_members = bl_malloc(sizeof(LLVMValueRef) * memc);

      barray_foreach(members, member)
      {
        llvm_members[i] = gen_as_const(cnt, member);
      }

      result =
          LLVMConstStructInContext(cnt->llvm_cnt, llvm_members, memc, type->data.strct.is_packed);
      bl_free(llvm_members);
    }
    return result;
  }

  default:
    bl_unimplemented;
  }

  bl_abort("should not happend!!!");
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
  assert(var);

  /* skip when we should not generate LLVM representation */
  if (!var->gen_llvm) return;

  if (var->is_in_gscope) {
    /* OK variable is declared in global scope so we need different generation here*/
    /* Generates destination for global if there is no one. Global variable can come later than it
     * is used, so we call same function during generation of the declref instruction IR. */
    gen_global_var_proto(cnt, var);

    /* Globals must be set to some value */
    assert(decl->init);

    LLVMValueRef tmp = fetch_value(cnt, decl->init);
    LLVMSetInitializer(var->llvm_value, tmp);
  } else {
    assert(var->llvm_value);

    if (decl->init) {
      /* There is special handling for initialization via init instruction */
      if (decl->init->kind == MIR_INSTR_INIT) {
        MirInstrInit *init = (MirInstrInit *)decl->init;
        MirType *     type = var->alloc_type;

        /* CLEANUP: can be simplified */
        switch (type->kind) {
        case MIR_TYPE_ARRAY: {
          MirConstValue *tmp     = &init->base.const_value;
          LLVMValueRef llvm_size = LLVMConstInt(cnt->llvm_i64_type, type->store_size_bytes, false);

          if (tmp->data.v_array.is_zero_initializer) {
            /* zero initialized array */
            build_call_memset_0(cnt, var->llvm_value, llvm_size);
          } else if (init->base.comptime) {
            /* compile time known constant initializer */
            LLVMTypeRef llvm_type = var->alloc_type->llvm_type;
            assert(llvm_type);
            LLVMValueRef llvm_const_arr = LLVMAddGlobal(cnt->llvm_module, llvm_type, "");
            LLVMSetGlobalConstant(llvm_const_arr, true);
            LLVMSetLinkage(llvm_const_arr, LLVMInternalLinkage);
            LLVMSetInitializer(llvm_const_arr, fetch_value(cnt, &init->base));

            build_call_memcpy(cnt, var->llvm_value, llvm_const_arr, llvm_size);
          } else {
            /* one or more initizalizer values are known only in runtime */
            BArray *     values = init->values;
            MirInstr *   value;
            LLVMValueRef llvm_value;
            LLVMValueRef llvm_value_dest;
            LLVMValueRef llvm_indices[2];
            llvm_indices[0] = cnt->llvm_const_i64;

            barray_foreach(values, value)
            {
              llvm_value = fetch_value(cnt, value);
              assert(llvm_value);

              llvm_indices[1] = LLVMConstInt(cnt->llvm_i64_type, i, true);

              llvm_value_dest = LLVMBuildGEP(cnt->llvm_builder, var->llvm_value, llvm_indices,
                                             ARRAY_SIZE(llvm_indices), "");

              LLVMBuildStore(cnt->llvm_builder, llvm_value, llvm_value_dest);
            }
          }
          break;
        }

        case MIR_TYPE_STRUCT: {
          MirConstValue *tmp     = &init->base.const_value;
          LLVMValueRef llvm_size = LLVMConstInt(cnt->llvm_i64_type, type->store_size_bytes, false);

          if (tmp->data.v_array.is_zero_initializer) {
            /* zero initialized array */
            build_call_memset_0(cnt, var->llvm_value, llvm_size);
          } else if (init->base.comptime) {
            /* compile time known constant initializer */
            LLVMTypeRef llvm_type = var->alloc_type->llvm_type;
            assert(llvm_type);
            LLVMValueRef llvm_const_arr = LLVMAddGlobal(cnt->llvm_module, llvm_type, "");
            LLVMSetGlobalConstant(llvm_const_arr, true);
            LLVMSetLinkage(llvm_const_arr, LLVMInternalLinkage);
            LLVMSetInitializer(llvm_const_arr, fetch_value(cnt, &init->base));

            build_call_memcpy(cnt, var->llvm_value, llvm_const_arr, llvm_size);
          } else {
            /* one or more initizalizer values are known only in runtime */
            BArray *     values = init->values;
            MirInstr *   value;
            LLVMValueRef llvm_value;
            LLVMValueRef llvm_value_dest;

            barray_foreach(values, value)
            {
              llvm_value = fetch_value(cnt, value);
              assert(llvm_value);

              llvm_value_dest = LLVMBuildStructGEP(cnt->llvm_builder, var->llvm_value, i, "");
              LLVMBuildStore(cnt->llvm_builder, llvm_value, llvm_value_dest);
            }
          }
          break;
        }
        default:
          bl_unimplemented;
        }

      } else {
        /* use simple store */
        LLVMValueRef llvm_init = fetch_value(cnt, decl->init);
        assert(llvm_init);
        LLVMBuildStore(cnt->llvm_builder, llvm_init, var->llvm_value);
      }
    }
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
gen_instr_vargs(Context *cnt, MirInstrVArgs *vargs)
{
  MirType *vargs_type = vargs->base.const_value.type;
  BArray * values     = vargs->values;
  assert(values);
  const size_t vargsc = bo_array_size(values);
  assert(vargs_type && mir_is_vargs_type(vargs_type));

  /* Setup tmp array values. */
  if (vargsc > 0) {
    MirInstr *   value;
    LLVMValueRef llvm_value;
    LLVMValueRef llvm_value_dest;
    LLVMValueRef llvm_indices[2];
    llvm_indices[0] = cnt->llvm_const_i64;

    barray_foreach(values, value)
    {
      llvm_value = fetch_value(cnt, value);
      assert(llvm_value);
      llvm_indices[1] = LLVMConstInt(cnt->llvm_i64_type, i, true);
      llvm_value_dest = LLVMBuildGEP(cnt->llvm_builder, vargs->arr_tmp->llvm_value, llvm_indices,
                                     ARRAY_SIZE(llvm_indices), "");
      LLVMBuildStore(cnt->llvm_builder, llvm_value, llvm_value_dest);
    }
  }

  {
    LLVMValueRef llvm_len = LLVMConstInt(cnt->llvm_i64_type, vargsc, false);
    LLVMValueRef llvm_dest =
        LLVMBuildStructGEP(cnt->llvm_builder, vargs->vargs_tmp->llvm_value, 0, "");
    LLVMBuildStore(cnt->llvm_builder, llvm_len, llvm_dest);

    LLVMTypeRef llvm_ptr_type =
        bo_array_at(vargs_type->data.strct.members, 1, MirType *)->llvm_type;
    LLVMValueRef llvm_ptr =
        vargs->arr_tmp ? vargs->arr_tmp->llvm_value : LLVMConstNull(llvm_ptr_type);
    llvm_dest = LLVMBuildStructGEP(cnt->llvm_builder, vargs->vargs_tmp->llvm_value, 1, "");
    llvm_ptr  = LLVMBuildBitCast(cnt->llvm_builder, llvm_ptr, llvm_ptr_type, "");
    LLVMBuildStore(cnt->llvm_builder, llvm_ptr, llvm_dest);
  }

  vargs->base.llvm_value = LLVMBuildLoad(cnt->llvm_builder, vargs->vargs_tmp->llvm_value, "");
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

  const char *var_name;
  LLVMTypeRef var_type;
  unsigned    var_alignment;
  MirVar *    var;

  barray_foreach(fn->variables, var)
  {
    assert(var);
#if NAMED_VARS
    var_name = var->llvm_name;
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

  if (!(fn->flags & (FLAG_EXTERN))) {
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
  case MIR_INSTR_VARGS:
    gen_instr_vargs(cnt, (MirInstrVArgs *)instr);
    break;

  case MIR_INSTR_INIT:
    /* noop */
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

  cnt.llvm_void_type = LLVMVoidTypeInContext(cnt.llvm_cnt);
  cnt.llvm_i1_type   = LLVMInt1TypeInContext(cnt.llvm_cnt);
  cnt.llvm_i8_type   = LLVMInt8TypeInContext(cnt.llvm_cnt);
  cnt.llvm_i32_type  = LLVMInt32TypeInContext(cnt.llvm_cnt);
  cnt.llvm_i64_type  = LLVMInt64TypeInContext(cnt.llvm_cnt);

  cnt.llvm_i8_ptr_type = LLVMPointerType(cnt.llvm_i8_type, 0);

  cnt.llvm_const_i64 = LLVMConstInt(cnt.llvm_i64_type, 0, false);
  cnt.llvm_trap_fn   = create_trap_fn(&cnt);
  cnt.llvm_memset_fn = create_memset_fn(&cnt);
  cnt.llvm_memcpy_fn = create_memcpy_fn(&cnt);

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
