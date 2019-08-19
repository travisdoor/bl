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

#include "assembly.h"
#include "builder.h"
#include "common.h"
#include "llvm_di.h"
#include "mir.h"
#include "unit.h"
#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
//#include <llvm-c/DebugInfo.h>

#define LLVM_INSTRINSIC_TRAP "llvm.debugtrap"
#define LLVM_INSTRINSIC_MEMSET "llvm.memset.p0i8.i64"
#define LLVM_INTRINSIC_MEMCPY "llvm.memcpy.p0i8.p0i8.i64"

#if BL_DEBUG
#define NAMED_VARS true
#else
#define NAMED_VARS false
#endif

typedef struct {
	bool        debug_build;
	Builder *   builder;
	Assembly *  assembly;
	BHashTable *gstring_cache;

	LLVMContextRef    llvm_cnt;
	LLVMModuleRef     llvm_module;
	LLVMTargetDataRef llvm_td;
	LLVMBuilderRef    llvm_builder;
	LLVMDIBuilderRef  llvm_dibuilder;

	/* Constants */
	LLVMValueRef llvm_const_i64;

	/* Types */
	LLVMTypeRef llvm_void_type;
	LLVMTypeRef llvm_i1_type;
	LLVMTypeRef llvm_i8_type;
	LLVMTypeRef llvm_i32_type;
	LLVMTypeRef llvm_i64_type;
	LLVMTypeRef llvm_i8_ptr_type;

	/* Intrinsics */
	LLVMValueRef llvm_instrinsic_trap;
	LLVMValueRef llvm_instrinsic_memset;
	LLVMValueRef llvm_intrinsic_memcpy;
} Context;

static inline LLVMValueRef
create_trap_fn(Context *cnt)
{
	LLVMTypeRef llvm_fn_type = LLVMFunctionType(cnt->llvm_void_type, NULL, 0, false);
	return LLVMAddFunction(cnt->llvm_module, LLVM_INSTRINSIC_TRAP, llvm_fn_type);
}

static inline LLVMValueRef
create_memset_fn(Context *cnt)
{
#ifdef BL_PLATFORM_LINUX
	LLVMTypeRef llvm_args[5] = {cnt->llvm_i8_ptr_type,
	                            cnt->llvm_i8_type,
	                            cnt->llvm_i64_type,
	                            cnt->llvm_i32_type,
	                            cnt->llvm_i1_type};

	LLVMTypeRef llvm_fn_type = LLVMFunctionType(cnt->llvm_void_type, llvm_args, 5, false);
#else
	LLVMTypeRef llvm_args[4] = {
	    cnt->llvm_i8_ptr_type, cnt->llvm_i8_type, cnt->llvm_i64_type, cnt->llvm_i1_type};

	LLVMTypeRef llvm_fn_type = LLVMFunctionType(cnt->llvm_void_type, llvm_args, 4, false);
#endif

	LLVMValueRef llvm_fn =
	    LLVMAddFunction(cnt->llvm_module, LLVM_INSTRINSIC_MEMSET, llvm_fn_type);
	return llvm_fn;
}

static inline LLVMValueRef
create_memcpy_fn(Context *cnt)
{
#ifdef BL_PLATFORM_LINUX
	LLVMTypeRef llvm_args[5] = {cnt->llvm_i8_ptr_type,
	                            cnt->llvm_i8_ptr_type,
	                            cnt->llvm_i64_type,
	                            cnt->llvm_i32_type,
	                            cnt->llvm_i1_type};
	LLVMTypeRef llvm_fn_type = LLVMFunctionType(cnt->llvm_void_type, llvm_args, 5, false);
#else
	LLVMTypeRef llvm_args[4] = {
	    cnt->llvm_i8_ptr_type, cnt->llvm_i8_ptr_type, cnt->llvm_i64_type, cnt->llvm_i1_type};
	LLVMTypeRef  llvm_fn_type = LLVMFunctionType(cnt->llvm_void_type, llvm_args, 4, false);
#endif

	LLVMValueRef llvm_fn =
	    LLVMAddFunction(cnt->llvm_module, LLVM_INTRINSIC_MEMCPY, llvm_fn_type);
	return llvm_fn;
}

static inline LLVMValueRef
build_call_memset_0(Context *    cnt,
                    LLVMValueRef llvm_dest_ptr,
                    LLVMValueRef llvm_size,
                    LLVMValueRef llvm_alignment)
{
#ifdef BL_PLATFORM_LINUX
	LLVMValueRef llvm_args[5] = {
	    LLVMBuildBitCast(cnt->llvm_builder, llvm_dest_ptr, cnt->llvm_i8_ptr_type, ""),
	    LLVMConstInt(cnt->llvm_i8_type, 0, false),
	    llvm_size,
	    llvm_alignment,
	    LLVMConstInt(cnt->llvm_i1_type, 0, false)};

	LLVMValueRef llvm_result =
	    LLVMBuildCall(cnt->llvm_builder, cnt->llvm_instrinsic_memset, llvm_args, 5, "");
#else
	LLVMValueRef llvm_args[4] = {
	    LLVMBuildBitCast(cnt->llvm_builder, llvm_dest_ptr, cnt->llvm_i8_ptr_type, ""),
	    LLVMConstInt(cnt->llvm_i8_type, 0, false),
	    llvm_size,
	    LLVMConstInt(cnt->llvm_i1_type, 0, false)};

	LLVMValueRef llvm_result =
	    LLVMBuildCall(cnt->llvm_builder, cnt->llvm_instrinsic_memset, llvm_args, 4, "");
#endif

	return llvm_result;
}

static inline LLVMValueRef
build_call_memcpy(Context *    cnt,
                  LLVMValueRef llvm_dest_ptr,
                  LLVMValueRef llvm_src_ptr,
                  LLVMValueRef llvm_size,
                  LLVMValueRef llvm_alignment)
{
#ifdef BL_PLATFORM_LINUX
	LLVMValueRef llvm_args[5] = {
	    LLVMBuildBitCast(cnt->llvm_builder, llvm_dest_ptr, cnt->llvm_i8_ptr_type, ""),
	    LLVMBuildBitCast(cnt->llvm_builder, llvm_src_ptr, cnt->llvm_i8_ptr_type, ""),
	    llvm_size,
	    llvm_alignment,
	    LLVMConstInt(cnt->llvm_i1_type, 0, false)};

	LLVMValueRef llvm_result =
	    LLVMBuildCall(cnt->llvm_builder, cnt->llvm_intrinsic_memcpy, llvm_args, 5, "");
#else
	LLVMValueRef llvm_args[4] = {
	    LLVMBuildBitCast(cnt->llvm_builder, llvm_dest_ptr, cnt->llvm_i8_ptr_type, ""),
	    LLVMBuildBitCast(cnt->llvm_builder, llvm_src_ptr, cnt->llvm_i8_ptr_type, ""),
	    llvm_size,
	    LLVMConstInt(cnt->llvm_i1_type, 0, false)};

	LLVMValueRef llvm_result =
	    LLVMBuildCall(cnt->llvm_builder, cnt->llvm_intrinsic_memcpy, llvm_args, 4, "");
#endif

	return llvm_result;
}

static void
emit_RTTI_types(Context *cnt);

static void
emit_instr(Context *cnt, MirInstr *instr);

/*
 * Tmp is optional but needed for naked compound expressions.
 */
static void
emit_instr_compound(Context *cnt, MirVar *_tmp_var, MirInstrCompound *cmp);

static void
emit_instr_binop(Context *cnt, MirInstrBinop *binop);

static void
emit_instr_phi(Context *cnt, MirInstrPhi *phi);

static void
emit_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info);

static void
emit_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static void
emit_instr_cast(Context *cnt, MirInstrCast *cast);

static void
emit_instr_addrof(Context *cnt, MirInstrAddrOf *addrof);

static void
emit_instr_unop(Context *cnt, MirInstrUnop *unop);

static void
emit_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static void
emit_instr_store(Context *cnt, MirInstrStore *store);

static void
emit_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);

static void
emit_instr_block(Context *cnt, MirInstrBlock *block);

static void
emit_instr_br(Context *cnt, MirInstrBr *br);

static void
emit_instr_arg(Context *cnt, MirInstrArg *arg);

static void
emit_instr_cond_br(Context *cnt, MirInstrCondBr *br);

static void
emit_instr_ret(Context *cnt, MirInstrRet *ret);

static void
emit_instr_decl_var(Context *cnt, MirInstrDeclVar *decl);

static void
emit_instr_load(Context *cnt, MirInstrLoad *load);

static void
emit_instr_call(Context *cnt, MirInstrCall *call);

static void
emit_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static void
emit_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr);

static void
emit_instr_vargs(Context *cnt, MirInstrVArgs *vargs);

static void
emit_instr_toany(Context *cnt, MirInstrToAny *toany);

static LLVMValueRef
emit_as_const(Context *cnt, MirConstValue *value);

static LLVMValueRef
emit_global_string_ptr(Context *cnt, const char *str, size_t len);

static void
emit_allocas(Context *cnt, MirFn *fn);

static inline LLVMValueRef
emit_fn_proto(Context *cnt, MirFn *fn)
{
	assert(fn);

	fn->llvm_value = LLVMGetNamedFunction(cnt->llvm_module, fn->llvm_name);
	if (!fn->llvm_value) {
		fn->llvm_value =
		    LLVMAddFunction(cnt->llvm_module, fn->llvm_name, fn->type->llvm_type);

		if (cnt->debug_build) llvm_di_create_fn(cnt->llvm_dibuilder, fn);
	}

	return fn->llvm_value;
}

static inline LLVMValueRef
emit_global_var_proto(Context *cnt, MirVar *var)
{
	assert(var);
	if (var->llvm_value) return var->llvm_value;

	LLVMTypeRef llvm_type = var->value.type->llvm_type;
	var->llvm_value       = LLVMAddGlobal(cnt->llvm_module, llvm_type, var->llvm_name);

	LLVMSetGlobalConstant(var->llvm_value, !var->is_mutable);

	/* Linkage should be later set by user. */
	LLVMSetLinkage(var->llvm_value, LLVMPrivateLinkage);
	LLVMSetAlignment(var->llvm_value, var->value.type->alignment);

	return var->llvm_value;
}

static inline LLVMValueRef
fetch_value(Context *cnt, MirInstr *instr)
{
	LLVMValueRef value = NULL;

	if (instr->comptime && !instr->llvm_value) {
		/* Declaration references must be generated even if they are compile time. */
		if (instr->kind == MIR_INSTR_DECL_REF) {
			emit_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
		} else {
			instr->llvm_value = emit_as_const(cnt, &instr->value);
		}
	}

	value = instr->llvm_value;
	assert(value);
	return value;
}

static inline LLVMBasicBlockRef
emit_basic_block(Context *cnt, MirInstrBlock *block)
{
	if (!block) return NULL;
	LLVMBasicBlockRef llvm_block = NULL;
	if (!block->base.llvm_value) {
		llvm_block = LLVMAppendBasicBlockInContext(
		    cnt->llvm_cnt, block->owner_fn->llvm_value, block->name);
		block->base.llvm_value = LLVMBasicBlockAsValue(llvm_block);
	} else {
		llvm_block = LLVMValueAsBasicBlock(block->base.llvm_value);
	}

	return llvm_block;
}

/* impl */
void
emit_RTTI_types(Context *cnt)
{
	BArray *table = cnt->assembly->mir_module->RTTI_tmp_vars;
	assert(table);

	MirVar *     var;
	LLVMValueRef llvm_var, llvm_value;
	LLVMTypeRef  llvm_var_type;

	const size_t count = bo_array_size(table);

	for (size_t i = 0; i < count; ++i) {
		var = bo_array_at(table, i, MirVar *);
		assert(var);

		llvm_var      = emit_global_var_proto(cnt, var);
		llvm_var_type = var->value.type->llvm_type;
		llvm_value    = emit_as_const(cnt, &var->value);

		LLVMSetInitializer(llvm_var, llvm_value);
		LLVMSetLinkage(llvm_var, LLVMPrivateLinkage);
		LLVMSetGlobalConstant(llvm_var, true);
		LLVMSetAlignment(llvm_var, LLVMABIAlignmentOfType(cnt->llvm_td, llvm_var_type));
		LLVMSetUnnamedAddr(llvm_var, true);
	}
}

void
emit_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
	ScopeEntry *entry = ref->scope_entry;
	assert(entry);

	switch (entry->kind) {
	case SCOPE_ENTRY_VAR: {
		MirVar *var = entry->data.var;
		if (var->is_in_gscope)
			ref->base.llvm_value = emit_global_var_proto(cnt, var);
		else
			ref->base.llvm_value = var->llvm_value;
		break;
	}
	case SCOPE_ENTRY_FN: {
		ref->base.llvm_value = emit_fn_proto(cnt, entry->data.fn);
		break;
	}
	default:
		bl_unimplemented;
	}

	assert(ref->base.llvm_value);
}

void
emit_instr_phi(Context *cnt, MirInstrPhi *phi)
{
	LLVMValueRef llvm_phi =
	    LLVMBuildPhi(cnt->llvm_builder, phi->base.value.type->llvm_type, "");

	const size_t       count   = bo_array_size(phi->incoming_blocks);
	LLVMValueRef *     llvm_iv = bl_malloc(sizeof(LLVMValueRef) * count);
	LLVMBasicBlockRef *llvm_ib = bl_malloc(sizeof(LLVMBasicBlockRef) * count);
	if (llvm_iv == NULL || llvm_ib == NULL) bl_abort("bad alloc");

	MirInstr *     value;
	MirInstrBlock *block;
	for (size_t i = 0; i < count; ++i) {
		value = bo_array_at(phi->incoming_values, i, MirInstr *);
		block = bo_array_at(phi->incoming_blocks, i, MirInstrBlock *);

		llvm_iv[i] = fetch_value(cnt, value);
		llvm_ib[i] = emit_basic_block(cnt, block);
	}

	LLVMAddIncoming(llvm_phi, llvm_iv, llvm_ib, (unsigned int)count);

	bl_free(llvm_iv);
	bl_free(llvm_ib);

	phi->base.llvm_value = llvm_phi;
}

void
emit_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
	unr->base.llvm_value =
	    LLVMBuildCall(cnt->llvm_builder, cnt->llvm_instrinsic_trap, NULL, 0, "");
}

void
emit_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info)
{
	MirType *type = type_info->expr_type;
	assert(type);
	assert(type->rtti.var);

	LLVMValueRef llvm_var = type->rtti.var->llvm_value;
	assert(llvm_var && "Missing LLVM value for RTTI variable.");

	LLVMTypeRef llvm_dest_type = type_info->base.value.type->llvm_type;

	llvm_var = LLVMBuildPointerCast(cnt->llvm_builder, llvm_var, llvm_dest_type, "");
	type_info->base.llvm_value = llvm_var;
}

void
emit_instr_cast(Context *cnt, MirInstrCast *cast)
{
	LLVMValueRef llvm_src       = cast->expr->llvm_value;
	LLVMTypeRef  llvm_dest_type = cast->base.value.type->llvm_type;
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

	case MIR_CAST_FPTRUNC:
		llvm_op = LLVMFPTrunc;
		break;

	case MIR_CAST_FPEXT:
		llvm_op = LLVMFPExt;
		break;

	case MIR_CAST_SITOFP:
		llvm_op = LLVMSIToFP;
		break;

	case MIR_CAST_UITOFP:
		llvm_op = LLVMUIToFP;
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

	cast->base.llvm_value =
	    LLVMBuildCast(cnt->llvm_builder, llvm_op, llvm_src, llvm_dest_type, "");
}

void
emit_instr_addrof(Context *cnt, MirInstrAddrOf *addrof)
{
	addrof->base.llvm_value = addrof->src->llvm_value;
	assert(addrof->base.llvm_value);
}

void
emit_instr_arg(Context *cnt, MirInstrArg *arg)
{
	MirFn *fn = arg->base.owner_block->owner_fn;
	assert(fn);
	LLVMValueRef llvm_fn = fn->llvm_value;
	assert(llvm_fn);

	arg->base.llvm_value = LLVMGetParam(llvm_fn, arg->i);
}

void
emit_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
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

		elem_ptr->base.llvm_value = LLVMBuildInBoundsGEP(
		    cnt->llvm_builder, llvm_arr_ptr, llvm_indices, array_size(llvm_indices), "");

		return;
	}

	LLVMValueRef llvm_indices[2];
	llvm_indices[0] = cnt->llvm_const_i64;
	llvm_indices[1] = llvm_index;

	elem_ptr->base.llvm_value = LLVMBuildGEP(
	    cnt->llvm_builder, llvm_arr_ptr, llvm_indices, array_size(llvm_indices), "");
}

void
emit_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
	LLVMValueRef llvm_target_ptr = fetch_value(cnt, member_ptr->target_ptr);
	assert(llvm_target_ptr);

	if (member_ptr->builtin_id == MIR_BUILTIN_ID_NONE) {
		assert(member_ptr->scope_entry->kind == SCOPE_ENTRY_MEMBER);
		MirMember *member = member_ptr->scope_entry->data.member;
		assert(member);

		const unsigned int index =
		    (const unsigned int)member_ptr->scope_entry->data.member->index;

		member_ptr->base.llvm_value =
		    LLVMBuildStructGEP(cnt->llvm_builder, llvm_target_ptr, index, "");
		assert(member_ptr->base.llvm_value);
	} else {
		/* builtin member */

		/* Valid only for slice types, we generate direct replacement for arrays. */
		if (member_ptr->builtin_id == MIR_BUILTIN_ID_ARR_LEN) {
			/* .len */
			member_ptr->base.llvm_value =
			    LLVMBuildStructGEP(cnt->llvm_builder, llvm_target_ptr, 0, "");
		} else if (member_ptr->builtin_id == MIR_BUILTIN_ID_ARR_PTR) {
			/* .ptr*/
			member_ptr->base.llvm_value =
			    LLVMBuildStructGEP(cnt->llvm_builder, llvm_target_ptr, 1, "");
		}
	}
}

void
emit_instr_load(Context *cnt, MirInstrLoad *load)
{
	assert(load->base.value.type && "invalid type of load instruction");
	LLVMValueRef   llvm_src  = fetch_value(cnt, load->src);
	const unsigned alignment = load->base.value.type->alignment;
	assert(llvm_src);
	load->base.llvm_value = LLVMBuildLoad(cnt->llvm_builder, llvm_src, "");
	LLVMSetAlignment(load->base.llvm_value, alignment);
}

LLVMValueRef
emit_global_string_ptr(Context *cnt, const char *str, size_t len)
{
	assert(str && len);
	uint64_t      hash  = bo_hash_from_str(str);
	bo_iterator_t found = bo_htbl_find(cnt->gstring_cache, hash);
	bo_iterator_t end   = bo_htbl_end(cnt->gstring_cache);

	if (!bo_iterator_equal(&found, &end)) {
		return bo_htbl_iter_peek_value(cnt->gstring_cache, &found, LLVMValueRef);
	}

	/* Generate global string constant */
	LLVMValueRef llvm_str = NULL;
	{
		assert(len && "String must be zero terminated");
		LLVMTypeRef llvm_str_arr_type =
		    LLVMArrayType(cnt->llvm_i8_type, (unsigned int)len + 1);
		llvm_str = LLVMAddGlobal(cnt->llvm_module, llvm_str_arr_type, ".str");

		LLVMValueRef *llvm_chars = bl_malloc(sizeof(LLVMValueRef) * (len + 1));
		if (!llvm_chars) bl_abort("bad alloc");

		for (size_t i = 0; i < len + 1; ++i) {
			llvm_chars[i] = LLVMConstInt(cnt->llvm_i8_type, str[i], true);
		}

		LLVMValueRef llvm_str_arr =
		    LLVMConstArray(cnt->llvm_i8_type, llvm_chars, (unsigned int)len + 1);
		LLVMSetInitializer(llvm_str, llvm_str_arr);
		LLVMSetLinkage(llvm_str, LLVMPrivateLinkage);
		LLVMSetGlobalConstant(llvm_str, true);
		LLVMSetAlignment(llvm_str, LLVMABIAlignmentOfType(cnt->llvm_td, llvm_str_arr_type));
		LLVMSetUnnamedAddr(llvm_str, true);
		llvm_str = LLVMConstBitCast(llvm_str, cnt->llvm_i8_ptr_type);
		bl_free(llvm_chars);
	}

	bo_htbl_insert(cnt->gstring_cache, hash, llvm_str);
	return llvm_str;
}

LLVMValueRef
emit_as_const(Context *cnt, MirConstValue *value)
{
	MirType *type = value->type;
	assert(type);
	LLVMTypeRef  llvm_type  = type->llvm_type;
	LLVMValueRef llvm_value = NULL;
	assert(llvm_type);

	switch (type->kind) {
	case MIR_TYPE_INT: {
		llvm_value =
		    LLVMConstInt(llvm_type, value->data.v_u64, type->data.integer.is_signed);
		break;
	}

	case MIR_TYPE_REAL: {
		const size_t size = type->store_size_bytes;

		if (size == sizeof(float)) { // float
			llvm_value = LLVMConstReal(llvm_type, value->data.v_f32);
			break;
		} else if (size == sizeof(double)) { // double
			llvm_value = LLVMConstReal(llvm_type, value->data.v_f64);
			break;
		}
		bl_abort("invalid floating point type");
	}

	case MIR_TYPE_BOOL:
		llvm_value = LLVMConstInt(llvm_type, value->data.v_s32, false);
		break;

	case MIR_TYPE_NULL:
		assert(value->data.v_ptr.data.any == NULL);
		llvm_value = LLVMConstNull(llvm_type);
		break;

	case MIR_TYPE_PTR: {
		type = mir_deref_type(type);
		assert(type);

		if (type->kind == MIR_TYPE_FN) {
			/* Constant pointer to the function. Value must contains pointer to MirFn
			 * instance! */
			MirFn *fn = value->data.v_ptr.data.any
			                ? value->data.v_ptr.data.value->data.v_ptr.data.fn
			                : NULL;
			assert(fn && "Function pointer not set for compile time known constant "
			             "pointer to function.");

			llvm_value = emit_fn_proto(cnt, fn);
			assert(llvm_value);
			break;
		} else {
			/* value must contains pointer to constant variable */
			MirVar *pointed = value->data.v_ptr.data.var;
			assert(pointed && pointed->llvm_value &&
			       "Invalid const pointer to variable.");

			llvm_value = pointed->llvm_value;
			break;
		}
	}

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
			llvm_elems[i] = emit_as_const(cnt, elem);
		}

		llvm_value = LLVMConstArray(llvm_elem_type, llvm_elems, (unsigned int)len);
		bl_free(llvm_elems);
		break;
	}

	case MIR_TYPE_STRING: {
		BArray *     members = value->data.v_struct.members;
		const size_t memc    = bo_array_size(members);
		assert(members);
		assert(memc == 2 && "not slice string?");

		MirConstValue *len_value = bo_array_at(members, 0, MirConstValue *);
		MirConstValue *str_value = bo_array_at(members, 1, MirConstValue *);
		assert(len_value && str_value);

		const uint64_t len = len_value->data.v_u64;
		const char *   str = str_value->data.v_ptr.data.str;
		assert(str);

		LLVMValueRef const_vals[2];
		const_vals[0] = LLVMConstInt(len_value->type->llvm_type, len, false);
		const_vals[1] = emit_global_string_ptr(cnt, str, len);

		llvm_value = LLVMConstNamedStruct(llvm_type, const_vals, 2);
		break;
	}

	case MIR_TYPE_SLICE:
	case MIR_TYPE_VARGS:
	case MIR_TYPE_STRUCT: {
		BArray *     members = value->data.v_struct.members;
		const size_t memc    = bo_array_size(members);

		MirConstValue *member;
		LLVMValueRef * llvm_members = bl_malloc(sizeof(LLVMValueRef) * memc);

		barray_foreach(members, member)
		{
			llvm_members[i] = emit_as_const(cnt, member);
		}

		llvm_value = LLVMConstNamedStruct(llvm_type, llvm_members, (unsigned int)memc);
		bl_free(llvm_members);
		break;
	}

	case MIR_TYPE_ENUM: {
		LLVMTypeRef llvm_base_type = value->type->llvm_type;

		llvm_value = LLVMConstInt(
		    llvm_base_type, value->data.v_u64, value->type->data.integer.is_signed);
		break;
	}

	default:
		bl_unimplemented;
	}

	assert(llvm_value);
	return llvm_value;
}

void
emit_instr_store(Context *cnt, MirInstrStore *store)
{
	LLVMValueRef   val       = fetch_value(cnt, store->src);
	LLVMValueRef   ptr       = fetch_value(cnt, store->dest);
	const unsigned alignment = store->src->value.type->alignment;
	assert(val && ptr);
	store->base.llvm_value = LLVMBuildStore(cnt->llvm_builder, val, ptr);
	LLVMSetAlignment(store->base.llvm_value, alignment);
}

void
emit_instr_unop(Context *cnt, MirInstrUnop *unop)
{
	LLVMValueRef llvm_val = fetch_value(cnt, unop->instr);
	assert(llvm_val);

	LLVMTypeKind lhs_kind   = LLVMGetTypeKind(LLVMTypeOf(llvm_val));
	const bool   float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

	switch (unop->op) {
	case UNOP_NOT: {
		assert(!float_kind && "Invalid negation of floating point type.");
		unop->base.llvm_value = LLVMBuildNot(cnt->llvm_builder, llvm_val, "");
		break;
	}

	case UNOP_NEG: {
		if (float_kind)
			unop->base.llvm_value = LLVMBuildFNeg(cnt->llvm_builder, llvm_val, "");
		else
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
emit_instr_compound(Context *cnt, MirVar *_tmp_var, MirInstrCompound *cmp)
{
	/*
	 * Temporary variable for naked compounds is implicitly generated variable. When compound is
	 * used for variable initialization, variable's allocated memory is used directly and MirVar
	 * must be passed in parameters.
	 */
	MirVar *tmp_var = _tmp_var ? _tmp_var : cmp->tmp_var;
	assert(tmp_var && "Missing temporary variable");

	LLVMValueRef llvm_tmp = tmp_var->llvm_value;
	assert(llvm_tmp);

	MirType *type = tmp_var->value.type;
	assert(type);

	/*
	 * Initializer variants:
	 * 1) Zero initialization - Compound is initialized to 0 by memset intrinsic.
	 * 2) Constant            - Compound is compile time known constant.
	 * 3) Runtime             - One or more compound members are known in runtime only.
	 */

	LLVMValueRef llvm_size = LLVMConstInt(cnt->llvm_i64_type, type->store_size_bytes, false);
	LLVMValueRef llvm_alignment = LLVMConstInt(cnt->llvm_i32_type, type->alignment, true);

	if (cmp->is_zero_initialized) {
		/* zero initialized */
		build_call_memset_0(cnt, llvm_tmp, llvm_size, llvm_alignment);
	} else if (cmp->base.comptime) {
		/* compile time known */
		LLVMTypeRef llvm_type = type->llvm_type;
		assert(llvm_type);
		LLVMValueRef llvm_const = LLVMAddGlobal(cnt->llvm_module, llvm_type, "");
		LLVMSetGlobalConstant(llvm_const, true);
		LLVMSetLinkage(llvm_const, LLVMPrivateLinkage);
		LLVMSetAlignment(llvm_const, type->alignment);
		LLVMSetInitializer(llvm_const, fetch_value(cnt, &cmp->base));

		build_call_memcpy(cnt, llvm_tmp, llvm_const, llvm_size, llvm_alignment);
	} else {
		BArray *     values = cmp->values;
		MirInstr *   value;
		LLVMValueRef llvm_value;
		LLVMValueRef llvm_value_dest;
		LLVMValueRef llvm_indices[2];
		llvm_indices[0] = cnt->llvm_const_i64;

		barray_foreach(values, value)
		{
			llvm_value = fetch_value(cnt, value);
			assert(llvm_value);

			switch (type->kind) {
			case MIR_TYPE_ARRAY:
				llvm_indices[1] = LLVMConstInt(cnt->llvm_i64_type, i, true);
				llvm_value_dest = LLVMBuildGEP(cnt->llvm_builder,
				                               llvm_tmp,
				                               llvm_indices,
				                               array_size(llvm_indices),
				                               "");
				break;

			case MIR_TYPE_STRING:
			case MIR_TYPE_SLICE:
			case MIR_TYPE_VARGS:
			case MIR_TYPE_STRUCT:
				llvm_value_dest = LLVMBuildStructGEP(
				    cnt->llvm_builder, tmp_var->llvm_value, (unsigned int)i, "");
				break;

			default:
				assert(i == 0);
				llvm_value_dest = llvm_tmp;
				break;
			}

			LLVMBuildStore(cnt->llvm_builder, llvm_value, llvm_value_dest);
		}
	}

	cmp->base.llvm_value = llvm_tmp;
}

void
emit_instr_binop(Context *cnt, MirInstrBinop *binop)
{
	LLVMValueRef lhs = fetch_value(cnt, binop->lhs);
	LLVMValueRef rhs = fetch_value(cnt, binop->rhs);
	assert(lhs && rhs);

	MirType *  type           = binop->lhs->value.type;
	const bool real_type      = type->kind == MIR_TYPE_REAL;
	const bool signed_integer = type->kind == MIR_TYPE_INT && type->data.integer.is_signed;

	switch (binop->op) {
	case BINOP_ADD:
		if (real_type)
			binop->base.llvm_value = LLVMBuildFAdd(cnt->llvm_builder, lhs, rhs, "");
		else
			binop->base.llvm_value = LLVMBuildAdd(cnt->llvm_builder, lhs, rhs, "");
		break;

	case BINOP_SUB:
		if (real_type)
			binop->base.llvm_value = LLVMBuildFSub(cnt->llvm_builder, lhs, rhs, "");
		else
			binop->base.llvm_value = LLVMBuildSub(cnt->llvm_builder, lhs, rhs, "");
		break;

	case BINOP_MUL:
		if (real_type)
			binop->base.llvm_value = LLVMBuildFMul(cnt->llvm_builder, lhs, rhs, "");
		else
			binop->base.llvm_value = LLVMBuildMul(cnt->llvm_builder, lhs, rhs, "");
		break;

	case BINOP_DIV:
		if (real_type)
			binop->base.llvm_value = LLVMBuildFDiv(cnt->llvm_builder, lhs, rhs, "");
		else if (signed_integer)
			binop->base.llvm_value = LLVMBuildSDiv(cnt->llvm_builder, lhs, rhs, "");
		else
			binop->base.llvm_value = LLVMBuildUDiv(cnt->llvm_builder, lhs, rhs, "");

		break;

	case BINOP_MOD:
		if (signed_integer)
			binop->base.llvm_value = LLVMBuildSRem(cnt->llvm_builder, lhs, rhs, "");
		else
			binop->base.llvm_value = LLVMBuildURem(cnt->llvm_builder, lhs, rhs, "");
		break;

	case BINOP_EQ:
		if (real_type)
			binop->base.llvm_value =
			    LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOEQ, lhs, rhs, "");
		else
			binop->base.llvm_value =
			    LLVMBuildICmp(cnt->llvm_builder, LLVMIntEQ, lhs, rhs, "");
		break;

	case BINOP_NEQ:
		if (real_type)
			binop->base.llvm_value =
			    LLVMBuildFCmp(cnt->llvm_builder, LLVMRealONE, lhs, rhs, "");
		else
			binop->base.llvm_value =
			    LLVMBuildICmp(cnt->llvm_builder, LLVMIntNE, lhs, rhs, "");
		break;

	case BINOP_GREATER:
		if (real_type)
			binop->base.llvm_value =
			    LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGT, lhs, rhs, "");
		else
			binop->base.llvm_value =
			    LLVMBuildICmp(cnt->llvm_builder,
			                  signed_integer ? LLVMIntSGT : LLVMIntUGT,
			                  lhs,
			                  rhs,
			                  "");
		break;

	case BINOP_LESS:
		if (real_type)
			binop->base.llvm_value =
			    LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLT, lhs, rhs, "");
		else
			binop->base.llvm_value =
			    LLVMBuildICmp(cnt->llvm_builder,
			                  signed_integer ? LLVMIntSLT : LLVMIntULT,
			                  lhs,
			                  rhs,
			                  "");
		break;

	case BINOP_GREATER_EQ:
		if (real_type)
			binop->base.llvm_value =
			    LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGE, lhs, rhs, "");
		else
			binop->base.llvm_value =
			    LLVMBuildICmp(cnt->llvm_builder,
			                  signed_integer ? LLVMIntSGE : LLVMIntUGE,
			                  lhs,
			                  rhs,
			                  "");
		break;

	case BINOP_LESS_EQ:
		if (real_type)
			binop->base.llvm_value =
			    LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLE, lhs, rhs, "");
		else
			binop->base.llvm_value =
			    LLVMBuildICmp(cnt->llvm_builder,
			                  signed_integer ? LLVMIntSLE : LLVMIntULE,
			                  lhs,
			                  rhs,
			                  "");
		break;

	case BINOP_AND:
		binop->base.llvm_value = LLVMBuildAnd(cnt->llvm_builder, lhs, rhs, "");
		break;

	case BINOP_OR:
		binop->base.llvm_value = LLVMBuildOr(cnt->llvm_builder, lhs, rhs, "");
		break;

	case BINOP_SHR:
		if (signed_integer)
			binop->base.llvm_value = LLVMBuildAShr(cnt->llvm_builder, lhs, rhs, "");
		else
			binop->base.llvm_value = LLVMBuildLShr(cnt->llvm_builder, lhs, rhs, "");
		break;

	case BINOP_SHL:
		binop->base.llvm_value = LLVMBuildShl(cnt->llvm_builder, lhs, rhs, "");
		break;

	default:
		bl_abort("Invalid binary operation.");
	}
}

void
emit_instr_call(Context *cnt, MirInstrCall *call)
{
	MirInstr *callee = call->callee;
	assert(callee);
	assert(callee->value.type);

	LLVMValueRef llvm_fn = callee->llvm_value
	                           ? callee->llvm_value
	                           : emit_fn_proto(cnt, callee->value.data.v_ptr.data.fn);

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

	//if (cnt->debug_build) llvm_di_set_instr_location(cnt->llvm_dibuilder, &call->base);
}

void
emit_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
	MirVar *var = decl->var;
	assert(var);

	/* skip when we should not generate LLVM representation */
	if (var->value.type->kind == MIR_TYPE_TYPE) return;

	if (var->is_in_gscope) {
		/* OK variable is declared in global scope so we need different generation here*/
		/* Generates destination for global if there is no one. Global variable can come
		 * later than
		 * it is used, so we call same function during generation of the declref instruction
		 * IR. */
		/* Globals must be set to some value */
		assert(decl->init);

		LLVMValueRef tmp = fetch_value(cnt, decl->init);

		emit_global_var_proto(cnt, var);
		LLVMSetInitializer(var->llvm_value, tmp);
	} else {
		assert(var->llvm_value);

		if (decl->init) {
			/* There is special handling for initialization via compound instruction */
			if (decl->init->kind == MIR_INSTR_COMPOUND) {
				emit_instr_compound(cnt, var, (MirInstrCompound *)decl->init);
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
emit_instr_ret(Context *cnt, MirInstrRet *ret)
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
emit_instr_br(Context *cnt, MirInstrBr *br)
{
	MirInstrBlock *then_block = br->then_block;
	assert(then_block);

	LLVMBasicBlockRef llvm_then_block = emit_basic_block(cnt, then_block);
	assert(llvm_then_block);
	br->base.llvm_value = LLVMBuildBr(cnt->llvm_builder, llvm_then_block);

	LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_then_block);
}

void
emit_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
	MirInstr *     cond       = br->cond;
	MirInstrBlock *then_block = br->then_block;
	MirInstrBlock *else_block = br->else_block;
	assert(cond && then_block);

	LLVMValueRef      llvm_cond       = fetch_value(cnt, cond);
	LLVMBasicBlockRef llvm_then_block = emit_basic_block(cnt, then_block);
	LLVMBasicBlockRef llvm_else_block = emit_basic_block(cnt, else_block);

	br->base.llvm_value =
	    LLVMBuildCondBr(cnt->llvm_builder, llvm_cond, llvm_then_block, llvm_else_block);
}

void
emit_instr_vargs(Context *cnt, MirInstrVArgs *vargs)
{
	MirType *vargs_type = vargs->base.value.type;
	BArray * values     = vargs->values;
	assert(values);
	const size_t vargsc = bo_array_size(values);
	assert(vargs_type && vargs_type->kind == MIR_TYPE_VARGS);

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
			llvm_value_dest = LLVMBuildGEP(cnt->llvm_builder,
			                               vargs->arr_tmp->llvm_value,
			                               llvm_indices,
			                               array_size(llvm_indices),
			                               "");
			LLVMBuildStore(cnt->llvm_builder, llvm_value, llvm_value_dest);
		}
	}

	{
		LLVMValueRef llvm_len = LLVMConstInt(cnt->llvm_i64_type, vargsc, false);
		LLVMValueRef llvm_dest =
		    LLVMBuildStructGEP(cnt->llvm_builder, vargs->vargs_tmp->llvm_value, 0, "");
		LLVMBuildStore(cnt->llvm_builder, llvm_len, llvm_dest);

		LLVMTypeRef  llvm_ptr_type = mir_get_struct_elem_type(vargs_type, 1)->llvm_type;
		LLVMValueRef llvm_ptr =
		    vargs->arr_tmp ? vargs->arr_tmp->llvm_value : LLVMConstNull(llvm_ptr_type);
		llvm_dest =
		    LLVMBuildStructGEP(cnt->llvm_builder, vargs->vargs_tmp->llvm_value, 1, "");
		llvm_ptr = LLVMBuildBitCast(cnt->llvm_builder, llvm_ptr, llvm_ptr_type, "");
		LLVMBuildStore(cnt->llvm_builder, llvm_ptr, llvm_dest);
	}

	vargs->base.llvm_value = LLVMBuildLoad(cnt->llvm_builder, vargs->vargs_tmp->llvm_value, "");
}

void
emit_instr_toany(Context *cnt, MirInstrToAny *toany)
{
	LLVMValueRef llvm_tmp       = toany->tmp->llvm_value;
	LLVMValueRef llvm_type_info = toany->rtti_type->rtti.var->llvm_value;
	LLVMValueRef llvm_data      = toany->expr->llvm_value;

	assert(llvm_type_info && "Missing LLVM value for RTTI variable.");
	assert(llvm_tmp);

	MirType *   any_type                = mir_deref_type(toany->base.value.type);
	LLVMTypeRef llvm_any_type_info_type = mir_get_struct_elem_type(any_type, 0)->llvm_type;
	LLVMTypeRef llvm_any_data_type      = mir_get_struct_elem_type(any_type, 1)->llvm_type;

	/* use tmp for expression */
	if (toany->expr_tmp) {
		MirVar *expr_tmp = toany->expr_tmp;
		assert(expr_tmp->llvm_value && "Missing tmp variable");

		llvm_data = emit_as_const(cnt, &toany->expr->value);
		LLVMBuildStore(cnt->llvm_builder, llvm_data, expr_tmp->llvm_value);
		llvm_data = expr_tmp->llvm_value;
	}

	{ /* setup tmp variable */
		LLVMValueRef llvm_dest;

		/* pointer to type info */
		llvm_dest = LLVMBuildStructGEP(cnt->llvm_builder, llvm_tmp, 0, "");

		llvm_type_info = LLVMBuildPointerCast(
		    cnt->llvm_builder, llvm_type_info, llvm_any_type_info_type, "");

		LLVMBuildStore(cnt->llvm_builder, llvm_type_info, llvm_dest);

		/* pointer to data */
		llvm_dest = LLVMBuildStructGEP(cnt->llvm_builder, llvm_tmp, 1, "");

		llvm_data =
		    llvm_data
		        ? LLVMBuildPointerCast(cnt->llvm_builder, llvm_data, llvm_any_data_type, "")
		        : LLVMConstNull(llvm_any_data_type);

		LLVMBuildStore(cnt->llvm_builder, llvm_data, llvm_dest);
	}

	toany->base.llvm_value = llvm_tmp;
}

void
emit_instr_block(Context *cnt, MirInstrBlock *block)
{
	MirFn *fn = block->owner_fn;
	assert(fn->llvm_value);
	LLVMBasicBlockRef llvm_block = emit_basic_block(cnt, block);
	assert(llvm_block);

	LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_block);

	/* gen allocas fist in entry block!!! */
	if (fn->first_block == block) {
		emit_allocas(cnt, fn);
	}

	MirInstr *instr = block->entry_instr;
	while (instr) {
		emit_instr(cnt, instr);
		instr = instr->next;
	}
}

void
emit_allocas(Context *cnt, MirFn *fn)
{
	assert(fn);

	const char *var_name;
	LLVMTypeRef var_type;
	unsigned    var_alignment;
	MirVar *    var;

	barray_foreach(fn->variables, var)
	{
		assert(var);

		if (!var->gen_llvm) continue;

#if NAMED_VARS
		var_name = var->llvm_name;
#else
		var_name = "";
#endif

		var_type      = var->value.type->llvm_type;
		var_alignment = (unsigned int)var->value.type->alignment;

		assert(var_type);

		var->llvm_value = LLVMBuildAlloca(cnt->llvm_builder, var_type, var_name);
		LLVMSetAlignment(var->llvm_value, var_alignment);
	}
}

void
emit_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
	MirFn *fn = fn_proto->base.value.data.v_ptr.data.fn;
	/* unused function */
	if (fn->ref_count == 0) return;
	emit_fn_proto(cnt, fn);

	if (is_not_flag(fn->flags, FLAG_EXTERN)) {
		MirInstr *block = (MirInstr *)fn->first_block;

		while (block) {
			emit_instr(cnt, block);
			block = block->next;
		}
	}
}

void
emit_instr(Context *cnt, MirInstr *instr)
{
	switch (instr->kind) {
	case MIR_INSTR_INVALID:
		bl_abort("Invalid instruction");

	case MIR_INSTR_CONST:
	case MIR_INSTR_SIZEOF:
	case MIR_INSTR_ALIGNOF:
	case MIR_INSTR_DECL_VARIANT:
	case MIR_INSTR_DECL_MEMBER:
	case MIR_INSTR_TYPE_FN:
	case MIR_INSTR_TYPE_STRUCT:
	case MIR_INSTR_TYPE_PTR:
	case MIR_INSTR_TYPE_ARRAY:
	case MIR_INSTR_TYPE_SLICE:
	case MIR_INSTR_TYPE_VARGS:
	case MIR_INSTR_TYPE_ENUM:

		break;

	case MIR_INSTR_BINOP:
		emit_instr_binop(cnt, (MirInstrBinop *)instr);
		break;
	case MIR_INSTR_FN_PROTO:
		emit_instr_fn_proto(cnt, (MirInstrFnProto *)instr);
		break;
	case MIR_INSTR_BLOCK:
		emit_instr_block(cnt, (MirInstrBlock *)instr);
		break;
	case MIR_INSTR_BR:
		emit_instr_br(cnt, (MirInstrBr *)instr);
		break;
	case MIR_INSTR_COND_BR:
		emit_instr_cond_br(cnt, (MirInstrCondBr *)instr);
		break;
	case MIR_INSTR_RET:
		emit_instr_ret(cnt, (MirInstrRet *)instr);
		break;
	case MIR_INSTR_DECL_VAR:
		emit_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
		break;
	case MIR_INSTR_DECL_REF:
		emit_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
		break;
	case MIR_INSTR_LOAD:
		emit_instr_load(cnt, (MirInstrLoad *)instr);
		break;
	case MIR_INSTR_STORE:
		emit_instr_store(cnt, (MirInstrStore *)instr);
		break;
	case MIR_INSTR_CALL:
		emit_instr_call(cnt, (MirInstrCall *)instr);
		break;
	case MIR_INSTR_ARG:
		emit_instr_arg(cnt, (MirInstrArg *)instr);
		break;
	case MIR_INSTR_UNOP:
		emit_instr_unop(cnt, (MirInstrUnop *)instr);
		break;
	case MIR_INSTR_UNREACHABLE:
		emit_instr_unreachable(cnt, (MirInstrUnreachable *)instr);
		break;
	case MIR_INSTR_MEMBER_PTR:
		emit_instr_member_ptr(cnt, (MirInstrMemberPtr *)instr);
		break;
	case MIR_INSTR_ELEM_PTR:
		emit_instr_elem_ptr(cnt, (MirInstrElemPtr *)instr);
		break;
	case MIR_INSTR_ADDROF:
		emit_instr_addrof(cnt, (MirInstrAddrOf *)instr);
		break;
	case MIR_INSTR_CAST:
		emit_instr_cast(cnt, (MirInstrCast *)instr);
		break;
	case MIR_INSTR_VARGS:
		emit_instr_vargs(cnt, (MirInstrVArgs *)instr);
		break;
	case MIR_INSTR_TYPE_INFO:
		emit_instr_type_info(cnt, (MirInstrTypeInfo *)instr);
		break;
	case MIR_INSTR_PHI:
		emit_instr_phi(cnt, (MirInstrPhi *)instr);
		break;
	case MIR_INSTR_COMPOUND:
		emit_instr_compound(cnt, NULL, (MirInstrCompound *)instr);
		break;
	case MIR_INSTR_TOANY:
		emit_instr_toany(cnt, (MirInstrToAny *)instr);
		break;
	}
}

static void
init_DI(Context *cnt)
{
	cnt->llvm_dibuilder = llvm_di_new_di_builder(cnt->llvm_module);
	cnt->assembly->mir_module->llvm_compile_unit =
	    llvm_di_create_assembly(cnt->llvm_dibuilder, cnt->assembly);
}

static void
terminate_DI(Context *cnt)
{
	llvm_di_builder_finalize(cnt->llvm_dibuilder);
	llvm_di_delete_di_builder(cnt->llvm_dibuilder);
}

/* public */
void
ir_run(Builder *builder, Assembly *assembly)
{
	Context cnt;
	memset(&cnt, 0, sizeof(Context));
	cnt.builder                = builder;
	cnt.assembly               = assembly;
	cnt.gstring_cache          = bo_htbl_new(sizeof(LLVMValueRef), 1024);
	cnt.llvm_cnt               = assembly->mir_module->llvm_cnt;
	cnt.llvm_module            = assembly->mir_module->llvm_module;
	cnt.llvm_td                = assembly->mir_module->llvm_td;
	cnt.llvm_builder           = LLVMCreateBuilderInContext(assembly->mir_module->llvm_cnt);
	cnt.llvm_void_type         = LLVMVoidTypeInContext(cnt.llvm_cnt);
	cnt.llvm_i1_type           = LLVMInt1TypeInContext(cnt.llvm_cnt);
	cnt.llvm_i8_type           = LLVMInt8TypeInContext(cnt.llvm_cnt);
	cnt.llvm_i32_type          = LLVMInt32TypeInContext(cnt.llvm_cnt);
	cnt.llvm_i64_type          = LLVMInt64TypeInContext(cnt.llvm_cnt);
	cnt.llvm_i8_ptr_type       = LLVMPointerType(cnt.llvm_i8_type, 0);
	cnt.llvm_const_i64         = LLVMConstInt(cnt.llvm_i64_type, 0, false);
	cnt.llvm_instrinsic_trap   = create_trap_fn(&cnt);
	cnt.llvm_instrinsic_memset = create_memset_fn(&cnt);
	cnt.llvm_intrinsic_memcpy  = create_memcpy_fn(&cnt);
	cnt.debug_build            = is_flag(builder->flags, BUILDER_DEBUG_BUILD);

	if (cnt.debug_build) init_DI(&cnt);

	emit_RTTI_types(&cnt);

	MirInstr *ginstr;
	barray_foreach(assembly->mir_module->global_instrs, ginstr)
	{
		emit_instr(&cnt, ginstr);
	}

	if (cnt.debug_build) terminate_DI(&cnt);

#if BL_DEBUG
	char *error = NULL;
	if (LLVMVerifyModule(cnt.llvm_module, LLVMReturnStatusAction, &error)) {
		msg_error("LLVM module not verified with error: %s", error);
	}
	LLVMDisposeMessage(error);
#endif

	LLVMDisposeBuilder(cnt.llvm_builder);
	bo_unref(cnt.gstring_cache);
}
