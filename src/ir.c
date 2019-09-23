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
#include "llvm_api.h"
#include "llvm_di.h"
#include "mir.h"
#include "unit.h"

#define LLVM_INSTRINSIC_TRAP "llvm.debugtrap"
#define LLVM_INSTRINSIC_MEMSET "llvm.memset.p0i8.i64"
#define LLVM_INTRINSIC_MEMCPY "llvm.memcpy.p0i8.p0i8.i64"

#if BL_DEBUG
#define NAMED_VARS false
#else
#define NAMED_VARS false
#endif

SmallArrayType(LLVMValue, LLVMValueRef, 32);
SmallArrayType(LLVMValue64, LLVMValueRef, 64);
SmallArrayType(LLVMType, LLVMTypeRef, 32);

typedef struct {
	Assembly *  assembly;
	BHashTable *gstring_cache;

	LLVMContextRef    llvm_cnt;
	LLVMModuleRef     llvm_module;
	LLVMTargetDataRef llvm_td;
	LLVMBuilderRef    llvm_builder;
	LLVMDIBuilderRef  llvm_di_builder;

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

	bool debug_mode;
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
emit_DI_instr_loc(Context *cnt, MirInstr *instr);

static void
emit_DI_fn(Context *cnt, MirFn *fn);

static void
emit_DI_var(Context *cnt, MirVar *var);

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
emit_instr_decl_direct_ref(Context *cnt, MirInstrDeclDirectRef *ref);

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

/* PERFORMANCE: We create implicit wrapper for all externals containing at least one argument
 * passing structure by value. We try to split the passed structure into 8B registers, as require C
 * ABI call convetion. */
static LLVMValueRef
emit_extern_wrapper_fn(Context *cnt, MirFn *fn)
{
	fn->llvm_value = LLVMAddFunction(cnt->llvm_module, fn->linkage_name, fn->type->llvm_type);

	/* Allways inline this function call */
	LLVMAttributeRef llvm_attr =
	    llvm_create_attribute(cnt->llvm_cnt, LLVM_ATTRIBUTE_ALWAYSINLINE);
	LLVMAddAttributeAtIndex(fn->llvm_value, (unsigned)LLVMAttributeFunctionIndex, llvm_attr);

	BL_ASSERT(fn->type->data.fn.args &&
	          "No need to generate wrapper for external function without arguments.");

	/* Build external fn definition. */
	SmallArray_LLVMType llvm_arg_types;
	sa_init(&llvm_arg_types);
	LLVMTypeRef llvm_ret_type = fn->type->data.fn.ret_type->llvm_type;
	BL_ASSERT(llvm_ret_type && "Missing external function return type.");

	SmallArray_ArgPtr *args = fn->type->data.fn.args;
	MirArg *           arg;
	SARRAY_FOREACH(args, arg)
	{
		switch (arg->llvm_easgm) {
		case LLVM_EASGM_8:
			sa_push_LLVMType(&llvm_arg_types, LLVMInt8TypeInContext(cnt->llvm_cnt));
			break;
		case LLVM_EASGM_16:
			sa_push_LLVMType(&llvm_arg_types, LLVMInt16TypeInContext(cnt->llvm_cnt));
			break;
		case LLVM_EASGM_32:
			sa_push_LLVMType(&llvm_arg_types, LLVMInt32TypeInContext(cnt->llvm_cnt));
			break;
		case LLVM_EASGM_64:
			sa_push_LLVMType(&llvm_arg_types, LLVMInt64TypeInContext(cnt->llvm_cnt));
			break;
		case LLVM_EASGM_64_8:
			sa_push_LLVMType(&llvm_arg_types, LLVMInt64TypeInContext(cnt->llvm_cnt));
			sa_push_LLVMType(&llvm_arg_types, LLVMInt8TypeInContext(cnt->llvm_cnt));
			break;
		case LLVM_EASGM_64_16:
			sa_push_LLVMType(&llvm_arg_types, LLVMInt64TypeInContext(cnt->llvm_cnt));
			sa_push_LLVMType(&llvm_arg_types, LLVMInt16TypeInContext(cnt->llvm_cnt));
			break;
		case LLVM_EASGM_64_32:
			sa_push_LLVMType(&llvm_arg_types, LLVMInt64TypeInContext(cnt->llvm_cnt));
			sa_push_LLVMType(&llvm_arg_types, LLVMInt32TypeInContext(cnt->llvm_cnt));
			break;
		case LLVM_EASGM_64_64:
			sa_push_LLVMType(&llvm_arg_types, LLVMInt64TypeInContext(cnt->llvm_cnt));
			sa_push_LLVMType(&llvm_arg_types, LLVMInt64TypeInContext(cnt->llvm_cnt));
			break;
		case LLVM_EASGM_BYVAL: {
			sa_push_LLVMType(&llvm_arg_types, LLVMPointerType(arg->type->llvm_type, 0));
			break;
		}

		case LLVM_EASGM_NONE:
			sa_push_LLVMType(&llvm_arg_types, arg->type->llvm_type);
			break;
		}
	}

	LLVMTypeRef llvm_fn_type =
	    LLVMFunctionType(llvm_ret_type, llvm_arg_types.data, llvm_arg_types.size, false);
	LLVMValueRef llvm_orig_fn =
	    LLVMAddFunction(cnt->llvm_module, fn->linkage_orig_name, llvm_fn_type);

	/* Generate wrapper body. User code will call this wrapper instead of direct call to the
	 * external function, wrapper must do the conversion of passed structure to expected EASGM
	 * configuration to provide compatibility with C call convetions. I'm not sure if this is
	 * needed on other platforms or it's valid only for UNIX with SystemV. */

	/* Prepare main block. */
	LLVMBasicBlockRef llvm_block      = LLVMAppendBasicBlock(fn->llvm_value, "entry");
	LLVMBasicBlockRef llvm_prev_block = LLVMGetInsertBlock(cnt->llvm_builder);
	LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_block);

	bool has_byval         = false;
	bool does_return_value = fn->type->data.fn.ret_type->kind != MIR_TYPE_VOID;

	/* Build extern fn call args */
	SmallArray_LLVMValue llvm_args;
	sa_init(&llvm_args);
	SARRAY_FOREACH(args, arg)
	{
		switch (arg->llvm_easgm) {
		case LLVM_EASGM_8:
		case LLVM_EASGM_16:
		case LLVM_EASGM_32:
		case LLVM_EASGM_64: {
			LLVMValueRef llvm_struct_tmp =
			    LLVMBuildAlloca(cnt->llvm_builder, arg->type->llvm_type, "");
			LLVMValueRef llvm_tmp =
			    LLVMBuildAlloca(cnt->llvm_builder, llvm_arg_types.data[i], "");

			LLVMBuildStore(
			    cnt->llvm_builder, LLVMGetParam(fn->llvm_value, i), llvm_struct_tmp);

			LLVMValueRef llvm_tmp_size =
			    LLVMConstInt(cnt->llvm_i64_type,
			                 LLVMStoreSizeOfType(cnt->llvm_td, llvm_arg_types.data[i]),
			                 false);

			LLVMValueRef llvm_tmp_alig = LLVMConstInt(
			    cnt->llvm_i64_type,
			    LLVMABIAlignmentOfType(cnt->llvm_td, llvm_arg_types.data[i]),
			    false);

			build_call_memcpy(
			    cnt, llvm_tmp, llvm_struct_tmp, llvm_tmp_size, llvm_tmp_alig);

			sa_push_LLVMValue(&llvm_args,
			                  LLVMBuildLoad(cnt->llvm_builder, llvm_tmp, ""));
			break;
		}

		case LLVM_EASGM_64_8:
		case LLVM_EASGM_64_16:
		case LLVM_EASGM_64_32:
		case LLVM_EASGM_64_64: {
			LLVMValueRef llvm_struct_tmp =
			    LLVMBuildAlloca(cnt->llvm_builder, arg->type->llvm_type, "");

			LLVMTypeRef llvm_tmp_type = LLVMStructTypeInContext(
			    cnt->llvm_cnt, &llvm_arg_types.data[i], 2, false);
			LLVMValueRef llvm_tmp =
			    LLVMBuildAlloca(cnt->llvm_builder, llvm_tmp_type, "");

			LLVMBuildStore(
			    cnt->llvm_builder, LLVMGetParam(fn->llvm_value, i), llvm_struct_tmp);

			LLVMValueRef llvm_tmp_size =
			    LLVMConstInt(cnt->llvm_i64_type,
			                 LLVMStoreSizeOfType(cnt->llvm_td, llvm_tmp_type),
			                 false);

			LLVMValueRef llvm_tmp_alig =
			    LLVMConstInt(cnt->llvm_i64_type,
			                 LLVMABIAlignmentOfType(cnt->llvm_td, llvm_tmp_type),
			                 false);

			build_call_memcpy(
			    cnt, llvm_tmp, llvm_struct_tmp, llvm_tmp_size, llvm_tmp_alig);

			LLVMValueRef llvm_tmp_1 =
			    LLVMBuildStructGEP(cnt->llvm_builder, llvm_tmp, 0, "");
			llvm_tmp_1 = LLVMBuildLoad(cnt->llvm_builder, llvm_tmp_1, "");

			LLVMValueRef llvm_tmp_2 =
			    LLVMBuildStructGEP(cnt->llvm_builder, llvm_tmp, 1, "");
			llvm_tmp_2 = LLVMBuildLoad(cnt->llvm_builder, llvm_tmp_2, "");

			sa_push_LLVMValue(&llvm_args, llvm_tmp_1);
			sa_push_LLVMValue(&llvm_args, llvm_tmp_2);
			break;
		}

		case LLVM_EASGM_BYVAL: {
			has_byval = has_byval ? has_byval : true;
			LLVMValueRef llvm_struct_tmp =
			    LLVMBuildAlloca(cnt->llvm_builder, arg->type->llvm_type, "");
			LLVMBuildStore(
			    cnt->llvm_builder, LLVMGetParam(fn->llvm_value, i), llvm_struct_tmp);
			sa_push_LLVMValue(&llvm_args, llvm_struct_tmp);

			break;
		}

		case LLVM_EASGM_NONE: {
			sa_push_LLVMValue(&llvm_args, LLVMGetParam(fn->llvm_value, i));
			break;
		}
		}
	}

	LLVMValueRef llvm_call =
	    LLVMBuildCall(cnt->llvm_builder, llvm_orig_fn, llvm_args.data, llvm_args.size, "");

	/* PERFORMANCE: LLVM shitty stuff, we cannot set callside attributes before call is created.
	 */
	if (has_byval) {
		SARRAY_FOREACH(args, arg)
		{
			if (arg->llvm_easgm != LLVM_EASGM_BYVAL) continue;
			/* Setup attributes. */
			LLVMAttributeRef llvm_attr = llvm_create_attribute_type(
			    cnt->llvm_cnt, LLVM_ATTRIBUTE_BYVAL, arg->type->llvm_type);

			LLVMAddCallSiteAttribute(llvm_call, (unsigned)i + 1, llvm_attr);

			/* Setup attributes. */
			llvm_attr = llvm_create_attribute_type(
			    cnt->llvm_cnt, LLVM_ATTRIBUTE_BYVAL, arg->type->llvm_type);

			LLVMAddAttributeAtIndex(llvm_orig_fn, (unsigned)i + 1, llvm_attr);
		}
	}

	if (does_return_value) {
		LLVMBuildRet(cnt->llvm_builder, llvm_call);
	} else {
		LLVMBuildRetVoid(cnt->llvm_builder);
	}

	sa_terminate(&llvm_args);
	sa_terminate(&llvm_arg_types);

	LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_prev_block);
	return fn->llvm_value;
}

static inline LLVMValueRef
emit_fn_proto(Context *cnt, MirFn *fn)
{
	BL_ASSERT(fn)
	if (!fn->emit_llvm) return NULL;

	fn->llvm_value = LLVMGetNamedFunction(cnt->llvm_module, fn->linkage_name);
	if (fn->llvm_value) return fn->llvm_value;

	if (fn->llvm_extern_wrap) {
		return emit_extern_wrapper_fn(cnt, fn);
	} else {
		fn->llvm_value =
		    LLVMAddFunction(cnt->llvm_module, fn->linkage_name, fn->type->llvm_type);
	}

	if (IS_FLAG(fn->flags, FLAG_INLINE)) {
		LLVMAttributeRef llvm_attr =
		    llvm_create_attribute(cnt->llvm_cnt, LLVM_ATTRIBUTE_ALWAYSINLINE);

		LLVMAddAttributeAtIndex(
		    fn->llvm_value, (unsigned)LLVMAttributeFunctionIndex, llvm_attr);
	}

	if (IS_FLAG(fn->flags, FLAG_NO_INLINE)) {
		LLVMAttributeRef llvm_attr =
		    llvm_create_attribute(cnt->llvm_cnt, LLVM_ATTRIBUTE_NOINLINE);

		LLVMAddAttributeAtIndex(
		    fn->llvm_value, (unsigned)LLVMAttributeFunctionIndex, llvm_attr);
	}

	return fn->llvm_value;
}

static inline LLVMValueRef
emit_global_var_proto(Context *cnt, MirVar *var)
{
	BL_ASSERT(var)
	if (var->llvm_value) return var->llvm_value;

	LLVMTypeRef llvm_type = var->value.type->llvm_type;
	var->llvm_value       = LLVMAddGlobal(cnt->llvm_module, llvm_type, var->llvm_name);

	LLVMSetGlobalConstant(var->llvm_value, !var->is_mutable);

	/* Linkage should be later set by user. */
	LLVMSetLinkage(var->llvm_value, LLVMPrivateLinkage);
	LLVMSetAlignment(var->llvm_value, (unsigned)var->value.type->alignment);

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
	BL_ASSERT(value)
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
emit_DI_instr_loc(Context *cnt, MirInstr *instr)
{
	if (!instr) {
		llvm_di_reset_current_location(cnt->llvm_builder);
		return;
	}

	if (instr->node) {
		LLVMMetadataRef llvm_scope = instr->node->owner_scope->llvm_di_meta;
		Location *      location   = instr->node->location;
		llvm_di_set_current_location(cnt->llvm_builder,
		                             (unsigned)location->line,
		                             (unsigned)location->col,
		                             llvm_scope,
		                             false);
	}
}

void
emit_DI_fn(Context *cnt, MirFn *fn)
{
	if (!fn->decl_node) return;

	Location *      location   = fn->decl_node->location;
	LLVMMetadataRef llvm_file  = location->unit->llvm_file_meta;
	LLVMMetadataRef llvm_scope = fn->decl_node->owner_scope->llvm_di_meta
	                                 ? fn->decl_node->owner_scope->llvm_di_meta
	                                 : llvm_file;

	LLVMMetadataRef tmp = llvm_di_create_fn(cnt->llvm_di_builder,
	                                        llvm_scope,
	                                        fn->id ? fn->id->str : fn->linkage_name,
	                                        fn->linkage_name,
	                                        llvm_file,
	                                        (unsigned)location->line,
	                                        fn->type->llvm_meta,
	                                        (unsigned)location->line);

	fn->body_scope->llvm_di_meta =
	    llvm_di_replace_temporary(cnt->llvm_di_builder, fn->body_scope->llvm_di_meta, tmp);

	llvm_di_set_subprogram(fn->llvm_value, fn->body_scope->llvm_di_meta);
}

void
emit_DI_var(Context *cnt, MirVar *var)
{
	if (!var->decl_node) return;

	Location *      location   = var->decl_node->location;
	LLVMMetadataRef llvm_file  = location->unit->llvm_file_meta;
	LLVMMetadataRef llvm_scope = var->decl_node->owner_scope->llvm_di_meta
	                                 ? var->decl_node->owner_scope->llvm_di_meta
	                                 : llvm_file;

	if (var->is_in_gscope) {
		llvm_di_set_current_location(cnt->llvm_builder,
		                             (unsigned)location->line,
		                             (unsigned)location->col,
		                             llvm_scope,
		                             false);

		LLVMMetadataRef llvm_meta =
		    llvm_di_create_global_variable_expression(cnt->llvm_di_builder,
		                                              llvm_scope,
		                                              var->id->str,
		                                              llvm_file,
		                                              (unsigned)location->line,
		                                              var->value.type->llvm_meta);

		LLVMGlobalSetMetadata(var->llvm_value, 0, llvm_meta);

	} else {
		LLVMMetadataRef llvm_meta =
		    llvm_di_create_auto_variable(cnt->llvm_di_builder,
		                                 llvm_scope,
		                                 var->id->str,
		                                 llvm_file,
		                                 (unsigned)location->line,
		                                 var->value.type->llvm_meta);

		llvm_di_insert_declare(cnt->llvm_di_builder,
		                       var->llvm_value,
		                       llvm_meta,
		                       (unsigned)location->line,
		                       (unsigned)location->col,
		                       llvm_scope,
		                       LLVMGetInsertBlock(cnt->llvm_builder));
	}
}

void
emit_RTTI_types(Context *cnt)
{
	BArray *table = cnt->assembly->MIR.RTTI_tmp_vars;
	BL_ASSERT(table)

	MirVar *     var;
	LLVMValueRef llvm_var, llvm_value;
	LLVMTypeRef  llvm_var_type;

	const size_t count = bo_array_size(table);

	for (size_t i = 0; i < count; ++i) {
		var = bo_array_at(table, i, MirVar *);
		BL_ASSERT(var)

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
	BL_ASSERT(entry)

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
		BL_UNIMPLEMENTED;
	}

	BL_ASSERT(ref->base.llvm_value)
}

void
emit_instr_decl_direct_ref(Context *cnt, MirInstrDeclDirectRef *ref)
{
	BL_ASSERT(ref->ref && ref->ref->kind == MIR_INSTR_DECL_VAR)

	MirVar *var = ((MirInstrDeclVar *)ref->ref)->var;
	BL_ASSERT(var)

	ref->base.llvm_value = var->llvm_value;
	BL_ASSERT(ref->base.llvm_value)
}

void
emit_instr_phi(Context *cnt, MirInstrPhi *phi)
{
	LLVMValueRef llvm_phi =
	    LLVMBuildPhi(cnt->llvm_builder, phi->base.value.type->llvm_type, "");

	const size_t count = phi->incoming_blocks->size;

	SmallArray_LLVMValue llvm_iv;
	SmallArray_LLVMValue llvm_ib;

	sa_init(&llvm_iv);
	sa_init(&llvm_ib);

	MirInstr *     value;
	MirInstrBlock *block;
	for (size_t i = 0; i < count; ++i) {
		value = phi->incoming_values->data[i];
		block = (MirInstrBlock *)phi->incoming_blocks->data[i];

		sa_push_LLVMValue(&llvm_iv, fetch_value(cnt, value));
		sa_push_LLVMValue(&llvm_ib, LLVMBasicBlockAsValue(emit_basic_block(cnt, block)));
	}

	LLVMAddIncoming(
	    llvm_phi, llvm_iv.data, (LLVMBasicBlockRef *)llvm_ib.data, (unsigned int)count);

	sa_terminate(&llvm_iv);
	sa_terminate(&llvm_ib);

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
	BL_ASSERT(type)
	BL_ASSERT(type->rtti.var)

	LLVMValueRef llvm_var = type->rtti.var->llvm_value;
	BL_ASSERT(llvm_var && "Missing LLVM value for RTTI variable.")

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
	BL_ASSERT(llvm_src && llvm_dest_type)

	switch (cast->op) {
	case MIR_CAST_NONE:
		cast->base.llvm_value = llvm_src;
		return;
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
		BL_ABORT("invalid cast type")
	}

	cast->base.llvm_value =
	    LLVMBuildCast(cnt->llvm_builder, llvm_op, llvm_src, llvm_dest_type, "");
}

void
emit_instr_addrof(Context *cnt, MirInstrAddrOf *addrof)
{
	addrof->base.llvm_value = addrof->src->llvm_value;
	BL_ASSERT(addrof->base.llvm_value)
}

void
emit_instr_arg(Context *cnt, MirInstrArg *arg)
{
	MirFn *fn = arg->base.owner_block->owner_fn;
	BL_ASSERT(fn)
	LLVMValueRef llvm_fn = fn->llvm_value;
	BL_ASSERT(llvm_fn);

	arg->base.llvm_value = LLVMGetParam(llvm_fn, arg->i);
}

void
emit_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
	LLVMValueRef llvm_arr_ptr = fetch_value(cnt, elem_ptr->arr_ptr);
	LLVMValueRef llvm_index   = fetch_value(cnt, elem_ptr->index);
	BL_ASSERT(llvm_arr_ptr && llvm_index)

	if (elem_ptr->target_is_slice) {
		/* special case for slices */
		llvm_arr_ptr = LLVMBuildStructGEP(cnt->llvm_builder, llvm_arr_ptr, 1, "");
		llvm_arr_ptr = LLVMBuildLoad(cnt->llvm_builder, llvm_arr_ptr, "");
		BL_ASSERT(llvm_arr_ptr)

		LLVMValueRef llvm_indices[1];
		llvm_indices[0] = llvm_index;

		elem_ptr->base.llvm_value = LLVMBuildInBoundsGEP(
		    cnt->llvm_builder, llvm_arr_ptr, llvm_indices, ARRAY_SIZE(llvm_indices), "");

		return;
	}

	LLVMValueRef llvm_indices[2];
	llvm_indices[0] = cnt->llvm_const_i64;
	llvm_indices[1] = llvm_index;

	elem_ptr->base.llvm_value = LLVMBuildGEP(
	    cnt->llvm_builder, llvm_arr_ptr, llvm_indices, ARRAY_SIZE(llvm_indices), "");
}

void
emit_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
	LLVMValueRef llvm_target_ptr = fetch_value(cnt, member_ptr->target_ptr);
	BL_ASSERT(llvm_target_ptr)

	if (member_ptr->builtin_id == MIR_BUILTIN_ID_NONE) {
		BL_ASSERT(member_ptr->scope_entry->kind == SCOPE_ENTRY_MEMBER)
		MirMember *member = member_ptr->scope_entry->data.member;
		BL_ASSERT(member)

		const unsigned int index =
		    (const unsigned int)member_ptr->scope_entry->data.member->index;

		member_ptr->base.llvm_value =
		    LLVMBuildStructGEP(cnt->llvm_builder, llvm_target_ptr, index, "");
		BL_ASSERT(member_ptr->base.llvm_value)
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
	BL_ASSERT(load->base.value.type && "invalid type of load instruction")
	LLVMValueRef   llvm_src  = fetch_value(cnt, load->src);
	const unsigned alignment = (const unsigned)load->base.value.type->alignment;
	BL_ASSERT(llvm_src);

	load->base.llvm_value = LLVMBuildLoad(cnt->llvm_builder, llvm_src, "");
	LLVMSetAlignment(load->base.llvm_value, alignment);
}

LLVMValueRef
emit_global_string_ptr(Context *cnt, const char *str, size_t len)
{
	BL_ASSERT(str && len)
	u64           hash  = bo_hash_from_str(str);
	bo_iterator_t found = bo_htbl_find(cnt->gstring_cache, hash);
	bo_iterator_t end   = bo_htbl_end(cnt->gstring_cache);

	if (!bo_iterator_equal(&found, &end)) {
		return bo_htbl_iter_peek_value(cnt->gstring_cache, &found, LLVMValueRef);
	}

	/* Generate global string constant */
	LLVMValueRef llvm_str = NULL;
	{
		BL_ASSERT(len && "String must be zero terminated")
		LLVMTypeRef llvm_str_arr_type =
		    LLVMArrayType(cnt->llvm_i8_type, (unsigned int)len + 1);
		llvm_str = LLVMAddGlobal(cnt->llvm_module, llvm_str_arr_type, ".str");

		SmallArray_LLVMValue64 llvm_chars;
		sa_init(&llvm_chars);

		for (size_t i = 0; i < len + 1; ++i) {
			sa_push_LLVMValue64(&llvm_chars,
			                    LLVMConstInt(cnt->llvm_i8_type, str[i], true));
		}

		LLVMValueRef llvm_str_arr =
		    LLVMConstArray(cnt->llvm_i8_type, llvm_chars.data, (unsigned int)len + 1);
		LLVMSetInitializer(llvm_str, llvm_str_arr);
		LLVMSetLinkage(llvm_str, LLVMPrivateLinkage);
		LLVMSetGlobalConstant(llvm_str, true);
		LLVMSetAlignment(llvm_str, LLVMABIAlignmentOfType(cnt->llvm_td, llvm_str_arr_type));
		LLVMSetUnnamedAddr(llvm_str, true);
		llvm_str = LLVMConstBitCast(llvm_str, cnt->llvm_i8_ptr_type);

		sa_terminate(&llvm_chars);
	}

	bo_htbl_insert(cnt->gstring_cache, hash, llvm_str);
	return llvm_str;
}

LLVMValueRef
emit_as_const(Context *cnt, MirConstValue *value)
{
	MirType *type = value->type;
	BL_ASSERT(type)
	LLVMTypeRef  llvm_type  = type->llvm_type;
	LLVMValueRef llvm_value = NULL;
	BL_ASSERT(llvm_type)

	switch (type->kind) {
	case MIR_TYPE_INT: {
		llvm_value =
		    LLVMConstInt(llvm_type, value->data.v_u64, type->data.integer.is_signed);
		break;
	}

	case MIR_TYPE_REAL: {
		const size_t size = type->store_size_bytes;

		if (size == sizeof(f32)) { // float
			llvm_value = LLVMConstReal(llvm_type, (f64)value->data.v_f32);
			break;
		} else if (size == sizeof(f64)) { // double
			llvm_value = LLVMConstReal(llvm_type, value->data.v_f64);
			break;
		}
		BL_ABORT("invalid floating point type")
	}

	case MIR_TYPE_BOOL:
		llvm_value = LLVMConstInt(llvm_type, (unsigned long long)value->data.v_s32, false);
		break;

	case MIR_TYPE_NULL:
		BL_ASSERT(value->data.v_ptr.data.any == NULL)
		llvm_value = LLVMConstNull(llvm_type);
		break;

	case MIR_TYPE_PTR: {
		type = mir_deref_type(type);
		BL_ASSERT(type)

		if (type->kind == MIR_TYPE_FN) {
			/* Constant pointer to the function. Value must contains pointer to MirFn
			 * instance! */
			MirFn *fn = value->data.v_ptr.data.any
			                ? value->data.v_ptr.data.value->data.v_ptr.data.fn
			                : NULL;
			BL_ASSERT(fn && "Function pointer not set for compile time known constant "
			                "pointer to function.")

			llvm_value = emit_fn_proto(cnt, fn);
			BL_ASSERT(llvm_value)
			break;
		} else {
			switch (value->data.v_ptr.kind) {
			case MIR_CP_VAR: {
				/* value must contains pointer to constant variable */
				MirVar *pointed = value->data.v_ptr.data.var;
				BL_ASSERT(pointed && pointed->llvm_value &&
				          "Invalid const pointer to variable.")

				llvm_value = pointed->llvm_value;
				break;
			}

			default: {
				/* Only null constants are allowed here */
				BL_ASSERT(
				    value->data.v_ptr.data.any == NULL &&
				    "Only pointers to fn and var can be generated as constants in "
				    "LLVM IR.")
				llvm_value = LLVMConstNull(llvm_type);
			}
			}

			break;
		}
	}

	case MIR_TYPE_ARRAY: {
		if (value->data.v_array.is_zero_initializer) {
			llvm_value = LLVMConstNull(llvm_type);
			break;
		}

		const size_t len            = (size_t)type->data.array.len;
		LLVMTypeRef  llvm_elem_type = type->data.array.elem_type->llvm_type;
		BL_ASSERT(len && llvm_elem_type)

		SmallArray_ConstValuePtr *elems = value->data.v_array.elems;
		MirConstValue *           elem;

		BL_ASSERT(elems)
		BL_ASSERT(len == elems->size)

		SmallArray_LLVMValue llvm_elems;
		sa_init(&llvm_elems);

		for (size_t i = 0; i < len; ++i) {
			elem = elems->data[i];
			sa_push_LLVMValue(&llvm_elems, emit_as_const(cnt, elem));
		}

		llvm_value = LLVMConstArray(llvm_elem_type, llvm_elems.data, (unsigned int)len);
		sa_terminate(&llvm_elems);
		break;
	}

	case MIR_TYPE_STRING: {
		if (value->data.v_struct.is_zero_initializer) {
			llvm_value = LLVMConstNull(llvm_type);
			break;
		}

		SmallArray_ConstValuePtr *members = value->data.v_struct.members;
		const size_t              memc    = members->size;
		BL_ASSERT(members)
		BL_ASSERT(memc == 2 && "not slice string?")

		MirConstValue *len_value = members->data[0];
		MirConstValue *str_value = members->data[1];
		BL_ASSERT(len_value && str_value)

		const u64   len = len_value->data.v_u64;
		const char *str = str_value->data.v_ptr.data.str;
		BL_ASSERT(str)

		LLVMValueRef const_vals[2];
		const_vals[0] = LLVMConstInt(len_value->type->llvm_type, len, false);
		const_vals[1] = emit_global_string_ptr(cnt, str, len);

		llvm_value = LLVMConstNamedStruct(llvm_type, const_vals, 2);
		break;
	}

	case MIR_TYPE_SLICE:
	case MIR_TYPE_VARGS:
	case MIR_TYPE_STRUCT: {
		if (value->data.v_struct.is_zero_initializer) {
			llvm_value = LLVMConstNull(llvm_type);
			break;
		}

		SmallArray_ConstValuePtr *members = value->data.v_struct.members;
		BL_ASSERT(members && "Missing struct members.")
		const size_t memc = members->size;

		MirConstValue *member;

		SmallArray_LLVMValue llvm_members;
		sa_init(&llvm_members);

		SARRAY_FOREACH(members, member)
		sa_push_LLVMValue(&llvm_members, emit_as_const(cnt, member));

		llvm_value = LLVMConstNamedStruct(llvm_type, llvm_members.data, (unsigned int)memc);
		sa_terminate(&llvm_members);
		break;
	}

	case MIR_TYPE_ENUM: {
		LLVMTypeRef llvm_base_type = value->type->llvm_type;

		llvm_value = LLVMConstInt(
		    llvm_base_type, value->data.v_u64, value->type->data.integer.is_signed);
		break;
	}

	default:
		BL_UNIMPLEMENTED;
	}

	BL_ASSERT(llvm_value)
	return llvm_value;
}

void
emit_instr_store(Context *cnt, MirInstrStore *store)
{
	LLVMValueRef   val       = fetch_value(cnt, store->src);
	LLVMValueRef   ptr       = fetch_value(cnt, store->dest);
	const unsigned alignment = (unsigned)store->src->value.type->alignment;
	BL_ASSERT(val && ptr)

	if (cnt->debug_mode) emit_DI_instr_loc(cnt, &store->base);

	store->base.llvm_value = LLVMBuildStore(cnt->llvm_builder, val, ptr);
	LLVMSetAlignment(store->base.llvm_value, alignment);
}

void
emit_instr_unop(Context *cnt, MirInstrUnop *unop)
{
	LLVMValueRef llvm_val = fetch_value(cnt, unop->expr);
	BL_ASSERT(llvm_val)

	LLVMTypeKind lhs_kind   = LLVMGetTypeKind(LLVMTypeOf(llvm_val));
	const bool   float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

	if (cnt->debug_mode) emit_DI_instr_loc(cnt, &unop->base);

	switch (unop->op) {
	case UNOP_NOT: {
		BL_ASSERT(!float_kind && "Invalid negation of floating point type.")
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
		BL_UNIMPLEMENTED;
	}
}

void
emit_instr_compound(Context *cnt, MirVar *_tmp_var, MirInstrCompound *cmp)
{
	if (cmp->base.comptime && !_tmp_var) {
		cmp->base.llvm_value = emit_as_const(cnt, &cmp->base.value);
		return;
	}

	/*
	 * Temporary variable for naked compounds is implicitly generated variable. When compound is
	 * used for variable initialization, variable's allocated memory is used directly and MirVar
	 * must be passed in parameters.
	 */
	MirVar *tmp_var = _tmp_var ? _tmp_var : cmp->tmp_var;

	BL_ASSERT(tmp_var && "Missing temporary variable")

	LLVMValueRef llvm_tmp = tmp_var->llvm_value;
	BL_ASSERT(llvm_tmp)

	MirType *type = tmp_var->value.type;
	BL_ASSERT(type)

	/*
	 * Initializer variants:
	 * 1) Zero initialization - Compound is initialized to 0 by memset intrinsic.
	 * 2) Constant            - Compound is compile time known constant.
	 * 3) Runtime             - One or more compound members are known in runtime only.
	 */

	LLVMValueRef llvm_size = LLVMConstInt(cnt->llvm_i64_type, type->store_size_bytes, false);
	LLVMValueRef llvm_alignment =
	    LLVMConstInt(cnt->llvm_i32_type, (unsigned)type->alignment, true);

	if (cmp->is_zero_initialized) {
		/* zero initialized */
		build_call_memset_0(cnt, llvm_tmp, llvm_size, llvm_alignment);
	} else if (cmp->base.comptime) {
		/* compile time known */
		LLVMTypeRef llvm_type = type->llvm_type;
		BL_ASSERT(llvm_type)
		LLVMValueRef llvm_const = LLVMAddGlobal(cnt->llvm_module, llvm_type, "");
		LLVMSetGlobalConstant(llvm_const, true);
		LLVMSetLinkage(llvm_const, LLVMPrivateLinkage);
		LLVMSetAlignment(llvm_const, (unsigned)type->alignment);
		LLVMSetInitializer(llvm_const, fetch_value(cnt, &cmp->base));

		build_call_memcpy(cnt, llvm_tmp, llvm_const, llvm_size, llvm_alignment);
	} else {
		SmallArray_InstrPtr *values = cmp->values;
		MirInstr *           value;
		LLVMValueRef         llvm_value;
		LLVMValueRef         llvm_value_dest;
		LLVMValueRef         llvm_indices[2];
		llvm_indices[0] = cnt->llvm_const_i64;

		SARRAY_FOREACH(values, value)
		{
			llvm_value = fetch_value(cnt, value);
			BL_ASSERT(llvm_value)

			switch (type->kind) {
			case MIR_TYPE_ARRAY:
				llvm_indices[1] = LLVMConstInt(cnt->llvm_i64_type, i, true);
				llvm_value_dest = LLVMBuildGEP(cnt->llvm_builder,
				                               llvm_tmp,
				                               llvm_indices,
				                               ARRAY_SIZE(llvm_indices),
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
				BL_ASSERT(i == 0)
				llvm_value_dest = llvm_tmp;
				break;
			}

			LLVMBuildStore(cnt->llvm_builder, llvm_value, llvm_value_dest);
		}

		cmp->base.llvm_value = LLVMBuildLoad(cnt->llvm_builder, llvm_tmp, "");
	}
}

void
emit_instr_binop(Context *cnt, MirInstrBinop *binop)
{
	LLVMValueRef lhs = fetch_value(cnt, binop->lhs);
	LLVMValueRef rhs = fetch_value(cnt, binop->rhs);
	BL_ASSERT(lhs && rhs)

	if (cnt->debug_mode) emit_DI_instr_loc(cnt, &binop->base);

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
		BL_ABORT("Invalid binary operation.")
	}
}

void
emit_instr_call(Context *cnt, MirInstrCall *call)
{
	MirInstr *callee = call->callee;
	BL_ASSERT(callee);
	BL_ASSERT(callee->value.type);

	MirType *callee_type = callee->value.type->kind == MIR_TYPE_FN
	                           ? callee->value.type
	                           : mir_deref_type(callee->value.type);
	BL_ASSERT(callee_type);
	BL_ASSERT(callee_type->kind == MIR_TYPE_FN);

	MirFn *      fn      = callee->value.data.v_ptr.data.fn;
	LLVMValueRef llvm_fn = callee->llvm_value ? callee->llvm_value : emit_fn_proto(cnt, fn);

	const size_t         argc = call->args ? call->args->size : 0;
	SmallArray_LLVMValue llvm_args;
	sa_init(&llvm_args);

	SmallArray_LLVMType llvm_callee_arg_types;
	sa_init(&llvm_callee_arg_types);

	if (callee_type->data.fn.has_byval) BL_LOG("call byval fn");

	if (argc) {
		MirInstr *arg_instr;
		MirArg *  arg;
		sa_resize_LLVMType(&llvm_callee_arg_types, argc);
		LLVMGetParamTypes(callee_type->llvm_type, llvm_callee_arg_types.data);

		SARRAY_FOREACH(call->args, arg_instr)
		{
			arg                   = callee_type->data.fn.args->data[i];
			LLVMValueRef llvm_arg = fetch_value(cnt, arg_instr);

			switch (arg->llvm_easgm) {
			case LLVM_EASGM_NONE: { /* Default behavior. */
				sa_push_LLVMValue(&llvm_args, llvm_arg);
				break;
			}

			case LLVM_EASGM_8:
			case LLVM_EASGM_16:
			case LLVM_EASGM_32:
			case LLVM_EASGM_64: {
				/* TODO: build allocas at the begining of the function!!! */
				/* TODO: build allocas at the begining of the function!!! */
				/* TODO: build allocas at the begining of the function!!! */
				LLVMValueRef llvm_struct_tmp =
				    LLVMBuildAlloca(cnt->llvm_builder, arg->type->llvm_type, "");
				LLVMValueRef llvm_tmp = LLVMBuildAlloca(
				    cnt->llvm_builder, llvm_callee_arg_types.data[i], "");

				LLVMBuildStore(cnt->llvm_builder,
				               LLVMGetParam(fn->llvm_value, i),
				               llvm_struct_tmp);

				LLVMValueRef llvm_tmp_size =
				    LLVMConstInt(cnt->llvm_i64_type,
				                 LLVMStoreSizeOfType(cnt->llvm_td,
				                                     llvm_callee_arg_types.data[i]),
				                 false);

				LLVMValueRef llvm_tmp_alig =
				    LLVMConstInt(cnt->llvm_i64_type,
				                 LLVMABIAlignmentOfType(
				                     cnt->llvm_td, llvm_callee_arg_types.data[i]),
				                 false);

				build_call_memcpy(
				    cnt, llvm_tmp, llvm_struct_tmp, llvm_tmp_size, llvm_tmp_alig);

				sa_push_LLVMValue(&llvm_args,
				                  LLVMBuildLoad(cnt->llvm_builder, llvm_tmp, ""));
				break;
			}

			case LLVM_EASGM_64_8:
			case LLVM_EASGM_64_16:
			case LLVM_EASGM_64_32:
			case LLVM_EASGM_64_64: {
				LLVMValueRef llvm_struct_tmp =
				    LLVMBuildAlloca(cnt->llvm_builder, arg->type->llvm_type, "");

				LLVMTypeRef llvm_tmp_type = LLVMStructTypeInContext(
				    cnt->llvm_cnt, &llvm_callee_arg_types.data[i], 2, false);
				LLVMValueRef llvm_tmp =
				    LLVMBuildAlloca(cnt->llvm_builder, llvm_tmp_type, "");

				LLVMBuildStore(cnt->llvm_builder,
				               LLVMGetParam(fn->llvm_value, i),
				               llvm_struct_tmp);

				LLVMValueRef llvm_tmp_size =
				    LLVMConstInt(cnt->llvm_i64_type,
				                 LLVMStoreSizeOfType(cnt->llvm_td, llvm_tmp_type),
				                 false);

				LLVMValueRef llvm_tmp_alig = LLVMConstInt(
				    cnt->llvm_i64_type,
				    LLVMABIAlignmentOfType(cnt->llvm_td, llvm_tmp_type),
				    false);

				build_call_memcpy(
				    cnt, llvm_tmp, llvm_struct_tmp, llvm_tmp_size, llvm_tmp_alig);

				LLVMValueRef llvm_tmp_1 =
				    LLVMBuildStructGEP(cnt->llvm_builder, llvm_tmp, 0, "");
				llvm_tmp_1 = LLVMBuildLoad(cnt->llvm_builder, llvm_tmp_1, "");

				LLVMValueRef llvm_tmp_2 =
				    LLVMBuildStructGEP(cnt->llvm_builder, llvm_tmp, 1, "");
				llvm_tmp_2 = LLVMBuildLoad(cnt->llvm_builder, llvm_tmp_2, "");

				sa_push_LLVMValue(&llvm_args, llvm_tmp_1);
				sa_push_LLVMValue(&llvm_args, llvm_tmp_2);
				break;
			}

			case LLVM_EASGM_BYVAL: {
				LLVMValueRef llvm_struct_tmp =
				    LLVMBuildAlloca(cnt->llvm_builder, arg->type->llvm_type, "");
				LLVMBuildStore(cnt->llvm_builder,
				               LLVMGetParam(fn->llvm_value, i),
				               llvm_struct_tmp);
				sa_push_LLVMValue(&llvm_args, llvm_struct_tmp);

				break;
			}
			}
		}
	}

	if (cnt->debug_mode) emit_DI_instr_loc(cnt, &call->base);

	BL_ASSERT(llvm_fn)
	call->base.llvm_value =
	    LLVMBuildCall(cnt->llvm_builder, llvm_fn, llvm_args.data, llvm_args.size, "");

	sa_terminate(&llvm_callee_arg_types);
	sa_terminate(&llvm_args);
}

void
emit_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
	MirVar *var = decl->var;
	BL_ASSERT(var)

	/* skip when we should not generate LLVM representation */
	if (var->value.type->kind == MIR_TYPE_TYPE) return;

	if (var->is_in_gscope) {
		/* OK variable is declared in global scope so we need different generation here*/
		/* Generates destination for global if there is no one. Global variable can come
		 * later than
		 * it is used, so we call same function during generation of the declref instruction
		 * IR. */
		/* Globals must be set to some value */
		BL_ASSERT(decl->init)

		LLVMValueRef tmp = fetch_value(cnt, decl->init);

		emit_global_var_proto(cnt, var);
		LLVMSetInitializer(var->llvm_value, tmp);

		if (cnt->debug_mode) {
			emit_DI_var(cnt, var);
			// emit_DI_instr_loc(cnt, &decl->base);
		}
	} else {
		BL_ASSERT(var->llvm_value)

		if (cnt->debug_mode) {
			emit_DI_var(cnt, var);
			emit_DI_instr_loc(cnt, &decl->base);
		}

		/* generate DI for debug build */
		if (decl->init) {
			/* There is special handling for initialization via compound instruction */
			if (decl->init->kind == MIR_INSTR_COMPOUND) {
				emit_instr_compound(cnt, var, (MirInstrCompound *)decl->init);
			} else {
				/* use simple store */
				LLVMValueRef llvm_init = fetch_value(cnt, decl->init);
				BL_ASSERT(llvm_init)
				LLVMBuildStore(cnt->llvm_builder, llvm_init, var->llvm_value);
			}
		}
	}
}

void
emit_instr_ret(Context *cnt, MirInstrRet *ret)
{
	if (cnt->debug_mode) emit_DI_instr_loc(cnt, &ret->base);

	LLVMValueRef llvm_ret;
	if (ret->value) {
		LLVMValueRef llvm_ret_value = fetch_value(cnt, ret->value);
		BL_ASSERT(llvm_ret_value)
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
	BL_ASSERT(then_block)

	LLVMBasicBlockRef llvm_then_block = emit_basic_block(cnt, then_block);
	BL_ASSERT(llvm_then_block)
	br->base.llvm_value = LLVMBuildBr(cnt->llvm_builder, llvm_then_block);

	LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_then_block);
}

void
emit_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
	MirInstr *     cond       = br->cond;
	MirInstrBlock *then_block = br->then_block;
	MirInstrBlock *else_block = br->else_block;
	BL_ASSERT(cond && then_block)

	LLVMValueRef      llvm_cond       = fetch_value(cnt, cond);
	LLVMBasicBlockRef llvm_then_block = emit_basic_block(cnt, then_block);
	LLVMBasicBlockRef llvm_else_block = emit_basic_block(cnt, else_block);

	br->base.llvm_value =
	    LLVMBuildCondBr(cnt->llvm_builder, llvm_cond, llvm_then_block, llvm_else_block);
}

void
emit_instr_vargs(Context *cnt, MirInstrVArgs *vargs)
{
	MirType *            vargs_type = vargs->base.value.type;
	SmallArray_InstrPtr *values     = vargs->values;
	BL_ASSERT(values)
	const size_t vargsc = values->size;
	BL_ASSERT(vargs_type && vargs_type->kind == MIR_TYPE_VARGS)

	/* Setup tmp array values. */
	if (vargsc > 0) {
		MirInstr *   value;
		LLVMValueRef llvm_value;
		LLVMValueRef llvm_value_dest;
		LLVMValueRef llvm_indices[2];
		llvm_indices[0] = cnt->llvm_const_i64;

		SARRAY_FOREACH(values, value)
		{
			llvm_value = fetch_value(cnt, value);
			BL_ASSERT(llvm_value)
			llvm_indices[1] = LLVMConstInt(cnt->llvm_i64_type, i, true);
			llvm_value_dest = LLVMBuildGEP(cnt->llvm_builder,
			                               vargs->arr_tmp->llvm_value,
			                               llvm_indices,
			                               ARRAY_SIZE(llvm_indices),
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

	BL_ASSERT(llvm_type_info && "Missing LLVM value for RTTI variable.")
	BL_ASSERT(llvm_tmp)

	MirType *   any_type                = mir_deref_type(toany->base.value.type);
	LLVMTypeRef llvm_any_type_info_type = mir_get_struct_elem_type(any_type, 0)->llvm_type;
	LLVMTypeRef llvm_any_data_type      = mir_get_struct_elem_type(any_type, 1)->llvm_type;

	/* use tmp for expression */
	if (toany->expr_tmp) {
		MirVar *expr_tmp = toany->expr_tmp;
		BL_ASSERT(expr_tmp->llvm_value && "Missing tmp variable")

		llvm_data = emit_as_const(cnt, &toany->expr->value);
		LLVMBuildStore(cnt->llvm_builder, llvm_data, expr_tmp->llvm_value);
		llvm_data = expr_tmp->llvm_value;
	} else if (toany->rtti_type_specification) {
		llvm_data = toany->rtti_type_specification->rtti.var->llvm_value;
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
	BL_ASSERT(fn->llvm_value)
	LLVMBasicBlockRef llvm_block = emit_basic_block(cnt, block);
	BL_ASSERT(llvm_block)

	LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_block);

	/* gen allocas fist in entry block!!! */
	if (fn->first_block == block) {
		emit_allocas(cnt, fn);
	}

	MirInstr *instr = block->entry_instr;
	while (instr) {
		if (instr->unrechable) break;
		emit_instr(cnt, instr);
		instr = instr->next;
	}
}

void
emit_allocas(Context *cnt, MirFn *fn)
{
	BL_ASSERT(fn)

	const char *var_name;
	LLVMTypeRef var_type;
	unsigned    var_alignment;
	MirVar *    var;

	BARRAY_FOREACH(fn->variables, var)
	{
		BL_ASSERT(var)

		if (!var->gen_llvm) continue;

#if NAMED_VARS
		var_name = var->llvm_name;
#else
		var_name = "";
#endif
		var_type      = var->value.type->llvm_type;
		var_alignment = (unsigned int)var->value.type->alignment;

		BL_ASSERT(var_type)

		var->llvm_value = LLVMBuildAlloca(cnt->llvm_builder, var_type, var_name);
		LLVMSetAlignment(var->llvm_value, var_alignment);
	}
}

void
emit_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
	MirFn *fn = fn_proto->base.value.data.v_ptr.data.fn;
	/* unused function */
	if (!fn->emit_llvm) return;
	emit_fn_proto(cnt, fn);

	if (IS_NOT_FLAG(fn->flags, FLAG_EXTERN)) {
		if (cnt->debug_mode) {
			emit_DI_instr_loc(cnt, NULL);
			emit_DI_fn(cnt, fn);
		}

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
		BL_ABORT("Invalid instruction")

	case MIR_INSTR_CONST:
	case MIR_INSTR_SIZEOF:
	case MIR_INSTR_ALIGNOF:
	case MIR_INSTR_DECL_VARIANT:
	case MIR_INSTR_DECL_MEMBER:
	case MIR_INSTR_DECL_ARG:
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
	case MIR_INSTR_DECL_DIRECT_REF:
		emit_instr_decl_direct_ref(cnt, (MirInstrDeclDirectRef *)instr);
		break;
	}
}

/* public */
void
ir_run(Assembly *assembly)
{
	Context cnt;
	memset(&cnt, 0, sizeof(Context));
	cnt.assembly               = assembly;
	cnt.gstring_cache          = bo_htbl_new(sizeof(LLVMValueRef), 1024);
	cnt.llvm_cnt               = assembly->llvm.cnt;
	cnt.llvm_module            = assembly->llvm.module;
	cnt.llvm_td                = assembly->llvm.TD;
	cnt.llvm_builder           = LLVMCreateBuilderInContext(assembly->llvm.cnt);
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
	cnt.llvm_di_builder        = assembly->llvm.di_builder;
	cnt.debug_mode             = builder.options.debug_build;

	emit_RTTI_types(&cnt);

	MirInstr *ginstr;
	BARRAY_FOREACH(assembly->MIR.global_instrs, ginstr)
	{
		emit_instr(&cnt, ginstr);
	}

	if (cnt.debug_mode) {
		llvm_di_builder_finalize(cnt.llvm_di_builder);
	}

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
