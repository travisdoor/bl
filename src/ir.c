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

// CLANUP: not the best solution!!!
#define ABORT_FN_NAME "__abort"

#if BL_DEBUG
#define NAMED_VARS true
#else
#define NAMED_VARS false
#endif

typedef struct {
	LLVMValueRef llvm_var;
	MirType *    type;
} RTTIIncomplete;

typedef enum { STATE_PASSED, STATE_POSTPONE } State;

TSMALL_ARRAY_TYPE(LLVMValue, LLVMValueRef, 32);
TSMALL_ARRAY_TYPE(LLVMValue64, LLVMValueRef, 64);
TSMALL_ARRAY_TYPE(LLVMType, LLVMTypeRef, 32);
TSMALL_ARRAY_TYPE(RTTIIncomplete, RTTIIncomplete, 64);

typedef struct {
	Assembly *assembly;

	LLVMContextRef    llvm_cnt;
	LLVMModuleRef     llvm_module;
	LLVMTargetDataRef llvm_td;
	LLVMBuilderRef    llvm_builder;
	LLVMDIBuilderRef  llvm_di_builder;

	/* Constants */
	LLVMValueRef llvm_const_i64;

	THashTable                 gstring_cache;
	TSmallArray_RTTIIncomplete incomplete_rtti;
	TList                      incomplete_queue;

	struct BuiltinTypes *builtin_types;
	bool                 debug_mode;
} Context;

static LLVMValueRef
rtti_emit(Context *cnt, MirType *type);

static void
rtti_satisfy_incomplete(Context *cnt, RTTIIncomplete *incomplete);

static LLVMValueRef
_rtti_emit(Context *cnt, MirType *type);

static LLVMValueRef
rtti_emit_base(Context *cnt, MirType *type, u8 kind, usize size);

static LLVMValueRef
rtti_emit_integer(Context *cnt, MirType *type);

static LLVMValueRef
rtti_emit_real(Context *cnt, MirType *type);

static LLVMValueRef
rtti_emit_array(Context *cnt, MirType *type);

static LLVMValueRef
rtti_emit_empty(Context *cnt, MirType *type, MirType *rtti_type);

static LLVMValueRef
rtti_emit_enum(Context *cnt, MirType *type);

static LLVMValueRef
rtti_emit_enum_variant(Context *cnt, MirVariant *variant);

static LLVMValueRef
rtti_emit_enum_variants_array(Context *cnt, TSmallArray_VariantPtr *variants);

static LLVMValueRef
rtti_emit_enum_variants_slice(Context *cnt, TSmallArray_VariantPtr *variants);

static LLVMValueRef
rtti_emit_struct(Context *cnt, MirType *type);

static LLVMValueRef
rtti_emit_struct_member(Context *cnt, MirMember *member);

static LLVMValueRef
rtti_emit_struct_members_array(Context *cnt, TSmallArray_MemberPtr *members);

static LLVMValueRef
rtti_emit_struct_members_slice(Context *cnt, TSmallArray_MemberPtr *members);

static LLVMValueRef
rtti_emit_fn(Context *cnt, MirType *type);

static LLVMValueRef
rtti_emit_fn_arg(Context *cnt, MirArg *arg);

static LLVMValueRef
rtti_emit_fn_args_array(Context *cnt, TSmallArray_ArgPtr *args);

static LLVMValueRef
rtti_emit_fn_args_slice(Context *cnt, TSmallArray_ArgPtr *args);

static void
emit_DI_instr_loc(Context *cnt, MirInstr *instr);

static void
emit_DI_fn(Context *cnt, MirFn *fn);

static void
emit_DI_var(Context *cnt, MirVar *var);

static State
emit_instr(Context *cnt, MirInstr *instr);

/*
 * Tmp is optional but needed for naked compound expressions.
 */
static LLVMValueRef
emit_const_string(Context *cnt, const char *str, usize len);

static State
emit_instr_compound(Context *cnt, MirVar *_tmp_var, MirInstrCompound *cmp);

static State
emit_instr_binop(Context *cnt, MirInstrBinop *binop);

static State
emit_instr_phi(Context *cnt, MirInstrPhi *phi);

static State
emit_instr_set_initializer(Context *cnt, MirInstrSetInitializer *si);

static State
emit_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info);

static State
emit_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static State
emit_instr_decl_direct_ref(Context *cnt, MirInstrDeclDirectRef *ref);

static State
emit_instr_cast(Context *cnt, MirInstrCast *cast);

static State
emit_instr_addrof(Context *cnt, MirInstrAddrOf *addrof);

static State
emit_instr_unop(Context *cnt, MirInstrUnop *unop);

static State
emit_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static State
emit_instr_store(Context *cnt, MirInstrStore *store);

static State
emit_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);

static State
emit_instr_block(Context *cnt, MirInstrBlock *block);

static State
emit_instr_br(Context *cnt, MirInstrBr *br);

static State
emit_instr_switch(Context *cnt, MirInstrSwitch *sw);

static State
emit_instr_const(Context *cnt, MirInstrConst *c);

static State
emit_instr_arg(Context *cnt, MirVar *dest, MirInstrArg *arg);

static State
emit_instr_cond_br(Context *cnt, MirInstrCondBr *br);

static State
emit_instr_ret(Context *cnt, MirInstrRet *ret);

static State
emit_instr_decl_var(Context *cnt, MirInstrDeclVar *decl);

static State
emit_instr_load(Context *cnt, MirInstrLoad *load);

static State
emit_instr_call(Context *cnt, MirInstrCall *call);

static State
emit_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static State
emit_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr);

static State
emit_instr_vargs(Context *cnt, MirInstrVArgs *vargs);

static State
emit_instr_toany(Context *cnt, MirInstrToAny *toany);

static void
emit_allocas(Context *cnt, MirFn *fn);

static void
emit_incomplete(Context *cnt);

static inline MirInstr *
push_back_incomplete(Context *cnt, MirInstr *instr)
{
	BL_ASSERT(instr && "Attempt to push null instruction into incomplete queue!");
	tlist_push_back(&cnt->incomplete_queue, instr);
	return instr;
}

static inline MirInstr *
pop_front_incomplete(Context *cnt)
{
	MirInstr *instr = NULL;
	TList *   queue = &cnt->incomplete_queue;
	if (!tlist_empty(queue)) {
		instr = tlist_front(MirInstr *, queue);
		tlist_pop_front(queue);
	}

	return instr;
}

static inline LLVMValueRef
emit_fn_proto(Context *cnt, MirFn *fn)
{
	BL_ASSERT(fn);
	if (!fn->emit_llvm) return NULL;
#if LLVM_EXCLUDE_UNUSED_SYM
	BL_ASSERT(fn->ref_count);
#endif

	fn->llvm_value = LLVMGetNamedFunction(cnt->llvm_module, fn->linkage_name);
	if (fn->llvm_value) return fn->llvm_value;

	fn->llvm_value = LLVMAddFunction(cnt->llvm_module, fn->linkage_name, fn->type->llvm_type);

	/* Setup attributes for sret */
	if (fn->type->data.fn.has_sret) {
		LLVMAddAttributeAtIndex(fn->llvm_value,
		                        LLVM_SRET_INDEX + 1,
		                        llvm_create_attribute(cnt->llvm_cnt, LLVM_ATTR_NOALIAS));

		LLVMAddAttributeAtIndex(fn->llvm_value,
		                        LLVM_SRET_INDEX + 1,
		                        llvm_create_attribute(cnt->llvm_cnt, LLVM_ATTR_STRUCTRET));
	}

	/* Setup attributes for byval */
	if (fn->type->data.fn.has_byval) {
		TSmallArray_ArgPtr *args = fn->type->data.fn.args;
		BL_ASSERT(args);

		MirArg *arg;
		TSA_FOREACH(args, arg)
		{
			if (arg->llvm_easgm != LLVM_EASGM_BYVAL) continue;
			/* Setup attributes. */
			LLVMAttributeRef llvm_attr = llvm_create_attribute_type(
			    cnt->llvm_cnt, LLVM_ATTR_BYVAL, arg->type->llvm_type);

			LLVMAddAttributeAtIndex(fn->llvm_value, arg->llvm_index + 1, llvm_attr);
		}
	}

	if (IS_FLAG(fn->flags, FLAG_INLINE)) {
		LLVMAttributeRef llvm_attr =
		    llvm_create_attribute(cnt->llvm_cnt, LLVM_ATTR_ALWAYSINLINE);

		LLVMAddAttributeAtIndex(
		    fn->llvm_value, (unsigned)LLVMAttributeFunctionIndex, llvm_attr);
	}

	if (IS_FLAG(fn->flags, FLAG_NO_INLINE)) {
		LLVMAttributeRef llvm_attr =
		    llvm_create_attribute(cnt->llvm_cnt, LLVM_ATTR_NOINLINE);

		LLVMAddAttributeAtIndex(
		    fn->llvm_value, (unsigned)LLVMAttributeFunctionIndex, llvm_attr);
	}

	return fn->llvm_value;
}

static inline bool
is_initialized(LLVMValueRef constant)
{
	return constant && LLVMGetInitializer(constant);
}

static inline LLVMValueRef
emit_global_var_proto(Context *cnt, MirVar *var)
{
	BL_ASSERT(var);
	if (var->llvm_value) return var->llvm_value;

	LLVMTypeRef llvm_type = var->value.type->llvm_type;
	var->llvm_value       = LLVMAddGlobal(cnt->llvm_module, llvm_type, var->linkage_name);

	LLVMSetGlobalConstant(var->llvm_value, !var->is_mutable);

	/* Linkage should be later set by user. */
	LLVMSetLinkage(var->llvm_value, LLVMPrivateLinkage);
	LLVMSetAlignment(var->llvm_value, (unsigned)var->value.type->alignment);

	return var->llvm_value;
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

	if (var->is_global) {
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

LLVMValueRef
emit_const_string(Context *cnt, const char *str, usize len)
{
	MirType *    type     = cnt->builtin_types->t_string;
	LLVMValueRef llvm_str = NULL;

	if (str) {
		MirType * raw_str_elem_type = mir_deref_type(mir_get_struct_elem_type(type, 1));
		u64       hash              = thash_from_str(str);
		TIterator found             = thtbl_find(&cnt->gstring_cache, hash);
		TIterator end               = thtbl_end(&cnt->gstring_cache);

		if (!TITERATOR_EQUAL(found, end)) {
			llvm_str = thtbl_iter_peek_value(LLVMValueRef, found);
		} else {

			LLVMValueRef llvm_str_content = llvm_const_string_in_context(
			    cnt->llvm_cnt, raw_str_elem_type->llvm_type, str, true);

			llvm_str =
			    LLVMAddGlobal(cnt->llvm_module, LLVMTypeOf(llvm_str_content), ".str");
			LLVMSetInitializer(llvm_str, llvm_str_content);
			LLVMSetLinkage(llvm_str, LLVMPrivateLinkage);
			LLVMSetGlobalConstant(llvm_str, true);

			/* Store for reuse into the cache! */
			thtbl_insert(&cnt->gstring_cache, hash, llvm_str);
		}
	} else {
		/* null string content  */
		MirType *str_type = mir_get_struct_elem_type(type, 1);
		llvm_str          = LLVMConstNull(str_type->llvm_type);
	}

	MirType *    len_type = mir_get_struct_elem_type(type, 0);
	MirType *    ptr_type = mir_get_struct_elem_type(type, 1);
	LLVMValueRef llvm_len = LLVMConstInt(len_type->llvm_type, (u64)len, true);

	TSmallArray_LLVMValue llvm_members;
	tsa_init(&llvm_members);

	tsa_push_LLVMValue(&llvm_members, llvm_len);
	tsa_push_LLVMValue(&llvm_members, LLVMConstBitCast(llvm_str, ptr_type->llvm_type));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(type->llvm_type, llvm_members.data, (u32)llvm_members.size);

	tsa_terminate(&llvm_members);

	return llvm_result;
}

State
emit_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
	ScopeEntry *entry = ref->scope_entry;
	BL_ASSERT(entry);

	switch (entry->kind) {
	case SCOPE_ENTRY_VAR: {
		MirVar *var = entry->data.var;
		if (var->is_global) {
			ref->base.llvm_value = emit_global_var_proto(cnt, var);
		} else {
			ref->base.llvm_value = var->llvm_value;
		}
		break;
	}
	case SCOPE_ENTRY_FN: {
		ref->base.llvm_value = emit_fn_proto(cnt, entry->data.fn);
		break;
	}
	default:
		BL_UNIMPLEMENTED;
	}

	BL_ASSERT(ref->base.llvm_value);
	return STATE_PASSED;
}

State
emit_instr_decl_direct_ref(Context *cnt, MirInstrDeclDirectRef *ref)
{
	BL_ASSERT(ref->ref && ref->ref->kind == MIR_INSTR_DECL_VAR);

	MirVar *var = ((MirInstrDeclVar *)ref->ref)->var;
	BL_ASSERT(var);

	ref->base.llvm_value = var->llvm_value;
	BL_ASSERT(ref->base.llvm_value);
	return STATE_PASSED;
}

State
emit_instr_phi(Context *cnt, MirInstrPhi *phi)
{
	LLVMValueRef llvm_phi =
	    LLVMBuildPhi(cnt->llvm_builder, phi->base.value.type->llvm_type, "");

	const usize count = phi->incoming_blocks->size;

	TSmallArray_LLVMValue llvm_iv;
	TSmallArray_LLVMValue llvm_ib;

	tsa_init(&llvm_iv);
	tsa_init(&llvm_ib);

	MirInstr *     value;
	MirInstrBlock *block;
	for (usize i = 0; i < count; ++i) {
		value = phi->incoming_values->data[i];
		block = (MirInstrBlock *)phi->incoming_blocks->data[i];

		tsa_push_LLVMValue(&llvm_iv, value->llvm_value);
		tsa_push_LLVMValue(&llvm_ib, LLVMBasicBlockAsValue(emit_basic_block(cnt, block)));
	}

	LLVMAddIncoming(
	    llvm_phi, llvm_iv.data, (LLVMBasicBlockRef *)llvm_ib.data, (unsigned int)count);

	tsa_terminate(&llvm_iv);
	tsa_terminate(&llvm_ib);

	phi->base.llvm_value = llvm_phi;
	return STATE_PASSED;
}

State
emit_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
	MirFn *abort_fn = unr->abort_fn;
	BL_ASSERT(abort_fn);
	if (!abort_fn->llvm_value) emit_fn_proto(cnt, abort_fn);
	if (cnt->debug_mode) emit_DI_instr_loc(cnt, &unr->base);
	LLVMBuildCall(cnt->llvm_builder, abort_fn->llvm_value, NULL, 0, "");
	return STATE_PASSED;
}

LLVMValueRef
rtti_emit_base(Context *cnt, MirType *type, u8 kind, usize size)
{
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirType *kind_type = mir_get_struct_elem_type(type, 0);
	tsa_push_LLVMValue(&llvm_vals, LLVMConstInt(kind_type->llvm_type, kind, false));

	MirType *size_type = mir_get_struct_elem_type(type, 1);
	tsa_push_LLVMValue(&llvm_vals, LLVMConstInt(size_type->llvm_type, size, false));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);
	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_empty(Context *cnt, MirType *type, MirType *rtti_type)
{
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirType *base_type = mir_get_struct_elem_type(rtti_type, 0);
	tsa_push_LLVMValue(&llvm_vals,
	                   rtti_emit_base(cnt, base_type, type->kind, type->store_size_bytes));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_enum(Context *cnt, MirType *type)
{
	MirType *             rtti_type = cnt->builtin_types->t_TypeInfoEnum;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	/* base */
	MirType *base_type = mir_get_struct_elem_type(rtti_type, 0);
	tsa_push_LLVMValue(&llvm_vals,
	                   rtti_emit_base(cnt, base_type, type->kind, type->store_size_bytes));

	/* name */
	const char *name = type->user_id ? type->user_id->str : type->id.str;
	tsa_push_LLVMValue(&llvm_vals, emit_const_string(cnt, name, strlen(name)));

	/* base_type */
	tsa_push_LLVMValue(&llvm_vals, _rtti_emit(cnt, type->data.enm.base_type));

	/* variants */
	tsa_push_LLVMValue(&llvm_vals, rtti_emit_enum_variants_slice(cnt, type->data.enm.variants));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_enum_variant(Context *cnt, MirVariant *variant)
{
	MirType *             rtti_type = cnt->builtin_types->t_TypeInfoEnumVariant;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	/* name */
	tsa_push_LLVMValue(&llvm_vals,
	                   emit_const_string(cnt, variant->id->str, strlen(variant->id->str)));

	/* value */
	MirType *value_type = mir_get_struct_elem_type(rtti_type, 1);
	tsa_push_LLVMValue(&llvm_vals,
	                   LLVMConstInt(value_type->llvm_type,
	                                MIR_CEV_READ_AS(u64, variant->value),
	                                value_type->data.integer.is_signed));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_enum_variants_array(Context *cnt, TSmallArray_VariantPtr *variants)
{
	MirType *             elem_type = cnt->builtin_types->t_TypeInfoEnumVariant;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirVariant *it;
	TSA_FOREACH(variants, it)
	{
		tsa_push_LLVMValue(&llvm_vals, rtti_emit_enum_variant(cnt, it));
	}

	LLVMValueRef llvm_result =
	    LLVMConstArray(elem_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	LLVMValueRef llvm_rtti_var =
	    LLVMAddGlobal(cnt->llvm_module, LLVMTypeOf(llvm_result), ".rtti_variants");
	LLVMSetLinkage(llvm_rtti_var, LLVMPrivateLinkage);
	LLVMSetGlobalConstant(llvm_rtti_var, true);
	LLVMSetInitializer(llvm_rtti_var, llvm_result);

	tsa_terminate(&llvm_vals);
	return llvm_rtti_var;
}

LLVMValueRef
rtti_emit_enum_variants_slice(Context *cnt, TSmallArray_VariantPtr *variants)
{
	MirType *             type = cnt->builtin_types->t_TypeInfoEnumVariants_slice;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirType *len_type = mir_get_struct_elem_type(type, 0);
	MirType *ptr_type = mir_get_struct_elem_type(type, 1);
	tsa_push_LLVMValue(
	    &llvm_vals,
	    LLVMConstInt(len_type->llvm_type, variants->size, len_type->data.integer.is_signed));

	tsa_push_LLVMValue(
	    &llvm_vals,
	    LLVMConstBitCast(rtti_emit_enum_variants_array(cnt, variants), ptr_type->llvm_type));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_struct(Context *cnt, MirType *type)
{
	MirType *             rtti_type = cnt->builtin_types->t_TypeInfoStruct;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	/* base */
	MirType *base_type = mir_get_struct_elem_type(rtti_type, 0);
	tsa_push_LLVMValue(&llvm_vals,
	                   rtti_emit_base(cnt, base_type, MIR_TYPE_STRUCT, type->store_size_bytes));

	/* name */
	const char *name = type->user_id ? type->user_id->str : type->id.str;
	tsa_push_LLVMValue(&llvm_vals, emit_const_string(cnt, name, strlen(name)));

	/* members */
	tsa_push_LLVMValue(&llvm_vals,
	                   rtti_emit_struct_members_slice(cnt, type->data.strct.members));

	/* is_slice */
	const bool is_slice      = type->kind == MIR_TYPE_SLICE || type->kind == MIR_TYPE_VARGS;
	MirType *  is_slice_type = mir_get_struct_elem_type(rtti_type, 3);
	tsa_push_LLVMValue(&llvm_vals,
	                   LLVMConstInt(is_slice_type->llvm_type,
	                                (u64)is_slice,
	                                is_slice_type->data.integer.is_signed));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_struct_member(Context *cnt, MirMember *member)
{
	MirType *             rtti_type = cnt->builtin_types->t_TypeInfoStructMember;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	/* name */
	tsa_push_LLVMValue(&llvm_vals,
	                   emit_const_string(cnt, member->id->str, strlen(member->id->str)));

	/* base_type */
	tsa_push_LLVMValue(&llvm_vals, _rtti_emit(cnt, member->type));

	/* offset_bytes */
	MirType *offset_type = mir_get_struct_elem_type(rtti_type, 2);
	tsa_push_LLVMValue(&llvm_vals,
	                   LLVMConstInt(offset_type->llvm_type,
	                                (u32)member->offset_bytes,
	                                offset_type->data.integer.is_signed));

	/* index */
	MirType *index_type = mir_get_struct_elem_type(rtti_type, 3);
	tsa_push_LLVMValue(&llvm_vals,
	                   LLVMConstInt(offset_type->llvm_type,
	                                (u32)member->index,
	                                index_type->data.integer.is_signed));

	/* tags */
	MirType *tags_type = mir_get_struct_elem_type(rtti_type, 4);
	tsa_push_LLVMValue(&llvm_vals,
	                   LLVMConstInt(offset_type->llvm_type,
	                                (u32)member->tags,
	                                tags_type->data.integer.is_signed));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_struct_members_array(Context *cnt, TSmallArray_MemberPtr *members)
{
	MirType *             elem_type = cnt->builtin_types->t_TypeInfoStructMember;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirMember *it;
	TSA_FOREACH(members, it)
	{
		tsa_push_LLVMValue(&llvm_vals, rtti_emit_struct_member(cnt, it));
	}

	LLVMValueRef llvm_result =
	    LLVMConstArray(elem_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	LLVMValueRef llvm_rtti_var =
	    LLVMAddGlobal(cnt->llvm_module, LLVMTypeOf(llvm_result), ".rtti_members");
	LLVMSetLinkage(llvm_rtti_var, LLVMPrivateLinkage);
	LLVMSetGlobalConstant(llvm_rtti_var, true);
	LLVMSetInitializer(llvm_rtti_var, llvm_result);

	tsa_terminate(&llvm_vals);
	return llvm_rtti_var;
}

LLVMValueRef
rtti_emit_struct_members_slice(Context *cnt, TSmallArray_MemberPtr *members)
{
	MirType *             type = cnt->builtin_types->t_TypeInfoStructMembers_slice;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirType *len_type = mir_get_struct_elem_type(type, 0);
	MirType *ptr_type = mir_get_struct_elem_type(type, 1);
	tsa_push_LLVMValue(
	    &llvm_vals,
	    LLVMConstInt(len_type->llvm_type, members->size, len_type->data.integer.is_signed));

	tsa_push_LLVMValue(
	    &llvm_vals,
	    LLVMConstBitCast(rtti_emit_struct_members_array(cnt, members), ptr_type->llvm_type));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_fn(Context *cnt, MirType *type)
{
	MirType *             rtti_type = cnt->builtin_types->t_TypeInfoFn;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	/* base */
	MirType *base_type = mir_get_struct_elem_type(rtti_type, 0);
	tsa_push_LLVMValue(&llvm_vals,
	                   rtti_emit_base(cnt, base_type, type->kind, type->store_size_bytes));

	/* name */
	const char *name = type->user_id ? type->user_id->str : type->id.str;
	tsa_push_LLVMValue(&llvm_vals, emit_const_string(cnt, name, strlen(name)));

	/* args */
	tsa_push_LLVMValue(&llvm_vals, rtti_emit_fn_args_slice(cnt, type->data.fn.args));

	/* ret_type */
	tsa_push_LLVMValue(&llvm_vals, _rtti_emit(cnt, type->data.fn.ret_type));

	/* is_vargs */
	MirType *is_vargs_type = mir_get_struct_elem_type(rtti_type, 4);
	tsa_push_LLVMValue(&llvm_vals,
	                   LLVMConstInt(is_vargs_type->llvm_type,
	                                (u64)type->data.fn.is_vargs,
	                                is_vargs_type->data.integer.is_signed));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_fn_arg(Context *cnt, MirArg *arg)
{
	MirType *             rtti_type = cnt->builtin_types->t_TypeInfoFnArg;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	/* name */
	tsa_push_LLVMValue(&llvm_vals, emit_const_string(cnt, arg->id->str, strlen(arg->id->str)));

	/* base_type */
	tsa_push_LLVMValue(&llvm_vals, _rtti_emit(cnt, arg->type));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_fn_args_array(Context *cnt, TSmallArray_ArgPtr *args)
{
	MirType *             elem_type = cnt->builtin_types->t_TypeInfoFnArg;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirArg *it;
	TSA_FOREACH(args, it)
	{
		tsa_push_LLVMValue(&llvm_vals, rtti_emit_fn_arg(cnt, it));
	}

	LLVMValueRef llvm_result =
	    LLVMConstArray(elem_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	LLVMValueRef llvm_rtti_var =
	    LLVMAddGlobal(cnt->llvm_module, LLVMTypeOf(llvm_result), ".rtti_args");
	LLVMSetLinkage(llvm_rtti_var, LLVMPrivateLinkage);
	LLVMSetGlobalConstant(llvm_rtti_var, true);
	LLVMSetInitializer(llvm_rtti_var, llvm_result);

	tsa_terminate(&llvm_vals);
	return llvm_rtti_var;
}

LLVMValueRef
rtti_emit_fn_args_slice(Context *cnt, TSmallArray_ArgPtr *args)
{
	MirType *             type = cnt->builtin_types->t_TypeInfoFnArgs_slice;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	const usize argc = args ? args->size : 0;

	MirType *len_type = mir_get_struct_elem_type(type, 0);
	MirType *ptr_type = mir_get_struct_elem_type(type, 1);
	tsa_push_LLVMValue(
	    &llvm_vals, LLVMConstInt(len_type->llvm_type, argc, len_type->data.integer.is_signed));

	if (argc) {
		tsa_push_LLVMValue(
		    &llvm_vals,
		    LLVMConstBitCast(rtti_emit_fn_args_array(cnt, args), ptr_type->llvm_type));
	} else {
		tsa_push_LLVMValue(&llvm_vals, LLVMConstNull(ptr_type->llvm_type));
	}

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_integer(Context *cnt, MirType *type)
{
	MirType *             rtti_type = cnt->builtin_types->t_TypeInfoInt;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirType *base_type = mir_get_struct_elem_type(rtti_type, 0);
	tsa_push_LLVMValue(&llvm_vals,
	                   rtti_emit_base(cnt, base_type, type->kind, type->store_size_bytes));

	MirType *bitcount_type = mir_get_struct_elem_type(rtti_type, 1);
	tsa_push_LLVMValue(
	    &llvm_vals,
	    LLVMConstInt(bitcount_type->llvm_type, (u32)type->data.integer.bitcount, true));

	MirType *is_signed_type = mir_get_struct_elem_type(rtti_type, 2);
	tsa_push_LLVMValue(
	    &llvm_vals,
	    LLVMConstInt(is_signed_type->llvm_type, (u32)type->data.integer.is_signed, true));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_real(Context *cnt, MirType *type)
{
	MirType *             rtti_type = cnt->builtin_types->t_TypeInfoReal;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirType *base_type = mir_get_struct_elem_type(rtti_type, 0);
	tsa_push_LLVMValue(&llvm_vals,
	                   rtti_emit_base(cnt, base_type, type->kind, type->store_size_bytes));

	MirType *bitcount_type = mir_get_struct_elem_type(rtti_type, 1);
	tsa_push_LLVMValue(
	    &llvm_vals,
	    LLVMConstInt(bitcount_type->llvm_type, (u32)type->data.integer.bitcount, true));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_ptr(Context *cnt, MirType *type)
{
	MirType *             rtti_type = cnt->builtin_types->t_TypeInfoPtr;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirType *base_type = mir_get_struct_elem_type(rtti_type, 0);
	tsa_push_LLVMValue(&llvm_vals,
	                   rtti_emit_base(cnt, base_type, type->kind, type->store_size_bytes));

	/* pointee */
	tsa_push_LLVMValue(&llvm_vals, _rtti_emit(cnt, type->data.ptr.expr));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit_array(Context *cnt, MirType *type)
{
	MirType *             rtti_type = cnt->builtin_types->t_TypeInfoArray;
	TSmallArray_LLVMValue llvm_vals;
	tsa_init(&llvm_vals);

	MirType *base_type = mir_get_struct_elem_type(rtti_type, 0);
	tsa_push_LLVMValue(&llvm_vals,
	                   rtti_emit_base(cnt, base_type, type->kind, type->store_size_bytes));

	/* name */
	const char *name = type->user_id ? type->user_id->str : type->id.str;
	tsa_push_LLVMValue(&llvm_vals, emit_const_string(cnt, name, strlen(name)));

	/* elem_type */
	tsa_push_LLVMValue(&llvm_vals, _rtti_emit(cnt, type->data.array.elem_type));

	/* len */
	MirType *len_type = mir_get_struct_elem_type(rtti_type, 3);
	tsa_push_LLVMValue(&llvm_vals,
	                   LLVMConstInt(len_type->llvm_type,
	                                type->data.array.len,
	                                len_type->data.integer.is_signed));

	LLVMValueRef llvm_result =
	    LLVMConstNamedStruct(rtti_type->llvm_type, llvm_vals.data, (u32)llvm_vals.size);

	tsa_terminate(&llvm_vals);
	return llvm_result;
}

LLVMValueRef
rtti_emit(Context *cnt, MirType *type)
{
	LLVMValueRef llvm_value = _rtti_emit(cnt, type);

	TSmallArray_RTTIIncomplete *pending = &cnt->incomplete_rtti;
	while (pending->size) {
		RTTIIncomplete incomplete = tsa_pop_RTTIIncomplete(pending);
		rtti_satisfy_incomplete(cnt, &incomplete);
	}

	return llvm_value;
}

void
rtti_satisfy_incomplete(Context *cnt, RTTIIncomplete *incomplete)
{
	MirType *    type          = incomplete->type;
	LLVMValueRef llvm_rtti_var = incomplete->llvm_var;

	BL_ASSERT(type->kind == MIR_TYPE_PTR);
	LLVMValueRef llvm_value = rtti_emit_ptr(cnt, type);

	BL_ASSERT(llvm_value);
	LLVMSetInitializer(llvm_rtti_var, llvm_value);
}

LLVMValueRef
_rtti_emit(Context *cnt, MirType *type)
{
	BL_ASSERT(type);
	BL_ASSERT(assembly_has_rtti(cnt->assembly, type->id.hash));

	MirVar *rtti_var = assembly_get_rtti(cnt->assembly, type->id.hash);
	if (rtti_var->llvm_value) {
		return rtti_var->llvm_value;
	}

	LLVMValueRef llvm_rtti_var = LLVMAddGlobal(
	    cnt->llvm_module, rtti_var->value.type->llvm_type, rtti_var->linkage_name);
	LLVMSetLinkage(llvm_rtti_var, LLVMPrivateLinkage);
	LLVMSetGlobalConstant(llvm_rtti_var, true);

	LLVMValueRef llvm_value = NULL;

	switch (type->kind) {
	case MIR_TYPE_INT:
		llvm_value = rtti_emit_integer(cnt, type);
		break;

	case MIR_TYPE_ENUM:
		llvm_value = rtti_emit_enum(cnt, type);
		break;

	case MIR_TYPE_REAL:
		llvm_value = rtti_emit_real(cnt, type);
		break;

	case MIR_TYPE_BOOL:
		llvm_value = rtti_emit_empty(cnt, type, cnt->builtin_types->t_TypeInfoBool);
		break;

	case MIR_TYPE_TYPE:
		llvm_value = rtti_emit_empty(cnt, type, cnt->builtin_types->t_TypeInfoType);
		break;

	case MIR_TYPE_VOID:
		llvm_value = rtti_emit_empty(cnt, type, cnt->builtin_types->t_TypeInfoVoid);
		break;

	case MIR_TYPE_NULL:
		llvm_value = rtti_emit_empty(cnt, type, cnt->builtin_types->t_TypeInfoNull);
		break;

	case MIR_TYPE_STRING:
		llvm_value = rtti_emit_empty(cnt, type, cnt->builtin_types->t_TypeInfoString);
		break;

	case MIR_TYPE_PTR:
		tsa_push_RTTIIncomplete(&cnt->incomplete_rtti,
		                        (RTTIIncomplete){.llvm_var = llvm_rtti_var, .type = type});
		goto SKIP;
		break;

	case MIR_TYPE_ARRAY:
		llvm_value = rtti_emit_array(cnt, type);
		break;

	case MIR_TYPE_SLICE:
	case MIR_TYPE_VARGS:
	case MIR_TYPE_STRUCT:
		llvm_value = rtti_emit_struct(cnt, type);
		break;

	case MIR_TYPE_FN:
		llvm_value = rtti_emit_fn(cnt, type);
		break;

	default: {
		char type_name[256];
		mir_type_to_str(type_name, 256, type, true);
		BL_ABORT("Missing LLVM RTTI generation for type '%s'", type_name);
	}
	}

	BL_ASSERT(llvm_value);

	LLVMSetInitializer(llvm_rtti_var, llvm_value);

SKIP:
	llvm_rtti_var = LLVMBuildCast(cnt->llvm_builder,
	                              LLVMBitCast,
	                              llvm_rtti_var,
	                              cnt->builtin_types->t_TypeInfo_ptr->llvm_type,
	                              "");

	rtti_var->llvm_value = llvm_rtti_var;
	return llvm_rtti_var;
}

State
emit_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info)
{
	BL_ASSERT(type_info->rtti_type);
	type_info->base.llvm_value = rtti_emit(cnt, type_info->rtti_type);
	return STATE_PASSED;
}

State
emit_instr_cast(Context *cnt, MirInstrCast *cast)
{
	LLVMValueRef llvm_src       = cast->expr->llvm_value;
	LLVMTypeRef  llvm_dest_type = cast->base.value.type->llvm_type;
	LLVMOpcode   llvm_op;
	BL_ASSERT(llvm_src && llvm_dest_type);

	switch (cast->op) {
	case MIR_CAST_NONE:
		cast->base.llvm_value = llvm_src;
		return STATE_PASSED;
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
	return STATE_PASSED;
}

State
emit_instr_addrof(Context *cnt, MirInstrAddrOf *addrof)
{
	if (addrof->src->kind == MIR_INSTR_FN_PROTO) {
		MirInstrFnProto *fn_proto = (MirInstrFnProto *)addrof->src;
		MirFn *          fn       = MIR_CEV_READ_AS(MirFn *, &fn_proto->base.value);
		addrof->base.llvm_value   = emit_fn_proto(cnt, fn);
	} else {
		addrof->base.llvm_value = addrof->src->llvm_value;
	}
	BL_ASSERT(addrof->base.llvm_value);
	return STATE_PASSED;
}

State
emit_instr_arg(Context *cnt, MirVar *dest, MirInstrArg *arg_instr)
{
	BL_ASSERT(dest);

	MirFn *fn = arg_instr->base.owner_block->owner_fn;
	BL_ASSERT(fn);
	MirType *    fn_type = fn->type;
	LLVMValueRef llvm_fn = fn->llvm_value;
	BL_ASSERT(llvm_fn);

	MirArg *     arg       = fn_type->data.fn.args->data[arg_instr->i];
	LLVMValueRef llvm_dest = dest->llvm_value;
	BL_ASSERT(llvm_dest);

	switch (arg->llvm_easgm) {
	case LLVM_EASGM_NONE: { /* Default. Arg value unchanged. */
		LLVMValueRef llvm_arg = LLVMGetParam(llvm_fn, arg->llvm_index);
		LLVMBuildStore(cnt->llvm_builder, llvm_arg, llvm_dest);
		break;
	}

	case LLVM_EASGM_8:
	case LLVM_EASGM_16:
	case LLVM_EASGM_32:
	case LLVM_EASGM_64: {
		LLVMValueRef llvm_arg = LLVMGetParam(llvm_fn, arg->llvm_index);
		llvm_dest             = LLVMBuildBitCast(
                    cnt->llvm_builder, llvm_dest, LLVMPointerType(LLVMTypeOf(llvm_arg), 0), "");

		llvm_arg = LLVMBuildStore(cnt->llvm_builder, llvm_arg, llvm_dest);
		break;
	}

	case LLVM_EASGM_64_8:
	case LLVM_EASGM_64_16:
	case LLVM_EASGM_64_32:
	case LLVM_EASGM_64_64: {
		LLVMValueRef llvm_arg_1 = LLVMGetParam(llvm_fn, arg->llvm_index);
		LLVMValueRef llvm_arg_2 = LLVMGetParam(llvm_fn, arg->llvm_index + 1);

		LLVMTypeRef llvm_arg_elem_types[] = {LLVMTypeOf(llvm_arg_1),
		                                     LLVMTypeOf(llvm_arg_2)};
		LLVMTypeRef llvm_arg_type =
		    LLVMStructTypeInContext(cnt->llvm_cnt, llvm_arg_elem_types, 2, false);

		llvm_dest = LLVMBuildBitCast(
		    cnt->llvm_builder, llvm_dest, LLVMPointerType(llvm_arg_type, 0), "");

		LLVMValueRef llvm_dest_1 = LLVMBuildStructGEP(cnt->llvm_builder, llvm_dest, 0, "");
		LLVMBuildStore(cnt->llvm_builder, llvm_arg_1, llvm_dest_1);
		LLVMValueRef llvm_dest_2 = LLVMBuildStructGEP(cnt->llvm_builder, llvm_dest, 1, "");
		LLVMBuildStore(cnt->llvm_builder, llvm_arg_2, llvm_dest_2);
		break;
	}

	case LLVM_EASGM_BYVAL: {
		LLVMValueRef llvm_arg = LLVMGetParam(llvm_fn, arg->llvm_index);
		llvm_arg              = LLVMBuildLoad(cnt->llvm_builder, llvm_arg, "");
		llvm_arg              = LLVMBuildStore(cnt->llvm_builder, llvm_arg, llvm_dest);
		break;
	}
	}

	arg_instr->base.llvm_value = NULL;
	return STATE_PASSED;
}

State
emit_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
	MirType *    arr_type     = mir_deref_type(elem_ptr->arr_ptr->value.type);
	LLVMValueRef llvm_arr_ptr = elem_ptr->arr_ptr->llvm_value;
	LLVMValueRef llvm_index   = elem_ptr->index->llvm_value;
	BL_ASSERT(llvm_arr_ptr && llvm_index);

	switch (arr_type->kind) {
	case MIR_TYPE_ARRAY: {
		LLVMValueRef llvm_indices[2];
		llvm_indices[0] = cnt->llvm_const_i64;
		llvm_indices[1] = llvm_index;

		elem_ptr->base.llvm_value = LLVMBuildGEP(
		    cnt->llvm_builder, llvm_arr_ptr, llvm_indices, TARRAY_SIZE(llvm_indices), "");
		break;
	}

	case MIR_TYPE_SLICE:
	case MIR_TYPE_STRING:
	case MIR_TYPE_VARGS: {
		/* special case for slices */
		llvm_arr_ptr = LLVMBuildStructGEP(cnt->llvm_builder, llvm_arr_ptr, 1, "");
		llvm_arr_ptr = LLVMBuildLoad(cnt->llvm_builder, llvm_arr_ptr, "");
		BL_ASSERT(llvm_arr_ptr);

		LLVMValueRef llvm_indices[1];
		llvm_indices[0] = llvm_index;

		elem_ptr->base.llvm_value = LLVMBuildInBoundsGEP(
		    cnt->llvm_builder, llvm_arr_ptr, llvm_indices, TARRAY_SIZE(llvm_indices), "");

		break;
	}

	default:
		BL_ABORT("Invalid elem ptr target type!");
	}
	return STATE_PASSED;
}

State
emit_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
	LLVMValueRef llvm_target_ptr = member_ptr->target_ptr->llvm_value;
	BL_ASSERT(llvm_target_ptr);

	if (member_ptr->builtin_id == MIR_BUILTIN_ID_NONE) {
		BL_ASSERT(member_ptr->scope_entry->kind == SCOPE_ENTRY_MEMBER);
		MirMember *member = member_ptr->scope_entry->data.member;
		BL_ASSERT(member);

		const unsigned int index =
		    (const unsigned int)member_ptr->scope_entry->data.member->index;

		member_ptr->base.llvm_value =
		    LLVMBuildStructGEP(cnt->llvm_builder, llvm_target_ptr, index, "");
		BL_ASSERT(member_ptr->base.llvm_value);
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
	return STATE_PASSED;
}

State
emit_instr_load(Context *cnt, MirInstrLoad *load)
{
	BL_ASSERT(load->base.value.type && "invalid type of load instruction");
	LLVMValueRef llvm_src = load->src->llvm_value;
	BL_ASSERT(llvm_src);

	if (mir_is_instr_in_global_block(&load->base) && mir_is_comptime(&load->base)) {
		/* When we try to create comptime constant composition and load value, initializer
		 * is needed. But loaded value could be in incomplete state during instruction emit,
		 * we need to postpone this instruction in such case and try to resume later. */
		if (!is_initialized(llvm_src)) {
			return STATE_POSTPONE;
		}

		load->base.llvm_value = LLVMGetInitializer(llvm_src);
		return STATE_PASSED;
	}

	load->base.llvm_value = LLVMBuildLoad(cnt->llvm_builder, llvm_src, "");

	const unsigned alignment = (const unsigned)load->base.value.type->alignment;
	LLVMSetAlignment(load->base.llvm_value, alignment);
	return STATE_PASSED;
}

State
emit_instr_store(Context *cnt, MirInstrStore *store)
{
	LLVMValueRef   val       = store->src->llvm_value;
	LLVMValueRef   ptr       = store->dest->llvm_value;
	const unsigned alignment = (unsigned)store->src->value.type->alignment;
	BL_ASSERT(val && ptr);

	if (cnt->debug_mode) emit_DI_instr_loc(cnt, &store->base);

	store->base.llvm_value = LLVMBuildStore(cnt->llvm_builder, val, ptr);
	LLVMSetAlignment(store->base.llvm_value, alignment);
	return STATE_PASSED;
}

State
emit_instr_unop(Context *cnt, MirInstrUnop *unop)
{
	LLVMValueRef llvm_val = unop->expr->llvm_value;
	BL_ASSERT(llvm_val);

	LLVMTypeKind lhs_kind   = LLVMGetTypeKind(LLVMTypeOf(llvm_val));
	const bool   float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

	if (cnt->debug_mode) emit_DI_instr_loc(cnt, &unop->base);

	switch (unop->op) {
	case UNOP_BIT_NOT:
	case UNOP_NOT: {
		BL_ASSERT(!float_kind && "Invalid negation of floating point type.");
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
	return STATE_PASSED;
}

State
emit_instr_compound(Context *cnt, MirVar *_tmp_var, MirInstrCompound *cmp)
{
	MirVar *tmp_var = _tmp_var ? _tmp_var : cmp->tmp_var;

	MirType *  type                = cmp->base.value.type;
	const bool is_zero_initialized = cmp->is_zero_initialized;

	if (mir_is_comptime(&cmp->base)) {
		if (is_zero_initialized) {
			cmp->base.llvm_value = LLVMConstNull(type->llvm_type);
			goto SKIP;
		}

		switch (type->kind) {
		case MIR_TYPE_ARRAY: {
			const usize len            = type->data.array.len;
			LLVMTypeRef llvm_elem_type = type->data.array.elem_type->llvm_type;
			BL_ASSERT(len && llvm_elem_type);

			TSmallArray_LLVMValue llvm_elems;
			tsa_init(&llvm_elems);

			MirInstr *it;
			TSA_FOREACH(cmp->values, it)
			{
				LLVMValueRef llvm_elem = it->llvm_value;
				BL_ASSERT(llvm_elem && "Invalid constant compound elem value!");

				tsa_push_LLVMValue(&llvm_elems, llvm_elem);
			}

			cmp->base.llvm_value =
			    LLVMConstArray(llvm_elem_type, llvm_elems.data, (unsigned int)len);

			tsa_terminate(&llvm_elems);
			break;
		}

		case MIR_TYPE_STRING: {
			const u32   len = MIR_CEV_READ_AS(const u32, &cmp->values->data[0]->value);
			const char *str =
			    MIR_CEV_READ_AS(const char *, &cmp->values->data[1]->value);

			cmp->base.llvm_value = emit_const_string(cnt, str, len);
			break;
		}

		case MIR_TYPE_SLICE:
		case MIR_TYPE_VARGS:
		case MIR_TYPE_STRUCT: {
			TSmallArray_LLVMValue llvm_members;
			tsa_init(&llvm_members);

			MirInstr *it;
			TSA_FOREACH(cmp->values, it)
			{
				LLVMValueRef llvm_member = it->llvm_value;
				BL_ASSERT(llvm_member && "Invalid constant compound member value!");

				tsa_push_LLVMValue(&llvm_members, llvm_member);
			}

			cmp->base.llvm_value = LLVMConstNamedStruct(
			    type->llvm_type, llvm_members.data, (u32)cmp->values->size);

			tsa_terminate(&llvm_members);
			break;
		}

		default: {
			BL_ASSERT(cmp->values->size == 1 &&
			          "Expected only one compound initializer value!");

			cmp->base.llvm_value = cmp->values->data[0]->llvm_value;
		}
		}

	SKIP:
		BL_ASSERT(cmp->base.llvm_value);
		if (tmp_var) {
			LLVMValueRef llvm_dest = _tmp_var->llvm_value;
			BL_ASSERT(llvm_dest);

			LLVMBuildStore(cnt->llvm_builder, cmp->base.llvm_value, llvm_dest);
		}
	} else {
		BL_ASSERT(tmp_var && "Missing tmp variable for compound expression!");
		BL_ASSERT(!cmp->is_zero_initialized &&
		          "Zero initialized compound should be comptime!");

		LLVMValueRef llvm_tmp = tmp_var->llvm_value;
		MirType *    type     = tmp_var->value.type;
		BL_ASSERT(llvm_tmp)
		BL_ASSERT(type)

		TSmallArray_InstrPtr *values = cmp->values;
		MirInstr *            value;
		LLVMValueRef          llvm_value;
		LLVMValueRef          llvm_value_dest;
		LLVMValueRef          llvm_indices[2];
		llvm_indices[0] = cnt->llvm_const_i64;

		TSA_FOREACH(values, value)
		{
			llvm_value = value->llvm_value;
			BL_ASSERT(llvm_value)

			switch (type->kind) {
			case MIR_TYPE_ARRAY:
				llvm_indices[1] =
				    LLVMConstInt(cnt->builtin_types->t_s64->llvm_type, i, true);
				llvm_value_dest = LLVMBuildGEP(cnt->llvm_builder,
				                               llvm_tmp,
				                               llvm_indices,
				                               TARRAY_SIZE(llvm_indices),
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
	return STATE_PASSED;
}

State
emit_instr_binop(Context *cnt, MirInstrBinop *binop)
{
	LLVMValueRef lhs = binop->lhs->llvm_value;
	LLVMValueRef rhs = binop->rhs->llvm_value;
	BL_ASSERT(lhs && rhs);

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
	return STATE_PASSED;
}

State
emit_instr_call(Context *cnt, MirInstrCall *call)
{
	/******************************************************************************************/
#define INSERT_TMP(_name, _type)                                                                   \
	LLVMValueRef _name = NULL;                                                                 \
	{                                                                                          \
		LLVMBasicBlockRef llvm_prev_block = LLVMGetInsertBlock(cnt->llvm_builder);         \
                                                                                                   \
		LLVMBasicBlockRef llvm_entry_block = LLVMValueAsBasicBlock(                        \
		    call->base.owner_block->owner_fn->first_block->base.llvm_value);               \
                                                                                                   \
		if (LLVMGetLastInstruction(llvm_entry_block)) {                                    \
			LLVMPositionBuilderBefore(cnt->llvm_builder,                               \
			                          LLVMGetLastInstruction(llvm_entry_block));       \
		} else {                                                                           \
			LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_entry_block);             \
		}                                                                                  \
                                                                                                   \
		_name = LLVMBuildAlloca(cnt->llvm_builder, _type, "");                             \
		LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_prev_block);                      \
	}                                                                                          \
	/******************************************************************************************/

	MirInstr *callee = call->callee;
	BL_ASSERT(callee);
	BL_ASSERT(callee->value.type);

	MirType *callee_type = callee->value.type->kind == MIR_TYPE_FN
	                           ? callee->value.type
	                           : mir_deref_type(callee->value.type);
	BL_ASSERT(callee_type);
	BL_ASSERT(callee_type->kind == MIR_TYPE_FN);

	MirFn *callee_fn =
	    mir_is_comptime(callee) ? MIR_CEV_READ_AS(MirFn *, &callee->value) : NULL;
	LLVMValueRef llvm_called_fn =
	    callee->llvm_value ? callee->llvm_value : emit_fn_proto(cnt, callee_fn);

	bool       has_byval_arg = false;
	const bool has_args      = call->args > 0;

	/* Tmp for arg values passed into the Call Instruction. */
	TSmallArray_LLVMValue llvm_args;
	tsa_init(&llvm_args);

	/* Callee required argument types. */
	TSmallArray_LLVMType llvm_callee_arg_types;
	tsa_init(&llvm_callee_arg_types);

	LLVMValueRef llvm_result = NULL;
	/* SRET must come first!!! */
	if (callee_type->data.fn.has_sret) {
		/* PERFORMANCE: Reuse ret_tmp inside function??? */
		INSERT_TMP(llvm_tmp, callee_type->data.fn.ret_type->llvm_type);
		tsa_push_LLVMValue(&llvm_args, llvm_tmp);
	}

	if (has_args) {
		MirInstr *arg_instr;
		MirArg *  arg;

		/* Get real argument types of LLMV function. */
		tsa_resize_LLVMType(&llvm_callee_arg_types,
		                    LLVMCountParamTypes(callee_type->llvm_type));
		LLVMGetParamTypes(callee_type->llvm_type, llvm_callee_arg_types.data);

		TSA_FOREACH(call->args, arg_instr)
		{
			arg                   = callee_type->data.fn.args->data[i];
			LLVMValueRef llvm_arg = arg_instr->llvm_value;

			switch (arg->llvm_easgm) {
			case LLVM_EASGM_NONE: { /* Default behavior. */
				tsa_push_LLVMValue(&llvm_args, llvm_arg);
				break;
			}

			case LLVM_EASGM_8:
			case LLVM_EASGM_16:
			case LLVM_EASGM_32:
			case LLVM_EASGM_64: { /* Struct fits into one register. */
				/* PERFORMANCE: insert only when llvm_arg is not alloca???
				 */
				INSERT_TMP(llvm_tmp, arg->type->llvm_type);
				LLVMBuildStore(cnt->llvm_builder, llvm_arg, llvm_tmp);

				LLVMBuildStore(cnt->llvm_builder, llvm_arg, llvm_tmp);
				llvm_tmp = LLVMBuildBitCast(
				    cnt->llvm_builder,
				    llvm_tmp,
				    LLVMPointerType(llvm_callee_arg_types.data[arg->llvm_index], 0),
				    "");

				tsa_push_LLVMValue(&llvm_args,
				                   LLVMBuildLoad(cnt->llvm_builder, llvm_tmp, ""));
				break;
			}

			case LLVM_EASGM_64_8:
			case LLVM_EASGM_64_16:
			case LLVM_EASGM_64_32:
			case LLVM_EASGM_64_64: { /* Struct fits into two registers. */
				/* PERFORMANCE: insert only when llvm_arg is not alloca???
				 */
				INSERT_TMP(llvm_tmp, arg->type->llvm_type);
				LLVMBuildStore(cnt->llvm_builder, llvm_arg, llvm_tmp);

				LLVMTypeRef llvm_tmp_elem_types[] = {
				    llvm_callee_arg_types.data[arg->llvm_index],
				    llvm_callee_arg_types.data[arg->llvm_index + 1]};
				LLVMTypeRef llvm_tmp_type = LLVMStructTypeInContext(
				    cnt->llvm_cnt, llvm_tmp_elem_types, 2, false);

				llvm_tmp = LLVMBuildBitCast(cnt->llvm_builder,
				                            llvm_tmp,
				                            LLVMPointerType(llvm_tmp_type, 0),
				                            "");

				LLVMValueRef llvm_tmp_1 =
				    LLVMBuildStructGEP(cnt->llvm_builder, llvm_tmp, 0, "");
				tsa_push_LLVMValue(
				    &llvm_args, LLVMBuildLoad(cnt->llvm_builder, llvm_tmp_1, ""));

				LLVMValueRef llvm_tmp_2 =
				    LLVMBuildStructGEP(cnt->llvm_builder, llvm_tmp, 1, "");
				tsa_push_LLVMValue(
				    &llvm_args, LLVMBuildLoad(cnt->llvm_builder, llvm_tmp_2, ""));
				break;
			}

			case LLVM_EASGM_BYVAL: { /* Struct is too big and must be passed by
				                  * value.
				                  */
				if (!has_byval_arg) has_byval_arg = true;
				/* PERFORMANCE: insert only when llvm_arg is not alloca???
				 */
				INSERT_TMP(llvm_tmp, arg->type->llvm_type);
				LLVMBuildStore(cnt->llvm_builder, llvm_arg, llvm_tmp);

				tsa_push_LLVMValue(&llvm_args, llvm_tmp);
				break;
			}
			}
		}
	}

	if (cnt->debug_mode) {
		emit_DI_instr_loc(cnt, &call->base);
	}

	LLVMValueRef llvm_call = LLVMBuildCall(
	    cnt->llvm_builder, llvm_called_fn, llvm_args.data, (unsigned int)llvm_args.size, "");

	if (callee_type->data.fn.has_sret) {
		LLVMAddCallSiteAttribute(llvm_call,
		                         LLVM_SRET_INDEX + 1,
		                         llvm_create_attribute(cnt->llvm_cnt, LLVM_ATTR_STRUCTRET));

		llvm_result = LLVMBuildLoad(cnt->llvm_builder, llvm_args.data[LLVM_SRET_INDEX], "");
	}

	/* PERFORMANCE: LLVM API requires to set call side attributes after call is created.
	 */
	if (has_byval_arg) {
		BL_ASSERT(has_args);
		TSmallArray_ArgPtr *args = callee_type->data.fn.args;
		MirArg *            arg;
		TSA_FOREACH(args, arg)
		{
			if (arg->llvm_easgm != LLVM_EASGM_BYVAL) continue;

			LLVMAttributeRef llvm_atrbt = llvm_create_attribute_type(
			    cnt->llvm_cnt, LLVM_ATTR_BYVAL, arg->type->llvm_type);
			LLVMAddCallSiteAttribute(llvm_call, arg->llvm_index + 1, llvm_atrbt);
		}
	}

	tsa_terminate(&llvm_callee_arg_types);
	tsa_terminate(&llvm_args);
	call->base.llvm_value = llvm_result ? llvm_result : llvm_call;
	return STATE_PASSED;
#undef INSERT_TMP
}

State
emit_instr_set_initializer(Context *cnt, MirInstrSetInitializer *si)
{
	MirVar *var = ((MirInstrDeclVar *)si->dest)->var;
#if LLVM_EXCLUDE_UNUSED_SYM
	if (var->ref_count == 0) return STATE_PASSED;
#endif
	BL_ASSERT(var->llvm_value);
	BL_ASSERT(si->src->llvm_value);

	LLVMSetInitializer(var->llvm_value, si->src->llvm_value);
	return STATE_PASSED;
}

State
emit_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
	MirVar *var = decl->var;
	BL_ASSERT(var);
#if LLVM_EXCLUDE_UNUSED_SYM
	if (var->ref_count == 0) return STATE_PASSED;
#endif

	/* skip when we should not generate LLVM representation */
	if (var->value.type->kind == MIR_TYPE_TYPE) return STATE_PASSED;

	if (var->is_global) {
		emit_global_var_proto(cnt, var);

		if (cnt->debug_mode) {
			emit_DI_var(cnt, var);
		}
	} else {
		BL_ASSERT(var->llvm_value);

		if (cnt->debug_mode) {
			emit_DI_var(cnt, var);
			emit_DI_instr_loc(cnt, &decl->base);
		}

		/* generate DI for debug build */
		if (decl->init) {
			/* There is special handling for initialization via compound
			 * instruction */
			if (decl->init->kind == MIR_INSTR_COMPOUND) {
				emit_instr_compound(cnt, var, (MirInstrCompound *)decl->init);
			} else if (decl->init->kind == MIR_INSTR_ARG) {
				emit_instr_arg(cnt, var, (MirInstrArg *)decl->init);
			} else {
				/* use simple store */
				LLVMValueRef llvm_init = decl->init->llvm_value;
				BL_ASSERT(llvm_init);
				LLVMBuildStore(cnt->llvm_builder, llvm_init, var->llvm_value);
			}
		}
	}
	return STATE_PASSED;
}

State
emit_instr_ret(Context *cnt, MirInstrRet *ret)
{
	if (cnt->debug_mode) emit_DI_instr_loc(cnt, &ret->base);

	MirFn *fn = ret->base.owner_block->owner_fn;
	BL_ASSERT(fn);

	MirType *fn_type = fn->type;

	if (fn_type->data.fn.has_sret) {
		LLVMValueRef llvm_ret_value = ret->value->llvm_value;
		LLVMValueRef llvm_sret      = LLVMGetParam(fn->llvm_value, LLVM_SRET_INDEX);
		LLVMBuildStore(cnt->llvm_builder, llvm_ret_value, llvm_sret);

		ret->base.llvm_value = LLVMBuildRetVoid(cnt->llvm_builder);
		return STATE_PASSED;
	}

	if (ret->value) {
		LLVMValueRef llvm_ret_value = ret->value->llvm_value;
		BL_ASSERT(llvm_ret_value);
		ret->base.llvm_value = LLVMBuildRet(cnt->llvm_builder, llvm_ret_value);
		return STATE_PASSED;
	}

	ret->base.llvm_value = LLVMBuildRetVoid(cnt->llvm_builder);
	return STATE_PASSED;
}

State
emit_instr_br(Context *cnt, MirInstrBr *br)
{
	MirInstrBlock *then_block = br->then_block;
	BL_ASSERT(then_block);

	LLVMBasicBlockRef llvm_then_block = emit_basic_block(cnt, then_block);
	BL_ASSERT(llvm_then_block);
	br->base.llvm_value = LLVMBuildBr(cnt->llvm_builder, llvm_then_block);

	LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_then_block);
	return STATE_PASSED;
}

State
emit_instr_switch(Context *cnt, MirInstrSwitch *sw)
{
	MirInstr *              value         = sw->value;
	MirInstrBlock *         default_block = sw->default_block;
	TSmallArray_SwitchCase *cases         = sw->cases;

	LLVMValueRef      llvm_value         = value->llvm_value;
	LLVMBasicBlockRef llvm_default_block = emit_basic_block(cnt, default_block);

	LLVMValueRef llvm_switch = LLVMBuildSwitch(
	    cnt->llvm_builder, llvm_value, llvm_default_block, (unsigned int)cases->size);

	for (usize i = 0; i < cases->size; ++i) {
		MirSwitchCase *   c             = &cases->data[i];
		LLVMValueRef      llvm_on_value = c->on_value->llvm_value;
		LLVMBasicBlockRef llvm_block    = emit_basic_block(cnt, c->block);

		LLVMAddCase(llvm_switch, llvm_on_value, llvm_block);
	}

	sw->base.llvm_value = llvm_switch;
	return STATE_PASSED;
}

State
emit_instr_const(Context *cnt, MirInstrConst *c)
{
	MirType *    type       = c->base.value.type;
	LLVMValueRef llvm_value = NULL;
	LLVMTypeRef  llvm_type  = type->llvm_type;

	switch (type->kind) {
	case MIR_TYPE_ENUM: {
		type = type->data.enm.base_type;
		BL_ASSERT(type->kind == MIR_TYPE_INT);
		const u64 i = MIR_CEV_READ_AS(u64, &c->base.value);
		llvm_value  = LLVMConstInt(llvm_type, i, type->data.integer.is_signed);
		break;
	}

	case MIR_TYPE_INT: {
		const u64 i = MIR_CEV_READ_AS(u64, &c->base.value);
		llvm_value  = LLVMConstInt(llvm_type, i, type->data.integer.is_signed);
		break;
	}

	case MIR_TYPE_BOOL: {
		const bool i = MIR_CEV_READ_AS(bool, &c->base.value);
		llvm_value   = LLVMConstInt(llvm_type, i, false);
		break;
	}

	case MIR_TYPE_REAL: {
		switch (type->store_size_bytes) {
		case 4: {
			const float i = MIR_CEV_READ_AS(float, &c->base.value);
			llvm_value    = LLVMConstReal(llvm_type, (double)i);
			break;
		}

		case 8: {
			const double i = MIR_CEV_READ_AS(double, &c->base.value);
			llvm_value     = LLVMConstReal(llvm_type, i);
			break;
		}

		default:
			BL_ABORT("Unknown real type!");
		}
		break;
	}

	case MIR_TYPE_NULL: {
		llvm_value = LLVMConstNull(llvm_type);
		break;
	}

	case MIR_TYPE_STRING: {
		VMStackPtr len_ptr =
		    vm_get_struct_elem_ptr(cnt->assembly, type, c->base.value.data, 0);
		VMStackPtr str_ptr =
		    vm_get_struct_elem_ptr(cnt->assembly, type, c->base.value.data, 1);

		const s64   len = vm_read_as(s64, len_ptr);
		const char *str = vm_read_as(const char *, str_ptr);

		llvm_value = emit_const_string(cnt, str, len);
		break;
	}

	default:
		BL_UNIMPLEMENTED;
	}

	BL_ASSERT(llvm_value && "Incomplete const value generation!");
	c->base.llvm_value = llvm_value;
	return STATE_PASSED;
}

State
emit_instr_cond_br(Context *cnt, MirInstrCondBr *br)
{
	MirInstr *     cond       = br->cond;
	MirInstrBlock *then_block = br->then_block;
	MirInstrBlock *else_block = br->else_block;
	BL_ASSERT(cond && then_block);

	LLVMValueRef      llvm_cond       = cond->llvm_value;
	LLVMBasicBlockRef llvm_then_block = emit_basic_block(cnt, then_block);
	LLVMBasicBlockRef llvm_else_block = emit_basic_block(cnt, else_block);

	br->base.llvm_value =
	    LLVMBuildCondBr(cnt->llvm_builder, llvm_cond, llvm_then_block, llvm_else_block);
	return STATE_PASSED;
}

State
emit_instr_vargs(Context *cnt, MirInstrVArgs *vargs)
{
	MirType *             vargs_type = vargs->base.value.type;
	TSmallArray_InstrPtr *values     = vargs->values;
	BL_ASSERT(values);
	const usize vargsc = values->size;
	BL_ASSERT(vargs_type && vargs_type->kind == MIR_TYPE_VARGS);

	/* Setup tmp array values. */
	if (vargsc > 0) {
		MirInstr *   value;
		LLVMValueRef llvm_value;
		LLVMValueRef llvm_value_dest;
		LLVMValueRef llvm_indices[2];
		llvm_indices[0] = cnt->llvm_const_i64;

		TSA_FOREACH(values, value)
		{
			llvm_value = value->llvm_value;
			BL_ASSERT(llvm_value);
			llvm_indices[1] =
			    LLVMConstInt(cnt->builtin_types->t_s64->llvm_type, i, true);
			llvm_value_dest = LLVMBuildGEP(cnt->llvm_builder,
			                               vargs->arr_tmp->llvm_value,
			                               llvm_indices,
			                               TARRAY_SIZE(llvm_indices),
			                               "");
			LLVMBuildStore(cnt->llvm_builder, llvm_value, llvm_value_dest);
		}
	}

	{
		LLVMValueRef llvm_len =
		    LLVMConstInt(cnt->builtin_types->t_s64->llvm_type, vargsc, false);
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
	return STATE_PASSED;
}

State
emit_instr_toany(Context *cnt, MirInstrToAny *toany)
{
	LLVMValueRef llvm_dest      = toany->tmp->llvm_value;
	LLVMValueRef llvm_type_info = rtti_emit(cnt, toany->rtti_type);

	BL_ASSERT(llvm_dest && llvm_type_info);

	/* Setup tmp variable */
	/* pointer to type info */
	LLVMValueRef llvm_dest_type_info = LLVMBuildStructGEP(cnt->llvm_builder, llvm_dest, 0, "");
	LLVMBuildStore(cnt->llvm_builder, llvm_type_info, llvm_dest_type_info);

	/* data */
	LLVMTypeRef llvm_dest_data_type =
	    mir_get_struct_elem_type(toany->tmp->value.type, 1)->llvm_type;
	LLVMValueRef llvm_dest_data = LLVMBuildStructGEP(cnt->llvm_builder, llvm_dest, 1, "");

	if (toany->expr_tmp) {
		LLVMValueRef llvm_dest_tmp = toany->expr_tmp->llvm_value;
		BL_ASSERT(llvm_dest_tmp && "Missing tmp variable!");

		LLVMBuildStore(cnt->llvm_builder, toany->expr->llvm_value, llvm_dest_tmp);

		LLVMValueRef llvm_data =
		    LLVMBuildPointerCast(cnt->llvm_builder, llvm_dest_tmp, llvm_dest_data_type, "");
		LLVMBuildStore(cnt->llvm_builder, llvm_data, llvm_dest_data);
	} else if (toany->rtti_data) {
		LLVMValueRef llvm_data_type_info = rtti_emit(cnt, toany->rtti_data);
		LLVMValueRef llvm_data           = LLVMBuildPointerCast(
                    cnt->llvm_builder, llvm_data_type_info, llvm_dest_data_type, "");
		LLVMBuildStore(cnt->llvm_builder, llvm_data, llvm_dest_data);
	} else {
		LLVMValueRef llvm_data = LLVMBuildPointerCast(
		    cnt->llvm_builder, toany->expr->llvm_value, llvm_dest_data_type, "");
		LLVMBuildStore(cnt->llvm_builder, llvm_data, llvm_dest_data);
	}

	toany->base.llvm_value = llvm_dest;
	return STATE_PASSED;
}

State
emit_instr_block(Context *cnt, MirInstrBlock *block)
{
	/* We don't want to genrate type resolvers for typedefs!!! */
	if (!block->emit_llvm) return STATE_PASSED;

	MirFn *           fn              = block->owner_fn;
	const bool        is_global       = fn == NULL;
	LLVMBasicBlockRef llvm_prev_block = LLVMGetInsertBlock(cnt->llvm_builder);

	if (!block->terminal) BL_ABORT("Block '%s', is not terminated", block->name);

	/* Global-scope blocks does not have LLVM equivalent, we can generate just the
	 * content of our block, but every instruction must be comptime constant. */
	if (!is_global) {
		BL_ASSERT(fn->llvm_value);
		LLVMBasicBlockRef llvm_block = emit_basic_block(cnt, block);
		BL_ASSERT(llvm_block);

		LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_block);

		/* gen allocas fist in entry block!!! */
		if (fn->first_block == block) {
			emit_allocas(cnt, fn);
		}
	} else {
		BL_ASSERT(block->base.value.is_comptime);
	}

	MirInstr *instr = block->entry_instr;
	while (instr) {
		const State s = emit_instr(cnt, instr);
		if (s == STATE_POSTPONE) {
			push_back_incomplete(cnt, instr);
			goto SKIP;
		}

		instr = instr->next;
	}

SKIP:
	LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_prev_block);
	return STATE_PASSED;
}

void
emit_incomplete(Context *cnt)
{
	MirInstr *        instr   = pop_front_incomplete(cnt);
	LLVMBasicBlockRef prev_bb = LLVMGetInsertBlock(cnt->llvm_builder);

	while (instr) {
		LLVMBasicBlockRef bb = LLVMValueAsBasicBlock(instr->owner_block->base.llvm_value);
		LLVMPositionBuilderAtEnd(cnt->llvm_builder, bb);

		const State s = emit_instr(cnt, instr);
		if (s == STATE_POSTPONE) {
			push_back_incomplete(cnt, instr);
		}

		if (s != STATE_POSTPONE && instr->next) {
			instr = instr->next;
		} else {
			instr = NULL;
			instr = pop_front_incomplete(cnt);
		}
	}

	LLVMPositionBuilderAtEnd(cnt->llvm_builder, prev_bb);
}

void
emit_allocas(Context *cnt, MirFn *fn)
{
	BL_ASSERT(fn);

	const char *var_name;
	LLVMTypeRef var_type;
	unsigned    var_alignment;
	MirVar *    var;

	TARRAY_FOREACH(MirVar *, fn->variables, var)
	{
		BL_ASSERT(var);
		if (!var->emit_llvm) continue;

#if NAMED_VARS
		var_name = var->linkage_name;
#else
		var_name = "";
#endif
		var_type      = var->value.type->llvm_type;
		var_alignment = (unsigned int)var->value.type->alignment;

		BL_ASSERT(var_type);

		var->llvm_value = LLVMBuildAlloca(cnt->llvm_builder, var_type, var_name);
		LLVMSetAlignment(var->llvm_value, var_alignment);
	}
}

State
emit_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
	MirFn *fn = MIR_CEV_READ_AS(MirFn *, &fn_proto->base.value);

	/* unused function */
	if (!fn->emit_llvm) return STATE_PASSED;
#if LLVM_EXCLUDE_UNUSED_SYM
	if (fn->ref_count == 0) return STATE_PASSED;
#endif

	emit_fn_proto(cnt, fn);

	/* External functions does not have any body block. */
	if (IS_NOT_FLAG(fn->flags, FLAG_EXTERN)) {
		if (cnt->debug_mode) {
			emit_DI_instr_loc(cnt, NULL);
			emit_DI_fn(cnt, fn);
		}

		MirInstr *block = (MirInstr *)fn->first_block;

		while (block) {
			if (!block->is_unreachable) {
				const State s = emit_instr(cnt, block);
				if (s != STATE_PASSED)
					BL_ABORT("Postpone for whole block is not supported!");
			}
			block = block->next;
		}
	}

	return STATE_PASSED;
}

State
emit_instr(Context *cnt, MirInstr *instr)
{
	State state = STATE_PASSED;
	if (instr->value.type->kind == MIR_TYPE_TYPE) return state;

	switch (instr->kind) {
	case MIR_INSTR_INVALID:
		BL_ABORT("Invalid instruction")

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
	case MIR_INSTR_ARG:
		break;

	case MIR_INSTR_BINOP:
		state = emit_instr_binop(cnt, (MirInstrBinop *)instr);
		break;
	case MIR_INSTR_FN_PROTO:
		state = emit_instr_fn_proto(cnt, (MirInstrFnProto *)instr);
		break;
	case MIR_INSTR_BLOCK:
		state = emit_instr_block(cnt, (MirInstrBlock *)instr);
		break;
	case MIR_INSTR_BR:
		state = emit_instr_br(cnt, (MirInstrBr *)instr);
		break;
	case MIR_INSTR_SWITCH:
		state = emit_instr_switch(cnt, (MirInstrSwitch *)instr);
		break;
	case MIR_INSTR_COND_BR:
		state = emit_instr_cond_br(cnt, (MirInstrCondBr *)instr);
		break;
	case MIR_INSTR_RET:
		state = emit_instr_ret(cnt, (MirInstrRet *)instr);
		break;
	case MIR_INSTR_DECL_VAR:
		state = emit_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
		break;
	case MIR_INSTR_DECL_REF:
		state = emit_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
		break;
	case MIR_INSTR_LOAD:
		state = emit_instr_load(cnt, (MirInstrLoad *)instr);
		break;
	case MIR_INSTR_STORE:
		state = emit_instr_store(cnt, (MirInstrStore *)instr);
		break;
	case MIR_INSTR_CALL:
		state = emit_instr_call(cnt, (MirInstrCall *)instr);
		break;
	case MIR_INSTR_UNOP:
		state = emit_instr_unop(cnt, (MirInstrUnop *)instr);
		break;
	case MIR_INSTR_UNREACHABLE:
		state = emit_instr_unreachable(cnt, (MirInstrUnreachable *)instr);
		break;
	case MIR_INSTR_MEMBER_PTR:
		state = emit_instr_member_ptr(cnt, (MirInstrMemberPtr *)instr);
		break;
	case MIR_INSTR_ELEM_PTR:
		state = emit_instr_elem_ptr(cnt, (MirInstrElemPtr *)instr);
		break;
	case MIR_INSTR_ADDROF:
		state = emit_instr_addrof(cnt, (MirInstrAddrOf *)instr);
		break;
	case MIR_INSTR_CAST:
		state = emit_instr_cast(cnt, (MirInstrCast *)instr);
		break;
	case MIR_INSTR_VARGS:
		state = emit_instr_vargs(cnt, (MirInstrVArgs *)instr);
		break;
	case MIR_INSTR_TYPE_INFO:
		state = emit_instr_type_info(cnt, (MirInstrTypeInfo *)instr);
		break;
	case MIR_INSTR_PHI:
		state = emit_instr_phi(cnt, (MirInstrPhi *)instr);
		break;
	case MIR_INSTR_COMPOUND: {
		MirInstrCompound *cmp = (MirInstrCompound *)instr;
		if (!cmp->is_naked && !mir_is_comptime(instr)) break;
		state = emit_instr_compound(cnt, NULL, cmp);
		break;
	}
	case MIR_INSTR_TOANY:
		state = emit_instr_toany(cnt, (MirInstrToAny *)instr);
		break;
	case MIR_INSTR_DECL_DIRECT_REF:
		state = emit_instr_decl_direct_ref(cnt, (MirInstrDeclDirectRef *)instr);
		break;
	case MIR_INSTR_CONST:
		state = emit_instr_const(cnt, (MirInstrConst *)instr);
		break;
	case MIR_INSTR_SET_INITIALIZER:
		state = emit_instr_set_initializer(cnt, (MirInstrSetInitializer *)instr);
		break;
	}

	return state;
}

/* public */
void
ir_run(Assembly *assembly)
{
	Context cnt;
	memset(&cnt, 0, sizeof(Context));
	cnt.assembly        = assembly;
	cnt.builtin_types   = &assembly->builtin_types;
	cnt.llvm_cnt        = assembly->llvm.cnt;
	cnt.llvm_module     = assembly->llvm.module;
	cnt.llvm_td         = assembly->llvm.TD;
	cnt.llvm_builder    = LLVMCreateBuilderInContext(assembly->llvm.cnt);
	cnt.llvm_const_i64  = LLVMConstInt(cnt.builtin_types->t_u64->llvm_type, 0, false);
	cnt.llvm_di_builder = assembly->llvm.di_builder;
	cnt.debug_mode      = assembly->options.build_mode == BUILD_MODE_DEBUG;
	thtbl_init(&cnt.gstring_cache, sizeof(LLVMValueRef), 1024);
	tsa_init(&cnt.incomplete_rtti);
	tlist_init(&cnt.incomplete_queue, sizeof(MirInstr *));

	MirInstr *ginstr;
	TARRAY_FOREACH(MirInstr *, &assembly->MIR.global_instrs, ginstr)
	{
		emit_instr(&cnt, ginstr);
	}

	emit_incomplete(&cnt);

	if (cnt.debug_mode) {
		llvm_di_builder_finalize(cnt.llvm_di_builder);
	}

#if BL_DEBUG
	char *error = NULL;
	if (LLVMVerifyModule(cnt.llvm_module, LLVMReturnStatusAction, &error)) {
		BL_ABORT("LLVM module not verified with error: %s", error);
	}
	LLVMDisposeMessage(error);
#endif

	LLVMDisposeBuilder(cnt.llvm_builder);

	tlist_terminate(&cnt.incomplete_queue);
	tsa_terminate(&cnt.incomplete_rtti);
	thtbl_terminate(&cnt.gstring_cache);
}
