//************************************************************************************************
// bl
//
// File:   mir_printer.c
// Author: Martin Dorazil
// Date:   3/15/18
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

#include "mir_printer.h"
#include "assembly.h"
#include "ast.h"
#include "mir.h"

#if BL_DEBUG
#define PRINT_ANALYZED_COMPTIMES true
#else
#define PRINT_ANALYZED_COMPTIMES false
#endif

typedef struct {
	Assembly *assembly;
	FILE *    stream;
} Context;

static void
print_comptime_value_or_id(Context *cnt, MirInstr *instr);

static INLINE void
print_type(Context *cnt, MirType *type, bool aligned, bool prefer_name)
{
	char tmp[256];
	mir_type_to_str(tmp, TARRAY_SIZE(tmp), type, prefer_name);
	if (aligned)
		fprintf(cnt->stream, "%16s", tmp);
	else
		fprintf(cnt->stream, "%s", tmp);
}

static INLINE void
print_instr_head(Context *cnt, MirInstr *instr, const char *name)
{
	if (!instr) return;

#if BL_DEBUG
	if (instr->ref_count == -1) {
		fprintf(cnt->stream, "    %%%-6llu (-)", (unsigned long long)instr->id);
	} else {
		fprintf(cnt->stream,
		        "    %%%-6llu (%d)",
		        (unsigned long long)instr->id,
		        instr->ref_count);
	}
#else
	fprintf(cnt->stream, "    %%%-6llu", (unsigned long long)instr->id);
#endif
	print_type(cnt, instr->value.type, true, true);
	fprintf(cnt->stream, " %s ", name);
}

static INLINE void
print_flags(Context *cnt, u32 flags)
{
	if (flags == 0) return;

	if (IS_FLAG(flags, FLAG_EXTERN)) fprintf(cnt->stream, "#extern");
	if (IS_FLAG(flags, FLAG_COMPILER)) fprintf(cnt->stream, " #compiler");
	if (IS_FLAG(flags, FLAG_TEST)) fprintf(cnt->stream, " #test");
	if (IS_FLAG(flags, FLAG_INLINE)) fprintf(cnt->stream, " #inline");
	if (IS_FLAG(flags, FLAG_NO_INLINE)) fprintf(cnt->stream, " #noinline");
	if (IS_FLAG(flags, FLAG_PRIVATE)) fprintf(cnt->stream, " #private");

	fprintf(cnt->stream, " ");
}

#define print_const_value(C, V) _print_const_value((C), (V)->type, (V)->data)

static INLINE void
_print_const_value(Context *cnt, MirType *type, VMStackPtr value)
{
	if (!type) return;
	if (!value) {
		fprintf(cnt->stream, "<null>");
		return;
	}

	switch (type->kind) {
	case MIR_TYPE_ENUM:
	case MIR_TYPE_INT: {
		if (type->data.integer.is_signed) {
			switch (type->store_size_bytes) {
			case 1:
				fprintf(cnt->stream, "%d", vm_read_as(s8, value));
				break;
			case 2:
				fprintf(cnt->stream, "%d", vm_read_as(s16, value));
				break;
			case 4:
				fprintf(cnt->stream, "%d", vm_read_as(s32, value));
				break;
			case 8:
				fprintf(cnt->stream, "%lld", vm_read_as(s64, value));
				break;
			default:
				fprintf(cnt->stream, "<INVALID>");
			}
		} else {
			switch (type->store_size_bytes) {
			case 1:
				fprintf(cnt->stream, "%u", vm_read_as(u8, value));
				break;
			case 2:
				fprintf(cnt->stream, "%u", vm_read_as(u16, value));
				break;
			case 4:
				fprintf(cnt->stream, "%u", vm_read_as(u32, value));
				break;
			case 8:
				fprintf(cnt->stream, "%llu", vm_read_as(u64, value));
				break;
			default:
				fprintf(cnt->stream, "<INVALID>");
			}
		}
		break;
	}

	case MIR_TYPE_REAL:
		switch (type->store_size_bytes) {
		case 4:
			fprintf(cnt->stream, "%f", vm_read_as(f32, value));
			break;
		case 8:
			fprintf(cnt->stream, "%f", vm_read_as(f64, value));
			break;
		default:
			fprintf(cnt->stream, "<INVALID>");
		}
		break;
		break;

	case MIR_TYPE_BOOL: {
		bool b = vm_read_as(bool, value);
		fprintf(cnt->stream, "%s", b ? "true" : "false");
		break;
	}

	case MIR_TYPE_TYPE: {
		MirType *type2 = vm_read_as(MirType *, value);
		print_type(cnt, type2, false, false);
		break;
	}

	case MIR_TYPE_PTR: {
		if (mir_deref_type(type)->kind == MIR_TYPE_FN) {
			MirFn *fn = vm_read_as(MirFn *, value);

			if (fn) {
				fprintf(cnt->stream, "&%s", fn->linkage_name);
			} else {
				fprintf(cnt->stream, "null");
			}
		} else {
			VMStackPtr ptr = vm_read_as(VMStackPtr, value);
			fprintf(cnt->stream, "%p", ptr);
		}
		break;
	}

	case MIR_TYPE_NULL:
		fprintf(cnt->stream, "null");
		break;

	case MIR_TYPE_STRING:
		fprintf(cnt->stream, "{");

		MirType * elem_type = mir_get_struct_elem_type(type, 0);
		ptrdiff_t offset    = vm_get_struct_elem_offset(cnt->assembly, type, 0);
		_print_const_value(cnt, elem_type, value + offset);

		fprintf(cnt->stream, ",\"");

		offset             = vm_get_struct_elem_offset(cnt->assembly, type, 1);
		VMStackPtr str_ptr = value + offset;
		str_ptr            = VM_STACK_PTR_DEREF(str_ptr);
		fprintf(cnt->stream, "%s\"}", (char *)str_ptr);
		break;

	case MIR_TYPE_DYNARR:
	case MIR_TYPE_SLICE:
	case MIR_TYPE_VARGS:
	case MIR_TYPE_STRUCT: {
		fprintf(cnt->stream, "{");

		MirMember *it;
		TSA_FOREACH(type->data.strct.members, it)
		{
			MirType *       member_type = it->type;
			const ptrdiff_t offset2 =
			    vm_get_struct_elem_offset(cnt->assembly, type, (u32)i);
			_print_const_value(cnt, member_type, value + offset2);
			if (i < (usize)type->data.strct.members->size - 1)
				fprintf(cnt->stream, ",");
		}

		fprintf(cnt->stream, "}");
		break;
	}

	case MIR_TYPE_ARRAY: {
		fprintf(cnt->stream, "[");

		MirType *elem_type2 = type->data.array.elem_type;
		for (u32 i = 0; i < (u32)type->data.array.len; ++i) {
			const ptrdiff_t offset2 = vm_get_array_elem_offset(type, i);
			_print_const_value(cnt, elem_type2, value + offset2);
			if (i < type->data.array.len - 1) fprintf(cnt->stream, ",");
		}

		fprintf(cnt->stream, "]");
		break;
	}

	default:
		fprintf(cnt->stream, "<cannot read value>");
	}
}

static void
print_instr_set_initializer(Context *cnt, MirInstrSetInitializer *si);

static void
print_instr_toany(Context *cnt, MirInstrToAny *toany);

static void
print_instr_phi(Context *cnt, MirInstrPhi *phi);

static void
print_instr_cast(Context *cnt, MirInstrCast *cast);

static void
print_instr_sizeof(Context *cnt, MirInstrSizeof *szof);

static void
print_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info);

static void
print_instr_alignof(Context *cnt, MirInstrAlignof *szof);

static void
print_instr_load(Context *cnt, MirInstrLoad *load);

static void
print_instr_addrof(Context *cnt, MirInstrAddrOf *addrof);

static void
print_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr);

static void
print_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr);

static void
print_instr_cond_br(Context *cnt, MirInstrCondBr *cond_br);

static void
print_instr_compound(Context *cnt, MirInstrCompound *init);

static void
print_instr_vargs(Context *cnt, MirInstrVArgs *vargs);

static void
print_instr_br(Context *cnt, MirInstrBr *br);

static void
print_instr_switch(Context *cnt, MirInstrSwitch *sw);

static void
print_instr_unreachable(Context *cnt, MirInstrUnreachable *unr);

static void
print_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);

static void
print_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn);

static void
print_instr_type_struct(Context *cnt, MirInstrTypeStruct *type_struct);

static void
print_instr_type_enum(Context *cnt, MirInstrTypeEnum *type_enum);

static void
print_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr);

static void
print_instr_type_array(Context *cnt, MirInstrTypeArray *type_array);

static void
print_instr_type_slice(Context *cnt, MirInstrTypeSlice *type_slice);

static void
print_instr_type_dynarr(Context *cnt, MirInstrTypeDynArr *type_dynarr);

static void
print_instr_type_vargs(Context *cnt, MirInstrTypeVArgs *type_vargs);

static void
print_instr_block(Context *cnt, MirInstrBlock *block);

static void
print_instr_decl_var(Context *cnt, MirInstrDeclVar *decl);

static void
print_instr_decl_member(Context *cnt, MirInstrDeclMember *decl);

static void
print_instr_decl_variant(Context *cnt, MirInstrDeclVariant *var);

static void
print_instr_decl_arg(Context *cnt, MirInstrDeclArg *decl);

static void
print_instr_const(Context *cnt, MirInstrConst *ci);

static void
print_instr_ret(Context *cnt, MirInstrRet *ret);

static void
print_instr_store(Context *cnt, MirInstrStore *store);

static void
print_instr_binop(Context *cnt, MirInstrBinop *binop);

static void
print_instr_call(Context *cnt, MirInstrCall *call);

static void
print_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref);

static void
print_instr_unop(Context *cnt, MirInstrUnop *unop);

static void
print_instr_arg(Context *cnt, MirInstrArg *arg);

static void
print_instr(Context *cnt, MirInstr *instr);

/* impl */
void
print_comptime_value_or_id(Context *cnt, MirInstr *instr)
{
	if (!instr) {
		fprintf(cnt->stream, "<invalid>");
		return;
	}

	if (!instr->value.is_comptime || !instr->analyzed) {
		fprintf(cnt->stream, "%%%llu", (unsigned long long)instr->id);
		return;
	}

	/* Value is compile time known constant. */
	if (instr->kind == MIR_INSTR_DECL_REF) {
		fprintf(cnt->stream, "%s", ((MirInstrDeclRef *)instr)->rid->str);
		return;
	}

	print_const_value(cnt, &instr->value);
}

void
print_instr_type_fn(Context *cnt, MirInstrTypeFn *type_fn)
{
	print_instr_head(cnt, &type_fn->base, "const fn");
	fprintf(cnt->stream, "(");
	if (type_fn->args) {
		MirInstr *tmp;
		TSA_FOREACH(type_fn->args, tmp)
		{
			fprintf(cnt->stream, "%%%llu", (unsigned long long)tmp->id);
			if (i + 1 < type_fn->args->size) fprintf(cnt->stream, ", ");
		}
	}

	fprintf(cnt->stream, ")");

	if (type_fn->ret_type)
		fprintf(cnt->stream, " %%%llu", (unsigned long long)type_fn->ret_type->id);
}

void
print_instr_set_initializer(Context *cnt, MirInstrSetInitializer *si)
{
	print_instr_head(cnt, &si->base, "setinit");
	print_comptime_value_or_id(cnt, si->src);
	fprintf(cnt->stream, " -> ");
	MirInstrDeclVar *dest = (MirInstrDeclVar *)si->dest;
	if (dest && dest->var->linkage_name) {
		fprintf(cnt->stream, "%s", dest->var->linkage_name);
	} else {
		print_comptime_value_or_id(cnt, si->dest);
	}
}

void
print_instr_phi(Context *cnt, MirInstrPhi *phi)
{
	print_instr_head(cnt, &phi->base, "phi");

	if (phi->incoming_blocks->size != phi->incoming_values->size) {
		fprintf(cnt->stream, "<value_count_does_not_match_block_count>");
		return;
	}

	MirInstr *     value;
	MirInstrBlock *block;
	const usize    c = phi->incoming_values->size;

	if (c == 0) {
		fprintf(cnt->stream, "<empty incomes>");
	}

	for (usize i = 0; i < c; ++i) {
		value = phi->incoming_values->data[i];
		block = (MirInstrBlock *)phi->incoming_blocks->data[i];

		fprintf(cnt->stream, "[");
		print_comptime_value_or_id(cnt, value);
		fprintf(cnt->stream, ", ");
		fprintf(cnt->stream, "%%%s_%llu", block->name, (unsigned long long)block->base.id);
		fprintf(cnt->stream, "] ");
	}
}

void
print_instr_toany(Context *cnt, MirInstrToAny *toany)
{
	print_instr_head(cnt, &toany->base, "toany");
	print_comptime_value_or_id(cnt, toany->expr);
}

void
print_instr_type_struct(Context *cnt, MirInstrTypeStruct *type_struct)
{
	print_instr_head(cnt, &type_struct->base, "const struct");
	fprintf(cnt->stream, "{");

	TSmallArray_InstrPtr *members = type_struct->members;
	MirInstr *            member;
	TSA_FOREACH(members, member)
	{
		print_comptime_value_or_id(cnt, member);
		if (i + 1 < members->size) fprintf(cnt->stream, ", ");
	}

	fprintf(cnt->stream, "}");
}

void
print_instr_type_enum(Context *cnt, MirInstrTypeEnum *type_enum)
{
	print_instr_head(cnt, &type_enum->base, "const enum");
	fprintf(cnt->stream, "{");

	TSmallArray_InstrPtr *variants = type_enum->variants;
	MirInstr *            variant;
	TSA_FOREACH(variants, variant)
	{
		fprintf(cnt->stream, "%%%llu", (unsigned long long)variant->id);
		if (i + 1 < variants->size) fprintf(cnt->stream, ", ");
	}

	fprintf(cnt->stream, "}");
}

void
print_instr_type_ptr(Context *cnt, MirInstrTypePtr *type_ptr)
{
	print_instr_head(cnt, &type_ptr->base, "const");
	fprintf(cnt->stream, "*%%%llu", (unsigned long long)type_ptr->type->id);
}

void
print_instr_type_array(Context *cnt, MirInstrTypeArray *type_array)
{
	print_instr_head(cnt, &type_array->base, "const");
	fprintf(cnt->stream,
	        "[%%%llu]%%%llu",
	        (unsigned long long)type_array->len->id,
	        (unsigned long long)type_array->elem_type->id);
}

void
print_instr_type_slice(Context *cnt, MirInstrTypeSlice *type_slice)
{
	print_instr_head(cnt, &type_slice->base, "const");
	fprintf(cnt->stream, "[]%%%llu", (unsigned long long)type_slice->elem_type->id);
}

void
print_instr_type_dynarr(Context *cnt, MirInstrTypeDynArr*type_dynarr)
{
	print_instr_head(cnt, &type_dynarr->base, "const");
	fprintf(cnt->stream, "[..]%%%llu", (unsigned long long)type_dynarr->elem_type->id);
}

void
print_instr_type_vargs(Context *cnt, MirInstrTypeVArgs *type_vargs)
{
	print_instr_head(cnt, &type_vargs->base, "const");
	if (!type_vargs->elem_type) return;
	fprintf(cnt->stream, "...%%%llu", (unsigned long long)type_vargs->elem_type->id);
}

void
print_instr_cast(Context *cnt, MirInstrCast *cast)
{
	switch (cast->op) {
	case MIR_CAST_NONE:
		print_instr_head(cnt, &cast->base, "nocast");
		break;
	case MIR_CAST_BITCAST:
		print_instr_head(cnt, &cast->base, "bitcast");
		break;
	case MIR_CAST_SEXT:
		print_instr_head(cnt, &cast->base, "sext");
		break;
	case MIR_CAST_ZEXT:
		print_instr_head(cnt, &cast->base, "zext");
		break;
	case MIR_CAST_TRUNC:
		print_instr_head(cnt, &cast->base, "trunc");
		break;
	case MIR_CAST_FPTOSI:
		print_instr_head(cnt, &cast->base, "fptosi");
		break;
	case MIR_CAST_FPTOUI:
		print_instr_head(cnt, &cast->base, "fptoui");
		break;
	case MIR_CAST_FPTRUNC:
		print_instr_head(cnt, &cast->base, "fptrunc");
		break;
	case MIR_CAST_FPEXT:
		print_instr_head(cnt, &cast->base, "fpext");
		break;
	case MIR_CAST_SITOFP:
		print_instr_head(cnt, &cast->base, "sitofp");
		break;
	case MIR_CAST_UITOFP:
		print_instr_head(cnt, &cast->base, "uitofp");
		break;
	case MIR_CAST_PTRTOINT:
		print_instr_head(cnt, &cast->base, "ptrtoint");
		break;
	case MIR_CAST_INTTOPTR:
		print_instr_head(cnt, &cast->base, "inttoptr");
		break;
	case MIR_CAST_PTRTOBOOL:
		print_instr_head(cnt, &cast->base, "ptrtobool");
		break;
	case MIR_CAST_INVALID:
		print_instr_head(cnt, &cast->base, "<invalid cast>");
		break;
	}

	fprintf(cnt->stream, "%%%llu", (unsigned long long)cast->expr->id);
}

void
print_instr_compound(Context *cnt, MirInstrCompound *init)
{
	print_instr_head(cnt, &init->base, "compound");
	if (init->type) {
		print_comptime_value_or_id(cnt, init->type);
	} else {
		print_type(cnt, init->base.value.type, false, true);
	}

	fprintf(cnt->stream, " {");
	TSmallArray_InstrPtr *values = init->values;
	if (values) {
		MirInstr *value;
		TSA_FOREACH(values, value)
		{
			print_comptime_value_or_id(cnt, value);
			if (i < values->size - 1) fprintf(cnt->stream, ", ");
		}
	} else {
		fprintf(cnt->stream, "<zero initializer>");
	}
	fprintf(cnt->stream, "}");

	if (init->is_naked) fprintf(cnt->stream, " /* naked */");
}

void
print_instr_vargs(Context *cnt, MirInstrVArgs *vargs)
{
	print_instr_head(cnt, &vargs->base, "vargs");
	print_type(cnt, vargs->type, false, true);

	fprintf(cnt->stream, " {");
	TSmallArray_InstrPtr *values = vargs->values;
	if (values) {
		MirInstr *value;
		TSA_FOREACH(values, value)
		{
			print_comptime_value_or_id(cnt, value);
			if (i < values->size - 1) fprintf(cnt->stream, ", ");
		}
	} else {
		fprintf(cnt->stream, "<invalid values>");
	}
	fprintf(cnt->stream, "}");
}

void
print_instr_sizeof(Context *cnt, MirInstrSizeof *szof)
{
	print_instr_head(cnt, &szof->base, "sizeof");
	fprintf(cnt->stream, " ");
	print_comptime_value_or_id(cnt, szof->expr);
}

void
print_instr_type_info(Context *cnt, MirInstrTypeInfo *type_info)
{
	print_instr_head(cnt, &type_info->base, "typeinfo");
	print_comptime_value_or_id(cnt, type_info->expr);
}

void
print_instr_alignof(Context *cnt, MirInstrAlignof *szof)
{
	print_instr_head(cnt, &szof->base, "alignof");
	fprintf(cnt->stream, " ");
	print_comptime_value_or_id(cnt, szof->expr);
}

void
print_instr_elem_ptr(Context *cnt, MirInstrElemPtr *elem_ptr)
{
	print_instr_head(cnt, &elem_ptr->base, "elemptr");
	fprintf(cnt->stream, "%%%llu[", (unsigned long long)elem_ptr->arr_ptr->id);
	print_comptime_value_or_id(cnt, elem_ptr->index);
	fprintf(cnt->stream, "]");
}

void
print_instr_member_ptr(Context *cnt, MirInstrMemberPtr *member_ptr)
{
	print_instr_head(cnt, &member_ptr->base, "memberptr");
	if (!member_ptr->target_ptr) {
		fprintf(cnt->stream, "<unknown>.");
	} else {
		print_comptime_value_or_id(cnt, member_ptr->target_ptr);
		fprintf(cnt->stream, ".");
	}

	if (member_ptr->builtin_id == MIR_BUILTIN_ID_NONE) {
		if (member_ptr->member_ident) {
			fprintf(cnt->stream, "%s", member_ptr->member_ident->data.ident.id.str);
		} else {
			fprintf(cnt->stream, "<unknown>");
		}
	} else {
		switch (member_ptr->builtin_id) {
		case MIR_BUILTIN_ID_ARR_LEN:
			fprintf(cnt->stream, "len");
			break;
		case MIR_BUILTIN_ID_ARR_PTR:
			fprintf(cnt->stream, "ptr");
			break;

		default:
			fprintf(cnt->stream, "<unknown>");
		}
	}
}

void
print_instr_unop(Context *cnt, MirInstrUnop *unop)
{
	print_instr_head(cnt, &unop->base, "unop");

	const char *op = ast_unop_to_str(unop->op);
	fprintf(cnt->stream, "%s", op);
	print_comptime_value_or_id(cnt, unop->expr);
}

void
print_instr_cond_br(Context *cnt, MirInstrCondBr *cond_br)
{
	print_instr_head(cnt, &cond_br->base, "br");
	print_comptime_value_or_id(cnt, cond_br->cond);
	fprintf(cnt->stream,
	        " ? %%%s_%llu : %%%s_%llu",
	        cond_br->then_block->name,
	        (unsigned long long)cond_br->then_block->base.id,
	        cond_br->else_block->name,
	        (unsigned long long)cond_br->else_block->base.id);
}

void
print_instr_arg(Context *cnt, MirInstrArg *arg)
{
	print_instr_head(cnt, &arg->base, "arg");
	fprintf(cnt->stream, "$%u", arg->i);
}

void
print_instr_unreachable(Context *cnt, MirInstrUnreachable *unr)
{
	print_instr_head(cnt, &unr->base, "unreachable");
}

void
print_instr_br(Context *cnt, MirInstrBr *br)
{
	print_instr_head(cnt, &br->base, "br");
	fprintf(cnt->stream,
	        "%%%s_%llu",
	        br->then_block->name,
	        (unsigned long long)br->then_block->base.id);
}

void
print_instr_switch(Context *cnt, MirInstrSwitch *sw)
{
	print_instr_head(cnt, &sw->base, "switch");
	print_comptime_value_or_id(cnt, sw->value);
	fprintf(cnt->stream, " {");

	MirSwitchCase *c;
	for (usize i = 0; i < sw->cases->size; ++i) {
		c = &sw->cases->data[i];

		print_comptime_value_or_id(cnt, c->on_value);
		fprintf(cnt->stream,
		        ": %%%s_%llu",
		        c->block->name,
		        (unsigned long long)c->block->base.id);

		if (i < sw->cases->size - 1) fprintf(cnt->stream, "; ");
	}

	fprintf(cnt->stream,
	        "} else %%%s_%llu",
	        sw->default_block->name,
	        (unsigned long long)sw->default_block->base.id);
}

void
print_instr_load(Context *cnt, MirInstrLoad *load)
{
	print_instr_head(cnt, &load->base, "load");
	print_comptime_value_or_id(cnt, load->src);
}

void
print_instr_addrof(Context *cnt, MirInstrAddrOf *addrof)
{
	print_instr_head(cnt, &addrof->base, "addrof");
	fprintf(cnt->stream, "%%%llu", (unsigned long long)addrof->src->id);
}

void
print_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
	MirVar *var = decl->var;
	BL_ASSERT(var);

	const char *name = var->linkage_name ? var->linkage_name : "<unknown>";

	if (var->is_global) {
		/* global scope variable */
		fprintf(cnt->stream, "\n@%s : ", name);
		print_type(cnt, var->value.type, false, true);
		fprintf(cnt->stream, " %s ", var->is_mutable ? "=" : ":");

		if (var->value.is_comptime) {
			print_const_value(cnt, &var->value);
		} else {
			/* HACK: globals use static allocation segment on the stack so relative
			 * pointer = absolute pointer. */
			VMStackPtr data_ptr = (VMStackPtr)var->rel_stack_ptr;
			_print_const_value(cnt, var->value.type, data_ptr);
		}
	} else {
		/* local scope variable */
		print_instr_head(cnt, &decl->base, "decl");

		fprintf(cnt->stream, "%s : ", name);
		print_type(cnt, var->value.type, false, true);
		if (decl->init) {
			fprintf(cnt->stream, " %s ", var->is_mutable ? "=" : ":");
			print_comptime_value_or_id(cnt, decl->init);
		}
	}

	print_flags(cnt, var->flags);
}

void
print_instr_decl_variant(Context *cnt, MirInstrDeclVariant *var)
{
	print_instr_head(cnt, &var->base, "declvariant");
	BL_ASSERT(var->variant);

	MirVariant *variant = var->variant;
	BL_ASSERT(variant);

	fprintf(cnt->stream, "%s", variant->id->str);

	if (var->value) {
		fprintf(cnt->stream, " :: ");
		print_comptime_value_or_id(cnt, var->value);
	}
}

void
print_instr_decl_arg(Context *cnt, MirInstrDeclArg *decl)
{
	print_instr_head(cnt, &decl->base, "declarg");

	MirArg *arg = decl->arg;
	BL_ASSERT(arg)

	fprintf(cnt->stream, "%s : ", arg->id ? arg->id->str : "-");
	print_comptime_value_or_id(cnt, decl->type);
}

void
print_instr_decl_member(Context *cnt, MirInstrDeclMember *decl)
{
	print_instr_head(cnt, &decl->base, "declmember");

	MirMember *member = decl->member;
	BL_ASSERT(member);

	fprintf(cnt->stream, "%s : ", member->id->str);
	print_comptime_value_or_id(cnt, decl->type);
}

void
print_instr_decl_ref(Context *cnt, MirInstrDeclRef *ref)
{
	print_instr_head(cnt, &ref->base, "declref");

	const char *name = ref->rid->str;
	fprintf(cnt->stream, "%s", name);
	if (ref->accept_incomplete_type) fprintf(cnt->stream, " /* accept incomplete */");
}

void
print_instr_decl_direct_ref(Context *cnt, MirInstrDeclDirectRef *ref)
{
	print_instr_head(cnt, &ref->base, "declref");

	print_comptime_value_or_id(cnt, ref->ref);
	fprintf(cnt->stream, " /* direct */");
}

void
print_instr_const(Context *cnt, MirInstrConst *cnst)
{
	print_instr_head(cnt, &cnst->base, "const");
	print_const_value(cnt, &cnst->base.value);
}

void
print_instr_call(Context *cnt, MirInstrCall *call)
{
	print_instr_head(cnt, &call->base, "call");

	MirFn *callee =
	    mir_is_comptime(call->callee) ? MIR_CEV_READ_AS(MirFn *, &call->callee->value) : NULL;
	const char *callee_name = callee ? callee->linkage_name : NULL;
	if (callee_name)
		fprintf(cnt->stream, "@%s", callee_name);
	else
		fprintf(cnt->stream, "%%%llu", (unsigned long long)call->callee->id);

	fprintf(cnt->stream, "(");
	if (call->args) {
		MirInstr *tmp;
		TSA_FOREACH(call->args, tmp)
		{
			print_comptime_value_or_id(cnt, tmp);
			if (i < call->args->size - 1) fprintf(cnt->stream, ", ");
		}
	}
	fprintf(cnt->stream, ")");
}

void
print_instr_ret(Context *cnt, MirInstrRet *ret)
{
	print_instr_head(cnt, &ret->base, "ret");
	if (ret->value) print_comptime_value_or_id(cnt, ret->value);
}

void
print_instr_store(Context *cnt, MirInstrStore *store)
{
	print_instr_head(cnt, &store->base, "store");
	BL_ASSERT(store->src && store->src);
	print_comptime_value_or_id(cnt, store->src);
	fprintf(cnt->stream, " -> %%%llu", (unsigned long long)store->dest->id);
	// print_comptime_value_or_id(cnt,store->dest);
}

void
print_instr_binop(Context *cnt, MirInstrBinop *binop)
{
	print_instr_head(cnt, &binop->base, "binop");
	BL_ASSERT(binop->lhs && binop->rhs);
	const char *op = ast_binop_to_str(binop->op);
	print_comptime_value_or_id(cnt, binop->lhs);
	fprintf(cnt->stream, " %s ", op);
	print_comptime_value_or_id(cnt, binop->rhs);
}

void
print_instr_block(Context *cnt, MirInstrBlock *block)
{
	const bool is_global = !block->owner_fn;
	if (block->base.prev || is_global) fprintf(cnt->stream, "\n");

#if BL_DEBUG
	fprintf(cnt->stream,
	        "%%%s_%llu (%u):",
	        block->name,
	        (unsigned long long)block->base.id,
	        block->base.ref_count);
#else
	fprintf(cnt->stream, "%%%s_%llu:", block->name, (unsigned long long)block->base.id);
#endif

	if (is_global) {
		fprintf(cnt->stream, " {\n");
	} else {
		if (!block->base.ref_count)
			fprintf(cnt->stream, " /* NEVER REACHED */\n");
		else
			fprintf(cnt->stream, "\n");
	}

	MirInstr *tmp = block->entry_instr;

	while (tmp) {
		print_instr(cnt, tmp);
		tmp = tmp->next;
	}

	if (is_global) {
		fprintf(cnt->stream, "}");
	}
}

void
print_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
	MirFn *fn = MIR_CEV_READ_AS(MirFn *, &fn_proto->base.value);
	BL_ASSERT(fn);

	fprintf(cnt->stream, "\n");

	if (fn_proto->base.analyzed) fprintf(cnt->stream, "/* analyzed */\n");
	if (!fn->emit_llvm) fprintf(cnt->stream, "/* no LLVM */\n");

	if (fn->linkage_name)
		fprintf(cnt->stream, "@%s ", fn->linkage_name);
	else
		fprintf(cnt->stream, "@%llu ", (unsigned long long)fn_proto->base.id);

	if (fn->ref_count >= 0)
		fprintf(cnt->stream, "(%d) ", fn->ref_count);
	else
		fprintf(cnt->stream, "(-) ");

	fprintf(cnt->stream, ": ");
	print_type(cnt, fn->type, false, false);
	fprintf(cnt->stream, " : ");

	print_flags(cnt, fn->flags);

	MirInstrBlock *tmp = fn->first_block;
	if (!tmp) return;
	fprintf(cnt->stream, "{\n");
	while (tmp) {
		print_instr_block(cnt, tmp);
		tmp = (MirInstrBlock *)tmp->base.next;
	}
	fprintf(cnt->stream, "}");
}

/* public */
void
print_instr(Context *cnt, MirInstr *instr)
{
#if !PRINT_ANALYZED_COMPTIMES
	if ((instr->owner_block || instr->kind == MIR_INSTR_BLOCK) &&
	    (instr->kind != MIR_INSTR_DECL_VAR) && instr->value.is_comptime && instr->analyzed)
		return;
#endif

	switch (instr->kind) {
	case MIR_INSTR_BLOCK:
		print_instr_block(cnt, (MirInstrBlock *)instr);
		break;
	case MIR_INSTR_INVALID:
		fprintf(cnt->stream, "INVALID");
		break;
	case MIR_INSTR_UNREACHABLE:
		print_instr_unreachable(cnt, (MirInstrUnreachable *)instr);
		break;
	case MIR_INSTR_DECL_VAR:
		print_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
		break;
	case MIR_INSTR_DECL_VARIANT:
		print_instr_decl_variant(cnt, (MirInstrDeclVariant *)instr);
		break;
	case MIR_INSTR_DECL_MEMBER:
		print_instr_decl_member(cnt, (MirInstrDeclMember *)instr);
		break;
	case MIR_INSTR_DECL_ARG:
		print_instr_decl_arg(cnt, (MirInstrDeclArg *)instr);
		break;
	case MIR_INSTR_CONST:
		print_instr_const(cnt, (MirInstrConst *)instr);
		break;
	case MIR_INSTR_LOAD:
		print_instr_load(cnt, (MirInstrLoad *)instr);
		break;
	case MIR_INSTR_STORE:
		print_instr_store(cnt, (MirInstrStore *)instr);
		break;
	case MIR_INSTR_RET:
		print_instr_ret(cnt, (MirInstrRet *)instr);
		break;
	case MIR_INSTR_BINOP:
		print_instr_binop(cnt, (MirInstrBinop *)instr);
		break;
	case MIR_INSTR_CALL:
		print_instr_call(cnt, (MirInstrCall *)instr);
		break;
	case MIR_INSTR_FN_PROTO:
		print_instr_fn_proto(cnt, (MirInstrFnProto *)instr);
		break;
	case MIR_INSTR_DECL_REF:
		print_instr_decl_ref(cnt, (MirInstrDeclRef *)instr);
		break;
	case MIR_INSTR_TYPE_FN:
		print_instr_type_fn(cnt, (MirInstrTypeFn *)instr);
		break;
	case MIR_INSTR_TYPE_STRUCT:
		print_instr_type_struct(cnt, (MirInstrTypeStruct *)instr);
		break;
	case MIR_INSTR_TYPE_ARRAY:
		print_instr_type_array(cnt, (MirInstrTypeArray *)instr);
		break;
	case MIR_INSTR_TYPE_SLICE:
		print_instr_type_slice(cnt, (MirInstrTypeSlice *)instr);
		break;
	case MIR_INSTR_TYPE_DYNARR:
		print_instr_type_dynarr(cnt, (MirInstrTypeDynArr *)instr);
		break;
	case MIR_INSTR_TYPE_VARGS:
		print_instr_type_vargs(cnt, (MirInstrTypeVArgs *)instr);
		break;
	case MIR_INSTR_TYPE_ENUM:
		print_instr_type_enum(cnt, (MirInstrTypeEnum *)instr);
		break;
	case MIR_INSTR_COND_BR:
		print_instr_cond_br(cnt, (MirInstrCondBr *)instr);
		break;
	case MIR_INSTR_BR:
		print_instr_br(cnt, (MirInstrBr *)instr);
		break;
	case MIR_INSTR_SWITCH:
		print_instr_switch(cnt, (MirInstrSwitch *)instr);
		break;
	case MIR_INSTR_UNOP:
		print_instr_unop(cnt, (MirInstrUnop *)instr);
		break;
	case MIR_INSTR_ARG:
		print_instr_arg(cnt, (MirInstrArg *)instr);
		break;
	case MIR_INSTR_ELEM_PTR:
		print_instr_elem_ptr(cnt, (MirInstrElemPtr *)instr);
		break;
	case MIR_INSTR_TYPE_PTR:
		print_instr_type_ptr(cnt, (MirInstrTypePtr *)instr);
		break;
	case MIR_INSTR_ADDROF:
		print_instr_addrof(cnt, (MirInstrAddrOf *)instr);
		break;
	case MIR_INSTR_MEMBER_PTR:
		print_instr_member_ptr(cnt, (MirInstrMemberPtr *)instr);
		break;
	case MIR_INSTR_CAST:
		print_instr_cast(cnt, (MirInstrCast *)instr);
		break;
	case MIR_INSTR_SIZEOF:
		print_instr_sizeof(cnt, (MirInstrSizeof *)instr);
		break;
	case MIR_INSTR_ALIGNOF:
		print_instr_alignof(cnt, (MirInstrAlignof *)instr);
		break;
	case MIR_INSTR_COMPOUND:
		print_instr_compound(cnt, (MirInstrCompound *)instr);
		break;
	case MIR_INSTR_VARGS:
		print_instr_vargs(cnt, (MirInstrVArgs *)instr);
		break;
	case MIR_INSTR_TYPE_INFO:
		print_instr_type_info(cnt, (MirInstrTypeInfo *)instr);
		break;
	case MIR_INSTR_PHI:
		print_instr_phi(cnt, (MirInstrPhi *)instr);
		break;
	case MIR_INSTR_TOANY:
		print_instr_toany(cnt, (MirInstrToAny *)instr);
		break;
	case MIR_INSTR_DECL_DIRECT_REF:
		print_instr_decl_direct_ref(cnt, (MirInstrDeclDirectRef *)instr);
		break;
	case MIR_INSTR_SET_INITIALIZER:
		print_instr_set_initializer(cnt, (MirInstrSetInitializer *)instr);
		break;
	}

	if (instr->value.is_comptime) fprintf(cnt->stream, " /* comptime */");

	fprintf(cnt->stream, "\n");
}

void
mir_print_assembly(Assembly *assembly, FILE *stream)
{
	Context cnt = {.assembly = assembly, .stream = stream};

	MirInstr *instr;
	TARRAY_FOREACH(MirInstr *, &assembly->MIR.global_instrs, instr)
	{
		print_instr(&cnt, instr);
	}
}
