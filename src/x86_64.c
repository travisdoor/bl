// =================================================================================================
// bl
//
// File:   x86_64.c
// Author: Martin Dorazil
// Date:   7/11/23
//
// Copyright 2023 Martin Dorazil
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
// =================================================================================================

// Experimental x64 backend.

#include "assembly.h"
#include "builder.h"
#include "common.h"
#include "stb_ds.h"
#include "threading.h"
#include "x86_64_instructions.h"

#define BYTE_CODE_BUFFER_SIZE 1024
#define SYMBOL_TABLE_SIZE 1024
#define STRING_TABLE_SIZE (1 * 1024 * 1024)      // 1MB
#define CODE_BLOCK_BUFFER_SIZE (2 * 1024 * 1024) // 2MB

static const u8 call_reg_order[4] = {RCX, RDX, R9, R8};

enum x64_value_kind {
	ADDRESS,
	OFFSET,
	REGISTER,
	IMMEDIATE,
};

struct x64_value {
	enum x64_value_kind kind;
	union {
		u32               address;
		s32               offset;
		enum x64_register reg;
		u64               imm;
	};
};

struct jmp_fixup {
	struct mir_instr_block *target_block;
	u32                     diff_value_position;
	u32                     jmp_position;
};

struct thread_context {
	array(u8) bytes;
	array(IMAGE_SYMBOL) syms;
	array(char) strs;
	array(struct x64_value) values;
	array(struct mir_instr_block *) emit_block_queue;
	array(struct jmp_fixup) jmp_fixups;

	u32 stack_allocation_size;
};

struct context {
	struct assembly *assembly;
	array(struct thread_context) tctx;

	struct {
		pthread_mutex_t mutex;
		array(u8) bytes;
		array(IMAGE_SYMBOL) syms;
		array(char) strs;
	} code;

	pthread_spinlock_t uq_name_lock;
};

// Resources:
// http://www.c-jump.com/CIS77/CPU/x86/lecture.html#X77_0010_real_encoding
// https://www.felixcloutier.com/x86/
// https://courses.cs.washington.edu/courses/cse378/03wi/lectures/LinkerFiles/coff.pdf

static inline u32 get_position(struct thread_context *tctx) {
	return (u32)arrlenu(tctx->bytes);
}

static inline void set_value(struct thread_context *tctx, struct mir_instr *instr, struct x64_value value) {
	arrput(tctx->values, value);
	bassert(instr->backend_value == 0 && "Instruction x64 value already set!");
	instr->backend_value = arrlenu(tctx->values);
}

static inline struct x64_value get_value(struct thread_context *tctx, struct mir_instr *instr) {
	bassert(instr->backend_value != 0);
	return tctx->values[instr->backend_value - 1];
}

static inline void unique_name(struct context *ctx, str_buf_t *dest, const char *prefix, const str_t name) {
	pthread_spin_lock(&ctx->uq_name_lock);
	static u64 n = 0;
	str_buf_append_fmt(dest, "{s}{str}{u64}", prefix, name, n++);
	pthread_spin_unlock(&ctx->uq_name_lock);
}

s32 add_code(struct thread_context *tctx, const void *buf, s32 len) {
	const u32 i = get_position(tctx);
	arrsetcap(tctx->bytes, BYTE_CODE_BUFFER_SIZE);
	arrsetlen(tctx->bytes, i + len);
	memcpy(&tctx->bytes[i], buf, len);
	return i;
}

// Add new symbol into thread local storage and set it's offset value relative to already generated
// code in the thread local bytes array. This value must be later fixed according to position in the
// final code section.
// Returns offet of the symbol in the binary.
static inline u32 add_sym(struct thread_context *tctx, str_t linkage_name) {
	bassert(linkage_name.len && linkage_name.ptr);
	const u32    offset = get_position(tctx);
	IMAGE_SYMBOL sym    = {
	       .SectionNumber = 1,
	       .Type          = 0x20,
	       .StorageClass  = IMAGE_SYM_CLASS_EXTERNAL,
	       .Value         = offset,
    };

	if (linkage_name.len > 8) {
		const u32 str_offset = (u32)arrlenu(tctx->strs);
		arrsetcap(tctx->strs, 256);
		arrsetlen(tctx->strs, str_offset + linkage_name.len + 1); // +1 zero terminator
		memcpy(&tctx->strs[str_offset], linkage_name.ptr, linkage_name.len);
		tctx->strs[str_offset + linkage_name.len] = '\0';

		sym.N.Name.Long = str_offset + sizeof(u32); // 4 bytes for the table size
	} else {
		memcpy(sym.N.ShortName, linkage_name.ptr, linkage_name.len);
	}

	arrsetcap(tctx->syms, 64);
	arrput(tctx->syms, sym);
	return offset;
}

static inline u64 add_block(struct thread_context *tctx, const str_t name) {
	const u32        address = add_sym(tctx, name);
	struct x64_value value   = {.kind = ADDRESS, .address = address};
	arrput(tctx->values, value);
	return arrlenu(tctx->values);
}

//
// Translate
//

static void allocate_stack(struct thread_context *tctx, struct mir_fn *fn) {
	u32 top = 0;

	for (usize i = 0; i < arrlenu(fn->variables); ++i) {
		struct mir_var *var = fn->variables[i];
		bassert(var);
		if (isnotflag(var->iflags, MIR_VAR_EMIT_LLVM)) continue;
		if (var->ref_count == 0) continue;

		struct mir_type *var_type = var->value.type;
		bassert(var_type);

		const u32 var_size      = (u32)var_type->store_size_bytes;
		const u32 var_alignment = (u32)var->value.type->alignment;

		if (!is_aligned((void *)(usize)top, var_alignment)) {
			top = (u32)(usize)next_aligned((void *)(usize)top, var_alignment);
		}

		blog("Variable: %dB aligned to: %dB at: 0x%x", var_type->store_size_bytes, var_alignment, top);
		struct x64_value value = {.kind = OFFSET, .offset = (s32)top};
		arrput(tctx->values, value);
		var->backend_value = arrlenu(tctx->values);

		top += var_size;
	}

	// Adjust stack memory to 16B.
	top = (u32)(usize)next_aligned((void *)(usize)top, 16);

	// Add shadow space.
	// @Performance: This is needed only in case this function is not a leaf function
	// or maybe just in case we call some stuff from C.
	top += 0x20;

	bassert(is_aligned((void *)(usize)top, 16));

	sub64_ri32(tctx, RSP, top);
	tctx->stack_allocation_size += top;

	blog("Total allocated: %dB", top);
}

static void emit_binop_r_immediate_int(struct thread_context *tctx, enum binop_kind op, const u8 reg, const u64 v, const usize vsize) {
	switch (op) {
	case BINOP_ADD:
		add_r_immediate(tctx, reg, v, vsize);
		break;
	case BINOP_SUB:
		sub_r_immediate(tctx, reg, v, vsize);
		break;
	case BINOP_LESS_EQ:
	case BINOP_GREATER_EQ:
	case BINOP_LESS:
	case BINOP_GREATER:
	case BINOP_NEQ:
	case BINOP_EQ:
		cmp_r_immediate(tctx, reg, v, vsize);
		break;
	default:
		BL_UNIMPLEMENTED;
	}
}

static void emit_binop_rr(struct thread_context *tctx, enum binop_kind op, const u8 reg1, const u8 reg2, const usize vsize) {
	switch (op) {
	case BINOP_ADD:
		add_rr(tctx, reg1, reg2, vsize);
		break;
	case BINOP_SUB:
		BL_UNIMPLEMENTED;
		break;
	case BINOP_LESS_EQ:
	case BINOP_GREATER_EQ:
	case BINOP_LESS:
	case BINOP_GREATER:
	case BINOP_NEQ:
	case BINOP_EQ:
		BL_UNIMPLEMENTED;
		break;
	default:
		BL_UNIMPLEMENTED;
	}
}

// @Cleanup: do we need ctx?
static void emit_instr(struct context *ctx, struct thread_context *tctx, struct mir_instr *instr) {
	bassert(instr->state == MIR_IS_COMPLETE && "Attempt to emit instruction in incomplete state!");
	// @Incomplete
	// if (!mir_type_has_llvm_representation((instr->value.type))) return state;
	switch (instr->kind) {

	case MIR_INSTR_FN_PROTO: {
		struct mir_instr_fn_proto *fn_proto = (struct mir_instr_fn_proto *)instr;
		struct mir_fn             *fn       = MIR_CEV_READ_AS(struct mir_fn *, &fn_proto->base.value);
		bmagic_assert(fn);

		str_t linkage_name = str_empty;
		if (isflag(fn->flags, FLAG_INTRINSIC)) {
			BL_UNIMPLEMENTED;
		} else {
			linkage_name = fn->linkage_name;
		}
		bassert(linkage_name.len && "Invalid function name!");

		// External functions does not have any body block.
		if (isflag(fn->flags, FLAG_EXTERN) || isflag(fn->flags, FLAG_INTRINSIC)) {
			BL_UNIMPLEMENTED;
			return;
		}

		add_sym(tctx, linkage_name);

		// Prologue
		push64_r(tctx, RBP);
		mov64_rr(tctx, RBP, RSP);

		// Generate all blocks in the function body.
		arrput(tctx->emit_block_queue, fn->first_block);
		while (arrlenu(tctx->emit_block_queue)) {
			struct mir_instr_block *block = arrpop(tctx->emit_block_queue);
			bassert(block->terminal);
			emit_instr(ctx, tctx, (struct mir_instr *)block);
		}

		break;
	}

	case MIR_INSTR_BLOCK: {
		struct mir_instr_block *block = (struct mir_instr_block *)instr;
		if (block->base.backend_value) break;

		struct mir_fn *fn        = block->owner_fn;
		const bool     is_global = fn == NULL;
		if (!block->terminal) babort("Block '%s', is not terminated", block->name);

		if (is_global) {
			BL_UNIMPLEMENTED;
		} else {
			str_buf_t name = get_tmp_str();
#if BL_DEBUG
			unique_name(ctx, &name, ".", block->name);
#else
			unique_name(ctx, &name, ".", cstr("B"));
#endif

			block->base.backend_value = add_block(tctx, str_buf_view(name));
			put_tmp_str(name);

			if (fn->first_block == block) {
				allocate_stack(tctx, fn);
			}
		}

		// Generate all instructions in the block.
		struct mir_instr *instr = block->entry_instr;
		while (instr) {
			emit_instr(ctx, tctx, instr);
			instr = instr->next;
		}
		break;
	}

	case MIR_INSTR_LOAD: {
		struct mir_instr_load *load = (struct mir_instr_load *)instr;
		struct mir_type       *type = load->base.value.type;
		bassert(type->kind == MIR_TYPE_INT);
		bassert(tctx->values[load->src->backend_value - 1].kind == OFFSET);
		const s32 rbp_offset_bytes = tctx->values[load->src->backend_value - 1].offset;

		const s32 reg_index = load->base.reg_hint;
		if (reg_index >= static_arrlenu(call_reg_order)) {
			BL_UNIMPLEMENTED;
		}
		const u8 reg = call_reg_order[reg_index];
		mov_r_rbp_offset(tctx, reg, rbp_offset_bytes, type->store_size_bytes);

		set_value(tctx, &load->base, (struct x64_value){.kind = REGISTER, .reg = reg});
		break;
	}

	case MIR_INSTR_STORE: {
		struct mir_instr_store *store = (struct mir_instr_store *)instr;
		struct mir_type        *type  = store->src->value.type;
		bassert(store->dest->backend_value);

		struct x64_value dest_value = get_value(tctx, store->dest);
		bassert(dest_value.kind == OFFSET);

		if (store->src->kind == MIR_INSTR_CONST) {
			switch (type->kind) {
			case MIR_TYPE_INT: {
				const u64 v = vm_read_int(type, store->src->value.data);
				mov_rbp_offset_immediate(tctx, dest_value.offset, v, type->store_size_bytes);
				break;
			}
			default:
				BL_UNIMPLEMENTED;
			}
		} else {
			bassert(type->kind == MIR_TYPE_INT && type->store_size_bytes == 4);
			bassert(is_byte_disp(dest_value.offset));
			struct x64_value src_value = get_value(tctx, store->src);
			bassert(src_value.kind == REGISTER);
			mov32_mr(tctx, RBP, (u8)dest_value.offset, src_value.reg);
		}

		break;
	}

	case MIR_INSTR_BINOP: {
		// RCX op RDX
		// IMM op RDX
		// RCX op IMM
		//
		// And stores result into RCX.

		struct mir_instr_binop *binop = (struct mir_instr_binop *)instr;
		struct mir_type        *type  = binop->lhs->value.type;
		bassert(type->kind == MIR_TYPE_INT && type->store_size_bytes == 4);

		struct x64_value lhs_value = get_value(tctx, binop->lhs);
		struct x64_value rhs_value = get_value(tctx, binop->rhs);

		if (lhs_value.kind == IMMEDIATE) {
			const u8 reg = call_reg_order[binop->lhs->reg_hint];
			mov32_ri32(tctx, reg, (u32)lhs_value.imm);
			lhs_value.kind = REGISTER;
			lhs_value.reg  = reg;
		}

		bassert(lhs_value.kind == REGISTER);

		if (rhs_value.kind == IMMEDIATE) {
			emit_binop_r_immediate_int(tctx, binop->op, lhs_value.reg, rhs_value.imm, type->store_size_bytes);
		} else {
			bassert(rhs_value.kind == REGISTER);
			emit_binop_rr(tctx, binop->op, lhs_value.reg, rhs_value.reg, type->store_size_bytes);
		}

		set_value(tctx, instr, (struct x64_value){.kind = REGISTER, .reg = lhs_value.reg});
		break;
	}

	case MIR_INSTR_RET: {
		// Epilogue
		if (tctx->stack_allocation_size) {
			// Cleanup stack allocations.
			add64_ri32(tctx, RSP, tctx->stack_allocation_size);
		}
		pop64_r(tctx, RBP);
		ret(tctx);
		break;
	}

	case MIR_INSTR_BR: {
		struct mir_instr_br    *br         = (struct mir_instr_br *)instr;
		struct mir_instr_block *then_block = br->then_block;
		bassert(then_block);

		if (then_block->base.backend_value) {
			const u32 diff_value_position = jmp_relative_i32(tctx, 0x0);

			struct jmp_fixup fixup = {
			    .target_block        = then_block,
			    .diff_value_position = diff_value_position,
			    .jmp_position        = get_position(tctx),
			};
			arrput(tctx->jmp_fixups, fixup);
		} else {
			// Generate block immediately after...
			arrput(tctx->emit_block_queue, then_block);
		}

		break;
	}

	case MIR_INSTR_COND_BR: {
		struct mir_instr_cond_br *br = (struct mir_instr_cond_br *)instr;
		bassert(br->cond && br->then_block && br->else_block);
		bassert(br->else_block->base.backend_value == 0);

		u32 diff_value_position = 0;

		// In case the condition is binary operation we swap the logic with attemt to generate the 'then'
		// branch immediately after conditional jump to safe one jmp instruction.
		bassert(br->cond->kind == MIR_INSTR_BINOP);
		struct mir_instr_binop *cond_binop = (struct mir_instr_binop *)br->cond;
		switch (cond_binop->op) {
		case BINOP_EQ:
			diff_value_position = jne_relative_i32(tctx, 0x0);
			break;
		case BINOP_NEQ:
			diff_value_position = je_relative_i32(tctx, 0x0);
			break;
		case BINOP_LESS:
			diff_value_position = jge_relative_i32(tctx, 0x0);
			break;
		case BINOP_GREATER:
			diff_value_position = jle_relative_i32(tctx, 0x0);
			break;
		case BINOP_LESS_EQ:
			diff_value_position = jg_relative_i32(tctx, 0x0);
			break;
		case BINOP_GREATER_EQ:
			diff_value_position = jl_relative_i32(tctx, 0x0);
			break;
		default:
			BL_UNIMPLEMENTED;
		}

		struct jmp_fixup fixup = {
		    .target_block        = br->else_block,
		    .diff_value_position = diff_value_position,
		    .jmp_position        = get_position(tctx),
		};
		arrput(tctx->jmp_fixups, fixup);

		if (br->else_block->base.backend_value == 0) {
			arrput(tctx->emit_block_queue, br->else_block);
		}

		// Then block immediately after conditional break.
		bassert(br->then_block->base.backend_value == 0);
		arrput(tctx->emit_block_queue, br->then_block);

		break;
	}

	case MIR_INSTR_CONST: {
		struct mir_instr_const *cnst = (struct mir_instr_const *)instr;
		struct mir_type        *type = cnst->base.value.type;
		bassert(type);
		struct x64_value value = {.kind = IMMEDIATE};

		switch (type->kind) {
		case MIR_TYPE_INT:
			value.imm = vm_read_int(type, cnst->base.value.data);
			break;
		default:
			BL_UNIMPLEMENTED;
		}
		set_value(tctx, instr, value);
		break;
	}

	case MIR_INSTR_DECL_VAR: {
		struct mir_instr_decl_var *decl = (struct mir_instr_decl_var *)instr;
		struct mir_var            *var  = decl->var;
		bassert(var);
		if (var->ref_count == 0) break;
		if (!mir_type_has_llvm_representation(var->value.type)) break;
		bassert(var->backend_value);

		bassert(tctx->values[var->backend_value - 1].kind == OFFSET);
		const s32 rbp_offset_bytes = tctx->values[var->backend_value - 1].offset;

		if (decl->init) {
			if (decl->init->kind == MIR_INSTR_COMPOUND) {
				BL_UNIMPLEMENTED;
			} else if (decl->init->kind == MIR_INSTR_ARG) {
				BL_UNIMPLEMENTED;
			} else {
				struct mir_type *type = var->value.type;
				bassert(type);
				if (decl->init->kind == MIR_INSTR_CONST) {
					switch (type->kind) {
					case MIR_TYPE_INT: {
						const u64 v = vm_read_int(type, decl->init->value.data);
						mov_rbp_offset_immediate(tctx, rbp_offset_bytes, v, type->store_size_bytes);
						break;
					}
					default:
						BL_UNIMPLEMENTED;
					}
				} else {
					bassert(type->store_size_bytes == 4);
					mov32_mr(tctx, RBP, (u8)rbp_offset_bytes, RAX);
				}
			}
		}
		break;
	}

	case MIR_INSTR_DECL_REF: {
		struct mir_instr_decl_ref *ref = (struct mir_instr_decl_ref *)instr;

		struct scope_entry *entry = ref->scope_entry;
		bassert(entry);
		switch (entry->kind) {
		case SCOPE_ENTRY_VAR: {
			struct mir_var *var = entry->data.var;
			if (isflag(var->iflags, MIR_VAR_GLOBAL)) {
				BL_UNIMPLEMENTED;
			} else {
				ref->base.backend_value = var->backend_value;
			}
			break;
		}
		case SCOPE_ENTRY_FN: {
			BL_UNIMPLEMENTED;
			break;
		}
		default:
			BL_UNIMPLEMENTED;
		}
		bassert(ref->base.llvm_value);
		break;
	}

	case MIR_INSTR_DECL_DIRECT_REF: {
		struct mir_instr_decl_direct_ref *ref = (struct mir_instr_decl_direct_ref *)instr;
		bassert(ref->ref && ref->ref->kind == MIR_INSTR_DECL_VAR);
		struct mir_var *var = ((struct mir_instr_decl_var *)ref->ref)->var;
		bassert(var);
		if (isflag(var->iflags, MIR_VAR_GLOBAL)) {
			BL_UNIMPLEMENTED;
		} else {
			ref->base.backend_value = var->backend_value;
		}
		break;
	}
	default:
		blog("Missing implementation for emmiting '%s' instruction.", mir_instr_name(instr));
	}
}

static void fixup_jump_offsets(struct thread_context *tctx) {
	for (usize i = 0; i < arrlenu(tctx->jmp_fixups); ++i) {
		struct jmp_fixup fixup = tctx->jmp_fixups[i];
		bassert(fixup.target_block);
		const u64 backend_value = fixup.target_block->base.backend_value;
		if (backend_value == 0) {
			babort("Cannot resolve relative jump target block!");
		}

		bassert(tctx->values[backend_value - 1].kind == ADDRESS);
		const s32 block_position = (s32)tctx->values[backend_value - 1].address;
		const s32 jmp_position   = (s32)fixup.jmp_position;
		const s32 diff           = block_position - jmp_position;
		memcpy(&tctx->bytes[fixup.diff_value_position], &diff, sizeof(s32));
	}
}

static void job(struct job_context *job_ctx) {
	struct context        *ctx          = job_ctx->x64.ctx;
	const u32              thread_index = job_ctx->thread_index;
	struct thread_context *tctx         = &ctx->tctx[thread_index];

	// Reset buffers
	arrsetlen(tctx->bytes, 0);
	arrsetlen(tctx->syms, 0);
	arrsetlen(tctx->strs, 0);
	arrsetlen(tctx->values, 0);
	arrsetlen(tctx->emit_block_queue, 0);
	arrsetlen(tctx->jmp_fixups, 0);
	tctx->stack_allocation_size = 0;

	struct mir_instr *top_instr = job_ctx->x64.top_instr;
	blog("Kind = %s", mir_instr_name(top_instr));

	emit_instr(ctx, tctx, top_instr);

	fixup_jump_offsets(tctx);

	// Write top-level function generated code into the code section if there is any generated code
	// present.
	const usize bytes_len = arrlenu(tctx->bytes);
	const usize syms_len  = arrlenu(tctx->syms);
	const usize strs_len  = arrlenu(tctx->strs);

	if (bytes_len == 0) return;

	pthread_mutex_lock(&ctx->code.mutex);

	const usize section_offset      = arrlenu(ctx->code.bytes);
	const usize string_table_offset = arrlenu(ctx->code.strs);
	const usize syms_offset         = arrlenu(ctx->code.syms);

	arrsetcap(ctx->code.bytes, CODE_BLOCK_BUFFER_SIZE);
	arrsetlen(ctx->code.bytes, section_offset + bytes_len);
	memcpy(&ctx->code.bytes[section_offset], tctx->bytes, bytes_len);

	// Fixup symbol positions.
	for (usize i = 0; i < arrlenu(tctx->syms); ++i) {
		IMAGE_SYMBOL *sym = &tctx->syms[i];
		sym->Value += (DWORD)section_offset;
		if (sym->N.Name.Long) {
			sym->N.Name.Long += (DWORD)string_table_offset;
		}
	}

	// Copy symbol table to global one.
	arrsetcap(ctx->code.syms, SYMBOL_TABLE_SIZE);
	arrsetlen(ctx->code.syms, syms_offset + syms_len);
	memcpy(&ctx->code.syms[syms_offset], tctx->syms, syms_len * sizeof(IMAGE_SYMBOL));

	// Copy string table to global one.
	arrsetcap(ctx->code.strs, STRING_TABLE_SIZE);
	arrsetlen(ctx->code.strs, string_table_offset + strs_len);
	memcpy(&ctx->code.strs[string_table_offset], tctx->strs, strs_len);

	pthread_mutex_unlock(&ctx->code.mutex);
}

static void create_object_file(struct context *ctx) {
	usize section_data_pointer = IMAGE_SIZEOF_FILE_HEADER + IMAGE_SIZEOF_SECTION_HEADER;
	usize sym_pointer          = section_data_pointer + arrlenu(ctx->code.bytes);

	IMAGE_FILE_HEADER header = {
	    .Machine              = IMAGE_FILE_MACHINE_AMD64,
	    .NumberOfSections     = 1,
	    .PointerToSymbolTable = (DWORD)sym_pointer,
	    .NumberOfSymbols      = (DWORD)arrlenu(ctx->code.syms),
	    .TimeDateStamp        = (DWORD)time(0),
	};

	IMAGE_SECTION_HEADER section = {
	    .Name             = ".text",
	    .Characteristics  = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES | IMAGE_SCN_MEM_EXECUTE,
	    .SizeOfRawData    = (DWORD)arrlenu(ctx->code.bytes),
	    .PointerToRawData = (DWORD)section_data_pointer,
	};

	str_buf_t            buf    = get_tmp_str();
	const struct target *target = ctx->assembly->target;
	const char          *name   = target->name;
	str_buf_append_fmt(&buf, "{str}/{s}.{s}", target->out_dir, name, OBJ_EXT);
	FILE *file = fopen(str_to_c(buf), "wb");
	if (!file) {
		// @Incomplete: Handle properly!
		babort("Cannot create the output file.");
	}

	// Write COFF header
	fwrite(&header, 1, IMAGE_SIZEOF_FILE_HEADER, file);
	// Section header
	fwrite(&section, 1, IMAGE_SIZEOF_SECTION_HEADER, file);
	// Write section code
	fwrite(ctx->code.bytes, 1, arrlenu(ctx->code.bytes), file);
	// Symbol table
	fwrite(ctx->code.syms, IMAGE_SIZEOF_SYMBOL, arrlenu(ctx->code.syms), file);
	// String table
	u32 strs_len = (u32)arrlenu(ctx->code.strs) + sizeof(u32); // See the COFF specifiction sec 5.6.
	fwrite(&strs_len, 1, sizeof(u32), file);
	fwrite(ctx->code.strs, 1, strs_len, file);

	fclose(file);
}

void x86_64run(struct assembly *assembly) {
	builder_warning("Using experimental x64 backend.");
	const u32 thread_count = get_thread_count();

	struct context ctx = {
	    .assembly = assembly,
	};

	pthread_mutex_init(&ctx.code.mutex, NULL);

	arrsetlen(ctx.tctx, thread_count);
	bl_zeromem(ctx.tctx, thread_count * sizeof(struct thread_context));

	// Submit top level instructions...
	for (usize i = 0; i < arrlenu(assembly->MIR.exported_instrs); ++i) {
		struct job_context job_ctx = {.x64 = {.ctx = &ctx, .top_instr = assembly->MIR.exported_instrs[i]}};
		submit_job(&job, &job_ctx);
	}
	wait_threads();

	// Write the output file.
	create_object_file(&ctx);

	// Cleanup
	for (usize i = 0; i < arrlenu(ctx.tctx); ++i) {
		struct thread_context *tctx = &ctx.tctx[i];
		arrfree(tctx->bytes);
		arrfree(tctx->syms);
		arrfree(tctx->strs);
		arrfree(tctx->values);
		arrfree(tctx->emit_block_queue);
		arrfree(tctx->jmp_fixups);
	}
	arrfree(ctx.tctx);
	arrfree(ctx.code.bytes);
	arrfree(ctx.code.syms);
	arrfree(ctx.code.strs);
	pthread_mutex_destroy(&ctx.code.mutex);
}
