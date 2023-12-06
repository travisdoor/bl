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

#define UNUSED_REGISTER_MAP_VALUE -1
#define RESERVED_REGISTER_MAP_VALUE -2

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
	union {
		struct mir_instr_fn_proto *fn_proto;
		struct mir_instr_block    *target_block;
	};

	hash_t hash;
	// Directly points to u32 value to be fixed.
	u64 virtual_address;
	// Points to the next instruction.
	u64 jmp_position;
};

struct symbol_table_entry {
	hash_t key;
	s32    symbol_table_index;
};

struct scheduled_entry {
	hash_t key;
};

struct thread_context {
	array(u8) bytes;
	array(IMAGE_SYMBOL) syms;
	array(char) strs;
	array(struct x64_value) values;
	array(struct mir_instr_block *) emit_block_queue;
	array(struct jmp_fixup) jmp_fixups;
	array(struct jmp_fixup) call_fixups;

	// Contains mapping of all available register of the target to x64_values.
	s32 register_table[REGISTER_COUNT];

	struct {
		// Contains offset to the byte code pointing to the actual value of sub instruction used
		// to preallocate stack in prologue, so we can allocate more memory later as needed.
		u32 alloc_value_offset;

		u32 arg_cache_allocated_size;
	} stack;
};

struct context {
	struct assembly *assembly;
	array(struct thread_context) tctx;

	struct {
		pthread_mutex_t mutex;
		array(u8) bytes;
		array(IMAGE_SYMBOL) syms;
		array(IMAGE_RELOCATION) relocs;
		array(char) strs;
	} code;

	hash_table(struct symbol_table_entry) symbol_table;
	hash_table(struct scheduled_entry) scheduled_for_generation;

	array(struct jmp_fixup) call_fixups;

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

#define get_value(tctx, V) _Generic((V),                \
	                                struct mir_instr *  \
	                                : _get_value_instr, \
	                                  struct mir_var *  \
	                                : _get_value_var)((tctx), (V))

static inline struct x64_value _get_value_instr(struct thread_context *tctx, struct mir_instr *instr) {
	bassert(instr->backend_value != 0);
	return tctx->values[instr->backend_value - 1];
}

static inline struct x64_value _get_value_var(struct thread_context *tctx, struct mir_var *var) {
	bassert(var->backend_value != 0);
	return tctx->values[var->backend_value - 1];
}

static inline void unique_name(struct context *ctx, str_buf_t *dest, const char *prefix, const str_t name) {
	pthread_spin_lock(&ctx->uq_name_lock);
	static u64 n = 0;
	str_buf_append_fmt(dest, "{s}{str}{u64}", prefix, name, n++);
	pthread_spin_unlock(&ctx->uq_name_lock);
}

s32 add_code(struct thread_context *tctx, const void *buf, s32 len) {
	const u32 i = get_position(tctx);
	arrsetlen(tctx->bytes, i + len);
	memcpy(&tctx->bytes[i], buf, len);
	return i;
}

// Add new symbol into thread local storage and set it's offset value relative to already generated
// code in the thread local bytes array. This value must be later fixed according to position in the
// final code section.
//
// External symbol does not record section number (value is 0). This is later used by linker as a hint
// the symbol is linked from other binary. Also there is no offset (value) pointing to our binary (value is 0).
//
// Returns offet of the symbol in the binary.
static inline u32 add_sym(struct thread_context *tctx, str_t linkage_name, u8 storage_class, bool is_external) {
	bassert(linkage_name.len && linkage_name.ptr);
	const u32    offset = get_position(tctx);
	IMAGE_SYMBOL sym    = {
	       .SectionNumber = is_external ? 0 : 1,
	       .Type          = 0x20,
	       .StorageClass  = storage_class,
	       .Value         = is_external ? 0 : offset,
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
	const u32        address = add_sym(tctx, name, IMAGE_SYM_CLASS_LABEL, false);
	struct x64_value value   = {.kind = ADDRESS, .address = address};
	arrput(tctx->values, value);
	return arrlenu(tctx->values);
}

//
// Translate
//

static inline usize get_stack_allocation_size(struct thread_context *tctx) {
	bassert(tctx->stack.alloc_value_offset > 0);
	return *(u32 *)(tctx->bytes + tctx->stack.alloc_value_offset);
}

static inline s32 allocate_stack_memory(struct thread_context *tctx, usize size) {
	const usize allocated = get_stack_allocation_size(tctx);
	if (allocated + size > 0xFFFFFFFF) {
		babort("Stack allocation is too big!");
	}
	(*(u32 *)(tctx->bytes + tctx->stack.alloc_value_offset)) += (u32)size;
	return (s32)allocated;
}

/* @Cleanup
static s32 allocated_stack_arg(struct thread_context *tctx, const s32 reg_index) {
    bassert(reg_index > 0);

    usize required_stack_size = MAX(reg_index, 4) * sizeof(u64);
    if (required_stack_size > tctx->stack.arg_cache_allocated_size) {
        const usize stack_allocated = get_stack_allocation_size(tctx);
        usize       to_allocate     = required_stack_size - tctx->stack.arg_cache_allocated_size;
        if (!is_aligned2(to_allocate + stack_allocated, 16)) {
            to_allocate = (next_aligned2(to_allocate + stack_allocated, 16) - stack_allocated);
        }
        blog("Allocate %lluB.", to_allocate);
        allocate_stack_memory(tctx, to_allocate);

        tctx->stack.arg_cache_allocated_size += (u32)to_allocate;
    }

    s32 stack_offset = -(s32)get_stack_allocation_size(tctx);
    stack_offset += sizeof(u64) * (reg_index - 1);
    bassert(stack_offset <= 0);
    return stack_offset;
}
*/

static void allocate_stack_variables(struct thread_context *tctx, struct mir_fn *fn) {
	usize top = 0;

	for (usize i = 0; i < arrlenu(fn->variables); ++i) {
		struct mir_var *var = fn->variables[i];
		bassert(var);
		if (isnotflag(var->iflags, MIR_VAR_EMIT_LLVM)) continue;
		if (var->ref_count == 0) continue;

		struct mir_type *var_type = var->value.type;
		bassert(var_type);

		const u32 var_size      = (u32)var_type->store_size_bytes;
		const u32 var_alignment = (u32)var->value.type->alignment;

		top = next_aligned2(top, var_alignment);

		struct x64_value value = {
		    .kind   = OFFSET,
		    .offset = -(s32)(top + var_size),
		};
		arrput(tctx->values, value);
		var->backend_value = arrlenu(tctx->values);

		top += var_size;
	}

	allocate_stack_memory(tctx, top);
}

static inline s32 vreg_value_index(struct thread_context *tctx, enum x64_register reg) {
	const s32 vi = tctx->register_table[reg];
	bassert(vi >= 0 && vi < arrlen(tctx->values));
	return vi + 1;
}

static inline struct x64_value vreg_value(struct thread_context *tctx, enum x64_register reg) {
	const s32 vi = tctx->register_table[reg];
	bassert(vi >= 0 && vi < arrlen(tctx->values));
	return tctx->values[vi];
}

// @Explain
static void vreg_reserve(struct thread_context *tctx, enum x64_register dest[], s32 num) {
	bassert(num > 0 && num <= REGISTER_COUNT);

	bool set[REGISTER_COUNT];
	s32  set_num = 0;
	for (s32 i = 0; i < REGISTER_COUNT && set_num < num; ++i) {
		if (dest[set_num] != -1 && dest[set_num] != i) {
			set[i] = false;
			continue;
		}
		if (tctx->register_table[i] == UNUSED_REGISTER_MAP_VALUE) {
			dest[set_num++]         = i;
			tctx->register_table[i] = (s32)arrlen(tctx->values);
			struct x64_value value  = {.kind = REGISTER, .reg = i};
			arrput(tctx->values, value);
			set[i] = true;
		} else {
			set[i] = false;
		}
	}
	if (set_num == num) return;
	for (s32 i = 0; i < REGISTER_COUNT && set_num < num; ++i) {
		if (set[i]) continue;
		if (tctx->register_table[i] == RESERVED_REGISTER_MAP_VALUE) continue;
		if (dest[set_num] != -1 && dest[set_num] != i) continue;
		// Spill register.
		const s32 vi = tctx->register_table[i];
		bassert(vi != UNUSED_REGISTER_MAP_VALUE);
		struct x64_value *spill_value = &tctx->values[vi];
		bassert(spill_value->kind == REGISTER);

		const s32 top    = allocate_stack_memory(tctx, 8);
		const s32 offset = -top;
		mov_mr(tctx, RBP, offset, spill_value->reg, 8);
		spill_value->kind   = OFFSET;
		spill_value->offset = offset;

		dest[set_num++]         = i;
		tctx->register_table[i] = (s32)arrlen(tctx->values);
		struct x64_value value  = {.kind = REGISTER, .reg = i};
		arrput(tctx->values, value);
	}

	bassert(set_num == num);
}

// Tries to find some free registers.
static void vreg_begin(struct thread_context *tctx, enum x64_register dest[], s32 num) {
	bassert(num > 0 && num <= REGISTER_COUNT);
	s32 set_num = 0;
	for (s32 i = 0; i < num; ++i) {
		dest[i] = -1;
	}
	for (s32 i = 0; i < REGISTER_COUNT && set_num < num; ++i) {
		if (tctx->register_table[i] == UNUSED_REGISTER_MAP_VALUE) {
			dest[set_num++] = i;
		}
	}
}

static void vreg_end(struct thread_context *tctx, const enum x64_register regs[], s32 num) {
	for (s32 i = 0; i < num; ++i) {
		bassert(regs[i] >= 0);
		bassert(tctx->register_table[regs[i]] == UNUSED_REGISTER_MAP_VALUE);
		tctx->register_table[regs[i]] = (s32)arrlen(tctx->values);
		struct x64_value value        = {.kind = REGISTER, .reg = regs[i]};
		arrput(tctx->values, value);
	}
}

static void spill(struct thread_context *tctx, enum x64_register reg) {
	const s32 vi = tctx->register_table[reg];
	bassert(vi != UNUSED_REGISTER_MAP_VALUE);
	struct x64_value *spill_value = &tctx->values[vi];
	bassert(spill_value->kind == REGISTER);

	const s32 top    = allocate_stack_memory(tctx, 8);
	const s32 offset = -top;
	mov_mr(tctx, RBP, offset, spill_value->reg, 8);
	spill_value->kind         = OFFSET;
	spill_value->offset       = offset;
	tctx->register_table[reg] = UNUSED_REGISTER_MAP_VALUE;
}

static inline void vreg_release(struct thread_context *tctx, const enum x64_register regs[], s32 num) {
	for (s32 i = 0; i < num; ++i) {
		tctx->register_table[regs[i]] = UNUSED_REGISTER_MAP_VALUE;
	}
}

static void emit_load(struct thread_context *tctx, struct mir_instr *instr, enum x64_register reg) {
	bassert(instr && instr->kind == MIR_INSTR_LOAD);
	struct mir_instr_load *load = (struct mir_instr_load *)instr;
	struct mir_type       *type = load->base.value.type;

	bassert(type->kind == MIR_TYPE_INT || type->kind == MIR_TYPE_PTR);
	bassert(tctx->values[load->src->backend_value - 1].kind == OFFSET);
	const s32 rbp_offset_bytes = tctx->values[load->src->backend_value - 1].offset;

	mov_rm(tctx, reg, RBP, rbp_offset_bytes, type->store_size_bytes);
}

static void emit_binop_ri(struct thread_context *tctx, enum binop_kind op, const u8 reg, const u64 v, const usize vsize) {
	switch (op) {
	case BINOP_ADD:
		add_ri(tctx, reg, v, vsize);
		break;
	case BINOP_SUB:
		sub_ri(tctx, reg, v, vsize);
		break;
	case BINOP_MUL:
		imul_ri(tctx, reg, reg, v, vsize);
		break;
	case BINOP_LESS_EQ:
	case BINOP_GREATER_EQ:
	case BINOP_LESS:
	case BINOP_GREATER:
	case BINOP_NEQ:
	case BINOP_EQ:
		cmp_ri(tctx, reg, v, vsize);
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
		sub_rr(tctx, reg1, reg2, vsize);
		break;
	case BINOP_MUL:
		imul_rr(tctx, reg1, reg2, vsize);
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

		add_sym(tctx, linkage_name, IMAGE_SYM_CLASS_EXTERNAL, isflag(fn->flags, FLAG_EXTERN));

		// External functions does not have any body block.
		if (isflag(fn->flags, FLAG_EXTERN)) {
			return;
		}

		// Prologue
		push64_r(tctx, RBP);
		mov_rr(tctx, RBP, RSP, 8);
		tctx->stack.alloc_value_offset = sub_ri(tctx, RSP, 0, 8);

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
				allocate_stack_variables(tctx, fn);
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

	case MIR_INSTR_LOAD:
		// Load instruction is generated as needed retrospectively. We have to do it this way due to
		// calling convention requirements where each argument must go into specific register. The
		// 'emit_load' should be used.
		break;

	case MIR_INSTR_STORE: {
		struct mir_instr_store *store = (struct mir_instr_store *)instr;
		struct mir_type        *type  = store->src->value.type;
		bassert(store->dest->backend_value);
		struct x64_value dest_value = get_value(tctx, store->dest);
		struct x64_value src_value;

		if (store->src->kind == MIR_INSTR_LOAD) {
			enum x64_register src_reg = -1;
			vreg_reserve(tctx, &src_reg, 1);
			emit_load(tctx, store->src, src_reg);
			src_value = vreg_value(tctx, src_reg);
		} else {
			src_value = get_value(tctx, store->src);
		}

		bassert(dest_value.kind == OFFSET);

		switch (type->kind) {
		case MIR_TYPE_INT: {
			if (src_value.kind == IMMEDIATE) {
				mov_mi(tctx, RBP, dest_value.offset, src_value.imm, type->store_size_bytes);
			} else if (src_value.kind == REGISTER) {
				mov_mr(tctx, RBP, dest_value.offset, src_value.reg, type->store_size_bytes);
			} else {
				BL_UNIMPLEMENTED;
			}
			break;
		}
		default:
			BL_UNIMPLEMENTED;
		}

		// Release source value...
		if (src_value.kind == REGISTER) vreg_release(tctx, &src_value.reg, 1);
		break;
	}

	case MIR_INSTR_BINOP: {
		// The result value ends up in LHS register, the RHS register is released at the end
		// if it was used.
		struct mir_instr_binop *binop = (struct mir_instr_binop *)instr;
		struct mir_type        *type  = binop->lhs->value.type;
		bassert(type->kind == MIR_TYPE_INT);

		const s32 LHS                = 0;
		const s32 RHS                = 1;
		s32       required_registers = 1; // LHS goes into register.

		enum x64_register regs[2];
		if (binop->rhs->kind == MIR_INSTR_LOAD) ++required_registers;
		if (binop->lhs->kind != MIR_INSTR_LOAD) {
			// This way we can reuse the LHS register if possible.
			struct x64_value lhs_value = get_value(tctx, binop->lhs);
			if (lhs_value.kind == REGISTER) vreg_release(tctx, &lhs_value.reg, 1);
		}

		vreg_begin(tctx, regs, required_registers);

		struct x64_value rhs_value;
		struct x64_value lhs_value;

		if (binop->rhs->kind == MIR_INSTR_LOAD) {
			bassert(regs[RHS] != -1);
			emit_load(tctx, binop->rhs, regs[RHS]);
			rhs_value.kind = REGISTER;
			rhs_value.reg  = regs[RHS];
		} else {
			rhs_value = get_value(tctx, binop->rhs);
		}

		if (binop->lhs->kind == MIR_INSTR_LOAD) {
			bassert(regs[LHS] != -1);
			emit_load(tctx, binop->lhs, regs[LHS]);
			lhs_value.kind = REGISTER;
			lhs_value.reg  = regs[LHS];
		} else {
			lhs_value = get_value(tctx, binop->lhs);
		}

		if (lhs_value.kind == IMMEDIATE) {
			bassert(regs[LHS] != -1);
			mov_ri(tctx, regs[LHS], lhs_value.imm, type->store_size_bytes);
			lhs_value.kind = REGISTER;
			lhs_value.reg  = regs[LHS];
		}

		bassert(lhs_value.kind == REGISTER);

		if (rhs_value.kind == IMMEDIATE) {
			emit_binop_ri(tctx, binop->op, lhs_value.reg, rhs_value.imm, type->store_size_bytes);
		} else {
			bassert(rhs_value.kind == REGISTER);
			emit_binop_rr(tctx, binop->op, lhs_value.reg, rhs_value.reg, type->store_size_bytes);
		}

		bassert(instr->backend_value == 0);
		bassert(regs[LHS] != -1);

		vreg_end(tctx, &regs[LHS], 1); // Save LHS
		instr->backend_value = tctx->register_table[regs[LHS]] + 1;
		break;
	}

	case MIR_INSTR_RET: {
		// Epilogue
		usize total_allocated = get_stack_allocation_size(tctx);
		if (total_allocated) {
			if (!is_aligned2(total_allocated, 16)) {
				const usize padding = next_aligned2(total_allocated, 16) - total_allocated;
				if (padding) {
					allocate_stack_memory(tctx, padding);
					total_allocated += padding;
				}
			}

			add_ri(tctx, RSP, total_allocated, 8);
			tctx->stack.alloc_value_offset = 0;
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
			const u32 virtual_address = jmp_relative_i32(tctx, 0x0);

			struct jmp_fixup fixup = {
			    .target_block    = then_block,
			    .virtual_address = virtual_address,
			    .jmp_position    = get_position(tctx),
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

		u32 virtual_address = 0;

		// In case the condition is binary operation we swap the logic with attemt to generate the 'then'
		// branch immediately after conditional jump to safe one jmp instruction.
		bassert(br->cond->kind == MIR_INSTR_BINOP);
		struct mir_instr_binop *cond_binop = (struct mir_instr_binop *)br->cond;
		switch (cond_binop->op) {
		case BINOP_EQ:
			virtual_address = jne_relative_i32(tctx, 0x0);
			break;
		case BINOP_NEQ:
			virtual_address = je_relative_i32(tctx, 0x0);
			break;
		case BINOP_LESS:
			virtual_address = jge_relative_i32(tctx, 0x0);
			break;
		case BINOP_GREATER:
			virtual_address = jle_relative_i32(tctx, 0x0);
			break;
		case BINOP_LESS_EQ:
			virtual_address = jg_relative_i32(tctx, 0x0);
			break;
		case BINOP_GREATER_EQ:
			virtual_address = jl_relative_i32(tctx, 0x0);
			break;
		default:
			BL_UNIMPLEMENTED;
		}

		struct jmp_fixup fixup = {
		    .target_block    = br->else_block,
		    .virtual_address = virtual_address,
		    .jmp_position    = get_position(tctx),
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

	case MIR_INSTR_CALL: {
		// x64 calling convention:
		// numbers - RCX, RDX, R8, R9, stack in reverese order

		struct mir_instr_call *call = (struct mir_instr_call *)instr;
		bassert(!mir_is_comptime(&call->base) && "Compile time calls should not be generated into the final binary!");
		struct mir_instr *callee = call->callee;
		bassert(callee);

		// There are three possible configurations:
		//
		// 1) We call regular static function; it's compile-time known and resolved by previous decl-ref instruction.
		// 2) We call via function pointer.
		// 3) We call immediate inline defined anonymous function (we have to generate one eventually).

		hash_t            callee_hash     = 0;
		struct mir_instr *callee_fn_proto = NULL;

		if (mir_is_comptime(callee)) {
			struct mir_fn *fn = MIR_CEV_READ_AS(struct mir_fn *, &callee->value);
			bmagic_assert(fn);
			blog("Calling: %.*s", fn->linkage_name.len, fn->linkage_name.ptr);
			callee_hash     = strhash(fn->linkage_name);
			callee_fn_proto = fn->prototype;
		} else {
			// We should use loaded value from previous instruction (probably in register?)...
			BL_UNIMPLEMENTED;
		}

		bassert(callee_hash);
		bassert(callee_fn_proto);
		const s32 arg_num = (s32)sarrlen(call->args);

		enum x64_register regs[] = {RCX, RDX, R8, R9};
		vreg_reserve(tctx, regs, MIN(4, arg_num));

		for (s32 index = 0; index < arg_num; ++index) {
			struct mir_instr *arg      = sarrpeek(call->args, index);
			struct mir_type  *arg_type = arg->value.type;

			if (arg->kind == MIR_INSTR_LOAD) {
				emit_load(tctx, arg, regs[index]);
			} else {
				struct x64_value value = get_value(tctx, arg);
				bassert(value.kind == IMMEDIATE);
				mov_ri(tctx, regs[index], value.imm, arg_type->store_size_bytes);
			}
		}

		vreg_release(tctx, regs, MIN(4, arg_num));
		sub_ri(tctx, RSP, 0x20, 8);

		// Note: we use u32 here, it should be enough in context of a single fuction, but all positions are stored in u64 because they are fixed
		// last when the whole binary is complete.
		const u32 virtual_address = call_relative_i32(tctx, 0);

		add_ri(tctx, RSP, 0x20, 8);

		struct jmp_fixup fixup = {
		    .fn_proto        = (struct mir_instr_fn_proto *)callee_fn_proto,
		    .hash            = callee_hash,
		    .virtual_address = virtual_address,
		    .jmp_position    = get_position(tctx),
		};
		arrput(tctx->call_fixups, fixup);

		// Result stored in RAX!
		struct x64_value value = {
		    .kind = REGISTER,
		    .reg  = RAX,
		};
		arrput(tctx->values, value);
		call->base.backend_value = arrlenu(tctx->values);
		break;
	}

	case MIR_INSTR_CONST: {
		struct mir_instr_const *cnst = (struct mir_instr_const *)instr;
		struct mir_type        *type = cnst->base.value.type;
		bassert(type);
		struct x64_value value = {.kind = IMMEDIATE};

		switch (type->kind) {
		case MIR_TYPE_PTR:
			// Currently used only for default intialization of pointer values, so we expect this to be NULL!
			bassert(vm_read_ptr(type, cnst->base.value.data) == NULL);
			bassert(type->store_size_bytes == 8);
			// fall-through
		case MIR_TYPE_INT:
			value.imm = vm_read_int(type, cnst->base.value.data);
			break;
		default:
			BL_UNIMPLEMENTED;
		}
		set_value(tctx, instr, value);
		break;
	}

	case MIR_INSTR_ARG: {
		struct mir_instr_arg *arg_instr = (struct mir_instr_arg *)instr;

		struct mir_fn *fn = arg_instr->base.owner_block->owner_fn;
		bassert(fn);
		struct mir_type *fn_type = fn->type;
		struct mir_arg  *arg     = sarrpeek(fn_type->data.fn.args, arg_instr->i);
		bassert(isnotflag(arg->flags, FLAG_COMPTIME) && "Comptime arguments should be evaluated and replaced by constants!");

		struct x64_value value = {
		    .kind = REGISTER,
		    .reg  = arg->llvm_index + 1, // @Incomplete?
		};
		set_value(tctx, instr, value);
		break;
	}

	case MIR_INSTR_DECL_VAR: {
		struct mir_instr_decl_var *decl = (struct mir_instr_decl_var *)instr;
		struct mir_var            *var  = decl->var;
		bassert(var);
		const struct x64_value init_value = get_value(tctx, decl->init);

		if (var->ref_count == 0) {
			if (init_value.kind == REGISTER) vreg_release(tctx, &init_value.reg, 1);
			break;
		}
		if (!mir_type_has_llvm_representation(var->value.type)) break;
		bassert(var->backend_value);

		if (!decl->init) break;

		if (decl->init->kind == MIR_INSTR_COMPOUND) {
			BL_UNIMPLEMENTED;
			break;
		}

		const struct x64_value var_value = get_value(tctx, var);
		struct mir_type       *type      = var->value.type;
		bassert(var_value.kind == OFFSET);

		if (init_value.kind == IMMEDIATE) {
			switch (type->kind) {
			case MIR_TYPE_PTR:
				bassert(type->store_size_bytes == 8);
				// fall-through
			case MIR_TYPE_INT: {
				mov_mi(tctx, RBP, var_value.offset, init_value.imm, type->store_size_bytes);
				break;
			}
			default:
				BL_UNIMPLEMENTED;
			}
		} else if (init_value.kind == REGISTER) {
			mov_mr(tctx, RBP, var_value.offset, init_value.reg, type->store_size_bytes);
			vreg_release(tctx, &init_value.reg, 1);
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
				bassert(var->backend_value);
				ref->base.backend_value = var->backend_value;
			}
			bassert(ref->base.llvm_value);

			break;
		}
		case SCOPE_ENTRY_FN: {
			// @Incomplete: Push function for generation.
			break;
		}
		default:
			BL_UNIMPLEMENTED;
		}
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
		struct jmp_fixup *fixup = &tctx->jmp_fixups[i];
		bassert(fixup->target_block);
		const u64 backend_value = fixup->target_block->base.backend_value;
		if (!backend_value) {
			babort("Cannot resolve relative jump target block!");
		}

		bassert(tctx->values[backend_value - 1].kind == ADDRESS);
		const s32 block_position = (s32)tctx->values[backend_value - 1].address;
		const s32 jmp_position   = (s32)fixup->jmp_position;
		const s32 diff           = block_position - jmp_position;
		memcpy(&tctx->bytes[fixup->virtual_address], &diff, sizeof(s32));
	}
}

static void job(struct job_context *job_ctx) {
	zone();
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
	arrsetlen(tctx->call_fixups, 0);
	tctx->stack.alloc_value_offset = 0;
	bl_zeromem(&tctx->stack, sizeof(tctx->stack));

	for (s32 i = 0; i < REGISTER_COUNT; ++i) {
		if ((i >= RAX && i <= RDX) || (i >= R8 && i <= R11)) {
			// We'll use just volatile registers...
			tctx->register_table[i] = UNUSED_REGISTER_MAP_VALUE;
		} else {
			tctx->register_table[i] = RESERVED_REGISTER_MAP_VALUE;
		}
	}

	arrsetcap(tctx->bytes, BYTE_CODE_BUFFER_SIZE);

	emit_instr(ctx, tctx, job_ctx->x64.top_instr);

	fixup_jump_offsets(tctx);

	const usize section_len = arrlenu(tctx->bytes);
	const usize syms_len    = arrlenu(tctx->syms);
	const usize strs_len    = arrlenu(tctx->strs);

	{
		pthread_mutex_lock(&ctx->code.mutex);

		// Write top-level function generated code into the code section if there is any generated code
		// present.

		const usize gsection_len = arrlenu(ctx->code.bytes);
		const usize gsyms_len    = arrlenu(ctx->code.syms);
		const usize gstrs_len    = arrlenu(ctx->code.strs);

		// Insert code.
		arrsetcap(ctx->code.bytes, CODE_BLOCK_BUFFER_SIZE);
		arrsetlen(ctx->code.bytes, gsection_len + section_len);
		memcpy(&ctx->code.bytes[gsection_len], tctx->bytes, section_len);

		// Call positions
		for (usize i = 0; i < arrlenu(tctx->call_fixups); ++i) {
			struct jmp_fixup *fixup = &tctx->call_fixups[i];
			bassert(fixup->hash);
			bassert(fixup->fn_proto);

			// Fix positions.
			fixup->virtual_address += gsection_len;
			fixup->jmp_position += gsection_len;

			// Generate function if it's not already generated.
			if (hmgeti(ctx->scheduled_for_generation, fixup->hash) == -1) {
				hmputs(ctx->scheduled_for_generation, (struct scheduled_entry){fixup->hash});

				struct job_context job_ctx = {.x64 = {.ctx = ctx, .top_instr = &fixup->fn_proto->base}};
				submit_job(&job, &job_ctx);
			}
		}

		// Duplicate to global context.
		const usize gcall_fixups_len = arrlenu(ctx->call_fixups);
		const usize call_fixups_len  = arrlenu(tctx->call_fixups);
		arrsetlen(ctx->call_fixups, gcall_fixups_len + call_fixups_len);
		memcpy(&ctx->call_fixups[gcall_fixups_len], tctx->call_fixups, call_fixups_len * sizeof(struct jmp_fixup));

		// Adjust symbol positions.
		for (usize i = 0; i < arrlenu(tctx->syms); ++i) {
			IMAGE_SYMBOL *sym = &tctx->syms[i];
			// 0 section number means the symbol is externally linked. There is no location in our
			// binary.
			if (sym->SectionNumber) {
				sym->Value += (DWORD)gsection_len;
			}
			char *sym_name = NULL;
			if (sym->N.Name.Short == 0) {
				sym_name = &tctx->strs[sym->N.Name.Long - sizeof(u32)];
				sym->N.Name.Long += (DWORD)gstrs_len;
			} else {
				sym_name = (char *)&sym->N.ShortName[0];
			}
			if (sym->StorageClass != IMAGE_SYM_CLASS_EXTERNAL) continue;

			const hash_t hash = strhash(make_str_from_c(sym_name));
			bassert(hmgeti(ctx->symbol_table, hash) == -1);
			struct symbol_table_entry entry = {
			    .key                = hash,
			    .symbol_table_index = (s32)(i + gsyms_len),
			};
			hmputs(ctx->symbol_table, entry);
		}

		// Copy symbol table to global one.
		arrsetcap(ctx->code.syms, SYMBOL_TABLE_SIZE);
		arrsetlen(ctx->code.syms, gsyms_len + syms_len);
		memcpy(&ctx->code.syms[gsyms_len], tctx->syms, syms_len * sizeof(IMAGE_SYMBOL));

		// Copy string table to global one.
		arrsetcap(ctx->code.strs, STRING_TABLE_SIZE);
		arrsetlen(ctx->code.strs, gstrs_len + strs_len);
		memcpy(&ctx->code.strs[gstrs_len], tctx->strs, strs_len);

		pthread_mutex_unlock(&ctx->code.mutex);
	}
	return_zone();
}

static void create_object_file(struct context *ctx) {
	usize section_data_pointer = IMAGE_SIZEOF_FILE_HEADER + IMAGE_SIZEOF_SECTION_HEADER;

	// Code block is aligned to 16 bytes, do we need this???
	const usize section_data_padding = next_aligned2(section_data_pointer, 16) - section_data_pointer;
	section_data_pointer += section_data_padding;

	const usize reloc_pointer = section_data_pointer + arrlenu(ctx->code.bytes);
	const usize sym_pointer   = section_data_pointer + arrlenu(ctx->code.bytes) + arrlenu(ctx->code.relocs) * sizeof(IMAGE_RELOCATION);

	const usize number_of_relocations = arrlenu(ctx->code.relocs);
	if (number_of_relocations > 0xFFFF) {
		babort("Relocation table is too large to be stored in COFF file!");
	}

	IMAGE_FILE_HEADER header = {
	    .Machine              = IMAGE_FILE_MACHINE_AMD64,
	    .NumberOfSections     = 1,
	    .PointerToSymbolTable = (DWORD)sym_pointer,
	    .NumberOfSymbols      = (DWORD)arrlenu(ctx->code.syms),
	    .TimeDateStamp        = (DWORD)time(0),
	};

	IMAGE_SECTION_HEADER section = {
	    .Name                 = ".text",
	    .Characteristics      = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES | IMAGE_SCN_MEM_EXECUTE,
	    .SizeOfRawData        = (DWORD)arrlenu(ctx->code.bytes),
	    .PointerToRawData     = (DWORD)section_data_pointer,
	    .PointerToRelocations = (DWORD)reloc_pointer,
	    .NumberOfRelocations  = (u16)number_of_relocations,
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
	// Gap to align the code section.
	u8 padding[16] = {0};
	bassert(section_data_padding <= static_arrlenu(padding));
	fwrite(padding, 1, section_data_padding, file);
	// Write section code
	fwrite(ctx->code.bytes, 1, arrlenu(ctx->code.bytes), file);
	// Write relocation table
	fwrite(ctx->code.relocs, arrlenu(ctx->code.relocs), sizeof(IMAGE_RELOCATION), file);
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

	// Fixup all call offsets.
	for (usize i = 0; i < arrlenu(ctx.call_fixups); ++i) {
		struct jmp_fixup *fixup = &ctx.call_fixups[i];
		bassert(fixup->hash);
		bassert(fixup->fn_proto);

		// Find the function position in the binary.
		const usize i = hmgeti(ctx.symbol_table, fixup->hash);
		if (i == -1) {
			babort("Internally linked symbol reference is not found in the binary!");
		}

		const s32     symbol_table_index = ctx.symbol_table[i].symbol_table_index;
		IMAGE_SYMBOL *sym                = &ctx.code.syms[symbol_table_index];
		if (sym->SectionNumber == 0) {
			// Push into COFF relocation table. The symbol is not in our binary.
			IMAGE_RELOCATION reloc = {
			    .Type             = IMAGE_REL_AMD64_REL32,
			    .SymbolTableIndex = symbol_table_index,
			    .VirtualAddress   = (DWORD)fixup->virtual_address,
			};
			arrput(ctx.code.relocs, reloc);
		} else {
			// Fix the location.
			const s32 sym_position = (s32)sym->Value;
			const s32 jmp_position = (s32)fixup->jmp_position;
			const s32 diff         = sym_position - jmp_position;
			memcpy(&ctx.code.bytes[fixup->virtual_address], &diff, sizeof(s32));
		}
	}

	// Write the output file.
	create_object_file(&ctx);

	// Cleanup
	// @Incomplete: Only if not in dirty mode?
	for (usize i = 0; i < arrlenu(ctx.tctx); ++i) {
		struct thread_context *tctx = &ctx.tctx[i];
		arrfree(tctx->bytes);
		arrfree(tctx->syms);
		arrfree(tctx->strs);
		arrfree(tctx->values);
		arrfree(tctx->emit_block_queue);
		arrfree(tctx->jmp_fixups);
		arrfree(tctx->call_fixups);
	}
	arrfree(ctx.tctx);
	arrfree(ctx.code.bytes);
	arrfree(ctx.code.syms);
	arrfree(ctx.code.strs);
	arrfree(ctx.code.relocs);
	arrfree(ctx.call_fixups);
	hmfree(ctx.symbol_table);
	hmfree(ctx.scheduled_for_generation);
	pthread_mutex_destroy(&ctx.code.mutex);
}
