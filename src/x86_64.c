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

//
// Known limitation:
//
//   - No optimiations at all, this backend is supposed to be fast as possible in compilation. For
//     high performance release builds use LLVM.
//   - We use only 32bit relative addressing which limits binary size to ~2GB, do we need more?
//   - Only single .text section is generated (maximum size is 4GB).
//   - Relocation table is can have 65535 entries only.
//

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

#define SECTION_EXTERN 0
#define SECTION_TEXT 1
#define SECTION_DATA 2

#define DT_FUNCTION 0x20

#define MEMCPY_BUILTIN cstr("__bl_memcpy")
hash_t MEMCPY_BUILTIN_HASH = 0;
#define MEMSET_BUILTIN cstr("__bl_memset")
hash_t MEMSET_BUILTIN_HASH = 0;

#define data_ptr_s32(data, p) ((s32 *)&(data)[p])

// @Performance: internal functions can support more arguments passed through registers.
static const enum x64_register CALL_ABI[4] = {RCX, RDX, R8, R9};

enum x64_value_kind {
	// Absolute value address.
	ADDRESS = 0,
	// Relative offset in the .text section.
	OFFSET = 1,
	// Relative relocated offset; this is mainly used for references between sections (e.g. global varaibles).
	// We have to record patch location each time the value is used, since we don't know the offset.
	RELOCATION = 2,
	// Register index.
	REGISTER = 3,
	// Immediate 64bit value.
	IMMEDIATE = 4,
	// RBP + Offset in register + static offset.
	REGISTER_OFFSET = 5,
	// Absolute address in register + static offset.
	REGISTER_ADDRESS = 6,
};

struct x64_value {
	enum x64_value_kind kind;
	union {
		u32               address;
		s32               offset;
		enum x64_register reg;
		u64               imm;
		struct {
			hash_t hash;
			s32    offset;
		} reloc;
		struct {
			enum x64_register reg;
			s32               offset;
		} reg_off_addr;
	};
};

#define peek(index) (tctx->values[(index)])
#define peek_immediate(index) (tctx->values[(index)].imm)
#define peek_relocation(index) (tctx->values[(index)].reloc)
#define peek_offset(index) (tctx->values[(index)].offset)
#define peek_register(index) (tctx->values[(index)].reg)
#define peek_register_offset(index) (tctx->values[(index)].reg_off_addr)
#define peek_register_address(index) (tctx->values[(index)].reg_off_addr)

struct sym_patch {
	s32               target_section;
	struct mir_instr *instr;
	hash_t            hash;
	u64               position; // Points to the relocation value.
	s32               type;
};

struct symbol_table_entry {
	hash_t key;
	s32    symbol_table_index;
};

struct scheduled_entry {
	hash_t key;
};

struct thread_context {
	array(u8) text;
	array(u8) data;
	array(IMAGE_SYMBOL) syms;
	array(char) strs;
	array(struct x64_value) values;
	array(struct mir_instr_block *) emit_block_queue;
	array(struct sym_patch) local_patches; // local to the function
	array(struct sym_patch) patches;

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
		array(u8) bytes;
		array(IMAGE_RELOCATION) relocs;
	} code;

	struct {
		array(u8) bytes;
		array(IMAGE_RELOCATION) relocs;
	} data;

	array(IMAGE_SYMBOL) syms;
	array(char) strs;

	hash_table(struct symbol_table_entry) symbol_table;

	hash_table(struct scheduled_entry) scheduled_for_generation;
	pthread_spinlock_t schedule_for_generation_lock;

	array(struct sym_patch) patches;

	pthread_mutex_t    mutex;
	pthread_spinlock_t uq_name_lock;
};

// Resources:
// http://www.c-jump.com/CIS77/CPU/x86/lecture.html#X77_0010_real_encoding
// https://www.felixcloutier.com/x86/
// https://courses.cs.washington.edu/courses/cse378/03wi/lectures/LinkerFiles/coff.pdf

static void job(struct job_context *job_ctx);

enum x64_type_kind {
	X64_UNKNOWN,
	X64_NUMBER   = 1,
	X64_COMPOSIT = 2, // all structs
};

static inline enum x64_type_kind get_type_kind(struct mir_type *type) {
	switch (type->kind) {
	case MIR_TYPE_INT:
	case MIR_TYPE_PTR:
	case MIR_TYPE_BOOL:
	case MIR_TYPE_ENUM:
	case MIR_TYPE_NULL:
	case MIR_TYPE_FN:
		return X64_NUMBER;

	case MIR_TYPE_STRING:
	case MIR_TYPE_VARGS:
	case MIR_TYPE_SLICE:
	case MIR_TYPE_DYNARR:
	case MIR_TYPE_STRUCT:
		return X64_COMPOSIT;

	default:
		str_buf_t type_name = mir_type2str(type, false);
		babort("Unsupported type in x64 backend: %.*s.", type_name.len, type_name.ptr);
	}
}

#define op(value_kind, type_kind) ((value_kind << 4) | get_type_kind(type_kind))
#define op_combined(a, b) ((((u64)a) << 8) | ((u64)b))

enum op_kind {
	// FFFF value_kind | FFFF type_kind
	OP_IMMEDIATE_NUMBER          = (IMMEDIATE << 4) | X64_NUMBER,
	OP_RELOCATION_NUMBER         = (RELOCATION << 4) | X64_NUMBER,
	OP_REGISTER_NUMBER           = (REGISTER << 4) | X64_NUMBER,
	OP_OFFSET_NUMBER             = (OFFSET << 4) | X64_NUMBER,
	OP_REGISTER_OFFSET_NUMBER    = (REGISTER_OFFSET << 4) | X64_NUMBER,
	OP_REGISTER_ADDRESS_NUMBER   = (REGISTER_ADDRESS << 4) | X64_NUMBER,
	OP_RELOCATION_COMPOSIT       = (RELOCATION << 4) | X64_COMPOSIT,
	OP_OFFSET_COMPOSIT           = (OFFSET << 4) | X64_COMPOSIT,
	OP_REGISTER_ADDRESS_COMPOSIT = (REGISTER_ADDRESS << 4) | X64_COMPOSIT,
};

static inline u32 get_position(struct thread_context *tctx, s32 section_number) {
	switch (section_number) {
	case SECTION_EXTERN:
		return 0;
	case SECTION_TEXT:
		return (u32)arrlenu(tctx->text);
	case SECTION_DATA:
		return (u32)arrlenu(tctx->data);
	default:
		babort("Invalid section number!");
	}
}

#ifdef BL_DEBUG
static void check_dangling_registers(struct thread_context *tctx) {
	// Check for dangling registers
	for (s32 i = 0; i < REGISTER_COUNT; ++i) {
		const s32 index = tctx->register_table[i];
		if (index >= 0) {
			babort("Dangling register %s (value: %d).", register_name[i], index);
		}
	}
}
#else
#	define check_dangling_registers(tctx) (void)0
#endif

static inline void unique_name(struct context *ctx, str_buf_t *dest, const char *prefix, const str_t name) {
	pthread_spin_lock(&ctx->uq_name_lock);
	static u64 n = 0;
	str_buf_clr(dest);
	str_buf_append_fmt(dest, "{s}{str}{u64}", prefix, name, n++);
	pthread_spin_unlock(&ctx->uq_name_lock);
}

void add_code(struct thread_context *tctx, const void *buf, s32 len) {
	const u32 i = get_position(tctx, SECTION_TEXT);
	arrsetlen(tctx->text, i + len);
	memcpy(&tctx->text[i], buf, len);
}

static inline u64 add_value(struct thread_context *tctx, struct x64_value value) {
#if defined(BL_DEBUG) && 0
	if (arrlenu(tctx->values) == 24)
		BL_DEBUG_BREAK;
#endif
	arrput(tctx->values, value);
	return arrlenu(tctx->values); // +1
}

// Returns offset in data section.
static inline u32 add_data(struct thread_context *tctx, const void *buf, s32 len) {
	const u32 position = get_position(tctx, SECTION_DATA);
	arrsetlen(tctx->data, position + len);
	void *dest = &tctx->data[position];
	if (buf) {
		memcpy(dest, buf, len);
	} else {
		// This is required.
		bl_zeromem(dest, len);
	}
	return position;
}

static inline void submit_instr(struct context *ctx, hash_t hash, struct mir_instr *instr) {
	pthread_spin_lock(&ctx->schedule_for_generation_lock);
	if (hmgeti(ctx->scheduled_for_generation, hash) == -1) {
		hmputs(ctx->scheduled_for_generation, (struct scheduled_entry){hash});
		submit_job(&job, &(struct job_context){.x64 = {.ctx = ctx, .top_instr = instr}});
	}
	pthread_spin_unlock(&ctx->schedule_for_generation_lock);
}

static inline hash_t submit_function_generation(struct context *ctx, struct mir_fn *fn) {
	bassert(fn);
	const hash_t hash = strhash(fn->linkage_name);
	submit_instr(ctx, hash, fn->prototype);
	return hash;
}

static inline hash_t submit_global_variable_generation(struct context *ctx, struct thread_context *tctx, struct mir_var *var) {
	bassert(var);
	bassert(isflag(var->iflags, MIR_VAR_GLOBAL));

	const hash_t      hash       = strhash(var->linkage_name);
	struct mir_instr *instr_init = var->initializer_block;
	bassert(instr_init && "Missing initializer block reference for IR global variable!");
	submit_instr(ctx, hash, instr_init);
	return hash;
}

// Add new symbol into thread local storage and set it's offset value relative to already generated
// code in the thread local bytes array. This value must be later fixed according to position in the
// final code section.
//
// Returns offset of the symbol in the binary.
u32 add_sym(struct thread_context *tctx, s32 section_number, u32 offset, str_t linkage_name, u8 storage_class, s32 data_type) {
	bassert(linkage_name.len && linkage_name.ptr);
	IMAGE_SYMBOL sym = {
	    .SectionNumber = section_number,
	    .Type          = data_type,
	    .StorageClass  = storage_class,
	    .Value         = offset,
	};

	if (linkage_name.len > 7) {
		const u32 str_offset = (u32)arrlenu(tctx->strs);
		arrsetcap(tctx->strs, 256);
		arrsetlen(tctx->strs, str_offset + linkage_name.len + 1); // +1 zero terminator
		memcpy(&tctx->strs[str_offset], linkage_name.ptr, linkage_name.len);
		tctx->strs[str_offset + linkage_name.len] = '\0';

		sym.N.Name.Long = str_offset + sizeof(u32); // 4 bytes for the leading table size
	} else {
		memcpy(sym.N.ShortName, linkage_name.ptr, linkage_name.len);
	}

	arrsetcap(tctx->syms, 64);
	arrput(tctx->syms, sym);
	return offset;
}

// Should be called from main thread only. One symbol cannot be added twice.
hash_t add_global_external_sym(struct context *ctx, str_t linkage_name, s32 data_type) {
	bassert(linkage_name.len && linkage_name.ptr);
	IMAGE_SYMBOL sym = {
	    .Type         = data_type,
	    .StorageClass = IMAGE_SYM_CLASS_EXTERNAL,
	};

	if (linkage_name.len > 7) {
		const u32 str_offset = (u32)arrlenu(ctx->strs);
		arrsetcap(ctx->strs, 256);
		arrsetlen(ctx->strs, str_offset + linkage_name.len + 1); // +1 zero terminator
		memcpy(&ctx->strs[str_offset], linkage_name.ptr, linkage_name.len);
		ctx->strs[str_offset + linkage_name.len] = '\0';

		sym.N.Name.Long = str_offset + sizeof(u32); // 4 bytes for the leading table size
	} else {
		memcpy(sym.N.ShortName, linkage_name.ptr, linkage_name.len);
	}

	const hash_t hash = strhash(linkage_name);
	bassert(hmgeti(ctx->symbol_table, hash) == -1);
	struct symbol_table_entry entry = {
	    .key                = hash,
	    .symbol_table_index = (s32)(arrlen(ctx->syms)),
	};

	arrput(ctx->syms, sym);
	hmputs(ctx->symbol_table, entry);

	return hash;
}

static inline u64 add_block(struct thread_context *tctx, const str_t name) {
	const u32        address = add_sym(tctx, SECTION_TEXT, get_position(tctx, SECTION_TEXT), name, IMAGE_SYM_CLASS_LABEL, 0);
	struct x64_value value   = {.kind = ADDRESS, .address = address};
	return add_value(tctx, value);
}

//
// Translate
//

static inline usize get_stack_allocation_size(struct thread_context *tctx) {
	bassert(tctx->stack.alloc_value_offset > 0);
	return *(u32 *)(tctx->text + tctx->stack.alloc_value_offset);
}

static inline s32 allocate_stack_memory(struct thread_context *tctx, usize size) {
	const usize allocated = get_stack_allocation_size(tctx);
	if (allocated + size > 0xFFFFFFFF) {
		babort("Stack allocation is too big!");
	}
	(*(u32 *)(tctx->text + tctx->stack.alloc_value_offset)) += (u32)size;
	return (s32)allocated;
}

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
		var->backend_value = add_value(tctx, value);
		top += var_size;
	}

	allocate_stack_memory(tctx, top);
}

// Tries to find some free registers.
static void find_free_registers(struct thread_context *tctx, enum x64_register dest[], s32 num, const enum x64_register exclude[], s32 exclude_num) {
	bassert(num > 0 && num <= REGISTER_COUNT);
	s32 set_num = 0;
	for (s32 i = 0; i < num; ++i) {
		dest[i] = -1;
	}
	for (s32 i = 0; i < REGISTER_COUNT && set_num < num; ++i) {
		bool skip = false;
		for (s32 j = 0; j < exclude_num; ++j) {
			if (i == exclude[j]) skip = true;
		}
		if (skip == false && tctx->register_table[i] == UNUSED_REGISTER_MAP_VALUE) {
			dest[set_num++] = i;
		}
	}
}

// Spill register into memory or another register (you can set some exclusions, these registers would not be used for spilling).
static enum x64_register spill(struct thread_context *tctx, enum x64_register reg, const enum x64_register exclude[], s32 exclude_num) {
	const s32 vi = tctx->register_table[reg];
	bassert(vi != RESERVED_REGISTER_MAP_VALUE);
	if (vi == UNUSED_REGISTER_MAP_VALUE) {
		// Register is already free, no need to spill.
		return reg;
	}
	struct x64_value *spill_value = &tctx->values[vi];

	// Try to find register where to move current value of reg.
	enum x64_register dest_reg = -1;
	for (s32 i = 0; i < REGISTER_COUNT; ++i) {
		if (tctx->register_table[i] != UNUSED_REGISTER_MAP_VALUE) continue;
		if (reg == i) continue;
		for (s32 j = 0; j < exclude_num; ++j) {
			if (exclude[j] == -1) continue;
			if (exclude[j] == i) goto SKIP;
		}
		dest_reg = i;
	SKIP:
		continue;
	}
	if (dest_reg != -1) {
		// Spill to register.
		bassert(dest_reg != reg);
		bassert(tctx->register_table[dest_reg] == UNUSED_REGISTER_MAP_VALUE);
		switch (spill_value->kind) {
		case REGISTER:
			bassert(spill_value->reg == reg);
			mov_rr(tctx, dest_reg, reg, 8);
			spill_value->reg = dest_reg;
			break;
		case REGISTER_ADDRESS:
			bassert(spill_value->reg_off_addr.reg == reg);
			mov_rr(tctx, dest_reg, reg, 8);
			spill_value->reg_off_addr.reg = dest_reg;
			break;
		default:
			BL_UNIMPLEMENTED;
		}
		// Set new register holding old value as used.
		tctx->register_table[dest_reg] = vi;
	} else {
		// Spill to memory.
		const s32 top    = allocate_stack_memory(tctx, 8);
		const s32 offset = -top;
		mov_mr(tctx, RBP, offset, reg, 8);
		spill_value->kind   = OFFSET;
		spill_value->offset = offset;
	}

	tctx->register_table[reg] = UNUSED_REGISTER_MAP_VALUE;
	return reg;
}

enum x64_register get_temporary_register(struct thread_context *tctx, const enum x64_register exclude[], s32 exclude_num) {
	enum x64_register reg;
	find_free_registers(tctx, &reg, 1, exclude, exclude_num);
	if (reg != -1) {
		return reg;
	}
	reg = spill(tctx, RAX, exclude, exclude_num);
	if (reg == -1) babort("Unable to find free register for temporary operations in x64 backend.");
	return reg;
}

// In case of register value is used, it's reserved for the value. Call 'release_value' to free it.
static inline void set_value(struct thread_context *tctx, struct mir_instr *instr, struct x64_value value) {
	bassert(instr->backend_value == 0 && "Instruction x64 value already set!");
	enum x64_register reg = -1;
	switch (value.kind) {
	case REGISTER:
		reg = value.reg;
		break;
	case REGISTER_ADDRESS:
	case REGISTER_OFFSET:
		reg = value.reg_off_addr.reg;
		break;
	default:
		break;
	}
	if (reg != -1) {
		bassert(tctx->register_table[reg] == UNUSED_REGISTER_MAP_VALUE && "Register already used?");
		tctx->register_table[reg] = (s32)arrlen(tctx->values);
	}
	instr->backend_value = add_value(tctx, value);
}

#define get_value(tctx, V) _Generic((V),                \
	                                struct mir_instr *  \
	                                : _get_value_instr, \
	                                  struct mir_arg *  \
	                                : _get_value_arg,   \
	                                  struct mir_var *  \
	                                : _get_value_var)((tctx), (V))

static inline u64 _get_value_instr(struct thread_context *tctx, struct mir_instr *instr) {
	bassert(instr->backend_value);
	return instr->backend_value - 1;
}

static inline u64 _get_value_var(struct thread_context *tctx, struct mir_var *var) {
	bassert(var->backend_value != 0);
	return var->backend_value - 1;
}

static inline u64 _get_value_arg(struct thread_context *tctx, struct mir_arg *arg) {
	bassert(arg->backend_value != 0);
	return arg->backend_value - 1;
}

#define release_value(tctx, V) _Generic((V),                    \
	                                    u64                     \
	                                    : _release_value_index, \
	                                      struct mir_instr *    \
	                                    : _release_value_instr)((tctx), (V))

static inline void _release_value_index(struct thread_context *tctx, u64 index) {
	const struct x64_value value = tctx->values[index];
	enum x64_register      reg   = -1;
	switch (value.kind) {
	case REGISTER:
		reg = value.reg;
		break;
	case REGISTER_ADDRESS:
	case REGISTER_OFFSET:
		reg = value.reg_off_addr.reg;
		break;
	default:
		return;
	}
	if (reg != -1) {
		bassert(reg >= 0 && reg < REGISTER_COUNT);
		// @Incomplete: assert when register is not used?
		tctx->register_table[reg] = UNUSED_REGISTER_MAP_VALUE;
	}
}

static inline void _release_value_instr(struct thread_context *tctx, struct mir_instr *instr) {
	_release_value_index(tctx, instr->backend_value - 1);
}

static inline void add_patch(struct thread_context *tctx, hash_t hash, u64 position, s32 type, s32 target_section) {
	bassert(hash);
	const struct sym_patch patch = {
	    .hash           = hash,
	    .position       = position,
	    .type           = type,
	    .target_section = target_section,
	};
	arrput(tctx->patches, patch);
}

static void emit_string_literal(struct context *ctx, struct thread_context *tctx, u32 dest_offset, str_t str) {
	struct assembly *assembly = ctx->assembly;
	struct mir_type *type     = assembly->builtin_types.t_string_literal;
	str_buf_t        name     = get_tmp_str();

	hash_t reloc_hash = 0;

	{ // String data in DATA segment.
		unique_name(ctx, &name, ".dstr", (const str_t){0});
		const u32 data_offset = add_data(tctx, str.ptr, str.len);
		add_sym(tctx, SECTION_DATA, data_offset, str_buf_view(name), IMAGE_SYM_CLASS_EXTERNAL, 0);

		reloc_hash = strhash(name);
	}

	{ // Write len
		const u32 len_offset = (u32)vm_get_struct_elem_offset(assembly, type, MIR_SLICE_LEN_INDEX);
		memcpy(tctx->data + dest_offset + len_offset, &str.len, mir_get_struct_elem_type(type, MIR_SLICE_LEN_INDEX)->store_size_bytes);
	}

	{ // Write ptr
		unique_name(ctx, &name, ".str", (const str_t){0});
		const u32 ptr_offset = (u32)vm_get_struct_elem_offset(assembly, type, MIR_SLICE_PTR_INDEX);
		add_sym(tctx, SECTION_DATA, dest_offset + ptr_offset, str_buf_view(name), IMAGE_SYM_CLASS_EXTERNAL, 0);

		add_patch(tctx, reloc_hash, dest_offset + ptr_offset, IMAGE_REL_AMD64_ADDR64, SECTION_DATA);
	}

	put_tmp_str(name);
}

static void emit_call_memcpy(struct thread_context *tctx, const u64 vi_dest, const u64 vi_src, s64 size) {
	const enum x64_register excluded[] = {RCX, RDX, R8};

	// dest
	switch (peek(vi_dest).kind) {
	case OFFSET:
		lea_rm(tctx, spill(tctx, RCX, excluded, 3), RBP, peek_offset(vi_dest), 8);
		break;
	default:
		BL_UNIMPLEMENTED;
	}

	// src
	switch (peek(vi_src).kind) {
	case RELOCATION:
		lea_rm_indirect(tctx, spill(tctx, RDX, excluded, 3), 0, 8);
		const u32 reloc_src_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
		add_patch(tctx, peek_relocation(vi_src).hash, reloc_src_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
		break;
	case REGISTER_ADDRESS: {
		const enum x64_register reg = peek_register(vi_src);
		if (reg != RDX) {
			mov_rr(tctx, spill(tctx, RDX, excluded, 3), reg, 8);
		}
		break;
	}
	default:
		BL_UNIMPLEMENTED;
	}

	// size
	mov_ri(tctx, spill(tctx, R8, excluded, 3), size, 8);

	sub_ri(tctx, RSP, 0x20, 8);
	call_relative_i32(tctx, 0);
	const u32 reloc_call_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
	add_patch(tctx, MEMCPY_BUILTIN_HASH, reloc_call_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
	add_ri(tctx, RSP, 0x20, 8);
}

static void emit_call_memset(struct thread_context *tctx, const u64 vi_dest, s32 v, s64 size) {
	const enum x64_register excluded[] = {RCX, RDX, R8};

	bassert(peek(vi_dest).kind == OFFSET);
	const u32 dest_offset = peek_offset(vi_dest);
	lea_rm(tctx, spill(tctx, RCX, excluded, 3), RBP, dest_offset, 8);
	mov_ri(tctx, spill(tctx, RDX, excluded, 3), v, 4);
	mov_ri(tctx, spill(tctx, R8, excluded, 3), size, 8);

	sub_ri(tctx, RSP, 0x20, 8);
	call_relative_i32(tctx, 0);
	const u32 reloc_call_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
	add_patch(tctx, MEMSET_BUILTIN_HASH, reloc_call_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
	add_ri(tctx, RSP, 0x20, 8);
}

// Build code to copy relocation value to memory offset, use mov for small values and memcpy for others.
static void emit_copy_relocation_to_offset(struct thread_context *tctx, const u64 vi_dest, const u64 vi_src, u32 size) {
	if (size <= REGISTER_SIZE) {
		// Fits into register (note this does not apply for any splines but we might reuse the same
		// code for structs too).
		enum x64_register reg = get_temporary_register(tctx, NULL, 0);

		zero_reg(tctx, reg, 8);
		mov_rm_indirect(tctx, reg, 0, size);
		const u32 reloc_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
		add_patch(tctx, peek_relocation(vi_src).hash, reloc_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
		mov_mr(tctx, RBP, peek_offset(vi_dest), reg, size);
	} else {
		// Use builtin memcpy
		emit_call_memcpy(tctx, vi_dest, vi_src, size);
	}
}

static void emit_load_to_register(struct thread_context *tctx, struct mir_instr *instr, enum x64_register reg) {
	bassert(instr && instr->kind == MIR_INSTR_LOAD);
	bassert(!mir_is_global(instr));
	struct mir_instr_load *load = (struct mir_instr_load *)instr;
	struct mir_type       *type = load->base.value.type;

	if (load->src->kind == MIR_INSTR_LOAD) {
		emit_load_to_register(tctx, load->src, reg);
	}

	const u64   vi_src     = get_value(tctx, load->src);
	const usize value_size = type->store_size_bytes;

	bool result_is_register_address = load->is_deref;

	enum op_kind op_kind = op(peek(vi_src).kind, type);

	switch (op_kind) {
	case OP_OFFSET_NUMBER:
		mov_rm(tctx, reg, RBP, peek_offset(vi_src), value_size);
		break;
	case OP_REGISTER_OFFSET_NUMBER: {
		mov_rm_sib(tctx, reg, RBP, peek_register_offset(vi_src).reg, peek_register_offset(vi_src).offset, value_size);
		break;
	}
	case OP_REGISTER_ADDRESS_NUMBER: {
		mov_rm(tctx, reg, peek_register_address(vi_src).reg, peek_register_address(vi_src).offset, value_size);
		break;
	}
	case OP_RELOCATION_NUMBER: {
		mov_rm_indirect(tctx, reg, peek_relocation(vi_src).offset, value_size);
		const u32 reloc_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
		add_patch(tctx, peek_relocation(vi_src).hash, reloc_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
		break;
	}
	case OP_OFFSET_COMPOSIT: {
		lea_rm(tctx, reg, RBP, peek_offset(vi_src), 8);
		result_is_register_address = true;
		break;
	}
	case OP_REGISTER_ADDRESS_COMPOSIT: {
		// @Performace [travis]: This is not so optimal, we might keep the address in the same register as it is, but
		// what in case the register we load into is required?
		const enum x64_register src_reg = peek_register_address(vi_src).reg;
		if (reg != src_reg) {
			mov_rr(tctx, reg, src_reg, 8);
		}
		result_is_register_address = true;
		break;
	}

	default:
		BL_UNIMPLEMENTED;
	}

	release_value(tctx, vi_src);

	if (result_is_register_address) {
		set_value(tctx, instr, (struct x64_value){.kind = REGISTER_ADDRESS, .reg_off_addr.reg = reg});
	} else {
		set_value(tctx, instr, (struct x64_value){.kind = REGISTER, .reg = reg});
	}
}

static void emit_global_compound(struct context *ctx, struct thread_context *tctx, struct mir_instr *instr, u32 dest_offset, u32 value_size) {
	bassert(instr->kind == MIR_INSTR_COMPOUND);
	bassert(mir_is_global(instr) && "Expected global compound expression!");
	bassert(mir_is_comptime(instr) && "Expected compile time known compound expression!");

	struct mir_instr_compound *cmp = (struct mir_instr_compound *)instr;
	bassert(cmp->is_naked == false && "Global compound expression cannot be naked!");

	if (mir_is_zero_initialized(cmp)) {
		// The memory allocation in data segment is required to be already zero initialized.
		return;
	}

	struct mir_type *type    = cmp->base.value.type;
	ints_t          *mapping = cmp->value_member_mapping;
	mir_instrs_t    *values  = cmp->values;

	for (usize i = 0; i < sarrlenu(values); ++i) {
		struct mir_instr *value_instr = sarrpeek(values, i);
		const u32         value_size  = (u32)value_instr->value.type->store_size_bytes;

		if (value_instr->kind == MIR_INSTR_COMPOUND) {
			BL_UNIMPLEMENTED;
			break;
		}

		const u64 vi = get_value(tctx, value_instr);

		s32 dest_value_offset = 0;
		if (mir_is_composite_type(type)) {
			const usize index = mapping ? sarrpeek(mapping, i) : i;
			dest_value_offset = dest_offset + (s32)vm_get_struct_elem_offset(ctx->assembly, type, (u32)index);
		} else {
			bassert(type->kind == MIR_TYPE_ARRAY);
			bassert(mapping == NULL && "Mapping is currently not supported for arrays.");
			dest_value_offset = dest_offset + (s32)vm_get_array_elem_offset(type, (u32)i);
		}

		enum op_kind op_kind = op(peek(vi).kind, value_instr->value.type);
		switch (op_kind) {
		case OP_RELOCATION_NUMBER:
			bassert(value_size == 8);
			memcpy(&tctx->data[dest_value_offset], &peek_relocation(vi).offset, sizeof(s32));
			add_patch(tctx, peek_relocation(vi).hash, dest_value_offset, IMAGE_REL_AMD64_ADDR64, SECTION_DATA);
			break;
		default:
			BL_UNIMPLEMENTED;
		}

		release_value(tctx, vi);
	}
}

static void emit_local_compound(struct context *ctx, struct thread_context *tctx, struct mir_instr *instr, const u64 vi_dest, u32 value_size) {
	bassert(instr && instr->kind == MIR_INSTR_COMPOUND);
	struct mir_instr_compound *cmp = (struct mir_instr_compound *)instr;
	bassert(!mir_is_global(&cmp->base));
	bassert(peek(vi_dest).kind == OFFSET);
	const s32 dest_offset = peek_offset(vi_dest);
	if (mir_is_zero_initialized(cmp)) {
		if (value_size <= REGISTER_SIZE) {
			mov_mi(tctx, RBP, dest_offset, 0, value_size);
		} else {
			emit_call_memset(tctx, vi_dest, 0, value_size);
		}
		return;
	}

	// The rest is local, non-comptime and not zero initialized.
	struct mir_type *type = cmp->base.value.type;
	bassert(type);

	mir_instrs_t *values  = cmp->values;
	ints_t       *mapping = cmp->value_member_mapping;

	if (mir_is_composite_type(type) && sarrlenu(values) < sarrlenu(type->data.strct.members)) {
		// Zero initialization only for composit types when not all values are initialized.
		if (value_size <= REGISTER_SIZE) {
			mov_mi(tctx, RBP, dest_offset, 0, value_size);
		} else {
			emit_call_memset(tctx, vi_dest, 0, value_size);
		}
	}

	for (usize i = 0; i < sarrlenu(values); ++i) {
		struct mir_instr *value_instr = sarrpeek(values, i);
		const u32         value_size  = (u32)value_instr->value.type->store_size_bytes;

		if (value_instr->kind == MIR_INSTR_LOAD) {
			enum x64_register reg = get_temporary_register(tctx, NULL, 0);
			emit_load_to_register(tctx, value_instr, reg);
		} else if (value_instr->kind == MIR_INSTR_COMPOUND) {
			BL_UNIMPLEMENTED;
			break;
		}

		const u64 vi = get_value(tctx, value_instr);

		s32 dest_value_offset = 0;
		if (mir_is_composite_type(type)) {
			const usize index = mapping ? sarrpeek(mapping, i) : i;
			dest_value_offset = dest_offset + (s32)vm_get_struct_elem_offset(ctx->assembly, type, (u32)index);
		} else {
			bassert(type->kind == MIR_TYPE_ARRAY);
			bassert(mapping == NULL && "Mapping is currently not supported for arrays.");
			dest_value_offset = dest_offset + (s32)vm_get_array_elem_offset(type, (u32)i);
		}

		enum op_kind op_kind = op(peek(vi).kind, value_instr->value.type);
		switch (op_kind) {
		case OP_IMMEDIATE_NUMBER:
			mov_mi(tctx, RBP, dest_value_offset, peek_immediate(vi), value_size);
			break;
		case OP_REGISTER_NUMBER:
			mov_mr(tctx, RBP, dest_value_offset, peek_register(vi), value_size);
			break;
		default:
			BL_UNIMPLEMENTED;
		}

		release_value(tctx, vi);
	}

	// @Incomplete: set_value?
}

static inline bool is_relational_binop(enum binop_kind op) {
	switch (op) {
	case BINOP_EQ:
	case BINOP_NEQ:
	case BINOP_LESS:
	case BINOP_GREATER:
	case BINOP_LESS_EQ:
	case BINOP_GREATER_EQ:
		return true;
	default:
		return false;
	}
}

static u64 emit_relational_binop_jmp(struct thread_context *tctx, enum binop_kind op) {
	switch (op) {
	case BINOP_EQ:
		jne_relative_i32(tctx, 0x0);
		break;
	case BINOP_NEQ:
		je_relative_i32(tctx, 0x0);
		break;
	case BINOP_LESS:
		jge_relative_i32(tctx, 0x0);
		break;
	case BINOP_GREATER:
		jle_relative_i32(tctx, 0x0);
		break;
	case BINOP_LESS_EQ:
		jg_relative_i32(tctx, 0x0);
		break;
	case BINOP_GREATER_EQ:
		jl_relative_i32(tctx, 0x0);
		break;
	default:
		babort("Binary operation is not relational.");
	}
	return get_position(tctx, SECTION_TEXT);
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
		cmp_rr(tctx, reg1, reg2, vsize);
		break;
	default:
		BL_UNIMPLEMENTED;
	}
}

static void emit_instr(struct context *ctx, struct thread_context *tctx, struct mir_instr *instr) {
	bassert(instr->state == MIR_IS_COMPLETE && "Attempt to emit instruction in incomplete state!");
	if (!mir_type_has_llvm_representation((instr->value.type))) return;

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

		const s32 section_number = isflag(fn->flags, FLAG_EXTERN) ? SECTION_EXTERN : SECTION_TEXT;
		add_sym(tctx, section_number, get_position(tctx, section_number), linkage_name, IMAGE_SYM_CLASS_EXTERNAL, DT_FUNCTION);

		// External functions does not have any body block.
		if (isflag(fn->flags, FLAG_EXTERN)) {
			return;
		}

		// Prologue
		push64_r(tctx, RBP);
		mov_rr(tctx, RBP, RSP, 8);
		sub_ri(tctx, RSP, 0, 8);
		tctx->stack.alloc_value_offset = get_position(tctx, SECTION_TEXT) - sizeof(s32);

		// @Explain!!!
		s32 reg_index = 0;
		for (usize i = 0; i < sarrlenu(fn->type->data.fn.args) && reg_index < static_arrlenu(CALL_ABI); ++i) {
			struct mir_arg *arg = sarrpeek(fn->type->data.fn.args, i);
			if (isflag(arg->flags, FLAG_COMPTIME)) continue;

			const enum x64_register reg = CALL_ABI[reg_index];
			bassert(tctx->register_table[reg] == UNUSED_REGISTER_MAP_VALUE && "Register already used?");
			struct x64_value value    = {.kind = REGISTER, .reg = reg};
			tctx->register_table[reg] = (s32)arrlen(tctx->values);
			arrput(tctx->values, value);

			arg->backend_value = arrlenu(tctx->values);
			++reg_index;
		}

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
			// Global initializer block.
			bassert(block->base.value.is_comptime);
		} else {
			str_buf_t name = get_tmp_str();
#ifdef BL_DEBUG
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

	// Generated ad-hoc.
	case MIR_INSTR_LOAD:
	case MIR_INSTR_COMPOUND:
		break;

	case MIR_INSTR_ARG:
		struct mir_instr_arg *arg_instr = (struct mir_instr_arg *)instr;

		struct mir_fn *fn = arg_instr->base.owner_block->owner_fn;
		bassert(fn);
		struct mir_type *fn_type = fn->type;
		struct mir_arg  *arg     = sarrpeek(fn_type->data.fn.args, arg_instr->i);
		bassert(isnotflag(arg->flags, FLAG_COMPTIME) && "Comptime arguments should be evaluated and replaced by constants!");
		bassert(arg->llvm_easgm == LLVM_EASGM_NONE);

		const enum x64_type_kind type_kind = get_type_kind(arg->type);
		const s32                index     = arg->llvm_index;

		if (index < static_arrlenu(CALL_ABI)) {
			enum x64_register reg = -1;

			// @Explain!!!
			const u64 vi = get_value(tctx, arg);
			if (peek(vi).kind != REGISTER) {
				bassert(peek(vi).kind == OFFSET);
				reg = spill(tctx, CALL_ABI[index], NULL, 0);
				mov_rm(tctx, reg, RBP, peek_offset(vi), 8);
			} else {
				reg = peek_register(vi);
			}

			release_value(tctx, vi);

			if (type_kind == X64_COMPOSIT) {
				set_value(tctx, instr, (struct x64_value){.kind = REGISTER_ADDRESS, .reg_off_addr.reg = reg});
			} else {
				bassert(arg->type->store_size_bytes < 9);
				set_value(tctx, instr, (struct x64_value){.kind = REGISTER, .reg = reg});
			}
		} else {
			const s32 arg_stack_offset = (index + 2) * 8; // +1 return address pushed by call
			if (type_kind == X64_COMPOSIT) {
				const enum x64_register reg = get_temporary_register(tctx, CALL_ABI, static_arrlenu(CALL_ABI));
				mov_rm(tctx, reg, RBP, arg_stack_offset, 8);
				set_value(tctx, instr, (struct x64_value){.kind = REGISTER_ADDRESS, .reg_off_addr.reg = reg});
			} else {
				set_value(tctx, instr, (struct x64_value){.kind = OFFSET, .offset = arg_stack_offset});
			}
		}
		break;

	case MIR_INSTR_STORE: {
		struct mir_instr_store *store      = (struct mir_instr_store *)instr;
		struct mir_type        *type       = store->src->value.type;
		const u32               value_size = (u32)type->store_size_bytes;

		if (store->dest->kind == MIR_INSTR_LOAD) {
			enum x64_register reg = get_temporary_register(tctx, NULL, 0);
			emit_load_to_register(tctx, store->dest, reg);
		}
		const u64 vi_dest = get_value(tctx, store->dest);

		if (store->src->kind == MIR_INSTR_LOAD) {
			enum x64_register reg = get_temporary_register(tctx, NULL, 0);
			emit_load_to_register(tctx, store->src, reg);
		} else if (store->src->kind == MIR_INSTR_COMPOUND) {
			emit_local_compound(ctx, tctx, store->src, vi_dest, value_size);
			release_value(tctx, vi_dest);
			break;
		}

		const u64 vi_src      = get_value(tctx, store->src);
		const s32 dest_offset = peek(vi_dest).kind == OFFSET ? peek_offset(vi_dest) : 0;

		u32 reloc_position = 0;

		const enum op_kind dest_kind = op(peek(vi_dest).kind, type);
		const enum op_kind src_kind  = op(peek(vi_src).kind, type);

		switch (op_combined(dest_kind, src_kind)) {
		case op_combined(OP_OFFSET_NUMBER, OP_IMMEDIATE_NUMBER):
			mov_mi(tctx, RBP, dest_offset, peek_immediate(vi_src), value_size);
			break;
		case op_combined(OP_RELOCATION_NUMBER, OP_IMMEDIATE_NUMBER):
			mov_mi_indirect(tctx, peek_relocation(vi_dest).offset, peek_immediate(vi_src), value_size);
			reloc_position = get_position(tctx, SECTION_TEXT) - sizeof(u64);
			add_patch(tctx, peek_relocation(vi_dest).hash, reloc_position, IMAGE_REL_AMD64_REL32_4, SECTION_TEXT);
			break;
		case op_combined(OP_OFFSET_NUMBER, OP_REGISTER_NUMBER):
			mov_mr(tctx, RBP, dest_offset, peek_register(vi_src), value_size);
			break;
		case op_combined(OP_RELOCATION_NUMBER, OP_REGISTER_NUMBER): {
			// register -> relocated memory.
			mov_mr_indirect(tctx, peek_relocation(vi_dest).offset, peek_register(vi_src), value_size);
			reloc_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
			add_patch(tctx, peek_relocation(vi_dest).hash, reloc_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
			break;
		}
		case op_combined(OP_OFFSET_COMPOSIT, OP_RELOCATION_COMPOSIT):
			emit_copy_relocation_to_offset(tctx, vi_dest, vi_src, value_size);
			break;
		case op_combined(OP_REGISTER_OFFSET_NUMBER, OP_IMMEDIATE_NUMBER):
			mov_mi_sib(tctx, RBP, peek_register_offset(vi_dest).reg, peek_register_offset(vi_dest).offset, peek_immediate(vi_src), value_size);
			break;
		case op_combined(OP_REGISTER_OFFSET_NUMBER, OP_REGISTER_NUMBER):
			mov_mr_sib(tctx, RBP, peek_register_offset(vi_dest).reg, peek_register_offset(vi_dest).offset, peek_register(vi_src), value_size);
			break;
		case op_combined(OP_REGISTER_ADDRESS_NUMBER, OP_REGISTER_NUMBER):
			mov_mr(tctx, peek_register_address(vi_dest).reg, peek_register_address(vi_dest).offset, peek_register(vi_src), value_size);
			break;
		case op_combined(OP_REGISTER_ADDRESS_NUMBER, OP_IMMEDIATE_NUMBER):
			mov_mi(tctx, peek_register_address(vi_dest).reg, peek_register_address(vi_dest).offset, peek_immediate(vi_src), value_size);
			break;

		default:
			babort("Unhandled operation kind.");
		}
		release_value(tctx, vi_dest);
		release_value(tctx, vi_src);
		break;
	}

	case MIR_INSTR_MEMBER_PTR: {
		struct mir_instr_member_ptr *mem = (struct mir_instr_member_ptr *)instr;
		if (mem->target_ptr->kind == MIR_INSTR_LOAD) {
			enum x64_register reg = get_temporary_register(tctx, NULL, 0);
			emit_load_to_register(tctx, mem->target_ptr, reg);
		}

		const u64 vi_target = get_value(tctx, mem->target_ptr);

		if (mem->builtin_id != BUILTIN_ID_NONE) {
			BL_UNIMPLEMENTED;
		}

		struct mir_member *member = mem->scope_entry->data.member;
		bassert(member);

		if (member->is_parent_union) {
			BL_UNIMPLEMENTED;
		} else {
			const s32        index       = (u32)member->index;
			struct mir_type *target_type = mir_deref_type(mem->target_ptr->value.type);
			const s32        offset      = (s32)vm_get_struct_elem_offset(ctx->assembly, target_type, index);
			struct x64_value value;
			switch (peek(vi_target).kind) {
			case OFFSET:
				value = (struct x64_value){
				    .kind   = OFFSET,
				    .offset = peek_offset(vi_target) + offset,
				};
				break;
			case RELOCATION:
				value = (struct x64_value){
				    .kind         = RELOCATION,
				    .reloc.hash   = peek_relocation(vi_target).hash,
				    .reloc.offset = peek_relocation(vi_target).offset + offset,
				};
				break;
			case REGISTER:
				value = (struct x64_value){
				    .kind                = REGISTER_ADDRESS,
				    .reg_off_addr.reg    = peek_register(vi_target),
				    .reg_off_addr.offset = offset,
				};
				break;
			case REGISTER_ADDRESS:
				value = (struct x64_value){
				    .kind                = REGISTER_ADDRESS,
				    .reg_off_addr.reg    = peek_register_address(vi_target).reg,
				    .reg_off_addr.offset = peek_register_address(vi_target).offset + offset,
				};
				break;
			default:
				BL_UNIMPLEMENTED;
			}

			release_value(tctx, vi_target);
			set_value(tctx, instr, value);
		}
		break;
	}

	case MIR_INSTR_ADDROF: {
		struct mir_instr_addrof *addr = (struct mir_instr_addrof *)instr;

		if (addr->src->kind == MIR_INSTR_LOAD) {
			enum x64_register reg = get_temporary_register(tctx, NULL, 0);
			emit_load_to_register(tctx, addr->src, reg);
		}

		const u64          vi_src     = get_value(tctx, addr->src);
		const usize        value_size = addr->src->value.type->store_size_bytes;
		const enum op_kind op_kind    = op(peek(vi_src).kind, addr->src->value.type);

		if (mir_is_global(instr)) {
			switch (op_kind) {
			case OP_RELOCATION_NUMBER: {
				set_value(tctx, instr, peek(vi_src));
				break;
			}
			default:
				BL_UNIMPLEMENTED;
			}
		} else {
			switch (op_kind) {
			case OP_RELOCATION_NUMBER: {
				enum x64_register reg = get_temporary_register(tctx, NULL, 0);
				lea_rm_indirect(tctx, reg, peek_relocation(vi_src).offset, value_size);
				const u32 reloc_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
				add_patch(tctx, peek_relocation(vi_src).hash, reloc_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
				set_value(tctx, instr, (struct x64_value){.kind = REGISTER, .reg = reg});
				break;
			}
			case OP_OFFSET_NUMBER: {
				enum x64_register reg = get_temporary_register(tctx, NULL, 0);
				lea_rm(tctx, reg, RBP, peek_offset(vi_src), 8);
				set_value(tctx, instr, (struct x64_value){.kind = REGISTER, .reg = reg});
				break;
			}
			default:
				BL_UNIMPLEMENTED;
			}
		}
		release_value(tctx, vi_src);
		break;
	}

	case MIR_INSTR_ELEM_PTR: {
		struct mir_instr_elem_ptr *elem = (struct mir_instr_elem_ptr *)instr;

		if (elem->index->kind == MIR_INSTR_LOAD) {
			enum x64_register reg = get_temporary_register(tctx, NULL, 0);
			emit_load_to_register(tctx, elem->index, reg);
		}

		const u64        vi_target   = get_value(tctx, elem->arr_ptr);
		const u64        vi_index    = get_value(tctx, elem->index);
		struct mir_type *target_type = mir_deref_type(elem->arr_ptr->value.type);

		struct x64_value result = {0};

		bassert(peek(vi_target).kind == OFFSET);
		const s32 target_offset = peek_offset(vi_target);

		switch (target_type->kind) {
		case MIR_TYPE_ARRAY:
			struct mir_type *elem_type = target_type->data.array.elem_type;
			switch (peek(vi_index).kind) {
			case IMMEDIATE: {
				const s32 offset = (s32)vm_get_array_elem_offset(target_type, (u32)peek_immediate(vi_index));

				result = (struct x64_value){
				    .kind   = OFFSET,
				    .offset = target_offset + offset,
				};
				break;
			}

			case REGISTER: {
				const s32               elem_size = (s32)elem_type->store_size_bytes;
				const enum x64_register reg       = peek_register(vi_index);
				imul_ri(tctx, reg, reg, elem_size, 8);

				result = (struct x64_value){
				    .kind                = REGISTER_OFFSET,
				    .reg_off_addr.reg    = reg,
				    .reg_off_addr.offset = target_offset,
				};
				break;
			}
			default:
				BL_UNIMPLEMENTED;
			}
			break;

		case MIR_TYPE_DYNARR:
		case MIR_TYPE_SLICE:
		case MIR_TYPE_STRING:
		case MIR_TYPE_VARGS: {
			const s32        elem_ptr_offset = (s32)vm_get_struct_elem_offset(ctx->assembly, target_type, MIR_SLICE_PTR_INDEX);
			struct mir_type *elem_type       = mir_get_struct_elem_type(target_type, MIR_SLICE_PTR_INDEX);
			bassert(mir_is_pointer_type(elem_type));
			const usize       elem_ptr_size = elem_type->store_size_bytes;
			const usize       elem_size     = mir_deref_type(elem_type)->store_size_bytes;
			enum x64_register reg           = get_temporary_register(tctx, NULL, 0);
			mov_rm(tctx, reg, RBP, target_offset + elem_ptr_offset, elem_ptr_size);

			switch (peek(vi_index).kind) {
			case IMMEDIATE: {
				result = (struct x64_value){
				    .kind                = REGISTER_ADDRESS,
				    .reg_off_addr.reg    = reg,
				    .reg_off_addr.offset = (s32)(peek_immediate(vi_index) * elem_size),
				};
				break;
			}
			case REGISTER: {
				const enum x64_register index_reg = peek_register(vi_index);
				imul_ri(tctx, index_reg, index_reg, elem_size, sizeof(s64)); // i * elem_size
				add_rr(tctx, reg, index_reg, sizeof(s64));                   // + base pointer

				result = (struct x64_value){
				    .kind             = REGISTER_ADDRESS,
				    .reg_off_addr.reg = reg,
				};
				break;
			}
			default:
				BL_UNIMPLEMENTED;
			}

			break;
		}
		default:
			babort("Invalid elem ptr target type!");
		}

		release_value(tctx, vi_target);
		release_value(tctx, vi_index);
		set_value(tctx, instr, result);

		break;
	}

	case MIR_INSTR_CAST: {
		struct mir_instr_cast *cast = (struct mir_instr_cast *)instr;

		const usize dest_size = cast->base.value.type->store_size_bytes;
		const usize src_size  = cast->expr->value.type->store_size_bytes;

		if (cast->expr->kind == MIR_INSTR_LOAD) {
			enum x64_register reg = get_temporary_register(tctx, NULL, 0);
			emit_load_to_register(tctx, cast->expr, reg);
		}

		const u64 vi = get_value(tctx, cast->expr);
		switch (cast->op) {
		case MIR_CAST_NONE:
		case MIR_CAST_BITCAST:
		case MIR_CAST_TRUNC:
		case MIR_CAST_PTRTOINT:
			release_value(tctx, vi);
			set_value(tctx, instr, peek(vi));
			goto SKIP;
		default:
			break;
		}

		bassert(peek(vi).kind == REGISTER);
		const enum x64_register reg = peek_register(vi);

		switch (cast->op) {
		case MIR_CAST_ZEXT:
			bassert(dest_size > src_size);
			if (src_size < 4) {
				BL_UNIMPLEMENTED;
			}
			break;

		case MIR_CAST_SEXT:
			bassert(dest_size > src_size);
			movsx_rr(tctx, reg, reg, dest_size, src_size);
			break;

		case MIR_CAST_PTRTOBOOL:
			cmp_ri(tctx, reg, 0, src_size);
			setne(tctx, reg);
			and_ri(tctx, reg, 1, ctx->assembly->builtin_types.t_bool->store_size_bytes);
			break;

		case MIR_CAST_FPTOSI:
		case MIR_CAST_FPTOUI:
		case MIR_CAST_FPTRUNC:
		case MIR_CAST_FPEXT:
		case MIR_CAST_SITOFP:
		case MIR_CAST_UITOFP:
		case MIR_CAST_INTTOPTR:
			BL_UNIMPLEMENTED;

		default:
			babort("invalid cast type");
		}

		release_value(tctx, vi);
		bassert(reg != -1);
		set_value(tctx, instr, (struct x64_value){.kind = REGISTER, .reg = reg});
	SKIP:
		break;
	}

	case MIR_INSTR_UNOP: {
		struct mir_instr_unop *unop       = (struct mir_instr_unop *)instr;
		const usize            value_size = instr->value.type->store_size_bytes;

		if (unop->expr->kind == MIR_INSTR_LOAD) {
			enum x64_register reg = get_temporary_register(tctx, NULL, 0);
			emit_load_to_register(tctx, unop->expr, reg);
		}

		const u64 vi = get_value(tctx, unop->expr);
		bassert(peek(vi).kind == REGISTER);
		const enum x64_register reg    = peek_register(vi);
		struct x64_value        result = peek(vi);

		switch (unop->op) {
		case UNOP_NEG: {
			enum x64_register zero = get_temporary_register(tctx, &reg, 1);
			zero_reg(tctx, zero, value_size);
			sub_rr(tctx, zero, reg, value_size);
			result.kind = REGISTER;
			result.reg  = zero;
			break;
		}

		case UNOP_NOT: {
			cmp_ri(tctx, reg, 0, value_size);
			if (!unop->is_condition) {
				bassert(value_size == 1);
				sete(tctx, reg);
				and_ri(tctx, reg, 1, ctx->assembly->builtin_types.t_bool->store_size_bytes);
			}
			break;
		}
		default:
			BL_UNIMPLEMENTED;
		}

		release_value(tctx, vi);
		set_value(tctx, instr, result);
		break;
	}

	case MIR_INSTR_BINOP: {
		// The result value ends up in LHS register, the RHS register is released at the end
		// if it was used.
		struct mir_instr_binop *binop = (struct mir_instr_binop *)instr;
		struct mir_type        *type  = binop->lhs->value.type;
		bassert(type->kind == MIR_TYPE_INT || type->kind == MIR_TYPE_PTR || type->kind == MIR_TYPE_BOOL);

		const s32 LHS                = 0;
		const s32 RHS                = 1;
		s32       required_registers = 1; // LHS goes into register.

		enum x64_register regs[2] = {-1, -1};
		if (binop->rhs->kind == MIR_INSTR_LOAD) ++required_registers;

		find_free_registers(tctx, regs, required_registers, NULL, 0);

		if (binop->rhs->kind == MIR_INSTR_LOAD) {
			if (regs[RHS] == -1) regs[RHS] = spill(tctx, RCX, regs, 2);
			emit_load_to_register(tctx, binop->rhs, regs[RHS]);
		}

		if (binop->lhs->kind == MIR_INSTR_LOAD) {
			if (regs[LHS] == -1) regs[LHS] = spill(tctx, RAX, regs, 2);
			emit_load_to_register(tctx, binop->lhs, regs[LHS]);
		}

		const u64              vi_lhs    = get_value(tctx, binop->lhs);
		const u64              vi_rhs    = get_value(tctx, binop->rhs);
		const struct x64_value rhs_value = peek(vi_rhs);
		struct x64_value       lhs_value = peek(vi_lhs);

		if (peek(vi_lhs).kind == IMMEDIATE) {
			if (regs[LHS] == -1) regs[LHS] = spill(tctx, RAX, regs, 2);
			mov_ri(tctx, regs[LHS], lhs_value.imm, type->store_size_bytes);
			lhs_value.kind = REGISTER;
			lhs_value.reg  = regs[LHS];
		}

		// Left hand side value must be in the register, the same register later contains the result of the operation.
		bassert(lhs_value.kind == REGISTER);

		const enum op_kind op_kind = op(rhs_value.kind, type);
		switch (op_kind) {
		case OP_IMMEDIATE_NUMBER:
			emit_binop_ri(tctx, binop->op, lhs_value.reg, rhs_value.imm, type->store_size_bytes);
			break;
		case OP_REGISTER_NUMBER:
			emit_binop_rr(tctx, binop->op, lhs_value.reg, rhs_value.reg, type->store_size_bytes);
			break;

		default:
			babort("Unhandled operation kind.");
		}

		bassert(instr->backend_value == 0);
		bassert(regs[LHS] != -1);

		if (!binop->is_condition && is_relational_binop(binop->op)) {
			// In this case the operation result is not used in conditional jump; the result is used as a bool value
			// instead, so we have to emit code setting LHS register to true or false based on the condition.
			bassert(binop->base.value.type->kind == MIR_TYPE_BOOL);
			str_buf_t name = get_tmp_str();

			const u64 p0 = emit_relational_binop_jmp(tctx, binop->op);

			mov_ri(tctx, regs[LHS], 1, 8); // Lets set whole register.
			jmp_relative_i32(tctx, 0);
			const u64 p1 = get_position(tctx, SECTION_TEXT);

			unique_name(ctx, &name, ".", cstr("B"));
			add_block(tctx, str_buf_view(name));
			*data_ptr_s32(tctx->text, p0 - sizeof(s32)) = (s32)(get_position(tctx, SECTION_TEXT) - p0);

			zero_reg(tctx, regs[LHS], 8);

			unique_name(ctx, &name, ".", cstr("B"));
			add_block(tctx, str_buf_view(name));
			*data_ptr_s32(tctx->text, p1 - sizeof(s32)) = (s32)(get_position(tctx, SECTION_TEXT) - p1);

			put_tmp_str(name);
		}

		release_value(tctx, vi_lhs);
		release_value(tctx, vi_rhs);
		set_value(tctx, instr, (struct x64_value){.kind = REGISTER, .reg = regs[LHS]});
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
			jmp_relative_i32(tctx, 0x0);
			const u64 position = get_position(tctx, SECTION_TEXT) - sizeof(s32);

			struct sym_patch patch = {
			    .instr    = &then_block->base,
			    .position = position,
			};
			arrput(tctx->local_patches, patch);
		} else {
			// Generate block immediately after...
			arrput(tctx->emit_block_queue, then_block);
		}

		break;
	}

	case MIR_INSTR_COND_BR: {
		// Conditional break transtation is a bit more complicated. In case the condition is binary or unary expression
		// we suppose the last generated instruction to be cmp. When value is loaded from variable we need to emit cmp
		// instruction manually.
		struct mir_instr_cond_br *br = (struct mir_instr_cond_br *)instr;
		bassert(br->cond && br->then_block && br->else_block);
		bassert(br->else_block->base.backend_value == 0);

		if (br->cond->kind == MIR_INSTR_LOAD) {
			enum x64_register reg = get_temporary_register(tctx, NULL, 0);
			emit_load_to_register(tctx, br->cond, reg);
		}

		const u64 vi_cond = get_value(tctx, br->cond);
		bassert(peek(vi_cond).kind == REGISTER);
		const enum x64_register reg = peek_register(vi_cond);

		const usize value_size     = br->cond->value.type->store_size_bytes;
		u64         patch_position = 0;

		if (br->cond->kind == MIR_INSTR_BINOP) {
			// Last generated instruction must be cmp.
			struct mir_instr_binop *cond_binop = (struct mir_instr_binop *)br->cond;
			bassert(cond_binop->is_condition);
			patch_position = emit_relational_binop_jmp(tctx, cond_binop->op);
		} else if (br->cond->kind == MIR_INSTR_UNOP) {
			// Last generated instruction must be cmp.
			struct mir_instr_unop *cond_unop = (struct mir_instr_unop *)br->cond;
			switch (cond_unop->op) {
			case UNOP_NOT:
				jne_relative_i32(tctx, 0x0);
				break;
			default:
				BL_UNIMPLEMENTED;
			}
		} else {
			// Compare register value with 0 and jump.
			cmp_ri(tctx, reg, 0, value_size);
			je_relative_i32(tctx, 0x0);
			patch_position = get_position(tctx, SECTION_TEXT);
		}

		struct sym_patch patch = {
		    .instr    = &br->else_block->base,
		    .position = patch_position - sizeof(s32),
		};
		arrput(tctx->local_patches, patch);

		if (br->else_block->base.backend_value == 0) {
			arrput(tctx->emit_block_queue, br->else_block);
		}

		// Then block immediately after conditional break.
		bassert(br->then_block->base.backend_value == 0);
		arrput(tctx->emit_block_queue, br->then_block);

		release_value(tctx, vi_cond);
		break;
	}

	case MIR_INSTR_CALL: {
		// x64 calling convention:
		// numbers - RCX, RDX, R8, R9, stack in reverse order

		struct mir_instr_call *call = (struct mir_instr_call *)instr;
		bassert(!mir_is_comptime(&call->base) && "Compile time calls should not be generated into the final binary!");
		struct mir_instr *callee = call->callee;
		bassert(callee);

		struct mir_type *callee_type = callee->value.type->kind == MIR_TYPE_FN ? callee->value.type : mir_deref_type(callee->value.type);
		bassert(callee_type);
		bassert(callee_type->kind == MIR_TYPE_FN);

		// There are three possible configurations:
		//
		// 1) We call regular static function; it's compile-time known and resolved by previous decl-ref instruction.
		// 2) We call via function pointer.
		// 3) We call immediate inline defined anonymous function (we have to generate one eventually).

		if (callee->kind == MIR_INSTR_LOAD) {
			enum x64_register reg = get_temporary_register(tctx, CALL_ABI, static_arrlenu(CALL_ABI));
			emit_load_to_register(tctx, callee, reg);
		}
		const u64 vi_callee = get_value(tctx, callee);
		const s32 arg_num   = (s32)sarrlen(call->args);

		usize stack_space = MAX(4, arg_num) * 8;
		if (!is_aligned2(stack_space, 16)) {
			stack_space = next_aligned2(stack_space, 16);
		}

		if (stack_space) sub_ri(tctx, RSP, stack_space, 8);
		const s32 args_in_register = MIN(static_arrlenu(CALL_ABI), arg_num);

		s32 arg_stack_offset = 0;

		for (s32 index = 0; index < arg_num; ++index) {
			struct mir_instr *arg_instr = sarrpeek(call->args, index);
			struct mir_arg   *arg       = sarrpeek(callee_type->data.fn.args, index);
			struct mir_type  *arg_type  = arg_instr->value.type;

			bassert(!isflag(arg->flags, FLAG_COMPTIME));
			// if (isflag(arg->flags, FLAG_COMPTIME)) continue;
			bassert(arg->llvm_easgm == LLVM_EASGM_NONE);

			const usize              arg_size  = arg_type->store_size_bytes;
			const enum x64_type_kind type_kind = get_type_kind(arg_type);

			if (index < 4) {
				// To register
				if (arg_instr->kind == MIR_INSTR_LOAD) {
					const enum x64_register reg = spill(tctx, CALL_ABI[index], CALL_ABI, args_in_register);
					emit_load_to_register(tctx, arg_instr, reg);
				} else {
					const u64 vi = get_value(tctx, arg_instr);
					switch (peek(vi).kind) {
					case IMMEDIATE: {
						bassert(type_kind != X64_COMPOSIT);
						const enum x64_register reg = spill(tctx, CALL_ABI[index], CALL_ABI, args_in_register);
						mov_ri(tctx, reg, peek_immediate(vi), arg_size);
						break;
					}
					case REGISTER: {
						bassert(type_kind != X64_COMPOSIT);
						if (peek_register(vi) != CALL_ABI[index]) {
							const enum x64_register reg = spill(tctx, CALL_ABI[index], CALL_ABI, args_in_register);
							mov_rr(tctx, reg, peek_register(vi), arg_size);
						}
						break;
					}
					case OFFSET: {
						bassert(type_kind != X64_COMPOSIT);
						const enum x64_register reg = spill(tctx, CALL_ABI[index], CALL_ABI, args_in_register);
						mov_rm(tctx, reg, RBP, peek_offset(vi), arg_size);
						break;
					}
					case RELOCATION: {
						const enum x64_register reg    = spill(tctx, CALL_ABI[index], CALL_ABI, args_in_register);
						const s32               offset = peek_relocation(vi).offset;
						if (type_kind == X64_COMPOSIT) {
							lea_rm_indirect(tctx, reg, offset, 8);
						} else {
							mov_rm_indirect(tctx, reg, offset, arg_size);
						}
						const u32 reloc_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
						add_patch(tctx, peek_relocation(vi).hash, reloc_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
						break;
					}
					default:
						babort("Invalid value kind!");
					}
				}
			} else {
				// Other args needs go to the stack.
				if (arg_instr->kind == MIR_INSTR_LOAD) {
					const enum x64_register reg = spill(tctx, RAX, NULL, 0);
					emit_load_to_register(tctx, arg_instr, reg);
				} else if (arg_instr->kind == MIR_INSTR_COMPOUND) {
					BL_UNIMPLEMENTED;
				}

				// @Note here we're relative to RSP!
				const u64 vi = get_value(tctx, arg_instr);
				switch (peek(vi).kind) {
				case IMMEDIATE: {
					bassert(type_kind != X64_COMPOSIT);
					mov_mi(tctx, RSP, arg_stack_offset, peek_immediate(vi), arg_size);
					break;
				}
				case REGISTER: {
					bassert(type_kind != X64_COMPOSIT);
					mov_mr(tctx, RSP, arg_stack_offset, peek_register(vi), arg_size);
					break;
				}
				case OFFSET: {
					bassert(type_kind != X64_COMPOSIT);
					// @Incomplete: Maybe spilled value???
					BL_UNIMPLEMENTED;
					break;
				}
				case RELOCATION: {
					if (type_kind == X64_COMPOSIT) {
						enum x64_register reg = get_temporary_register(tctx, CALL_ABI, static_arrlenu(CALL_ABI));
						lea_rm_indirect(tctx, reg, peek_relocation(vi).offset, 8);
						const u32 reloc_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
						add_patch(tctx, peek_relocation(vi).hash, reloc_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
						mov_mr(tctx, RSP, arg_stack_offset, reg, 8);
					} else {
						BL_UNIMPLEMENTED;
					}
					break;
				}
				default:
					babort("Invalid value kind!");
				}
			}

			arg_stack_offset += 8;
			release_value(tctx, arg_instr);
		}

		const enum x64_value_kind callee_kind = peek(vi_callee).kind;
		if (callee_kind == RELOCATION) {
			call_relative_i32(tctx, 0);
			const u64 reloc_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
			add_patch(tctx, peek_relocation(vi_callee).hash, reloc_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
		} else if (callee_kind == REGISTER) {
			call_r(tctx, peek_register(vi_callee));
		} else {
			BL_UNIMPLEMENTED;
		}

		if (stack_space) add_ri(tctx, RSP, stack_space, 8);
		release_value(tctx, vi_callee);

		// Store RAX register in case the function returns and the result is used.
		if ((callee->value.type->data.fn.ret_type->kind != MIR_TYPE_VOID) && call->base.ref_count > 1) {
			enum x64_register reg = spill(tctx, RAX, NULL, 0);
			set_value(tctx, instr, (struct x64_value){.kind = REGISTER, .reg = reg});
		}

		break;
	}

	case MIR_INSTR_CONST: {
		struct mir_instr_const *cnst = (struct mir_instr_const *)instr;
		struct mir_type        *type = cnst->base.value.type;
		bassert(type);
		struct x64_value value = {.kind = IMMEDIATE};

		const enum x64_type_kind type_kind = get_type_kind(type);
		switch (type_kind) {

		case X64_NUMBER:
			value.imm = vm_read_int(type, cnst->base.value.data);
			break;

		case X64_COMPOSIT: {
			if (type->data.strct.is_string_literal) {
				vm_stack_ptr_t len_ptr = vm_get_struct_elem_ptr(ctx->assembly, type, cnst->base.value.data, MIR_SLICE_LEN_INDEX);
				vm_stack_ptr_t str_ptr = vm_get_struct_elem_ptr(ctx->assembly, type, cnst->base.value.data, MIR_SLICE_PTR_INDEX);
				const s64      len     = vm_read_as(s64, len_ptr);
				char          *str     = vm_read_as(char *, str_ptr);

				str_buf_t        name      = get_tmp_str();
				struct sym_patch ptr_patch = {.type = IMAGE_REL_AMD64_ADDR64, .target_section = SECTION_DATA};
				{
					unique_name(ctx, &name, ".str", (const str_t){0});
					const u32 len_offset = get_position(tctx, SECTION_DATA);

					const u32 data_offset = add_data(tctx, NULL, (s32)type->store_size_bytes);
					add_sym(tctx, SECTION_DATA, data_offset, str_buf_view(name), IMAGE_SYM_CLASS_EXTERNAL, 0);
					memcpy(&tctx->data[len_offset], &len, mir_get_struct_elem_type(type, MIR_SLICE_LEN_INDEX)->store_size_bytes);
					ptr_patch.position = len_offset + mir_get_struct_elem_type(type, MIR_SLICE_PTR_INDEX)->store_size_bytes;

					value.kind       = RELOCATION;
					value.reloc.hash = strhash(name);
				}

				{
					unique_name(ctx, &name, ".dstr", (const str_t){0});

					const u32 data_offset = add_data(tctx, str, (s32)len);
					add_sym(tctx, SECTION_DATA, data_offset, str_buf_view(name), IMAGE_SYM_CLASS_EXTERNAL, 0);

					ptr_patch.hash = strhash(name);
				}

				arrput(tctx->patches, ptr_patch);
				put_tmp_str(name);
			} else {
				// Only string literals can be represented as constant for now! Other slices are not
				// handled.
				BL_UNIMPLEMENTED;
			}
			break;
		}

		default:
			BL_UNIMPLEMENTED;
		}
		set_value(tctx, instr, value);
		break;
	}

	case MIR_INSTR_CALL_LOC: {
		struct mir_instr_call_loc *loc = (struct mir_instr_call_loc *)instr;

		str_buf_t name = get_tmp_str();
		unique_name(ctx, &name, ".loc", (const str_t){0});
		const hash_t hash = strhash(name);

		const struct mir_type *type = ctx->assembly->builtin_types.t_CodeLocation;

		// Alocate in DATA segment.
		const u32 data_offset = add_data(tctx, NULL, (s32)type->store_size_bytes);
		add_sym(tctx, SECTION_DATA, data_offset, str_buf_view(name), IMAGE_SYM_CLASS_EXTERNAL, 0);

		// Write file
		char *filepath = loc->call_location->unit->filepath;
		emit_string_literal(ctx, tctx, data_offset, make_str_from_c(filepath));

		// Write line
		const u32 line_member_offset                  = data_offset + (u32)vm_get_struct_elem_offset(ctx->assembly, type, 1);
		*data_ptr_s32(tctx->data, line_member_offset) = loc->call_location->line;

		// Call Location yields pointer to static data...
		const enum x64_register reg = get_temporary_register(tctx, NULL, 0);
		lea_rm_indirect(tctx, reg, 0, 8);
		const u32 reloc_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
		add_patch(tctx, hash, reloc_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
		set_value(tctx, instr, (struct x64_value){.kind = REGISTER, .reg = reg});

		put_tmp_str(name);
		break;
	}

	case MIR_INSTR_DECL_VAR: {
		struct mir_instr_decl_var *decl = (struct mir_instr_decl_var *)instr;
		struct mir_var            *var  = decl->var;
		struct mir_type           *type = var->value.type;

		if (!mir_type_has_llvm_representation(var->value.type)) {
			blog("Skip variable: %.*s", var->linkage_name.len, var->linkage_name.ptr);
			break;
		}

		const u32 value_size = (u32)type->store_size_bytes;

		if (decl->init) {
			if (decl->init->kind == MIR_INSTR_LOAD) {
				if (var->ref_count == 0) break;
				enum x64_register reg = get_temporary_register(tctx, NULL, 0);
				emit_load_to_register(tctx, decl->init, reg);
			} else if (decl->init->kind == MIR_INSTR_COMPOUND) {
				if (var->ref_count == 0) break;
				emit_local_compound(ctx, tctx, decl->init, get_value(tctx, var), value_size);
				break; // we're done here
			}

			const u64 vi_init = get_value(tctx, decl->init);
			if (var->ref_count == 0) {
				release_value(tctx, vi_init);
				break;
			}

			const u64 vi_var = get_value(tctx, var);
			bassert(peek(vi_var).kind == OFFSET);
			const s32 var_offset = peek_offset(vi_var);

			const enum op_kind op_kind = op(peek(vi_init).kind, type);
			switch (op_kind) {
			case OP_OFFSET_NUMBER: {
				enum x64_register reg = get_temporary_register(tctx, NULL, 0);
				mov_rm(tctx, reg, RBP, peek_offset(vi_init), value_size);
				mov_mr(tctx, RBP, var_offset, reg, value_size);
				break;
			}
			case OP_IMMEDIATE_NUMBER:
				mov_mi(tctx, RBP, var_offset, peek_immediate(vi_init), value_size);
				break;
			case OP_REGISTER_NUMBER:
				mov_mr(tctx, RBP, var_offset, peek_register(vi_init), value_size);
				break;
			case OP_RELOCATION_NUMBER: {
				enum x64_register reg = get_temporary_register(tctx, NULL, 0);
				mov_rm_indirect(tctx, reg, peek_relocation(vi_init).offset, value_size);
				const u32 reloc_position = get_position(tctx, SECTION_TEXT) - sizeof(s32);
				add_patch(tctx, peek_relocation(vi_init).hash, reloc_position, IMAGE_REL_AMD64_REL32, SECTION_TEXT);
				mov_mr(tctx, RBP, var_offset, reg, value_size);
			}
			case OP_RELOCATION_COMPOSIT: {
				emit_copy_relocation_to_offset(tctx, vi_var, vi_init, value_size);
				break;
			}

			case OP_REGISTER_ADDRESS_COMPOSIT: {
				emit_call_memcpy(tctx, vi_var, vi_init, value_size);
				break;
			}

			default:
				babort("Unhandled operation kind.");
			}
			release_value(tctx, vi_init);
		}

		break;
	}

	case MIR_INSTR_DECL_REF: {
		struct mir_instr_decl_ref *ref = (struct mir_instr_decl_ref *)instr;

		// In some cases the symbol location might not be known (e.g.: function was not generated yet).
		// In this case we set this instruction to RELOCATION value and each use-case must record patch
		// for later resolving of the offset.

		struct scope_entry *entry = ref->scope_entry;
		bassert(entry);
		switch (entry->kind) {
		case SCOPE_ENTRY_VAR: {
			struct mir_var *var = entry->data.var;
			if (isflag(var->iflags, MIR_VAR_GLOBAL)) {
				const hash_t hash = submit_global_variable_generation(ctx, tctx, var);
				set_value(tctx, instr, (struct x64_value){.kind = RELOCATION, .reloc.hash = hash});
			} else {
				bassert(var->backend_value);
				ref->base.backend_value = var->backend_value;
			}
			break;
		}
		case SCOPE_ENTRY_FN: {
			// @Incomplete: Push function for generation.
			struct mir_fn *fn   = entry->data.fn;
			const hash_t   hash = submit_function_generation(ctx, fn);
			set_value(tctx, instr, (struct x64_value){.kind = RELOCATION, .reloc.hash = hash});

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

	case MIR_INSTR_SET_INITIALIZER: {
		struct mir_instr_set_initializer *si = (struct mir_instr_set_initializer *)instr;
		for (usize i = 0; i < sarrlenu(si->dests); ++i) {
			struct mir_instr *dest = sarrpeek(si->dests, i);
			struct mir_var   *var  = ((struct mir_instr_decl_var *)dest)->var;
			if (!var->ref_count) continue;

			const usize value_size  = var->value.type->store_size_bytes;
			const u32   data_offset = add_data(tctx, NULL, (s32)value_size);
			add_sym(tctx, SECTION_DATA, data_offset, var->linkage_name, IMAGE_SYM_CLASS_EXTERNAL, 0);

			if (si->src->backend_value == 0) {
				struct mir_instr *cmp = si->src;
				bassert(cmp->kind == MIR_INSTR_COMPOUND);
				emit_global_compound(ctx, tctx, cmp, data_offset, (u32)value_size);
				break;
			} else {
				const u64   vi_init  = get_value(tctx, si->src);
				const void *data_src = NULL;

				switch (peek(vi_init).kind) {
				case IMMEDIATE:
					data_src = &peek_immediate(vi_init);
					break;
				default:
					BL_UNIMPLEMENTED;
				}
				memcpy(tctx->data + data_offset, data_src, value_size);
				release_value(tctx, vi_init);
			}
		}
		break;
	}

	default:
		bwarn("Missing implementation for emitting '%s' instruction.", mir_instr_name(instr));
	}
}

static void patch_jump_offsets(struct thread_context *tctx) {
	for (usize i = 0; i < arrlenu(tctx->local_patches); ++i) {
		struct sym_patch *patch = &tctx->local_patches[i];
		bassert(patch->instr);
		bassert(patch->instr->kind == MIR_INSTR_BLOCK);
		const u64 backend_value = patch->instr->backend_value;
		if (!backend_value) {
			babort("Cannot resolve relative jump target block!");
		}

		bassert(tctx->values[backend_value - 1].kind == ADDRESS);
		const s32 block_position            = (s32)tctx->values[backend_value - 1].address;
		const s32 position                  = (s32)patch->position;
		*data_ptr_s32(tctx->text, position) = block_position - (position + sizeof(s32));
	}
}

void job(struct job_context *job_ctx) {
	zone();
	struct context        *ctx          = job_ctx->x64.ctx;
	const u32              thread_index = job_ctx->thread_index;
	struct thread_context *tctx         = &ctx->tctx[thread_index];

	// Reset buffers
	arrsetlen(tctx->text, 0);
	arrsetlen(tctx->data, 0);
	arrsetlen(tctx->syms, 0);
	arrsetlen(tctx->strs, 0);
	arrsetlen(tctx->values, 0);
	arrsetlen(tctx->emit_block_queue, 0);
	arrsetlen(tctx->local_patches, 0);
	arrsetlen(tctx->patches, 0);
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

	arrsetcap(tctx->text, BYTE_CODE_BUFFER_SIZE);

	emit_instr(ctx, tctx, job_ctx->x64.top_instr);
	check_dangling_registers(tctx);

	patch_jump_offsets(tctx);

	const usize text_len = arrlenu(tctx->text);
	const usize data_len = arrlenu(tctx->data);
	const usize syms_len = arrlenu(tctx->syms);
	const usize strs_len = arrlenu(tctx->strs);

	{
		pthread_mutex_lock(&ctx->mutex);

		// Write top-level function generated code into the code section if there is any generated code
		// present.

		const usize gtext_len = arrlenu(ctx->code.bytes);
		const usize gdata_len = arrlenu(ctx->data.bytes);
		const usize gsyms_len = arrlenu(ctx->syms);
		const usize gstrs_len = arrlenu(ctx->strs);

		// Insert code.
		arrsetcap(ctx->code.bytes, CODE_BLOCK_BUFFER_SIZE);
		arrsetlen(ctx->code.bytes, gtext_len + text_len);
		memcpy(&ctx->code.bytes[gtext_len], tctx->text, text_len);

		// Insert data.
		arrsetlen(ctx->data.bytes, gdata_len + data_len);
		memcpy(&ctx->data.bytes[gdata_len], tctx->data, data_len);

		// patch positions.
		for (usize i = 0; i < arrlenu(tctx->patches); ++i) {
			struct sym_patch *patch = &tctx->patches[i];
			bassert(patch->hash);
			if (patch->target_section == SECTION_TEXT) {
				patch->position += gtext_len;
			} else if (patch->target_section == SECTION_DATA) {
				patch->position += gdata_len;
			}
		}

		// Duplicate to global context.
		const usize gpatches_len = arrlenu(ctx->patches);
		const usize patches_len  = arrlenu(tctx->patches);
		arrsetlen(ctx->patches, gpatches_len + patches_len);
		memcpy(&ctx->patches[gpatches_len], tctx->patches, patches_len * sizeof(struct sym_patch));

		// Adjust symbol positions.
		for (usize i = 0; i < arrlenu(tctx->syms); ++i) {
			IMAGE_SYMBOL *sym = &tctx->syms[i];
			// 0 section number means the symbol is externally linked. There is no location in our
			// binary.
			switch (sym->SectionNumber) {
			case SECTION_EXTERN:
				break;
			case SECTION_TEXT:
				sym->Value += (DWORD)gtext_len;
				break;
			case SECTION_DATA:
				sym->Value += (DWORD)gdata_len;
				break;
			default:
				babort("Invalid section number!");
			}
			char *sym_name = NULL;
			if (sym->N.Name.Short == 0) {
				sym_name = &tctx->strs[sym->N.Name.Long - sizeof(u32)];
				sym->N.Name.Long += (DWORD)gstrs_len;
			} else {
				sym_name = (char *)&sym->N.ShortName[0];
			}
			if (sym->StorageClass == IMAGE_SYM_CLASS_LABEL) continue;

			const hash_t hash = strhash(make_str_from_c(sym_name));
			bassert(hmgeti(ctx->symbol_table, hash) == -1);
			struct symbol_table_entry entry = {
			    .key                = hash,
			    .symbol_table_index = (s32)(i + gsyms_len),
			};
			hmputs(ctx->symbol_table, entry);
		}

		// Copy symbol table to global one.
		arrsetcap(ctx->syms, SYMBOL_TABLE_SIZE);
		arrsetlen(ctx->syms, gsyms_len + syms_len);
		memcpy(&ctx->syms[gsyms_len], tctx->syms, syms_len * sizeof(IMAGE_SYMBOL));

		// Copy string table to global one.
		arrsetcap(ctx->strs, STRING_TABLE_SIZE);
		arrsetlen(ctx->strs, gstrs_len + strs_len);
		memcpy(&ctx->strs[gstrs_len], tctx->strs, strs_len);

		pthread_mutex_unlock(&ctx->mutex);
	}
	return_zone();
}

static void create_object_file(struct context *ctx) {
	usize text_section_pointer = IMAGE_SIZEOF_FILE_HEADER + IMAGE_SIZEOF_SECTION_HEADER * 2;

	// Code block is aligned to 16 bytes, do we need this???
	const usize section_data_padding = next_aligned2(text_section_pointer, 16) - text_section_pointer;
	text_section_pointer += section_data_padding;

	const usize text_reloc_pointer = text_section_pointer + arrlenu(ctx->code.bytes);

	const usize text_reloc_num = arrlenu(ctx->code.relocs);
	if (text_reloc_num > 0xFFFF) {
		babort("Text relocation table is too large to be stored for a single section in the COFF file!");
	}

	const usize data_reloc_num = arrlenu(ctx->data.relocs);
	if (data_reloc_num > 0xFFFF) {
		babort("Data relocation table is too large to be stored for a single section in the COFF file!");
	}

	const usize data_section_pointer   = text_reloc_pointer + sizeof(IMAGE_RELOCATION) * text_reloc_num;
	const usize data_reloc_pointer     = data_section_pointer + arrlenu(ctx->data.bytes);
	const usize symbol_section_pointer = data_reloc_pointer + sizeof(IMAGE_RELOCATION) * data_reloc_num;

	IMAGE_FILE_HEADER header = {
	    .Machine              = IMAGE_FILE_MACHINE_AMD64,
	    .NumberOfSections     = 2,
	    .PointerToSymbolTable = (DWORD)symbol_section_pointer,
	    .NumberOfSymbols      = (DWORD)arrlenu(ctx->syms),
	    .TimeDateStamp        = (DWORD)time(0),
	};

	IMAGE_SECTION_HEADER section_text = {
	    .Name                 = ".text",
	    .Characteristics      = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES | IMAGE_SCN_MEM_EXECUTE,
	    .SizeOfRawData        = (DWORD)arrlenu(ctx->code.bytes),
	    .PointerToRawData     = (DWORD)text_section_pointer,
	    .PointerToRelocations = (DWORD)text_reloc_pointer,
	    .NumberOfRelocations  = (u16)text_reloc_num,
	};

	IMAGE_SECTION_HEADER section_data = {
	    .Name                 = ".data",
	    .Characteristics      = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_ALIGN_16BYTES,
	    .SizeOfRawData        = (DWORD)arrlenu(ctx->data.bytes),
	    .PointerToRawData     = (DWORD)data_section_pointer,
	    .PointerToRelocations = (DWORD)data_reloc_pointer,
	    .NumberOfRelocations  = (u16)data_reloc_num,
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

	//
	// Headers
	//
	// Write COFF header
	fwrite(&header, 1, IMAGE_SIZEOF_FILE_HEADER, file);
	// Text section header
	fwrite(&section_text, 1, IMAGE_SIZEOF_SECTION_HEADER, file);
	fwrite(&section_data, 1, IMAGE_SIZEOF_SECTION_HEADER, file);

	// .text
	// Gap to align the code section.
	u8 padding[16] = {0};
	bassert(section_data_padding <= static_arrlenu(padding));
	fwrite(padding, 1, section_data_padding, file);
	fwrite(ctx->code.bytes, 1, arrlenu(ctx->code.bytes), file);
	fwrite(ctx->code.relocs, text_reloc_num, sizeof(IMAGE_RELOCATION), file);

	// .data
	fwrite(ctx->data.bytes, 1, arrlenu(ctx->data.bytes), file);
	fwrite(ctx->data.relocs, data_reloc_num, sizeof(IMAGE_RELOCATION), file);

	// Symbol table
	fwrite(ctx->syms, IMAGE_SIZEOF_SYMBOL, arrlenu(ctx->syms), file);

	// String table
	u32 strs_len = (u32)arrlenu(ctx->strs) + sizeof(u32); // See the COFF specifiction sec 5.6.
	fwrite(&strs_len, 1, sizeof(u32), file);
	fwrite(ctx->strs, 1, strs_len, file);

	fclose(file);
}

void x86_64run(struct assembly *assembly) {
	builder_warning("Using experimental x64 backend.");
	if (assembly->target->reg_split) {
		builder_error("Register split feature is not available when x64 backend is used. This feature is part of System V calling convetions.");
		return;
	}

	const u32 thread_count = get_thread_count();

	struct context ctx = {
	    .assembly = assembly,
	};

	pthread_mutex_init(&ctx.mutex, NULL);
	pthread_spin_init(&ctx.uq_name_lock, 0);
	pthread_spin_init(&ctx.schedule_for_generation_lock, 0);

	arrsetlen(ctx.tctx, thread_count);
	bl_zeromem(ctx.tctx, thread_count * sizeof(struct thread_context));

	MEMCPY_BUILTIN_HASH = add_global_external_sym(&ctx, MEMCPY_BUILTIN, DT_FUNCTION);
	MEMSET_BUILTIN_HASH = add_global_external_sym(&ctx, MEMSET_BUILTIN, DT_FUNCTION);

	// Submit top level instructions...
	for (usize i = 0; i < arrlenu(assembly->MIR.exported_instrs); ++i) {
		struct job_context job_ctx = {.x64 = {.ctx = &ctx, .top_instr = assembly->MIR.exported_instrs[i]}};
		submit_job(&job, &job_ctx);
	}
	wait_threads();

	for (usize i = 0; i < arrlenu(ctx.patches); ++i) {
		struct sym_patch *patch = &ctx.patches[i];
		bassert(patch->hash);
		bassert(patch->target_section);

		// Find the function position in the binary.
		const usize i = hmgeti(ctx.symbol_table, patch->hash);
		if (i == -1) {
			babort("Internally linked symbol reference is not found in the binary!");
		}

		const s32     symbol_table_index = ctx.symbol_table[i].symbol_table_index;
		IMAGE_SYMBOL *sym                = &ctx.syms[symbol_table_index];
		if (sym->SectionNumber != SECTION_TEXT || patch->target_section != sym->SectionNumber) {
			// @Performance: In case the symbol is from other section we probably have to rely on linker
			// doing relocations, in case of external symbol it's correct way to do it.

			IMAGE_RELOCATION reloc = {
			    .Type             = patch->type,
			    .SymbolTableIndex = symbol_table_index,
			    .VirtualAddress   = (DWORD)patch->position,
			};

			if (patch->target_section == SECTION_TEXT) {
				arrput(ctx.code.relocs, reloc);
			} else if (patch->target_section == SECTION_DATA) {
				arrput(ctx.data.relocs, reloc);
			}
		} else {
			// Fix the location.
			const s32 sym_position                  = (s32)sym->Value;
			const s32 position                      = (s32)patch->position;
			*data_ptr_s32(ctx.code.bytes, position) = sym_position - (position + sizeof(s32));
		}
	}

	// Write the output file.
	create_object_file(&ctx);

	// Cleanup
	if (builder.options->do_cleanup_when_done) {
		for (usize i = 0; i < arrlenu(ctx.tctx); ++i) {
			struct thread_context *tctx = &ctx.tctx[i];
			arrfree(tctx->text);
			arrfree(tctx->data);
			arrfree(tctx->syms);
			arrfree(tctx->strs);
			arrfree(tctx->values);
			arrfree(tctx->emit_block_queue);
			arrfree(tctx->local_patches);
			arrfree(tctx->patches);
		}

		arrfree(ctx.tctx);
		arrfree(ctx.strs);
		arrfree(ctx.code.bytes);
		arrfree(ctx.code.relocs);

		arrfree(ctx.data.bytes);
		arrfree(ctx.data.relocs);

		arrfree(ctx.syms);
		arrfree(ctx.patches);
		hmfree(ctx.symbol_table);
		hmfree(ctx.scheduled_for_generation);
	}

	pthread_spin_destroy(&ctx.schedule_for_generation_lock);
	pthread_spin_destroy(&ctx.uq_name_lock);
	pthread_mutex_destroy(&ctx.mutex);
}
