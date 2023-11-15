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

#define BYTE_CODE_BUFFER_SIZE 1024
#define SYMBOL_TABLE_SIZE 1024
#define STRING_TABLE_SIZE (1 * 1024 * 1024)      // 1MB
#define CODE_BLOCK_BUFFER_SIZE (2 * 1024 * 1024) // 2MB

struct thread_context {
	array(u8) bytes;
	array(IMAGE_SYMBOL) syms;
	array(char) strs;
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

//
// x86_64 encoding
//

#define REX_W 0b01001000

#define MOD_REG_ADDR 0b11

#define RAX 0
#define RCX 1
#define RDX 2
#define RBX 3
#define RSP 4
#define RBP 5
#define RSI 6
#define RDI 7

#define encode_mod_reg_rm(mod, reg, rm) (((mod) << 6) | (reg << 3) | (rm))

static inline void unique_name(struct context *ctx, str_buf_t *dest, const char *prefix, const str_t name) {
	pthread_spin_lock(&ctx->uq_name_lock);
	static u64 n = 0;
	str_buf_append_fmt(dest, "{s}{str}{u64}", prefix, name, n++);
	pthread_spin_unlock(&ctx->uq_name_lock);
}

static inline void add_code(struct thread_context *tctx, const void *buf, s32 len) {
	const usize i = arrlenu(tctx->bytes);
	arrsetcap(tctx->bytes, BYTE_CODE_BUFFER_SIZE);
	arrsetlen(tctx->bytes, i + len);
	memcpy(&tctx->bytes[i], buf, len);
}

// Add new symbol into thread local storage and set it's offset value relative to already generated
// code in the thread local bytes array. This value must be later fixed according to position in the
// final code section.
static inline void add_sym(struct thread_context *tctx, str_t linkage_name) {
	bassert(linkage_name.len && linkage_name.ptr);
	IMAGE_SYMBOL sym = {
	    .SectionNumber = 1,
	    .Type          = 0x20,
	    .StorageClass  = IMAGE_SYM_CLASS_EXTERNAL,
	    .Value         = (DWORD)arrlenu(tctx->bytes),
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
}

// 64 bit
static inline void push64_r(struct thread_context *tctx, u8 r) {
	const u8 buf[] = {REX_W, 0xFF, encode_mod_reg_rm(MOD_REG_ADDR, 0x6, r)};
	add_code(tctx, buf, 3);
}

static inline void pop64_r(struct thread_context *tctx, u8 r) {
	const u8 buf[] = {REX_W, 0x8F, encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r)};
	add_code(tctx, buf, 3);
}

static inline void mov64_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {REX_W, 0x89, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 3);
}

static inline void mov64_ri32(struct thread_context *tctx, u8 r, u32 imm) {
	const u8 buf[] = {REX_W, 0xC7, encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r)};
	add_code(tctx, buf, 3);
	add_code(tctx, &imm, sizeof(imm));
}

static inline void xor64_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {REX_W, 0x31, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 3);
}

static inline void sub64_ri8(struct thread_context *tctx, u8 r, u8 imm) {
	const u8 buf[] = {REX_W, 0x83, encode_mod_reg_rm(MOD_REG_ADDR, 0x5, r), imm};
	add_code(tctx, buf, 4);
}

// 32 bit
static inline void mov32_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {0x89, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 2);
}

static inline void xor32_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {0x31, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 2);
}

static inline void ret(struct thread_context *tctx) {
	const u8 buf[] = {0xC3};
	add_code(tctx, buf, 1);
}

//
// Translate
//

// @Cleanup: do we need ctx?
static void emit_instr(struct context *ctx, struct thread_context *tctx, struct mir_instr *instr);
static void emit_instr_fn_proto(struct context *ctx, struct thread_context *tctx, struct mir_instr_fn_proto *fn_proto);
static void emit_instr_block(struct context *ctx, struct thread_context *tctx, struct mir_instr_block *block);

void emit_instr_block(struct context *ctx, struct thread_context *tctx, struct mir_instr_block *block) {
	struct mir_fn *fn        = block->owner_fn;
	const bool     is_global = fn == NULL;
	if (!block->terminal) babort("Block '%s', is not terminated", block->name);

	if (!is_global) {
		add_sym(tctx, block->name);
		if (fn->first_block == block) {
			blog("Missing emit of allocas.");
			// emit_allocas(ctx, fn);
		}
	} else {
		babort("Missing implementation for global blocks!");
	}

	// Generate all instructions in the block.
	struct mir_instr *instr = block->entry_instr;
	while (instr) {
		emit_instr(ctx, tctx, instr);
		instr = instr->next;
	}
}

void emit_instr_fn_proto(struct context *ctx, struct thread_context *tctx, struct mir_instr_fn_proto *fn_proto) {
	struct mir_fn *fn = MIR_CEV_READ_AS(struct mir_fn *, &fn_proto->base.value);
	bmagic_assert(fn);

	str_t linkage_name = str_empty;
	if (isflag(fn->flags, FLAG_INTRINSIC)) {
		babort("Intrinsic are not implemented yet!");
	} else {
		linkage_name = fn->linkage_name;
	}
	bassert(linkage_name.len && "Invalid function name!");

	// External functions does not have any body block.
	if (isflag(fn->flags, FLAG_EXTERN) || isflag(fn->flags, FLAG_INTRINSIC)) {
		bassert("Handling of extern functions not implemented!");
		return;
	}

	add_sym(tctx, linkage_name);

	// Prologue
	push64_r(tctx, RBP);
	mov64_rr(tctx, RBP, RSP);

	// Generate all blocks in the function body.
	struct mir_instr *block = (struct mir_instr *)fn->first_block;
	while (block) {
		if (!block->is_unreachable) {
			emit_instr(ctx, tctx, block);
		}
		block = block->next;
	}

	// Epilogue
	pop64_r(tctx, RBP);
	ret(tctx);
}

void emit_instr(struct context *ctx, struct thread_context *tctx, struct mir_instr *instr) {
	bassert(instr->state == MIR_IS_COMPLETE && "Attempt to emit instruction in incomplete state!");
	// @Incomplete
	// if (!mir_type_has_llvm_representation((instr->value.type))) return state;
	switch (instr->kind) {
	case MIR_INSTR_FN_PROTO:
		emit_instr_fn_proto(ctx, tctx, (struct mir_instr_fn_proto *)instr);
		break;
	case MIR_INSTR_BLOCK:
		emit_instr_block(ctx, tctx, (struct mir_instr_block *)instr);
		break;
	default:
		blog("Missing implementation for emmiting '%s' instruction.", mir_instr_name(instr));
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

	struct mir_instr *top_instr = job_ctx->x64.top_instr;
	blog("Kind = %s", mir_instr_name(top_instr));

	emit_instr(ctx, tctx, top_instr);

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
	}
	arrfree(ctx.tctx);
	arrfree(ctx.code.bytes);
	arrfree(ctx.code.syms);
	arrfree(ctx.code.strs);
	pthread_mutex_destroy(&ctx.code.mutex);
}

/*
int main() {
struct context ctx;
init_ctx(&ctx);

mov64_rr(&ctx, RAX, RCX);
mov64_rr(&ctx, RBP, RSP);
mov32_rr(&ctx, RAX, RCX);
xor64_rr(&ctx, RAX, RAX);
sub64_ri8(&ctx, RSP, 0x20);
mov64_ri32(&ctx, RAX, 0x10);
ret(&ctx);

usize section_data_pointer = IMAGE_SIZEOF_FILE_HEADER + IMAGE_SIZEOF_SECTION_HEADER;
usize sym_pointer          = section_data_pointer + ctx.len;

IMAGE_FILE_HEADER header = {
.Machine              = IMAGE_FILE_MACHINE_AMD64,
.NumberOfSections     = 1,
.PointerToSymbolTable = sym_pointer,
.NumberOfSymbols      = 2,
.TimeDateStamp        = time(0),
};

IMAGE_SECTION_HEADER section = {
.Name             = ".text",
.Characteristics  = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_ALIGN_16BYTES | IMAGE_SCN_MEM_EXECUTE,
.SizeOfRawData    = ctx.len,
.PointerToRawData = section_data_pointer,
};

IMAGE_SYMBOL syms[2] = {
{
.N.ShortName   = "_start",
.SectionNumber = 1,
.Type          = 0x20,
.StorageClass  = IMAGE_SYM_CLASS_EXTERNAL,
},
{
.N.Name.Long  = 4, // Offset to the string table (contains table len).
.Type         = 0x20,
.StorageClass = IMAGE_SYM_CLASS_EXTERNAL,
},
};

usize file_ptr;
file_ptr = output&ctx, &header, IMAGE_SIZEOF_FILE_HEADER);
printf("Header:  0x%llx\n", file_ptr);

file_ptr = output(&ctx, &section, IMAGE_SIZEOF_SECTION_HEADER);
printf("Section: 0x%llx\n", file_ptr);

file_ptr = output(&ctx, ctx.code, ctx.len);
printf("Code:    0x%llx (0x%lx)\n", file_ptr, section.PointerToRawData);

file_ptr = output(&ctx, syms, IMAGE_SIZEOF_SYMBOL * 2);
printf("Syms:    0x%llx (0x%lx)\n", file_ptr, header.PointerToSymbolTable);

// Size of the string table
const u32 strs_size = 12 + sizeof(u32);
output(&ctx, &strs_size, sizeof(u32));

// Content of the string table (zero terminated!).
const char *strs = "ExitProcess";
output(&ctx, strs, 12);

terminate_ctx(&ctx);
printf("Output written!");
return 0;
}
*/
