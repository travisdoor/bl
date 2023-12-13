// =================================================================================================
// bl
//
// File:   x86_64_instructions.h
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

#include "common.h"

struct thread_context;

#define REX 0b01000000
#define REX_W 0b01001000

#define MOD_INDIRECT 0
#define MOD_BYTE_DISP 1
#define MOD_FOUR_BYTE_DISP 2
#define MOD_REG_ADDR 3

enum x64_register {
	INVALID_REGISTER = -1,

	RAX = 0, // Volatile
	RCX = 1, // Volatile
	RDX = 2, // Volatile
	RBX = 3,
	RSP = 4,
	RBP = 5,
	RSI = 6,
	RDI = 7,
	R8  = 8,  // Volatile
	R9  = 9,  // Volatile
	R10 = 10, // Volatile
	R11 = 11, // Volatile
	R12 = 12,
	R13 = 13,
	R14 = 14,
	R15 = 15,

	REGISTER_COUNT,
};

#define encode_mod_reg_rm(mod, reg, rm) (((mod) << 6) | ((reg & 0b111) << 3) | (rm & 0b111))
#define encode_sib(scale, index, base) (((scale) << 6) | ((index & 0b111) << 3) | (base & 0b111))
#define is_byte_disp(off) ((off) >= -128 && (off) < 128)

static inline void add_code(struct thread_context *tctx, const void *buf, s32 len);

// Encode the instruction extension byte for 64bit mode.
static inline u8 encode_rex(bool is64, u8 reg, u8 rm) {
	u8 rex = is64 ? 0b1000 : 0;

	if ((reg & 0b1000) > 0) rex |= 0b100;
	if ((rm & 0b1000) > 0) rex |= 0b1;
	if (rex > 0) rex |= 0b01000000; // Prefix
	return rex;
}

static inline void encode_base(struct thread_context *tctx, u8 rex, u8 op_base, u8 mrr, usize size) {
	u8  buf[4];
	s32 i = 0;
	if (size == 2) buf[i++] = 0x66;
	if (rex) buf[i++] = rex;
	buf[i++] = size == 1 ? op_base : (op_base + 1);
	buf[i++] = mrr;
	add_code(tctx, buf, i);
}

static inline void xor_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size);

static inline void zero_reg(struct thread_context *tctx, u8 r) {
	xor_rr(tctx, r, r, 8);
}

static inline void nop(struct thread_context *tctx) {
	const u8 buf[] = {0x90};
	add_code(tctx, buf, 1);
}

static inline void push64_r(struct thread_context *tctx, u8 r) {
	const u8 buf[] = {encode_rex(true, 0x6, r), 0xFF, encode_mod_reg_rm(MOD_REG_ADDR, 0x6, r)};
	add_code(tctx, buf, 3);
}

static inline void pop64_r(struct thread_context *tctx, u8 r) {
	const u8 buf[] = {encode_rex(true, 0, r), 0x8F, encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r)};
	add_code(tctx, buf, 3);
}

static inline void lea_rm(struct thread_context *tctx, u8 r1, u8 r2, s32 offset, usize size) {
	bassert(size > 1 && "Invalid size for lea instruction.");
	const u8 disp = is_byte_disp(offset) ? MOD_BYTE_DISP : MOD_FOUR_BYTE_DISP;
	const u8 rex  = encode_rex(size == 8, r1, r2);
	const u8 mrr  = encode_mod_reg_rm(disp, r1, r2);

	u8  buf[4];
	s32 i = 0;
	if (size == 2) buf[i++] = 0x66;
	if (rex) buf[i++] = rex;
	buf[i++] = 0x8D;
	buf[i++] = mrr;
	add_code(tctx, buf, i);

	add_code(tctx, &offset, disp == MOD_BYTE_DISP ? 1 : 4);
}

static inline void lea_rm_indirect(struct thread_context *tctx, u8 r1, s32 offset, usize size) {
	bassert(size > 1 && "Invalid size for lea instruction.");
	const u8 r2 = RBP;

	const u8 rex  = encode_rex(size == 8, r1, r2);
	const u8 mrr  = encode_mod_reg_rm(MOD_INDIRECT, r1, r2);

	u8  buf[4];
	s32 i = 0;
	if (size == 2) buf[i++] = 0x66;
	if (rex) buf[i++] = rex;
	buf[i++] = 0x8D;
	buf[i++] = mrr;
	add_code(tctx, buf, i);

	add_code(tctx, &offset, 4);
}

// 64bit only
static inline void movabs64_ri(struct thread_context *tctx, u8 r, u64 imm) {
	const u8 buf[] = {encode_rex(true, 0x0, r), 0xB8 | (r & 0b111)};
	add_code(tctx, buf, 2);
	add_code(tctx, &imm, sizeof(imm));
}

static inline void mov_mr(struct thread_context *tctx, u8 r1, s32 offset, u8 r2, usize size) {
	const u8 disp = is_byte_disp(offset) ? MOD_BYTE_DISP : MOD_FOUR_BYTE_DISP;
	const u8 mrr  = encode_mod_reg_rm(disp, r2, r1);
	const u8 rex  = encode_rex(size == 8, r2, r1);
	encode_base(tctx, rex, 0x88, mrr, size);
	if (r1 == RSP) {
		// @Incomplete: Might be broken???
		const u8 sib = encode_sib(0x0, r1, r1);
		add_code(tctx, &sib, 1);
	}
	add_code(tctx, &offset, disp == MOD_BYTE_DISP ? 1 : 4);
}

// Immediate to stack. Use RAX for 64bit immediate value.
static inline void mov_mi(struct thread_context *tctx, u8 r, s32 offset, u64 imm, usize size) {
	if (size == 8 && imm > 0xFFFFFFFF) {
		// Special case... because why not...
		movabs64_ri(tctx, RAX, imm);
		mov_mr(tctx, r, offset, RAX, size);
		return;
	}

	const u8 disp = is_byte_disp(offset) ? MOD_BYTE_DISP : MOD_FOUR_BYTE_DISP;
	const u8 mrr  = encode_mod_reg_rm(disp, 0x0, r);
	const u8 rex  = encode_rex(size == 8, 0x0, r);
	encode_base(tctx, rex, 0xC6, mrr, size);
	if (r == RSP) {
		// @Incomplete: Might be broken???
		const u8 sib = encode_sib(0x0, r, r);
		add_code(tctx, &sib, 1);
	}
	add_code(tctx, &offset, disp == MOD_BYTE_DISP ? 1 : 4);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

static inline void mov_mi_indirect(struct thread_context *tctx, s32 offset, u64 imm, usize size) {
	const u8 r = RBP;
	if (size == 8 && imm > 0xFFFFFFFF) {
		// @Incomplete: Might not work for indirect addressing?
		bassert(false);
		// Special case... because why not...
		movabs64_ri(tctx, RAX, imm);
		mov_mr(tctx, r, offset, RAX, size);
		return;
	}

	const u8 mrr = encode_mod_reg_rm(MOD_INDIRECT, 0x0, r);
	const u8 rex = encode_rex(size == 8, 0x0, r);
	encode_base(tctx, rex, 0xC6, mrr, size);
	add_code(tctx, &offset, 4);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

// Immediate value to register.
static inline void mov_ri(struct thread_context *tctx, u8 r, u64 imm, usize size) {
	// @Performance: xor for zero values???
	// @Performance: xor for zero values???
	// @Performance: xor for zero values???

	if (size == 8 && imm > 0xFFFFFFFF) {
		movabs64_ri(tctx, r, imm);
	}
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r);
	const u8 rex = encode_rex(size == 8, 0x0, r);
	encode_base(tctx, rex, 0xC6, mrr, size);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

// Stack to register
static inline void mov_rm(struct thread_context *tctx, u8 r1, u8 r2, s32 offset, usize size) {
	const u8 disp = is_byte_disp(offset) ? MOD_BYTE_DISP : MOD_FOUR_BYTE_DISP;
	const u8 rex  = encode_rex(size == 8, r1, r2);
	const u8 mrr  = encode_mod_reg_rm(disp, r1, r2);
	encode_base(tctx, rex, 0x8A, mrr, size);
	add_code(tctx, &offset, disp == MOD_BYTE_DISP ? 1 : 4);
}

static inline void mov_rm_indirect(struct thread_context *tctx, u8 r1, s32 offset, usize size) {
	const u8 r2  = RBP;
	const u8 rex = encode_rex(size == 8, r1, r2);
	const u8 mrr = encode_mod_reg_rm(MOD_INDIRECT, r1, r2);
	encode_base(tctx, rex, 0x8A, mrr, size);
	add_code(tctx, &offset, 4);
}

static inline void mov_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size) {
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r2, r1);
	const u8 rex = encode_rex(size == 8, r2, r1);
	encode_base(tctx, rex, 0x88, mrr, size);
}

inline void xor_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size) {
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r2, r1);
	const u8 rex = encode_rex(size == 8, r2, r1);
	encode_base(tctx, rex, 0x30, mrr, size);
}

static inline void add_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size) {
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r2, r1);
	const u8 rex = encode_rex(size == 8, r2, r1);
	encode_base(tctx, rex, 0x0, mrr, size);
}

static inline void add_ri(struct thread_context *tctx, u8 r, u64 imm, usize size) {
	if (size == 8 && imm > 0xFFFFFFFF) {
		movabs64_ri(tctx, RAX, imm);
		add_rr(tctx, r, RAX, size);
		return;
	}
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r);
	const u8 rex = encode_rex(size == 8, 0x0, r);
	encode_base(tctx, rex, 0x80, mrr, size);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

static inline void sub_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size) {
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r2, r1);
	const u8 rex = encode_rex(size == 8, r2, r1);
	encode_base(tctx, rex, 0x28, mrr, size);
}

static inline void sub_ri(struct thread_context *tctx, u8 r, u64 imm, usize size) {
	if (size == 8 && imm > 0xFFFFFFFF) {
		mov_ri(tctx, RAX, imm, size);
		sub_rr(tctx, r, RAX, size);
	}
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, 0x5, r);
	const u8 rex = encode_rex(size == 8, 0x0, r);
	encode_base(tctx, rex, 0x80, mrr, size);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

static inline void imul_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size) {
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r1, r2);
	const u8 rex = encode_rex(size == 8, r2, r1);

	u8  buf[5];
	s32 i = 0;
	if (size == 1 || size == 2) buf[i++] = 0x66;
	if (rex) buf[i++] = rex;
	buf[i++] = 0x0F;
	buf[i++] = 0xAF;
	buf[i++] = mrr;
	add_code(tctx, buf, i);
}

static inline void imul_ri(struct thread_context *tctx, u8 r1, u8 r2, u64 imm, usize size) {
	if (size == 8 && imm > 0xFFFFFFFF) {
		movabs64_ri(tctx, RAX, imm);
		imul_rr(tctx, r1, RAX, size);
		return;
	}
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r2, r1);
	const u8 rex = encode_rex(size == 8, r2, r1);

	u8  buf[4];
	s32 i = 0;
	if (rex) buf[i++] = rex;
	switch (size) {
	case 1:
		buf[i++] = 0x6B;
		break;
	case 2:
	case 4:
	case 8:
		buf[i++] = 0x69;
		break;
	}
	buf[i++] = mrr;
	add_code(tctx, buf, i);
	// Note we never get over 4 bytes imm value.
	add_code(tctx, &imm, (s32)(size == 1 ? sizeof(u8) : sizeof(u32)));
}

static inline void cmp_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size) {
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r2, r1);
	const u8 rex = encode_rex(size == 8, r2, r1);
	encode_base(tctx, rex, 0x38, mrr, size);
}

// Use RAX for 64bit immediate value.
static inline void cmp_ri(struct thread_context *tctx, u8 r, u64 imm, usize size) {
	if (size == 8 && imm > 0xFFFFFFFF) {
		mov_ri(tctx, RAX, imm, size);
		cmp_rr(tctx, r, RAX, size);
		return;
	}
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, 0x7, r);
	const u8 rex = encode_rex(size == 8, 0x7, r);
	encode_base(tctx, rex, 0x80, mrr, size);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

static inline void ret(struct thread_context *tctx) {
	const u8 buf[] = {0xC3};
	add_code(tctx, buf, 1);
}

static inline void jmp_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0xE9};
	add_code(tctx, buf, 1);
	add_code(tctx, &offset, sizeof(offset));
}

// Returns position of 32bit offset value in the current code section. This might be used
// for postponed fixup in case target block is not generated yet.
static inline void jne_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x85};
	add_code(tctx, buf, 2);
	add_code(tctx, &offset, sizeof(offset));
}

static inline void je_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x84};
	add_code(tctx, buf, 2);
	add_code(tctx, &offset, sizeof(offset));
}

static inline void jg_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x8F};
	add_code(tctx, buf, 2);
	add_code(tctx, &offset, sizeof(offset));
}

static inline void jl_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x8C};
	add_code(tctx, buf, 2);
	add_code(tctx, &offset, sizeof(offset));
}

static inline void jge_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x8D};
	add_code(tctx, buf, 2);
	add_code(tctx, &offset, sizeof(offset));
}

static inline void jle_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x8E};
	add_code(tctx, buf, 2);
	add_code(tctx, &offset, sizeof(offset));
}

static inline void call_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0xE8};
	add_code(tctx, buf, 1);
	add_code(tctx, &offset, sizeof(offset));
}
