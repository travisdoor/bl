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

#define MOD_REG_ADDR 0b11
#define MOD_BYTE_DISP 0b01
#define MOD_FOUR_BYTE_DISP 0b10

enum x64_register {
	RAX = 0,
	RCX = 1,
	RDX = 2,
	RBX = 3,
	RSP = 4,
	RBP = 5,
	RSI = 6,
	RDI = 7,
	R8  = 8,
	R9  = 9,
	R10 = 10,
	R11 = 11,
	R12 = 12,
	R13 = 13,
	R14 = 14,
	R15 = 15,
};

#define encode_mod_reg_rm(mod, reg, rm) (((mod) << 6) | ((reg & 0b111) << 3) | (rm & 0b111))
#define is_byte_disp(off) ((off) >= -128 && (off) < 128)

static inline s32 add_code(struct thread_context *tctx, const void *buf, s32 len);

static inline u8 encode_rex(u8 reg, u8 rm) {
	u8 mod = REX_W;
	if ((reg & 0b1000) > 0) {
		mod |= 0b100;
	}
	if ((rm & 0b1000) > 0) {
		mod |= 0b1;
	}
	return mod;
}

static inline u8 encode_rex2(bool is64, u8 reg, u8 rm) {
	u8 rex = is64 ? 0b1000 : 0;

	if ((reg & 0b1000) > 0) rex |= 0b100;
	if ((rm & 0b1000) > 0) rex |= 0b1;
	if (rex > 0) rex |= 0b01000000; // Prefix
	return rex;
}

static inline void encode_base(struct thread_context *tctx, u8 op_base, u8 mrr, u8 rex, usize size) {
	u8  buf[4];
	s32 i = 0;
	if (size == 2) buf[i++] = 0x66;
	if (rex) buf[i++] = rex;
	buf[i++] = size == 1 ? op_base : (op_base + 1);
	buf[i++] = mrr;
	add_code(tctx, buf, i);
}

static inline void nop(struct thread_context *tctx) {
	const u8 buf[] = {0x90};
	add_code(tctx, buf, 1);
}

static inline void push64_r(struct thread_context *tctx, u8 r) {
	const u8 buf[] = {encode_rex(0x6, r), 0xFF, encode_mod_reg_rm(MOD_REG_ADDR, 0x6, r)};
	add_code(tctx, buf, 3);
}

static inline void pop64_r(struct thread_context *tctx, u8 r) {
	const u8 buf[] = {encode_rex(0, r), 0x8F, encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r)};
	add_code(tctx, buf, 3);
}

// 64bit only
static inline void movabs64_ri(struct thread_context *tctx, u8 r, u64 imm) {
	const u8 buf[] = {encode_rex2(true, 0x0, r), 0xB8 | (r & 0b111)};
	add_code(tctx, buf, 2);
	add_code(tctx, &imm, sizeof(imm));
}

// Register to stack.
static inline void mov_mr(struct thread_context *tctx, u8 r1, s32 offset, u8 r2, usize size) {
	const u8 disp = is_byte_disp(offset) ? MOD_BYTE_DISP : MOD_FOUR_BYTE_DISP;
	const u8 mrr  = encode_mod_reg_rm(disp, r2, r1);
	const u8 rex  = encode_rex2(size == 8, r2, r1);
	encode_base(tctx, 0x88, mrr, rex, size);
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
	const u8 rex  = encode_rex2(size == 8, 0x0, r);
	encode_base(tctx, 0xC6, mrr, rex, size);
	add_code(tctx, &offset, disp == MOD_BYTE_DISP ? 1 : 4);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

// Immediate value to register.
static inline void mov_ri(struct thread_context *tctx, u8 r, u64 imm, usize size) {
	if (size == 8 && imm > 0xFFFFFFFF) {
		movabs64_ri(tctx, r, imm);
		return;
	}
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r);
	const u8 rex = encode_rex2(size == 8, 0x0, r);
	encode_base(tctx, 0xC6, mrr, rex, size);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

// Stack to register
static inline void mov_rm(struct thread_context *tctx, u8 r1, u8 r2, s32 offset, usize size) {
	const u8 disp = is_byte_disp(offset) ? MOD_BYTE_DISP : MOD_FOUR_BYTE_DISP;
	const u8 mrr  = encode_mod_reg_rm(disp, r1, r2);
	const u8 rex  = encode_rex2(size == 8, r1, r2);
	encode_base(tctx, 0x8A, mrr, rex, size);
	add_code(tctx, &offset, disp == MOD_BYTE_DISP ? 1 : 4);
}

static inline void mov_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size) {
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r2, r1);
	const u8 rex = encode_rex2(size == 8, r2, r1);
	encode_base(tctx, 0x88, mrr, rex, size);
}

static inline void add_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size) {
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r2, r1);
	const u8 rex = encode_rex2(size == 8, r2, r1);
	encode_base(tctx, 0x0, mrr, rex, size);
}

static inline void add_ri(struct thread_context *tctx, u8 r, u64 imm, usize size) {
	if (size == 8 && imm > 0xFFFFFFFF) {
		movabs64_ri(tctx, RAX, imm);
		add_rr(tctx, r, RAX, size);
		return;
	}
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r);
	const u8 rex = encode_rex2(size == 8, 0x0, r);
	encode_base(tctx, 0x80, mrr, rex, size);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

static inline void sub_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size) {
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r2, r1);
	const u8 rex = encode_rex2(size == 8, r2, r1);
	encode_base(tctx, 0x28, mrr, rex, size);
}

static inline void sub_ri(struct thread_context *tctx, u8 r, u64 imm, usize size) {
	if (size == 8 && imm > 0xFFFFFFFF) {
		mov_ri(tctx, RAX, imm, size);
		sub_rr(tctx, r, RAX, size);
		return;
	}
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, 0x5, r);
	const u8 rex = encode_rex2(size == 8, 0x0, r);
	encode_base(tctx, 0x80, mrr, rex, size);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

static inline void cmp_rr(struct thread_context *tctx, u8 r1, u8 r2, usize size) {
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, r2, r1);
	const u8 rex = encode_rex2(size == 8, r2, r1);
	encode_base(tctx, 0x38, mrr, rex, size);
}

// Use RAX for 64bit immediate value.
static inline void cmp_ri(struct thread_context *tctx, u8 r, u64 imm, usize size) {
	if (size == 8 && imm > 0xFFFFFFFF) {
		mov_ri(tctx, RAX, imm, size);
		cmp_rr(tctx, r, RAX, size);
		return;
	}
	const u8 mrr = encode_mod_reg_rm(MOD_REG_ADDR, 0x7, r);
	const u8 rex = encode_rex2(size == 8, 0x7, r);
	encode_base(tctx, 0x80, mrr, rex, size);
	add_code(tctx, &imm, (s32)MIN(size, sizeof(u32)));
}

static inline void ret(struct thread_context *tctx) {
	const u8 buf[] = {0xC3};
	add_code(tctx, buf, 1);
}

static inline u32 jmp_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0xE9};
	add_code(tctx, buf, 1);
	return add_code(tctx, &offset, sizeof(offset));
}

// Returns position of 32bit offset value in the current code section. This might be used
// for postponed fixup in case target block is not generated yet.
static inline u32 jne_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x85};
	add_code(tctx, buf, 2);
	return add_code(tctx, &offset, sizeof(offset));
}

static inline u32 je_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x84};
	add_code(tctx, buf, 2);
	return add_code(tctx, &offset, sizeof(offset));
}

static inline u32 jg_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x8F};
	add_code(tctx, buf, 2);
	return add_code(tctx, &offset, sizeof(offset));
}

static inline u32 jl_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x8C};
	add_code(tctx, buf, 2);
	return add_code(tctx, &offset, sizeof(offset));
}

static inline u32 jge_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x8D};
	add_code(tctx, buf, 2);
	return add_code(tctx, &offset, sizeof(offset));
}

static inline u32 jle_relative_i32(struct thread_context *tctx, s32 offset) {
	const u8 buf[] = {0x0F, 0x8E};
	add_code(tctx, buf, 2);
	return add_code(tctx, &offset, sizeof(offset));
}
