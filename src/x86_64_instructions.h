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

static inline void nop(struct thread_context *tctx) {
	const u8 buf[] = {0x90};
	add_code(tctx, buf, 1);
}

// 64 bit
static inline void push64_r(struct thread_context *tctx, u8 r) {
	const u8 buf[] = {encode_rex(0x6, r), 0xFF, encode_mod_reg_rm(MOD_REG_ADDR, 0x6, r)};
	add_code(tctx, buf, 3);
}

static inline void pop64_r(struct thread_context *tctx, u8 r) {
	const u8 buf[] = {encode_rex(0, r), 0x8F, encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r)};
	add_code(tctx, buf, 3);
}

static inline void mov64_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {encode_rex(r2, r1), 0x89, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 3);
}

static inline void mov64_ri32(struct thread_context *tctx, u8 r, u32 imm) {
	const u8 buf[] = {encode_rex(0, r), 0xC7, encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r)};
	add_code(tctx, buf, 3);
	add_code(tctx, &imm, sizeof(imm));
}

static inline void mov64_ri64(struct thread_context *tctx, u8 r, u64 imm) {
	const u8 buf[] = {encode_rex(0, r), 0xB8, encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r)};
	add_code(tctx, buf, 3);
	add_code(tctx, &imm, sizeof(imm));
}

static inline void mov64_mr(struct thread_context *tctx, u8 r1, s32 offset, u8 r2) {
	if (is_byte_disp(offset)) {
		const u8 buf[] = {encode_rex(r2, r1), 0x89, encode_mod_reg_rm(MOD_BYTE_DISP, r2, r1), offset};
		add_code(tctx, buf, 4);
	} else {
		const u8 buf[] = {encode_rex(r2, r1), 0x89, encode_mod_reg_rm(MOD_FOUR_BYTE_DISP, r2, r1), offset};
		add_code(tctx, buf, 3);
		add_code(tctx, &offset, sizeof(offset));
	}
}

static inline void movabs64_rax_i64(struct thread_context *tctx, u64 imm) {
	const u8 buf[] = {encode_rex(0, 0), 0xB8};
	add_code(tctx, buf, 2);
	add_code(tctx, &imm, sizeof(imm));
}

static inline void xor64_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {encode_rex(r2, r1), 0x31, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 3);
}

static inline void sub64_ri8(struct thread_context *tctx, u8 r, u8 imm) {
	const u8 buf[] = {encode_rex(0x5, r), 0x83, encode_mod_reg_rm(MOD_REG_ADDR, 0x5, r), imm};
	add_code(tctx, buf, 4);
}

static inline void sub64_ri32(struct thread_context *tctx, u8 r, u32 imm) {
	const u8 buf[] = {encode_rex(0x5, r), 0x81, encode_mod_reg_rm(MOD_REG_ADDR, 0x5, r)};
	add_code(tctx, buf, 3);
	add_code(tctx, &imm, sizeof(imm));
}

static inline void add64_ri32(struct thread_context *tctx, u8 r, u32 imm) {
	const u8 buf[] = {encode_rex(0, r), 0x81, encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r)};
	add_code(tctx, buf, 3);
	add_code(tctx, &imm, sizeof(imm));
}

// 32 bit
static inline void mov32_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {0x89, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 2);
}

static inline void mov32_ri32(struct thread_context *tctx, u8 r1, u32 imm) {
	const u8 buf[] = {0xB9 | (r1 & 0b111)};
	add_code(tctx, buf, 1);
	add_code(tctx, &imm, sizeof(imm));
}

static inline void mov32_mi32(struct thread_context *tctx, u8 r, s32 offset, u32 imm) {
	if (is_byte_disp(offset)) {
		const u8 buf[] = {0xC7, encode_mod_reg_rm(MOD_BYTE_DISP, 0x0, r), offset};
		add_code(tctx, buf, 3);
	} else {
		const u8 buf[] = {0xC7, encode_mod_reg_rm(MOD_FOUR_BYTE_DISP, 0x0, r)};
		add_code(tctx, buf, 2);
		add_code(tctx, &offset, sizeof(offset));
	}
	add_code(tctx, &imm, sizeof(imm));
}

static inline void mov32_rm(struct thread_context *tctx, u8 r1, u8 r2, s32 offset) {
	if (is_byte_disp(offset)) {
		const u8 buf[] = {0x8B, encode_mod_reg_rm(MOD_BYTE_DISP, r1, r2), offset};
		add_code(tctx, buf, 3);
	} else {
		const u8 buf[] = {0x8B, encode_mod_reg_rm(MOD_FOUR_BYTE_DISP, r1, r2), offset};
		add_code(tctx, buf, 3);
		add_code(tctx, &offset, sizeof(offset));
	}
}

static inline void mov32_mr(struct thread_context *tctx, u8 r1, u8 offset, u8 r2) {
	const u8 buf[] = {0x89, encode_mod_reg_rm(MOD_BYTE_DISP, r2, r1), offset};
	add_code(tctx, buf, 3);
}

static inline void xor32_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {0x31, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 2);
}

static inline void add32_ri32(struct thread_context *tctx, u8 r, u32 imm) {
	const u8 buf[] = {0x81, encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r)};
	add_code(tctx, buf, 2);
	add_code(tctx, &imm, sizeof(imm));
}

static inline void add32_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {0x1, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 2);
}

static inline void sub32_ri32(struct thread_context *tctx, u8 r, u32 imm) {
	const u8 buf[] = {0x81, encode_mod_reg_rm(MOD_REG_ADDR, 0x5, r)};
	add_code(tctx, buf, 2);
	add_code(tctx, &imm, sizeof(imm));
}

static inline void cmp32_ri32(struct thread_context *tctx, u8 r, u32 imm) {
	const u8 buf[] = {0x81, encode_mod_reg_rm(MOD_REG_ADDR, 0x7, r)};
	add_code(tctx, buf, 2);
	add_code(tctx, &imm, sizeof(imm));
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

// 16 bit
static inline void mov16_mi16(struct thread_context *tctx, u8 r, s32 offset, u16 imm) {
	if (is_byte_disp(offset)) {
		const u8 buf[] = {0x66, 0xC7, encode_mod_reg_rm(MOD_BYTE_DISP, 0x0, r), offset};
		add_code(tctx, buf, 4);
	} else {
		const u8 buf[] = {0x66, 0xC7, encode_mod_reg_rm(MOD_FOUR_BYTE_DISP, 0x0, r)};
		add_code(tctx, buf, 3);
		add_code(tctx, &offset, sizeof(offset));
	}
	add_code(tctx, &imm, sizeof(imm));
}

// 8 bit
static inline void mov8_mi8(struct thread_context *tctx, u8 r, s32 offset, u8 imm) {
	if (is_byte_disp(offset)) {
		const u8 buf[] = {0xC6, encode_mod_reg_rm(MOD_BYTE_DISP, 0x0, r), offset, imm};
		add_code(tctx, buf, 4);
	} else {
		const u8 buf[] = {0xC6, encode_mod_reg_rm(MOD_FOUR_BYTE_DISP, 0x0, r)};
		add_code(tctx, buf, 2);
		add_code(tctx, &offset, sizeof(offset));
		add_code(tctx, &imm, sizeof(imm));
	}
}

static inline void jmp_relative_i8(struct thread_context *tctx, s8 offset) {
	const u8 buf[] = {0xEB, (u8)offset};
	add_code(tctx, buf, 2);
}

// Helpers

// Move immediate value to stack memory on RBP+offset.
static inline void mov_rbp_offset_immediate(struct thread_context *tctx, const s32 offset, const u64 v, const usize vsize) {
	switch (vsize) {
	case 1:
		mov8_mi8(tctx, RBP, offset, (u8)v);
		break;
	case 2:
		mov16_mi16(tctx, RBP, offset, (u16)v);
		break;
	case 4:
		mov32_mi32(tctx, RBP, offset, (u32)v);
		break;
	case 8:
		movabs64_rax_i64(tctx, v);
		mov64_mr(tctx, RBP, offset, RAX);
		break;
	default:
		BL_UNIMPLEMENTED;
	}
}

static inline void mov_r_rbp_offset(struct thread_context *tctx, const u8 r, const s32 offset, const usize vsize) {
	switch (vsize) {
	case 1:
		BL_UNIMPLEMENTED;
		break;
	case 2:
		BL_UNIMPLEMENTED;
		break;
	case 4:
		mov32_rm(tctx, r, RBP, offset);
		break;
	case 8:
		BL_UNIMPLEMENTED;
		break;
	default:
		BL_UNIMPLEMENTED;
	}
}

static inline void add_r_immediate(struct thread_context *tctx, const u8 reg, const u64 v, const usize vsize) {
	switch (vsize) {
	case 1:
		BL_UNIMPLEMENTED;
		break;
	case 2:
		BL_UNIMPLEMENTED;
		break;
	case 4:
		add32_ri32(tctx, reg, (u32)v);
		break;
	case 8:
		BL_UNIMPLEMENTED;
		break;
	default:
		BL_UNIMPLEMENTED;
	}
}

static inline void add_rr(struct thread_context *tctx, const u8 reg1, const u8 reg2, const usize vsize) {
	switch (vsize) {
	case 1:
		BL_UNIMPLEMENTED;
		break;
	case 2:
		BL_UNIMPLEMENTED;
		break;
	case 4:
		add32_rr(tctx, reg1, reg2);
		break;
	case 8:
		BL_UNIMPLEMENTED;
		break;
	default:
		BL_UNIMPLEMENTED;
	}
}

static inline void sub_r_immediate(struct thread_context *tctx, const u8 reg, const u64 v, const usize vsize) {
	switch (vsize) {
	case 1:
		BL_UNIMPLEMENTED;
		break;
	case 2:
		BL_UNIMPLEMENTED;
		break;
	case 4:
		sub32_ri32(tctx, reg, (u32)v);
		break;
	case 8:
		BL_UNIMPLEMENTED;
		break;
	default:
		BL_UNIMPLEMENTED;
	}
}

static inline void cmp_r_immediate(struct thread_context *tctx, const u8 reg, const u64 v, const usize vsize) {
	switch (vsize) {
	case 1:
		BL_UNIMPLEMENTED;
		break;
	case 2:
		BL_UNIMPLEMENTED;
		break;
	case 4:
		cmp32_ri32(tctx, reg, (u32)v);
		break;
	case 8:
		BL_UNIMPLEMENTED;
		break;
	default:
		BL_UNIMPLEMENTED;
	}
}