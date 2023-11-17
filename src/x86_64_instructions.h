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

#define RAX 0
#define RCX 1
#define RDX 2
#define RBX 3
#define RSP 4
#define RBP 5
#define RSI 6
#define RDI 7

#define EAX 0
#define ECX 1
#define EDX 2
#define EBX 3
#define ESP 4
#define EBP 5
#define ESI 6
#define EDI 7

#define encode_mod_reg_rm(mod, reg, rm) (((mod) << 6) | (reg << 3) | (rm))
#define is_byte_disp(off) ((off) >= -128 && (off) < 128)

static inline s32 add_code(struct thread_context *tctx, const void *buf, s32 len);

static inline void nop(struct thread_context *tctx) {
	const u8 buf[] = {0x90};
	add_code(tctx, buf, 1);
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

static inline void sub64_ri32(struct thread_context *tctx, u8 r, u32 imm) {
	const u8 buf[] = {REX_W, 0x81, encode_mod_reg_rm(MOD_REG_ADDR, 0x5, r)};
	add_code(tctx, buf, 3);
	add_code(tctx, &imm, sizeof(imm));
}

static inline void add64_ri32(struct thread_context *tctx, u8 r, u32 imm) {
	const u8 buf[] = {REX_W, 0x81, encode_mod_reg_rm(MOD_REG_ADDR, 0x0, r)};
	add_code(tctx, buf, 3);
	add_code(tctx, &imm, sizeof(imm));
}

// 32 bit
static inline void mov32_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {0x89, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 2);
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

static inline void mov32_rm32(struct thread_context *tctx, u8 r1, u8 r2, u8 offset) {
	const u8 buf[] = {0x8B, encode_mod_reg_rm(MOD_BYTE_DISP, r1, r2), offset};
	add_code(tctx, buf, 3);
}

static inline void mov32_m32r(struct thread_context *tctx, u8 r1, u8 offset, u8 r2) {
	const u8 buf[] = {0x89, encode_mod_reg_rm(MOD_BYTE_DISP, r2, r1), offset};
	add_code(tctx, buf, 3);
}

static inline void xor32_rr(struct thread_context *tctx, u8 r1, u8 r2) {
	const u8 buf[] = {0x31, encode_mod_reg_rm(MOD_REG_ADDR, r2, r1)};
	add_code(tctx, buf, 2);
}

static inline void ret(struct thread_context *tctx) {
	const u8 buf[] = {0xC3};
	add_code(tctx, buf, 1);
}

static inline void jmp_relative_i32(struct thread_context *tctx, s32 offset) {
	if (offset != 0)
		offset = _byteswap_ulong(offset + sizeof(u8) + sizeof(u32));

	const u8 buf[] = {0xE9};
	add_code(tctx, buf, 1);
	add_code(tctx, &offset, sizeof(offset));
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
		mov8_mi8(tctx, RBP, (u8)offset, (u8)v);
		break;
	case 2:
		mov16_mi16(tctx, RBP, (u8)offset, (u16)v);
		break;
	case 4:
		mov32_mi32(tctx, RBP, (u8)offset, (u32)v);
		break;
	case 8:
		BL_UNIMPLEMENTED;
		break;
	default:
		BL_UNIMPLEMENTED;
	}
}
