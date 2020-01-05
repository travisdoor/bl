//************************************************************************************************
// bl
//
// File:   common.h
// Author: Martin Dorazil
// Date:   03/03/2018
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

#ifndef BL_COMMON_H
#define BL_COMMON_H

#include "bldebug.h"
#include "blmemory.h"
#include "config.h"
#include "error.h"
#include "messages.h"
#include <limits.h>
#include <stddef.h>
#include <tlib/tlib.h>

struct Assembly;

#if defined(BL_COMPILER_CLANG) || defined(BL_COMPILER_GNUC)
#define BL_DEPRECATED __attribute__((deprecated))
#else
#define BL_DEPRECATED
#endif

#define IS_FLAG(_v, _flag) ((bool)((_v & _flag) == _flag))
#define IS_NOT_FLAG(_v, _flag) ((bool)((_v & _flag) != _flag))

#define ARRAY_FOREACH(arr, it)                                                                     \
	for (usize _keep = 1, i = 0, _size = ARRAY_SIZE((arr)); _keep && i != _size;               \
	     _keep = !_keep, i++)                                                                  \
		for (it = (arr)[i]; _keep; _keep = !_keep)

extern u64 main_thread_id;

TSMALL_ARRAY_TYPE(AstPtr, struct Ast *, 16);
TSMALL_ARRAY_TYPE(TypePtr, struct MirType *, 16);
TSMALL_ARRAY_TYPE(MemberPtr, struct MirMember *, 16);
TSMALL_ARRAY_TYPE(VariantPtr, struct MirVariant *, 16);
TSMALL_ARRAY_TYPE(ArgPtr, struct MirArg *, 16);
TSMALL_ARRAY_TYPE(InstrPtr, struct MirInstr *, 16);
TSMALL_ARRAY_TYPE(ConstValuePtr, struct MirConstValue *, 16);
TSMALL_ARRAY_TYPE(Char, char, 128);

typedef struct ID {
	const char *str;
	u64         hash;
} ID;

void
id_init(ID *id, const char *str);

/*
 * Replace all backslashes in passed path with forward slash, this is used as workaround on Windows
 * platform due to inconsistency 'Unix vs Windows' path separators. This function will modify passed
 * buffer.
 */
void
win_fix_path(char *buf, usize buf_size);

bool
file_exists(const char *filepath);

const char *
brealpath(const char *file, char *out, s32 out_len);

bool
get_dir_from_filepath(char *buf, const usize l, const char *filepath);

bool
get_filename_from_filepath(char *buf, const usize l, const char *filepath);

bool
get_current_exec_path(char *buf, usize buf_size);

bool
get_current_exec_dir(char *buf, usize buf_size);

void
date_time(char *buf, s32 len, const char *format);

bool
is_aligned(const void *p, usize alignment);

void
align_ptr_up(void **p, usize alignment, ptrdiff_t *adjustment);

void
print_bits(s32 const size, void const *const ptr);

int
count_bits(u64 n);

void
platform_lib_name(const char *name, char *buffer, usize max_len);

/*
 * Creates BArray inside Assembly arena.
 * Note: no free is needed.
 */
TArray *
create_arr(struct Assembly *assembly, usize size);

/*
 * Creates SmallArray inside Assembly arena.
 * Note: no free is needed.
 */
void *
_create_sarr(struct Assembly *cnt, usize arr_size);

u32
next_pow_2(u32 n);

#define create_sarr(T, Asm) ((T *)_create_sarr((Asm), sizeof(T)))

#endif
