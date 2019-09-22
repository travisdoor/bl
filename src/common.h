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
#include "small_array.h"
#include <bobject/containers/array.h>
#include <limits.h>

struct Assembly;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t   s8;
typedef int16_t  s16;
typedef int32_t  s32;
typedef int64_t  s64;
typedef float    f32;
typedef double   f64;

#if defined(BL_COMPILER_CLANG) || defined(BL_COMPILER_GNUC)
#define DEPRECATED __attribute__((deprecated))
#else
#define DEPRECATED
#endif

#define ARRAY_SIZE(_array) (sizeof(_array) / sizeof(_array[0]))

#define IS_FLAG(_v, _flag) ((bool)((_v & _flag) == _flag))

#define IS_NOT_FLAG(_v, _flag) ((bool)((_v & _flag) != _flag))

#define SARRAY_FOREACH(arr, it)                                                                    \
	if ((arr)->size)                                                                           \
		for (size_t i = 0; i < (arr)->size && ((it) = (arr)->data[i]); ++i)

#define BARRAY_FOREACH(arr, it)                                                                    \
	if (bo_array_size((arr)))                                                                  \
		for (size_t i = 0;                                                                 \
		     i < bo_array_size((arr)) && ((it) = bo_array_at((arr), i, void *));           \
		     ++i)

#define BLIST_FOREACH(list, it)                                                                    \
	(it) = bo_list_begin((list));                                                              \
	for (bo_iterator_t end = bo_list_end((list)); !bo_iterator_equal(&(it), &end);             \
	     bo_list_iter_next((list), &(it)))

#define ARRAY_FOREACH(arr, it)                                                                     \
	for (size_t _keep = 1, i = 0, _size = ARRAY_SIZE((arr)); _keep && i != _size;              \
	     _keep = !_keep, i++)                                                                  \
		for (it = (arr)[i]; _keep; _keep = !_keep)

#define BHTBL_FOREACH(htbl, it)                                                                    \
	(it) = bo_htbl_begin((htbl));                                                              \
	for (bo_iterator_t end = bo_htbl_end((htbl)); !bo_iterator_equal(&(it), &end);             \
	     bo_htbl_iter_next((htbl), &(it)))

#define BLIST_FOREACH(list, it)                                                                    \
	(it) = bo_list_begin((list));                                                              \
	for (bo_iterator_t end = bo_list_end((list)); !bo_iterator_equal(&(it), &end);             \
	     bo_list_iter_next((list), &(it)))

extern u64 main_thread_id;

SmallArrayType(AstPtr, struct Ast *, 16);
SmallArrayType(TypePtr, struct MirType *, 16);
SmallArrayType(MemberPtr, struct MirMember *, 16);
SmallArrayType(VariantPtr, struct MirVariant *, 16);
SmallArrayType(ArgPtr, struct MirArg *, 16);
SmallArrayType(InstrPtr, struct MirInstr *, 16);
SmallArrayType(ConstValuePtr, struct MirConstValue *, 16);
SmallArrayType(Char, char, 128);

typedef struct ID {
	const char *str;
	u64         hash;
} ID;

void
id_init(ID *id, const char *str);

bool
file_exists(const char *filepath);

const char *
brealpath(const char *file, char *out, s32 out_len);

bool
get_dir_from_filepath(char *buf, const size_t l, const char *filepath);

bool
get_filename_from_filepath(char *buf, const size_t l, const char *filepath);

bool
get_current_exec_path(char *buf, size_t buf_size);

bool
get_current_exec_dir(char *buf, size_t buf_size);

void
date_time(char *buf, s32 len, const char *format);

bool
is_aligned(const void *p, size_t alignment);

void
align_ptr_up(void **p, size_t alignment, ptrdiff_t *adjustment);

void
print_bits(s32 const size, void const *const ptr);

int
count_bits(u64 n);

void
platform_lib_name(const char *name, char *buffer, size_t max_len);

/*
 * Creates BArray inside Assembly arena.
 * Note: no free is needed.
 */
BArray *
create_arr(struct Assembly *assembly, size_t size);

/*
 * Creates SmallArray inside Assembly arena.
 * Note: no free is needed.
 */
void *
_create_sarr(struct Assembly *cnt, size_t arr_size);

#define create_sarr(T, Asm) ((T *)_create_sarr((Asm), sizeof(T)))

#endif
