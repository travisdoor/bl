//************************************************************************************************
// bl
//
// File:   common.c
// Author: Martin Dorazil
// Date:   11.8.18
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

#include "common.h"
#include <bobject/containers/hash.h>
#include <time.h>

#ifndef BL_COMPILER_MSVC
#include "unistd.h"
#endif

void id_init(ID *id, const char *str)
{
	assert(id);
	id->hash = bo_hash_from_str(str);
	id->str = str;
}

bool file_exists(const char *filepath)
{
#ifdef BL_COMPILER_MSVC
	return (bool)PathFileExistsA(filepath);
#else
	return access(filepath, F_OK) != -1;
#endif
}

const char *brealpath(const char *file, char *out, int32_t out_len)
{
	const char *resolved = NULL;
	assert(out);
	assert(out_len);
	if (!file)
		return resolved;

#ifdef BL_COMPILER_MSVC
	if (GetFullPathNameA(file, out_len, out, NULL) && file_exists(out))
		return &out[0];
	return NULL;
#else
	return realpath(file, out);
#endif
}

void date_time(char *buf, int32_t len, const char *format)
{
	assert(buf && len);
	time_t timer;
	struct tm *tm_info;

	time(&timer);
	tm_info = localtime(&timer);

	strftime(buf, len, format, tm_info);
}

bool is_aligned(const void *p, size_t alignment) { return (uintptr_t)p % alignment == 0; }

void align_ptr_up(void **p, size_t alignment, ptrdiff_t *adjustment)
{
	ptrdiff_t adj;
	if (is_aligned(*p, alignment)) {
		if (adjustment)
			*adjustment = 0;
		return;
	}

	const size_t mask = alignment - 1;
	assert((alignment & mask) == 0 && "wrong alignemet"); // pwr of 2
	const uintptr_t i_unaligned = (uintptr_t)(*p);
	const uintptr_t misalignment = i_unaligned & mask;

	adj = alignment - misalignment;
	*p = (void *)(i_unaligned + adj);
	if (adjustment)
		*adjustment = adj;
}

void print_bits(int32_t const size, void const *const ptr)
{
	unsigned char *b = (unsigned char *)ptr;
	unsigned char byte;
	int32_t i, j;

	for (i = size - 1; i >= 0; i--) {
		for (j = 7; j >= 0; j--) {
			byte = (b[i] >> j) & 1;
			printf("%u", byte);
		}
	}
	puts("");
}
