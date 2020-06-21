//*****************************************************************************
// tlib-c
//
// File:   hash.h
// Author: Martin Dorazil
// Date:   1/10/2019
//
// Copyright 2019 Martin Dorazil
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
//*****************************************************************************

#ifndef T_HASH_H
#define T_HASH_H

#include "tlib/common.h"

static inline u32
thash_from_str(const char *str)
{
	u32 hash = 5381;
	s32 c;

	while ((c = *str++))
		hash = ((hash << 5) + hash) + c;

	return hash;
}

static inline u32
thash_from_strn(const char *str, usize len)
{
	u32   hash = 5381;
	usize i    = 0;
	s32   c;

	while ((c = *str++) && (i < len)) {
		++i;
		hash = ((hash << 5) + hash) + c;
	}

	return hash;
}

#endif
