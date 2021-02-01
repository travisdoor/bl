//*****************************************************************************
// tlib-c
//
// File:   string.h
// Author: Martin Dorazil
// Date:   29/9/2019
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

#ifndef T_STRING_H
#define T_STRING_H

#include "tlib/common.h"

typedef struct TString {
    char *data;
    usize len;
    usize allocated;

    /* Small strings can fit into this statically allocated block. */
    char _tmp[64];
} TString;

/* clang-format off */
/* Create new string on heap. */
TAPI TString *
tstring_new(void);

/* Delete string on heap. */
TAPI void
tstring_delete(TString *str);

/* Initialize string. */
TAPI void 
tstring_init(TString *str);

/* Terminate string. */
TAPI void
tstring_terminate(TString *str);

/* Reserve desired len, if current allocated block is big enough this function
 * has no effect.
 */
TAPI void
tstring_reserve(TString *str, usize len);

/* Append c-string at the end. */
TAPI void
tstring_append(TString *str, const char *v);

/* Append one character at the end. */
TAPI void
tstring_append_c(TString *str, const char v);

/* Append N characters from c-string at the end. */
TAPI void
tstring_append_n(TString *str, const char *v, usize N);

/* Set string based of formatting and passed data (similar to printf). 
 * This will clear old data.
 */
#define tstring_setf(str, format, ...) \
    {\
        tstring_clear(str);\
        tstring_appendf(str, format, ##__VA_ARGS__);\
    }

TAPI void
tstring_appendf(TString *str, const char *format, ...);

TAPI s32
tstring_replace_all(TString *str, char old, char replace);

/* Clear the string, allocated block (does not free). */
TAPI void
tstring_clear(TString *str);
/* clang-format on */

#endif
