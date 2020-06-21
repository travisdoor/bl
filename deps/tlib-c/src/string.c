//*****************************************************************************
// tlib-c
//
// File:   string.c
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

#include "tlib/string.h"
#include <stdarg.h>

static void
ensure_space(TString *str, usize space)
{
	if (!space) return;
	space += 1; /* zero terminated */
	if (space <= TARRAY_SIZE(str->_tmp)) return;
	if (str->allocated >= space) return;

	if (str->allocated == 0) {
		char *tmp = malloc(space * sizeof(char));
		memcpy(tmp, str->data, str->len + 1);
		str->data = tmp;
	} else {
		space *= 2;
		str->data = realloc(str->data, space * sizeof(char));
	}

	str->allocated = space;
}

/* public */
TString *
tstring_new(void)
{
	TString *str = malloc(sizeof(TString));
	if (!str) TABORT("Bad alloc.");

	tstring_init(str);
	return str;
}

void
tstring_delete(TString *str)
{
	if (!str) return;
	tstring_terminate(str);

	free(str);
}

void
tstring_init(TString *str)
{
	str->_tmp[0]   = '\0';
	str->data      = str->_tmp;
	str->len       = 0;
	str->allocated = 0;
}

void
tstring_terminate(TString *str)
{
	if (str->allocated) free(str->data);
	str->allocated = 0;
	str->len       = 0;
	str->_tmp[0]   = '\0';
	str->data      = str->_tmp;
}

void
tstring_reserve(TString *str, usize len)
{
	ensure_space(str, len);
}

void
tstring_append(TString *str, const char *v)
{
	tstring_append_n(str, v, strlen(v));
}

void
tstring_clear(TString *str)
{
	str->data[0] = '\0';
	str->len     = 0;
}

void
tstring_append_n(TString *str, const char *v, usize N)
{
	if (!v) return;
	const usize new_len = str->len + N;
	ensure_space(str, new_len);

	memcpy(&str->data[str->len], v, N * sizeof(char));
	str->data[new_len] = '\0';
	str->len           = new_len;
}

void
tstring_append_c(TString *str, const char v)
{
	tstring_append_n(str, &v, 1);
}

void
tstring_setf(TString *str, const char *format, ...)
{
	tstring_clear(str);
	ensure_space(str, strlen(format));

	va_list argp;
	va_start(argp, format);
	while (*format != '\0') {
		if (*format == '%') {
			format++;
			if (*format == '%') {
				tstring_append_c(str, '%');
			} else if (*format == 's') {
				char *s = va_arg(argp, char *);
				tstring_append(str, s);
			} else {
				fputs("Unssuported formating character for 'tstring_setf'.",
				      stdout);
			}
		} else {
			tstring_append_c(str, *format);
		}
		format++;
	}
	va_end(argp);
}
