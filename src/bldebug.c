//************************************************************************************************
// blc
//
// File:   bldebug.c
// Author: Martin Dorazil
// Date:   26.1.18
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

#include "bldebug.h"
#include "common.h"
#include <stdarg.h>

#define MAX_LOG_MSG_SIZE 2048

void
_log(bl_log_msg_type_e t, const char *file, int32_t line, const char *msg, ...)
{
	char    buffer[MAX_LOG_MSG_SIZE];
	va_list args;
	va_start(args, msg);
	vsnprintf(buffer, MAX_LOG_MSG_SIZE, msg, args);

	switch (t) {
	case LOG_ASSERT:
		fprintf(stderr, RED("assert [%s:%d]: %s") "\n", file, line, buffer);
		break;
	case LOG_ABORT:
		fprintf(stderr, RED("abort [%s:%d]: %s") "\n", file, line, buffer);
		break;
	case LOG_WARNING:
		fprintf(stderr, YELLOW("bl_warning [%s:%d]: %s") "\n", file, line, buffer);
		break;
	case LOG_MSG:
		fprintf(stdout, "bl_log [%s:%d]: %s\n", file, line, buffer);
		break;
	default:
		break;
	}

	va_end(args);
}

void
print_trace(void)
{
#if defined(BL_PLATFORM_MACOS) || defined(BL_PLATFORM_LINUX)
#include <execinfo.h>
	void * tmp[32];
	size_t size;
	char **strings;
	size_t i;

	size    = backtrace(tmp, ARRAY_SIZE(tmp));
	strings = backtrace_symbols(tmp, size);

	printf("Obtained %zd stack frames.\n", size);

	for (i = 1; i < size; i++)
		printf("%s\n", strings[i]);

	free(strings);
#endif
}
