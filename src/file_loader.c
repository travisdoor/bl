//************************************************************************************************
// bl
//
// File:   file_loader.c
// Author: Martin Dorazil
// Date:   04/02/2018
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
#include "stages.h"
#include "unit.h"
#include <stdio.h>
#include <string.h>

#define load_error(builder, code, tok, pos, format, ...)                                           \
	{                                                                                          \
		if (tok)                                                                           \
			builder_msg(builder, BUILDER_MSG_ERROR, (code), &(tok)->src, (pos),        \
				    (format), ##__VA_ARGS__);                                      \
		else                                                                               \
			builder_error(builder, (format), ##__VA_ARGS__);                           \
	}

void file_loader_run(Builder *builder, Unit *unit)
{
	if (!unit->filepath) {
		load_error(builder, ERR_FILE_NOT_FOUND, unit->loaded_from, BUILDER_CUR_WORD,
			   "file not found %s", unit->name);
		return;
	}

	FILE *f = fopen(unit->filepath, "r");

	if (f == NULL) {
		load_error(builder, ERR_FILE_READ, unit->loaded_from, BUILDER_CUR_WORD,
			   "cannot read file %s", unit->name);
		return;
	}

	fseek(f, 0, SEEK_END);
	size_t fsize = (size_t)ftell(f);
	if (fsize == 0) {
		fclose(f);
		load_error(builder, ERR_FILE_EMPTY, unit->loaded_from, BUILDER_CUR_WORD,
			   "invalid or empty source file %s", unit->name);
		return;
	}

	fseek(f, 0, SEEK_SET);

	char *src = calloc(fsize + 1, sizeof(char));
	if (src == NULL)
		bl_abort("bad alloc");
	if (!fread(src, sizeof(char), fsize, f))
		bl_abort("cannot read file %s", unit->name);

	src[fsize] = '\0';
	fclose(f);

	unit->src = src;
}
