//************************************************************************************************
// bl
//
// File:   unit.c
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

#include "unit.h"
#include "bldebug.h"
#include "blmemory.h"
#include "token.h"
#include <limits.h>
#include <string.h>

/* public */
Unit *
unit_new_file(const char *filepath, Token *loaded_from, Unit *parent_unit)
{
	Unit *unit = bl_calloc(1, sizeof(Unit));
	if (!unit) bl_abort("bad alloc");

	search_file(
	    filepath, &unit->filepath, &unit->dirpath, parent_unit ? parent_unit->dirpath : NULL);

	unit->name = strdup(filepath);

	char tmp[PATH_MAX] = {0};
	if (get_filename_from_filepath(tmp, array_size(tmp), filepath)) {
		unit->filename = strdup(tmp);
	} else {
		bl_abort("invalid file");
	}

	unit->loaded_from = loaded_from;
	unit->ast         = NULL;

	tokens_init(&unit->tokens);
	return unit;
}

void
unit_delete(Unit *unit)
{
	free(unit->filepath);
	free(unit->dirpath);
	free(unit->src);
	free(unit->name);
	free(unit->filename);
	tokens_terminate(&unit->tokens);
	bl_free(unit);
}

const char *
unit_get_src_ln(Unit *unit, int32_t line, long *len)
{
	int32_t     l    = 1;
	const char *iter = unit->src;
	while (iter && l != line) {
		++l;
		iter = strchr(iter, '\n');
		iter = iter ? iter + 1 : NULL;
	}

	if (len) {
		long l = 0;
		if (iter) l = (long)(strchr(iter, '\n') - iter);
		if (l < 0) l = (long)strlen(iter);
		(*len) = l;
	}

	return iter;
}
