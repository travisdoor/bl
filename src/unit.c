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

static bool
search_source_file(const char *filepath, char **out_filepath, char **out_dirpath, const char *wdir)
{
	if (filepath == NULL) goto NOT_FOUND;

	char        tmp[PATH_MAX] = {0};
	const char *rpath         = tmp;

	/* Lookup in working directory. */
	if (wdir) {
		strncpy(tmp, wdir, TARRAY_SIZE(tmp));
		strncat(tmp, PATH_SEPARATOR, TARRAY_SIZE(tmp) - 1);
		strncat(tmp, filepath, TARRAY_SIZE(tmp) - 1);

		if (file_exists(tmp)) {
			goto FOUND;
		}
	}

	rpath = brealpath(filepath, tmp, PATH_MAX);

	if (rpath != NULL) {
		goto FOUND;
	}

	/* file has not been found in current working direcotry -> search in LIB_DIR */
	if (ENV_LIB_DIR) {
		char tmp_lib_dir[PATH_MAX];

		strncpy(tmp_lib_dir, ENV_LIB_DIR, TARRAY_SIZE(tmp_lib_dir));
		strncat(tmp_lib_dir, PATH_SEPARATOR, TARRAY_SIZE(tmp_lib_dir) - 1);
		strncat(tmp_lib_dir, filepath, TARRAY_SIZE(tmp_lib_dir) - 1);

		rpath = brealpath(tmp_lib_dir, tmp, PATH_MAX);

		if (rpath != NULL) {
			goto FOUND;
		}
	}

	/* file has not been found in current working direcotry -> search in PATH */
	{
		char  tmp_env[PATH_MAX] = {0};
		char *env               = strdup(getenv(ENV_PATH));
		char *s                 = env;
		char *p                 = NULL;

		do {
			p = strchr(s, ENVPATH_SEPARATOR);
			if (p != NULL) {
				p[0] = 0;
			}

			strncpy(tmp_env, s, TARRAY_SIZE(tmp_env));
			strncat(tmp_env, PATH_SEPARATOR, TARRAY_SIZE(tmp_env) - 1);
			strncat(tmp_env, filepath, TARRAY_SIZE(tmp_env) - 1);

			rpath = brealpath(&tmp_env[0], tmp, PATH_MAX);

			s = p + 1;
		} while (p != NULL && rpath == NULL);

		free(env);
		if (rpath) {
			goto FOUND;
		}
	}

NOT_FOUND:
	return false;

FOUND:
	/* Absolute file path. */
	*out_filepath = strdup(rpath);

	/* Absolute directory path. */
	memset(tmp, 0, TARRAY_SIZE(tmp));
	if (get_dir_from_filepath(tmp, PATH_MAX, *out_filepath)) {
		*out_dirpath = strdup(tmp);
	}

	return true;
}

/* public */
Unit *unit_new_file(const char *filepath, Token *loaded_from, Unit *parent_unit)
{
	Unit *unit = bl_malloc(sizeof(Unit));
	memset(unit, 0, sizeof(Unit));

	search_source_file(
	    filepath, &unit->filepath, &unit->dirpath, parent_unit ? parent_unit->dirpath : NULL);

	unit->name = strdup(filepath);

	char tmp[PATH_MAX] = {0};
	if (get_filename_from_filepath(tmp, TARRAY_SIZE(tmp), filepath)) {
		unit->filename = strdup(tmp);
	} else {
		BL_ABORT("invalid file");
	}

	unit->loaded_from = loaded_from;
	unit->ast         = NULL;

	tokens_init(&unit->tokens);

	return unit;
}

void unit_delete(Unit *unit)
{
	free(unit->filepath);
	free(unit->dirpath);
	free(unit->src);
	free(unit->name);
	free(unit->filename);
	tokens_terminate(&unit->tokens);
	bl_free(unit);
}

const char *unit_get_src_ln(Unit *unit, s32 line, long *len)
{
	s32         l    = 1;
	const char *iter = unit->src;
	while (iter && l != line) {
		++l;
		iter = strchr(iter, '\n');
		iter = iter ? iter + 1 : NULL;
	}

	if (len) {
		long l2 = 0;
		if (iter) l2 = (long)(strchr(iter, '\n') - iter);
		if (l < 0) l2 = (long)strlen(iter);
		(*len) = l2;
	}

	return iter;
}
