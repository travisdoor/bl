//************************************************************************************************
// blc
//
// File:   linker.c
// Author: Martin Dorazil
// Date:   09/02/2018
//
// Copyright 2017 Martin Dorazil
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
#include "error.h"
#include "stages.h"

#define link_error(builder, code, tok, pos, format, ...)                                           \
	{                                                                                          \
		if (tok)                                                                           \
			builder_msg(builder,                                                       \
			            BUILDER_MSG_ERROR,                                             \
			            (code),                                                        \
			            &(tok)->src,                                                   \
			            (pos),                                                         \
			            (format),                                                      \
			            ##__VA_ARGS__);                                                \
		else                                                                               \
			builder_error(builder, (format), ##__VA_ARGS__);                           \
	}

typedef struct {
	Assembly *assembly;
	Builder * builder;
	bool      verbose;
} Context;

/* TODO: Support cross-platform build targets? */
void
platform_lib_name(const char *name, char *buffer, size_t max_len)
{
	if (!name) return;

#ifdef BL_PLATFORM_MACOS
	snprintf(buffer, max_len, "lib%s.dylib", name);
#elif defined(BL_PLATFORM_LINUX)
	snprintf(buffer, max_len, "lib%s.so", name);
#elif defined(BL_PLATFORM_WIN)
	snprintf(buffer, max_len, "%s.dll", name);
#else
	bl_abort("Unknown dynamic library format.");
#endif
}

static bool
link_lib(Context *cnt, const char *lib_name, Token *token, bool is_internal)
{
	char tmp[PATH_MAX] = {0};
	platform_lib_name(lib_name, tmp, PATH_MAX);

	DLLib *handle = dlLoadLibrary(lib_name ? tmp : NULL);
	if (!handle) return false;

	NativeLib native_lib = {.handle      = handle,
	                        .linked_from = token,
	                        .user_name   = lib_name,
	                        .filename    = NULL,
	                        .filepath    = NULL,
	                        .dirpath     = NULL,
	                        .is_internal = is_internal};

	native_lib.filename = strdup(tmp);
	dlGetLibraryPath(handle, tmp, PATH_MAX);
	native_lib.filepath = strdup(tmp);

	if (cnt->verbose) {
		msg_log("runtime linked file: %s", native_lib.filepath);
	}

	bo_array_push_back(cnt->assembly->dl.libs, native_lib);
	return true;
}

static bool
link_working_environment(Context *cnt)
{
#ifdef BL_PLATFORM_WIN
	const char *libc = MSVC_CRT;
#else
	const char *libc = NULL;
#endif
	return link_lib(cnt, libc, NULL, true);
}

void
linker_run(Builder *builder, Assembly *assembly)
{
	Context cnt = {.assembly = assembly,
	               .builder  = builder,
	               .verbose  = is_flag(builder->flags, BUILDER_VERBOSE)};

	if (cnt.verbose) {
		msg_log("running runtime linker...");
	}

	if (!link_working_environment(&cnt)) {
		Token *dummy = NULL;
		link_error(builder,
		           ERR_LIB_NOT_FOUND,
		           dummy,
		           BUILDER_CUR_WORD,
		           "Cannot link working environment.");
		return;
	}

	BHashTable *  cache = assembly->link_cache;
	Token *       token;
	bo_iterator_t it;
	bhtbl_foreach(cache, it)
	{
		token = bo_htbl_iter_peek_value(cache, &it, Token *);
		assert(token);

		if (!link_lib(&cnt, token->value.str, token, false)) {
			link_error(builder,
			           ERR_LIB_NOT_FOUND,
			           token,
			           BUILDER_CUR_WORD,
			           "Unresolved external library '%s'",
			           token->value.str);
		}
	}
}
