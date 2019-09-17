//************************************************************************************************
// bl
//
// File:   native_bin.c
// Author: Martin Dorazil
// Date:   10/03/2018
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

#include "config.h"
#include "stages.h"

#ifdef BL_PLATFORM_WIN
static const char *link_flag      = "";
static const char *link_path_flag = "/LIBPATH";
static const char *cmd            = "call \"%s\" %s && \"%s\" %s.obj /OUT:%s.exe %s";
#else
static const char *link_flag      = "-l";
static const char *link_path_flag = "-L";
static const char *cmd            = "%s %s.o -o %s %s";
#endif

typedef struct {
	Builder * builder;
	Assembly *assembly;
} Context;

static void
add_lib_paths(Context *cnt, char *buf, size_t len)
{

	const char *dir;
	BARRAY_FOREACH(cnt->assembly->dl.lib_paths, dir)
	{
		strncat(buf, " ", len);
		strncat(buf, link_path_flag, len);
		strncat(buf, dir, len);
	}
}

static void
add_libs(Context *cnt, char *buf, size_t len)
{
	NativeLib *lib;
	for (size_t i = 0; i < bo_array_size(cnt->assembly->dl.libs); ++i) {
		lib = &bo_array_at(cnt->assembly->dl.libs, i, NativeLib);
		if (lib->is_internal) continue;
		if (!lib->user_name) continue;

		strncat(buf, " ", len);
		strncat(buf, link_flag, len);
		strncat(buf, lib->user_name, len);
	}
}

void
native_bin_run(Builder *builder, Assembly *assembly)
{
	char    buf[4096] = {0};
	Context cnt       = {.builder = builder, .assembly = assembly};

#ifdef BL_PLATFORM_WIN
	const char *linker_exec = conf_data_get_str(builder->conf, CONF_LINKER_EXEC_KEY);
	{ /* setup link command */
		const char *vc_vars_all = conf_data_get_str(builder->conf, CONF_VC_VARS_ALL_KEY);
		const char *vc_arch     = "x64"; // TODO: set by compiler target arch
		const char *opt         = conf_data_get_str(builder->conf, CONF_LINKER_OPT_KEY);
		sprintf(buf,
		        cmd,
		        vc_vars_all,
		        vc_arch,
		        linker_exec,
		        assembly->name,
		        assembly->name,
		        opt);
	}
#else
        const char *linker_exec = conf_data_get_str(builder->conf, CONF_LINKER_EXEC_KEY);
        { /* setup link command */
                const char *opt = conf_data_get_str(builder->conf, CONF_LINKER_OPT_KEY);
                snprintf(
                    buf, ARRAY_SIZE(buf), cmd, linker_exec, assembly->name, assembly->name, opt);
        }
#endif

	add_lib_paths(&cnt, buf, ARRAY_SIZE(buf));
	add_libs(&cnt, buf, ARRAY_SIZE(buf));

	msg_log("Running native linker...");
	if (assembly->options.verbose_mode) msg_log("%s", buf);
	/* TODO: handle error */
	if (system(buf) != 0) {
		builder_msg(builder,
		            BUILDER_MSG_ERROR,
		            ERR_LIB_NOT_FOUND,
		            NULL,
		            BUILDER_CUR_WORD,
		            "Native link execution failed '%s'",
		            buf);
	}
}
