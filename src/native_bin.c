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
static const char *link_path_flag = "/LIBPATH:";
static const char *cmd            = "call \"%s\" %s && \"%s\" %s.obj /OUT:%s.exe %s %s";
#else
static const char *link_flag      = "-l";
static const char *link_path_flag = "-L";
static const char *cmd            = "%s %s.o -o %s %s %s";
#endif

typedef struct {
	Assembly *assembly;
} Context;

static void
add_lib_paths(Context *cnt, TString *buf)
{

	const char *dir;
	TARRAY_FOREACH(const char *, &cnt->assembly->options.lib_paths, dir)
	{
		tstring_append(buf, " ");
#ifdef BL_PLATFORM_WIN
		tstring_append(buf, "\"");
#endif
		tstring_append(buf, link_path_flag);
		tstring_append(buf, dir);
#ifdef BL_PLATFORM_WIN
		tstring_append(buf, "\"");
#endif
	}
}

static void
add_libs(Context *cnt, TString *buf)
{
	NativeLib *lib;
	for (usize i = 0; i < cnt->assembly->options.libs.size; ++i) {
		lib = &tarray_at(NativeLib, &cnt->assembly->options.libs, i);
		if (lib->is_internal) continue;
		if (!lib->user_name) continue;

		tstring_append(buf, " ");
		tstring_append(buf, link_flag);
		tstring_append(buf, lib->user_name);
#ifdef BL_PLATFORM_WIN
		tstring_append(buf, ".lib");
#endif
	}
}

void
native_bin_run(Assembly *assembly)
{
	TString buf;
	tstring_init(&buf);
	Context cnt = {.assembly = assembly};

#ifdef BL_PLATFORM_WIN
	const char *linker_exec = conf_data_get_str(builder.conf, CONF_LINKER_EXEC_KEY);
	{ /* setup link command */
		const char *vc_vars_all = conf_data_get_str(builder.conf, CONF_VC_VARS_ALL_KEY);
		const char *vc_arch     = "x64"; // TODO: set by compiler target arch
		const char *default_opt = conf_data_get_str(builder.conf, CONF_LINKER_OPT_KEY);
		const char *custom_opt  = assembly->options.custom_linker_opt.len
		                             ? assembly->dl.custom_linker_opt.data
		                             : "";

		tstring_setf(&buf,
		             cmd,
		             vc_vars_all,
		             vc_arch,
		             linker_exec,
		             assembly->name,
		             assembly->name,
		             default_opt,
		             custom_opt);
	}
#else
        const char *linker_exec = conf_data_get_str(builder.conf, CONF_LINKER_EXEC_KEY);
        { /* setup link command */
                const char *default_opt = conf_data_get_str(builder.conf, CONF_LINKER_OPT_KEY);
                const char *custom_opt  = assembly->options.custom_linker_opt.len
                                             ? assembly->options.custom_linker_opt.data
                                             : "";

                tstring_setf(&buf,
                             cmd,
                             linker_exec,
                             assembly->name,
                             assembly->name,
                             default_opt,
                             custom_opt);
        }
#endif

	add_lib_paths(&cnt, &buf);
	add_libs(&cnt, &buf);

	msg_log("Running native linker...");
	if (builder.options.verbose) msg_log("%s", buf.data);
	/* TODO: handle error */
	if (system(buf.data) != 0) {
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_LIB_NOT_FOUND,
		            NULL,
		            BUILDER_CUR_WORD,
		            "Native link execution failed '%s'",
		            buf.data);
	}

	tstring_terminate(&buf);
}
