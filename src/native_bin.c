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

void
native_bin_run(Builder *builder, Assembly *assembly)
{
	char buf[2048] = {0};

#ifdef BL_PLATFORM_WIN
	const char *link_flag = "";
	const char *cmd       = "call \"%s\" %s && \"%s\" %s.obj /OUT:%s.exe %s";

	{ /* setup link command */
		const char *vc_vars_all = conf_data_get_str(builder->conf, CONF_VC_VARS_ALL_KEY);
		const char *vc_arch     = "x64"; // TODO: set by compiler target arch
		const char *linker_exec = conf_data_get_str(builder->conf, CONF_LINKER_EXEC_KEY);
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
	const char *link_flag = "-l";
	const char *cmd       = "%s %s.o -o %s %s";

	{ /* setup link command */
		const char *linker_exec = conf_data_get_str(builder->conf, CONF_LINKER_EXEC_KEY);
		const char *opt         = conf_data_get_str(builder->conf, CONF_LINKER_OPT_KEY);
		sprintf(buf, cmd, linker_exec, assembly->name, assembly->name, opt);
	}
#endif

	NativeLib *lib;
	for (size_t i = 0; i < bo_array_size(assembly->dl.libs); ++i) {
		lib = &bo_array_at(assembly->dl.libs, i, NativeLib);
		if (lib->is_internal) continue;
		if (!lib->user_name) continue;

		strcat(buf, " ");
		strcat(buf, link_flag);
		strcat(buf, lib->user_name);
	}

	// msg_log("%s", buf);
	/* TODO: handle error */
	system(buf);
}
