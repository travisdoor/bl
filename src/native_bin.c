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
#if defined(BL_PLATFORM_WIN)
	const char *link_flag = "";
	const char *cmd = "\"%s\" %s.obj /OUT:%s %s";
#else
	const char *link_flag = "-l";
	const char *cmd = "%s %s.o -o %s %s";
#endif
	char        buf[2048];

	{ /* setup link command */
		const char *linker_exec = conf_data_get_str(builder->conf, CONF_LINKER_EXEC_KEY);
		const char *opt         = conf_data_get_str(builder->conf, CONF_LINKER_OPT_KEY);
		sprintf(buf, cmd, linker_exec, assembly->name, assembly->name, opt);
	}

	NativeLib *lib;
	for (size_t i = 0; i < bo_array_size(assembly->dl.libs); ++i) {
		lib = &bo_array_at(assembly->dl.libs, i, NativeLib);

		if (!lib->user_name) continue;

		strcat(buf, " ");
		strcat(buf, link_flag);
		strcat(buf, lib->user_name);
	}

	msg_log("%s", buf);
	/* TODO: handle error */
	int32_t result = system(buf);
	if (result != 0) {
		return;
	}
}
