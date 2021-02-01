//************************************************************************************************
// bl
//
// File:   link.c
// Author: Martin Dorazil
// Date:   1/02/2020
//
// Copyright 2020 Martin Dorazil
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

#include "assembly.h"
#include "builder.h"
#include "config.h"

#define EXECUTABLE_EXT "exe"
#define LIB_EXT "lib"
#define OBJECT_EXT "obj"
#define LIB_PATH_FLAG "/LIBPATH"
#define OUT_FLAG "/OUT"

static void append_lib_paths(Assembly *assembly, TString *buf)
{
    const char *dir;
    TARRAY_FOREACH(const char *, &assembly->options.lib_paths, dir)
    {
        tstring_appendf(buf, "%s:\"%s\" ", LIB_PATH_FLAG, dir);
    }
}

static void append_libs(Assembly *assembly, TString *buf)
{
    NativeLib *lib;
    for (usize i = 0; i < assembly->options.libs.size; ++i) {
        lib = &tarray_at(NativeLib, &assembly->options.libs, i);
        if (lib->is_internal) continue;
        if (!lib->user_name) continue;
        tstring_appendf(buf, "%s.%s ", lib->user_name, LIB_EXT);
    }
}

s32 link_exe(Assembly UNUSED(*assembly))
{
    TString *   buf         = get_tmpstr();
    const char *linker_exec = conf_data_get_str(&builder.conf, CONF_LINKER_EXEC_KEY);
    const char *out_dir     = assembly->options.out_dir.data;
    const char *name        = assembly->name;
    // const bool is_debug = assembly->options.build_mode == BUILD_MODE_DEBUG;

    // set executable
    tstring_appendf(buf, "%s ", linker_exec);
    // set input file
    tstring_appendf(buf, "\"%s/%s.%s\" ", out_dir, name, OBJECT_EXT);
    // set output file
    tstring_appendf(buf, "%s:\"%s/%s.%s\" ", OUT_FLAG, out_dir, name, EXECUTABLE_EXT);
    append_lib_paths(assembly, buf);
    append_libs(assembly, buf);

    BL_LOG("command: %s", buf->data);
    put_tmpstr(buf);
    return 0;
}
