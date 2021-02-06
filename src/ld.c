//************************************************************************************************
// bl
//
// File:   ld.c
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

#if BL_PLATFORM_LINUX
#define SHARED_EXT "so"
#elif BL_PLATFORM_MACOS
#define SHARED_EXT "dylib"
#else
#error "Unknown platform!"
#endif
#define OBJECT_EXT "o"
#define DEFAULT_ENTRY "_start"

#define FLAG_LIBPATH "-L"
#define FLAG_LIB "-l"
#define FLAG_ENTRY "-e"
#define FLAG_OUT "-o"

// Wrapper for ld linker on Unix platforms.
static const char *get_out_extension(Assembly *assembly)
{
    switch (assembly->options.build_output_kind) {
    case BUILD_OUT_EXECUTABLE:
        return "";
    case BUILD_OUT_SHARED_LIB:
        return SHARED_EXT;
    }
    BL_ABORT("Unknown output kind!");
}

static void append_lib_paths(Assembly *assembly, TString *buf)
{
    const char *dir;
    TARRAY_FOREACH(const char *, &assembly->options.lib_paths, dir)
    {
        tstring_appendf(buf, "%s%s ", FLAG_LIBPATH, dir);
    }
}

static void append_libs(Assembly *assembly, TString *buf)
{
    NativeLib *lib;
    for (usize i = 0; i < assembly->options.libs.size; ++i) {
        lib = &tarray_at(NativeLib, &assembly->options.libs, i);
        if (lib->is_internal) continue;
        if (!lib->user_name) continue;
        tstring_appendf(buf, "%s%s ", FLAG_LIB, lib->user_name);
    }
}

static void append_options(Assembly *assembly, TString *buf)
{
    // Some defaults which should be possible to modify later
    switch (assembly->options.build_output_kind) {
    case BUILD_OUT_EXECUTABLE: {
        tstring_appendf(buf, "%s %s ", FLAG_ENTRY, DEFAULT_ENTRY);
        break;
    }
    case BUILD_OUT_SHARED_LIB: {
        BL_ABORT("Unimplemented linker output type!");
        break;
    }
    }
}

static void append_default_opt(Assembly *assembly, TString *buf)
{
    const char *default_opt = conf_data_get_str(&builder.conf, CONF_LINKER_OPT_KEY);
    tstring_appendf(buf, "%s ", default_opt);
}

static void append_custom_opt(Assembly *assembly, TString *buf)
{
    const char *custom_opt = assembly->options.custom_linker_opt.data;
    if (custom_opt) tstring_appendf(buf, "%s ", custom_opt);
}

s32 ld(Assembly *assembly)
{
    TString *   buf         = get_tmpstr();
    const char *linker_exec = conf_data_get_str(&builder.conf, CONF_LINKER_EXEC_KEY);
    const char *out_dir     = assembly->options.out_dir.data;
    const char *name        = assembly->name;

    // set executable
    tstring_appendf(buf, "%s ", linker_exec);
    // set input file
    tstring_appendf(buf, "%s/%s.%s ", out_dir, name, OBJECT_EXT);
    // set output file
    const char *ext = get_out_extension(assembly);
    if (strlen(ext)) {
        tstring_appendf(buf, "%s %s/%s.%s ", FLAG_OUT, out_dir, name, ext);
    } else {
        tstring_appendf(buf, "%s %s/%s ", FLAG_OUT, out_dir, name);
    }
    append_options(assembly, buf);
    append_lib_paths(assembly, buf);
    append_libs(assembly, buf);
    append_default_opt(assembly, buf);
    append_custom_opt(assembly, buf);

    if (builder.options.verbose) builder_log("%s", buf->data);
    s32 state = system(buf->data);
    put_tmpstr(buf);
    return state;
}