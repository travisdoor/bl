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
#define DLL_EXT "dll"
#define LIB_EXT "lib"
#define OBJECT_EXT "obj"

#define FLAG_LIBPATH "/LIBPATH"
#define FLAG_OUT "/OUT"
#define FLAG_ENTRY "/ENTRY"
#define FLAG_SUBSYSTEM "/SUBSYSTEM"
#define FLAG_NOLOGO "/NOLOGO"
#define FLAG_INCPREMENTAL "/INCREMENTAL"
#define FLAG_MACHINE "/MACHINE"
#define FLAG_DEBUG "/DEBUG"
#define FLAG_DLL "/DLL"

#define DEFAULT_ARCH "x64"
#define DEFAULT_ENTRY "__os_start"

static void append_lib_paths(Assembly *assembly, TString *buf)
{
    const char *dir;
    TARRAY_FOREACH(const char *, &assembly->options.lib_paths, dir)
    {
        tstring_appendf(buf, "%s:\"%s\" ", FLAG_LIBPATH, dir);
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

static void append_options(Assembly *assembly, TString *buf)
{
    // Some defaults which should be possible to modify later
    const bool is_debug = assembly->options.build_mode == BUILD_MODE_DEBUG;

    tstring_appendf(buf, "%s ", FLAG_NOLOGO);
    //tstring_appendf(buf, "%s:%s ", FLAG_ENTRY, DEFAULT_ENTRY);
    //tstring_appendf(buf, "%s:%s ", FLAG_SUBSYSTEM, "CONSOLE");
    tstring_appendf(buf, "%s ", FLAG_DLL);
    tstring_appendf(buf, "%s:%s ", FLAG_INCPREMENTAL, "NO");
    tstring_appendf(buf, "%s:%s ", FLAG_MACHINE, DEFAULT_ARCH);
    if (is_debug) tstring_appendf(buf, "%s ", FLAG_DEBUG);
}

static void append_default_opt(Assembly *assembly, TString *buf)
{
    const bool  is_debug    = assembly->options.build_mode == BUILD_MODE_DEBUG;
    const char *default_opt = is_debug ? conf_data_get_str(&builder.conf, CONF_LINKER_OPT_DEBUG_KEY)
                                       : conf_data_get_str(&builder.conf, CONF_LINKER_OPT_KEY);
    tstring_appendf(buf, "%s ", default_opt);
}

static void append_custom_opt(Assembly *assembly, TString *buf)
{
    const char *custom_opt = assembly->options.custom_linker_opt.data;
    if (custom_opt) tstring_appendf(buf, "%s ", custom_opt);
}

s32 link_exe(Assembly *assembly)
{
    TString *   buf         = get_tmpstr();
    const char *linker_exec = conf_data_get_str(&builder.conf, CONF_LINKER_EXEC_KEY);
    const char *out_dir     = assembly->options.out_dir.data;
    const char *name        = assembly->name;

    tstring_append(buf, "call ");
    if (!builder.options.no_vcvars) {
        const char *vc_vars_all = conf_data_get_str(&builder.conf, CONF_VC_VARS_ALL_KEY);
        tstring_appendf(buf, "\"%s\" %s >NUL && ", vc_vars_all, DEFAULT_ARCH);
    }

    // set executable
    tstring_appendf(buf, "\"%s\" ", linker_exec);
    // set input file
    tstring_appendf(buf, "\"%s/%s.%s\" ", out_dir, name, OBJECT_EXT);
    // set output file
    tstring_appendf(buf, "%s:\"%s/%s.%s\" ", FLAG_OUT, out_dir, name, DLL_EXT);
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
