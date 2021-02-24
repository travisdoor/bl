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

// Wrapper for lld.exe on Windows platform.

#define LLD_FLAVOR "link"
#define EXECUTABLE_EXT "exe"
#define DLL_EXT "dll"
#define LIB_EXT "lib"
#define OBJECT_EXT "obj"

#define FLAG_LIBPATH "/LIBPATH"
#define FLAG_OUT "/OUT"
#define FLAG_ENTRY "/ENTRY"
#define FLAG_DEBUG "/DEBUG"

static const char *get_out_extension(Assembly *assembly)
{
    switch (assembly->target->kind) {
    case ASSEMBLY_EXECUTABLE:
        return EXECUTABLE_EXT;
    case ASSEMBLY_SHARED_LIB:
        return DLL_EXT;
    default:
        BL_ABORT("Unknown output kind!");
    }
}

static void append_lib_paths(Assembly *assembly, TString *buf)
{
    const char *dir;
    TARRAY_FOREACH(const char *, &assembly->lib_paths, dir)
    {
        tstring_appendf(buf, "%s:\"%s\" ", FLAG_LIBPATH, dir);
    }
}

static void append_libs(Assembly *assembly, TString *buf)
{
    NativeLib *lib;
    for (usize i = 0; i < assembly->libs.size; ++i) {
        lib = &tarray_at(NativeLib, &assembly->libs, i);
        if (lib->is_internal) continue;
        if (!lib->user_name) continue;
        tstring_appendf(buf, "%s.%s ", lib->user_name, LIB_EXT);
    }
}

static void append_default_opt(Assembly *assembly, TString *buf)
{
    const bool is_debug = assembly->target->opt == ASSEMBLY_OPT_DEBUG;
    if (is_debug) tstring_appendf(buf, "%s ", FLAG_DEBUG);
    const char *default_opt = "";
    switch (assembly->target->kind) {
    case ASSEMBLY_EXECUTABLE:
        default_opt = conf_data_get_str(&builder.conf, CONF_LINKER_OPT_EXEC_KEY);
        break;
    case ASSEMBLY_SHARED_LIB:
        default_opt = conf_data_get_str(&builder.conf, CONF_LINKER_OPT_SHARED_KEY);
        break;
    default:
        BL_ABORT("Unknown output kind!");
    }
    tstring_appendf(buf, "%s ", default_opt);
}

static void append_custom_opt(Assembly *assembly, TString *buf)
{
    const char *custom_opt = assembly->custom_linker_opt.data;
    if (custom_opt) tstring_appendf(buf, "%s ", custom_opt);
}

static void append_linker_exec(TString *buf)
{
    if (conf_data_has_key(&builder.conf, CONF_LINKER_EXECUTABLE)) {
        const char *custom_linker = conf_data_get_str(&builder.conf, CONF_LINKER_EXECUTABLE);
        if (strlen(custom_linker)) {
            tstring_appendf(buf, "\"%s\" ", custom_linker);
            return;
        }
    }
    // Use LLD as default.
    tstring_appendf(buf, "\"%s/%s\" -flavor %s ", builder.exec_dir, BL_LINKER, LLD_FLAVOR);
}

s32 lld_link(Assembly *assembly)
{
    TString *   buf     = get_tmpstr();
    const char *out_dir = assembly->out_dir.data;
    const char *name    = assembly->target->name;

    tstring_append(buf, "call ");
    if (!assembly->target->no_vcvars) {
        const char *vc_vars_all = conf_data_get_str(&builder.conf, CONF_VC_VARS_ALL_KEY);
        tstring_appendf(buf, "\"%s\" %s >NUL && ", vc_vars_all, "x64");
    }

    // set executable
    append_linker_exec(buf);
    // set input file
    tstring_appendf(buf, "\"%s/%s.%s\" ", out_dir, name, OBJECT_EXT);
    // set output file
    tstring_appendf(buf, "%s:\"%s/%s.%s\" ", FLAG_OUT, out_dir, name, get_out_extension(assembly));
    append_lib_paths(assembly, buf);
    append_libs(assembly, buf);
    append_default_opt(assembly, buf);
    append_custom_opt(assembly, buf);

    builder_log("%s", buf->data);
    s32 state = system(buf->data);
    put_tmpstr(buf);
    return state;
}
