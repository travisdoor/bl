// =================================================================================================
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
// =================================================================================================

#include "assembly.h"
#include "builder.h"
#include "conf.h"
#include "stb_ds.h"

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

static const char *get_out_extension(struct assembly *assembly)
{
    switch (assembly->target->kind) {
    case ASSEMBLY_EXECUTABLE:
        return EXECUTABLE_EXT;
    case ASSEMBLY_SHARED_LIB:
        return DLL_EXT;
    default:
        babort("Unknown output kind!");
    }
}

static void append_lib_paths(struct assembly *assembly, char **buf)
{
    for (usize i = 0; i < arrlenu(assembly->lib_paths); ++i) {
        strappend(*buf, "%s:\"%s\" ", FLAG_LIBPATH, assembly->lib_paths[i]);
    }
}

static void append_libs(struct assembly *assembly, char **buf)
{
    for (usize i = 0; i < arrlenu(assembly->libs); ++i) {
        struct native_lib *lib = &assembly->libs[i];
        if (lib->is_internal) continue;
        if (!lib->user_name) continue;
        strappend(*buf, "%s.%s ", lib->user_name, LIB_EXT);
    }
}

static void append_default_opt(struct assembly *assembly, char **buf)
{
    const bool is_debug = assembly->target->opt == ASSEMBLY_OPT_DEBUG;
    if (is_debug) strappend(*buf, "%s ", FLAG_DEBUG);
    const char *default_opt = "";
    switch (assembly->target->kind) {
    case ASSEMBLY_EXECUTABLE:
        default_opt = confreads(builder.config, CONF_LINKER_OPT_EXEC_KEY, NULL);
        break;
    case ASSEMBLY_SHARED_LIB:
        default_opt = confreads(builder.config, CONF_LINKER_OPT_SHARED_KEY, NULL);
        break;
    default:
        babort("Unknown output kind!");
    }
    strappend(*buf, "%s ", default_opt);
}

static void append_custom_opt(struct assembly *assembly, char **buf)
{
    const char *custom_opt = assembly->custom_linker_opt;
    if (custom_opt) strappend(*buf, "%s ", custom_opt);
}

static void append_linker_exec(char **buf)
{
    const char *custom_linker = confreads(builder.config, CONF_LINKER_EXECUTABLE, "");
    if (strlen(custom_linker)) {
        strappend(*buf, "\"%s\" ", custom_linker);
        return;
    }
    // Use LLD as default.
    strappend(*buf, "\"%s/%s\" -flavor %s ", builder.exec_dir, BL_LINKER, LLD_FLAVOR);
}

s32 lld_link(struct assembly *assembly)
{
    runtime_measure_begin(linking);
    char                *buf     = gettmpstr();
    const struct target *target  = assembly->target;
    const char          *out_dir = target->out_dir;
    const char          *name    = target->name;

    strappend(buf, "call ");

    // set executable
    append_linker_exec(&buf);
    // set input file
    strappend(buf, "\"%s/%s.%s\" ", out_dir, name, OBJECT_EXT);
    // set output file
    strappend(buf, "%s:\"%s/%s.%s\" ", FLAG_OUT, out_dir, name, get_out_extension(assembly));
    append_lib_paths(assembly, &buf);
    append_libs(assembly, &buf);
    append_default_opt(assembly, &buf);
    append_custom_opt(assembly, &buf);

    builder_log("%s", buf);
    s32 state = system(buf);
    puttmpstr(buf);
    assembly->stats.linking_s = runtime_measure_end(linking);
    return state;
}
