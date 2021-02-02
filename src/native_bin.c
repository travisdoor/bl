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

#if !BL_PLATFORM_WIN
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

s32 link_exe(Assembly *assembly);

static const char *link_flag      = "-l";
static const char *link_path_flag = "-L";
static const char *cmd            = "%s %s/%s.o -o %s/%s %s %s";

static void copy_user_libs(Assembly *assembly)
{
    TString *   dest_path = get_tmpstr();
    const char *out_dir   = assembly->options.out_dir.data;
    NativeLib * lib;
    for (usize i = 0; i < assembly->options.libs.size; ++i) {
        lib = &tarray_at(NativeLib, &assembly->options.libs, i);
        if (lib->is_internal) continue;
        if (!lib->user_name) continue;
        char *lib_dest_name = lib->filename;
#if BL_PLATFORM_LINUX || BL_PLATFORM_MACOS
        struct stat statbuf;
        lstat(lib->filepath, &statbuf);
        if (S_ISLNK(statbuf.st_mode)) {
            char buf[PATH_MAX] = {0};
            if (readlink(lib->filepath, buf, TARRAY_SIZE(buf)) == -1) {
                builder_error("Cannot follow symlink '%s' with error: %d", lib->filepath, errno);
                continue;
            }
            lib_dest_name = buf;
        }
#endif

        tstring_setf(dest_path, "%s/%s", out_dir, lib_dest_name);
        if (file_exists(dest_path->data)) continue;
        builder_warning("Copy '%s' to '%s'.", lib->filepath, dest_path->data);
        if (!copy_file(lib->filepath, dest_path->data)) {
            builder_error("Cannot copy '%s' to '%s'.", lib->filepath, dest_path->data);
        }
    }
    put_tmpstr(dest_path);
}

static void append_lib_paths(Assembly *assembly, TString *buf)
{

    const char *dir;
    TARRAY_FOREACH(const char *, &assembly->options.lib_paths, dir)
    {
        tstring_append(buf, " ");
        tstring_append(buf, link_path_flag);
        tstring_append(buf, dir);
    }
}

static void append_libs(Assembly *assembly, TString *buf)
{
    NativeLib *lib;
    for (usize i = 0; i < assembly->options.libs.size; ++i) {
        lib = &tarray_at(NativeLib, &assembly->options.libs, i);
        if (lib->is_internal) continue;
        if (!lib->user_name) continue;

        tstring_append(buf, " ");
        tstring_append(buf, link_flag);
        tstring_append(buf, lib->user_name);
    }
}

void native_bin_run(Assembly *assembly)
{
    builder_log("Running native linker...");
    const char *out_dir = assembly->options.out_dir.data;
#if !BL_PLATFORM_WIN
    TString *buf = get_tmpstr();
    TracyCZone(_tctx, true);

    const char *linker_exec = conf_data_get_str(&builder.conf, CONF_LINKER_EXEC_KEY);
    { /* setup link command */
        const char *default_opt = conf_data_get_str(&builder.conf, CONF_LINKER_OPT_KEY);
        const char *custom_opt =
            assembly->options.custom_linker_opt.len ? assembly->options.custom_linker_opt.data : "";

        tstring_setf(buf,
                     cmd,
                     linker_exec,
                     out_dir,
                     assembly->name,
                     out_dir,
                     assembly->name,
                     default_opt,
                     custom_opt);
    }

    append_lib_paths(assembly, buf);
    append_libs(assembly, buf);

    builder_log("Running native linker...");
    if (builder.options.verbose) builder_log("%s", buf->data);
    if (system(buf->data) != 0) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_LIB_NOT_FOUND,
                    NULL,
                    BUILDER_CUR_WORD,
                    "Native link execution failed '%s'",
                    buf->data);
        goto DONE;
    }

    if (assembly->options.copy_deps) {
        builder_log("Copy assembly dependencies into '%s'.", out_dir);
        copy_user_libs(assembly);
    }

DONE:
    put_tmpstr(buf);
    TracyCZoneEnd(_tctx);
#else
    TracyCZone(_tctx, true);
    if (link_exe(assembly) != 0) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_LIB_NOT_FOUND,
                    NULL,
                    BUILDER_CUR_WORD,
                    "Native link execution failed.");
        goto DONE;
    }

    if (assembly->options.copy_deps) {
        builder_log("Copy assembly dependencies into '%s'.", out_dir);
        copy_user_libs(assembly);
    }
DONE:
    TracyCZoneEnd(_tctx);
#endif
}
