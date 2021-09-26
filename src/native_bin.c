// =================================================================================================
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
// =================================================================================================

#include "builder.h"

#if !BL_PLATFORM_WIN
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

typedef s32 (*LinkerFn)(struct assembly *);

s32 lld_link(struct assembly *assembly);
s32 lld_ld(struct assembly *assembly);

static void copy_user_libs(struct assembly *assembly)
{
    TString *            dest_path = get_tmpstr();
    const struct target *target    = assembly->target;
    const char *         out_dir   = target->out_dir.data;
    struct native_lib *  lib;
    for (usize i = 0; i < assembly->libs.size; ++i) {
        lib = &tarray_at(struct native_lib, &assembly->libs, i);
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

void native_bin_run(struct assembly *assembly)
{
    builder_log("Running native linker...");
    LinkerFn linker = NULL;
#if BL_PLATFORM_WIN
    linker = &lld_link;
#elif BL_PLATFORM_LINUX || BL_PLATFORM_MACOS
    linker = &lld_ld;
#else
#error "Unknown platform"
#endif

    const char *out_dir = assembly->target->out_dir.data;
    ZONE();
    if (linker(assembly) != 0) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_LIB_NOT_FOUND,
                    NULL,
                    BUILDER_CUR_WORD,
                    "Native link execution failed.");
        goto DONE;
    }

    if (assembly->target->copy_deps) {
        builder_log("Copy assembly dependencies into '%s'.", out_dir);
        copy_user_libs(assembly);
    }
DONE:
    RETURN_ZONE();
}
