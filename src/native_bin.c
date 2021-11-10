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
#include "stb_ds.h"

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
    char                *dest_path = gettmpstr();
    const struct target *target    = assembly->target;
    const char          *out_dir   = target->out_dir;
    for (usize i = 0; i < arrlenu(assembly->libs); ++i) {
        struct native_lib *lib = &assembly->libs[i];
        if (lib->is_internal) continue;
        if (!lib->user_name) continue;
        char *lib_dest_name = lib->filename;
#if BL_PLATFORM_LINUX || BL_PLATFORM_MACOS
        struct stat statbuf;
        lstat(lib->filepath, &statbuf);
        if (S_ISLNK(statbuf.st_mode)) {
            char buf[PATH_MAX] = {0};
            if (readlink(lib->filepath, buf, static_arrlenu(buf)) == -1) {
                builder_error("Cannot follow symlink '%s' with error: %d", lib->filepath, errno);
                continue;
            }
            lib_dest_name = buf;
        }
#endif

        strprint(dest_path, "%s/%s", out_dir, lib_dest_name);
        if (file_exists(dest_path)) continue;
        builder_info("Copy '%s' to '%s'.", lib->filepath, dest_path);
        if (!copy_file(lib->filepath, dest_path)) {
            builder_error("Cannot copy '%s' to '%s'.", lib->filepath, dest_path);
        }
    }
    puttmpstr(dest_path);
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

    const char *out_dir = assembly->target->out_dir;
    zone();
    if (linker(assembly) != 0) {
        builder_msg(MSG_ERR, ERR_LIB_NOT_FOUND, NULL, CARET_WORD, "Native link execution failed.");
        goto DONE;
    }

    if (assembly->target->copy_deps) {
        builder_log("Copy assembly dependencies into '%s'.", out_dir);
        copy_user_libs(assembly);
    }
DONE:
    return_zone();
}
