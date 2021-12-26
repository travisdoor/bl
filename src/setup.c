// =================================================================================================
// blc
//
// File:   setup.c
// Author: Martin Dorazil
// Date:   23/12/2021
//
// Copyright 2021 Martin Dorazil
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
#include "common.h"
#include "stb_ds.h"

#if BL_PLATFORM_WIN
#include "wbs.h"
#endif

struct context {
    const char *triple;
    const char *filepath;

    char *version;
    char *lib_dir;
    char *linker_opt_exec;
    char *linker_opt_shared;
    char *linker_lib_path;
    char *linker_executable;

    struct string_cache *cache;
};

static bool default_config(struct context *ctx);
static bool x86_64_pc_windows_msvc(struct context *ctx);
static bool x86_64_pc_linux_gnu(struct context *ctx);
static bool xx_apple_darwin(struct context *ctx);

static char *make_content(const struct context *ctx);

bool setup(const char *exec_dir, const char *filepath, const char *triple)
{
    struct context ctx    = {0};
    ctx.triple            = triple;
    ctx.filepath          = filepath;
    ctx.linker_executable = "";
    ctx.linker_opt_exec   = "";
    ctx.linker_opt_shared = "";
    ctx.linker_lib_path   = "";

    bool state = false;

    // common config
    ctx.version  = BL_VERSION;
    char *libdir = gettmpstr();
    strprint(libdir, "%s/%s", exec_dir, BL_API_DIR);
    if (!normalize_path(&libdir)) {
        builder_error("BL API directory not found. (Expected location is '%s').", libdir);
        puttmpstr(libdir);
        return false;
    }
    ctx.lib_dir = scprint(&ctx.cache, "%s", libdir);
    puttmpstr(libdir);

    if (strcmp(ctx.triple, "x86_64-pc-windows-msvc") == 0) {
#ifdef BL_WBS
        state = x86_64_pc_windows_msvc(&ctx);
#else
        state = default_config(&ctx);
#endif
    } else if (strcmp(ctx.triple, "x86_64-pc-linux-gnu") == 0) {
        state = x86_64_pc_linux_gnu(&ctx);
    } else if (strcmp(ctx.triple, "x86_64-apple-darwin") == 0) {
        state = xx_apple_darwin(&ctx);
    } else if (strcmp(ctx.triple, "arm64-apple-darwin") == 0) {
        state = xx_apple_darwin(&ctx);
    } else {
        state = default_config(&ctx);
    }
    if (!state) {
        builder_error("Generate new configuration file at '%s' for target triple '%s' failed!",
                      ctx.filepath,
                      ctx.triple);
        return false;
    }
    char *content = make_content(&ctx);
    if (file_exists(ctx.filepath)) { // backup old-one
        char *bakfilepath = gettmpstr();
        char  date[26];
        date_time(date, static_arrlenu(date), "%d-%m-%Y_%H-%M-%S");
        strprint(bakfilepath, "%s.%s", ctx.filepath, date);
        builder_warning("Creating backup of previous configuration file at '%s'.", bakfilepath);
        copy_file(ctx.filepath, bakfilepath);
        puttmpstr(bakfilepath);
    }

    FILE *file = fopen(ctx.filepath, "w");
    if (!file) {
        builder_error("Cannot open file '%s' for writing!", ctx.filepath);
        puttmpstr(content);
        return false;
    }
    fputs(content, file);
    fclose(file);

    puttmpstr(content);
    scfree(&ctx.cache);
    return true;
}

char *make_content(const struct context *ctx)
{
#define TEMPLATE                                                                                   \
    "# Automatically generated configuration file used by 'blc' compiler.\n"                       \
    "# To generate new one use 'blc --configure' command.\n\n"                                     \
    "# Compiler version, this should match the executable version 'blc --version'.\n"              \
    "version: \"%s\"\n\n"                                                                          \
    "# Main API directory containing all modules and source files. This option is mandatory.\n"    \
    "lib_dir: \"%s\"\n"                                                                            \
    "\n"                                                                                           \
    "# Default configuration environment. This section is mandatory.\n"                            \
    "default:\n"                                                                                   \
    "    # Optional path to the linker executable, 'lld' linker is used by default on some "       \
    "platforms.\n"                                                                                 \
    "    linker_executable: \"%s\"\n"                                                              \
    "    # Linker flags and options used to produce executable binaries.\n"                        \
    "    linker_opt_exec: \"%s\"\n"                                                                \
    "    # Linker flags and options used to produce shared libraries.\n"                           \
    "    linker_opt_shared: \"%s\"\n"                                                              \
    "    # File system location where linker should lookup for dependencies.\n"                    \
    "    linker_lib_path: \"%s\"\n\n"

    char *tmp = gettmpstr();
    strprint(tmp,
             TEMPLATE,
             ctx->version,
             ctx->lib_dir,
             ctx->linker_executable,
             ctx->linker_opt_exec,
             ctx->linker_opt_shared,
             ctx->linker_lib_path);
    return tmp;

#undef TEMPLATE
}

// =================================================================================================
// Targets
// =================================================================================================
bool default_config(struct context UNUSED(*ctx))
{
    builder_warning("Automatic generation of configuration file is not supported for target triple "
                    "'%s' empty file will be generated at '%s'.",
                    ctx->triple,
                    ctx->filepath);
    return true;
}

#ifdef BL_WBS
bool x86_64_pc_windows_msvc(struct context *ctx)
{
    ctx->linker_executable = "";
    ctx->linker_opt_exec =
        "/NOLOGO /ENTRY:__os_start /SUBSYSTEM:CONSOLE /INCREMENTAL:NO /MACHINE:x64";
    ctx->linker_opt_shared = "/NOLOGO /INCREMENTAL:NO /MACHINE:x64 /DLL";

    struct wbs *wbs = wbslookup();
    if (!wbs->is_valid) {
        builder_error("Configuration failed!");
        goto FAILED;
    }
    ctx->linker_lib_path =
        scprint(&ctx->cache, "%s;%s;%s", wbs->ucrt_path, wbs->um_path, wbs->msvc_lib_path);

    wbsfree(wbs);
    return true;
FAILED:
    wbsfree(wbs);
    return false;
}
#endif

bool x86_64_pc_linux_gnu(struct context *ctx)
{
    return true;
}

static bool xx_apple_darwin(struct context *ctx)
{
    const char *COMMAND_LINE_TOOLS = "/Library/Developer/CommandLineTools";
    const char *MACOS_SDK          = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib";
    const char *LINKER_LIB_PATH    = "/usr/lib:/usr/local/lib";
    const char *LINKER_OPT_EXEC    = "-e ___os_start";
    const char *LINKER_OPT_SHARED  = "-dylib";

    if (!dir_exists(COMMAND_LINE_TOOLS)) {
        builder_error("Cannot find Command Line Tools on '%s', use 'xcode-select --install'.",
                      COMMAND_LINE_TOOLS);
        return false;
    }

    char *libpath   = gettmpstr();
    char *optexec   = gettmpstr();
    char *optshared = gettmpstr();

    strprint(libpath, "%s", LINKER_LIB_PATH);
    char *osver = execute("sw_vers -productVersion");
    if (strlenu(osver) == 0) {
        builder_error("Cannot detect macOS product version!");
    } else {
        s32 major, minor, patch;
        major = minor = patch = 0;
        if (sscanf(osver, "%d.%d.%d", &major, &minor, &patch) == 3) {
            if (major >= 11) {
                if (!dir_exists(MACOS_SDK)) {
                    builder_error("Cannot find macOS SDK on '%s'.", MACOS_SDK);
                } else {
                    strappend(libpath, ":%s", MACOS_SDK);
                }
            }
        }
        strappend(optexec, "-macosx_version_min %s -sdk_version %s ", osver, osver);
        strappend(optshared, "-macosx_version_min %s -sdk_version %s ", osver, osver);
    }

    strappend(optexec, "%s", LINKER_OPT_EXEC);
    strappend(optshared, "%s", LINKER_OPT_SHARED);

    ctx->linker_lib_path   = scdup(&ctx->cache, libpath, strlenu(libpath));
    ctx->linker_opt_exec   = scdup(&ctx->cache, optexec, strlenu(optexec));
    ctx->linker_opt_shared = scdup(&ctx->cache, optshared, strlenu(optshared));
    puttmpstr(osver);
    puttmpstr(optexec);
    puttmpstr(optshared);
    puttmpstr(libpath);

    char *ldpath = execute("which ld");
    if (strlenu(ldpath) == 0) {
        builder_error("The 'ld' linker not found on system!");
    }
    ctx->linker_executable = scdup(&ctx->cache, ldpath, strlenu(ldpath));
    puttmpstr(ldpath);

    return true;
}
