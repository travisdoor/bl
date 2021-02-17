//************************************************************************************************
// blc
//
// File:   main.c
// Author: Martin Dorazil
// Date:   04/02/2018
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

#include "assembly.h"
#include "bldebug.h"
#include "builder.h"
#include "error.h"
#include "unit.h"
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#if BL_PLATFORM_WIN
#include <windows.h>
#endif

static const char *ABOUT_TEXT =
#include "about_text.txt"
    ;

static char *get_exec_dir(void)
{
    char tmp[PATH_MAX] = {0};
    if (!get_current_exec_dir(tmp, PATH_MAX)) {
        BL_ABORT("Cannot locate compiler executable path.");
    }
    return strdup(tmp);
}

static bool load_conf_file(const char *exec_dir)
{
    char path[PATH_MAX] = {0};
    snprintf(path, TARRAY_SIZE(path), "%s/../%s", exec_dir, BL_CONF_FILE);
    if (!file_exists(path)) {
        builder_error("Configuration file '%s' not found, run 'blc --configure' or 'bl-config' to "
                      "generate one.",
                      path);
        return false;
    }
    builder_load_config(path);
    const char *expected = BL_VERSION;
    const char *got      = conf_data_has_key(&builder.conf, CONF_VERSION)
                               ? conf_data_get_str(&builder.conf, CONF_VERSION)
                               : "(UNKNOWN)";
    if (strcmp(expected, got)) {
        builder_warning("Invalid version of configuration file expected is '%s' not '%s'. Consider "
                        "running 'blc --configure' or 'bl-config' again.",
                        expected,
                        got);
    }
    return true;
}

static int generate_conf(void)
{
    TString *cmd = get_tmpstr();
#if BL_PLATFORM_LINUX || BL_PLATFORM_MACOS
    tstring_setf(cmd, "%s/%s -f -s", builder_get_exec_dir(), BL_CONFIGURE_SH);
#else
    tstring_setf(cmd, "call \"%s/%s\" -f -s", builder_get_exec_dir(), BL_CONFIGURE_SH);
#endif
    const s32 state = system(cmd->data);
    put_tmpstr(cmd);
    return state;
}

//**************************************************************************************************
// MAIN
//**************************************************************************************************
int main(s32 argc, char *argv[])
{
    //**********************************************************************************************
#define EXIT(_state)                                                                               \
    state = _state;                                                                                \
    goto RELEASE;                                                                                  \
    //**********************************************************************************************

#ifdef BL_DEBUG
    puts("Running in DEBUG mode");
#endif
    setlocale(LC_ALL, "C");
    tlib_set_allocator(&_bl_malloc, &bl_free);

    s32   state     = EXIT_SUCCESS;
    char *exec_dir  = NULL;
    char *conf_file = NULL;

    exec_dir = get_exec_dir();
    builder_init(exec_dir);
    s32 next_arg = builder_parse_options(argc, argv);
    builder_log("Compiler version: %s, LLVM: %d", BL_VERSION, LLVM_VERSION_MAJOR);
    if (next_arg == -1) {
        builder_print_help(stdout);
        EXIT(EXIT_FAILURE);
    }

    // Run configure if needed.
    if (builder.options.run_configure) {
        if (generate_conf() != 0) {
            builder_error("Cannot generate config file.");
            EXIT(EXIT_FAILURE);
        }

        EXIT(EXIT_SUCCESS);
    }

    argv += next_arg;
    const s32 vm_argc = argc - next_arg;
    char **   vm_argv = argv;

    if (builder.options.print_help) {
        builder_print_help(stdout);
        EXIT(EXIT_SUCCESS);
    }

    if (builder.options.print_about) {
        fprintf(stdout, ABOUT_TEXT, BL_VERSION, LLVM_VERSION_STRING);
        EXIT(EXIT_SUCCESS);
    }

    // Load config file
    if (!load_conf_file(exec_dir)) {
        EXIT(EXIT_FAILURE);
    }

    // setup LIB_DIR
    builder_set_lib_dir(conf_data_get_str(&builder.conf, CONF_LIB_DIR_KEY));

    if (builder.options.where_is_api) {
        fprintf(stdout, "%s", builder_get_lib_dir());
        EXIT(EXIT_SUCCESS);
    }

    const bool use_build_pipeline = builder.options.assembly_kind == ASSEMBLY_BUILD_PIPELINE;
    if (*argv == NULL && !use_build_pipeline) {
        builder_warning("Nothing to do, no input files, sorry :(");
        EXIT(EXIT_SUCCESS);
    }

    Assembly *assembly = assembly_new(builder.options.assembly_kind, "out");
    assembly_set_vm_args(assembly, vm_argc, vm_argv);
    if (use_build_pipeline) {
        assembly->options.opt = ASSEMBLY_OPT_RELEASE_FAST;
        assembly_add_unit(assembly, BUILD_SCRIPT_FILE, NULL);
    } else {
        while (*argv != NULL) {
            assembly_add_unit(assembly, *argv, NULL);
            if (assembly->options.run) break;
            argv++;
        }
    }

    builder_add_assembly(assembly);
    state = builder_compile_all();

    char date[26];
    date_time(date, 26, "%d-%m-%Y %H:%M:%S");
    builder_note("Finished at %s", date);

RELEASE:
    builder_terminate();
    free(exec_dir);
    free(conf_file);
    BL_LOG("Exit with state %d.", state);
    return state;
#undef EXIT
}
