//*****************************************************************************
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
//*****************************************************************************

#include "assembly.h"
#include "bldebug.h"
#include "builder.h"
#include "error.h"
#include "threading.h"
#include "unit.h"
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

char *ENV_EXEC_DIR      = NULL;
char *ENV_LIB_DIR       = NULL;
char *ENV_CONF_FILEPATH = NULL;

static void free_env(void)
{
    free(ENV_EXEC_DIR);
    free(ENV_LIB_DIR);
    free(ENV_CONF_FILEPATH);
}

static void setup_env(void)
{
    char tmp[PATH_MAX] = {0};

    if (!get_current_exec_dir(tmp, PATH_MAX)) {
        BL_ABORT("Cannot locate compiler executable path.");
    }

    ENV_EXEC_DIR = strdup(tmp);

    strcat(tmp, PATH_SEPARATOR ".." PATH_SEPARATOR);
    strcat(tmp, BL_CONF_FILE);

    if (strlen(tmp) == 0) BL_ABORT("Invalid conf file path.");
    ENV_CONF_FILEPATH = strdup(tmp);
}

static int generate_conf(void)
{
    char tmp[PATH_MAX] = {0};

#if defined(BL_PLATFORM_LINUX) || defined(BL_PLATFORM_MACOS)
    strcat(tmp, "sh ");
    strcat(tmp, ENV_EXEC_DIR);
    strcat(tmp, PATH_SEPARATOR);
    strcat(tmp, BL_CONFIGURE_SH);
#else
    strcat(tmp, "\"");
    strcat(tmp, ENV_EXEC_DIR);
    strcat(tmp, PATH_SEPARATOR);
    strcat(tmp, BL_CONFIGURE_SH);
    strcat(tmp, "\"");
#endif

    return system(tmp);
}

#define EXIT(_state)                                                                               \
    state = _state;                                                                                \
    goto RELEASE;

int main(s32 argc, char *argv[])
{
    s32         state = EXIT_SUCCESS;
    const char *help_text =
#include "help_text.txt"
        ;
    const char *about_text =
#include "about_text.txt"
        ;

    setlocale(LC_ALL, "C");
    setup_env();
    main_thread_id = thread_get_id();

#ifdef BL_DEBUG
    puts("Running in DEBUG mode");
    printf("Main thread ID: 0x%llx\n", main_thread_id);
#endif

    builder_init();
    s32 next_arg = builder_parse_options(argc, argv);
    builder_log("Compiler version: %s, LLVM: %d", BL_VERSION, LLVM_VERSION_MAJOR);
    if (next_arg == -1) {
        fprintf(stdout, "%s", help_text);
        EXIT(EXIT_FAILURE);
    }

    // Run configure if needed.
    if (builder.options.run_configure) {
        if (generate_conf() != 0) {
            builder_error("Cannot generate '%s' file. If you are compiler developer please "
                          "run configuration script in 'install' directory.",
                          ENV_CONF_FILEPATH);
            EXIT(EXIT_FAILURE);
        }

        EXIT(EXIT_SUCCESS);
    }

    argv += next_arg;
    const s32 vm_argc = argc - next_arg;
    char **   vm_argv = argv;

    if (builder.options.print_help) {
        fprintf(stdout, "%s", help_text);
        EXIT(EXIT_SUCCESS);
    }

    if (builder.options.print_about) {
        fprintf(stdout, about_text, BL_VERSION, LLVM_VERSION_STRING);
        EXIT(EXIT_SUCCESS);
    }

    if (!file_exists(ENV_CONF_FILEPATH)) {
        builder_error("Configuration file '%s' not found, run 'blc -configure' to "
                      "generate one.",
                      ENV_CONF_FILEPATH);
        EXIT(EXIT_FAILURE);
    }

    builder_load_config(ENV_CONF_FILEPATH);

    // setup LIB_DIR
    ENV_LIB_DIR = strdup(conf_data_get_str(&builder.conf, CONF_LIB_DIR_KEY));

    if (builder.options.where_is_api) {
        fprintf(stdout, "%s", ENV_LIB_DIR);
        EXIT(EXIT_SUCCESS);
    }

    if (*argv == NULL && !builder.options.use_pipeline) {
        builder_warning("Nothing to do, no input files, sorry :(");
        EXIT(EXIT_SUCCESS);
    }

    Assembly *assembly = assembly_new("out");
    assembly_set_vm_args(assembly, vm_argc, vm_argv);
    if (builder.options.use_pipeline) {
        assembly->options.build_mode = BUILD_MODE_BUILD;

        Unit *unit = unit_new_file(BUILD_SCRIPT_FILE, NULL);

        bool added = assembly_add_unit_unique(assembly, unit);
        if (added == false) {
            unit_delete(unit);
        }
    }

    while (*argv != NULL) {
        Unit *unit  = unit_new_file(*argv, NULL);
        bool  added = assembly_add_unit_unique(assembly, unit);
        if (added == false) {
            unit_delete(unit);
        }
        if (builder.options.run) break;
        argv++;
    }

    builder_add_assembly(assembly);
    state = builder_compile_all();

    char date[26];
    date_time(date, 26, "%d-%m-%Y %H:%M:%S");
    builder_note("Finished at %s", date);

RELEASE:
    builder_terminate();
    free_env();
    BL_LOG("Exit with state %d.", state);
    return state;
}
