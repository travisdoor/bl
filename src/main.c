// =================================================================================================
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
// =================================================================================================

#include "assembly.h"
#include "builder.h"
#include <locale.h>
#include <stdio.h>
#include <string.h>

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

// =================================================================================================
// Command line arguments + Options
// =================================================================================================

#define GEN_ARGS_DEFINITIONS
#include "command_line_arguments.inc"
#undef GEN_ARGS_DEFINITIONS

typedef struct ApplicationOptions {
    bool print_help;
    bool print_about;
    bool where_is_api;
    bool configure;
} ApplicationOptions;

typedef struct Options {
    ApplicationOptions     app;
    struct builder_options builder;
    struct target *        target;
} Options;

void print_help(FILE *stream)
{
    const char *text = "Usage:\n  blc [options] <source-files>\n\nAlternative usage:\n  blc "
                       "[-r|-rs|-b] <source-file> [arguments]\n\nOptions:\n";
    fprintf(stream, "%s", text);

    char buf[256];
    for (u32 i = 0; i < TARRAY_SIZE(ARGS); ++i) {
        const Arg *arg = &ARGS[i];
        if (strlen(arg->s)) {
            snprintf(buf, TARRAY_SIZE(buf), "-%s, -%s", arg->s, arg->l);
        } else {
            snprintf(buf, TARRAY_SIZE(buf), "-%s", arg->l);
        }
        fprintf(stream, "  %-30s = %s\n", buf, arg->help);
    }
}

void print_about(FILE *stream)
{
    const char *text =
#include "about_text.txt"
        ;

    fprintf(stream, text, BL_VERSION, LLVM_VERSION_STRING);
}

void print_where_is_api(FILE *stream)
{
    fprintf(stream, "%s", builder_get_lib_dir());
}

#define INVALID_ARGS -1

s32 parse_arguments(Options *opt, s32 argc, char *argv[])
{
    BL_ASSERT(opt->target && "Target not initialized!");
#define ARG(kind, action)                                                                          \
    if ((strcmp(&argv[i][1], ARGS[kind].s) == 0) || (strcmp(&argv[i][1], ARGS[kind].l) == 0)) {    \
        action;                                                                                    \
        continue;                                                                                  \
    }
#define ARG_BREAK(kind, action)                                                                    \
    if ((strcmp(&argv[i][1], ARGS[kind].s) == 0) || (strcmp(&argv[i][1], ARGS[kind].l) == 0)) {    \
        action;                                                                                    \
        ++i;                                                                                       \
        break;                                                                                     \
    }

    // skip executable name
    s32 i = 1;
    for (; i < argc && *argv[i] == '-'; i++) {
        ARG_BREAK(ARG_BUILD, opt->target->kind = ASSEMBLY_BUILD_PIPELINE;)
        ARG_BREAK(ARG_RUN, opt->target->run = true;)
        ARG_BREAK(ARG_RUN_SCRIPT, {
            opt->builder.silent  = true;
            opt->target->run     = true;
            opt->target->no_llvm = true;
        })

        // Application
        ARG(ARG_HELP, opt->app.print_help = true;)
        ARG(ARG_ABOUT, opt->app.print_about = true;)
        ARG(ARG_WHERE_IS_API, opt->app.where_is_api = true; opt->builder.silent = true;)
        ARG(ARG_CONFIGURE, opt->app.configure = true;)

        // Builder
        ARG(ARG_VERBOSE, opt->builder.verbose = true;)
        ARG(ARG_SILENT, opt->builder.silent = true;)
        ARG(ARG_NO_COLOR, opt->builder.no_color = true;)
        ARG(ARG_NO_JOBS, opt->builder.no_jobs = true;)
        ARG(ARG_NO_WARNING, opt->builder.no_warning = true;)
        ARG(ARG_FULL_PATH, opt->builder.full_path_reports = true;)
        ARG(ARG_NO_USAGE_CHECK, opt->builder.no_usage_check = true;)
        ARG(ARG_TIME_REPORT, opt->builder.time_report = true;)

        // Target
        ARG(ARG_LEX_DUMP, opt->target->print_tokens = true;)
        ARG(ARG_AST_DUMP, opt->target->print_ast = true;)
        ARG(ARG_EMIT_LLVM, opt->target->emit_llvm = true;)
        ARG(ARG_EMIT_ASM, opt->target->emit_asm = true;)
        ARG(ARG_EMIT_MIR, opt->target->emit_mir = true;)
        ARG(ARG_DI_DWARF, opt->target->di = ASSEMBLY_DI_DWARF;)
        ARG(ARG_DI_CODEVIEW, opt->target->di = ASSEMBLY_DI_CODEVIEW;)
        ARG(ARG_RELEASE_FAST, opt->target->opt = ASSEMBLY_OPT_RELEASE_FAST;)
        ARG(ARG_RELEASE_SMALL, opt->target->opt = ASSEMBLY_OPT_RELEASE_SMALL;)
        ARG(ARG_ASSERT_ON, opt->target->assert_mode = ASSERT_ALWAYS_ENABLED;)
        ARG(ARG_ASSERT_OFF, opt->target->assert_mode = ASSERT_ALWAYS_DISABLED;)
        ARG(ARG_REG_SPLIT_ON, opt->target->reg_split = true;)
        ARG(ARG_REG_SPLIT_OFF, opt->target->reg_split = false;)
        ARG(ARG_VERIFY_LLVM, opt->target->verify_llvm = true;)
        ARG(ARG_RUN_TESTS, opt->target->run_tests = true;)
        ARG(ARG_SHARED, opt->target->kind = ASSEMBLY_SHARED_LIB;)
        ARG(ARG_NO_API, opt->target->no_api = true;)
        ARG(ARG_NO_BIN, opt->target->no_bin = true;)
        ARG(ARG_NO_LLVM, opt->target->no_llvm = true;)
        ARG(ARG_NO_ANALYZE, opt->target->no_analyze = true;)
        ARG(ARG_DOCS, opt->target->kind = ASSEMBLY_DOCS;)
        ARG(ARG_SYNTAX_ONLY, opt->target->syntax_only = true;)
        ARG(ARG_VMDBG_ATTACH, opt->target->vmdbg_enabled = true;)

        builder_error("Invalid argument '%s'", argv[i]);
        return INVALID_ARGS;
    }

    return i;
#undef ARG
#undef ARG_BREAK
}

s32 parse_input_files(Options *opt, s32 argc, char *argv[])
{
    while (*argv != NULL) {
        target_add_file(opt->target, *argv);
        // Add ony first file rest will be consumed as command line arguments in running script.
        if (opt->target->run) break;
        argv++;
        argc--;
    }
    return argc;
}

// =================================================================================================
// MAIN
// =================================================================================================
int main(s32 argc, char *argv[])
{
    setvbuf(stdout, NULL, _IONBF, 0);
    // =============================================================================================
#define EXIT(_state)                                                                               \
    state = _state;                                                                                \
    goto RELEASE;                                                                                  \
    // =============================================================================================

#ifdef BL_DEBUG
    puts("Running in DEBUG mode");
#endif
    Options opt = {0};
    setlocale(LC_ALL, "C");
    tlib_set_allocator(&_bl_malloc, &bl_free);

    s32   state     = EXIT_SUCCESS;
    char *exec_dir  = NULL;
    char *conf_file = NULL;

    const f64 start_time_ms = get_tick_ms();

    exec_dir = get_exec_dir();
    builder_init(&opt.builder, exec_dir);
    // Just create default empty target assembly options here and setup it later depending on user
    // passed arguments!
    opt.target = builder_add_default_target("out");
    // Parse command line arguments and return count args parsed.
    const s32 parsed_argc = parse_arguments(&opt, argc, argv);
    builder_log("Compiler version: %s, LLVM: %d", BL_VERSION, LLVM_VERSION_MAJOR);
    if (parsed_argc == INVALID_ARGS) {
        print_help(stdout);
        EXIT(EXIT_FAILURE);
    }
    // Shift pointer of argv.
    argc -= parsed_argc;
    argv += parsed_argc;

    // Run configure if needed.
    if (opt.app.configure) {
        if (generate_conf() != 0) {
            builder_error("Cannot generate config file.");
            EXIT(EXIT_FAILURE);
        }

        EXIT(EXIT_SUCCESS);
    }

    if (opt.app.print_help) {
        print_help(stdout);
        EXIT(EXIT_SUCCESS);
    }

    if (opt.app.print_about) {
        print_about(stdout);
        EXIT(EXIT_SUCCESS);
    }

    // Load configuration file
    if (!load_conf_file(exec_dir)) {
        EXIT(EXIT_FAILURE);
    }

    // Setup LIB_DIR
    builder_set_lib_dir(conf_data_get_str(&builder.conf, CONF_LIB_DIR_KEY));

    if (opt.app.where_is_api) {
        print_where_is_api(stdout);
        EXIT(EXIT_SUCCESS);
    }

    const bool use_build_pipeline = opt.target->kind == ASSEMBLY_BUILD_PIPELINE;
    if (argc == 0 && !use_build_pipeline) {
        builder_warning("Nothing to do, no input files, sorry :(");
        EXIT(EXIT_SUCCESS);
    }

    // Forward reminding arguments to vm.
    target_set_vm_args(opt.target, argc, argv);
    if (!use_build_pipeline) {
        parse_input_files(&opt, argc, argv);
    }

    state                = builder_compile(opt.target);
    const f64 runtime_ms = get_tick_ms() - start_time_ms;
    builder_note("Finished in %.3f seconds.", runtime_ms * 0.001);

RELEASE:
    builder_terminate();
    free(exec_dir);
    free(conf_file);
    BL_LOG("Exit with state %d.", state);
    return state;
#undef EXIT
}
