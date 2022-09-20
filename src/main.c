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
#include "conf.h"
#include "stb_ds.h"
#include <locale.h>
#include <stdio.h>
#include <string.h>

#if BL_CRTDBG_ALLOC
#include <crtdbg.h>
#endif

bool setup(const char *filepath, const char *triple);

static char *get_exec_dir(void)
{
    char tmp[PATH_MAX] = "";
    if (!get_current_exec_dir(tmp, PATH_MAX)) {
        babort("Cannot locate compiler executable path.");
    }
    return strdup(tmp);
}

static bool generate_conf(void)
{
    struct target_triple triple;
    if (!target_init_default_triple(&triple)) {
        return false;
    }
    char *str = target_triple_to_string(&triple);
    blog("Triple: %s", str);
    char *filepath = tstr();
    strprint(filepath, "%s/../%s", builder_get_exec_dir(), BL_CONFIG_FILE);
    const bool state = setup(filepath, str);
    bfree(str);
    put_tstr(filepath);
    return state;
}

static bool load_conf_file(void)
{
    char *filepath = tstr();
    strprint(filepath, "%s/../%s", builder_get_exec_dir(), BL_CONFIG_FILE);
    if (!file_exists(filepath)) {
        if (!generate_conf()) {
            builder_error("Failed to generate the configuration file, please report the issue on "
                          "https://github.com/travisdoor/bl/issues");
            goto FAILED;
        }
    }
    if (!builder_load_config(filepath)) goto FAILED;
    const char *got = confreads(builder.config, CONF_VERSION, "(UNKNOWN)");
    if (strcmp(got, BL_VERSION) != 0) {
        builder_warning("Invalid version of current configuration file '%s'. Expected is '%s', got "
                        "'%s'. Consider generation of new one using 'blc --configure'.",
                        filepath,
                        BL_VERSION,
                        got);
    }

    put_tstr(filepath);
    return true;
FAILED:
    put_tstr(filepath);
    return false;
}

// =================================================================================================
// Command line arguments + Options
// =================================================================================================

typedef struct ApplicationOptions {
    bool print_help;
    bool print_about;
    bool print_version;
    bool print_host_triple;
    bool print_supported;
    bool where_is_api;
    bool configure;
} ApplicationOptions;

typedef struct Options {
    ApplicationOptions     app;
    struct builder_options builder;
    struct target         *target;
} Options;

enum getarg_opt_kind {
    FLAG = 0,
    ENUM,
    NUMBER,
    STRING,
};

struct getarg_opt {
    const char          *name;
    enum getarg_opt_kind kind;
    union {
        bool  *b;
        s32   *n;
        char **s;
    } property;
    const char *variants;
    const char *help;
    const s32   id;
};

static s32
getarg(s32 argc, char *argv[], struct getarg_opt *opts, s32 *optindex, const char **positional)
{
    bassert(opts && optindex);
    (*positional) = NULL;
    (*optindex) += 1;
    if (*optindex >= argc) return -1;
    char *arg = argv[*optindex];
    if (arg[0] == '-') {
        // find =
        s32   len   = 0;
        char *value = arg;
        while (*value) {
            if ((*value++) == '=') break;
            ++len;
        }
        if (*value == '\0') {
            value = NULL;
        } else {
            *(arg + len) = '\0';
        }

        struct getarg_opt *opt;
        s32                i = 0;
        while ((opt = opts++)->name) {
            if (strcmp(arg, opt->name) == 0) {
                switch (opt->kind) {
                case FLAG:
                    if (value) {
                        builder_error("Unexpected value for flag '%s'", opt->name);
                        return '?';
                    }
                    if (opt->property.b) (*opt->property.b) = true;
                    break;
                case ENUM:
                    if (value) {
                        bool        found     = false;
                        s32         j         = 0;
                        char       *variants  = strdup(opt->variants);
                        const char *delimiter = "|";
                        char       *it        = variants;
                        char       *token;
                        while ((token = strtok_r(it, delimiter, &it))) {
                            if (strcmp(value, token) == 0) {
                                found = true;
                                break;
                            }
                            ++j;
                        }
                        free(variants);
                        if (found) {
                            if (opt->property.n) (*opt->property.n) = j;
                            break;
                        }
                    }
                    builder_error("Expected <%s> value for '%s'", opt->variants, opt->name);
                    return '?';
                case NUMBER:
                    if (!value) {
                        builder_error("Expected <N> value for property '%s'", opt->name);
                        return '?';
                    }
                    if (opt->property.n) (*opt->property.n) = atoi(value);
                    break;
                case STRING:
                    if (!value) {
                        builder_error("Expected <STRING> value for property '%s'", opt->name);
                        return '?';
                    }
                    if (opt->property.s) (*opt->property.s) = value;
                    break;
                }
                return opt->id;
            }
            ++i;
        }
        builder_error("Unknown argument '%s'", arg);
        return '?';
    }
    (*positional) = arg;
    return 0;
}

int _cmpfunc(const void *a, const void *b)
{
    struct getarg_opt *first  = (struct getarg_opt *)a;
    struct getarg_opt *second = (struct getarg_opt *)b;
    // List -<name> before --<name>
    const s32 fs = strncmp(first->name, "--", 2) == 0 ? 2 : 0;
    const s32 ss = strncmp(second->name, "--", 2) == 0 ? 2 : 0;
    return strcmp(first->name + fs, second->name + ss);
}

static void sort_optlist(struct getarg_opt *opts)
{
    s32 len = 0;
    for (; opts[len].name; ++len)
        ;
    qsort(opts, len, sizeof(struct getarg_opt), &_cmpfunc);
}

void print_help(FILE *stream, struct getarg_opt *opts)
{
    const char *text = "Usage:\n"
                       "  blc [options] [source-files]\n\n"
                       "Alternative usage:\n"
                       "  blc [options] <-build> [build-arguments]\n"
                       "  blc [options] <-run> <source-file> [arguments] [forwarded-arguments]\n\n"
                       "Options:\n";
    fprintf(stream, "%s", text);
    struct getarg_opt *opt;
    while ((opt = opts++)->name) {
        char arg[128];
        switch (opt->kind) {
        case FLAG:
            snprintf(arg, static_arrlenu(arg), "%s", opt->name);
            break;
        case ENUM:
            snprintf(arg, static_arrlenu(arg), "%s=<%s>", opt->name, opt->variants);
            break;
        case NUMBER:
            snprintf(arg, static_arrlenu(arg), "%s=<N>", opt->name);
            break;
        case STRING:
            snprintf(arg, static_arrlenu(arg), "%s=<STRING>", opt->name);
            break;
        }
        if (strlen(arg) > 24)
            fprintf(stream, "  %s\n                           %s\n", arg, opt->help);
        else
            fprintf(stream, "  %-24s %s\n", arg, opt->help);
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

void print_host_triple(FILE *stream)
{
    struct target_triple triple;
    if (target_init_default_triple(&triple)) {
        char *triple_str = target_triple_to_string(&triple);
        fprintf(stream, "%s", triple_str);
        bfree(triple_str);
    }
}

void print_supported(FILE *stream)
{
    char **list = builder_get_supported_targets();
    char **it   = list;
    for (; *it; it++) {
        fprintf(stream, "%s\n", *it);
    }
    bfree(list);
}

// =================================================================================================
// MAIN
// =================================================================================================
int main(s32 argc, char *argv[])
{
    // _crtBreakAlloc = 1782;

#define EXIT(_state)                                                                               \
    state = _state;                                                                                \
    goto RELEASE;

#ifdef BL_DEBUG
    puts("Running in DEBUG mode");
    printf("CPU count: %d\n", cpu_thread_count());
#endif
    Options opt = {0};
    setlocale(LC_ALL, "C.utf8");

    s32   state     = EXIT_SUCCESS;
    char *exec_dir  = NULL;
    char *conf_file = NULL;

    const f64 start_time_ms = get_tick_ms();

    exec_dir                = get_exec_dir();
    opt.builder.error_limit = 10;
    opt.builder.doc_out_dir = "out";
    builder_init(&opt.builder, exec_dir);
    builder_log("Compiler version: %s, LLVM: %d", BL_VERSION, LLVM_VERSION_MAJOR);
    // Just create default empty target assembly options here and setup it later depending on
    // user passed arguments!
    opt.target = builder_add_default_target("out");

    char *user_working_directory = NULL;
    bool  has_input_files        = false;

#define ID_BUILD 1
#define ID_RUN 2
#define ID_DOC 3
#define ID_SHARED 4
#define ID_VMDBG_BREAK_ON 5
#define ID_RELEASE 6
#define ID_SILENT_RUN 7

    struct getarg_opt optlist[] = {
        {
            .name = "-build",
            .help = "Invoke project build pipeline. All following arguments are forwarded into the "
                    "build script and ignored by compiler itself. Use as '-build [arguments]'.",
            .id   = ID_BUILD,
        },
        {
            .name = "-run",
            .help =
                "Execute BL program using interpreter and exit. The compiler expects <source-file> "
                "after '-run' flag, the file name and all following command line arguments are "
                "passed into the executed program and ignored by compiler itself. Use as '-run "
                "<source-file> [arguments]'.",
            .id = ID_RUN,
        },
        {
            .name = "-silent-run",
            .help =
                "Execute BL program using interpreter and exit. The compiler expects <source-file> "
                "after '-silent-run' flag, the file name and all following command line arguments "
                "are passed into the executed program and ignored by compiler itself. Use as "
                "'-silent-run <source-file> [arguments]'. This flag also suppress all compiler "
                "console outputs. Basically it combines '-run' and '--silent' into a single flag. "
                "This can be useful in case the compiler is called implicitly from UNIX shebang.",
            .id = ID_SILENT_RUN,
        },
        {
            .name = "-doc",
            .help = "Generate documentation and exit.",
            .id   = ID_DOC,
        },
        {
            .name       = "--version",
            .help       = "Print compiler version and exit.",
            .property.b = &opt.app.print_version,
        },
        {
            .kind = STRING,
            .name = "--doc-out-dir",
            .help = "Set documentation output directory. (Use 'out' in current working directory "
                    "by default.)",
            .property.s = &opt.builder.doc_out_dir,
        },
        {
            .kind = STRING,
            .name = "--work-dir",
            .help = "Set current working directory. Compiler use by default the current working "
                    "directory to output all files.",
            .property.s = &user_working_directory,
        },
        {
            .name = "-shared",
            .help = "Compile shared library.",
            .id   = ID_SHARED,
        },
        {
            .name       = "--help",
            .property.b = &opt.app.print_help,
            .help       = "Print usage information and exit.",
        },
        {
            .name       = "--about",
            .property.b = &opt.app.print_about,
            .help       = "Print compiler info and exit",
        },
        {
            .name       = "--where-is-api",
            .property.b = &opt.app.where_is_api,
            .help       = "Print path to API folder and exit.",
        },
        {
            .name       = "--target-host",
            .property.b = &opt.app.print_host_triple,
            .help       = "Print current host target triple and exit.",
        },
        {
            .name       = "--target-supported",
            .property.b = &opt.app.print_supported,
            .help = "Print all supported targets and exit. (Cross compilation is not allowed yet!)",
        },
        {
            .name       = "--target-experimental",
            .property.b = &opt.builder.enable_experimental_targets,
            .help       = "Enable experimental compilation targets.",
        },
        {
            .name       = "--configure",
            .property.b = &opt.app.configure,
            .help       = "Generate configuration file and exit.",
        },
        {
            .name       = "--verbose",
            .property.b = &opt.builder.verbose,
            .help       = "Enable verbose mode.",
        },
        {
            .name       = "--silent",
            .property.b = &opt.builder.silent,
            .help       = "Disable compiler console logging.",
        },
        {
            .name       = "--no-color",
            .property.b = &opt.builder.no_color,
            .help       = "Disable colored output.",
        },
        {
            .name       = "--no-jobs",
            .property.b = &opt.builder.no_jobs,
            .help       = "Enable single-thread mode.",
        },
        {
            .name       = "--no-warning",
            .property.b = &opt.builder.no_warning,
            .help       = "Ignore all warnings.",
        },
        {
            .name       = "--full-path",
            .property.b = &opt.builder.full_path_reports,
            .help       = "Report full file paths.",
        },
        {
            .name       = "--no-usage-check",
            .property.b = &opt.builder.no_usage_check,
            .help       = "Disable checking of unused symbols.",
        },
        {
            .name       = "--stats",
            .property.b = &opt.builder.stats,
            .help       = "Print compilation statistics.",
        },
        {
            .name       = "--lex-dump",
            .property.b = &opt.target->print_tokens,
            .help       = "Print tokens.",
        },
        {
            .name       = "--ast-dump",
            .property.b = &opt.target->print_ast,
            .help       = "Print AST.",
        },
        {
            .name       = "--emit-llvm",
            .property.b = &opt.target->emit_llvm,
            .help       = "Write LLVM-IR to file.",
        },
        {
            .name       = "--emit-asm",
            .property.b = &opt.target->emit_asm,
            .help       = "Write assembly to file.",
        },
        {
            .name       = "--emit-mir",
            .property.b = &opt.target->emit_mir,
            .help       = "Write MIR to file.",
        },
        {
            .name       = "--di",
            .kind       = ENUM,
            .property.n = (s32 *)&opt.target->di,
            .variants   = "dwarf|codeview",
            .help       = "Set debug info format.",
        },
        {
            .name       = "-opt",
            .kind       = ENUM,
            .property.n = (s32 *)&opt.target->opt,
            .variants   = "debug|release-fast|release-small|release-with-debug-info",
            .help       = "Specify binary optimization mode (use 'debug' by default).",
        },
        {
            .name = "-release",
            .kind = FLAG,
            .help = "Specify binary optimization mode to release. (same as '-opt=release-fast')",
            .id   = ID_RELEASE,
        },
        {
            .name       = "--assert",
            .kind       = ENUM,
            .property.n = (s32 *)&opt.target->assert_mode,
            .variants   = "default|on|off",
            .help =
                "Set assert mode ('default' option sets assert 'on' in debug and 'off' in release "
                "mode).",
        },
        {
            .name       = "--reg-split",
            .kind       = ENUM,
            .property.n = (s32 *)&opt.target->reg_split,
            .variants   = "off|on",
            .help = "Enable/disable splitting of structures passed into the function by value into "
                    "registers.",
        },
        {
            .name       = "--verify-llvm",
            .property.b = &opt.target->verify_llvm,
            .help       = "Verify LLVM IR after generation.",
        },
        {
            .name       = "--run-tests",
            .property.b = &opt.target->run_tests,
            .help       = "Execute all unit tests in compile time.",
        },
        {
            .name       = "--tests-minimal-output",
            .property.b = &opt.target->tests_minimal_output,
            .help = "Reduce compile-time tests (--run-tests) output (remove results section).",
        },
        {
            .name       = "--no-api",
            .property.b = &opt.target->no_api,
            .help       = "Don't load internal API.",
        },
        {
            .name       = "--no-bin",
            .property.b = &opt.target->no_bin,
            .help       = "Don't write binary to disk.",
        },
        {
            .name       = "--no-llvm",
            .property.b = &opt.target->no_llvm,
            .help       = "Disable LLVM back-end.",
        },
        {
            .name       = "--no-analyze",
            .property.b = &opt.target->no_analyze,
            .help       = "Disable analyze pass, only parse and exit.",
        },
        {
            .name       = "--syntax-only",
            .property.b = &opt.target->syntax_only,
            .help       = "Check syntax and exit.",
        },
        {
            .name       = "--vmdbg-attach",
            .property.b = &opt.target->vmdbg_enabled,
            .help       = "Attach compile-time execution debugger.",
        },
        {
            .name       = "--vmdbg-break-on",
            .kind       = NUMBER,
            .property.n = &opt.target->vmdbg_break_on,
            .help       = "Attach compile-time execution debugger and sets break point to the MIR "
                          "instruction with <N> id.",
            .id         = ID_VMDBG_BREAK_ON,
        },
        {
            .name       = "--error-limit",
            .kind       = NUMBER,
            .property.n = &opt.builder.error_limit,
            .help       = "Set maximum reported error count.",
        },
        {0},
    };

    sort_optlist(optlist);

    s32 index = 0;
    while (true) {
        const char *positional;

        s32 c = getarg(argc, argv, optlist, &index, &positional);
        if (c == -1) break;

        switch (c) {
        case '?': // Unknown/invalid argument.
            EXIT(EXIT_FAILURE);
        case ID_BUILD: // Build pipeline.
            opt.target->kind = ASSEMBLY_BUILD_PIPELINE;
            opt.target->run  = false;
            index += 1;
            // Rest of arguments is forwarded into the build script.
            goto SKIP;
        case ID_DOC: // Generate documentation.
            opt.target->kind = ASSEMBLY_DOCS;
            opt.target->run  = false;
            break;
        case ID_SHARED: // Shared library.
            opt.target->kind = ASSEMBLY_SHARED_LIB;
            opt.target->run  = false;
            break;
        case ID_SILENT_RUN: // Silent run mode.
            opt.builder.silent = true;
        case ID_RUN: // Run mode.
            opt.target->kind    = ASSEMBLY_EXECUTABLE;
            opt.target->run     = true;
            opt.target->no_llvm = true;
            if (index + 1 == argc || argv[index + 1][0] == '-') {
                builder_error("Expected file name after '-run' flag.");
                EXIT(EXIT_FAILURE);
            }
            break;
        case ID_VMDBG_BREAK_ON: // Break on
            opt.target->vmdbg_enabled = true;
            break;
        case ID_RELEASE:
            opt.target->opt = ASSEMBLY_OPT_RELEASE_FAST;
            break;
        default:
            if (positional) {
                target_add_file(opt.target, positional);
                has_input_files = true;
                if (opt.target->run) {
                    // Rest of arguments is forwarded into the executed assembly.
                    goto SKIP;
                }
                break;
            }
            break;
        }
    }
SKIP:
    // Shift pointer of argv.
    argc -= index;
    argv += index;

    // Run configure if needed.
    if (opt.app.configure) {
        if (!generate_conf()) {
            EXIT(EXIT_FAILURE);
        }
        EXIT(EXIT_SUCCESS);
    }

    if (opt.app.print_help) {
        print_help(stdout, optlist);
        EXIT(EXIT_SUCCESS);
    }

    if (opt.app.print_about) {
        print_about(stdout);
        EXIT(EXIT_SUCCESS);
    }

    if (opt.app.print_version) {
        fprintf(stdout, "%s", BL_VERSION);
        EXIT(EXIT_SUCCESS);
    }

    if (opt.app.print_host_triple) {
        print_host_triple(stdout);
        EXIT(EXIT_SUCCESS);
    }

    if (opt.app.print_supported) {
        print_supported(stdout);
        EXIT(EXIT_SUCCESS);
    }

    // Load configuration file
    if (!load_conf_file()) {
        EXIT(EXIT_FAILURE);
    }

    if (opt.app.where_is_api) {
        print_where_is_api(stdout);
        EXIT(EXIT_SUCCESS);
    }

    if (opt.target->kind != ASSEMBLY_BUILD_PIPELINE && !has_input_files) {
        builder_error("No input files, use 'blc my-source-file.bl' or 'blc -build' in case the "
                      "'build.bl' is present.");
        EXIT(EXIT_FAILURE);
    }

    // Forward reminding arguments to vm.
    target_set_vm_args(opt.target, argc, argv);

    // Use default triple here, this should be adjustable by users when cross-compilation will be
    // allowed!
    if (!target_init_default_triple(&opt.target->triple)) {
        exit(ERR_UNSUPPORTED_TARGET);
    }

    if (user_working_directory) {
        if (!set_current_working_dir(user_working_directory)) {
            builder_error("Cannot set working directory to '%s'.", user_working_directory);
            EXIT(EXIT_FAILURE);
        } else {
            builder_info("Running in '%s'.", user_working_directory);
        }
    }

    state                = builder_compile(opt.target);
    const f64 runtime_ms = get_tick_ms() - start_time_ms;
    builder_info("Finished in %.3f seconds.", runtime_ms * 0.001);

RELEASE:
    builder_terminate();
    free(exec_dir);
    free(conf_file);
    blog("Exit with state %d.", state);
#if BL_CRTDBG_ALLOC
    _CrtDumpMemoryLeaks();
#endif
    return state;
#undef EXIT
}
