//************************************************************************************************
// blc
//
// File:   builder.c
// Author: Martin Dorazil
// Date:   14.2.18
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

#include <stdarg.h>
#include <time.h>

#include "assembly.h"
#include "builder.h"
#include "common.h"
#include "experimental/puml_export.h"
#include "stages.h"
#include "token.h"
#include "unit.h"

#ifdef BL_PLATFORM_WIN
#include <windows.h>
#endif

#define MAX_MSG_LEN 1024
#define MAX_ERROR_REPORTED 10

Builder builder;

static int  compile_unit(Unit *unit, Assembly *assembly);
static int  compile_assembly(Assembly *assembly);
static bool llvm_initialized = false;

static void str_cache_dtor(TString *str)
{
    tstring_terminate(str);
}

static void llvm_init(void)
{
    if (llvm_initialized) return;

    LLVMInitializeX86Target();
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmPrinter();

    llvm_initialized = true;
}

static void llvm_terminate(void)
{
    LLVMShutdown();
}

#define INTERRUPT_ON_ERROR                                                                         \
    if (builder.errorc) goto INTERRUPT;
#define FINISH_IF(expr)                                                                            \
    if ((expr)) goto FINISH;

int compile_unit(Unit *unit, Assembly *assembly)
{
    if (unit->loaded_from) {
        builder_log(
            "Compile: %s (loaded from '%s')", unit->name, unit->loaded_from->location.unit->name);
    } else {
        builder_log("Compile: %s", unit->name);
    }

    file_loader_run(unit);
    INTERRUPT_ON_ERROR;
    lexer_run(unit);
    INTERRUPT_ON_ERROR;
    if (builder.options.print_tokens) {
        token_printer_run(unit);
        INTERRUPT_ON_ERROR;
    }
    parser_run(assembly, unit);
    return COMPILE_OK;
INTERRUPT:
    return COMPILE_FAIL;
}

int compile_assembly(Assembly *assembly)
{
    { // MSG
        BuilderMessage msg = {.kind = BUILDER_MSG_ASSEMBLY_BEGIN};
        builder_invoke_message(assembly, &msg);
    }
    if (builder.options.print_ast) {
        ast_printer_run(assembly, stdout);
    }
    INTERRUPT_ON_ERROR;

    linker_run(assembly);
    INTERRUPT_ON_ERROR;

    FINISH_IF(builder.options.syntax_only);

    mir_run(assembly);
    if (builder.options.emit_mir) mir_writer_run(assembly);
    INTERRUPT_ON_ERROR;

    FINISH_IF(builder.options.no_analyze);
    FINISH_IF(builder.options.no_llvm);
    FINISH_IF(assembly->options.build_mode == BUILD_MODE_BUILD);
    ir_run(assembly);
    INTERRUPT_ON_ERROR;

    ir_opt_run(assembly);
    INTERRUPT_ON_ERROR;

    if (builder.options.emit_llvm) {
        bc_writer_run(assembly);
        INTERRUPT_ON_ERROR;
    }

    if (!builder.options.no_bin) {
        obj_writer_run(assembly);
        INTERRUPT_ON_ERROR;
        native_bin_run(assembly);
        INTERRUPT_ON_ERROR;
    }

FINISH : {
    BuilderMessage msg = {.kind = BUILDER_MSG_ASSEMBLY_END};
    builder_invoke_message(assembly, &msg);
}
    return COMPILE_OK;
INTERRUPT : { // MSG
    BuilderMessage msg = {.kind = BUILDER_MSG_ASSEMBLY_FAILED};
    builder_invoke_message(assembly, &msg);
}
    return COMPILE_FAIL;
}

/* public */
s32 builder_parse_options(s32 argc, char *argv[])
{
#define IS_PARAM(_arg) (strcmp(&argv[optind][1], _arg) == 0)

    builder.options.build_mode = BUILD_MODE_DEBUG;

#if defined(BL_PLATFORM_WIN)
    builder.options.build_di_kind = BUILD_DI_CODEVIEW;
#else
    builder.options.build_di_kind = BUILD_DI_DWARF;
#endif

    s32 optind = 1;
    for (; optind < argc && argv[optind][0] == '-'; optind++) {
        if (IS_PARAM("ast-dump")) {
            builder.options.print_ast = true;
        } else if (IS_PARAM("h") || IS_PARAM("help")) {
            builder.options.print_help = true;
        } else if (IS_PARAM("a") || IS_PARAM("about")) {
            builder.options.print_about = true;
        } else if (IS_PARAM("b") || IS_PARAM("build")) {
            builder.options.use_pipeline = true;
        } else if (IS_PARAM("lex-dump")) {
            builder.options.print_tokens = true;
        } else if (IS_PARAM("syntax-only")) {
            builder.options.syntax_only = true;
        } else if (IS_PARAM("emit-llvm")) {
            builder.options.emit_llvm = true;
        } else if (IS_PARAM("emit-mir")) {
            builder.options.emit_mir = true;
        } else if (IS_PARAM("r") || IS_PARAM("run")) {
            builder.options.run = true;
        } else if (IS_PARAM("rt") || IS_PARAM("run-tests")) {
            builder.options.run_tests = true;
        } else if (IS_PARAM("no-bin")) {
            builder.options.no_bin = true;
        } else if (IS_PARAM("no-warning")) {
            builder.options.no_warn = true;
        } else if (IS_PARAM("verbose")) {
            builder.options.verbose = true;
        } else if (IS_PARAM("no-color")) {
            builder.options.no_color = true;
        } else if (IS_PARAM("no-api")) {
            builder.options.no_api = true;
        } else if (IS_PARAM("no-analyze")) {
            builder.options.no_analyze = true;
        } else if (IS_PARAM("no-llvm")) {
            builder.options.no_llvm = true;
        } else if (IS_PARAM("configure")) {
            builder.options.run_configure = true;
        } else if (IS_PARAM("reg-split-on")) {
            builder.options.reg_split = true;
        } else if (IS_PARAM("reg-split-off")) {
            builder.options.reg_split = false;
        } else if (IS_PARAM("release-fast")) {
            builder.options.build_mode = BUILD_MODE_RELEASE_FAST;
        } else if (IS_PARAM("release-small")) {
            builder.options.build_mode = BUILD_MODE_RELEASE_SMALL;
        } else if (IS_PARAM("di-dwarf")) {
            builder.options.build_di_kind = BUILD_DI_DWARF;
        } else if (IS_PARAM("di-codeview")) {
            builder.options.build_di_kind = BUILD_DI_CODEVIEW;
        } else if (IS_PARAM("no-vcvars")) {
            builder.options.no_vcvars = true;
        } else {
            builder_error("invalid params '%s'", &argv[optind][1]);
            return -1;
        }
    }
    argv += optind;

#if !defined(BL_PLATFORM_WIN)
    if (builder.options.no_vcvars) {
        builder_warning("Ignore parameter '-no-vcvars', this is valid on Windows only!");
    }
#endif

    return optind;
#undef IS_PARAM
}

void builder_init(void)
{
    memset(&builder, 0, sizeof(Builder));
    builder.errorc = builder.max_error = builder.test_failc = 0;

    conf_data_init(&builder.conf);
    arena_init(&builder.str_cache, sizeof(TString), 256, (ArenaElemDtor)str_cache_dtor);

    // TODO: this is invalid for Windows MSVC DLLs???
#if defined(BL_PLATFORM_MACOS) || defined(BL_PLATFORM_LINUX)
    builder.options.reg_split = true;
#else
    builder.options.reg_split = false;
#endif

    // initialize LLVM statics
    llvm_init();
    tarray_init(&builder.assembly_queue, sizeof(Assembly *));
    tsa_init(&builder.message_handlers);
    // tsa_push_MessageHandler(&builder.message_handlers, puml_message_handler);
}

void builder_terminate(void)
{
    tsa_terminate(&builder.message_handlers);
    Assembly *assembly;
    TARRAY_FOREACH(Assembly *, &builder.assembly_queue, assembly)
    {
        assembly_delete(assembly);
    }

    tarray_terminate(&builder.assembly_queue);
    conf_data_terminate(&builder.conf);
    arena_terminate(&builder.str_cache);

    llvm_terminate();
}

int builder_compile_config(const char *filepath, ConfData *out_data)
{
    Unit *unit = unit_new_file(filepath, NULL, NULL);
    // load
    file_loader_run(unit);
    INTERRUPT_ON_ERROR;
    // use standart lexer
    lexer_run(unit);
    INTERRUPT_ON_ERROR;
    conf_parser_run(unit, out_data);
    INTERRUPT_ON_ERROR;
    unit_delete(unit);
    return COMPILE_OK;
INTERRUPT:
    return COMPILE_FAIL;
}

int builder_load_config(const char *filepath)
{
    conf_data_clear(&builder.conf);
    return builder_compile_config(filepath, &builder.conf);
}

void builder_add_assembly(Assembly *assembly)
{
    if (!assembly) return;
    tarray_push(&builder.assembly_queue, assembly);
}

int builder_compile_all(void)
{
    Assembly *assembly;
    TARRAY_FOREACH(Assembly *, &builder.assembly_queue, assembly)
    {
        s32 state = builder_compile(assembly);
        if (state != COMPILE_OK) return state;
    }

    return COMPILE_OK;
}

int builder_compile(Assembly *assembly)
{
    clock_t begin = clock();
    Unit *  unit;
    s32     state       = COMPILE_OK;
    builder.total_lines = 0;

    builder_note("Compile assembly: %s [%s]",
                 assembly->name,
                 build_mode_to_str(assembly->options.build_mode));

    // This will apply all modification to build mode, target platform, etc. made on assembly
    // instance during initialization process. (Must be called only once);
    assembly_apply_options(assembly);

    unit = unit_new_file(BUILTIN_FILE, NULL, NULL);
    if (!assembly_add_unit_unique(assembly, unit)) {
        unit_delete(unit);
    }

    // include core source file
    if (!builder.options.no_api) {
        unit = unit_new_file(OS_PRELOAD_FILE, NULL, NULL);
        if (!assembly_add_unit_unique(assembly, unit)) {
            unit_delete(unit);
        }
    }

    if (assembly->options.build_mode == BUILD_MODE_BUILD) {
        unit = unit_new_file(BUILD_API_FILE, NULL, NULL);
        if (!assembly_add_unit_unique(assembly, unit)) {
            unit_delete(unit);
        }
    }

    TARRAY_FOREACH(Unit *, &assembly->units, unit)
    {
        // IDEA: can run in separate thread
        if ((state = compile_unit(unit, assembly)) != COMPILE_OK) {
            break;
        }
    }

    if (state == COMPILE_OK) state = compile_assembly(assembly);

    clock_t end        = clock();
    f64     time_spent = (f64)(end - begin) / CLOCKS_PER_SEC;

    builder_log("Compiled %i lines in %f seconds.", builder.total_lines, time_spent);
    if (state != COMPILE_OK) {
        builder_warning("There were errors, sorry...");
    }

    // We return count of failed compile time test cases as state even if there was no
    // compilation errors. Compilation errors has priority here.
    return builder.errorc ? builder.max_error : builder.test_failc;
}

void builder_msg(BuilderMsgType type,
                 s32            code,
                 Location *     src,
                 BuilderCurPos  pos,
                 const char *   format,
                 ...)
{
    if (type == BUILDER_MSG_ERROR && builder.errorc > MAX_ERROR_REPORTED) return;
    if (type == BUILDER_MSG_LOG && !builder.options.verbose) return;
    if (builder.options.no_warn && type == BUILDER_MSG_WARNING) return;

    TString tmp;

    tstring_init(&tmp);
    char msg[MAX_MSG_LEN] = {0};

    if (src) {
        s32 line = src->line;
        s32 col  = src->col;
        s32 len  = src->len;

        switch (pos) {
        case BUILDER_CUR_AFTER:
            col += len;
            len = 1;
            break;

        case BUILDER_CUR_WORD:
            break;

        case BUILDER_CUR_BEFORE:
            col -= col < 1 ? 0 : 1;
            len = 1;
            break;

        case BUILDER_CUR_NONE:
            break;
        }

        snprintf(msg, MAX_MSG_LEN, "%s:%d:%d ", src->unit->filepath, line, col);

        va_list args;
        va_start(args, format);
        vsnprintf(msg + strlen(msg), MAX_MSG_LEN - strlen(msg), format, args);
        va_end(args);

        tstring_append(&tmp, &msg[0]);

        s32         pad      = sprintf(msg, "%d", src->line) + 2;
        long        line_len = 0;
        const char *line_str = unit_get_src_ln(src->unit, src->line - 1, &line_len);
        if (line_str && line_len) {
            sprintf(msg, "\n%*d", pad, src->line - 1);
            tstring_append(&tmp, &msg[0]);
            tstring_append(&tmp, " | ");
            tstring_append_n(&tmp, line_str, line_len);
        }

        line_str = unit_get_src_ln(src->unit, src->line, &line_len);
        if (line_str && line_len) {
            sprintf(msg, "\n>%*d", pad - 1, src->line);
            tstring_append(&tmp, &msg[0]);
            tstring_append(&tmp, " | ");
            tstring_append_n(&tmp, line_str, line_len);
        }

        if (pos != BUILDER_CUR_NONE) {
            sprintf(msg, "\n%*s", pad, "");
            tstring_append(&tmp, &msg[0]);
            tstring_append(&tmp, " | ");

            for (s32 i = 0; i < col + len - 1; ++i) {
                if (i < col - 1)
                    tstring_append(&tmp, " ");
                else
                    tstring_append(&tmp, "^");
            }
        }

        sprintf(msg, "\n%*d", pad, src->line + 1);
        tstring_append(&tmp, &msg[0]);
        tstring_append(&tmp, " | ");

        line_str = unit_get_src_ln(src->unit, src->line + 1, &line_len);
        if (line_str && line_len) {
            tstring_append_n(&tmp, line_str, line_len);
        }
    } else {
        va_list args;
        va_start(args, format);
        vsnprintf(msg + strlen(msg), MAX_MSG_LEN - strlen(msg), format, args);
        va_end(args);
        tstring_append(&tmp, &msg[0]);
    }

    switch (type) {
    case BUILDER_MSG_ERROR: {
        builder.errorc++;
        builder.max_error = code > builder.max_error ? code : builder.max_error;
        color_print(stderr, BL_RED, tmp.data);
        break;
    }
    case BUILDER_MSG_WARNING: {
        color_print(stdout, BL_YELLOW, tmp.data);
        break;
    }

    default: {
        color_print(stdout, BL_NO_COLOR, tmp.data);
    }
    }

    tstring_terminate(&tmp);

#if ASSERT_ON_CMP_ERROR
    if (type == BUILDER_MSG_ERROR) BL_ASSERT(false);
#endif
}

TString *builder_create_cached_str(void)
{
    TString *str = arena_alloc(&builder.str_cache);
    tstring_init(str);

    return str;
}

void builder_invoke_message(const Assembly *assembly, const BuilderMessage *msg)
{
    if (!msg) return;
    if (builder.message_handlers.size == 0) return;
    BuilderMessageHandler it;
    TSA_FOREACH(&builder.message_handlers, it)
    {
        it(assembly, msg);
    }
}
