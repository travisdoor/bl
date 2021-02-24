// =================================================================================================
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
// =================================================================================================

#include <stdarg.h>
#include <time.h>

#include "assembly.h"
#include "builder.h"
#include "common.h"
#include "token.h"
#include "unit.h"

#if BL_PLATFORM_WIN
#include "winpthreads.h"
#include <windows.h>
#define sleep_ms(ms) Sleep(ms)
#else
#include <pthread.h>
#include <unistd.h>
#define sleep_ms(ms) usleep(ms * 1000)
#endif

#define MAX_MSG_LEN 1024
#define MAX_ERROR_REPORTED 10
#define MAX_THREAD 8

Builder builder;

// =================================================================================================
// Stages
// =================================================================================================
void conf_parser_run(Unit *unit, ConfData *out_data);

// Unit
void file_loader_run(Assembly *assembly, Unit *unit);
void lexer_run(Assembly *assembly, Unit *unit);
void token_printer_run(Assembly *assembly, Unit *unit);
void parser_run(Assembly *assembly, Unit *unit);

// Assembly
void ast_printer_run(Assembly *assembly, FILE *stream);
void docs_run(Assembly *assembly);
void ir_run(Assembly *assembly);
void ir_opt_run(Assembly *assembly);
void obj_writer_run(Assembly *assembly);
void linker_run(Assembly *assembly);
void bc_writer_run(Assembly *assembly);
void native_bin_run(Assembly *assembly);
void mir_writer_run(Assembly *assembly);

// VM
s32 vm_entry_run(Assembly *assembly);
s32 vm_build_entry_run(Assembly *assembly);
s32 vm_tests_run(Assembly *assembly);

// =================================================================================================
// Builder
// =================================================================================================
static int  compile_unit(Unit *unit, Assembly *assembly);
static int  compile_assembly(Assembly *assembly);
static bool llvm_initialized = false;

typedef struct ThreadingImpl {
    Assembly *    assembly;
    pthread_t     workers[MAX_THREAD];
    TArray        queue;
    volatile s32  active;       // count of currently active workers
    volatile s32  will_exit;    // true when main thread will exit
    volatile bool is_compiling; // true when async compilation is running

    pthread_mutex_t str_tmp_lock;
    pthread_mutex_t log_mutex;
    pthread_mutex_t queue_mutex;
    pthread_cond_t  queue_condition;
    pthread_mutex_t active_mutex;
    pthread_cond_t  active_condition;
} ThreadingImpl;

static void async_push(Unit *unit)
{
    ThreadingImpl *threading = builder.threading;
    pthread_mutex_lock(&threading->queue_mutex);
    tarray_push(&threading->queue, unit);
    if (threading->is_compiling) pthread_cond_signal(&threading->queue_condition);
    pthread_mutex_unlock(&threading->queue_mutex);
}

static Unit *async_pop_unsafe(void)
{
    ThreadingImpl *threading = builder.threading;
    Unit *         unit      = NULL;
    if (threading->queue.size) {
        unit = tarray_at(Unit *, &threading->queue, threading->queue.size - 1);
        tarray_pop(&threading->queue);
    }
    return unit;
}

static ThreadingImpl *threading_new(void)
{
    ThreadingImpl *t = bl_malloc(sizeof(ThreadingImpl));
    memset(t, 0, sizeof(ThreadingImpl));
    pthread_mutex_init(&t->str_tmp_lock, NULL);
    pthread_mutex_init(&t->queue_mutex, NULL);
    pthread_mutex_init(&t->active_mutex, NULL);
    pthread_mutex_init(&t->log_mutex, NULL);
    pthread_cond_init(&t->queue_condition, NULL);
    pthread_cond_init(&t->active_condition, NULL);
    tarray_init(&t->queue, sizeof(Unit *));
    return t;
}

static void threading_delete(ThreadingImpl *t)
{
    ThreadingImpl *threading = builder.threading;
    pthread_mutex_lock(&threading->queue_mutex);
    threading->will_exit = true;
    pthread_cond_broadcast(&threading->queue_condition);
    pthread_mutex_unlock(&threading->queue_mutex);
    for (usize i = 0; i < TARRAY_SIZE(t->workers); ++i) {
        pthread_join(t->workers[i], NULL);
    }
    pthread_mutex_destroy(&t->queue_mutex);
    pthread_mutex_destroy(&t->active_mutex);
    pthread_mutex_destroy(&t->log_mutex);
    pthread_mutex_destroy(&t->str_tmp_lock);
    pthread_cond_destroy(&t->queue_condition);
    pthread_cond_destroy(&t->active_condition);
    tarray_terminate(&t->queue);
    bl_free(t);
}

static void *worker(void UNUSED(*args))
{
    ThreadingImpl *threading = builder.threading;
    while (true) {
        pthread_mutex_lock(&threading->queue_mutex);
        Unit *unit;
        while (!threading->is_compiling || !(unit = async_pop_unsafe())) {
            if (threading->will_exit) {
                pthread_mutex_unlock(&threading->queue_mutex);
                pthread_exit(NULL);
            }
            pthread_cond_wait(&threading->queue_condition, &threading->queue_mutex);
        }
        BL_ASSERT(unit);
        pthread_mutex_lock(&threading->active_mutex);
        threading->active++;
        pthread_mutex_unlock(&threading->active_mutex);
        pthread_mutex_unlock(&threading->queue_mutex);

        compile_unit(unit, threading->assembly);

        pthread_mutex_lock(&threading->active_mutex);
        threading->active--;
        pthread_cond_signal(&threading->active_condition);
        pthread_mutex_unlock(&threading->active_mutex);
    }
    pthread_exit(NULL);
    return NULL;
}

static void start_threads()
{
    ThreadingImpl *threading = builder.threading;
    for (usize i = 0; i < TARRAY_SIZE(threading->workers); ++i) {
        pthread_create(&threading->workers[i], NULL, &worker, NULL);
    }
}

static void async_compile(Assembly *assembly)
{
    ThreadingImpl *threading = builder.threading;

    Unit *unit;
    TARRAY_FOREACH(Unit *, &assembly->units, unit)
    {
        async_push(unit);
    }

    threading->assembly     = assembly;
    threading->active       = 0;
    threading->is_compiling = true;
    pthread_cond_broadcast(&threading->queue_condition);

    while (true) {
        pthread_mutex_lock(&threading->queue_mutex);
        pthread_mutex_lock(&threading->active_mutex);
        if (threading->active || threading->queue.size) {
            pthread_mutex_unlock(&threading->queue_mutex);
            pthread_cond_wait(&threading->active_condition, &threading->active_mutex);
        } else {
            pthread_mutex_unlock(&threading->queue_mutex);
            pthread_mutex_unlock(&threading->active_mutex);
            break;
        }
        pthread_mutex_unlock(&threading->active_mutex);
    }
    threading->is_compiling = false;
    // Eventually use asserts here when there will be no threading-related errors.
    if (threading->active) BL_ABORT("Not all units processed! (active)");
    if (threading->queue.size) BL_ABORT("Not all units processed! (queued)");
}

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

    file_loader_run(assembly, unit);
    INTERRUPT_ON_ERROR;
    lexer_run(assembly, unit);
    INTERRUPT_ON_ERROR;
    if (assembly->target->print_tokens) {
        token_printer_run(assembly, unit);
        INTERRUPT_ON_ERROR;
    }
    parser_run(assembly, unit);
    return COMPILE_OK;
INTERRUPT:
    return COMPILE_FAIL;
}

int compile_assembly(Assembly *assembly)
{
    if (assembly->target->print_ast) ast_printer_run(assembly, stdout);
    INTERRUPT_ON_ERROR;

    if (assembly->target->docs) docs_run(assembly);
    FINISH_IF(assembly->target->docs);

    linker_run(assembly);
    INTERRUPT_ON_ERROR;

    FINISH_IF(assembly->target->syntax_only);

    mir_run(assembly);
    if (assembly->target->emit_mir) mir_writer_run(assembly);
    INTERRUPT_ON_ERROR;
    FINISH_IF(assembly->target->no_analyze);

    //********************************************************************************************
    // EXECUTION
    //********************************************************************************************
    // Run main
    if (assembly->target->run) builder.last_script_mode_run_status = vm_entry_run(assembly);

    // Handle build mode
    if (assembly->target->kind == ASSEMBLY_BUILD_PIPELINE) vm_build_entry_run(assembly);

    // Run test cases
    if (assembly->target->run_tests) builder.test_failc = vm_tests_run(assembly);
    //********************************************************************************************

    FINISH_IF(assembly->target->no_llvm);
    FINISH_IF(assembly->target->kind == ASSEMBLY_BUILD_PIPELINE);
    ir_run(assembly);
    INTERRUPT_ON_ERROR;

    ir_opt_run(assembly);
    INTERRUPT_ON_ERROR;

    if (assembly->target->emit_llvm) {
        bc_writer_run(assembly);
        INTERRUPT_ON_ERROR;
    }

    if (!assembly->target->no_bin) {
        obj_writer_run(assembly);
        INTERRUPT_ON_ERROR;
        native_bin_run(assembly);
        INTERRUPT_ON_ERROR;
    }

FINISH:
    return COMPILE_OK;
INTERRUPT:
    return COMPILE_FAIL;
}

static int compile(Assembly *assembly)
{
    clock_t begin       = clock();
    s32     state       = COMPILE_OK;
    builder.total_lines = 0;

    builder_note("Compile assembly: %s [%s]", assembly->name, opt_to_str(assembly->target->opt));

    // Prepare assembly for compilation.
    assembly_prepare(assembly);
    if (!assembly->target->docs) assembly_add_unit(assembly, BUILTIN_FILE, NULL);

    // include core source file
    if (!assembly->target->no_api && !assembly->target->docs)
        assembly_add_unit(assembly, OS_PRELOAD_FILE, NULL);

    const bool build_mode = assembly->target->kind == ASSEMBLY_BUILD_PIPELINE;
    if (build_mode) assembly_add_unit(assembly, BUILD_API_FILE, NULL);

    if (builder.options.no_jobs) {
        Unit *unit;
        TARRAY_FOREACH(Unit *, &assembly->units, unit)
        {
            if ((state = compile_unit(unit, assembly)) != COMPILE_OK) break;
        }
    } else {
        async_compile(assembly);
    }

    if (state == COMPILE_OK) state = compile_assembly(assembly);

    clock_t end        = clock();
    f64     time_spent = (f64)(end - begin) / CLOCKS_PER_SEC;

    builder_log("Compiled %i lines in %f seconds.", builder.total_lines, time_spent);
    if (state != COMPILE_OK) {
        builder_warning("There were errors, sorry...");
    }

    // Cleanup assembly.
    assembly_cleanup(assembly);
    if (builder.errorc) return builder.max_error;
    if (assembly->target->run) return builder.last_script_mode_run_status;
    if (assembly->target->run_tests) return builder.test_failc;
    return EXIT_SUCCESS;
}

// =================================================================================================
// PUBLIC
// =================================================================================================
void builder_init(const char *exec_dir)
{
    BL_ASSERT(exec_dir);
    memset(&builder, 0, sizeof(Builder));
    builder.threading = threading_new();
    builder.errorc = builder.max_error = builder.test_failc = 0;
    builder.last_script_mode_run_status                     = 0;

    builder.exec_dir = strdup(exec_dir);

    conf_data_init(&builder.conf);
    arena_init(&builder.str_cache, sizeof(TString), 256, (ArenaElemDtor)str_cache_dtor);

    // initialize LLVM statics
    llvm_init();
    tarray_init(&builder.targets, sizeof(Target *));
    tarray_init(&builder.tmp_strings, sizeof(TString *));
    start_threads();
}

void builder_terminate(void)
{
    Target *target;
    TARRAY_FOREACH(Target *, &builder.targets, target)
    {
        target_delete(target);
    }
    tarray_terminate(&builder.targets);

    TString *str;
    TARRAY_FOREACH(TString *, &builder.tmp_strings, str)
    {
        tstring_delete(str);
    }
    BL_LOG("Used %llu temp-strings.", builder.tmp_strings.size);
    conf_data_terminate(&builder.conf);
    arena_terminate(&builder.str_cache);

    llvm_terminate();
    threading_delete(builder.threading);
    free(builder.exec_dir);
    free(builder.lib_dir);
}

void builder_set_lib_dir(const char *lib_dir)
{
    BL_ASSERT(lib_dir);
    BL_ASSERT(builder.lib_dir == NULL && "Library directory already set!");
    builder.lib_dir = strdup(lib_dir);
}

const char *builder_get_lib_dir(void)
{
    BL_ASSERT(builder.lib_dir && "Library directory not set, call 'builder_set_lib_dir' first.")
    return builder.lib_dir;
}

const char *builder_get_exec_dir(void)
{
    BL_ASSERT(builder.exec_dir && "Executable directory not set, call 'builder_init' first.")
    return builder.exec_dir;
}

int builder_compile_config(const char *filepath, ConfData *out_data, Token *import_from)
{
    Unit *unit = unit_new(filepath, import_from);
    // load
    file_loader_run(NULL, unit);
    INTERRUPT_ON_ERROR;
    // use standart lexer
    lexer_run(NULL, unit);
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
    return builder_compile_config(filepath, &builder.conf, NULL);
}

Target *builder_add_target(const char *name)
{
    Target *target = target_new(name);
    tarray_push(&builder.targets, target);
    return target;
}

int builder_compile_all(void)
{
    return COMPILE_OK;
}

s32 builder_compile(const Target *target)
{
    BL_ASSERT(target && "Invalid compilation target!");

    // @CLEANUP move to compile_assembly!!!
    // @CLEANUP move to compile_assembly!!!
    // @CLEANUP move to compile_assembly!!!
    UnitStageFn unit_stages[4] = {0};
    {
        s32 si = 0;

        unit_stages[si++] = &file_loader_run;
        unit_stages[si++] = &lexer_run;
        if (target->print_tokens) unit_stages[si++] = &token_printer_run;
        unit_stages[si++] = &parser_run;
    }

    Assembly *assembly = assembly_new(target);
    s32       state    = compile_assembly(assembly);
    assembly_delete(assembly);
    return state;
}

void builder_msg(BuilderMsgType type,
                 s32            code,
                 Location *     src,
                 BuilderCurPos  pos,
                 const char *   format,
                 ...)
{
    ThreadingImpl *threading = builder.threading;
    pthread_mutex_lock(&threading->log_mutex);
    if (type == BUILDER_MSG_ERROR && builder.errorc > MAX_ERROR_REPORTED) goto DONE;
    if (type == BUILDER_MSG_LOG && !builder.options.verbose) goto DONE;
    if (type != BUILDER_MSG_ERROR && builder.options.silent) goto DONE;
    if (builder.options.no_warn && type == BUILDER_MSG_WARNING) goto DONE;
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
        color_print(stderr, BL_RED, "%s", tmp.data);
        break;
    }
    case BUILDER_MSG_WARNING: {
        color_print(stdout, BL_YELLOW, "%s", tmp.data);
        break;
    }

    default: {
        color_print(stdout, BL_NO_COLOR, "%s", tmp.data);
    }
    }

    tstring_terminate(&tmp);
DONE:
    pthread_mutex_unlock(&threading->log_mutex);

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

TString *get_tmpstr(void)
{
    ThreadingImpl *threading = builder.threading;
    pthread_mutex_lock(&threading->str_tmp_lock);
    TString *   str  = NULL;
    const usize size = builder.tmp_strings.size;
    if (size) {
        str = tarray_at(TString *, &builder.tmp_strings, size - 1);
        tarray_pop(&builder.tmp_strings);
    } else {
        str = tstring_new();
        tstring_reserve(str, 256);
    }
    BL_ASSERT(str);
    pthread_mutex_unlock(&threading->str_tmp_lock);
    return str;
}

void put_tmpstr(TString *str)
{
    BL_ASSERT(str);
    tstring_clear(str);
    ThreadingImpl *threading = builder.threading;
    pthread_mutex_lock(&threading->str_tmp_lock);
    tarray_push(&builder.tmp_strings, str);
    pthread_mutex_unlock(&threading->str_tmp_lock);
}

void builder_async_submit_unit(Unit *unit)
{
    BL_ASSERT(unit);
    if (builder.options.no_jobs) return;
    ThreadingImpl *threading = builder.threading;
    if (!threading->is_compiling) return;

    async_push(unit);
}
