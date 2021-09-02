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

#define MAX_ERROR_REPORTED 10
#define MAX_THREAD 8

struct builder builder;

struct id builtin_ids[_BUILTIN_ID_COUNT] = {
#define GEN_BUILTIN_IDS
#include "builtin.inc"
#undef GEN_BUILTIN_IDS
};

// =================================================================================================
// Stages
// =================================================================================================
void conf_parser_run(struct unit *unit, conf_data_t *out_data);

void file_loader_run(struct assembly *assembly, struct unit *unit);
void lexer_run(struct assembly *assembly, struct unit *unit);
void token_printer_run(struct assembly *assembly, struct unit *unit);
void parser_run(struct assembly *assembly, struct unit *unit);
void ast_printer_run(struct assembly *assembly);
void docs_run(struct assembly *assembly);
void ir_run(struct assembly *assembly);
void ir_opt_run(struct assembly *assembly);
void obj_writer_run(struct assembly *assembly);
void linker_run(struct assembly *assembly);
void bc_writer_run(struct assembly *assembly);
void native_bin_run(struct assembly *assembly);
void mir_writer_run(struct assembly *assembly);

// Virtual Machine
void vm_entry_run(struct assembly *assembly);
void vm_build_entry_run(struct assembly *assembly);
void vm_tests_run(struct assembly *assembly);

// =================================================================================================
// Builder
// =================================================================================================
static int  compile_unit(struct unit *unit, struct assembly *assembly, unit_stage_fn_t *pipeline);
static int  compile_assembly(struct assembly *assembly, assembly_stage_fn_t *pipeline);
static bool llvm_initialized = false;

// =================================================================================================
// Threading
// =================================================================================================
struct threading_impl {
    struct assembly *assembly;
    pthread_t        workers[MAX_THREAD];
    TArray           queue;
    volatile s32     active;       // count of currently active workers
    volatile s32     will_exit;    // true when main thread will exit
    volatile bool    is_compiling; // true when async compilation is running

    pthread_mutex_t str_tmp_lock;
    pthread_mutex_t log_mutex;
    pthread_mutex_t queue_mutex;
    pthread_cond_t  queue_condition;
    pthread_mutex_t active_mutex;
    pthread_cond_t  active_condition;

    unit_stage_fn_t *unit_pipeline;
};

static void async_push(struct unit *unit)
{
    struct threading_impl *threading = builder.threading;
    pthread_mutex_lock(&threading->queue_mutex);
    tarray_push(&threading->queue, unit);
    if (threading->is_compiling) pthread_cond_signal(&threading->queue_condition);
    pthread_mutex_unlock(&threading->queue_mutex);
}

static struct unit *async_pop_unsafe(void)
{
    struct threading_impl *threading = builder.threading;
    struct unit *          unit      = NULL;
    if (threading->queue.size) {
        unit = tarray_at(struct unit *, &threading->queue, threading->queue.size - 1);
        tarray_pop(&threading->queue);
    }
    return unit;
}

static struct threading_impl *threading_new(void)
{
    struct threading_impl *t = bl_malloc(sizeof(struct threading_impl));
    memset(t, 0, sizeof(struct threading_impl));
    pthread_mutex_init(&t->str_tmp_lock, NULL);
    pthread_mutex_init(&t->queue_mutex, NULL);
    pthread_mutex_init(&t->active_mutex, NULL);
    pthread_mutex_init(&t->log_mutex, NULL);
    pthread_cond_init(&t->queue_condition, NULL);
    pthread_cond_init(&t->active_condition, NULL);
    tarray_init(&t->queue, sizeof(struct unit *));
    return t;
}

static void threading_delete(struct threading_impl *t)
{
    struct threading_impl *threading = builder.threading;
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
    struct threading_impl *threading = builder.threading;
    while (true) {
        pthread_mutex_lock(&threading->queue_mutex);
        struct unit *unit;
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

        compile_unit(unit, threading->assembly, threading->unit_pipeline);

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
    struct threading_impl *threading = builder.threading;
    for (usize i = 0; i < TARRAY_SIZE(threading->workers); ++i) {
        pthread_create(&threading->workers[i], NULL, &worker, NULL);
    }
}

static void async_compile(struct assembly *assembly, unit_stage_fn_t *unit_pipeline)
{
    struct threading_impl *threading = builder.threading;

    struct unit *unit;
    TARRAY_FOREACH(struct unit *, &assembly->units, unit)
    {
        async_push(unit);
    }

    threading->assembly      = assembly;
    threading->unit_pipeline = unit_pipeline;
    threading->active        = 0;
    threading->is_compiling  = true;
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

// =================================================================================================
// Builder
// =================================================================================================
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

    LLVMInitializeAArch64Target();
    LLVMInitializeAArch64TargetInfo();
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeAArch64AsmPrinter();

    llvm_initialized = true;
}

static void llvm_terminate(void)
{
    LLVMShutdown();
}

int compile_unit(struct unit *unit, struct assembly *assembly, unit_stage_fn_t *pipeline)
{
    BL_ASSERT(pipeline && "Invalid unit pipeline!");
    if (unit->loaded_from) {
        builder_log(
            "Compile: %s (loaded from '%s')", unit->name, unit->loaded_from->location.unit->name);
    } else {
        builder_log("Compile: %s", unit->name);
    }
    s32             i     = 0;
    unit_stage_fn_t stage = NULL;
    while ((stage = pipeline[i++])) {
        stage(assembly, unit);
        if (builder.errorc) return COMPILE_FAIL;
    }
    return COMPILE_OK;
}

int compile_assembly(struct assembly *assembly, assembly_stage_fn_t *pipeline)
{
    BL_ASSERT(assembly);
    BL_ASSERT(pipeline && "Invalid assembly pipeline!");
    s32                 i     = 0;
    assembly_stage_fn_t stage = NULL;
    while ((stage = pipeline[i++])) {
        stage(assembly);
        if (builder.errorc) {
            return COMPILE_FAIL;
        }
    }
    return COMPILE_OK;
}

static void entry_run(struct assembly *assembly)
{
    vm_entry_run(assembly);
    builder.last_script_mode_run_status = assembly->vm_run.last_execution_status;
}

static void build_entry_run(struct assembly *assembly)
{
    vm_build_entry_run(assembly);
}

static void tests_run(struct assembly *assembly)
{
    vm_tests_run(assembly);
    builder.test_failc = assembly->vm_run.last_execution_status;
}

#define STAGE(i, fn)                                                                               \
    {                                                                                              \
        BL_ASSERT(i < stage_count - 1 && "Stage out of bounds!");                                  \
        stages[i++] = fn;                                                                          \
    }

static void setup_unit_pipeline(struct assembly *assembly, unit_stage_fn_t *stages, s32 stage_count)
{

    const struct target *t = assembly->target;

    s32 index = 0;
    memset(stages, 0, stage_count * sizeof(unit_stage_fn_t));
    STAGE(index, &file_loader_run);
    STAGE(index, &lexer_run);
    if (t->print_tokens) STAGE(index, &token_printer_run);
    STAGE(index, &parser_run);
}

static void
setup_assembly_pipeline(struct assembly *assembly, assembly_stage_fn_t *stages, s32 stage_count)
{
    const struct target *t = assembly->target;

    s32 index = 0;
    memset(stages, 0, stage_count * sizeof(assembly_stage_fn_t));
    if (t->print_ast) STAGE(index, &ast_printer_run);
    if (t->kind == ASSEMBLY_DOCS) {
        STAGE(index, &docs_run);
        return;
    }
    if (t->syntax_only) return;
    STAGE(index, &linker_run);
    STAGE(index, &mir_run);
    if (t->run) STAGE(index, &entry_run);
    if (t->kind == ASSEMBLY_BUILD_PIPELINE) STAGE(index, build_entry_run);
    if (t->run_tests) STAGE(index, tests_run);
    if (t->emit_mir) STAGE(index, &mir_writer_run);
    if (t->no_analyze) return;
    if (t->no_llvm) return;
    if (t->kind == ASSEMBLY_BUILD_PIPELINE) return;
    STAGE(index, &ir_run);
    STAGE(index, &ir_opt_run);
    if (t->emit_llvm) STAGE(index, &bc_writer_run);
    if (t->no_bin) return;
    STAGE(index, &obj_writer_run);
    STAGE(index, &native_bin_run);
}

#undef STAGE

static void print_stats(struct assembly *assembly)
{
    const f64 total_s = assembly->stats.parsing_lexing_s + assembly->stats.mir_s +
                        assembly->stats.llvm_s + assembly->stats.linking_s;

    builder_note(
        "Compiled: %s\n"
        "--------------------------------------------------------------------------------\n"
        "Lexing & Parsing: %10.3f seconds    %3.0f%%\n"
        "MIR:              %10.3f seconds    %3.0f%%\n"
        "LLVM IR:          %10.3f seconds    %3.0f%%\n"
        "Linking:          %10.3f seconds    %3.0f%%\n\n"
        "Polymorph:        %10lld generated in %.3f seconds\n"
        "--------------------------------------------------------------------------------\n"
        "Total:            %10.3f seconds\n"
        "Lines:              %8d\n"
        "Speed:            %10.0f lines/second\n",
        assembly->target->name,
        assembly->stats.parsing_lexing_s,
        assembly->stats.parsing_lexing_s / total_s * 100.,
        assembly->stats.mir_s,
        assembly->stats.mir_s / total_s * 100.,
        assembly->stats.llvm_s,
        assembly->stats.llvm_s / total_s * 100.,
        assembly->stats.linking_s,
        assembly->stats.linking_s / total_s * 100.,
        assembly->stats.polymorph_count,
        assembly->stats.polymorph_s,
        total_s,
        builder.total_lines,
        ((f64)builder.total_lines) / total_s);
}

static void clear_stats(struct assembly *assembly)
{
    memset(&assembly->stats, 0, sizeof(assembly->stats));
}

static int compile(struct assembly *assembly)
{
    s32 state           = COMPILE_OK;
    builder.total_lines = 0;

    unit_stage_fn_t     unit_pipeline[5];
    assembly_stage_fn_t assembly_pipeline[14];
    setup_unit_pipeline(assembly, unit_pipeline, TARRAY_SIZE(unit_pipeline));
    setup_assembly_pipeline(assembly, assembly_pipeline, TARRAY_SIZE(assembly_pipeline));

    if (builder.options->no_jobs) {
        BL_LOG("Running in single thread mode!");
        struct unit *unit;
        TARRAY_FOREACH(struct unit *, &assembly->units, unit)
        {
            if ((state = compile_unit(unit, assembly, unit_pipeline)) != COMPILE_OK) break;
        }
    } else {
        // Compile units in parallel.
        RUNTIME_MEASURE_BEGIN_S(process_unit);
        async_compile(assembly, unit_pipeline);
        assembly->stats.parsing_lexing_s = RUNTIME_MEASURE_END_S(process_unit);
    }
    // Compile assembly using pipeline.
    if (state == COMPILE_OK) state = compile_assembly(assembly, assembly_pipeline);

    if (state != COMPILE_OK) {
        if (assembly->target->kind == ASSEMBLY_BUILD_PIPELINE) {
            builder_error("Build pipeline failed.");
        } else {
            builder_error("Compilation of target '%s' failed.", assembly->target->name);
        }
    }
    if (builder.options->time_report && assembly->target->kind != ASSEMBLY_BUILD_PIPELINE) {
        print_stats(assembly);
    }
    clear_stats(assembly);
    if (builder.errorc) return builder.max_error;
    if (assembly->target->run) return builder.last_script_mode_run_status;
    if (assembly->target->run_tests) return builder.test_failc;
    return EXIT_SUCCESS;
}

// =================================================================================================
// PUBLIC
// =================================================================================================
void builder_init(const struct builder_options *options, const char *exec_dir)
{
    BL_ASSERT(options && "Invalid builder options!");
    BL_ASSERT(exec_dir && "Invalid executable directory!");
    memset(&builder, 0, sizeof(struct builder));
    builder.threading = threading_new();
    builder.options   = options;
    builder.errorc = builder.max_error = builder.test_failc = 0;
    builder.last_script_mode_run_status                     = 0;

    builder.exec_dir = strdup(exec_dir);

    conf_data_init(&builder.conf);
    arena_init(&builder.str_cache,
               sizeof(TString),
               alignment_of(TString),
               1024,
               (arena_elem_dtor_t)str_cache_dtor);

    // initialize LLVM statics
    llvm_init();
    tarray_init(&builder.targets, sizeof(struct target *));
    tarray_init(&builder.tmp_strings, sizeof(TString *));
    start_threads();
    // Generate hashes for builtin ids.
    for (s32 i = 0; i < _BUILTIN_ID_COUNT; ++i) {
        builtin_ids[i].hash = thash_from_str(builtin_ids[i].str);
    }
    builder.is_initialized = true;
}

void builder_terminate(void)
{
    struct target *target;
    TARRAY_FOREACH(struct target *, &builder.targets, target)
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
    BL_ASSERT(builder.lib_dir && "Library directory not set, call 'builder_set_lib_dir' first.");
    return builder.lib_dir;
}

const char *builder_get_exec_dir(void)
{
    BL_ASSERT(builder.exec_dir && "Executable directory not set, call 'builder_init' first.");
    return builder.exec_dir;
}

int builder_compile_config(const char *filepath, conf_data_t *out_data, struct token *import_from)
{
    struct unit *unit = unit_new(filepath, import_from);
    file_loader_run(NULL, unit);
    if (builder.errorc) goto INTERRUPT;
    lexer_run(NULL, unit);
    if (builder.errorc) goto INTERRUPT;
    conf_parser_run(unit, out_data);
    if (builder.errorc) goto INTERRUPT;
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

struct target *_builder_add_target(const char *name, bool is_default)
{
    struct target *target = NULL;
    if (is_default) {
        target = target_new(name);
        if (!target_init_default_triple(&target->triple)) {
            exit(ERR_UNSUPPORTED_TARGET);
        }
        builder.default_target = target;
    } else {
        target = target_dup(name, builder.default_target);
    }
    BL_ASSERT(target);
    tarray_push(&builder.targets, target);
    return target;
}

int builder_compile_all(void)
{
    s32            state = COMPILE_OK;
    struct target *target;
    TARRAY_FOREACH(struct target *, &builder.targets, target)
    {
        if (target->kind == ASSEMBLY_BUILD_PIPELINE) continue;
        state = builder_compile(target);
        if (state != COMPILE_OK) break;
    }
    return state;
}

s32 builder_compile(const struct target *target)
{
    BL_MAGIC_ASSERT(target);
    struct assembly *assembly = assembly_new(target);

    s32 state = compile(assembly);

    assembly_delete(assembly);
    return state;
}

void builder_msg(enum builder_msg_type type,
                 s32                   code,
                 struct location *     src,
                 enum builder_cur_pos  pos,
                 const char *          format,
                 ...)
{
    struct threading_impl *threading = builder.threading;
    pthread_mutex_lock(&threading->log_mutex);
    if (type == BUILDER_MSG_ERROR && builder.errorc > MAX_ERROR_REPORTED) goto DONE;
    if (type == BUILDER_MSG_LOG && !builder.options->verbose) goto DONE;
    if (type != BUILDER_MSG_ERROR && builder.options->silent) goto DONE;
    if (builder.options->no_warning && type == BUILDER_MSG_WARNING) goto DONE;

    if (type == BUILDER_MSG_ERROR) {
        builder.errorc++;
        builder.max_error = code > builder.max_error ? code : builder.max_error;
    }

    char  msg[1024];
    FILE *stream = stdout;
    if (type == BUILDER_MSG_ERROR) stream = stderr;

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

        const char *filepath =
            builder.options->full_path_reports ? src->unit->filepath : src->unit->filename;
        fprintf(stream, "%s:%d:%d: ", filepath, line, col);
        switch (type) {
        case BUILDER_MSG_ERROR: {
            color_print(stream, BL_RED, "error: ");
            break;
        }
        case BUILDER_MSG_WARNING: {
            color_print(stream, BL_YELLOW, "warning: ");
            break;
        }

        default:
            break;
        }

        va_list args;
        va_start(args, format);
        vsnprintf(msg, TARRAY_SIZE(msg), format, args);
        va_end(args);
        fprintf(stream, "%s", msg);

        long      line_len = 0;
        const s32 padding  = snprintf(NULL, 0, "%+d", src->line) + 2;

        // Line one
        const char *line_str = unit_get_src_ln(src->unit, src->line - 1, &line_len);
        if (line_str && line_len) {
            fprintf(stream, "\n%*d | %.*s", padding, src->line - 1, (int)line_len, line_str);
        }

        // Line two
        line_str = unit_get_src_ln(src->unit, src->line, &line_len);
        if (line_str && line_len) {
            fprintf(stream, "\n>%*d | %.*s", padding - 1, src->line, (int)line_len, line_str);
        }
        // Line cursors
        if (pos != BUILDER_CUR_NONE) {
            s32 written_bytes = 0;
            for (s32 i = 0; i < col + len - 1; ++i) {
                written_bytes += snprintf(msg + written_bytes,
                                          TARRAY_SIZE(msg) - written_bytes,
                                          "%s",
                                          i >= col - 1 ? "^" : " ");
            }
            fprintf(stream, "\n%*s | ", padding, "");
            color_print(stream, BL_GREEN, "%s", msg);
        }

        // Line three
        line_str = unit_get_src_ln(src->unit, src->line + 1, &line_len);
        if (line_str && line_len) {
            fprintf(stream, "\n%*d | %.*s", padding, src->line + 1, (int)line_len, line_str);
        }
        fprintf(stream, "\n\n");
    } else {
        switch (type) {
        case BUILDER_MSG_ERROR: {
            color_print(stream, BL_RED, "error: ");
            break;
        }
        case BUILDER_MSG_WARNING: {
            color_print(stream, BL_YELLOW, "warning: ");
            break;
        }
        default:
            break;
        }
        va_list args;
        va_start(args, format);
        vsnprintf(msg, TARRAY_SIZE(msg), format, args);
        va_end(args);
        fprintf(stream, "%s\n", msg);
    }
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
    struct threading_impl *threading = builder.threading;
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
    struct threading_impl *threading = builder.threading;
    pthread_mutex_lock(&threading->str_tmp_lock);
    tarray_push(&builder.tmp_strings, str);
    pthread_mutex_unlock(&threading->str_tmp_lock);
}

void builder_async_submit_unit(struct unit *unit)
{
    BL_ASSERT(unit);
    if (builder.options->no_jobs) return;
    struct threading_impl *threading = builder.threading;
    if (!threading->is_compiling) return;

    async_push(unit);
}
