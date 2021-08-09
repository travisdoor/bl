// =================================================================================================
// bl
//
// File:   builder.h
// Author: Martin Dorazil
// Date:   02/03/2018
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

#ifndef BL_BUILDER_H
#define BL_BUILDER_H

#include "assembly.h"
#include "conf_data.h"
#include "error.h"
#include "mir.h"

#define COMPILE_OK 0
#define COMPILE_FAIL 1

struct threading_impl;

typedef void (*unit_stage_fn_t)(struct assembly *, struct unit *);
typedef void (*assembly_stage_fn_t)(struct assembly *);

struct builder_options {
    bool verbose;
    bool no_color;
    bool silent;
    bool no_jobs;
    bool no_warning;
    bool full_path_reports;
    bool no_usage_check;
    bool time_report;
};

struct builder {
    const struct builder_options *options;
    const struct target *         default_target;
    char *                        exec_dir;
    char *                        lib_dir;
    struct arena                  str_cache;
    volatile s32                  total_lines;
    s32                           errorc;
    s32                           max_error;
    s32                           test_failc;
    s32                           last_script_mode_run_status;
    conf_data_t                   conf;
    TArray                        targets;
    TArray                        tmp_strings;
    struct threading_impl *       threading;
    bool                          is_initialized;
};

// struct builder global instance.
extern struct builder builder;

enum builder_msg_type {
    BUILDER_MSG_ERROR,
    BUILDER_MSG_WARNING,
    BUILDER_MSG_NOTE,
    BUILDER_MSG_LOG,
};

enum builder_cur_pos { BUILDER_CUR_AFTER, BUILDER_CUR_WORD, BUILDER_CUR_BEFORE, BUILDER_CUR_NONE };

struct location;

// Initialize builder global instance with executable directory specified.
void        builder_init(const struct builder_options *options, const char *exec_dir);
void        builder_terminate(void);
void        builder_set_lib_dir(const char *lib_dir);
const char *builder_get_lib_dir(void);
const char *builder_get_exec_dir(void);
int         builder_load_config(const char *filepath);
int builder_compile_config(const char *filepath, conf_data_t *out_data, struct token *import_from);

#define builder_add_target(name) _builder_add_target(name, false)
#define builder_add_default_target(name) _builder_add_target(name, true)
struct target *_builder_add_target(const char *name, bool is_default);

s32 builder_compile_all(void);
s32 builder_compile(const struct target *target);

// Submit new unit for async compilation, in case no-jobs flag is set, this function does nothing.
void builder_async_submit_unit(struct unit *unit);

#define builder_log(format, ...)                                                                   \
    builder_msg(BUILDER_MSG_LOG, -1, NULL, BUILDER_CUR_NONE, format, ##__VA_ARGS__)
#define builder_note(format, ...)                                                                  \
    builder_msg(BUILDER_MSG_NOTE, -1, NULL, BUILDER_CUR_NONE, format, ##__VA_ARGS__)
#define builder_warning(format, ...)                                                               \
    builder_msg(BUILDER_MSG_WARNING, -1, NULL, BUILDER_CUR_NONE, format, ##__VA_ARGS__)
#define builder_error(format, ...)                                                                 \
    builder_msg(BUILDER_MSG_ERROR, -1, NULL, BUILDER_CUR_NONE, format, ##__VA_ARGS__)

void builder_msg(enum builder_msg_type type,
                 s32                   code,
                 struct location *     src,
                 enum builder_cur_pos  pos,
                 const char *          format,
                 ...);

TString *builder_create_cached_str(void);
TString *get_tmpstr(void);
void     put_tmpstr(TString *str);

#endif
