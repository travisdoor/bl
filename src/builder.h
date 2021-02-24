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

struct ThreadingImpl;

typedef void (*UnitStageFn)(Assembly *, Unit *);
typedef void (*AssemblyStageFn)(Assembly *);

typedef struct BuilderOptions {
    bool verbose;
    bool no_color;
    bool silent;
    bool no_jobs;
    bool no_warn;
} BuilderOptions;

typedef struct Builder {
    BuilderOptions options;
    char *         exec_dir;
    char *         lib_dir;
    Arena          str_cache;
    volatile s32   total_lines;
    s32            errorc;
    s32            max_error;
    s32            test_failc;
    s32            last_script_mode_run_status;
    ConfData       conf;

    TArray targets;
    TArray tmp_strings;

    struct ThreadingImpl *threading;
} Builder;

// Builder global instance.
extern Builder builder;

typedef enum {
    BUILDER_MSG_ERROR,
    BUILDER_MSG_WARNING,
    BUILDER_MSG_NOTE,
    BUILDER_MSG_LOG,
} BuilderMsgType;

typedef enum {
    BUILDER_CUR_AFTER,
    BUILDER_CUR_WORD,
    BUILDER_CUR_BEFORE,
    BUILDER_CUR_NONE
} BuilderCurPos;

struct Location;

// Initialize builder global instance with executable directory specified.
void        builder_init(const char *exec_dir);
void        builder_terminate(void);
void        builder_set_lib_dir(const char *lib_dir);
const char *builder_get_lib_dir(void);
const char *builder_get_exec_dir(void);
int         builder_load_config(const char *filepath);
int     builder_compile_config(const char *filepath, ConfData *out_data, struct Token *import_from);
Target *builder_add_target(const char *name);
s32     builder_compile_all(void);
s32     builder_compile(const Target *target);

// Submit new unit for async compilation, in case no-jobs flag is set, this function does nothing.
void builder_async_submit_unit(Unit *unit);

#define builder_log(format, ...)                                                                   \
    builder_msg(BUILDER_MSG_LOG, -1, NULL, BUILDER_CUR_NONE, format, ##__VA_ARGS__)
#define builder_note(format, ...)                                                                  \
    builder_msg(BUILDER_MSG_NOTE, -1, NULL, BUILDER_CUR_NONE, format, ##__VA_ARGS__)
#define builder_warning(format, ...)                                                               \
    builder_msg(BUILDER_MSG_WARNING, -1, NULL, BUILDER_CUR_NONE, format, ##__VA_ARGS__)
#define builder_error(format, ...)                                                                 \
    builder_msg(BUILDER_MSG_ERROR, -1, NULL, BUILDER_CUR_NONE, format, ##__VA_ARGS__)

void builder_msg(BuilderMsgType   type,
                 s32              code,
                 struct Location *src,
                 BuilderCurPos    pos,
                 const char *     format,
                 ...);

TString *builder_create_cached_str(void);
TString *get_tmpstr(void);
void     put_tmpstr(TString *str);

#endif
