//************************************************************************************************
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
//************************************************************************************************

#ifndef BL_BUILDER_H
#define BL_BUILDER_H

#include "assembly.h"
#include "conf_data.h"
#include "error.h"
#include "mir.h"

#define COMPILE_OK 0
#define COMPILE_FAIL 1

typedef struct BuilderOptions {
	BuildMode   build_mode;
	BuildDIKind build_di_kind;
	bool        print_help;
	bool        print_tokens;
	bool        print_ast;
	bool        run;
	bool        run_tests;
	bool        run_configure;
	bool        no_bin;
	bool        no_warn;
	bool        no_api;
	bool        no_llvm;
	bool        no_analyze;
	bool        emit_llvm;
	bool        emit_mir;
	bool        load_from_file;
	bool        syntax_only;
	bool        verbose;
	bool        reg_split;
	bool        use_pipeline;
	bool        no_color;
	bool        no_vcvars;
} BuilderOptions;

typedef enum {
	BUILDER_MSG_ASSEMBLY_BEGIN,
	BUILDER_MSG_ASSEMBLY_END,
	BUILDER_MSG_ASSEMBLY_FAILED,
	BUILDER_MSG_VAR,
	BUILDER_MSG_FN,
} BuilderMessageKind;

typedef struct BuilderMessage {
	BuilderMessageKind kind;
	union {
		MirVar *var;
		MirFn * fn;
	} data;
} BuilderMessage;

typedef void (*BuilderMessageHandler)(const Assembly *, const BuilderMessage *);
TSMALL_ARRAY_TYPE(MessageHandler, BuilderMessageHandler, 8);

typedef struct Builder {
	BuilderOptions             options;
	TSmallArray_MessageHandler message_handlers;
	Arena                      str_cache;
	s32                        total_lines;
	s32                        errorc;
	s32                        test_failc;
	ConfData *                 conf;

	TArray assembly_queue;
} Builder;

/* Builder global instance */
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

void builder_init(void);
void builder_terminate(void);
s32  builder_parse_options(s32 argc, char *argv[]);
int  builder_load_conf_file(const char *filepath);
void builder_add_assembly(Assembly *assembly);
s32  builder_compile_all(void);
s32  builder_compile(Assembly *assembly);
void builder_invoke_message(const Assembly *assembly, const BuilderMessage *msg);
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

#endif
