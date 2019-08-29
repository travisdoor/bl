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

#include <bobject/containers/string.h>
#include <stdarg.h>
#include <time.h>

#include "assembly.h"
#include "builder.h"
#include "common.h"
#include "stages.h"
#include "token.h"
#include "unit.h"

#define MAX_MSG_LEN 1024
#define MAX_ERROR_REPORTED 10

static int
compile_unit(Builder *builder, Unit *unit, Assembly *assembly, uint32_t flags);

static int
compile_assembly(Builder *builder, Assembly *assembly, uint32_t flags);

static bool llvm_initialized = false;

static void
llvm_init(void)
{
	if (llvm_initialized) return;

	LLVMInitializeX86Target();
	LLVMInitializeX86TargetInfo();
	LLVMInitializeX86TargetMC();
	LLVMInitializeX86AsmPrinter();

	/*
	LLVMInitializeAllTargetInfos();
	LLVMInitializeAllTargets();
	LLVMInitializeAllTargetMCs();
	LLVMInitializeAllAsmParsers();
	LLVMInitializeAllAsmPrinters();
	LLVMLinkInMCJIT();
	*/
	llvm_initialized = true;
}

#define interrupt_on_error(_builder)                                                               \
	if ((_builder)->errorc) return COMPILE_FAIL;

int
compile_unit(Builder *builder, Unit *unit, Assembly *assembly, uint32_t flags)
{
	if (is_flag(flags, BUILDER_FLAG_VERBOSE)) {
		if (unit->loaded_from) {
			msg_log("Compile: %s (loaded from '%s')",
			        unit->name,
			        unit->loaded_from->location.unit->name);
		} else {
			msg_log("Compile: %s", unit->name);
		}
	}

	if (is_flag(flags, BUILDER_FLAG_LOAD_FROM_FILE)) {
		file_loader_run(builder, unit);
		interrupt_on_error(builder);
	}

	lexer_run(builder, unit);
	interrupt_on_error(builder);

	if (is_flag(flags, BUILDER_FLAG_PRINT_TOKENS)) {
		token_printer_run(unit);
		interrupt_on_error(builder);
	}

	parser_run(builder, assembly, unit);

	return COMPILE_OK;
}

int
compile_assembly(Builder *builder, Assembly *assembly, uint32_t flags)
{
	if (is_flag(flags, BUILDER_FLAG_PRINT_AST)) {
		ast_printer_run(assembly, stdout);
	}
	interrupt_on_error(builder);

	linker_run(builder, assembly);
	interrupt_on_error(builder);

	if (is_not_flag(flags, BUILDER_FLAG_SYNTAX_ONLY)) {
		mir_run(builder, assembly);
		if (is_flag(flags, BUILDER_FLAG_EMIT_MIR)) mir_writer_run(assembly);
		interrupt_on_error(builder);

		ir_run(builder, assembly);
		interrupt_on_error(builder);

		ir_opt_run(builder, assembly);
		interrupt_on_error(builder);

		if (is_flag(flags, BUILDER_FLAG_EMIT_LLVM)) {
			bc_writer_run(builder, assembly);
			interrupt_on_error(builder);
		}

		if (is_not_flag(flags, BUILDER_FLAG_NO_BIN)) {
			obj_writer_run(builder, assembly);
			interrupt_on_error(builder);
			native_bin_run(builder, assembly);
			interrupt_on_error(builder);
		}
	}

	return COMPILE_OK;
}

/* public */
Builder *
builder_new(void)
{
	Builder *builder = bl_calloc(1, sizeof(Builder));
	if (!builder) bl_abort("bad alloc");

	builder->flags     = 0;
	builder->errorc    = 0;
	builder->str_cache = bo_array_new_bo(bo_typeof(BString), true);
	builder->conf      = conf_data_new();

	/* initialize LLVM statics */
	llvm_init();

	return builder;
}

void
builder_delete(Builder *builder)
{
	conf_data_delete(builder->conf);
	bo_unref(builder->str_cache);
	bl_free(builder);
}

int
builder_load_conf_file(Builder *builder, const char *filepath)
{
	Unit *unit = unit_new_file(filepath, NULL, NULL);

	/* load */
	file_loader_run(builder, unit);
	interrupt_on_error(builder);

	/* use standart lexer */
	lexer_run(builder, unit);
	interrupt_on_error(builder);

	/* print output */
	/*
	token_printer_run(unit);
	interrupt_on_error(builder);
	*/

	/* print output */
	conf_parser_run(builder, unit);
	interrupt_on_error(builder);

	unit_delete(unit);

	return COMPILE_OK;
}

int
builder_compile(Builder *builder, Assembly *assembly, uint32_t flags, OptLvl opt_lvl)
{
	clock_t begin = clock();
	Unit *  unit;
	int32_t state = COMPILE_OK;

	builder->flags = flags;
	msg_log("Compile assembly: %s", assembly->name);

	assembly_setup(assembly, flags, opt_lvl);

	{
		unit = unit_new_file(CORE_SOURCE_FILE, NULL, NULL);
		if (!assembly_add_unit_unique(assembly, unit)) {
			unit_delete(unit);
		}
	}

	barray_foreach(assembly->units, unit)
	{
		/* IDEA: can run in separate thread */
		if ((state = compile_unit(builder, unit, assembly, flags)) != COMPILE_OK) {
			break;
		}
	}

	if (state == COMPILE_OK) state = compile_assembly(builder, assembly, flags);

	clock_t end        = clock();
	double  time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

	msg_log("Compiled %i lines in %f seconds.", builder->total_lines, time_spent);
	if (state != COMPILE_OK) {
		msg_log("There were errors, sorry...");
	}

	return state;
}

void
builder_error(Builder *builder, const char *format, ...)
{
	if (builder->errorc > MAX_ERROR_REPORTED) return;
	char error[MAX_MSG_LEN] = {0};

	va_list args;
	va_start(args, format);
	vsnprintf(error, MAX_MSG_LEN, format, args);
	va_end(args);

	msg_error("%s", &error[0]);
	builder->errorc++;
}

void
builder_warning(Builder *builder, const char *format, ...)
{
	char warning[MAX_MSG_LEN] = {0};

	va_list args;
	va_start(args, format);
	vsnprintf(warning, MAX_MSG_LEN, format, args);
	va_end(args);

	msg_warning("%s", &warning[0]);
}

void
builder_msg(Builder *      builder,
            BuilderMsgType type,
            int32_t        code,
            Location *     src,
            BuilderCurPos  pos,
            const char *   format,
            ...)
{
	if (type == BUILDER_MSG_ERROR && builder->errorc > MAX_ERROR_REPORTED) return;
	if (is_flag(builder->flags, BUILDER_FLAG_NO_WARN) && type == BUILDER_MSG_WARNING) return;

	BString *tmp              = bo_string_new(MAX_MSG_LEN);
	char     msg[MAX_MSG_LEN] = {0};

	if (src) {
		int32_t line     = src->line;
		int32_t col      = src->col;
		int32_t len      = src->len;
		char *  color    = NULL;
		char *  msg_mark = NULL;

		switch (type) {
		case BUILDER_MSG_ERROR:
			color    = RED_BEGIN;
			msg_mark = "E";
			break;
		case BUILDER_MSG_LOG:
			color    = GREEN_BEGIN;
			msg_mark = "";
			break;
		case BUILDER_MSG_NOTE:
			color    = BLUE_BEGIN;
			msg_mark = "N";
			break;
		case BUILDER_MSG_WARNING:
			color    = YELLOW_BEGIN;
			msg_mark = "W";
			break;
		}

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

		if (code == 0) {
			snprintf(msg, MAX_MSG_LEN, "%s:%d:%d ", src->unit->filepath, line, col);
		} else {
			snprintf(msg,
			         MAX_MSG_LEN,
			         "[%s%04d] %s:%d:%d ",
			         msg_mark,
			         code,
			         src->unit->filepath,
			         line,
			         col);
		}

		va_list args;
		va_start(args, format);
		vsnprintf(msg + strlen(msg), MAX_MSG_LEN - strlen(msg), format, args);
		va_end(args);
		bo_string_append(tmp, &msg[0]);

		int32_t     pad      = sprintf(msg, "%d", src->line) + 2;
		long        line_len = 0;
		const char *line_str = unit_get_src_ln(src->unit, src->line - 1, &line_len);
		if (line_str && line_len) {
			sprintf(msg, "\n%*d", pad, src->line - 1);
			bo_string_append(tmp, &msg[0]);
			bo_string_append(tmp, " | ");
			bo_string_appendn(tmp, line_str, line_len);
		}

		line_str = unit_get_src_ln(src->unit, src->line, &line_len);
		if (line_str && line_len) {
			bo_string_append(tmp, color);
			sprintf(msg, "\n>%*d", pad - 1, src->line);
			bo_string_append(tmp, &msg[0]);
			bo_string_append(tmp, " | ");
			bo_string_appendn(tmp, line_str, line_len);
			bo_string_append(tmp, COLOR_END);
		}

		if (pos != BUILDER_CUR_NONE) {
			sprintf(msg, "\n%*s", pad, "");
			bo_string_append(tmp, &msg[0]);
			bo_string_append(tmp, " | ");

			bo_string_append(tmp, color);
			for (int32_t i = 0; i < col + len - 1; ++i) {
				if (i < col - 1)
					bo_string_append(tmp, " ");
				else
					bo_string_append(tmp, "^");
			}
			bo_string_append(tmp, COLOR_END);
		}

		sprintf(msg, "\n%*d", pad, src->line + 1);
		bo_string_append(tmp, &msg[0]);
		bo_string_append(tmp, " | ");

		line_str = unit_get_src_ln(src->unit, src->line + 1, &line_len);
		if (line_str && line_len) {
			bo_string_appendn(tmp, line_str, line_len);
		}
	} else {
		va_list args;
		va_start(args, format);
		vsnprintf(msg + strlen(msg), MAX_MSG_LEN - strlen(msg), format, args);
		va_end(args);
		bo_string_append(tmp, &msg[0]);
	}

	if (type == BUILDER_MSG_ERROR) {
		builder->errorc++;
		msg_error("%s", bo_string_get(tmp));
	} else if (type == BUILDER_MSG_WARNING) {
		msg_warning("%s", bo_string_get(tmp));
	} else if (type == BUILDER_MSG_LOG) {
		msg_log("%s", bo_string_get(tmp));
	} else {
		msg_note("%s", bo_string_get(tmp));
	}

	bo_unref(tmp);

#if ASSERT_ON_CMP_ERROR
	if (type == BUILDER_MSG_ERROR) assert(false);
#endif
}

BString *
builder_create_cached_str(Builder *builder)
{
	BString *str = bo_string_new(64);
	bo_array_push_back(builder->str_cache, str);
	return str;
}
