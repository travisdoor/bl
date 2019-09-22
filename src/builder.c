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

Builder builder;

static int
compile_unit(Unit *unit, Assembly *assembly, u32 flags);

static int
compile_assembly(Assembly *assembly, u32 flags);

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

#define INTERRUPT_ON_ERROR                                                                         \
	if (builder.errorc) return COMPILE_FAIL;

int
compile_unit(Unit *unit, Assembly *assembly, u32 flags)
{
	if (IS_FLAG(flags, BUILDER_FLAG_VERBOSE)) {
		if (unit->loaded_from) {
			msg_log("Compile: %s (loaded from '%s')",
			        unit->name,
			        unit->loaded_from->location.unit->name);
		} else {
			msg_log("Compile: %s", unit->name);
		}
	}

	if (IS_FLAG(flags, BUILDER_FLAG_LOAD_FROM_FILE)) {
		file_loader_run(unit);
		INTERRUPT_ON_ERROR;
	}

	lexer_run(unit);
	INTERRUPT_ON_ERROR;

	if (IS_FLAG(flags, BUILDER_FLAG_PRINT_TOKENS)) {
		token_printer_run(unit);
		INTERRUPT_ON_ERROR;
	}

	parser_run(assembly, unit);

	return COMPILE_OK;
}

int
compile_assembly(Assembly *assembly, u32 flags)
{
	if (IS_FLAG(flags, BUILDER_FLAG_PRINT_AST)) {
		ast_printer_run(assembly, stdout);
	}
	INTERRUPT_ON_ERROR;

	linker_run(assembly);
	INTERRUPT_ON_ERROR;

	if (IS_FLAG(flags, BUILDER_FLAG_SYNTAX_ONLY)) return COMPILE_OK;

	mir_run(assembly);
	if (IS_FLAG(flags, BUILDER_FLAG_EMIT_MIR)) mir_writer_run(assembly);
	INTERRUPT_ON_ERROR;

	if (IS_FLAG(flags, BUILDER_FLAG_NO_LLVM)) return COMPILE_OK;
	ir_run(assembly);
	INTERRUPT_ON_ERROR;

	ir_opt_run(assembly);
	INTERRUPT_ON_ERROR;

	if (IS_FLAG(flags, BUILDER_FLAG_EMIT_LLVM)) {
		bc_writer_run(assembly);
		INTERRUPT_ON_ERROR;
	}

	if (IS_NOT_FLAG(flags, BUILDER_FLAG_NO_BIN)) {
		obj_writer_run(assembly);
		INTERRUPT_ON_ERROR;
		native_bin_run(assembly);
		INTERRUPT_ON_ERROR;
	}

	return COMPILE_OK;
}

/* public */
void
builder_init(void)
{
	builder.flags     = 0;
	builder.errorc    = 0;
	builder.str_cache = bo_array_new_bo(bo_typeof(BString), true);
	builder.conf      = conf_data_new();

	/* initialize LLVM statics */
	llvm_init();
}

void
builder_terminate(void)
{
	conf_data_delete(builder.conf);
	bo_unref(builder.str_cache);
}

int
builder_load_conf_file(const char *filepath)
{
	Unit *unit = unit_new_file(filepath, NULL, NULL);

	/* load */
	file_loader_run(unit);
	INTERRUPT_ON_ERROR;

	/* use standart lexer */
	lexer_run(unit);
	INTERRUPT_ON_ERROR;

	/* print output */
	/*
	token_printer_run(unit);
	INTERRUPT_ON_ERROR(builder);
	*/

	/* print output */
	conf_parser_run(unit);
	INTERRUPT_ON_ERROR;

	unit_delete(unit);

	return COMPILE_OK;
}

int
builder_compile(Assembly *assembly, u32 flags, OptLvl opt_lvl)
{
	clock_t begin = clock();
	Unit *  unit;
	s32     state = COMPILE_OK;

	builder.flags = flags;
	msg_log("Compile assembly: %s", assembly->name);

	assembly_setup(assembly, flags, opt_lvl);

	{
		unit = unit_new_file(CORE_SOURCE_FILE, NULL, NULL);
		if (!assembly_add_unit_unique(assembly, unit)) {
			unit_delete(unit);
		}
	}

	BARRAY_FOREACH(assembly->units, unit)
	{
		/* IDEA: can run in separate thread */
		if ((state = compile_unit(unit, assembly, flags)) != COMPILE_OK) {
			break;
		}
	}

	if (state == COMPILE_OK) state = compile_assembly(assembly, flags);

	clock_t end        = clock();
	f64     time_spent = (f64)(end - begin) / CLOCKS_PER_SEC;

	msg_log("Compiled %i lines in %f seconds.", builder.total_lines, time_spent);
	if (state != COMPILE_OK) {
		msg_log("There were errors, sorry...");
	}

	return state;
}

void
builder_error(const char *format, ...)
{
	if (builder.errorc > MAX_ERROR_REPORTED) return;
	char error[MAX_MSG_LEN] = {0};

	va_list args;
	va_start(args, format);
	vsnprintf(error, MAX_MSG_LEN, format, args);
	va_end(args);

	msg_error("%s", &error[0]);
	builder.errorc++;
}

void
builder_warning(const char *format, ...)
{
	char warning[MAX_MSG_LEN] = {0};

	va_list args;
	va_start(args, format);
	vsnprintf(warning, MAX_MSG_LEN, format, args);
	va_end(args);

	msg_warning("%s", &warning[0]);
}

void
builder_msg(BuilderMsgType type,
            s32            code,
            Location *     src,
            BuilderCurPos  pos,
            const char *   format,
            ...)
{
	if (type == BUILDER_MSG_ERROR && builder.errorc > MAX_ERROR_REPORTED) return;
	if (IS_FLAG(builder.flags, BUILDER_FLAG_NO_WARN) && type == BUILDER_MSG_WARNING) return;

	BString *tmp              = bo_string_new(MAX_MSG_LEN);
	char     msg[MAX_MSG_LEN] = {0};

	if (src) {
		s32   line     = src->line;
		s32   col      = src->col;
		s32   len      = src->len;
		char *color    = NULL;
		char *msg_mark = NULL;

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

		s32         pad      = sprintf(msg, "%d", src->line) + 2;
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
			for (s32 i = 0; i < col + len - 1; ++i) {
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
		builder.errorc++;
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
	if (type == BUILDER_MSG_ERROR) BL_ASSERT(false);
#endif
}

BString *
builder_create_cached_str(void)
{
	BString *str = bo_string_new(64);
	bo_array_push_back(builder.str_cache, str);
	return str;
}
