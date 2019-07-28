//*****************************************************************************
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
//*****************************************************************************

#include "assembly.h"
#include "bldebug.h"
#include "builder.h"
#include "error.h"
#include "messages.h"
#include "unit.h"
#include <locale.h>
#include <stdio.h>
#include <string.h>

char *ENV_EXEC_DIR      = NULL;
char *ENV_LIB_DIR       = NULL;
char *ENV_CONF_FILEPATH = NULL;

static void
print_help(void)
{
	fprintf(stdout,
	        "Usage\n\n"
	        "  blc [options] <source-files>\n\n"
	        "Options\n"
	        "  -h, -help           = Print usage information and exit.\n"
	        "  -r, -run            = Execute 'main' method in compile time.\n"
	        "  -rt, -run-tests     = Execute all unit tests in compile time.\n"
	        "  -emit-llvm          = Write LLVM-IR to file.\n"
	        "  -emit-mir           = Write MIR to file.\n"
	        "  -ast-dump           = Print AST.\n"
	        "  -lex-dump           = Print output of lexer.\n"
	        "  -mir-pre-dump       = Print output of MIR pre analyze stage.\n"
	        "  -mir-post-dump      = Print output of MIR post analyze stage.\n"
	        "  -syntax-only        = Check syntax and exit.\n"
	        "  -no-bin             = Don't write binary to disk.\n"
	        "  -no-warning         = Ignore all warnings.\n"
	        "  -verbose            = Verbose mode.\n"
	        "  -no-api             = Don't load internal api.\n"
	        "  -force-test-to-llvm = Force llvm generation of unit tests.\n"
	        "  -configure          = Generate config file.\n");
}

static void
free_env(void)
{
	free(ENV_EXEC_DIR);
	free(ENV_LIB_DIR);
	free(ENV_CONF_FILEPATH);
}

static void
setup_env(void)
{
	char tmp[PATH_MAX] = {0};

	if (!get_current_exec_dir(tmp, PATH_MAX)) {
		bl_abort("Cannot locate compiler executable path.");
	}

	ENV_EXEC_DIR = strdup(tmp);

	strcat(tmp, PATH_SEPARATOR ".." PATH_SEPARATOR);
	strcat(tmp, BL_CONF_FILE);

	if (strlen(tmp) == 0) bl_abort("Invalid conf file path.");
	ENV_CONF_FILEPATH = strdup(tmp);
	atexit(free_env);
}

static int
generate_conf(void)
{
	char tmp[PATH_MAX] = {0};

#if defined(BL_PLATFORM_LINUX) || defined(BL_PLATFORM_MACOS)
	strcat(tmp, "sh ");
	strcat(tmp, ENV_EXEC_DIR);
	strcat(tmp, PATH_SEPARATOR);
	strcat(tmp, BL_CONFIGURE_SH);
#else 
	strcat(tmp, "\"");
	strcat(tmp, ENV_EXEC_DIR);
	strcat(tmp, PATH_SEPARATOR);
	strcat(tmp, BL_CONFIGURE_SH);
	strcat(tmp, "\"");
#endif


	return system(tmp);
}

int
main(int32_t argc, char *argv[])
{
	setlocale(LC_ALL, "C");
	setup_env();

	uint32_t build_flags = BUILDER_LOAD_FROM_FILE;

	puts("compiler version: " BL_VERSION " (pre-alpha)");
#ifdef BL_DEBUG
	puts("running in DEBUG mode");
#endif

#define arg_is(_arg) (strcmp(&argv[optind][1], _arg) == 0)

	bool    help      = false;
	bool    configure = false;
	int32_t optind;
	for (optind = 1; optind < argc && argv[optind][0] == '-'; optind++) {
		if (arg_is("ast-dump")) {
			build_flags |= BUILDER_PRINT_AST;
		} else if (arg_is("h") || arg_is("help")) {
			help = true;
		} else if (arg_is("lex-dump")) {
			build_flags |= BUILDER_PRINT_TOKENS;
		} else if (arg_is("mir-pre-dump")) {
			build_flags |= BUILDER_VERBOSE_MIR_PRE;
		} else if (arg_is("mir-post-dump")) {
			build_flags |= BUILDER_VERBOSE_MIR_POST;
		} else if (arg_is("syntax-only")) {
			build_flags |= BUILDER_SYNTAX_ONLY;
		} else if (arg_is("emit-llvm")) {
			build_flags |= BUILDER_EMIT_LLVM;
		} else if (arg_is("emit-mir")) {
			build_flags |= BUILDER_EMIT_MIR;
		} else if (arg_is("r") || arg_is("run")) {
			build_flags |= BUILDER_RUN;
		} else if (arg_is("rt") || arg_is("run-tests")) {
			build_flags |= BUILDER_RUN_TESTS;
		} else if (arg_is("no-bin")) {
			build_flags |= BUILDER_NO_BIN;
		} else if (arg_is("no-warning")) {
			build_flags |= BUILDER_NO_WARN;
		} else if (arg_is("verbose")) {
			build_flags |= BUILDER_VERBOSE;
		} else if (arg_is("no-api")) {
			build_flags |= BUILDER_NO_API;
		} else if (arg_is("force-test-to-llvm")) {
			build_flags |= BUILDER_FORCE_TEST_LLVM;
		} else if (arg_is("configure")) {
			configure = true;
		} else {
			msg_error("invalid params '%s'", &argv[optind][1]);
			print_help();
			exit(EXIT_FAILURE);
		}
	}
	argv += optind;

#undef arg_is

	if (configure) {
		if (generate_conf() != 0) {
			msg_error("Cannot generate '%s' file. If you are compiler developer please "
			          "run configuration script in 'install' directory.",
			          ENV_CONF_FILEPATH);
			exit(EXIT_FAILURE);
		}
		exit(EXIT_SUCCESS);
	}

	if (help) {
		print_help();
		exit(EXIT_SUCCESS);
	}

	if (!file_exists(ENV_CONF_FILEPATH)) {
		msg_error("Configuration file '%s' not found, run 'blc -configure' to "
		          "generate one.",
		          ENV_CONF_FILEPATH);
		exit(EXIT_FAILURE);
	}

	if (*argv == NULL) {
		msg_warning("nothing to do, no input files, sorry :(");
		exit(EXIT_SUCCESS);
	}

	Builder *builder = builder_new();
	builder_load_conf_file(builder, ENV_CONF_FILEPATH);

	/* setup LIB_DIR */
	ENV_LIB_DIR = strdup(conf_data_get_str(builder->conf, CONF_LIB_DIR_KEY));

	/*
	 * HACK: use name of first file as assembly name
	 */
	char *assembly_name = strrchr(*argv, PATH_SEPARATORC);
	if (assembly_name == NULL) {
		assembly_name = *argv;
	} else {
		++assembly_name;
	}

	assembly_name = strdup(assembly_name);
#ifdef BL_COMPILER_MSVC
	PathRemoveExtensionA(assembly_name);
#else
	char *ext = rindex(assembly_name, '.');
	if (ext != NULL) {
		(*ext) = '\0';
	}
#endif

	Assembly *assembly = assembly_new(assembly_name);
	free(assembly_name);

	/* init actors */
	while (*argv != NULL) {
		Unit *unit = unit_new_file(*argv, NULL, NULL);

		bool added = assembly_add_unit_unique(assembly, unit);
		if (added == false) {
			unit_delete(unit);
		}

		argv++;
	}

	int32_t state = builder_compile(builder, assembly, build_flags);

	char date[26];
	date_time(date, 26, "%d-%m-%Y %H:%M:%S");
	msg_log("\nfinished at %s", date);

	if (state == COMPILE_OK) {
		msg_log(GREEN("done"));
	}

	assembly_delete(assembly);
	builder_delete(builder);

	return state;
}
