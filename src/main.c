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
#include "threading.h"
#include "unit.h"
#include <locale.h>
#include <stdio.h>
#include <string.h>

char *ENV_EXEC_DIR      = NULL;
char *ENV_LIB_DIR       = NULL;
char *ENV_CONF_FILEPATH = NULL;

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
		BL_ABORT("Cannot locate compiler executable path.");
	}

	ENV_EXEC_DIR = strdup(tmp);

	strcat(tmp, PATH_SEPARATOR ".." PATH_SEPARATOR);
	strcat(tmp, BL_CONF_FILE);

	if (strlen(tmp) == 0) BL_ABORT("Invalid conf file path.");
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
main(s32 argc, char *argv[])
{
	const char *help_text =
#include "help_text.txt"
	    ;

	setlocale(LC_ALL, "C");
	setup_env();
	main_thread_id = thread_get_id();

	puts("Compiler version: " BL_VERSION);
#ifdef BL_DEBUG
	puts("Running in DEBUG mode");
	printf("Main thread ID: 0x%llx\n", main_thread_id);
#endif

	builder_init();
	s32 next_arg = builder_parse_options(argc, argv);
	if (next_arg == -1) {
		fprintf(stdout, "%s", help_text);
		builder_terminate();

		exit(EXIT_FAILURE);
	}

	/* Run configure if needed. */
	if (builder.options.run_configure) {
		if (generate_conf() != 0) {
			msg_error("Cannot generate '%s' file. If you are compiler developer please "
			          "run configuration script in 'install' directory.",
			          ENV_CONF_FILEPATH);
			builder_terminate();
			exit(EXIT_FAILURE);
		}

		builder_terminate();
		exit(EXIT_SUCCESS);
	}

	argv += next_arg;

	if (builder.options.print_help) {
		fprintf(stdout, "%s", help_text);
		builder_terminate();

		exit(EXIT_SUCCESS);
	}

	if (!file_exists(ENV_CONF_FILEPATH)) {
		msg_error("Configuration file '%s' not found, run 'blc -configure' to "
		          "generate one.",
		          ENV_CONF_FILEPATH);

		builder_terminate();
		exit(EXIT_FAILURE);
	}

	if (*argv == NULL) {
		msg_warning("nothing to do, no input files, sorry :(");

		builder_terminate();
		exit(EXIT_SUCCESS);
	}

	builder_load_conf_file(ENV_CONF_FILEPATH);

	/* setup LIB_DIR */
	ENV_LIB_DIR = strdup(conf_data_get_str(builder.conf, CONF_LIB_DIR_KEY));

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
	if (builder.options.use_pipeline) assembly->options.build_mode = BUILD_MODE_BUILD;

	while (*argv != NULL) {
		Unit *unit = unit_new_file(*argv, NULL, NULL);

		bool added = assembly_add_unit_unique(assembly, unit);
		if (added == false) {
			unit_delete(unit);
		}

		argv++;
	}

	builder_add_assembly(assembly);
	s32 state = builder_compile_all();

	char date[26];
	date_time(date, 26, "%d-%m-%Y %H:%M:%S");
	msg_log("\nFinished at %s", date);

	builder_terminate();
	return state;
}
