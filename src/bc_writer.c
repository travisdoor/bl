//************************************************************************************************
// bl
//
// File:   bc_writer.c
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

#include "assembly.h"
#include "bldebug.h"
#include "error.h"
#include "stages.h"
#include <string.h>

void
bc_writer_run(Builder *builder, Assembly *assembly)
{
	char *export_file = malloc(sizeof(char) * (strlen(assembly->name) + 4));
	if (!export_file) BL_ABORT("bad alloc");
	strcpy(export_file, assembly->name);
	strcat(export_file, ".ll");

	char *str = LLVMPrintModuleToString(assembly->llvm.module);

	FILE *f = fopen(export_file, "w");
	if (f == NULL) {
		builder_error(builder, "cannot open file %s", export_file);
		free(export_file);
		return;
	}
	fprintf(f, "%s\n", str);
	fclose(f);
	LLVMDisposeMessage(str);

	msg_log("Byte code written into " GREEN("%s"), export_file);

	free(export_file);
}
