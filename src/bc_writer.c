// =================================================================================================
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
// =================================================================================================

#include "bldebug.h"
#include "builder.h"
#include "error.h"
#include <string.h>

void bc_writer_run(struct assembly *assembly)
{
    ZONE();
    TString *            export_file = get_tmpstr();
    const struct target *target      = assembly->target;
    const char *         name        = target->name;
    tstring_setf(export_file, "%s/%s.ll", target->out_dir.data, name);
    char *str = LLVMPrintModuleToString(assembly->llvm.module);
    FILE *f   = fopen(export_file->data, "w");
    if (f == NULL) {
        builder_error("Cannot open file %s", export_file->data);
        put_tmpstr(export_file);
        RETURN_ZONE();
    }
    fprintf(f, "%s\n", str);
    fclose(f);
    LLVMDisposeMessage(str);

    builder_note("Byte code written into %s", export_file->data);

    put_tmpstr(export_file);
    RETURN_ZONE();
}
