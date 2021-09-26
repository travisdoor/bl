// =================================================================================================
// bl
//
// File:   mir_writer.c
// Author: Martin Dorazil
// Date:   20.12.18
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
#include "mir_printer.h"

static void print_header(const char *name, const char *filename, FILE *stream)
{
    char date[26];
    date_time(date, 26, "%d-%m-%Y %H:%M:%S");

    fprintf(stream,
            "/* \n"
            "  Mid-level IR source file generated by blc.\n\n"
            "  Assembly:    %s\n"
            "  File:        %s\n"
            "  Created:     %s\n"
            "  Blc version: %s\n"
            "*/\n",
            name,
            filename,
            date,
            BL_VERSION);
}

void mir_writer_run(struct assembly *assembly)
{
    const char *         name        = assembly->target->name;
    TString *            export_file = get_tmpstr();
    const struct target *target      = assembly->target;
    tstring_setf(export_file, "%s/%s.blm", target->out_dir.data, name);
    FILE *f = fopen(export_file->data, "w");
    if (f == NULL) {
        builder_error("cannot open file %s", export_file->data);
        put_tmpstr(export_file);
        return;
    }
    print_header(name, export_file->data, f);
    mir_print_assembly(assembly, f);
    fclose(f);
    builder_note("Mir code written into %s", export_file->data);
    put_tmpstr(export_file);
}
