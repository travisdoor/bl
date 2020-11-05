//************************************************************************************************
// bl
//
// File:   file_loader.c
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
//************************************************************************************************

#include "common.h"
#include "stages.h"
#include "unit.h"
#include <stdio.h>
#include <string.h>

void file_loader_run(Unit *unit)
{
    TracyCZone(_tctx, true);
    if (!unit->filepath) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_FILE_NOT_FOUND,
                    TOKEN_OPTIONAL_LOCATION(unit->loaded_from),
                    BUILDER_CUR_WORD,
                    "File not found '%s'.",
                    unit->name);
        TracyCZoneEnd(_tctx);
        return;
    }

    FILE *f = fopen(unit->filepath, "rb");

    if (f == NULL) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_FILE_READ,
                    TOKEN_OPTIONAL_LOCATION(unit->loaded_from),
                    BUILDER_CUR_WORD,
                    "Cannot read file '%s'.",
                    unit->name);

        TracyCZoneEnd(_tctx);
        return;
    }

    fseek(f, 0, SEEK_END);
    usize fsize = (usize)ftell(f);
    if (fsize == 0) {
        fclose(f);
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_FILE_EMPTY,
                    TOKEN_OPTIONAL_LOCATION(unit->loaded_from),
                    BUILDER_CUR_WORD,
                    "Invalid or empty source file '%s'.",
                    unit->name);

        TracyCZoneEnd(_tctx);
        return;
    }

    fseek(f, 0, SEEK_SET);

    char *src = calloc(fsize + 1, sizeof(char));
    if (src == NULL) BL_ABORT("bad alloc");
    if (!fread(src, sizeof(char), fsize, f)) BL_ABORT("Cannot read file '%s'.", unit->name);

    src[fsize] = '\0';
    fclose(f);

    unit->src = src;
    TracyCZoneEnd(_tctx);
}
