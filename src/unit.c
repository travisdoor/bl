// =================================================================================================
// bl
//
// File:   unit.c
// Author: Martin Dorazil
// Date:   26.1.18
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

#include "unit.h"
#include "bldebug.h"
#include "blmemory.h"
#include "common.h"
#include "token.h"
#include <string.h>

#if BL_PLATFORM_WIN
#include <windows.h>
#endif

u64 unit_hash(const char *filepath, struct Token *load_from)
{
    Unit *parent_unit = load_from ? load_from->location.unit : NULL;
    char *real_path   = NULL;
    search_source_file(
        filepath, SEARCH_FLAG_ALL, parent_unit ? parent_unit->dirpath : NULL, &real_path, NULL);
    const u64 hash = thash_from_str(real_path ? real_path : filepath);
    free(real_path);
    return hash;
}

// public
Unit *unit_new(const char *filepath, Token *load_from)
{
    Unit *parent_unit = load_from ? load_from->location.unit : NULL;
    Unit *unit        = bl_malloc(sizeof(Unit));
    memset(unit, 0, sizeof(Unit));
    search_source_file(filepath,
                       SEARCH_FLAG_ALL,
                       parent_unit ? parent_unit->dirpath : NULL,
                       &unit->filepath,
                       &unit->dirpath);
    unit->name         = strdup(filepath);
    char tmp[PATH_MAX] = {0};
    if (get_filename_from_filepath(tmp, TARRAY_SIZE(tmp), filepath)) {
        unit->filename = strdup(tmp);
    } else {
        BL_ABORT("invalid file");
    }
    unit->loaded_from = load_from;
    unit->hash        = thash_from_str(unit->filepath ? unit->filepath : unit->name);
    tokens_init(&unit->tokens);
    return unit;
}

void unit_delete(Unit *unit)
{
    free(unit->filepath);
    free(unit->dirpath);
    free(unit->src);
    free(unit->name);
    free(unit->filename);
    tokens_terminate(&unit->tokens);
    bl_free(unit);
}

const char *unit_get_src_ln(Unit *unit, s32 line, long *len)
{
    if (line < 1) return NULL;
    const char *c     = unit->src;
    const char *begin = c;
    while (true) {
        if (*c == '\n') {
            --line;
            if (line == 0) break;
            begin = c + 1;
        }
        if (*c == '\0') {
            --line;
            break;
        }
        ++c;
    }
    if (line > 0) return NULL;
    if (len) {
        (*len) = (long)(c - begin);
        BL_ASSERT(*len >= 0);
    }
    return begin;
}
