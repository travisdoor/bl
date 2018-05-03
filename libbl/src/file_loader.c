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

#include <stdio.h>
#include <limits.h>
#include <string.h>
#include "bl/unit.h"
#include "stages_impl.h"
#include "common_impl.h"

#define ENV_PATH "PATH"

static FILE *
search_file(const char *filepath)
{
  if (filepath == NULL)
    return NULL;

  FILE *f = fopen(filepath, "r");
  if (f != NULL) {
    return f;
  }

  char  tmp[PATH_MAX];
  char *dup          = strdup(getenv(ENV_PATH));
  char *s            = dup;
  char *p            = NULL;
  int   filepath_len = strlen(filepath);

  do {
    p = strchr(s, ':');
    if (p != NULL) {
      p[0] = 0;
    }

    if (strlen(s) + filepath_len + strlen("/") >= PATH_MAX)
      bl_abort("path too long");

    strcpy(&tmp[0], s);
    strcat(&tmp[0], "/");
    strcat(&tmp[0], filepath);

    f = fopen(&tmp[0], "r");
    s = p + 1;
  } while (p != NULL && f == NULL);

  free(dup);
  return f;
}

bl_error_e
bl_file_loader_run(bl_builder_t *builder, bl_unit_t *unit)
{
  FILE *f = search_file(unit->filepath);

  if (f == NULL) {
    bl_builder_error(builder, "file not found %s", unit->filepath);
    return BL_ERR_FILE_NOT_FOUND;
  }

  fseek(f, 0, SEEK_END);
  size_t fsize = (size_t)ftell(f);
  if (fsize == 0) {
    fclose(f);
    bl_builder_error(builder, "invalid source file %s", unit->filepath);
    return BL_ERR_INVALID_SOURCE;
  }

  fseek(f, 0, SEEK_SET);

  char * src    = malloc(sizeof(char) * (fsize + 1));
  size_t result = fread(src, fsize, 1, f);
  if (result != 1) {
    bl_abort("cannot read file %s", unit->filepath);
  }

  src[fsize] = '\0';
  fclose(f);

  unit->src = src;
  return BL_NO_ERR;
}
