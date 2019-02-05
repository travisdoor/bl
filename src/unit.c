//************************************************************************************************
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
//************************************************************************************************

#include <string.h>
#include <limits.h>
#include "unit.h"
#include "blmemory.h"
#include "bldebug.h"
#include "token.h"

static void
init(Unit *unit)
{
  tokens_init(&unit->tokens);
  unit->ast = NULL;
}

static char *
search_file(const char *filepath)
{
  if (filepath == NULL) return NULL;

  char        tmp_rpath[PATH_MAX];
  const char *rpath = brealpath(filepath, tmp_rpath, PATH_MAX);

  if (rpath != NULL) {
    return strdup(rpath);
  }

  /* file has not been found in current working direcotry -> search in PATH */
  char   tmp_env[PATH_MAX];
  char * env          = strdup(getenv(ENV_PATH));
  char * s            = env;
  char * p            = NULL;
  size_t filepath_len = strlen(filepath);

  do {
    p = strchr(s, ENVPATH_SEPARATOR);
    if (p != NULL) {
      p[0] = 0;
    }

    if (strlen(s) + filepath_len + strlen(PATH_SEPARATOR) >= PATH_MAX) bl_abort("path too long");

    strcpy(&tmp_env[0], s);
    strcat(&tmp_env[0], PATH_SEPARATOR);
    strcat(&tmp_env[0], filepath);

    rpath = brealpath(&tmp_env[0], tmp_rpath, PATH_MAX);

    s = p + 1;
  } while (p != NULL && rpath == NULL);

  free(env);
  if (rpath) return strdup(rpath);
  return NULL;
}

/* public */
Unit *
unit_new_file(const char *filepath, Token *loaded_from)
{
  Unit *unit = bl_calloc(1, sizeof(Unit));
  if (!unit) bl_abort("bad alloc");
  unit->filepath    = search_file(filepath);
  unit->name        = strdup(filepath);
  unit->loaded_from = loaded_from;
  init(unit);
  return unit;
}

Unit *
unit_new_str(const char *name, const char *src)
{
  Unit *unit = bl_calloc(1, sizeof(Unit));
  if (!unit) bl_abort("bad alloc");
  unit->filepath = strdup(name);
  unit->name     = strdup(name);

  if (src)
    unit->src = strdup(src);
  else
    bl_abort("invalid source for %s unit", unit->name);

  init(unit);
  return unit;
}

void
unit_delete(Unit *unit)
{
  free(unit->filepath);
  free(unit->src);
  free(unit->name);
  tokens_terminate(&unit->tokens);
  bl_free(unit);
}

const char *
unit_get_src_file(Unit *unit)
{
  return unit->filepath;
}

const char *
unit_get_src(Unit *unit)
{
  return unit->src;
}

const char *
unit_get_name(Unit *unit)
{
  return unit->name;
}

const char *
unit_get_src_ln(Unit *unit, int32_t line, long *len)
{
  int32_t     l    = 1;
  const char *iter = unit->src;
  while (iter && l != line) {
    ++l;
    iter = strchr(iter, '\n');
    iter = iter ? iter + 1 : NULL;
  }

  if (len) {
    long l = 0;
    if (iter) l = (long)(strchr(iter, '\n') - iter);
    if (l < 0) l = (long)strlen(iter);
    (*len) = l;
  }

  return iter;
}
