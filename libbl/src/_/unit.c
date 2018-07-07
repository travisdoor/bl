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
#include "unit_impl.h"
#include "blmemory_impl.h"
#include "bl/bldebug.h"

#define ENV_PATH "PATH"

static void
init(bl_unit_t *unit)
{
  bl_tokens_init(&unit->tokens);
  bl_ast_init(&unit->ast);
}

static char *
search_file(const char *filepath)
{
  if (filepath == NULL)
    return NULL;

  char  tmp_rpath[PATH_MAX];
  char *rpath = realpath(filepath, tmp_rpath);
  if (rpath != NULL) {
    return strdup(rpath);
  }

  /* file has not been found in current working direcotry -> search in PATH */
  char  tmp_env[PATH_MAX];
  char *env          = strdup(getenv(ENV_PATH));
  char *s            = env;
  char *p            = NULL;
  int   filepath_len = strlen(filepath);

  do {
    p = strchr(s, ':');
    if (p != NULL) {
      p[0] = 0;
    }

    if (strlen(s) + filepath_len + strlen("/") >= PATH_MAX)
      bl_abort("path too long");

    strcpy(&tmp_env[0], s);
    strcat(&tmp_env[0], "/");
    strcat(&tmp_env[0], filepath);

    rpath = realpath(&tmp_env[0], tmp_rpath);
    s     = p + 1;
  } while (p != NULL && rpath == NULL);

  free(env);
  if (rpath)
    return strdup(rpath);
  return NULL;
}

/* public */
bl_unit_t *
bl_unit_new_file(const char *filepath)
{
  bl_unit_t *unit = bl_calloc(1, sizeof(bl_unit_t));
  unit->filepath  = search_file(filepath);
  unit->name      = strdup(filepath);
  init(unit);
  return unit;
}

bl_unit_t *
bl_unit_new_str(const char *name, const char *src)
{
  bl_unit_t *unit = bl_calloc(1, sizeof(bl_unit_t));
  unit->filepath  = strdup(name);
  unit->name      = strdup(name);

  if (src)
    unit->src = strdup(src);
  else
    bl_abort("invalid source for %s unit", unit->name);

  init(unit);
  return unit;
}

void
bl_unit_delete(bl_unit_t *unit)
{
  free(unit->filepath);
  free(unit->src);
  free(unit->name);
  bl_tokens_terminate(&unit->tokens);
  bl_ast_terminate(&unit->ast);
  bl_free(unit);
}

const char *
bl_unit_get_src_file(bl_unit_t *unit)
{
  return unit->filepath;
}

const char *
bl_unit_get_src(bl_unit_t *unit)
{
  return unit->src;
}

const char *
bl_unit_get_name(bl_unit_t *unit)
{
  return unit->name;
}

const char *
bl_unit_get_src_ln(bl_unit_ref unit, int line, long *len)
{
  int         l    = 1;
  const char *iter = unit->src;
  while (iter && l != line) {
    ++l;
    iter = strchr(iter, '\n');
    iter = iter ? iter + 1 : NULL;
  }

  if (len) {
    long l = 0;
    if (iter)
       l = strchr(iter, '\n') - iter;
    if (l < 0)
      l = strlen(iter);
    (*len) = l;
  }

  return iter;
}
