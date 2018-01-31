//*****************************************************************************
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
//*****************************************************************************

#include <bobject/containers/string.h>
#include "unit.h"
#include "tokens.h"
#include "pnode.h"

#define return_with_error(e) \
  { \
    self->last_err = e; \
    return false;  \
  }

/* class Unit */
bo_decl_params_begin(Unit)
  const char *filepath;
bo_end();

/* class Unit object members */
bo_decl_members_begin(Unit, BObject)
  /* members */
  BString *filepath;
  BString *src;
  Tokens  *tokens;
  Pnode   *proot;

  /* error */
  bl_err   last_err;
bo_end();

bo_impl_type(Unit, BObject);

void
UnitKlass_init(UnitKlass *klass)
{
}

void
Unit_ctor(Unit *self, UnitParams *p)
{
  /* constructor */
  self->filepath = bo_string_new_str(p->filepath);
}

void
Unit_dtor(Unit *self)
{
  bo_unref(self->filepath);
  bo_unref(self->src);
}

bo_copy_result
Unit_copy(Unit *self, Unit *other)
{
  return BO_NO_COPY;
}
/* class Unit end */

static bool
load_file(Unit *self)
{
  FILE *f = fopen(bo_string_get(self->filepath), "r");
  if (f == NULL)
    return_with_error(BL_ERR_FILE_NOT_FOUND)

  fseek(f, 0, SEEK_END);
  size_t fsize = (size_t) ftell(f);
  if (fsize == 0) {
    fclose(f);
    return_with_error(BL_ERR_INVALID_SOURCE);
  }

  fseek(f, 0, SEEK_SET);

  self->src = bo_string_new(fsize);
  fread((char *)bo_string_get(self->src), fsize, 1, f);
  fclose(f);

  return true;
}

/* public */
Unit *
bl_unit_new(const char *filepath)
{
  return bo_new(Unit, NULL);
}

bool
bl_unit_compile(Unit *self)
{
  if (!load_file(self))
    return false;

  // pass to lexer
  // pass to parser
  // generate C source
  return true;
}

bl_err
bl_unit_last_error(Unit *self)
{
  return self->last_err;
}

