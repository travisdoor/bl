//*****************************************************************************
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
//*****************************************************************************

#include <stdio.h>
#include "file_loader.h"
#include "unit.h"
#include "domains.h"
#include "bldebug.h"
#include "bldebug.h"

static bool
run(FileLoader *self,
    Unit       *unit);

static int
domain(FileLoader *self);

/* FileLoader constructor parameters */
bo_decl_params_begin(FileLoader)
bo_end();

bo_impl_type(FileLoader, Stage);

/* FileLoader class init */
void
FileLoaderKlass_init(FileLoaderKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run 
    = (bool (*)(Stage*, Actor *)) run;
  bo_vtbl_cl(klass, Stage)->domain
    = (int (*)(Stage*)) domain;
}

/* FileLoader constructor */
void
FileLoader_ctor(FileLoader *self, FileLoaderParams *p)
{
  bo_parent_ctor(Stage, p);
}

/* FileLoader destructor */
void
FileLoader_dtor(FileLoader *self)
{
}

/* FileLoader copy constructor */
bo_copy_result
FileLoader_copy(FileLoader *self, FileLoader *other)
{
  return BO_NO_COPY;
}

bool
run(FileLoader *self,
    Unit       *unit)
{
  FILE *f = fopen(unit->filepath, "r");
  if (f == NULL) {
    bl_error("file %s not found\n", unit->filepath);
    return false;
  }

  fseek(f, 0, SEEK_END);
  size_t fsize = (size_t) ftell(f);
  if (fsize == 0) {
    fclose(f);
    bl_error("invalid source in file %s\n", unit->filepath);
    return false;
  }

  fseek(f, 0, SEEK_SET);

  unit->src = bo_string_new(fsize);
  fread((char *)bo_string_get(unit->src), fsize, 1, f);
  fclose(f);
  return true;
}

int
domain(FileLoader *self)
{
  return BL_DOMAIN_UNIT;
}

/* public */
FileLoader *
bl_file_loader_new(void)
{
    return bo_new(FileLoader, NULL);
}

