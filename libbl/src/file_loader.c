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
#include "file_loader_impl.h"
#include "unit_impl.h"
#include "bl/bldebug.h"

static bool
run(FileLoader *self,
    Unit       *unit);

static int
domain(FileLoader *self);

/* FileLoader constructor parameters */
bo_decl_params_with_base_begin(FileLoader, Stage)
bo_end();

bo_impl_type(FileLoader, Stage);

/* FileLoader class init */
void
FileLoaderKlass_init(FileLoaderKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run 
    = (bool (*)(Stage*, Actor *)) run;
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
    bl_actor_error((Actor *)unit, "file not found %s", unit->filepath);
    return false;
  }

  fseek(f, 0, SEEK_END);
  size_t fsize = (size_t) ftell(f);
  if (fsize == 0) {
    fclose(f);
    bl_actor_error((Actor *)unit, "invalid source file %s", unit->filepath);
    return false;
  }

  fseek(f, 0, SEEK_SET);

  unit->src = malloc(sizeof(char) * (fsize + 1));
  fread(unit->src, fsize, 1, f);
  unit->src[fsize] = '\0';
  fclose(f);

  bl_log("processing file %s", unit->name);

  return true;
}

/* public */
FileLoader *
bl_file_loader_new(bl_compile_group_e group)
{
  FileLoaderParams p = {
    .base.group = group
  };

  return bo_new(FileLoader, &p);
}

