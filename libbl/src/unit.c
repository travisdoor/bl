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

#include <string.h>
#include "unit_impl.h"

/* class Unit */
bo_decl_params_begin(Unit)
  const char *filepath;
  const char *src;
bo_end();

bo_impl_type(Unit, Actor);

void
UnitKlass_init(UnitKlass *klass)
{
}

void
Unit_ctor(Unit *self, UnitParams *p)
{
  /* constructor */
  bo_parent_ctor(Actor, p);
  self->filepath = strdup(p->filepath);
  /* TODO: backslash on windows */
  self->name = strrchr(self->filepath, '/');
  if (self->name == NULL)
    self->name = self->filepath;
  else
    self->name++;

  if (p->src)
    self->src = strdup(p->src);

  bl_sym_tbl_init(&self->sym_tbl);
  bl_tokens_init(&self->tokens);
  bl_ast_init(&self->ast);
}

void
Unit_dtor(Unit *self)
{
  free(self->filepath);
  free(self->src);
  bl_tokens_terminate(&self->tokens);
  bl_ast_terminate(&self->ast);
  bl_sym_tbl_terminate(&self->sym_tbl);

  LLVMDisposeModule(self->module);
}

bo_copy_result
Unit_copy(Unit *self, Unit *other)
{
  return BO_NO_COPY;
}
/* class Unit end */

/* public */
Unit *
bl_unit_new_file(const char *filepath)
{
  UnitParams params = {
    .filepath = filepath,
    .src = NULL
  };
  return bo_new(Unit, &params);
}

Unit *
bl_unit_new_str(const char *name,
                const char *src)
{
  UnitParams params = {
    .filepath = name,
    .src = src
  };
  return bo_new(Unit, &params);
}

const char*
bl_unit_get_src_file(Unit *self)
{
  return self->filepath;
}

const char*
bl_unit_get_src(Unit *self)
{
  return self->src;
}

const char*
bl_unit_get_name(Unit *self)
{
  return self->name;
}

void
bl_unit_set_src(Unit *self,
                char *src)
{
  free(self->src);
  self->src = src;
}

LLVMModuleRef
bl_unit_get_module(Unit *self)
{
  return self->module;
}

void
bl_unit_set_llvm_module(Unit *self,
                        LLVMModuleRef module)
{
//  LLVMDisposeModule(self->module);
  self->module = module;
}
