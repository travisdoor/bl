//*****************************************************************************
// bl 
//
// File:   module.c
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
#include "module_impl.h"

/* Module members */
bo_decl_members_begin(Module, Actor)
bo_end();

/* Module constructor parameters */
bo_decl_params_begin(Module)
bo_end();

bo_impl_type(Module, Actor);

/* Module class init */
void
ModuleKlass_init(ModuleKlass *klass)
{
}

/* Module constructor */
void
Module_ctor(Module *self, ModuleParams *p)
{
  bo_parent_ctor(Actor, p);
}

/* Module destructor */
void
Module_dtor(Module *self)
{
}

/* Module copy constructor */
bo_copy_result
Module_copy(Module *self, Module *other)
{
  return BO_NO_COPY;
}

/* public */
Module *
bl_module_new(void)
{
  ModuleParams p = {
  };
  
  return bo_new(Module, &p);
}

