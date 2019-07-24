//*****************************************************************************
// Biscuit Object
//
// File:   bobject.c
// Author: Martin Dorazil
// Date:   27/10/2017
//
// Copyright 2017 Martin Dorazil
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
#include <stdlib.h>
#include <string.h>
#include "bobject/bobject.h"
#include "common_impl.h"

#define ERR_BAD_ALLOC "bad allocation"
#define ERR_NO_COPY "object is non-copyable"
#define ERR_COPY_FAIL "copying of object failed"

struct _BObjectKlass g_BObjectKlass =
  {
    .parent = NULL,
    .vtable = NULL,
    .size = 0,
    .ctor = NULL,
    .dtor = NULL,
    .copy = NULL,
    .initializer = NULL,
    .initialized = true,
    .type_name = "BObject"
  };

struct _BObjectVtable g_BObjectVtable = {0};

static void
init_klass(struct _Klass *cl)
{
  if (cl->initialized == false)
  {
    if (cl->parent->initializer != NULL)
    {
      init_klass(cl->parent);
    }

    memcpy(cl->vtable, cl->parent->vtable, cl->parent->vtab_size);
    cl->initializer(cl);
    cl->initialized = true;
  }
}

static inline void
chain_destruction(BObject *obj)
{
  struct _Klass *cl = obj->type;
  while (cl && cl->dtor) {
    cl->dtor(obj);
    cl = cl->parent;
  }
}

void *
_bo_new(struct _Klass *cl, void *p)
{
  /* initialize class binding once */
  init_klass(cl);

  BObject *obj = bo_calloc(1, cl->size);
  if (obj == NULL)
    bo_abort(ERR_BAD_ALLOC);

  obj->type = cl;
  bo_atomic_store(&obj->ref_count, 1);
  cl->ctor(obj, p);

  return obj;
}

BObject *
_bo_ref(BObject *obj)
{
  if (obj == NULL)
    return NULL;
  bo_atomic_add(&obj->ref_count, 1);
  return obj;
}

void
_bo_unref(BObject *obj)
{
  if (obj == NULL)
    return;

  if(bo_atomic_sub(&obj->ref_count, 1) == 1)
  {
    chain_destruction(obj);
    bo_free(obj);
  }
}

int
_bo_ref_count(BObject *obj)
{
  if (obj == NULL)
    return 0;

  return bo_atomic_load(&obj->ref_count);
}

BObject *
_bo_duplicate(BObject *src)
{
  if (src == NULL)
    return NULL;

  BObject *dest = bo_calloc(1, src->type->size);
  if (dest == NULL)
    bo_abort(ERR_BAD_ALLOC);

  dest->type = src->type;
  bo_atomic_store(&dest->ref_count, 1);

  bo_copy_result result;
  result = dest->type->copy(dest, src);
  switch (result) {
    case BO_COPY_SUCCESS:
      return dest;
    case BO_COPY_DEFAULT:
      memcpy(dest, src, dest->type->size);
      return dest;
    case BO_NO_COPY:
      bo_abort(ERR_NO_COPY);
    case BO_COPY_FAIL:
      bo_abort(ERR_COPY_FAIL);
    default:
      abort();
  }
}
