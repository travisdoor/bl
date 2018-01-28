//*****************************************************************************
// bl 
//
// File:   src_context.c
// Author: Martin Dorazil
// Date:   28/01/2018
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

#include <bobject/containers/hash.h>
#include "src_context.h"

/* SrcContext constructor parameters */
bo_decl_params_begin(SrcContext)
bo_end();

bo_impl_type(SrcContext, BObject);

/* SrcContext class init */
void
SrcContextKlass_init(SrcContextKlass *klass)
{
}

/* SrcContext constructor */
void
SrcContext_ctor(SrcContext *self, SrcContextParams *p)
{
  self->includes    = bo_string_new(256);
  self->fdecl       = bo_string_new(256);
  self->impl        = bo_string_new(1024);
  self->ext_mapping = bo_htbl_new(sizeof(bl_token_t *), 1024);
}

/* SrcContext destructor */
void
SrcContext_dtor(SrcContext *self)
{
  bo_unref(self->includes);
  bo_unref(self->fdecl);
  bo_unref(self->impl);
  bo_unref(self->ext_mapping);
}

/* SrcContext copy constructor */
bo_copy_result
SrcContext_copy(SrcContext *self, SrcContext *other)
{
  return BO_NO_COPY;
}

/* public */
SrcContext *
bl_src_context_new(void)
{
  return bo_new(SrcContext, NULL);
}


const bl_token_t *
bl_src_context_getem(SrcContext       *self,
                     const bl_token_t *name)
{
  uint32_t hash = bo_hash_from_strn(name->content.as_string, name->len);
  bo_iterator_t found = bo_htbl_find(self->ext_mapping, hash);
  bo_iterator_t end = bo_htbl_end(self->ext_mapping);
  if (bo_iterator_equal(&found, &end))
    return NULL;

  return bo_htbl_iter_peek_value(self->ext_mapping, &found, const bl_token_t *);
}

bool 
bl_src_context_addem(SrcContext       *self,
                     const bl_token_t *key,
                     const bl_token_t *value)
{
  uint32_t hash = bo_hash_from_strn(key->content.as_string, key->len);
  if (bo_htbl_has_key(self->ext_mapping, hash))
    return false;

  bo_htbl_insert(self->ext_mapping, hash, value);
  return true;
}

