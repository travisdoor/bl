//*****************************************************************************
// bl
//
// File:   tokens.c
// Author: Martin Dorazil
// Date:   29.1.18
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

#include <bobject/containers/array.h>
#include <stdarg.h>
#include "tokens.h"
#include "bldebug.h"

/* class Tokens */

/* class Tokens constructor params */
bo_decl_params_begin(Tokens)
  /* constructor params */
  BString *src;
bo_end();

/* class Tokens object members */
bo_decl_members_begin(Tokens, BObject)
  /* members */
  BString *src;
  BArray  *buf;
  size_t   iter;
  size_t   marker;
bo_end();

bo_impl_type(Tokens, BObject);

void
TokensKlass_init(TokensKlass *klass)
{
}

void
Tokens_ctor(Tokens *self, TokensParams *p)
{
  /* constructor */
  self->src = bo_ref(p->src);
  self->buf = bo_array_new(sizeof(bl_token_t));
}

void
Tokens_dtor(Tokens *self)
{
  bo_unref(self->buf);
  bo_unref(self->src);
}

bo_copy_result
Tokens_copy(Tokens *self, Tokens *other)
{
  return BO_NO_COPY;
}
/* class Tokens end */

Tokens *
bl_tokens_new(BString *src)
{
  TokensParams p = {
    .src = src,
  };

  return bo_new(Tokens, &p);
}

void
bl_tokens_push(Tokens *self, bl_token_t *t)
{
  bo_array_push_back(self->buf, *t); 
}

bl_token_t *
bl_tokens_peek(Tokens *self)
{
  return bl_tokens_peek_nth(self, 1);
}

bl_token_t *
bl_tokens_peek_2nd(Tokens *self)
{
  return bl_tokens_peek_nth(self, 2);
}

bl_token_t *
bl_tokens_peek_nth(Tokens *self,
                   size_t  n)
{
  const size_t i = self->iter + n - 1;
  if (i < bo_array_size(self->buf))
    return &bo_array_at(self->buf, i, bl_token_t);

  return NULL;
}

bl_token_t *
bl_tokens_consume(Tokens *self)
{
  if (self->iter < bo_array_size(self->buf))
    return &bo_array_at(self->buf, self->iter++, bl_token_t);

  return NULL;
}

bool
bl_tokens_current_is(Tokens  *self,
                     bl_sym_e sym)
{
  return (&bo_array_at(self->buf, self->iter, bl_token_t))->sym == sym;
}

bool
bl_tokens_next_is(Tokens  *self,
                  bl_sym_e sym)
{
  return (&bo_array_at(self->buf, self->iter+1, bl_token_t))->sym == sym;
}

bool
bl_tokens_current_is_not(Tokens  *self,
                         bl_sym_e sym)
{
  return (&bo_array_at(self->buf, self->iter, bl_token_t))->sym != sym;
}

bool
bl_tokens_next_is_not(Tokens  *self,
                      bl_sym_e sym)
{
  return (&bo_array_at(self->buf, self->iter+1, bl_token_t))->sym != sym;
}

bool
bl_tokens_is_seq(Tokens *self,
                 int     cnt,
                 ...)
{
  bool ret     = true;
  size_t i     = self->iter;
  size_t c     = bo_array_size(self->buf);
  bl_sym_e sym = BL_SYM_EOF;

  va_list valist;
  va_start(valist, cnt);

  for (i = 0; i < cnt && i < c; i++) {
    sym = va_arg(valist, bl_sym_e);
    if ((&bo_array_at(self->buf, i, bl_token_t))->sym != sym) {
      ret = false;
      break;
    }
  }

  va_end(valist);
  return ret;
}

void
bl_tokens_set_marker(Tokens *self)
{
  self->marker = self->iter;
}

void
bl_tokens_back_to_marker(Tokens *self)
{
  self->iter = self->marker;
}

void
bl_tokens_resert_iter(Tokens *self)
{
  self->iter = 0;
}

