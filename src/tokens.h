//*****************************************************************************
// bl
//
// File:   tokens.h
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

#ifndef TOKENS_H_Z3NM7BJC
#define TOKENS_H_Z3NM7BJC

#include <bobject/bobject.h>
#include <bobject/containers/string.h>
#include "token.h"

/* class Tokens declaration */
bo_decl_type_begin(Tokens, BObject)
  /* virtuals */
bo_end();

Tokens *
bl_tokens_new(BString *src);

void
bl_tokens_push(Tokens *self, bl_token_t *t);

bl_token_t *
bl_tokens_peek(Tokens *self);

bl_token_t *
bl_tokens_peek_2nd(Tokens *self);

bl_token_t *
bl_tokens_peek_nth(Tokens *self,
                   size_t  n);

bl_token_t *
bl_tokens_consume(Tokens *self);

void
bl_tokens_resert_iter(Tokens *self);

#endif /* end of include guard: TOKENS_H_Z3NM7BJC */
