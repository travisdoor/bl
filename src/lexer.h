//*****************************************************************************
// bl
//
// File:   lexer.h
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

#ifndef LEXER_H_HFL8RYQ5
#define LEXER_H_HFL8RYQ5

#include <bobject/bobject.h>
#include <bobject/containers/string.h>
#include "unit.h"
#include "token.h"
#include "error.h"

/* class declaration */
bo_decl_type_begin(Lexer, BObject)
  /* virtuals */
bo_end();

Lexer *
bl_lexer_new(bl_notify_f notif);

bool
bl_lexer_scan(Lexer *self, 
              BString *src);

bl_token_t *
bl_lexer_peek(Lexer *self);

bl_token_t *
bl_lexer_peek_2nd(Lexer *self);

bl_token_t *
bl_lexer_peek_nth(Lexer *self,
                   size_t  n);

bl_token_t *
bl_lexer_consume(Lexer *self);

bl_token_t *
bl_lexer_consume_if(Lexer  *self,
                     bl_sym_e sym);

bool
bl_lexer_current_is(Lexer  *self,
                     bl_sym_e sym);

bool
bl_lexer_next_is(Lexer  *self,
                  bl_sym_e sym);

bool
bl_lexer_current_is_not(Lexer  *self,
                         bl_sym_e sym);

bool
bl_lexer_next_is_not(Lexer  *self,
                      bl_sym_e sym);

bool
bl_lexer_is_seq(Lexer *self,
                 int     cnt,
                 ...);

void
bl_lexer_resert_iter(Lexer *self);

void
bl_lexer_set_marker(Lexer *self);

void
bl_lexer_back_to_marker(Lexer *self);

#endif /* end of include guard: LEXER_H_2F7YITOG */

