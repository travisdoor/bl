//************************************************************************************************
// blc
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
//************************************************************************************************

#include <stdarg.h>
#include "tokens_impl.h"

void
bl_tokens_init(bl_tokens_t *tokens)
{
  tokens->buf          = bo_array_new(sizeof(bl_token_t));
  tokens->string_cache = bo_array_new_bo(bo_typeof(BString), true);
}

void
bl_tokens_terminate(bl_tokens_t *tokens)
{
  bo_unref(tokens->buf);
  bo_unref(tokens->string_cache);
}

void
bl_tokens_push(bl_tokens_t *tokens, bl_token_t *t)
{
  bo_array_push_back(tokens->buf, *t);
}

bl_token_t *
bl_tokens_peek(bl_tokens_t *tokens)
{
  return bl_tokens_peek_nth(tokens, 1);
}

bl_token_t *
bl_tokens_peek_2nd(bl_tokens_t *tokens)
{
  return bl_tokens_peek_nth(tokens, 2);
}

bl_token_t *
bl_tokens_peek_last(bl_tokens_t *tokens)
{
  const size_t i = bo_array_size(tokens->buf);
  if (i == 0) {
    return NULL;
  }

  return &bo_array_at(tokens->buf, i, bl_token_t);
}

bl_token_t *
bl_tokens_peek_prev(bl_tokens_t *tokens)
{
  if (tokens->iter > 0) {
    return &bo_array_at(tokens->buf, tokens->iter - 1, bl_token_t);
  }
  return NULL;
}

bl_token_t *
bl_tokens_peek_nth(bl_tokens_t *tokens, size_t n)
{
  const size_t i = tokens->iter + n - 1;
  if (i < bo_array_size(tokens->buf))
    return &bo_array_at(tokens->buf, i, bl_token_t);

  return NULL;
}

bl_token_t *
bl_tokens_consume(bl_tokens_t *tokens)
{
  if (tokens->iter < bo_array_size(tokens->buf))
    return &bo_array_at(tokens->buf, tokens->iter++, bl_token_t);

  return NULL;
}

bl_token_t *
bl_tokens_consume_if(bl_tokens_t *tokens, bl_sym_e sym)
{
  bl_token_t *tok;
  if (tokens->iter < bo_array_size(tokens->buf)) {
    tok = &bo_array_at(tokens->buf, tokens->iter, bl_token_t);
    if (tok->sym == sym) {
      tokens->iter++;
      return tok;
    }
  }

  return NULL;
}

bool
bl_tokens_current_is(bl_tokens_t *tokens, bl_sym_e sym)
{
  return (&bo_array_at(tokens->buf, tokens->iter, bl_token_t))->sym == sym;
}

bool
bl_tokens_previous_is(bl_tokens_t *tokens, bl_sym_e sym)
{
  if (tokens->iter > 0)
    return (&bo_array_at(tokens->buf, tokens->iter - 1, bl_token_t))->sym == sym;
  return false;
}

bool
bl_tokens_next_is(bl_tokens_t *tokens, bl_sym_e sym)
{
  return (&bo_array_at(tokens->buf, tokens->iter + 1, bl_token_t))->sym == sym;
}

bool
bl_tokens_current_is_not(bl_tokens_t *tokens, bl_sym_e sym)
{
  return (&bo_array_at(tokens->buf, tokens->iter, bl_token_t))->sym != sym;
}

bool
bl_tokens_next_is_not(bl_tokens_t *tokens, bl_sym_e sym)
{
  return (&bo_array_at(tokens->buf, tokens->iter + 1, bl_token_t))->sym != sym;
}

bool
bl_tokens_is_seq(bl_tokens_t *tokens, int cnt, ...)
{
  bool ret     = true;
  size_t c     = bo_array_size(tokens->buf);
  bl_sym_e sym = BL_SYM_EOF;
  cnt += tokens->iter;

  va_list valist;
  va_start(valist, cnt);

  for (size_t i = tokens->iter; i < cnt && i < c; ++i) {
    sym = va_arg(valist, bl_sym_e);
    if ((&bo_array_at(tokens->buf, i, bl_token_t))->sym != sym) {
      ret = false;
      break;
    }
  }

  va_end(valist);
  return ret;
}

void
bl_tokens_set_marker(bl_tokens_t *tokens)
{
  tokens->marker = tokens->iter;
}

void
bl_tokens_back_to_marker(bl_tokens_t *tokens)
{
  tokens->iter = tokens->marker;
}

void
bl_tokens_reset_iter(bl_tokens_t *tokens)
{
  tokens->iter = 0;
}

BArray *
bl_tokens_get_all(bl_tokens_t *tokens)
{
  return tokens->buf;
}

int
bl_tokens_count(bl_tokens_t *tokens)
{
  return (int)bo_array_size(tokens->buf);
}

BString *
bl_tokens_create_cached_str(bl_tokens_t *tokens)
{
  BString *str = bo_string_new(64);
  bo_array_push_back(tokens->string_cache, str);
  return str;
}
