//*****************************************************************************
// biscuit
//
// File:   string.c
// Author: Martin Dorazil
// Date:   02/01/2018
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

#include "bobject/containers/string.h"
#include "common_impl.h"

/* class BString */
bo_decl_params_begin(BString)
  size_t      capacity;
  const char *str;
bo_end();

/* class BString object members */
bo_decl_members_begin(BString, BObject)
  /* members */
  char  *str;
  size_t len;
  size_t capacity;
bo_end();

bo_impl_type(BString, BObject);

void
BStringKlass_init(BStringKlass *klass)
{
}

void
BString_ctor(BString *self, BStringParams *p)
{
  /* constructor */
  bo_assert(p, "invalid params");

  /* initialize self */
  if (p->capacity == 0 || p->str == NULL) {
    bo_string_reserve(self, p->capacity);
    self->str[0] = '\0';
  }
  else
    bo_string_set(self, p->str);
}

void
BString_dtor(BString *self)
{
  bo_free(self->str);
}

bo_copy_result
BString_copy(BString *self, BString *other)
{
  self->str      = bo_malloc(sizeof(char) * (other->capacity + 1));
  self->len      = other->len;
  self->capacity = other->capacity;
  
  strcpy(self->str, other->str);
  return BO_COPY_SUCCESS;
}
/* class BString end */

BString *
bo_string_new(size_t capacity)
{
  BStringParams params = {
    .capacity = capacity,
    .str      = NULL
  };

  return bo_new(BString, &params);
}

BString *
bo_string_new_str(const char *str)
{
  BStringParams params = {
    .capacity = strlen(str),
    .str      = str 
  };

  return bo_new(BString, &params);
}

void
bo_string_set(BString *self, const char *str)
{
  bo_assert(bo_is_typeof(self, BString), "invalid string");
  const size_t len = strlen(str);
  if (len > self->capacity)
    bo_string_reserve(self, len);

  strcpy(self->str, str);
  self->len = len;
}

const char *
bo_string_get(BString *self)
{
  bo_assert(bo_is_typeof(self, BString), "invalid string");
  return self->str;
}

void
bo_string_append(BString *self, const char *str)
{
  bo_assert(bo_is_typeof(self, BString), "invalid string");
  bo_string_appendn(self, str, strlen(str));
}

void
bo_string_appendn(BString    *self,
                  const char *str,
                  size_t      n)
{
  const size_t new_len = self->len + n;
  if (new_len > self->capacity)
    bo_string_reserve(self, new_len);

  strncat(self->str, str, n);
  self->len = new_len;
}

void
bo_string_append_str(BString *self, BString *str)
{
  bo_assert(bo_is_typeof(self, BString) && bo_is_typeof(self, BString), "invalid string");
  bo_string_append(self, bo_string_get(str));
}

void
bo_string_pop_back(BString *self)
{
  bo_assert(bo_is_typeof(self, BString), "invalid string");
  if (self->len == 0)
    return;
  self->len--;
  self->str[self->len] = '\0';
}

void
bo_string_reserve(BString *self,
                  size_t   capacity)
{
  bo_assert(bo_is_typeof(self, BString), "invalid string");
  if (capacity + 1 <= self->capacity)
    return;

  self->str = bo_realloc(self->str, sizeof(char) * (capacity + 1));
  if (!self->str)
    bo_abort("cannot resize string to capacity: %zu", capacity);

  self->capacity = capacity;
}

size_t
bo_string_len(BString *self)
{
  bo_assert(bo_is_typeof(self, BString), "invalid string");
  return self->len;
}

void
bo_string_clear(BString *self)
{
  bo_assert(bo_is_typeof(self, BString), "invalid string");
  self->len = 0;
  self->str[0] = '\0';
}

int
bo_string_cmp(BString *self, BString *other)
{
  bo_assert(bo_is_typeof(self, BString) && bo_is_typeof(other, BString), "invalid string");
  return strcmp(self->str, other->str);
}

int
bo_string_tok(BString *self,
              char    *delimiter,
              char    *out[],
              int      max_tok)
{
  bo_assert(bo_is_typeof(self, BString), "invalid string");
  if (!max_tok || !out || !self->len)
    return 0;

  int i = 0;
  out[i] = strtok(self->str, delimiter);
  i++;

  while ((out[i] = strtok(NULL, delimiter)) != NULL && i < max_tok) {
    i++;
  }

  return i;
}

