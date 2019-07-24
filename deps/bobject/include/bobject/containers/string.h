//*****************************************************************************
// biscuit
//
// File:   string.h
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

#ifndef BOBJECT_STRING_H
#define BOBJECT_STRING_H

#include <bobject/bobject.h>
#include "bobject/utils.h"

BO_BEGIN_DECLS

/* class BString declaration */
bo_decl_type_begin(BString, BObject)
  /* virtuals */
bo_end();

extern BO_EXPORT BString *
bo_string_new(size_t capacity);

extern BO_EXPORT BString *
bo_string_new_str(const char *str);

extern BO_EXPORT void
bo_string_set(BString *self, const char *str);

extern BO_EXPORT const char *
bo_string_get(BString *self);

extern BO_EXPORT void
bo_string_append(BString *self, const char *str);

extern BO_EXPORT void
bo_string_appendn(BString    *self,
                  const char *str,
                  size_t      n);

extern BO_EXPORT void
bo_string_append_str(BString *self, BString *str);

extern BO_EXPORT void
bo_string_pop_back(BString *self);

extern BO_EXPORT void
bo_string_reserve(BString *self,
                  size_t   capacity);

extern BO_EXPORT size_t
bo_string_len(BString *self);

extern BO_EXPORT void
bo_string_clear(BString *self);

extern BO_EXPORT int
bo_string_cmp(BString *self, BString *other);

extern BO_EXPORT int
bo_string_tok(BString *self,
              char    *delimiter,
              char    *out[],
              int max_tok);

BO_END_DECLS

#endif //BOBJECT_STRING_H
