//*****************************************************************************
// blc
//
// File:   type.h
// Author: Martin Dorazil
// Date:   11/02/2018
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

#ifndef BL_TYPE_H
#define BL_TYPE_H

#include <bobject/bobject.h>

BO_BEGIN_DECLS

#define BL_TYPE_LIST \
  tp(NONE,    "") \
  tp(VOID,   "void") \
  tp(CHAR,   "char") \
  tp(BOOL,   "bool") \
  tp(I32,    "i32") \
  tp(I64,    "i64") \
  tp(F32,    "f32") \
  tp(F64,    "f64") \
  tp(STRING, "string") \

typedef enum {
#define tp(tok, str) BL_TYPE_##tok,
  BL_TYPE_LIST
#undef tp
  BL_TYPE_COUNT
} bl_type_e;

/* class Type declaration */
bo_decl_type_begin(Type, BObject)
  /* virtuals */
bo_end();

extern BO_EXPORT Type *
bl_type_new(const char *name);

extern  BO_EXPORT uint32_t
bl_type_get(Type *self);

extern  BO_EXPORT const char *
bl_type_get_name(Type *self);

extern  BO_EXPORT bool
bl_type_is(Type    *self,
           uint32_t t);

extern  BO_EXPORT bool
bl_type_is_fundamental(Type *self);

extern  BO_EXPORT bool
bl_type_is_user_defined(Type *self);

extern  BO_EXPORT bool
bl_type_is_not(Type    *self,
               uint32_t t);

BO_END_DECLS

#endif //BL_TYPE_H
