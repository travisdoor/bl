//*****************************************************************************
// blc
//
// File:   unit.h
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

#ifndef UNIT_H_IDHOJTNW
#define UNIT_H_IDHOJTNW

#include <bobject/bobject.h>
#include <llvm-c/Core.h>

BO_BEGIN_DECLS

typedef struct bl_unit *bl_unit_ref;

extern BO_EXPORT bl_unit_ref
bl_unit_new_file(const char *filepath);

extern BO_EXPORT bl_unit_ref
bl_unit_new_str(const char *name, const char *src);

extern BO_EXPORT void
bl_unit_delete(bl_unit_ref unit);

extern BO_EXPORT const char *
bl_unit_get_src_file(bl_unit_ref unit);

extern BO_EXPORT const char *
bl_unit_get_src(bl_unit_ref unit);

extern BO_EXPORT const char *
bl_unit_get_name(bl_unit_ref unit);

BO_END_DECLS

#endif /* end of include guard: UNIT_H_IDHOJTNW */
