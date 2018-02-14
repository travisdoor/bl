//*****************************************************************************
// bl
//
// File:   builder.h
// Author: Martin Dorazil
// Date:   14.2.18
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

#ifndef BL_BUILDER_H
#define BL_BUILDER_H

#include <bobject/bobject.h>
#include "bl/assembly.h"

BO_BEGIN_DECLS

#define BL_BUILDER_EXPORT_BC      0x00000001
#define BL_BUILDER_RUN            0x00000002
#define BL_BUILDER_PRINT_TOKENS   0x00000004
#define BL_BUILDER_PRINT_AST      0x00000008
#define BL_BUILDER_LOAD_FROM_FILE 0x00000010

/* class Builder declaration */
bo_decl_type_begin(Builder, BObject)
  /* virtuals */
bo_end();

extern BO_EXPORT Builder *
bl_builder_new(unsigned int flags);

extern BO_EXPORT bool
bl_builder_compile(Builder *self,
                   Assembly *assembly);

extern BO_EXPORT Actor *
bl_builder_get_failed(Builder *self);

BO_END_DECLS

#endif //BL_BUILDER_H
