//************************************************************************************************
// blc
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
//************************************************************************************************

#ifndef BL_BUILDER_H
#define BL_BUILDER_H

#include "bl/assembly.h"
#include "bl/error.h"

BO_BEGIN_DECLS

#define BL_BUILDER_RUN 0x00000002
#define BL_BUILDER_PRINT_TOKENS 0x00000004
#define BL_BUILDER_PRINT_AST 0x00000008
#define BL_BUILDER_LOAD_FROM_FILE 0x00000010
#define BL_BUILDER_SYNTAX_ONLY 0x00000020
#define BL_BUILDER_EMIT_LLVM 0x00000040
#define BL_BUILDER_RUN_TESTS 0x00000080
#define BL_BUILDER_NO_BIN 0x00000100

typedef struct bl_builder *bl_builder_ref;
typedef void (*bl_diag_handler_f)(const char *, void *);

extern BO_EXPORT bl_builder_ref
                 bl_builder_new(void);

extern BO_EXPORT void
bl_builder_delete(bl_builder_ref builder);

extern BO_EXPORT bl_error_e
                 bl_builder_compile(bl_builder_ref builder, bl_assembly_ref assembly, uint32_t flags);

extern BO_EXPORT void
bl_builder_set_error_diag_handler(bl_builder_ref builder, bl_diag_handler_f handler, void *context);

extern BO_EXPORT void
bl_builder_set_warning_diag_handler(bl_builder_ref builder, bl_diag_handler_f handler,
                                    void *context);

BO_END_DECLS

#endif // BL_BUILDER_H
