//************************************************************************************************
// blc
//
// File:   assembly.h
// Author: Martin Dorazil
// Date:   09/02/2018
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
//************************************************************************************************

#ifndef BL_ASSEMBLY_H
#define BL_ASSEMBLY_H

#include <bobject/bobject.h>
#include "bl/unit.h"

BO_BEGIN_DECLS

typedef struct bl_assembly *bl_assembly_ref;

extern BO_EXPORT bl_assembly_ref
bl_assembly_new(const char *name);

extern BO_EXPORT void
bl_assembly_delete(bl_assembly_ref assembly);

extern BO_EXPORT void
bl_assembly_add_unit(bl_assembly_ref assembly, bl_unit_ref unit);

extern BO_EXPORT int
bl_assembly_get_unit_count(bl_assembly_ref assembly);

extern BO_EXPORT bl_unit_ref
bl_assembly_get_unit(bl_assembly_ref assembly, int i);

extern BO_EXPORT const char *
bl_assembly_get_name(bl_assembly_ref assembly);

BO_END_DECLS

#endif // BL_ASSEMBLY_H
