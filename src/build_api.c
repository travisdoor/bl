//************************************************************************************************
// bl
//
// File:   build_api.c
// Author: Martin Dorazil
// Date:   8/1/20
//
// Copyright 2020 Martin Dorazil
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

/**
 * Those methods are used only during compilation pass to setup builder pipeline. No C interfaces
 * are needed here.
 */

#include "builder.h"
#include "common.h"

Assembly *
__add_executable(const char *name)
{
	// BL_LOG("Add executable '%s'.", name);
	if (!name) return NULL;

	Assembly *new_assembly = assembly_new(name);
	builder_add_assembly(new_assembly);

	return new_assembly;
}

Unit *
__add_unit(Assembly *assembly, const char *filepath)
{
	// BL_LOG("Add unit '%s' into assembly '%s'.", filepath, assembly->name);
	if (!assembly) return NULL;
	if (!filepath) return NULL;

	Unit *new_unit = unit_new_file(filepath, NULL, NULL);
	assembly_add_unit_unique(assembly, new_unit);

	return new_unit;
}
