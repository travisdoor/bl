// =================================================================================================
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
// =================================================================================================

// Those methods are used only during compilation pass to setup builder pipeline. No C interfaces
// are needed here.

#include "builder.h"
#include "common.h"

// @CLEANUP
// @CLEANUP
// @CLEANUP

BL_EXPORT Assembly *__add_executable(const char *name)
{
    /* @INCOMPLETE
    Assembly *new_assembly = assembly_new(ASSEMBLY_EXECUTABLE, name);
    builder_add_assembly(new_assembly);
    return new_assembly;
     */
    return NULL;
}

BL_EXPORT Assembly *__add_library(const char *name)
{
    /*
    Assembly *new_assembly = assembly_new(ASSEMBLY_SHARED_LIB, name);
    builder_add_assembly(new_assembly);
    return new_assembly;
     */
    return NULL;
}

BL_EXPORT Unit *__add_unit(Assembly *assembly, const char *filepath)
{
    return assembly_add_unit(assembly, filepath, NULL);
}

BL_EXPORT void __add_lib_path(Assembly *assembly, const char *path)
{
    assembly_add_lib_path(assembly, path);
}

BL_EXPORT s32 __compile(Assembly *assembly)
{
    if (!assembly) BL_ABORT("Invalid assembly!");
    //return builder_compile(assembly);
    return 0;
}

BL_EXPORT void __link_library(Assembly *assembly, const char *name)
{
    assembly_add_native_lib(assembly, name, NULL);
}

BL_EXPORT void __append_linker_options(Assembly *assembly, const char *opt)
{
    assembly_append_linker_options(assembly, opt);
}

BL_EXPORT void __set_output_dir(Assembly *assembly, const char *dir)
{
    assembly_set_output_dir(assembly, dir);
}

BL_EXPORT const char *__get_output_dir(Assembly *assembly)
{
    return assembly->out_dir.len > 0 ? assembly->out_dir.data : NULL;
}

BL_EXPORT void __set_module_dir(Assembly *assembly, const char *dir, const s32 policy)
{
    assembly_set_module_dir(assembly, dir, policy);
}

BL_EXPORT const char *__get_module_dir(Assembly *assembly)
{
    return assembly->module_dir.len > 0 ? assembly->module_dir.data : NULL;
}

BL_EXPORT s32 __get_module_import_policy(Assembly *assembly)
{
    return assembly->target->module_policy;
}
