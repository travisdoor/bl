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

BL_EXPORT Target *__add_target(const char *name, AssemblyKind kind)
{
    Target *target = builder_add_target(name);
    target->kind   = kind;
    return target;
}

BL_EXPORT void __add_unit(Target *target, const char *filepath)
{
    target_add_file(target, filepath);
}

BL_EXPORT void __add_lib_path(Target *target, const char *path)
{
    target_add_lib_path(target, path);
}

BL_EXPORT s32 __compile(Target *target)
{
    return builder_compile(target);
}

BL_EXPORT s32 __compile_all(void)
{
    return builder_compile_all();
}

BL_EXPORT void __link_library(Target *target, const char *name)
{
    target_add_lib(target, name);
}

BL_EXPORT void __append_linker_options(Target *target, const char *opt)
{
    target_append_linker_options(target, opt);
}

BL_EXPORT void __set_output_dir(Target *target, const char *dir)
{
    target_set_output_dir(target, dir);
}

BL_EXPORT const char *__get_output_dir(Target *target)
{
    BL_MAGIC_ASSERT(target);
    return target->out_dir.len > 0 ? target->out_dir.data : NULL;
}

BL_EXPORT void __set_module_dir(Target *target, const char *dir, const s32 policy)
{
    target_set_module_dir(target, dir, policy);
}

BL_EXPORT const char *__get_module_dir(Target *target)
{
    return target->module_dir.len > 0 ? target->module_dir.data : NULL;
}

BL_EXPORT s32 __get_module_import_policy(Target *target)
{
    BL_MAGIC_ASSERT(target);
    return target->module_policy;
}
