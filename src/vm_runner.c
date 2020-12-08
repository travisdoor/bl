//************************************************************************************************
// bl
//
// File:   vm_runner.c
// Author: Martin Dorazil
// Date:   8.12.20
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

#include "assembly.h"
#include "bldebug.h"
#include "error.h"
#include "stages.h"
#include "vm.h"

s32 vm_entry_run(Assembly *assembly)
{
    VM *   vm    = &assembly->vm;
    MirFn *entry = assembly->entry;
    builder_note("\nExecuting 'main' in compile time...");
    if (!entry) {
        builder_error("Assembly '%s' has no entry function!", assembly->name);
        return EXIT_FAILURE;
    }
    MirType *fn_type = entry->type;
    BL_ASSERT(fn_type && fn_type->kind == MIR_TYPE_FN);
    BL_ASSERT(!fn_type->data.fn.args);
    vm_provide_command_line_arguments(vm, assembly->vm_argc, assembly->vm_argv);
    VMStackPtr ret_ptr = NULL;
    if (vm_execute_fn(vm, assembly, entry, &ret_ptr)) {
        if (ret_ptr) {
            MirType * ret_type = fn_type->data.fn.ret_type;
            const s64 result   = vm_read_int(ret_type, ret_ptr);
            builder_note("Execution finished with state: %lld\n", (long long)result);
            return (s32)result;
        } else {
            builder_note("Execution finished without errors");
        }
    } else {
        builder_note("Execution finished with errors");
    }
    return EXIT_SUCCESS;
}
