// =================================================================================================
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
// =================================================================================================

#include "bldebug.h"
#include "builder.h"
#include "error.h"
#include "vm.h"

#define TEXT_LINE "--------------------------------------------------------------------------------"

void vm_tests_run(struct assembly *assembly)
{
    struct virtual_machine *vm    = &assembly->vm;
    TArray *                cases = &assembly->testing.cases;
    printf("\nTesting start in compile time\n");
    printf(TEXT_LINE "\n");

    const usize    tc = cases->size;
    s32            fc = 0;
    struct mir_fn *test_fn;

    typedef struct {
        const char *name;
        f64         runtime_ms;
    } CaseMeta;

    TArray failed;
    tarray_init(&failed, sizeof(CaseMeta));

    TARRAY_FOREACH(struct mir_fn *, cases, test_fn)
    {
        BL_ASSERT(IS_FLAG(test_fn->flags, FLAG_TEST_FN));
        const f64   start      = get_tick_ms();
        const bool  passed     = vm_execute_fn(vm, assembly, test_fn, NULL);
        const f64   runtime_ms = get_tick_ms() - start;
        const char *name       = test_fn->id->str;
        if (passed) {
            printf("[ PASS |      ] %s (%f ms)\n", name, runtime_ms);
        } else {
            printf("[      | FAIL ] %s (%f ms)\n", name, runtime_ms);
            CaseMeta tmp = {.name = name, .runtime_ms = runtime_ms};
            tarray_push(&failed, tmp);
            builder.errorc = 0;
            ++fc;
        }
    }

    s32 perc = 100;
    if (fc > 0) perc = (s32)((f32)(tc - fc) / ((f32)tc * 0.01f));
    printf("\nResults:\n");
    printf(TEXT_LINE "\n");
    for (usize i = 0; i < failed.size; ++i) {
        CaseMeta *f = &tarray_at(CaseMeta, &failed, i);
        printf("[      | FAIL ] %s (%f ms)\n", f->name, f->runtime_ms);
    }

    if (failed.size) printf(TEXT_LINE "\n");
    printf("Executed: %llu, passed %d%%.\n", (unsigned long long)tc, perc);
    printf(TEXT_LINE "\n");
    tarray_terminate(&failed);
    assembly->vm_run.last_execution_status = fc;
}

void vm_build_entry_run(struct assembly *assembly)
{
    struct virtual_machine *vm     = &assembly->vm;
    struct mir_fn *         entry  = assembly->vm_run.build_entry;
    const struct target *   target = assembly->target;
    if (!entry) {
        builder_error("struct assembly '%s' has no build entry function!", assembly->target->name);
        assembly->vm_run.last_execution_status = EXIT_FAILURE;
        return;
    }
    if (target->vm.argc > 0) {
        vm_provide_command_line_arguments(vm, target->vm.argc, target->vm.argv);
    }
    vm_override_var(vm, assembly->vm_run.is_comptime_run, true);
    vm_execute_fn(vm, assembly, entry, NULL);
    vm_override_var(vm, assembly->vm_run.is_comptime_run, false);
    assembly->vm_run.last_execution_status = EXIT_SUCCESS;
}

void vm_entry_run(struct assembly *assembly)
{
    struct virtual_machine *vm     = &assembly->vm;
    struct mir_fn *         entry  = assembly->vm_run.entry;
    const struct target *   target = assembly->target;
    builder_note("\nExecuting 'main' in compile time...");
    if (!entry) {
        builder_error("struct assembly '%s' has no entry function!", assembly->target->name);
        assembly->vm_run.last_execution_status = EXIT_FAILURE;
        return;
    }
    struct mir_type *fn_type = entry->type;
    BL_ASSERT(fn_type && fn_type->kind == MIR_TYPE_FN);
    BL_ASSERT(!fn_type->data.fn.args);
    if (target->vm.argc > 0) {
        vm_provide_command_line_arguments(vm, target->vm.argc, target->vm.argv);
    }
    vm_override_var(vm, assembly->vm_run.is_comptime_run, true);
    vm_stack_ptr_t ret_ptr = NULL;
    s32            result  = EXIT_SUCCESS;
    if (vm_execute_fn(vm, assembly, entry, &ret_ptr)) {
        if (ret_ptr) {
            struct mir_type *ret_type = fn_type->data.fn.ret_type;
            result                    = (s32)vm_read_int(ret_type, ret_ptr);
            builder_note("Execution finished with state: %d\n", result);
        } else {
            builder_note("Execution finished without errors");
        }
    } else {
        builder_note("Execution finished with errors");
    }
    vm_override_var(vm, assembly->vm_run.is_comptime_run, false);
    assembly->vm_run.last_execution_status = result;
}
