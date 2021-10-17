// =================================================================================================
// bl
//
// File:   vmdbg.c
// Author: Martin Dorazil
// Date:   27/09/2021
//
// Copyright 2021 Martin Dorazil
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

#include "vmdbg.h"
#include "builder.h"
#include "mir_printer.h"

enum state {
    CONTINUE,
    STEPPING,
};

static enum state              state         = CONTINUE;
static bool                    mir_mode      = false;
static bool                    verbose_stack = false;
static struct virtual_machine *current_vm    = NULL;

static void print(struct mir_instr *instr)
{
    if (!mir_mode && instr->node && instr->node->location) {
        builder_print_location(stdout, instr->node->location, 0, 0);
    } else {
        if (instr->prev) {
            printf("  ");
            mir_print_instr(stdout, current_vm->assembly, instr->prev);
        }
        printf("> ");
        mir_print_instr(stdout, current_vm->assembly, instr);
        if (instr->next) {
            printf("  ");
            mir_print_instr(stdout, current_vm->assembly, instr->next);
        }
    }
}

static void print_backtrace(void)
{
    struct vm_frame *frame = current_vm->stack->ra;
    s32              index = 1;
    while (frame) {
        struct mir_fn *fn =
            frame->caller ? mir_get_callee((struct mir_instr_call *)frame->caller) : NULL;
        if (fn) {
            const char *name = mir_get_fn_readable_name(fn);
            if (index == 1) {
                color_print(stdout, BL_YELLOW, "%3d: %s\n", index, name);
            } else {
                printf("%3d: %s\n", index, name);
            }
        } else {
            printf("%3d: (unknown)\n", index);
        }
        frame = frame->prev;
        ++index;
    }
}

static void cmd(void)
{
#define CMD(s, l) ((strcmp(buf, s) == 0) || (strcmp(buf, l) == 0))
    static char buf[256];
NEXT:
    printf(": ");
    if (scanf("%s", buf) != 1) goto NEXT;
    if (CMD("q", "quit")) {
        printf("exiting...\n");
        vmdbg_detach();
    } else if (CMD("n", "next")) {
        state = STEPPING;
    } else if (CMD("c", "continue")) {
        state = CONTINUE;
    } else if (CMD("p", "print")) {
        print(current_vm->stack->pc);
        goto NEXT;
    } else if (CMD("bt", "backtrace")) {
        print_backtrace();
        goto NEXT;
    } else if (CMD("vs=on", "verbose-stack=on")) {
        verbose_stack = true;
        goto NEXT;
    } else if (CMD("vs=off", "verbose-stack=off")) {
        verbose_stack = false;
        goto NEXT;
    } else if (CMD("mir=on", "mir-mode=on")) {
        mir_mode = true;
        goto NEXT;
    } else if (CMD("mir=off", "mir-mode=off")) {
        mir_mode = false;
        goto NEXT;
    } else if (CMD("h", "help")) {
        printf("  h, help                             = Show this help.\n"
               "  q, quit                             = Stop debugging.\n"
               "  n, next                             = Step to next instruction.\n"
               "  c, continue                         = Continue execution.\n"
               "  p, print                            = Print current instruction.\n"
               "  bt, backtrace                       = Print current backtrace.\n"
               "  vs=<on|off>, verbose-stack=<on|off> = Log stack operations.\n"
               "  mir=<on|off>, mir-mode=<on|off>     = Enable/disable mir instruction level "
               "debugging.\n");
        goto NEXT;
    } else {
        builder_error("Invalid command.");
        builder.errorc = 0; // @Hack: reset error count.
        goto NEXT;
    }
#undef CMD
}

// =================================================================================================
// Public
// =================================================================================================
void vmdbg_attach(struct virtual_machine *vm)
{
    bassert(current_vm == NULL);
    current_vm = vm;
}

void vmdbg_detach(void)
{
    current_vm = NULL;
    state      = CONTINUE;
}

void vmdbg_notify_instr(struct mir_instr *instr)
{
    static struct unit *last_unit = NULL;
    static u16          last_line = -1;
    if (!current_vm) return;
    if (state != STEPPING) return;
    if (mir_mode) {
        print(instr);
    } else if (instr->node && instr->node->location) {
        struct location *loc = instr->node->location;
        if (last_unit == loc->unit && last_line == loc->line) {
            return;
        }
        last_unit = loc->unit;
        last_line = loc->line;
        print(instr);
    } else {
        return;
    }
    cmd();
}

void vmdbg_notify_stack_op(enum vmdbg_stack_op op, struct mir_type *type, void *ptr)
{
    if (!current_vm) return;
    if (!verbose_stack) return;
    struct virtual_machine *vm = current_vm;
    switch (op) {
    case VMDBG_PUSH_RA:
        if (vm->stack->pc) {
            color_print(stdout,
                        BL_RED,
                        "%6zu %20s  PUSH RA\n",
                        (size_t)vm->stack->pc->id,
                        mir_instr_name(vm->stack->pc));
        } else {
            color_print(stdout, BL_RED, "     - %20s  PUSH RA\n", "Terminal");
        }
        break;
    case VMDBG_POP_RA:
        color_print(stdout,
                    BL_BLUE,
                    "%6llu %20s  POP RA\n",
                    vm->stack->pc->id,
                    mir_instr_name(vm->stack->pc));
        break;
    case VMDBG_PUSH: {
        unsigned long long size = type->store_size_bytes;
        char               type_name[256];
        mir_type_to_str(type_name, 256, type, true);
        if (vm->stack->pc) {
            color_print(stdout,
                        BL_RED,
                        "%6llu %20s  PUSH    (%lluB, %p) %s\n",
                        (unsigned long long)vm->stack->pc->id,
                        mir_instr_name(vm->stack->pc),
                        size,
                        ptr,
                        type_name);
        } else {
            color_print(stdout,
                        BL_RED,
                        "     -                       PUSH    (%lluB, %p) %s\n",
                        size,
                        ptr,
                        type_name);
        }
        break;
    }
    case VMDBG_POP: {
        unsigned long long size = type->store_size_bytes;
        char               type_name[256];
        mir_type_to_str(type_name, 256, type, true);
        if (vm->stack->pc) {
            color_print(stdout,
                        BL_BLUE,
                        "%6llu %20s  POP     (%lluB, %p) %s\n",
                        vm->stack->pc->id,
                        mir_instr_name(vm->stack->pc),
                        size,
                        vm->stack->top_ptr - size,
                        type_name);
        } else {
            color_print(stdout,
                        BL_BLUE,
                        "     -                       POP     (%lluB, %p) %s\n",
                        size,
                        vm->stack->top_ptr - size,
                        type_name);
        }
        break;
    }
    }
}

void vmdbg_break(void)
{
    if (!current_vm) return;
    printf("\nHit breakpoint in assembly '%s'.\n", current_vm->assembly->target->name);
    state = STEPPING;
    vmdbg_notify_instr(current_vm->stack->pc);
}
