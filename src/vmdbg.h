// =================================================================================================
// bl
//
// File:   vmdbg.h
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

#ifndef BL_VMDBG_H
#define BL_VMDBG_H

struct virtual_machine;
struct mir_instr;
struct mir_type;

enum vmdbg_stack_op {
	VMDBG_PUSH_RA,
	VMDBG_POP_RA,
	VMDBG_PUSH,
	VMDBG_POP,
};

void vmdbg_attach(struct virtual_machine *vm);
void vmdbg_detach(void);
void vmdbg_notify_instr(struct mir_instr *instr);
void vmdbg_notify_stack_op(enum vmdbg_stack_op op, struct mir_type *type, void *ptr);
void vmdbg_notify_stack_swap(void);
void vmdbg_break(void);

#endif
