//************************************************************************************************
// bl
//
// File:   vm.h
// Author: Martin Dorazil
// Date:   9/17/19
//
// Copyright 2019 Martin Dorazil
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

#ifndef BL_VM_H
#define BL_VM_H

#include "common.h"

/* Stack data manipulation helper macros. */
#define VM_STACK_PTR_DEREF(ptr) ((VMStackPtr) * ((uintptr_t *)(ptr)))

struct MirType;
struct MirInstr;
struct MirInstrBlock;
struct MirInstrCall;
struct MirInstrDeclVar;
struct MirFn;
struct MirVar;
struct Builder;
struct Assembly;

typedef u8        VMValue[16];
typedef ptrdiff_t VMRelativeStackPtr;
typedef u8 *      VMStackPtr;

typedef struct VMFrame {
	struct VMFrame * prev;
	struct MirInstr *caller; /* Optional */
} VMFrame;

typedef struct VMStack {
	VMStackPtr            top_ptr;         /* pointer to top of the stack */
	usize                 used_bytes;      /* size of the used stack in bytes */
	usize                 allocated_bytes; /* total allocated size of the stack in bytes */
	VMFrame *             ra;              /* current frame beginning (return address)*/
	struct MirInstr *     pc;         /* currently executed instruction (program counter) */
	struct MirInstrBlock *prev_block; /* used by phi instruction */
	bool                  aborted;    /* true when execution was aborted */
} VMStack;

typedef struct VM {
	VMStack *        stack;
	struct Assembly *assembly;
	TSmallArray_Char dyncall_sig_tmp;
	bool             aborted;
} VM;

void
vm_init(VM *vm, usize stack_size);

void
vm_terminate(VM *vm);

void
vm_execute_instr(VM *vm, struct Assembly *assembly, struct MirInstr *instr);

bool
vm_eval_instr(VM *vm, struct Assembly *assembly, struct MirInstr *instr);

bool
vm_execute_instr_top_level_call(VM *vm, struct Assembly *assembly, struct MirInstrCall *call);

bool
vm_execute_fn(VM *vm, struct Assembly *assembly, struct MirFn *fn, VMStackPtr *out_ptr);

/* Allocate space on the stack for passed variable in VM. This method works also for comptime
 * variables, but it's used only for implicit compiler generated variables without SetInitializer
 * instruction defined! When SetInitializer is used we can simply move memory pointer from
 * initialization value to variable const expression value (to safe memory and time needed by
 * copying).
 */
VMStackPtr
vm_alloc_global(VM *vm, struct Assembly *assembly, struct MirVar *var);

VMStackPtr
vm_alloc_raw(VM *vm, struct Assembly *assembly, struct MirType *type);

VMStackPtr
vm_read_var(VM *vm, struct MirVar *var);

#define vm_read_as(T, src) (*((T *)(src)))
#define vm_write_as(T, dest, src) (*((T *)(dest)) = (src))

u64
vm_read_int(struct MirType *type, VMStackPtr src);

f64
vm_read_double(struct MirType *type, VMStackPtr src);

f32
vm_read_float(struct MirType *type, VMStackPtr src);

VMStackPtr
vm_read_ptr(struct MirType *type, VMStackPtr src);

void
vm_write_int(struct MirType *type, VMStackPtr dest, u64 i);

void
vm_write_double(struct MirType *type, VMStackPtr dest, f64 i);

void
vm_write_float(struct MirType *type, VMStackPtr dest, f32 i);

void
vm_write_ptr(struct MirType *type, VMStackPtr dest, VMStackPtr ptr);

ptrdiff_t
vm_get_struct_elem_offset(struct Assembly *assembly, struct MirType *type, u32 i);

ptrdiff_t
vm_get_array_elem_offset(struct MirType *type, u32 i);

VMStackPtr
vm_get_struct_elem_ptr(struct Assembly *assembly, struct MirType *type, VMStackPtr ptr, u32 i);

VMStackPtr
vm_get_array_elem_ptr(struct MirType *type, VMStackPtr ptr, u32 i);

#endif
