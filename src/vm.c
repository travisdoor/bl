//************************************************************************************************
// bl
//
// File:   vm.c
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

#include "vm.h"
#include "builder.h"
#include "mir.h"
#include "threading.h"

#define MAX_ALIGNMENT 8
#define VERBOSE_EXEC false
#define CHCK_STACK true
#define PTR_SIZE sizeof(void *) /* HACK: can cause problems with different build targets. */

// Debug helpers
#if BL_DEBUG && VERBOSE_EXEC
#define LOG_PUSH_RA                                                                                \
	{                                                                                          \
		if (vm->stack->pc) {                                                               \
			fprintf(stdout,                                                            \
			        "%6llu %20s  PUSH RA\n",                                           \
			        vm->stack->pc->id,                                                 \
			        mir_instr_name(vm->stack->pc));                                    \
		} else {                                                                           \
			fprintf(stdout, "     - %20s  PUSH RA\n", "Terminal");                     \
		}                                                                                  \
	}

#define LOG_POP_RA                                                                                 \
	{                                                                                          \
		fprintf(stdout,                                                                    \
		        "%6llu %20s  POP RA\n",                                                    \
		        vm->stack->pc->id,                                                         \
		        mir_instr_name(vm->stack->pc));                                            \
	}

#define LOG_PUSH_STACK                                                                             \
	{                                                                                          \
		char type_name[256];                                                               \
		mir_type_to_str(type_name, 256, type, true);                                       \
		if (vm->stack->pc) {                                                               \
			fprintf(stdout,                                                            \
			        "%6llu %20s  PUSH    (%luB, %p) %s\n",                             \
			        (unsigned long long)vm->stack->pc->id,                             \
			        mir_instr_name(vm->stack->pc),                                     \
			        size,                                                              \
			        tmp,                                                               \
			        type_name);                                                        \
		} else {                                                                           \
			fprintf(stdout,                                                            \
			        "     -                       PUSH    (%luB, %p) %s\n",            \
			        size,                                                              \
			        tmp,                                                               \
			        type_name);                                                        \
		}                                                                                  \
	}

#define LOG_POP_STACK                                                                              \
	{                                                                                          \
		char type_name[256];                                                               \
		mir_type_to_str(type_name, 256, type, true);                                       \
		if (vm->stack->pc) {                                                               \
			fprintf(stdout,                                                            \
			        "%6llu %20s  POP     (%luB, %p) %s\n",                             \
			        vm->stack->pc->id,                                                 \
			        mir_instr_name(vm->stack->pc),                                     \
			        size,                                                              \
			        vm->stack->top_ptr - size,                                         \
			        type_name);                                                        \
		} else {                                                                           \
			fprintf(stdout,                                                            \
			        "     -                       POP     (%luB, %p) %s\n",            \
			        size,                                                              \
			        vm->stack->top_ptr - size,                                         \
			        type_name);                                                        \
		}                                                                                  \
	}

#else
#define LOG_PUSH_RA
#define LOG_POP_RA
#define LOG_PUSH_STACK
#define LOG_POP_STACK
#endif

#if BL_DEBUG && CHCK_STACK
#define CHCK_SIZE() sizeof(void *)
#define CHCK_WRITE(_ptr, _data_size) memcpy((_ptr) + (_data_size), &(_ptr), CHCK_SIZE())
#define CHCK_VALIDATE(_ptr, _data_size)                                                            \
	if ((*(intptr_t *)((_ptr) + (_data_size))) != (intptr_t)(_ptr)) {                          \
		BL_ABORT("Stack memory malformed!");                                               \
	}
#else
#define CHCK_SIZE() 0
#define CHCK_WRITE(_ptr, _data_size)                                                               \
	while (0) {                                                                                \
	}

#define CHCK_VALIDATE(_ptr, _data_size)                                                            \
	while (0) {                                                                                \
	}
#endif

TSMALL_ARRAY_TYPE(ConstExprValue, MirConstExprValue, 32);

/*************/
/* fwd decls */
/*************/
static void
calculate_binop(MirType *  dest_type,
                MirType *  src_type,
                VMStackPtr dest,
                VMStackPtr lhs,
                VMStackPtr rhs,
                BinopKind  op);

static void
calculate_unop(VMStackPtr dest, VMStackPtr v, UnopKind op, MirType *type);

static void
do_cast(VMStackPtr dest, VMStackPtr src, MirType *dest_type, MirType *src_type, MirCastOp op);

static void
reset_stack(VMStack *stack);

/* zero max nesting = unlimited nesting */
static void
print_call_stack(VM *vm, usize max_nesting);

static void
dyncall_cb_read_arg(VM *vm, MirConstExprValue *dest, DCArgs *src);

static char
dyncall_cb_handler(DCCallback *cb, DCArgs *args, DCValue *result, void *userdata);

static void
_dyncall_generate_signature(VM *vm, MirType *type);

static const char *
dyncall_generate_signature(VM *vm, MirType *type);

static DCCallback *
dyncall_fetch_callback(VM *vm, MirFn *fn);

static void
dyncall_push_arg(VM *vm, VMStackPtr val_ptr, MirType *type);

static bool
execute_fn_top_level(VM *vm, MirInstr *call, VMStackPtr *out_ptr);

static bool
execute_fn_impl_top_level(VM *vm, MirFn *fn, TSmallArray_ConstExprValue *args, VMStackPtr *out_ptr);

static bool
_execute_fn_top_level(VM *                        vm,
                      MirFn *                     fn,
                      MirInstr *                  call,       /* Optional */
                      TSmallArray_ConstExprValue *arg_values, /* Optional */
                      VMStackPtr *                out_ptr     /* Optional */
);

static void
interp_instr(VM *vm, MirInstr *instr);

static void
interp_extern_call(VM *vm, MirFn *fn, MirInstrCall *call);

static void
interp_instr_toany(VM *vm, MirInstrToAny *toany);

static void
interp_instr_unreachable(VM *vm, MirInstrUnreachable *unr);

static void
interp_instr_phi(VM *vm, MirInstrPhi *phi);

static void
interp_instr_cast(VM *vm, MirInstrCast *cast);

static void
interp_instr_addrof(VM *vm, MirInstrAddrOf *addrof);

static void
interp_instr_br(VM *vm, MirInstrBr *br);

static void
interp_instr_switch(VM *vm, MirInstrSwitch *sw);

static void
interp_instr_elem_ptr(VM *vm, MirInstrElemPtr *elem_ptr);

static void
interp_instr_member_ptr(VM *vm, MirInstrMemberPtr *member_ptr);

static void
interp_instr_arg(VM *vm, MirInstrArg *arg);

static void
interp_instr_cond_br(VM *vm, MirInstrCondBr *br);

static void
interp_instr_load(VM *vm, MirInstrLoad *load);

static void
interp_instr_store(VM *vm, MirInstrStore *store);

static void
interp_instr_binop(VM *vm, MirInstrBinop *binop);

static void
interp_instr_unop(VM *vm, MirInstrUnop *unop);

static void
interp_instr_call(VM *vm, MirInstrCall *call);

static void
interp_instr_ret(VM *vm, MirInstrRet *ret);

static void
interp_instr_compound(VM *vm, VMStackPtr tmp_ptr, MirInstrCompound *init);

static void
interp_instr_vargs(VM *vm, MirInstrVArgs *vargs);

static void
interp_instr_decl_var(VM *vm, MirInstrDeclVar *decl);

static void
interp_instr_decl_ref(VM *vm, MirInstrDeclRef *ref);

static void
interp_instr_decl_direct_ref(VM *vm, MirInstrDeclDirectRef *ref);

static void
eval_instr(VM *vm, MirInstr *instr);

static void
eval_instr_type_info(VM *vm, MirInstrTypeInfo *type_info);

static void
eval_instr_member_ptr(VM *vm, MirInstrMemberPtr *member_ptr);

static void
eval_instr_elem_ptr(VM *vm, MirInstrElemPtr *elem_ptr);

static void
eval_instr_decl_var(VM *vm, MirInstrDeclVar *decl_var);

static void
eval_instr_decl_ref(VM *vm, MirInstrDeclRef *decl_ref);

static void
eval_instr_decl_direct_ref(VM *vm, MirInstrDeclDirectRef *decl_ref);

static void
eval_instr_binop(VM *vm, MirInstrBinop *binop);

static void
eval_instr_unop(VM *vm, MirInstrUnop *unop);

static void
eval_instr_load(VM *vm, MirInstrLoad *load);

static void
eval_instr_addrof(VM *vm, MirInstrAddrOf *addrof);

static void
eval_instr_set_initializer(VM *vm, MirInstrSetInitializer *si);

static void
eval_instr_cast(VM *vm, MirInstrCast *cast);

static void
eval_instr_compound(VM *vm, MirInstrCompound *compound);

/***********/
/* inlines */
/***********/
static inline bool
needs_tmp_alloc(MirConstExprValue *v)
{
	return v->type->store_size_bytes > sizeof(v->_tmp);
}

static inline MirFn *
get_callee(MirInstrCall *call)
{
	MirConstExprValue *val = &call->callee->value;
	BL_ASSERT(val->type && val->type->kind == MIR_TYPE_FN);

	MirFn *fn = MIR_CEV_READ_AS(MirFn *, val);
	BL_ASSERT(fn);
	return fn;
}

static inline void
exec_abort(VM *vm, s32 report_stack_nesting)
{
	print_call_stack(vm, report_stack_nesting);
	vm->stack->aborted = true;
}

static inline usize
stack_alloc_size(usize size)
{
	BL_ASSERT(size != 0);
	size += CHCK_SIZE();
	return size + (MAX_ALIGNMENT - (size % MAX_ALIGNMENT));
}

/* allocate memory on frame stack, size is in bits!!! */
static inline VMStackPtr
stack_alloc(VM *vm, usize size)
{
	BL_ASSERT(size && "trying to allocate 0 bits on stack");

#if BL_DEBUG && CHCK_STACK
	const usize orig_size = size;
#endif
	size = stack_alloc_size(size);
	vm->stack->used_bytes += size;
	if (vm->stack->used_bytes > vm->stack->allocated_bytes) {
		msg_error("Stack overflow!!!");
		exec_abort(vm, 10);
	}

	VMStackPtr mem     = (VMStackPtr)vm->stack->top_ptr;
	vm->stack->top_ptr = vm->stack->top_ptr + size;

	if (!is_aligned(mem, MAX_ALIGNMENT)) {
		BL_WARNING("BAD ALIGNMENT %p, %d bytes", mem, size);
	}

	CHCK_WRITE(mem, orig_size);

	return mem;
}

/* shift stack top by the size in bytes */
static inline VMStackPtr
stack_free(VM *vm, usize size)
{
#if BL_DEBUG && CHCK_STACK
	const usize orig_size = size;
#endif

	size               = stack_alloc_size(size);
	VMStackPtr new_top = vm->stack->top_ptr - size;
	if (new_top < (u8 *)(vm->stack->ra + 1)) BL_ABORT("Stack underflow!!!");
	vm->stack->top_ptr = new_top;
	vm->stack->used_bytes -= size;

	CHCK_VALIDATE(new_top, orig_size);

	return new_top;
}

static inline void
push_ra(VM *vm, MirInstr *caller)
{
	VMFrame *prev = vm->stack->ra;
	VMFrame *tmp  = (VMFrame *)stack_alloc(vm, sizeof(VMFrame));
	tmp->caller   = caller;
	tmp->prev     = prev;
	vm->stack->ra = tmp;
	LOG_PUSH_RA;
}

static inline MirInstr *
pop_ra(VM *vm)
{
	if (!vm->stack->ra) return NULL;
	MirInstr *caller = vm->stack->ra->caller;

	LOG_POP_RA;

	/* rollback */
	VMStackPtr new_top_ptr = (VMStackPtr)vm->stack->ra;
	vm->stack->used_bytes  = vm->stack->top_ptr - new_top_ptr;
	vm->stack->top_ptr     = new_top_ptr;
	vm->stack->ra          = vm->stack->ra->prev;
	return caller;
}

static inline VMStackPtr
stack_push_empty(VM *vm, MirType *type)
{
	BL_ASSERT(type);
	const usize size = type->store_size_bytes;
	BL_ASSERT(size && "pushing zero sized data on stack");
	VMStackPtr tmp = stack_alloc(vm, size);

	LOG_PUSH_STACK;
	return tmp;
}

static inline VMStackPtr
stack_push(VM *vm, void *value, MirType *type)
{
	BL_ASSERT(value && "try to push NULL value");
	VMStackPtr tmp = stack_push_empty(vm, type);
	memcpy(tmp, value, type->store_size_bytes);

	/* pointer relative to frame top */
	return tmp;
}

static inline VMStackPtr
stack_pop(VM *vm, MirType *type)
{
	BL_ASSERT(type);
	const usize size = type->store_size_bytes;
	BL_ASSERT(size && "popping zero sized data on stack");

	LOG_POP_STACK;

	return stack_free(vm, size);
}

/* Global variables are allocated in static data segment, so there is no need to
 * use relative pointer. When we set ignore to true original pointer is returned
 * as absolute pointer to the stack.  */
static inline VMStackPtr
stack_rel_to_abs_ptr(VM *vm, VMRelativeStackPtr rel_ptr, bool ignore)
{
	if (ignore) return (VMStackPtr)rel_ptr;
	BL_ASSERT(rel_ptr);

	VMStackPtr base = (VMStackPtr)vm->stack->ra;
	BL_ASSERT(base);
	return base + rel_ptr;
}

/* Fetch value into Temp  */
static inline VMStackPtr
fetch_value(VM *vm, MirConstExprValue *v)
{
	if (v->is_comptime) return v->data;
	return stack_pop(vm, v->type);
}

static inline MirInstr *
get_pc(VM *vm)
{
	return vm->stack->pc;
}

static inline VMFrame *
get_ra(VM *vm)
{
	return vm->stack->ra;
}

static inline void
set_pc(VM *vm, MirInstr *instr)
{
	vm->stack->pc = instr;
}

static inline VMRelativeStackPtr
stack_alloc_var(VM *vm, MirVar *var)
{
	BL_ASSERT(var);
	BL_ASSERT(!var->value.is_comptime && "cannot allocate compile time constant");
	/* allocate memory for variable on stack */

	VMStackPtr tmp     = stack_push_empty(vm, var->value.type);
	var->rel_stack_ptr = tmp - (VMStackPtr)vm->stack->ra;
	return var->rel_stack_ptr;
}

static inline void
stack_alloc_local_vars(VM *vm, MirFn *fn)
{
	BL_ASSERT(fn);
	/* Init all stack variables. */
	TArray *vars = fn->variables;
	MirVar *var;
	TARRAY_FOREACH(MirVar *, vars, var)
	{
		if (var->value.is_comptime) continue;
		stack_alloc_var(vm, var);
	}
}

/********/
/* impl */
/********/
void
calculate_binop(MirType *  dest_type,
                MirType *  src_type,
                VMStackPtr dest,
                VMStackPtr lhs,
                VMStackPtr rhs,
                BinopKind  op)
{
	/******************************************************************************************/
#define ARITHMETIC(T)                                                                              \
	case BINOP_ADD:                                                                            \
		VM_WRITE_AS(T, dest, VM_READ_AS(T, lhs) + VM_READ_AS(T, rhs));                     \
		break;                                                                             \
	case BINOP_SUB:                                                                            \
		VM_WRITE_AS(T, dest, VM_READ_AS(T, lhs) - VM_READ_AS(T, rhs));                     \
		break;                                                                             \
	case BINOP_MUL:                                                                            \
		VM_WRITE_AS(T, dest, VM_READ_AS(T, lhs) * VM_READ_AS(T, rhs));                     \
		break;                                                                             \
	case BINOP_DIV:                                                                            \
		if (VM_READ_AS(T, rhs) == 0) BL_ABORT("Divide by zero, this should be an error!"); \
		VM_WRITE_AS(T, dest, VM_READ_AS(T, lhs) * VM_READ_AS(T, rhs));                     \
		break;
	/******************************************************************************************/

	/******************************************************************************************/
#define RELATIONAL(T)                                                                              \
	case BINOP_EQ:                                                                             \
		VM_WRITE_AS(bool, dest, VM_READ_AS(T, lhs) == VM_READ_AS(T, rhs));                 \
		break;                                                                             \
	case BINOP_NEQ:                                                                            \
		VM_WRITE_AS(bool, dest, VM_READ_AS(T, lhs) != VM_READ_AS(T, rhs));                 \
		break;                                                                             \
	case BINOP_GREATER:                                                                        \
		VM_WRITE_AS(bool, dest, VM_READ_AS(T, lhs) > VM_READ_AS(T, rhs));                  \
		break;                                                                             \
	case BINOP_LESS:                                                                           \
		VM_WRITE_AS(bool, dest, VM_READ_AS(T, lhs) < VM_READ_AS(T, rhs));                  \
		break;                                                                             \
	case BINOP_LESS_EQ:                                                                        \
		VM_WRITE_AS(bool, dest, VM_READ_AS(T, lhs) <= VM_READ_AS(T, rhs));                 \
		break;                                                                             \
	case BINOP_GREATER_EQ:                                                                     \
		VM_WRITE_AS(bool, dest, VM_READ_AS(T, lhs) >= VM_READ_AS(T, rhs));                 \
		break;
	/******************************************************************************************/

	/******************************************************************************************/
#define LOGICAL(T)                                                                                 \
	case BINOP_AND:                                                                            \
		VM_WRITE_AS(T, dest, VM_READ_AS(T, lhs) & VM_READ_AS(T, rhs));                     \
		break;                                                                             \
	case BINOP_OR:                                                                             \
		VM_WRITE_AS(T, dest, VM_READ_AS(T, lhs) | VM_READ_AS(T, rhs));                     \
		break;
	/******************************************************************************************/

	/******************************************************************************************/
#define OTHER(T)                                                                                   \
	case BINOP_MOD:                                                                            \
		VM_WRITE_AS(T, dest, VM_READ_AS(T, lhs) % VM_READ_AS(T, rhs));                     \
		break;                                                                             \
	case BINOP_SHR:                                                                            \
		VM_WRITE_AS(T, dest, VM_READ_AS(T, lhs) >> VM_READ_AS(T, rhs));                    \
		break;                                                                             \
	case BINOP_SHL:                                                                            \
		VM_WRITE_AS(T, dest, VM_READ_AS(T, lhs) << VM_READ_AS(T, rhs));                    \
		break;
	/******************************************************************************************/

	/* Valid types: integers, floats, doubles, enums (as ints), bool, pointers. */

	const usize size    = src_type->store_size_bytes;
	const bool  is_real = src_type->kind == MIR_TYPE_REAL;
	const bool  is_signed =
	    src_type->kind == (MIR_TYPE_INT && src_type->data.integer.is_signed) ||
	    (src_type->kind == MIR_TYPE_ENUM && src_type->data.enm.base_type->data.integer.is_signed);

	if (is_real) { /* f32 or f64 */
		if (size == 4) {
			switch (op) {
				ARITHMETIC(f32)
				RELATIONAL(f32)
			default:
				BL_ABORT("Invalid binary operation!");
			}
		} else if (size == 8) {
			switch (op) {
				ARITHMETIC(f64)
				RELATIONAL(f64)
			default:
				BL_ABORT("Invalid binary operation!");
			}
		} else {
			abort();
		}
	} else if (is_signed) { /* signed integers  */
		if (size == 1) {
			switch (op) {
				ARITHMETIC(s8)
				RELATIONAL(s8)
				LOGICAL(s8)
				OTHER(s8)
			default:
				BL_ABORT("Invalid binary operation!");
			}
		} else if (size == 2) {
			switch (op) {
				ARITHMETIC(s16)
				RELATIONAL(s16)
				LOGICAL(s16)
				OTHER(s16)
			default:
				BL_ABORT("Invalid binary operation!");
			}
		} else if (size == 4) {
			switch (op) {
				ARITHMETIC(s32)
				RELATIONAL(s32)
				LOGICAL(s32)
				OTHER(s32)
			default:
				BL_ABORT("Invalid binary operation!");
			}
		} else if (size == 8) {
			switch (op) {
				ARITHMETIC(s64)
				RELATIONAL(s64)
				LOGICAL(s64)
				OTHER(s64)
			default:
				BL_ABORT("Invalid binary operation!");
			}
		} else {
			abort();
		}
	} else { /* unsigned integers */
		if (size == 1) {
			switch (op) {
				ARITHMETIC(u8)
				RELATIONAL(u8)
				LOGICAL(u8)
				OTHER(u8)
			default:
				BL_ABORT("Invalid binary operation!");
			}
		} else if (size == 2) {
			switch (op) {
				ARITHMETIC(u16)
				RELATIONAL(u16)
				LOGICAL(u16)
				OTHER(u16)
			default:
				BL_ABORT("Invalid binary operation!");
			}
		} else if (size == 4) {
			switch (op) {
				ARITHMETIC(u32)
				RELATIONAL(u32)
				LOGICAL(u32)
				OTHER(u32)
			default:
				BL_ABORT("Invalid binary operation!");
			}
		} else if (size == 8) {
			switch (op) {
				ARITHMETIC(u64)
				RELATIONAL(u64)
				LOGICAL(u64)
				OTHER(u64)
			default:
				BL_ABORT("Invalid binary operation!");
			}
		} else {
			abort();
		}
	}

#undef ARITHMETIC
#undef RELATIONAL
#undef LOGICAL
#undef OTHER
}

void
calculate_unop(VMStackPtr dest, VMStackPtr v, UnopKind op, MirType *type)
{
	/******************************************************************************************/
#define UNOP_CASE(T)                                                                               \
	case sizeof(T): {                                                                          \
		switch (op) {                                                                      \
		case UNOP_NOT:                                                                     \
			VM_WRITE_AS(T, dest, !VM_READ_AS(T, v));                                   \
			break;                                                                     \
		case UNOP_NEG:                                                                     \
			VM_WRITE_AS(T, dest, VM_READ_AS(T, v) * -1);                               \
			break;                                                                     \
		case UNOP_POS:                                                                     \
			VM_WRITE_AS(T, dest, VM_READ_AS(T, v));                                    \
			break;                                                                     \
		default:                                                                           \
			BL_UNIMPLEMENTED;                                                          \
		}                                                                                  \
	} break;
	/******************************************************************************************/

	const usize s = type->store_size_bytes;

	switch (type->kind) {
	case MIR_TYPE_BOOL:
	case MIR_TYPE_INT: {
		if (type->data.integer.is_signed) {
			switch (s) {
				UNOP_CASE(s8);
				UNOP_CASE(s16);
				UNOP_CASE(s32);
				UNOP_CASE(s64);
			default:
				BL_ABORT("invalid integer data type");
			}
		} else {
			switch (s) {
				UNOP_CASE(u8);
				UNOP_CASE(u16);
				UNOP_CASE(u32);
				UNOP_CASE(u64);
			default:
				BL_ABORT("invalid integer data type");
			}
		}
		break;
	}

	case MIR_TYPE_REAL: {
		switch (s) {
			UNOP_CASE(f32);
			UNOP_CASE(f64);
		default:
			BL_ABORT("invalid real data type");
		}
		break;
	}

	default:
		BL_ABORT("invalid unop type");
	}
#undef UNOP_CASE
}

void
do_cast(VMStackPtr dest, VMStackPtr src, MirType *dest_type, MirType *src_type, MirCastOp op)
{
	BL_ASSERT(dest && "Missing cast destination!");
	BL_ASSERT(src && "Missing cast source!");
	BL_ASSERT(dest_type && "Missing cast destination type!");
	BL_ASSERT(src_type && "Missing cast source type!");

	const usize src_size  = src_type->store_size_bytes;
	const usize dest_size = dest_type->store_size_bytes;

	switch (op) {
	case MIR_CAST_INTTOPTR:
	case MIR_CAST_PTRTOINT:
	case MIR_CAST_NONE:
	case MIR_CAST_BITCAST:
	case MIR_CAST_ZEXT:
	case MIR_CAST_TRUNC:
		memset(dest, 0, dest_size);
		memcpy(dest, src, src_size);
		break;

	case MIR_CAST_SEXT: {
		/* src is smaller than dest */
		const u64 v = vm_read_int(src_type, src);
		vm_write_int(dest_type, dest, v);
		break;
	}

	case MIR_CAST_FPEXT: {
		/* src is smaller than dest */
		const f32 v = vm_read_float(src_type, src);
		vm_write_double(dest_type, dest, (f64)v);
		break;
	}

	case MIR_CAST_FPTRUNC: {
		/* src is bigger than dest */
		const f64 v = vm_read_double(src_type, src);
		vm_write_float(dest_type, dest, (f32)v);
		break;
	}

	case MIR_CAST_FPTOUI:
	case MIR_CAST_FPTOSI: {
		/* real to signed integer */
		switch (src_size) {
		case 4: {
			const f32 v = vm_read_float(src_type, src);
			vm_write_int(dest_type, dest, (u64)v);
			break;
		}

		case 8: {
			const f64 v = vm_read_double(src_type, src);
			vm_write_int(dest_type, dest, (u64)v);
			break;
		}
		default:
			BL_ABORT("Invalid!");
		}
		break;
	}

	case MIR_CAST_SITOFP: {
		/* signed integer real */
		const s64 v = (s64)vm_read_int(src_type, src);
		switch (dest_size) {
		case 4: {
			vm_write_float(dest_type, dest, v);
			break;
		}

		case 8: {
			vm_write_double(dest_type, dest, v);
			break;
		}
		default:
			BL_ABORT("Invalid!");
		}
		break;
	}

	case MIR_CAST_UITOFP: {
		const u64 v = vm_read_int(src_type, src);
		switch (dest_size) {
		case 4: {
			vm_write_float(dest_type, dest, v);
			break;
		}

		case 8: {
			vm_write_double(dest_type, dest, v);
			break;
		}
		default:
			BL_ABORT("Invalid!");
		}
		break;
	}

	default:
		BL_ABORT("invalid cast operation");
	}
}

void
print_call_stack(VM *vm, usize max_nesting)
{
	MirInstr *instr = vm->stack->pc;
	VMFrame * fr    = vm->stack->ra;
	usize     n     = 0;

	if (!instr) return;
	/* print last instruction */
	builder_msg(BUILDER_MSG_LOG, 0, instr->node->location, BUILDER_CUR_WORD, "");

	while (fr) {
		instr = (MirInstr *)fr->caller;
		fr    = fr->prev;
		if (!instr) break;

		if (max_nesting && n == max_nesting) {
			msg_note("continue...");
			break;
		}

		builder_msg(BUILDER_MSG_LOG, 0, instr->node->location, BUILDER_CUR_WORD, "");
		++n;
	}
}

void
reset_stack(VMStack *stack)
{
	stack->pc         = NULL;
	stack->ra         = NULL;
	stack->prev_block = NULL;
	stack->aborted    = false;
	const usize size  = stack_alloc_size(sizeof(VMStack));
	stack->used_bytes = size;
	stack->top_ptr    = (u8 *)stack + size;
}

void
dyncall_cb_read_arg(VM *vm, MirConstExprValue *dest, DCArgs *src)
{
	BL_ASSERT(dest->type && "Argument destination has no type specified.");

	memset(&dest->data, 0, sizeof(dest->_tmp));

	switch (dest->type->kind) {
	case MIR_TYPE_INT: {
		const usize bitcount = (usize)dest->type->data.integer.bitcount;
		switch (bitcount) {
		case 8:
			MIR_CEV_WRITE_AS(u8, dest, dcbArgUChar(src));
			break;
		case 16:
			MIR_CEV_WRITE_AS(u16, dest, dcbArgUShort(src));
			break;
		case 32:
			MIR_CEV_WRITE_AS(u32, dest, dcbArgULong(src));
			break;
		case 64:
			MIR_CEV_WRITE_AS(u64, dest, dcbArgULongLong(src));
			break;
		default:
			BL_ABORT("invalid bitcount");
		}

		break;
	}

	case MIR_TYPE_REAL: {
		const u64 bitcount = (u64)dest->type->data.real.bitcount;
		switch (bitcount) {
		case 32:
			MIR_CEV_WRITE_AS(f32, dest, dcbArgFloat(src));
			break;
		case 64:
			MIR_CEV_WRITE_AS(f64, dest, dcbArgDouble(src));
			break;
		default:
			BL_ABORT("invalid bitcount");
		}

		break;
	}

	case MIR_TYPE_BOOL: {
		MIR_CEV_WRITE_AS(bool, dest, dcbArgBool(src));
		break;
	}

	case MIR_TYPE_PTR: {
		MIR_CEV_WRITE_AS(VMStackPtr, dest, dcbArgPointer(src));
		break;
	}

	default:
		BL_UNIMPLEMENTED;
	}
}

char
dyncall_cb_handler(DCCallback *cb, DCArgs *dc_args, DCValue *result, void *userdata)
{
	/* TODO: External callback can be invoked from different thread. This can cause problems for
	 * now since interpreter is strictly single-threaded, but we must handle such situation in
	 * future. */
	BL_ASSERT(thread_get_id() == main_thread_id &&
	          "External callback handler must be invoked from main thread.");

	DyncallCBContext *cnt = (DyncallCBContext *)userdata;
	MirFn *           fn  = cnt->fn;
	VM *              vm  = cnt->vm;
	BL_ASSERT(fn && vm);

	MirType *  ret_type     = fn->type->data.fn.ret_type;
	const bool is_fn_extern = IS_FLAG(cnt->fn->flags, FLAG_EXTERN);
	const bool has_args     = fn->type->data.fn.args;
	const bool has_return   = ret_type->kind != MIR_TYPE_VOID;

	if (is_fn_extern) {
		/* TODO: external callback */
		/* TODO: external callback */
		/* TODO: external callback */
		BL_ABORT("External function used as callback is not supported yet!");
	}

	TSmallArray_ConstExprValue arg_tmp;
	tsa_init(&arg_tmp);

	if (has_args) {
		TSmallArray_ArgPtr *args = fn->type->data.fn.args;
		tsa_resize_ConstExprValue(&arg_tmp, args->size);

		MirArg *it;
		TSA_FOREACH(args, it)
		{
			arg_tmp.data[i].type = it->type;
			dyncall_cb_read_arg(vm, &arg_tmp.data[i], dc_args);
		}
	}

	VMStackPtr ret_ptr = NULL;
	if (!execute_fn_impl_top_level(vm, fn, &arg_tmp, &ret_ptr)) {
		result->L = 0;
	} else if (has_return) {
		BL_ASSERT(ret_ptr && "Function is supposed to return some value.");
		result->L = vm_read_int(ret_type, ret_ptr);
	}

	tsa_terminate(&arg_tmp);
	return dyncall_generate_signature(vm, ret_type)[0];
}

void
_dyncall_generate_signature(VM *vm, MirType *type)
{
	TSmallArray_Char *tmp = &vm->dyncall_sig_tmp;

	switch (type->kind) {
	case MIR_TYPE_FN: {
		if (type->data.fn.args) {
			MirArg *arg;
			TSA_FOREACH(type->data.fn.args, arg)
			{
				_dyncall_generate_signature(vm, arg->type);
			}
		}
		tsa_push_Char(tmp, DC_SIGCHAR_ENDARG);
		_dyncall_generate_signature(vm, type->data.fn.ret_type);
		break;
	}

	case MIR_TYPE_INT: {
		const bool is_signed = type->data.integer.is_signed;
		switch (type->store_size_bytes) {
		case 1:
			tsa_push_Char(tmp, is_signed ? DC_SIGCHAR_CHAR : DC_SIGCHAR_UCHAR);
			break;
		case 2:
			tsa_push_Char(tmp, is_signed ? DC_SIGCHAR_SHORT : DC_SIGCHAR_USHORT);
			break;
		case 4:
			tsa_push_Char(tmp, is_signed ? DC_SIGCHAR_INT : DC_SIGCHAR_UINT);
			break;
		case 8:
			tsa_push_Char(tmp, is_signed ? DC_SIGCHAR_LONGLONG : DC_SIGCHAR_ULONGLONG);
			break;
		}
		break;
	}

	case MIR_TYPE_REAL: {
		switch (type->store_size_bytes) {
		case 4:
			tsa_push_Char(tmp, DC_SIGCHAR_FLOAT);
			break;
		case 8:
			tsa_push_Char(tmp, DC_SIGCHAR_DOUBLE);
			break;
		}
		break;
	}

	case MIR_TYPE_NULL:
	case MIR_TYPE_PTR: {
		tsa_push_Char(tmp, DC_SIGCHAR_POINTER);
		break;
	}

	case MIR_TYPE_VOID: {
		tsa_push_Char(tmp, DC_SIGCHAR_VOID);
		break;
	}

	case MIR_TYPE_STRUCT: {
		TSmallArray_MemberPtr *members = type->data.strct.members;
		MirMember *            member;
		TSA_FOREACH(members, member)
		{
			_dyncall_generate_signature(vm, member->type);
		}
		break;
	}

	case MIR_TYPE_ENUM: {
		_dyncall_generate_signature(vm, type->data.enm.base_type);
		break;
	}

	case MIR_TYPE_ARRAY: {
		for (s64 i = 0; i < type->data.array.len; i += 1) {
			_dyncall_generate_signature(vm, type->data.array.elem_type);
		}
		break;
	}

	default: {
		char type_name[256];
		mir_type_to_str(type_name, 256, type, true);
		BL_ABORT("Unsupported DC-signature type '%s'.", type_name);
	}
	}
}

const char *
dyncall_generate_signature(VM *vm, MirType *type)
{
	TSmallArray_Char *tmp = &vm->dyncall_sig_tmp;
	tmp->size             = 0; /* reset size */

	_dyncall_generate_signature(vm, type);
	tsa_push_Char(tmp, '\0');

	return tmp->data;
}

DCCallback *
dyncall_fetch_callback(VM *vm, MirFn *fn)
{
	if (fn->dyncall.extern_callback_handle) return fn->dyncall.extern_callback_handle;

	const char *sig = dyncall_generate_signature(vm, fn->type);

	fn->dyncall.context = (DyncallCBContext){.fn = fn, .vm = vm};

	fn->dyncall.extern_callback_handle =
	    dcbNewCallback(sig, &dyncall_cb_handler, &fn->dyncall.context);

	return fn->dyncall.extern_callback_handle;
}

void
dyncall_push_arg(VM *vm, VMStackPtr val_ptr, MirType *type)
{
	BL_ASSERT(type);

	DCCallVM *dvm = vm->assembly->dl.vm;
	BL_ASSERT(dvm);

	if (type->kind == MIR_TYPE_ENUM) {
		type = type->data.enm.base_type;
	}

	switch (type->kind) {
	case MIR_TYPE_BOOL: {
		dcArgBool(dvm, vm_read_int(type, val_ptr));
		break;
	}

	case MIR_TYPE_INT: {
		const u64 v = vm_read_int(type, val_ptr);
		switch (type->store_size_bytes) {
		case 1:
			dcArgChar(dvm, v);
			break;
		case 2:
			dcArgShort(dvm, v);
			break;
		case 4:
			dcArgInt(dvm, v);
			break;
		case 8:
			dcArgLongLong(dvm, v);
			break;
		default:
			BL_ABORT("unsupported external call integer argument type");
		}
		break;
	}

	case MIR_TYPE_REAL: {
		switch (type->store_size_bytes) {
		case 4:
			dcArgFloat(dvm, vm_read_float(type, val_ptr));
			break;
		case 8:
			dcArgDouble(dvm, vm_read_double(type, val_ptr));
			break;
		default:
			BL_ABORT("unsupported external call integer argument type");
		}
		break;
	}

	case MIR_TYPE_NULL: {
		dcArgPointer(dvm, NULL);
		break;
	}

	case MIR_TYPE_STRUCT: {
		BL_ABORT(
		    "External function taking structure argument by value cannot be executed by "
		    "interpreter on this platform.");
		break;
	}

	case MIR_TYPE_ARRAY: {
		BL_ABORT("External function taking array argument by value cannot be executed by "
		         "interpreter on this platform.");
		break;
	}

	case MIR_TYPE_PTR: {
		VMStackPtr tmp = vm_read_ptr(type, val_ptr);
		if (mir_deref_type(type)->kind == MIR_TYPE_FN) {
			BL_UNIMPLEMENTED_REGION(
			    MirConstValue *value = (MirConstValue *)val_ptr;
			    BL_ASSERT(value->type->kind == MIR_TYPE_PTR);
			    BL_ASSERT(value->data.v_ptr.data.any);
			    BL_ASSERT(value->data.v_ptr.kind == MIR_CP_VALUE);

			    value = value->data.v_ptr.data.value;
			    BL_ASSERT(value->type->kind == MIR_TYPE_FN);
			    BL_ASSERT(value->data.v_ptr.data.any);
			    BL_ASSERT(value->data.v_ptr.kind == MIR_CP_FN);

			    MirFn *fn = value->data.v_ptr.data.fn;
			    dcArgPointer(dvm, (DCpointer)dyncall_fetch_callback(vm, fn)););
		} else {
			dcArgPointer(dvm, (DCpointer)tmp);
		}
		break;
	}

	default:
		BL_ABORT("unsupported external call argument type");
	}
}

void
interp_extern_call(VM *vm, MirFn *fn, MirInstrCall *call)
{
	MirType *ret_type = fn->type->data.fn.ret_type;
	BL_ASSERT(ret_type);

	DCCallVM *dvm = vm->assembly->dl.vm;
	BL_ASSERT(vm);

	/* call setup and clenup */
	if (!fn->dyncall.extern_entry) {
		msg_error("External function '%s' not found!", fn->linkage_name);
		exec_abort(vm, 0);
		return;
	}

	dcMode(dvm, DC_CALL_C_DEFAULT);
	dcReset(dvm);

	/* pop all arguments from the stack */
	VMStackPtr            arg_ptr;
	TSmallArray_InstrPtr *arg_values = call->args;
	if (arg_values) {
		MirInstr *arg_value;
		TSA_FOREACH(arg_values, arg_value)
		{
			arg_ptr = fetch_value(vm, &arg_value->value);
			dyncall_push_arg(vm, arg_ptr, arg_value->value.type);
		}
	}

	bool does_return = true;

	VMValue result = {0};
	switch (ret_type->kind) {
	case MIR_TYPE_ENUM:
	case MIR_TYPE_INT:
		switch (ret_type->store_size_bytes) {
		case 1:
			VM_WRITE_AS(s8, &result, dcCallChar(dvm, fn->dyncall.extern_entry));
			break;
		case 2:
			VM_WRITE_AS(s8, &result, dcCallShort(dvm, fn->dyncall.extern_entry));
			break;
		case 4:
			VM_WRITE_AS(s8, &result, dcCallInt(dvm, fn->dyncall.extern_entry));
			break;
		case 8:
			VM_WRITE_AS(s8, &result, dcCallLongLong(dvm, fn->dyncall.extern_entry));
			break;
		default:
			BL_ABORT("unsupported integer size for external call result");
		}
		break;

	case MIR_TYPE_PTR:
		VM_WRITE_AS(VMStackPtr, &result, dcCallPointer(dvm, fn->dyncall.extern_entry));
		break;

	case MIR_TYPE_REAL: {
		switch (ret_type->store_size_bytes) {
		case 4:
			VM_WRITE_AS(f32, &result, dcCallFloat(dvm, fn->dyncall.extern_entry));
			break;
		case 8:
			VM_WRITE_AS(f64, &result, dcCallDouble(dvm, fn->dyncall.extern_entry));
			break;
		default:
			BL_ABORT("Unsupported real number size for external call "
			         "result");
		}
		break;
	}

	case MIR_TYPE_VOID:
		dcCallVoid(dvm, fn->dyncall.extern_entry);
		does_return = false;
		break;

	case MIR_TYPE_STRUCT: {
		BL_ABORT("External function '%s' returning structure cannot be executed by "
		         "interpreter on "
		         "this platform.",
		         fn->id->str);
	}

	case MIR_TYPE_ARRAY: {
		BL_ABORT(
		    "External function '%s' returning array cannot be executed by interpreter on "
		    "this platform.",
		    fn->id->str);
	}

	default: {
		char type_name[256];
		mir_type_to_str(type_name, 256, ret_type, true);
		BL_ABORT("Unsupported external call return type '%s'", type_name);
	}
	}

	/* PUSH result only if it is used */
	if (call->base.ref_count > 1 && does_return) {
		stack_push(vm, (VMStackPtr)&result, ret_type);
	}
}

bool
execute_fn_top_level(VM *vm, MirInstr *call, VMStackPtr *out_ptr)
{
	return _execute_fn_top_level(vm, get_callee((MirInstrCall *)call), call, NULL, out_ptr);
}

bool
execute_fn_impl_top_level(VM *vm, MirFn *fn, TSmallArray_ConstExprValue *args, VMStackPtr *out_ptr)
{
	return _execute_fn_top_level(vm, fn, NULL, args, out_ptr);
}

bool
_execute_fn_top_level(VM *                        vm,
                      MirFn *                     fn,
                      MirInstr *                  call,
                      TSmallArray_ConstExprValue *arg_values,
                      VMStackPtr *                out_ptr)
{
	BL_ASSERT(fn);

	if (!fn->fully_analyzed)
		BL_ABORT("Function is not fully analyzed for compile time execution!!!");

	MirType *           ret_type = fn->type->data.fn.ret_type;
	TSmallArray_ArgPtr *args     = fn->type->data.fn.args;

	const bool does_return_value    = ret_type->kind != MIR_TYPE_VOID;
	const bool is_return_value_used = call ? call->ref_count > 1 : true;
	const bool is_caller_comptime   = call ? call->value.is_comptime : false;
	const bool pop_return_value =
	    does_return_value && is_return_value_used && !is_caller_comptime;
	const usize argc = args ? args->size : 0;

	if (args) {
		BL_ASSERT(
		    !call &&
		    "Caller instruction cannot be used when call arguments are passed explicitly.");

		BL_ASSERT(argc == args->size && "Invalid count of eplicitly passed arguments");

		/* Push all arguments in reverse order on the stack. */
		for (usize i = argc; i-- > 0;) {
			stack_push(vm, arg_values->data[i].data, arg_values->data[i].type);
		}
	}

	/* push terminal frame on stack */
	push_ra(vm, call);

	/* allocate local variables */
	stack_alloc_local_vars(vm, fn);

	/* setup entry instruction */
	set_pc(vm, fn->first_block->entry_instr);

	/* iterate over entry block of executable */
	MirInstr *instr, *prev;
	while (true) {
		instr = get_pc(vm);
		prev  = instr;
		if (!instr || vm->stack->aborted) break;

		interp_instr(vm, instr);

		/* stack head can be changed by br instructions */
		if (!get_pc(vm) || get_pc(vm) == prev) set_pc(vm, instr->next);
	}

	if (vm->stack->aborted) return false;

	if (pop_return_value) {
		VMStackPtr ret_ptr = stack_pop(vm, ret_type);
		if (out_ptr) (*out_ptr) = ret_ptr;
	} else if (is_caller_comptime) {
		if (out_ptr) (*out_ptr) = (VMStackPtr)&call->value.data;
	}

	return true;
}

void
interp_instr(VM *vm, MirInstr *instr)
{
	if (!instr) return;
	if (!instr->analyzed) {
		BL_ABORT("instruction %s has not been analyzed!", mir_instr_name(instr));
	}

	/* Skip all comptimes. */
	if (instr->value.is_comptime) return;

	switch (instr->kind) {
	case MIR_INSTR_CAST:
		interp_instr_cast(vm, (MirInstrCast *)instr);
		break;
	case MIR_INSTR_ADDROF:
		interp_instr_addrof(vm, (MirInstrAddrOf *)instr);
		break;
	case MIR_INSTR_BINOP:
		interp_instr_binop(vm, (MirInstrBinop *)instr);
		break;
	case MIR_INSTR_UNOP:
		interp_instr_unop(vm, (MirInstrUnop *)instr);
		break;
	case MIR_INSTR_CALL:
		interp_instr_call(vm, (MirInstrCall *)instr);
		break;
	case MIR_INSTR_RET:
		interp_instr_ret(vm, (MirInstrRet *)instr);
		break;
	case MIR_INSTR_DECL_VAR:
		interp_instr_decl_var(vm, (MirInstrDeclVar *)instr);
		break;
	case MIR_INSTR_DECL_REF:
		interp_instr_decl_ref(vm, (MirInstrDeclRef *)instr);
		break;
	case MIR_INSTR_DECL_DIRECT_REF:
		interp_instr_decl_direct_ref(vm, (MirInstrDeclDirectRef *)instr);
		break;
	case MIR_INSTR_STORE:
		interp_instr_store(vm, (MirInstrStore *)instr);
		break;
	case MIR_INSTR_LOAD:
		interp_instr_load(vm, (MirInstrLoad *)instr);
		break;
	case MIR_INSTR_BR:
		interp_instr_br(vm, (MirInstrBr *)instr);
		break;
	case MIR_INSTR_COND_BR:
		interp_instr_cond_br(vm, (MirInstrCondBr *)instr);
		break;
	case MIR_INSTR_PHI:
		interp_instr_phi(vm, (MirInstrPhi *)instr);
		break;
	case MIR_INSTR_UNREACHABLE:
		interp_instr_unreachable(vm, (MirInstrUnreachable *)instr);
		break;
	case MIR_INSTR_ARG:
		interp_instr_arg(vm, (MirInstrArg *)instr);
		break;
	case MIR_INSTR_ELEM_PTR:
		interp_instr_elem_ptr(vm, (MirInstrElemPtr *)instr);
		break;
	case MIR_INSTR_MEMBER_PTR:
		interp_instr_member_ptr(vm, (MirInstrMemberPtr *)instr);
		break;
	case MIR_INSTR_VARGS:
		interp_instr_vargs(vm, (MirInstrVArgs *)instr);
		break;
	case MIR_INSTR_COMPOUND: {
		MirInstrCompound *cmp = (MirInstrCompound *)instr;
		if (!cmp->is_naked) break;
		interp_instr_compound(vm, NULL, cmp);
		break;
	}
	case MIR_INSTR_TOANY:
		interp_instr_toany(vm, (MirInstrToAny *)instr);
		break;
	case MIR_INSTR_SWITCH:
		interp_instr_switch(vm, (MirInstrSwitch *)instr);
		break;

	default:
		BL_ABORT("missing execution for instruction: %s", mir_instr_name(instr));
	}
}

void
interp_instr_toany(VM *vm, MirInstrToAny *toany)
{
	MirVar *dest_var  = toany->tmp;
	MirVar *type_info = assembly_get_rtti(vm->assembly, toany->rtti_type->id.hash);
	BL_ASSERT(type_info->value.is_comptime);

	MirType *  dest_type = dest_var->value.type;
	VMStackPtr dest      = vm_read_var(vm, dest_var);

	/* type info */
	MirType *  dest_type_info_type = mir_get_struct_elem_type(dest_type, 0);
	VMStackPtr dest_type_info =
	    vm_get_struct_elem_ptr(vm->assembly, dest_var->value.type, dest, 0);

	vm_write_ptr(dest_type_info_type, dest_type_info, vm_read_var(vm, type_info));

	/* data */
	MirType *  dest_data_type = mir_get_struct_elem_type(dest_type, 1);
	VMStackPtr dest_data = vm_get_struct_elem_ptr(vm->assembly, dest_var->value.type, dest, 1);

	MirType *data_type = toany->expr->value.type;

	if (toany->expr_tmp) {
		VMStackPtr data      = fetch_value(vm, &toany->expr->value);
		MirVar *   expr_var  = toany->expr_tmp;
		VMStackPtr dest_expr = vm_read_var(vm, expr_var);

		/* copy value to the tmp variable  */
		memcpy(dest_expr, data, data_type->store_size_bytes);

		/* setup destination pointer */
		memcpy(dest_data, &dest_expr, dest_data_type->store_size_bytes);
	} else if (toany->rtti_data) {
		MirVar *rtti_data_var = assembly_get_rtti(vm->assembly, toany->rtti_data->id.hash);
		VMStackPtr rtti_data  = vm_read_var(vm, rtti_data_var);
		/* setup destination pointer */
		memcpy(dest_data, &rtti_data, dest_data_type->store_size_bytes);
	} else {
		VMStackPtr data = fetch_value(vm, &toany->expr->value);
		BL_ASSERT(mir_is_pointer_type(dest_data_type));
		memcpy(dest_data, data, dest_data_type->store_size_bytes);
	}

	stack_push(vm, &dest, toany->base.value.type);
}

void
interp_instr_phi(VM *vm, MirInstrPhi *phi)
{
	MirInstrBlock *prev_block = vm->stack->prev_block;
	BL_ASSERT(prev_block && "Invalid previous block for phi instruction.");
	BL_ASSERT(phi->incoming_blocks && phi->incoming_values);
	BL_ASSERT(phi->incoming_blocks->size == phi->incoming_values->size);

	const usize c = phi->incoming_values->size;
	BL_ASSERT(c > 0);

	MirInstr *     value = NULL;
	MirInstrBlock *block;
	for (usize i = 0; i < c; ++i) {
		value = phi->incoming_values->data[i];
		block = (MirInstrBlock *)phi->incoming_blocks->data[i];

		if (block->base.id == prev_block->base.id) break;
	}

	BL_ASSERT(value && "Invalid value for phi income.");

	/* Pop used value from stack or use constant. Result will be pushed on the
	 * stack or used as constant value of phi when phi is compile time known
	 * constant. */
	{
		MirType *phi_type = phi->base.value.type;
		BL_ASSERT(phi_type);

		VMStackPtr value_ptr = fetch_value(vm, &value->value);
		stack_push(vm, value_ptr, phi_type);
	}
}

void
interp_instr_addrof(VM *vm, MirInstrAddrOf *addrof)
{
	MirInstr *src  = addrof->src;
	MirType * type = src->value.type;
	BL_ASSERT(type);

	if (src->kind == MIR_INSTR_ELEM_PTR || src->kind == MIR_INSTR_COMPOUND) {
		/* address of the element is already on the stack */
		return;
	}

	VMStackPtr ptr = fetch_value(vm, &src->value);
	ptr            = VM_STACK_PTR_DEREF(ptr);

	stack_push(vm, (VMStackPtr)&ptr, type);
}

void
interp_instr_elem_ptr(VM *vm, MirInstrElemPtr *elem_ptr)
{
	/* pop index from stack */
	MirType *  arr_type   = mir_deref_type(elem_ptr->arr_ptr->value.type);
	VMStackPtr index_ptr  = fetch_value(vm, &elem_ptr->index->value);
	VMStackPtr arr_ptr    = fetch_value(vm, &elem_ptr->arr_ptr->value);
	VMStackPtr result_ptr = NULL;
	BL_ASSERT(arr_ptr && index_ptr);

	BL_ASSERT(elem_ptr->index->value.type->store_size_bytes == sizeof(s64));
	const s64 index = VM_READ_AS(s64, index_ptr);
	arr_ptr         = VM_STACK_PTR_DEREF(arr_ptr);

	switch (arr_type->kind) {
	case MIR_TYPE_ARRAY: {
		const s64 len = arr_type->data.array.len;
		if (index >= len) {
			msg_error("Array index is out of the bounds! Array index "
			          "is: %lli, "
			          "but array size "
			          "is: %lli",
			          (long long)index,
			          (long long)len);
			exec_abort(vm, 0);
		}

		result_ptr = vm_get_array_elem_ptr(arr_type, arr_ptr, index);
		break;
	}

	case MIR_TYPE_SLICE:
	case MIR_TYPE_STRING:
	case MIR_TYPE_VARGS: {
		MirType *len_type = mir_get_struct_elem_type(arr_type, MIR_SLICE_LEN_INDEX);
		MirType *ptr_type = mir_get_struct_elem_type(arr_type, MIR_SLICE_PTR_INDEX);

		MirType *elem_type = mir_deref_type(ptr_type);
		BL_ASSERT(elem_type);

		VMStackPtr len_ptr = vm_get_struct_elem_ptr(vm->assembly, arr_type, arr_ptr, 0);
		VMStackPtr ptr_ptr = vm_get_struct_elem_ptr(vm->assembly, arr_type, arr_ptr, 1);

		VMStackPtr ptr_tmp = vm_read_ptr(ptr_type, ptr_ptr);
		const s64  len_tmp = vm_read_int(len_type, len_ptr);

		if (!ptr_tmp) {
			msg_error("Dereferencing null pointer! Slice has not been set?");
			exec_abort(vm, 0);
		}

		if (index >= len_tmp) {
			msg_error("Array index is out of the bounds! Array index is: %lli, but "
			          "array size is: %lli",
			          (long long)index,
			          (long long)len_tmp);
			exec_abort(vm, 0);
		}

		result_ptr = (VMStackPtr)(ptr_tmp + index * elem_type->store_size_bytes);
		break;
	}

	default:
		BL_ABORT("Invalid elem ptr target type!");
	}

	/* push result address on the stack */
	stack_push(vm, (VMStackPtr)&result_ptr, elem_ptr->base.value.type);
}

void
interp_instr_member_ptr(VM *vm, MirInstrMemberPtr *member_ptr)
{
	BL_ASSERT(member_ptr->target_ptr);
	MirType *target_type = member_ptr->target_ptr->value.type;

	/* lookup for base structure declaration type
	 * IDEA: maybe we can store parent type to the member type? But what about
	 * builtin types???
	 */
	BL_ASSERT(target_type->kind == MIR_TYPE_PTR && "expected pointer");
	target_type = mir_deref_type(target_type);
	BL_ASSERT(mir_is_composit_type(target_type) && "expected structure");

	/* fetch address of the struct begin */
	VMStackPtr ptr = fetch_value(vm, &member_ptr->target_ptr->value);
	ptr            = VM_STACK_PTR_DEREF(ptr);
	BL_ASSERT(ptr);

	VMStackPtr result = NULL;

	if (member_ptr->builtin_id == MIR_BUILTIN_ID_NONE) {
		BL_ASSERT(member_ptr->scope_entry &&
		          member_ptr->scope_entry->kind == SCOPE_ENTRY_MEMBER);
		MirMember *member = member_ptr->scope_entry->data.member;
		BL_ASSERT(member);
		const s64 index = member->index;

		/* let the llvm solve poiner offest */
		result = vm_get_struct_elem_ptr(vm->assembly, target_type, ptr, (u32)index);
	} else {
		/* builtin member */
		if (member_ptr->builtin_id == MIR_BUILTIN_ID_ARR_PTR) {
			/* slice .ptr */
			result = vm_get_struct_elem_ptr(vm->assembly, target_type, ptr, 1);
		} else if (member_ptr->builtin_id == MIR_BUILTIN_ID_ARR_LEN) {
			/* slice .len*/
			result = vm_get_struct_elem_ptr(vm->assembly, target_type, ptr, 0);
		} else {
			BL_ABORT("invalid slice member!");
		}
	}

	/* push result address on the stack */
	stack_push(vm, (VMStackPtr)&result, member_ptr->base.value.type);
}

void
interp_instr_unreachable(VM *vm, MirInstrUnreachable *unr)
{
	msg_error("execution reached unreachable code");
	exec_abort(vm, 0);
}

void
interp_instr_br(VM *vm, MirInstrBr *br)
{
	BL_ASSERT(br->then_block);
	vm->stack->prev_block = br->base.owner_block;
	set_pc(vm, br->then_block->entry_instr);
}

void
interp_instr_switch(VM *vm, MirInstrSwitch *sw)
{
	MirType *  value_type = sw->value->value.type;
	VMStackPtr value_ptr  = fetch_value(vm, &sw->value->value);
	BL_ASSERT(value_ptr);

	const s64 value       = vm_read_int(value_type, value_ptr);
	vm->stack->prev_block = sw->base.owner_block;

	TSmallArray_SwitchCase *cases = sw->cases;
	for (usize i = 0; i < cases->size; ++i) {
		MirSwitchCase *c = &cases->data[i];

		const s64 on_value = vm_read_int(value_type, c->on_value->value.data);
		if (value == on_value) {
			set_pc(vm, c->block->entry_instr);
			return;
		}
	}

	set_pc(vm, sw->default_block->entry_instr);
}

void
interp_instr_cast(VM *vm, MirInstrCast *cast)
{
	MirType *  dest_type = cast->base.value.type;
	MirType *  src_type  = cast->expr->value.type;
	VMStackPtr src_ptr   = fetch_value(vm, &cast->expr->value);

	VMValue tmp = {0};
	do_cast((VMStackPtr)&tmp, src_ptr, dest_type, src_type, cast->op);
	stack_push(vm, &tmp, dest_type);
}

void
interp_instr_arg(VM *vm, MirInstrArg *arg)
{
	/* Caller is optional, when we call function implicitly there is no call instruction which
	 * we can use, so we need to handle also this situation. In such case we expect all
	 * arguments to be already pushed on the stack. */
	MirInstrCall *caller = (MirInstrCall *)get_ra(vm)->caller;

	if (caller) {
		TSmallArray_InstrPtr *arg_values = caller->args;
		BL_ASSERT(arg_values);
		MirInstr *curr_arg_value = arg_values->data[arg->i];

		if (mir_is_comptime(curr_arg_value)) {
			MirType *type = curr_arg_value->value.type;
			stack_push(vm, curr_arg_value->value.data, type);
		} else {
			/* Arguments are located in reverse order right before return address on the
			 * stack
			 * so we can find them inside loop adjusting address up on the stack. */
			MirInstr *arg_value = NULL;
			/* starting point */
			VMStackPtr arg_ptr = (VMStackPtr)vm->stack->ra;
			for (u32 i = 0; i <= arg->i; ++i) {
				arg_value = arg_values->data[i];
				BL_ASSERT(arg_value);
				if (mir_is_comptime(arg_value)) continue;
				arg_ptr -=
				    stack_alloc_size(arg_value->value.type->store_size_bytes);
			}

			stack_push(vm, (VMStackPtr)arg_ptr, arg->base.value.type);
		}

		return;
	}

	/* Caller instruction not specified!!! */
	MirFn *fn = arg->base.owner_block->owner_fn;
	BL_ASSERT(fn && "Arg instruction cannot determinate current function");

	/* All arguments must be already on the stack in reverse order. */
	TSmallArray_ArgPtr *args = fn->type->data.fn.args;
	BL_ASSERT(args && "Function has no arguments");

	/* starting point */
	VMStackPtr arg_ptr = (VMStackPtr)vm->stack->ra;
	for (u32 i = 0; i <= arg->i; ++i) {
		arg_ptr -= stack_alloc_size(args->data[i]->type->store_size_bytes);
	}

	stack_push(vm, (VMStackPtr)arg_ptr, arg->base.value.type);
}

void
interp_instr_cond_br(VM *vm, MirInstrCondBr *br)
{
	BL_ASSERT(br->cond);
	MirType *type = br->cond->value.type;

	/* pop condition from stack */
	VMStackPtr cond_ptr = fetch_value(vm, &br->cond->value);
	BL_ASSERT(cond_ptr);

	const bool condition = vm_read_int(type, cond_ptr);

	/* Set previous block. */
	vm->stack->prev_block = br->base.owner_block;
	if (condition) {
		set_pc(vm, br->then_block->entry_instr);
	} else {
		set_pc(vm, br->else_block->entry_instr);
	}
}

void
interp_instr_decl_ref(VM *vm, MirInstrDeclRef *ref)
{
	ScopeEntry *entry = ref->scope_entry;
	BL_ASSERT(entry);

	switch (entry->kind) {
	case SCOPE_ENTRY_VAR: {
		MirVar *var = entry->data.var;
		BL_ASSERT(var);

		VMStackPtr real_ptr = vm_read_var(vm, var);
		stack_push(vm, &real_ptr, ref->base.value.type);
		break;
	}

	case SCOPE_ENTRY_FN:
	case SCOPE_ENTRY_TYPE:
	case SCOPE_ENTRY_MEMBER:
	case SCOPE_ENTRY_VARIANT:
		break;

	default:
		BL_ABORT("invalid declaration reference");
	}
}

void
interp_instr_decl_direct_ref(VM *vm, MirInstrDeclDirectRef *ref)
{
	BL_ASSERT(ref->ref->kind == MIR_INSTR_DECL_VAR);
	MirVar *var = ((MirInstrDeclVar *)ref->ref)->var;
	BL_ASSERT(var);

	VMStackPtr real_ptr = vm_read_var(vm, var);
	stack_push(vm, &real_ptr, ref->base.value.type);
}

void
interp_instr_compound(VM *vm, VMStackPtr tmp_ptr, MirInstrCompound *cmp)
{
	BL_ASSERT(!mir_is_comptime(&cmp->base));

	const bool will_push = tmp_ptr == NULL;
	if (will_push) {
		BL_ASSERT(cmp->tmp_var && "Missing temp variable for compound.");
		tmp_ptr = vm_read_var(vm, cmp->tmp_var);
	}

	BL_ASSERT(tmp_ptr);

	MirType *  type = cmp->base.value.type;
	MirType *  elem_type;
	VMStackPtr elem_ptr = tmp_ptr;

	MirInstr *value;
	TSA_FOREACH(cmp->values, value)
	{
		elem_type = value->value.type;
		switch (type->kind) {

		case MIR_TYPE_STRING:
		case MIR_TYPE_SLICE:
		case MIR_TYPE_VARGS:
		case MIR_TYPE_STRUCT:
			elem_ptr = vm_get_struct_elem_ptr(vm->assembly, type, tmp_ptr, (u32)i);
			break;

		case MIR_TYPE_ARRAY:
			elem_ptr = vm_get_array_elem_ptr(type, tmp_ptr, (u32)i);
			break;

		default:
			BL_ASSERT(i == 0 && "Invalid elem count for non-agregate type!!!");
		}

		if (value->kind == MIR_INSTR_COMPOUND) {
			interp_instr_compound(vm, elem_ptr, (MirInstrCompound *)value);
		} else {
			VMStackPtr value_ptr = fetch_value(vm, &value->value);
			memcpy(elem_ptr, value_ptr, elem_type->store_size_bytes);
		}
	}

	if (will_push) stack_push(vm, tmp_ptr, cmp->base.value.type);
}

void
interp_instr_vargs(VM *vm, MirInstrVArgs *vargs)
{
	TSmallArray_InstrPtr *values    = vargs->values;
	MirVar *              arr_tmp   = vargs->arr_tmp;
	MirVar *              vargs_tmp = vargs->vargs_tmp;

	BL_ASSERT(vargs_tmp->value.type->kind == MIR_TYPE_VARGS);
	BL_ASSERT(vargs_tmp->rel_stack_ptr && "Unalocated vargs slice!!!");
	BL_ASSERT(values);

	VMStackPtr arr_tmp_ptr = arr_tmp ? vm_read_var(vm, arr_tmp) : NULL;

	/* Fill vargs tmp array with values from stack or constants. */
	{
		MirInstr * value;
		VMStackPtr value_ptr;
		TSA_FOREACH(values, value)
		{
			const usize value_size = value->value.type->store_size_bytes;
			VMStackPtr  dest       = arr_tmp_ptr + i * value_size;

			value_ptr = fetch_value(vm, &value->value);
			memcpy(dest, value_ptr, value_size);
		}
	}

	/* Push vargs slice on the stack. */
	{
		VMStackPtr vargs_tmp_ptr = vm_read_var(vm, vargs_tmp);
		// set len
		VMStackPtr len_ptr =
		    vargs_tmp_ptr + vm_get_struct_elem_offest(
		                        vm->assembly, vargs_tmp->value.type, MIR_SLICE_LEN_INDEX);

		BL_ASSERT(mir_get_struct_elem_type(vargs_tmp->value.type, MIR_SLICE_LEN_INDEX)
		              ->store_size_bytes == sizeof(s64));
		VM_WRITE_AS(s64, len_ptr, values->size);

		// set ptr
		VMStackPtr ptr_ptr =
		    vargs_tmp_ptr + vm_get_struct_elem_offest(
		                        vm->assembly, vargs_tmp->value.type, MIR_SLICE_PTR_INDEX);

		VM_WRITE_AS(VMStackPtr, ptr_ptr, arr_tmp_ptr);

		stack_push(vm, vargs_tmp_ptr, vargs_tmp->value.type);
	}
}

void
interp_instr_decl_var(VM *vm, MirInstrDeclVar *decl)
{
	MirVar *var = decl->var;
	BL_ASSERT(var);
	BL_ASSERT(decl->base.value.type);

	if (var->is_global || var->value.is_comptime) return;

	/* initialize variable if there is some init value */
	if (decl->init) {
		VMStackPtr var_ptr = vm_read_var(vm, var);

		if (!mir_is_comptime(decl->init) && decl->init->kind == MIR_INSTR_COMPOUND) {
			/* used compound initialization!!! */
			interp_instr_compound(vm, var_ptr, (MirInstrCompound *)decl->init);
		} else {
			/* read initialization value if there is one */
			VMStackPtr init_ptr = fetch_value(vm, &decl->init->value);
			memcpy(var_ptr, init_ptr, var->value.type->store_size_bytes);
		}
	}
}

void
interp_instr_load(VM *vm, MirInstrLoad *load)
{
	/* pop source from stack or load directly when src is declaration, push on
	 * to stack dereferenced value of source */
	MirType *dest_type = load->base.value.type;
	BL_ASSERT(dest_type);
	BL_ASSERT(mir_is_pointer_type(load->src->value.type));

	VMStackPtr src_ptr = fetch_value(vm, &load->src->value);
	src_ptr            = VM_STACK_PTR_DEREF(src_ptr);

	if (!src_ptr) {
		msg_error("Dereferencing null pointer!");
		exec_abort(vm, 0);
	}

	stack_push(vm, src_ptr, dest_type);
}

void
interp_instr_store(VM *vm, MirInstrStore *store)
{
	/* loads destination (in case it is not direct reference to declaration) and
	 * source from stack
	 */
	MirType *src_type = store->src->value.type;
	BL_ASSERT(src_type);

	VMStackPtr dest_ptr = fetch_value(vm, &store->dest->value);
	VMStackPtr src_ptr  = fetch_value(vm, &store->src->value);

	dest_ptr = VM_STACK_PTR_DEREF(dest_ptr);

	BL_ASSERT(dest_ptr && src_ptr);
	memcpy(dest_ptr, src_ptr, src_type->store_size_bytes);
}

void
interp_instr_call(VM *vm, MirInstrCall *call)
{
	BL_ASSERT(call->callee && call->base.value.type);
	BL_ASSERT(call->callee->value.type);

	VMStackPtr callee_ptr      = fetch_value(vm, &call->callee->value);
	MirType *  callee_ptr_type = call->callee->value.type;

	/* Function called via pointer. */
	if (mir_is_pointer_type(call->callee->value.type)) {
		BL_ASSERT(mir_deref_type(call->callee->value.type)->kind == MIR_TYPE_FN);
	}

	MirFn *callee = (MirFn *)vm_read_ptr(callee_ptr_type, callee_ptr);
	if (callee == NULL) {
		msg_error("Function pointer not set!");
		exec_abort(vm, 0);
		return;
	}

	BL_ASSERT(callee->type);

	if (IS_FLAG(callee->flags, FLAG_EXTERN)) {
		interp_extern_call(vm, callee, call);
	} else {
		/* Push current frame stack top. (Later poped by ret instruction)*/
		push_ra(vm, &call->base);
		BL_ASSERT(callee->first_block->entry_instr);

		stack_alloc_local_vars(vm, callee);

		/* setup entry instruction */
		set_pc(vm, callee->first_block->entry_instr);
	}
}

void
interp_instr_ret(VM *vm, MirInstrRet *ret)
{
	MirFn *fn = ret->base.owner_block->owner_fn;
	BL_ASSERT(fn);

	/* read callee from frame stack */
	MirInstrCall *caller       = (MirInstrCall *)get_ra(vm)->caller;
	MirType *     ret_type     = fn->type->data.fn.ret_type;
	VMStackPtr    ret_data_ptr = NULL;

	/* pop return value from stack */
	if (ret->value) {
		ret_data_ptr = fetch_value(vm, &ret->value->value);
		BL_ASSERT(ret_data_ptr);

		if (caller ? caller->base.ref_count == 1 : false) ret_data_ptr = NULL;
	}

	/* do frame stack rollback */
	MirInstr *pc = (MirInstr *)pop_ra(vm);

	/* clean up all arguments from the stack */
	if (caller) {
		TSmallArray_InstrPtr *arg_values = caller->args;
		if (arg_values) {
			MirInstr *arg_value;
			TSA_FOREACH(arg_values, arg_value)
			{
				if (mir_is_comptime(arg_value)) continue;
				stack_pop(vm, arg_value->value.type);
			}
		}
	} else {
		/* When caller was not specified we expect all arguments to be pushed on the stack
		 * so we must clear them all. Remember they were pushed in reverse order, so now we
		 * have to pop them in order they are defined. */

		TSmallArray_ArgPtr *args = fn->type->data.fn.args;
		if (args) {
			MirArg *arg;
			TSA_FOREACH(args, arg)
			{
				stack_pop(vm, arg->type);
			}
		}
	}

	/* push return value on the stack if there is one */
	if (ret_data_ptr) {
		/* Determinate if caller instruction is comptime, if caller does not exist we are
		 * going to push result on the stack. */
		const bool is_caller_comptime = caller ? caller->base.value.is_comptime : false;
		if (is_caller_comptime) {
			caller->base.value.data = ret->value->value.data;
		} else {
			stack_push(vm, ret_data_ptr, ret_type);
		}
	}

	/* set program counter to next instruction */
	pc = pc ? pc->next : NULL;
	set_pc(vm, pc);
}

void
interp_instr_binop(VM *vm, MirInstrBinop *binop)
{
	/* binop expects lhs and rhs on stack in exact order and push result again
	 * to the stack */

	VMStackPtr lhs_ptr = fetch_value(vm, &binop->lhs->value);
	VMStackPtr rhs_ptr = fetch_value(vm, &binop->rhs->value);
	BL_ASSERT(rhs_ptr && lhs_ptr);

	MirType *dest_type = binop->base.value.type;
	MirType *src_type  = binop->lhs->value.type;

	VMValue tmp = {0};
	calculate_binop(dest_type, src_type, (VMStackPtr)&tmp, lhs_ptr, rhs_ptr, binop->op);

	stack_push(vm, &tmp, dest_type);
}

void
interp_instr_unop(VM *vm, MirInstrUnop *unop)
{
	MirType *  type  = unop->base.value.type;
	VMStackPtr v_ptr = fetch_value(vm, &unop->expr->value);

	VMValue tmp = {0};
	calculate_unop((VMStackPtr)&tmp, v_ptr, unop->op, type);

	stack_push(vm, &tmp, type);
}

void
eval_instr(VM *vm, MirInstr *instr)
{
	if (!instr) return;
	BL_ASSERT(instr->value.is_comptime);

	switch (instr->kind) {
	case MIR_INSTR_DECL_REF:
		eval_instr_decl_ref(vm, (MirInstrDeclRef *)instr);
		break;

	case MIR_INSTR_DECL_DIRECT_REF:
		eval_instr_decl_direct_ref(vm, (MirInstrDeclDirectRef *)instr);
		break;

	case MIR_INSTR_BINOP:
		eval_instr_binop(vm, (MirInstrBinop *)instr);
		break;

	case MIR_INSTR_UNOP:
		eval_instr_unop(vm, (MirInstrUnop *)instr);
		break;

	case MIR_INSTR_SET_INITIALIZER:
		eval_instr_set_initializer(vm, (MirInstrSetInitializer *)instr);
		break;

	case MIR_INSTR_LOAD:
		eval_instr_load(vm, (MirInstrLoad *)instr);
		break;

	case MIR_INSTR_ADDROF:
		eval_instr_addrof(vm, (MirInstrAddrOf *)instr);
		break;

	case MIR_INSTR_CAST:
		eval_instr_cast(vm, (MirInstrCast *)instr);
		break;

	case MIR_INSTR_DECL_VAR:
		eval_instr_decl_var(vm, (MirInstrDeclVar *)instr);
		break;

	case MIR_INSTR_COMPOUND:
		eval_instr_compound(vm, (MirInstrCompound *)instr);
		break;

	case MIR_INSTR_ELEM_PTR:
		eval_instr_elem_ptr(vm, (MirInstrElemPtr *)instr);
		break;

	case MIR_INSTR_MEMBER_PTR:
		eval_instr_member_ptr(vm, (MirInstrMemberPtr *)instr);
		break;

	case MIR_INSTR_TYPE_INFO:
		eval_instr_type_info(vm, (MirInstrTypeInfo *)instr);
		break;

	case MIR_INSTR_BLOCK:
	case MIR_INSTR_CONST:
	case MIR_INSTR_FN_PROTO:
	case MIR_INSTR_CALL:
	case MIR_INSTR_TYPE_ARRAY:
	case MIR_INSTR_TYPE_PTR:
	case MIR_INSTR_TYPE_FN:
	case MIR_INSTR_TYPE_STRUCT:
	case MIR_INSTR_TYPE_ENUM:
	case MIR_INSTR_TYPE_SLICE:
	case MIR_INSTR_TYPE_VARGS:
	case MIR_INSTR_DECL_MEMBER:
	case MIR_INSTR_DECL_ARG:
	case MIR_INSTR_DECL_VARIANT:
	case MIR_INSTR_SIZEOF:
	case MIR_INSTR_ALIGNOF:
		break;

	default:
		BL_ABORT("Missing evaluation for instruction '%s'.", mir_instr_name(instr));
	}
}

void
eval_instr_type_info(VM *vm, MirInstrTypeInfo *type_info)
{
	BL_ASSERT(type_info->rtti_type && "Missing RTTI type!");
	MirVar *rtti_var = assembly_get_rtti(vm->assembly, type_info->rtti_type->id.hash);

	MIR_CEV_WRITE_AS(VMStackPtr, &type_info->base.value, rtti_var->value.data);
}

void
eval_instr_elem_ptr(VM *vm, MirInstrElemPtr *elem_ptr)
{
	MirType *  arr_type   = mir_deref_type(elem_ptr->arr_ptr->value.type);
	VMStackPtr arr_ptr    = MIR_CEV_READ_AS(VMStackPtr, &elem_ptr->arr_ptr->value);
	VMStackPtr result_ptr = NULL;
	const s64  index      = MIR_CEV_READ_AS(s64, &elem_ptr->index->value);

	switch (arr_type->kind) {
	case MIR_TYPE_ARRAY: {
		result_ptr = vm_get_array_elem_ptr(arr_type, arr_ptr, index);
		break;
	}

	case MIR_TYPE_SLICE:
	case MIR_TYPE_STRING:
	case MIR_TYPE_VARGS: {
		MirType *len_type = mir_get_struct_elem_type(arr_type, MIR_SLICE_LEN_INDEX);
		MirType *ptr_type = mir_get_struct_elem_type(arr_type, MIR_SLICE_PTR_INDEX);

		MirType *elem_type = mir_deref_type(ptr_type);
		BL_ASSERT(elem_type);

		VMStackPtr len_ptr = vm_get_struct_elem_ptr(vm->assembly, arr_type, arr_ptr, 0);
		VMStackPtr ptr_ptr = vm_get_struct_elem_ptr(vm->assembly, arr_type, arr_ptr, 1);

		VMStackPtr ptr_tmp = vm_read_ptr(ptr_type, ptr_ptr);
		const s64  len_tmp = vm_read_int(len_type, len_ptr);

		if (!ptr_tmp) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_JIT_RUN_FAILED,
			            elem_ptr->base.node->location,
			            BUILDER_CUR_WORD,
			            "Dereferencing null pointer! Slice has not been set?");

			return;
		}

		if (index >= len_tmp) {
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_JIT_RUN_FAILED,
			            elem_ptr->base.node->location,
			            BUILDER_CUR_WORD,
			            "Array index is out of the bounds! Array index is: %lli, but "
			            "array size is: %lli",
			            (long long)index,
			            (long long)len_tmp);

			return;
		}
		break;
	}

	default:
		BL_ABORT("Invalid elem ptr target type!");
	}

	MIR_CEV_WRITE_AS(VMStackPtr, &elem_ptr->base.value, result_ptr);
}

void
eval_instr_member_ptr(VM *vm, MirInstrMemberPtr *member_ptr)
{
	switch (member_ptr->scope_entry->kind) {
	case SCOPE_ENTRY_MEMBER: {
		MirMember *member     = member_ptr->scope_entry->data.member;
		VMStackPtr strct_ptr  = MIR_CEV_READ_AS(VMStackPtr, &member_ptr->target_ptr->value);
		VMStackPtr result_ptr = NULL;

		result_ptr = strct_ptr + member->offset_bytes;

		MIR_CEV_WRITE_AS(VMStackPtr, &member_ptr->base.value, result_ptr);
		break;
	}

	case SCOPE_ENTRY_VARIANT: {
		MirVariant *variant         = member_ptr->scope_entry->data.variant;
		member_ptr->base.value.data = variant->value->data;
		break;
	}

	default:
		BL_ABORT("Invalid scope entry for member_ptr instruction!");
	}
}

void
eval_instr_compound(VM *vm, MirInstrCompound *compound)
{
	MirConstExprValue *value = &compound->base.value;
	if (needs_tmp_alloc(value)) {
		/* Compound data does't fit into default static memory register, we need to allcate
		 * temporary block on the stack. */
		value->data = stack_push_empty(vm, value->type);
	}

	if (compound->is_zero_initialized) {
		memset(value->data, 0, value->type->store_size_bytes);
		return;
	}

	MirInstr *it;
	TSA_FOREACH(compound->values, it)
	{
		VMStackPtr dest_ptr = value->data;
		VMStackPtr src_ptr  = it->value.data;
		BL_ASSERT(src_ptr && "Invalid compound element value!");

		switch (value->type->kind) {
		case MIR_TYPE_ARRAY: {
			ptrdiff_t offset = vm_get_array_elem_offset(value->type, i);
			dest_ptr += offset;
			memcpy(
			    dest_ptr, src_ptr, value->type->data.array.elem_type->store_size_bytes);
			break;
		}

		case MIR_TYPE_STRUCT:
		case MIR_TYPE_STRING:
		case MIR_TYPE_SLICE:
		case MIR_TYPE_VARGS: {
			MirType * member_type = value->type->data.strct.members->data[i]->type;
			ptrdiff_t offset =
			    vm_get_struct_elem_offest(vm->assembly, value->type, (u32)i);
			dest_ptr += offset;

			memcpy(dest_ptr, src_ptr, member_type->store_size_bytes);
			break;
		}

		case MIR_TYPE_INT:
		case MIR_TYPE_REAL:
		case MIR_TYPE_BOOL:
		case MIR_TYPE_PTR:
		case MIR_TYPE_ENUM: {
			BL_ASSERT(i == 0 && "Non-agregate type initialized with multiple values!");
			memcpy(dest_ptr, src_ptr, value->type->store_size_bytes);
			break;
		}

		default:
			BL_ABORT("Invalid type of compound element!");
		}
	}
}

void
eval_instr_decl_var(VM *vm, MirInstrDeclVar *decl_var)
{
	BL_ASSERT(decl_var->init && "Missing variable initializer!");
	MirVar *var     = decl_var->var;
	var->value.data = decl_var->init->value.data;
	BL_ASSERT(var->value.data && "Invalid variable initializer!");
}

void
eval_instr_cast(VM *vm, MirInstrCast *cast)
{
	MirType *  dest_type = cast->base.value.type;
	MirType *  src_type  = cast->expr->value.type;
	VMStackPtr src       = cast->expr->value.data;

	do_cast(cast->base.value.data, src, dest_type, src_type, cast->op);
}

void
eval_instr_addrof(VM *vm, MirInstrAddrOf *addrof)
{
	addrof->base.value.data = addrof->src->value.data;
}

void
eval_instr_load(VM *vm, MirInstrLoad *load)
{
	VMStackPtr src = MIR_CEV_READ_AS(VMStackPtr, &load->src->value);
	BL_ASSERT(src);
	load->base.value.data = src;
}

void
eval_instr_set_initializer(VM *vm, MirInstrSetInitializer *si)
{
	MirVar *var = ((MirInstrDeclVar *)si->dest)->var;
	BL_ASSERT(var->is_global && "Only globals can be initialized by initializer!");

	if (var->value.is_comptime) {
		/* This is little optimization, we can simply reuse initializer pointer since we are
		 * dealing with constant values and variable is immutable comptime. */
		var->value.data = si->src->value.data;
	} else {
		MirType *var_type = var->value.type;

		/* Gloabals always use static segment allocation!!! */
		VMStackPtr var_ptr = vm_read_var(vm, var);

		/* Runtime variable needs it's own memory location so we must create copy of
		 * initializer data.*/
		memcpy(var_ptr, si->src->value.data, var_type->store_size_bytes);
	}
}

void
eval_instr_unop(VM *vm, MirInstrUnop *unop)
{
	MirType *type = unop->base.value.type;

	VMStackPtr v_data    = unop->expr->value.data;
	VMStackPtr dest_data = unop->base.value.data;

	calculate_unop(dest_data, v_data, unop->op, type);
}

void
eval_instr_binop(VM *vm, MirInstrBinop *binop)
{
	BL_ASSERT(binop->lhs->value.is_comptime && binop->rhs->value.is_comptime);

	VMStackPtr lhs_ptr  = binop->lhs->value.data;
	VMStackPtr rhs_ptr  = binop->rhs->value.data;
	VMStackPtr dest_ptr = binop->base.value.data;

	MirType *dest_type = binop->base.value.type;
	MirType *src_type  = binop->lhs->value.type;

	calculate_binop(dest_type, src_type, dest_ptr, lhs_ptr, rhs_ptr, binop->op);
}

void
eval_instr_decl_ref(VM *vm, MirInstrDeclRef *decl_ref)
{
	ScopeEntry *entry = decl_ref->scope_entry;
	BL_ASSERT(entry);

	switch (entry->kind) {
	case SCOPE_ENTRY_FN:
		MIR_CEV_WRITE_AS(MirFn *, &decl_ref->base.value, entry->data.fn);
		break;

	case SCOPE_ENTRY_TYPE:
		MIR_CEV_WRITE_AS(MirType *, &decl_ref->base.value, entry->data.type);
		break;

	case SCOPE_ENTRY_VAR:
		MIR_CEV_WRITE_AS(VMStackPtr, &decl_ref->base.value, entry->data.var->value.data);
		break;

	default:
		BL_UNIMPLEMENTED;
	}
}

void
eval_instr_decl_direct_ref(VM *vm, MirInstrDeclDirectRef *decl_ref)
{
	MirVar *var = ((MirInstrDeclVar *)decl_ref->ref)->var;
	MIR_CEV_WRITE_AS(VMStackPtr *, &decl_ref->base.value, &var->value.data);
}

/* public */
void
vm_init(VM *vm, usize stack_size)
{
	if (stack_size == 0) BL_ABORT("invalid frame stack size");

	VMStack *stack = bl_malloc(sizeof(char) * stack_size);
	if (!stack) BL_ABORT("bad alloc");
#if BL_DEBUG
	memset(stack, 0, stack_size);
#endif

	stack->allocated_bytes = stack_size;
	reset_stack(stack);

	vm->stack = stack;

	tsa_init(&vm->dyncall_sig_tmp);
}

void
vm_terminate(VM *vm)
{
	tsa_terminate(&vm->dyncall_sig_tmp);
	bl_free(vm->stack);
}

void
vm_execute_instr(VM *vm, Assembly *assembly, MirInstr *instr)
{
	vm->assembly = assembly;
	interp_instr(vm, instr);
}

void
vm_eval_instr(VM *vm, Assembly *assembly, struct MirInstr *instr)
{
	vm->assembly = assembly;
	eval_instr(vm, instr);
}

bool
vm_execute_fn(VM *vm, Assembly *assembly, MirFn *fn, VMStackPtr *out_ptr)
{
	vm->assembly       = assembly;
	vm->stack->aborted = false;
	return execute_fn_impl_top_level(vm, fn, NULL, out_ptr);
}

bool
vm_execute_instr_top_level_call(VM *vm, Assembly *assembly, MirInstrCall *call)
{
	vm->assembly = assembly;
	BL_ASSERT(call && call->base.analyzed);

	assert(call->base.value.is_comptime && "Top level call is expected to be comptime.");
	if (call->args) BL_ABORT("exec call top level has not implemented passing of arguments");

	return execute_fn_top_level(vm, &call->base, NULL);
}

VMStackPtr
vm_alloc_global(VM *vm, Assembly *assembly, MirVar *var)
{
	vm->assembly = assembly;
	BL_ASSERT(var);
	BL_ASSERT(var->is_global && "Allocated variable is supposed to be global variable.");

	if (var->value.is_comptime) {
		if (needs_tmp_alloc(&var->value)) {
			var->value.data = stack_push_empty(vm, var->value.type);
		} else {
			var->value.data = (VMStackPtr)&var->value._tmp;
		}

		return var->value.data;
	}

	var->rel_stack_ptr = stack_alloc_var(vm, var);

	/* HACK: we can ignore relative pointers for globals. */
	return (VMStackPtr)var->rel_stack_ptr;
}

VMStackPtr
vm_alloc_const_expr_value(VM *vm, Assembly *assembly, MirConstExprValue *value)
{
	BL_ASSERT(value->is_comptime);
	BL_ASSERT(value->type);

	return value->data;
}

VMStackPtr
vm_alloc_raw(VM *vm, struct Assembly *assembly, MirType *type)
{
	return stack_push_empty(vm, type);
}

void *
_vm_read_value(usize size, VMStackPtr value)
{
	BL_ASSERT(value);

	static VMValue tmp;
	memset(&tmp, 0, sizeof(tmp));

	if (size == 0) BL_ABORT("Reading value of zero size is invalid!!!");
	if (size > sizeof(tmp)) BL_ABORT("Cannot read value bigger then %sB", sizeof(tmp));

	memcpy(&tmp, value, size);
	return &tmp;
}

/* Try to fetch variable allocation pointer. */
VMStackPtr
vm_read_var(VM *vm, MirVar *var)
{
	VMStackPtr ptr = NULL;
	if (var->value.is_comptime) {
		ptr = var->value.data;
	} else {
		ptr = stack_rel_to_abs_ptr(vm, var->rel_stack_ptr, var->is_global);
	}

	BL_ASSERT(ptr && "Attept to get allocation pointer of unallocated variable!");
	return ptr;
}

u64
vm_read_int(struct MirType *type, VMStackPtr src)
{
	BL_ASSERT(src && "Attempt to read null source!");
	u64 result = 0;
	memcpy(&result, src, type->store_size_bytes);
	return result;
}

f64
vm_read_double(struct MirType *type, VMStackPtr src)
{
	const usize size = type->store_size_bytes;
	BL_ASSERT(src && "Attempt to read null source!");
	BL_ASSERT(size == sizeof(f64) && "Target type is not f64 type!");

	f64 result = 0;
	memcpy(&result, src, size);
	return result;
}

VMStackPtr
vm_read_ptr(struct MirType *type, VMStackPtr src)
{
	const usize size = type->store_size_bytes;
	BL_ASSERT(src && "Attempt to read null source!");
	BL_ASSERT(size == sizeof(VMStackPtr) && "Target type is not pointer type!");

	VMStackPtr result = 0;
	memcpy(&result, src, size);
	return result;
}

f32
vm_read_float(struct MirType *type, VMStackPtr src)
{
	const usize size = type->store_size_bytes;
	BL_ASSERT(src && "Attempt to read null source!");
	BL_ASSERT(size == sizeof(f32) && "Target type is not f64 type!");

	f32 result = 0;
	memcpy(&result, src, size);
	return result;
}

void
vm_write_int(MirType *type, VMStackPtr dest, u64 i)
{
	BL_ASSERT(dest && "Attempt to write to the null destination!");
	memcpy(dest, &i, type->store_size_bytes);
}

void
vm_write_double(MirType *type, VMStackPtr dest, f64 i)
{
	const usize size = type->store_size_bytes;
	BL_ASSERT(size == sizeof(f64) && "Target type is not f64 type!");
	BL_ASSERT(dest && "Attempt to write to the null destination!");
	memcpy(dest, &i, type->store_size_bytes);
}

void
vm_write_float(MirType *type, VMStackPtr dest, f32 i)
{
	const usize size = type->store_size_bytes;
	BL_ASSERT(size == sizeof(f32) && "Target type is not f64 type!");
	BL_ASSERT(dest && "Attempt to write to the null destination!");
	memcpy(dest, &i, type->store_size_bytes);
}

void
vm_write_ptr(struct MirType *type, VMStackPtr dest, VMStackPtr ptr)
{
	BL_ASSERT(dest && "Attempt to write to the null destination!");
	memcpy(dest, &ptr, type->store_size_bytes);
}

void
_vm_write_value(usize dest_size, VMStackPtr dest, VMStackPtr src)
{
	BL_ASSERT(dest && "Attempt to write to the null destination!");
	memcpy(dest, src, dest_size);
}

ptrdiff_t
vm_get_struct_elem_offest(Assembly *assembly, MirType *type, u32 i)
{
	BL_ASSERT(mir_is_composit_type(type) && "Expected structure type");
	return (ptrdiff_t)LLVMOffsetOfElement(assembly->llvm.TD, type->llvm_type, i);
}

ptrdiff_t
vm_get_array_elem_offset(MirType *type, u32 i)
{
	BL_ASSERT(type->kind == MIR_TYPE_ARRAY && "Expected array type");
	MirType *elem_type = type->data.array.elem_type;
	BL_ASSERT(elem_type);
	return (ptrdiff_t)elem_type->store_size_bytes * i;
}

VMStackPtr
vm_get_struct_elem_ptr(Assembly *assembly, MirType *type, VMStackPtr ptr, u32 i)
{
	return ptr + vm_get_struct_elem_offest(assembly, type, i);
}

VMStackPtr
vm_get_array_elem_ptr(MirType *type, VMStackPtr ptr, u32 i)
{
	return ptr + vm_get_array_elem_offset(type, i);
}
