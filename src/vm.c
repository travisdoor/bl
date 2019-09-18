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

#define MAX_ALIGNMENT 8
#define VERBOSE_EXEC false
#define CHCK_STACK true
#define PTR_SIZE sizeof(void *) /* HACK: can cause problems with different build targets. */

#define pop_stack_as(cnt, type, T) ((T)pop_stack((cnt), (type)))

// Debug helpers
#if BL_DEBUG && VERBOSE_EXEC
#define LOG_PUSH_RA                                                                                \
	{                                                                                          \
		if (instr) {                                                                       \
			fprintf(stdout,                                                            \
			        "%6llu %20s  PUSH RA\n",                                           \
			        cnt->exec.stack->pc->id,                                           \
			        mir_instr_name(cnt->exec.stack->pc));                              \
		} else {                                                                           \
			fprintf(stdout, "     - %20s  PUSH RA\n", "Terminal");                     \
		}                                                                                  \
	}

#define LOG_POP_RA                                                                                 \
	{                                                                                          \
		fprintf(stdout,                                                                    \
		        "%6llu %20s  POP RA\n",                                                    \
		        cnt->exec.stack->pc->id,                                                   \
		        mir_instr_name(cnt->exec.stack->pc));                                      \
	}

#define LOG_PUSH_STACK                                                                             \
	{                                                                                          \
		char type_name[256];                                                               \
		mir_type_to_str(type_name, 256, type, true);                                       \
		if (cnt->exec.stack->pc) {                                                         \
			fprintf(stdout,                                                            \
			        "%6llu %20s  PUSH    (%luB, %p) %s\n",                             \
			        (unsigned long long)cnt->exec.stack->pc->id,                       \
			        mir_instr_name(cnt->exec.stack->pc),                               \
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
		fprintf(stdout,                                                                    \
		        "%6llu %20s  POP     (%luB, %p) %s\n",                                     \
		        cnt->exec.stack->pc->id,                                                   \
		        mir_instr_name(cnt->exec.stack->pc),                                       \
		        size,                                                                      \
		        cnt->exec.stack->top_ptr - size,                                           \
		        type_name);                                                                \
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

SmallArrayType(ConstValue, MirConstValue, 32);

/*************/
/* fwd decls */
/*************/
static void
reset_stack(VMStack *stack);

static void
copy_comptime_to_stack(VM *vm, VMStackPtr dest_ptr, MirConstValue *src_value);

/* zero max nesting = unlimited nesting */
static void
print_call_stack(VM *vm, size_t max_nesting);

static void
dyncall_cb_read_arg(VM *vm, MirConstValue *dest, DCArgs *src);

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
execute_fn_impl_top_level(VM *vm, MirFn *fn, SmallArray_ConstValue *args, VMStackPtr *out_ptr);

static bool
_execute_fn_top_level(VM *                   vm,
                      MirFn *                fn,
                      MirInstr *             call,   /* Optional */
                      SmallArray_ConstValue *args,   /* Optional */
                      VMStackPtr *           out_ptr /* Optional */
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
interp_instr_type_info(VM *vm, MirInstrTypeInfo *type_info);

static void
interp_instr_cast(VM *vm, MirInstrCast *cast);

static void
interp_instr_addrof(VM *vm, MirInstrAddrOf *addrof);

static void
interp_instr_br(VM *vm, MirInstrBr *br);

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
interp_instr_decl_var(VM *vm, MirInstrDeclVar *var);

static void
interp_instr_decl_ref(VM *vm, MirInstrDeclRef *ref);

static void
interp_instr_decl_direct_ref(VM *vm, MirInstrDeclDirectRef *ref);

/***********/
/* inlines */
/***********/
static inline MirFn *
get_callee(MirInstrCall *call)
{
	MirConstValue *callee_val = &call->callee->value;
	BL_ASSERT(callee_val->type && callee_val->type->kind == MIR_TYPE_FN);

	MirFn *fn = callee_val->data.v_ptr.data.fn;
	BL_ASSERT(fn);
	return fn;
}

static inline void
exec_abort(VM *vm, int32_t report_stack_nesting)
{
	print_call_stack(vm, report_stack_nesting);
	vm->stack->aborted = true;
}

static inline size_t
stack_alloc_size(size_t size)
{
	BL_ASSERT(size != 0);
	size += CHCK_SIZE();
	return size + (MAX_ALIGNMENT - (size % MAX_ALIGNMENT));
}

/* allocate memory on frame stack, size is in bits!!! */
static inline VMStackPtr
stack_alloc(VM *vm, size_t size)
{
	BL_ASSERT(size && "trying to allocate 0 bits on stack");

#if BL_DEBUG && CHCK_STACK
	const size_t orig_size = size;
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
stack_free(VM *vm, size_t size)
{
#if BL_DEBUG && CHCK_STACK
	const size_t orig_size = size;
#endif

	size               = stack_alloc_size(size);
	VMStackPtr new_top = vm->stack->top_ptr - size;
	if (new_top < (uint8_t *)(vm->stack->ra + 1)) BL_ABORT("Stack underflow!!!");
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
push_stack_empty(VM *vm, MirType *type)
{
	BL_ASSERT(type);
	const size_t size = type->store_size_bytes;
	BL_ASSERT(size && "pushing zero sized data on stack");
	VMStackPtr tmp = stack_alloc(vm, size);

	LOG_PUSH_STACK;
	return tmp;
}

static inline VMStackPtr
push_stack(VM *vm, void *value, MirType *type)
{
	BL_ASSERT(value && "try to push NULL value");
	VMStackPtr   tmp  = push_stack_empty(vm, type);
	const size_t size = type->store_size_bytes;
	memcpy(tmp, value, size);

	/* pointer relative to frame top */
	return tmp;
}

static inline VMStackPtr
pop_stack(VM *vm, MirType *type)
{
	BL_ASSERT(type);
	const size_t size = type->store_size_bytes;
	BL_ASSERT(size && "popping zero sized data on stack");

	LOG_POP_STACK;

	return stack_free(vm, size);
}

/* Global variables are allocated in static data segment, so there is no need to
 * use relative pointer. When we set ignore to true original pointer is returned
 * as absolute pointer to the stack.  */
static inline VMStackPtr
read_stack_ptr(VM *vm, VMRelativeStackPtr rel_ptr, bool ignore)
{
	if (ignore) return (VMStackPtr)rel_ptr;
	BL_ASSERT(rel_ptr);

	VMStackPtr base = (VMStackPtr)vm->stack->ra;
	BL_ASSERT(base);
	return base + rel_ptr;
}

/* Return pointer to value evaluated from src instruction. Source can be compile
 * time constant or allocated on the stack.*/
static inline VMStackPtr
fetch_value(VM *vm, MirInstr *src)
{
	if (src->comptime || src->kind == MIR_INSTR_DECL_REF ||
	    src->kind == MIR_INSTR_DECL_DIRECT_REF) {
		return (VMStackPtr)&src->value.data;
	}

	return pop_stack(vm, src->value.type);
}

static inline void
read_value(MirConstValueData *dest, VMStackPtr src, MirType *type)
{
	BL_ASSERT(dest && src && type);
	const size_t size = type->store_size_bytes;

	switch (type->kind) {
	case MIR_TYPE_INT:
	case MIR_TYPE_REAL:
	case MIR_TYPE_ENUM:
	case MIR_TYPE_BOOL:
	case MIR_TYPE_NULL:
		memcpy(dest, src, size);
		break;

	case MIR_TYPE_FN:
		mir_set_const_ptr(&dest->v_ptr, *(MirFn **)src, MIR_CP_FN);
		break;

	case MIR_TYPE_TYPE:
		mir_set_const_ptr(&dest->v_ptr, *(MirType **)src, MIR_CP_TYPE);
		break;

	case MIR_TYPE_PTR:
		mir_set_const_ptr(&dest->v_ptr, *(void **)src, MIR_CP_UNKNOWN);
		break;

	default: {
		char type_name[256];
		mir_type_to_str(type_name, 256, type, true);
		BL_ABORT("Cannot load pointer to value of type '%s'", type_name);
	}
	}
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
	BL_ASSERT(!var->comptime && "cannot allocate compile time constant");
	/* allocate memory for variable on stack */

	VMStackPtr tmp     = push_stack_empty(vm, var->value.type);
	var->rel_stack_ptr = tmp - (VMStackPtr)vm->stack->ra;
	return var->rel_stack_ptr;
}

static inline void
stack_alloc_local_vars(VM *vm, MirFn *fn)
{
	BL_ASSERT(fn);
	/* Init all stack variables. */
	BArray *vars = fn->variables;
	MirVar *var;
	BARRAY_FOREACH(vars, var)
	{
		if (var->comptime) continue;
		stack_alloc_var(vm, var);
	}
}

/********/
/* impl */
/********/
void
print_call_stack(VM *vm, size_t max_nesting)
{
	MirInstr *instr = vm->stack->pc;
	VMFrame * fr    = vm->stack->ra;
	size_t    n     = 0;

	if (!instr) return;
	/* print last instruction */
	builder_msg(vm->builder, BUILDER_MSG_LOG, 0, instr->node->location, BUILDER_CUR_WORD, "");

	while (fr) {
		instr = (MirInstr *)fr->caller;
		fr    = fr->prev;
		if (!instr) break;

		if (max_nesting && n == max_nesting) {
			msg_note("continue...");
			break;
		}

		builder_msg(
		    vm->builder, BUILDER_MSG_LOG, 0, instr->node->location, BUILDER_CUR_WORD, "");
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
	const size_t size = stack_alloc_size(sizeof(VMStack));
	stack->used_bytes = size;
	stack->top_ptr    = (uint8_t *)stack + size;
}

/*
 * Produce decomposition of compile time known value to the stack location. Stack location must have
 * enough allocated space.
 */
void
copy_comptime_to_stack(VM *vm, VMStackPtr dest_ptr, MirConstValue *src_value)
{
	/* This may cause recursive calls for aggregate data types. */
	BL_ASSERT(dest_ptr && src_value);
	MirConstValueData *data     = &src_value->data;
	MirType *          src_type = src_value->type;
	BL_ASSERT(src_type);

	switch (src_type->kind) {
	case MIR_TYPE_SLICE:
	case MIR_TYPE_STRING:
	case MIR_TYPE_VARGS:
	case MIR_TYPE_STRUCT: {
		if (src_value->data.v_struct.is_zero_initializer) {
			memset(dest_ptr, 0, src_type->store_size_bytes);
		} else {
			SmallArray_ConstValuePtr *members = data->v_struct.members;
			MirConstValue *           member;

			BL_ASSERT(members);
			const size_t memc = members->size;
			for (uint32_t i = 0; i < memc; ++i) {
				member = members->data[i];

				/* copy all members to variable allocated memory on the
				 * stack */
				VMStackPtr elem_dest_ptr =
				    dest_ptr +
				    mir_get_struct_elem_offest(vm->assembly, src_type, i);
				BL_ASSERT(elem_dest_ptr);
				copy_comptime_to_stack(vm, elem_dest_ptr, member);
			}
		}
		break;
	}

	case MIR_TYPE_ARRAY: {
		if (src_value->data.v_array.is_zero_initializer) {
			memset(dest_ptr, 0, src_type->store_size_bytes);
		} else {
			SmallArray_ConstValuePtr *elems = data->v_array.elems;
			MirConstValue *           elem;

			BL_ASSERT(elems);
			const size_t memc = elems->size;
			for (uint32_t i = 0; i < memc; ++i) {
				elem = elems->data[i];

				/* copy all elems to variable allocated memory on the stack
				 */
				VMStackPtr elem_dest_ptr =
				    dest_ptr + mir_get_array_elem_offset(src_type, i);
				copy_comptime_to_stack(vm, elem_dest_ptr, elem);
			}
		}

		break;
	}

	case MIR_TYPE_PTR: {
		MirConstPtr *const_ptr = &src_value->data.v_ptr;
		switch (const_ptr->kind) {

		case MIR_CP_VAR: {
			MirVar *var = const_ptr->data.var;
			BL_ASSERT(var);

			VMStackPtr var_ptr =
			    read_stack_ptr(vm, var->rel_stack_ptr, var->is_in_gscope);
			memcpy(dest_ptr, &var_ptr, src_type->store_size_bytes);
			break;
		}

		default: {
			memcpy(dest_ptr, (VMStackPtr)src_value, src_type->store_size_bytes);
		}
		}

		break;
	}

	default:
		BL_ASSERT(dest_ptr && "Invalid destination pointer");
		BL_ASSERT(src_value && "Invalid source value pointer");
		memcpy(dest_ptr, (VMStackPtr)src_value, src_type->store_size_bytes);
	}
}

void
dyncall_cb_read_arg(VM *vm, MirConstValue *dest, DCArgs *src)
{
	BL_ASSERT(dest->type && "Argument destination has no type specified.");

	memset(&dest->data, 0, sizeof(dest->data));

	switch (dest->type->kind) {
	case MIR_TYPE_INT: {
		const size_t bitcount = dest->type->data.integer.bitcount;
		switch (bitcount) {
		case 8:
			dest->data.v_u8 = dcbArgUChar(src);
			break;
		case 16:
			dest->data.v_u16 = dcbArgUShort(src);
			break;
		case 32:
			dest->data.v_u32 = dcbArgULong(src);
			break;
		case 64:
			dest->data.v_u64 = dcbArgULongLong(src);
			break;
		default:
			BL_ABORT("invalid bitcount");
		}

		break;
	}

	case MIR_TYPE_REAL: {
		const size_t bitcount = dest->type->data.real.bitcount;
		switch (bitcount) {
		case 32:
			dest->data.v_f32 = dcbArgFloat(src);
			break;
		case 64:
			dest->data.v_f64 = dcbArgDouble(src);
			break;
		default:
			BL_ABORT("invalid bitcount");
		}

		break;
	}

	case MIR_TYPE_BOOL: {
		dest->data.v_bool = dcbArgBool(src);
		break;
	}

	case MIR_TYPE_PTR: {
		mir_set_const_ptr(&dest->data.v_ptr, dcbArgPointer(src), MIR_CP_STACK);
		break;
	}

	default:
		BL_UNIMPLEMENTED;
	}
}

char
dyncall_cb_handler(DCCallback *cb, DCArgs *args, DCValue *result, void *userdata)
{
	/* TODO: External callback can be invoked from different thread. This can cause problems for
	 * now since interpreter is strictly single-threaded, but we must handle such situation in
	 * future. */
	BL_ASSERT(thread_get_id() == main_thread_id &&
	          "External callback handler must be invoked from main thread.");

	DyncallCBContext *cnt = (DyncallCBContext *)userdata;
	BL_ASSERT(cnt && cnt->fn && cnt->vm);

	const bool is_fn_extern = IS_FLAG(cnt->fn->flags, FLAG_EXTERN);
	const bool has_args     = cnt->fn->type->data.fn.arg_types;
	const bool has_return   = cnt->fn->type->data.fn.ret_type->kind != MIR_TYPE_VOID;

	if (is_fn_extern) {
		/* TODO: external callback */
		/* TODO: external callback */
		/* TODO: external callback */
		BL_ABORT("External function used as callback is not supported yet!");
	}

	if (has_args) {
		SmallArray_ConstValue arg_tmp;
		sa_init(&arg_tmp);

		SmallArray_TypePtr *arg_types = cnt->fn->type->data.fn.arg_types;
		sa_resize_ConstValue(&arg_tmp, arg_types->size);

		MirType *it;
		SARRAY_FOREACH(arg_types, it)
		{
			arg_tmp.data[i].type = it;
			dyncall_cb_read_arg(cnt->vm, &arg_tmp.data[i], args);
		}

		/* Push all arguments in reverse order on the stack. */
		for (size_t i = arg_types->size; i-- > 0;) {
			VMStackPtr dest_ptr = push_stack_empty(cnt->vm, arg_types->data[i]);
			copy_comptime_to_stack(cnt->vm, dest_ptr, &arg_tmp.data[i]);
		}

		sa_terminate(&arg_tmp);
	}

	/* Push current frame stack top. (Later poped by ret instruction) */
	push_ra(cnt->vm, NULL);
	BL_ASSERT(cnt->fn->first_block->entry_instr);

	stack_alloc_local_vars(cnt->vm, cnt->fn);

	/* setup entry instruction */
	set_pc(cnt->vm, cnt->fn->first_block->entry_instr);

	if (has_return) {
		/* TODO: handle return value */
		/* TODO: handle return value */
		/* TODO: handle return value */
		BL_ABORT("Callback return value is not implemented yet.");
	}

	return DC_SIGCHAR_VOID;
}

void
_dyncall_generate_signature(VM *vm, MirType *type)
{
	SmallArray_Char *tmp = &vm->dyncall_sig_tmp;

	switch (type->kind) {
	case MIR_TYPE_FN: {
		if (type->data.fn.arg_types) {
			MirType *arg_type;
			SARRAY_FOREACH(type->data.fn.arg_types, arg_type)
			{
				_dyncall_generate_signature(vm, arg_type);
			}
		}
		sa_push_Char(tmp, DC_SIGCHAR_ENDARG);
		_dyncall_generate_signature(vm, type->data.fn.ret_type);
		break;
	}

	case MIR_TYPE_INT: {
		const bool is_signed = type->data.integer.is_signed;
		switch (type->data.integer.bitcount) {
		case 8:
			sa_push_Char(tmp, is_signed ? DC_SIGCHAR_CHAR : DC_SIGCHAR_UCHAR);
			break;
		case 16:
			sa_push_Char(tmp, is_signed ? DC_SIGCHAR_SHORT : DC_SIGCHAR_USHORT);
			break;
		case 32:
			sa_push_Char(tmp, is_signed ? DC_SIGCHAR_INT : DC_SIGCHAR_UINT);
			break;
		case 64:
			sa_push_Char(tmp, is_signed ? DC_SIGCHAR_LONGLONG : DC_SIGCHAR_ULONGLONG);
			break;
		}
		break;
	}

	case MIR_TYPE_REAL: {
		switch (type->data.real.bitcount) {
		case 32:
			sa_push_Char(tmp, DC_SIGCHAR_FLOAT);
			break;
		case 64:
			sa_push_Char(tmp, DC_SIGCHAR_DOUBLE);
			break;
		}
		break;
	}

	case MIR_TYPE_NULL:
	case MIR_TYPE_PTR: {
		sa_push_Char(tmp, DC_SIGCHAR_POINTER);
		break;
	}

	case MIR_TYPE_VOID: {
		sa_push_Char(tmp, DC_SIGCHAR_VOID);
		break;
	}

	case MIR_TYPE_BOOL: {
		sa_push_Char(tmp, DC_SIGCHAR_BOOL);
		break;
	}

	default:
		BL_ABORT("Unsupported external callback type.");
	}
}

const char *
dyncall_generate_signature(VM *vm, MirType *type)
{
	SmallArray_Char *tmp = &vm->dyncall_sig_tmp;
	tmp->size            = 0; /* reset size */

	_dyncall_generate_signature(vm, type);
	sa_push_Char(tmp, '\0');

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

	/* CLEANUP: include dvm into VM */
	/* CLEANUP: include dvm into VM */
	/* CLEANUP: include dvm into VM */
	/* CLEANUP: include dvm into VM */
	DCCallVM *dvm = vm->assembly->dl.vm;
	BL_ASSERT(dvm);
	MirConstValueData tmp = {0};
	read_value(&tmp, val_ptr, type);

	if (type->kind == MIR_TYPE_ENUM) {
		type = type->data.enm.base_type;
	}

	switch (type->kind) {
	case MIR_TYPE_BOOL: {
		dcArgBool(dvm, tmp.v_bool);
		break;
	}

	case MIR_TYPE_INT: {
		switch (type->data.integer.bitcount) {
		case 64:
			dcArgLongLong(dvm, tmp.v_s64);
			break;
		case 32:
			dcArgInt(dvm, (DCint)tmp.v_s32);
			break;
		case 16:
			dcArgShort(dvm, (DCshort)tmp.v_s16);
			break;
		case 8:
			dcArgChar(dvm, (DCchar)tmp.v_s8);
			break;
		default:
			BL_ABORT("unsupported external call integer argument type");
		}
		break;
	}

	case MIR_TYPE_REAL: {
		switch (type->data.real.bitcount) {
		case 32:
			dcArgFloat(dvm, tmp.v_f32);
			break;
		case 64:
			dcArgDouble(dvm, tmp.v_f64);
			break;
		default:
			BL_ABORT("unsupported external call integer argument type");
		}
		break;
	}

	case MIR_TYPE_NULL: {
		dcArgPointer(dvm, (DCpointer)tmp.v_ptr.data.any);
		break;
	}

	case MIR_TYPE_PTR: {
		if (mir_deref_type(type)->kind == MIR_TYPE_FN) {
			MirConstValue *value = (MirConstValue *)val_ptr;
			BL_ASSERT(value->type->kind == MIR_TYPE_PTR);
			BL_ASSERT(value->data.v_ptr.data.any);
			BL_ASSERT(value->data.v_ptr.kind == MIR_CP_VALUE);

			value = value->data.v_ptr.data.value;
			BL_ASSERT(value->type->kind == MIR_TYPE_FN);
			BL_ASSERT(value->data.v_ptr.data.any);
			BL_ASSERT(value->data.v_ptr.kind == MIR_CP_FN);

			MirFn *fn = value->data.v_ptr.data.fn;
			dcArgPointer(dvm, (DCpointer)dyncall_fetch_callback(vm, fn));
		} else {
			dcArgPointer(dvm, (DCpointer)tmp.v_ptr.data.any);
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
	BL_ASSERT(fn->dyncall.extern_entry);
	dcMode(dvm, DC_CALL_C_DEFAULT);
	dcReset(dvm);

	/* pop all arguments from the stack */
	VMStackPtr           arg_ptr;
	SmallArray_InstrPtr *arg_values = call->args;
	if (arg_values) {
		MirInstr *arg_value;
		SARRAY_FOREACH(arg_values, arg_value)
		{
			arg_ptr = fetch_value(vm, arg_value);
			dyncall_push_arg(vm, arg_ptr, arg_value->value.type);
		}
	}

	bool does_return = true;

	MirConstValueData result = {0};
	switch (ret_type->kind) {
	case MIR_TYPE_INT:
		switch (ret_type->store_size_bytes) {
		case sizeof(char):
			result.v_s8 = dcCallChar(dvm, fn->dyncall.extern_entry);
			break;
		case sizeof(short):
			result.v_s16 = dcCallShort(dvm, fn->dyncall.extern_entry);
			break;
		case sizeof(int):
			result.v_s32 = dcCallInt(dvm, fn->dyncall.extern_entry);
			break;
		case sizeof(long long):
			result.v_s64 = dcCallLongLong(dvm, fn->dyncall.extern_entry);
			break;
		default:
			BL_ABORT("unsupported integer size for external call result");
		}
		break;

	case MIR_TYPE_ENUM:
		switch (ret_type->data.enm.base_type->store_size_bytes) {
		case sizeof(char):
			result.v_s8 = dcCallChar(dvm, fn->dyncall.extern_entry);
			break;
		case sizeof(short):
			result.v_s16 = dcCallShort(dvm, fn->dyncall.extern_entry);
			break;
		case sizeof(int):
			result.v_s32 = dcCallInt(dvm, fn->dyncall.extern_entry);
			break;
		case sizeof(long long):
			result.v_s64 = dcCallLongLong(dvm, fn->dyncall.extern_entry);
			break;
		default:
			BL_ABORT("unsupported integer size for external call result");
		}
		break;

	case MIR_TYPE_PTR:
		result.v_ptr.data.any = dcCallPointer(dvm, fn->dyncall.extern_entry);
		break;

	case MIR_TYPE_REAL: {
		switch (ret_type->store_size_bytes) {
		case sizeof(float):
			result.v_f32 = dcCallFloat(dvm, fn->dyncall.extern_entry);
			break;
		case sizeof(double):
			result.v_f64 = dcCallDouble(dvm, fn->dyncall.extern_entry);
			break;
		default:
			BL_ABORT("unsupported real number size for external call "
			         "result");
		}
		break;
	}

	case MIR_TYPE_VOID:
		dcCallVoid(dvm, fn->dyncall.extern_entry);
		does_return = false;
		break;

	default:
		BL_ABORT("unsupported external call return type");
	}

	/* PUSH result only if it is used */
	if (call->base.ref_count > 1 && does_return) {
		push_stack(vm, (VMStackPtr)&result, ret_type);
	}
}

bool
execute_fn_top_level(VM *vm, MirInstr *call, VMStackPtr *out_ptr)
{
	return _execute_fn_top_level(vm, get_callee((MirInstrCall *)call), call, NULL, out_ptr);
}

bool
execute_fn_impl_top_level(VM *vm, MirFn *fn, SmallArray_ConstValue *args, VMStackPtr *out_ptr)
{
	return _execute_fn_top_level(vm, fn, NULL, args, out_ptr);
}

bool
_execute_fn_top_level(VM *                   vm,
                      MirFn *                fn,
                      MirInstr *             call,
                      SmallArray_ConstValue *args,
                      VMStackPtr *           out_ptr)
{
	BL_ASSERT(fn);

	if (!fn->fully_analyzed)
		BL_ABORT("Function is not fully analyzed for compile time execution!!!");

	MirType *  ret_type             = fn->type->data.fn.ret_type;
	const bool does_return_value    = ret_type->kind != MIR_TYPE_VOID;
	const bool is_return_value_used = call ? call->ref_count > 1 : true;
	const bool is_caller_comptime   = call ? call->comptime : false;
	const bool pop_return_value =
	    does_return_value && is_return_value_used && !is_caller_comptime;

	if (args) {
		BL_ASSERT(
		    !call &&
		    "Caller instruction cannot be used when call arguments are passed explicitly.");
		SmallArray_TypePtr *arg_types = fn->type->data.fn.arg_types;

		BL_ASSERT(arg_types->size == args->size &&
		          "Invalid count of eplicitly passed arguments");

		/* Push all arguments in reverse order on the stack. */
		for (size_t i = arg_types->size; i-- > 0;) {
			VMStackPtr dest_ptr = push_stack_empty(vm, arg_types->data[i]);
			copy_comptime_to_stack(vm, dest_ptr, &args->data[i]);
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
		if (get_pc(vm) == prev) set_pc(vm, instr->next);
	}

	if (vm->stack->aborted) return false;

	if (pop_return_value) {
		VMStackPtr ret_ptr = pop_stack(vm, ret_type);
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
	case MIR_INSTR_TYPE_INFO:
		interp_instr_type_info(vm, (MirInstrTypeInfo *)instr);
		break;
	case MIR_INSTR_COMPOUND:
		interp_instr_compound(vm, NULL, (MirInstrCompound *)instr);
		break;
	case MIR_INSTR_TOANY:
		interp_instr_toany(vm, (MirInstrToAny *)instr);
		break;

	default:
		BL_ABORT("missing execution for instruction: %s", mir_instr_name(instr));
	}
}

void
interp_instr_toany(VM *vm, MirInstrToAny *toany)
{
	MirVar *   tmp      = toany->tmp;
	MirVar *   expr_tmp = toany->expr_tmp;
	VMStackPtr tmp_ptr  = read_stack_ptr(vm, tmp->rel_stack_ptr, tmp->is_in_gscope);
	MirType *  tmp_type = tmp->value.type;

	/* type_info */
	MirVar *expr_type_rtti = toany->rtti_type->rtti.var;
	BL_ASSERT(expr_type_rtti);

	VMStackPtr dest           = tmp_ptr + mir_get_struct_elem_offest(vm->assembly, tmp_type, 0);
	MirType *  type_info_type = mir_get_struct_elem_type(tmp_type, 0);

	VMStackPtr rtti_ptr =
	    read_stack_ptr(vm, expr_type_rtti->rel_stack_ptr, expr_type_rtti->is_in_gscope);

	memcpy(dest, &rtti_ptr, type_info_type->store_size_bytes);

	VMStackPtr data_ptr = fetch_value(vm, toany->expr);

	/* data */
	dest               = tmp_ptr + mir_get_struct_elem_offest(vm->assembly, tmp_type, 1);
	MirType *data_type = mir_get_struct_elem_type(tmp_type, 1);

	if (!toany->has_data) {
		memset(dest, 0, data_type->store_size_bytes);
	} else if (toany->rtti_type_specification) {
		/* Use type specificaiton as an data value. */
		MirVar *spec_type_rtti = toany->rtti_type_specification->rtti.var;
		BL_ASSERT(spec_type_rtti);

		VMStackPtr rtti_spec_ptr =
		    read_stack_ptr(vm, spec_type_rtti->rel_stack_ptr, spec_type_rtti->is_in_gscope);

		memcpy(dest, &rtti_spec_ptr, PTR_SIZE);
	} else if (expr_tmp) { // set data
		VMStackPtr expr_tmp_ptr =
		    read_stack_ptr(vm, expr_tmp->rel_stack_ptr, expr_tmp->is_in_gscope);

		if (toany->expr->comptime) {
			copy_comptime_to_stack(vm, expr_tmp_ptr, (MirConstValue *)data_ptr);
		} else {
			memcpy(expr_tmp_ptr, data_ptr, data_type->store_size_bytes);
		}

		memcpy(dest, &expr_tmp_ptr, data_type->store_size_bytes);
	} else {
		memcpy(dest, data_ptr, data_type->store_size_bytes);
	}

	push_stack(vm, &tmp_ptr, toany->base.value.type);
}

void
interp_instr_phi(VM *vm, MirInstrPhi *phi)
{
	MirInstrBlock *prev_block = vm->stack->prev_block;
	BL_ASSERT(prev_block && "Invalid previous block for phi instruction.");
	BL_ASSERT(phi->incoming_blocks && phi->incoming_values);
	BL_ASSERT(phi->incoming_blocks->size == phi->incoming_values->size);

	const size_t c = phi->incoming_values->size;
	BL_ASSERT(c > 0);

	MirInstr *     value = NULL;
	MirInstrBlock *block;
	for (size_t i = 0; i < c; ++i) {
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

		VMStackPtr value_ptr = fetch_value(vm, value);

		if (phi->base.comptime) {
			memcpy(&phi->base.value.data, value_ptr, sizeof(phi->base.value.data));
		} else {
			push_stack(vm, value_ptr, phi_type);
		}
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

	VMStackPtr ptr = fetch_value(vm, src);

	ptr = ((MirConstValueData *)ptr)->v_ptr.data.stack_ptr;

	if (addrof->base.comptime) {
		// memcpy(&addrof->base.value.data, ptr, sizeof(addrof->base.value.data));
		mir_set_const_ptr(
		    &addrof->base.value.data.v_ptr, *(VMStackPtr **)ptr, MIR_CP_VALUE);
	} else {
		push_stack(vm, (VMStackPtr)&ptr, type);
	}
}

void
interp_instr_type_info(VM *vm, MirInstrTypeInfo *type_info)
{
	// HACK: cleanup stack
	fetch_value(vm, type_info->expr);

	MirVar *type_info_var = type_info->expr_type->rtti.var;
	BL_ASSERT(type_info_var);

	MirType *type = type_info->base.value.type;
	BL_ASSERT(type);

	VMStackPtr ptr =
	    read_stack_ptr(vm, type_info_var->rel_stack_ptr, type_info_var->is_in_gscope);

	push_stack(vm, (VMStackPtr)&ptr, type);
}

void
interp_instr_elem_ptr(VM *vm, MirInstrElemPtr *elem_ptr)
{
	/* pop index from stack */
	BL_ASSERT(mir_is_pointer_type(elem_ptr->arr_ptr->value.type));
	MirType *         arr_type   = mir_deref_type(elem_ptr->arr_ptr->value.type);
	MirType *         index_type = elem_ptr->index->value.type;
	VMStackPtr        index_ptr  = fetch_value(vm, elem_ptr->index);
	MirConstValueData result     = {0};

	VMStackPtr arr_ptr = fetch_value(vm, elem_ptr->arr_ptr);
	arr_ptr            = ((MirConstValueData *)arr_ptr)->v_ptr.data.stack_ptr;
	BL_ASSERT(arr_ptr && index_ptr);

	MirConstValueData index = {0};
	read_value(&index, index_ptr, index_type);

	if (elem_ptr->target_is_slice) {
		MirType *len_type = mir_get_struct_elem_type(arr_type, MIR_SLICE_LEN_INDEX);
		MirType *ptr_type = mir_get_struct_elem_type(arr_type, MIR_SLICE_PTR_INDEX);

		MirType *elem_type = mir_deref_type(ptr_type);
		BL_ASSERT(elem_type);

		MirConstValueData ptr_tmp = {0};
		MirConstValueData len_tmp = {0};
		const ptrdiff_t   len_member_offset =
		    mir_get_struct_elem_offest(vm->assembly, arr_type, 0);
		const ptrdiff_t ptr_member_offset =
		    mir_get_struct_elem_offest(vm->assembly, arr_type, 1);

		VMStackPtr ptr_ptr = arr_ptr + ptr_member_offset;
		VMStackPtr len_ptr = arr_ptr + len_member_offset;

		read_value(&ptr_tmp, ptr_ptr, ptr_type);
		read_value(&len_tmp, len_ptr, len_type);

		if (!ptr_tmp.v_ptr.data.stack_ptr) {
			msg_error("Dereferencing null pointer! Slice has not been set?");
			exec_abort(vm, 0);
		}

		BL_ASSERT(len_tmp.v_s64 > 0);

		if (index.v_s64 >= len_tmp.v_s64) {
			msg_error("Array index is out of the bounds! Array index is: %lli, but "
			          "array size is: %lli",
			          (long long)index.v_s64,
			          (long long)len_tmp.v_s64);
			exec_abort(vm, 0);
		}

		result.v_ptr.data.stack_ptr = (VMStackPtr)(
		    (ptr_tmp.v_ptr.data.stack_ptr) + (index.v_u64 * elem_type->store_size_bytes));
	} else {
		MirType *elem_type = arr_type->data.array.elem_type;
		BL_ASSERT(elem_type);

		{
			const int64_t len = arr_type->data.array.len;
			if (index.v_s64 >= len) {
				msg_error("Array index is out of the bounds! Array index "
				          "is: %lli, "
				          "but array size "
				          "is: %lli",
				          (long long)index.v_s64,
				          (long long)len);
				exec_abort(vm, 0);
			}
		}

		if (elem_ptr->arr_ptr->comptime) BL_ABORT_ISSUE(57);

		result.v_ptr.data.stack_ptr =
		    (VMStackPtr)((arr_ptr) + (index.v_u64 * elem_type->store_size_bytes));

#if BL_DEBUG
		{
			ptrdiff_t _diff = result.v_u64 - (uintptr_t)arr_ptr;
			BL_ASSERT(_diff / elem_type->store_size_bytes == index.v_u64);
		}
#endif
	}

	/* push result address on the stack */
	push_stack(vm, &result, elem_ptr->base.value.type);
}

void
interp_instr_member_ptr(VM *vm, MirInstrMemberPtr *member_ptr)
{
	BL_ASSERT(member_ptr->target_ptr);
	MirType *         target_type = member_ptr->target_ptr->value.type;
	MirConstValueData result      = {0};

	/* lookup for base structure declaration type
	 * IDEA: maybe we can store parent type to the member type? But what about
	 * builtin types???
	 */
	BL_ASSERT(target_type->kind == MIR_TYPE_PTR && "expected pointer");
	target_type = mir_deref_type(target_type);
	BL_ASSERT(mir_is_composit_type(target_type) && "expected structure");

	/* fetch address of the struct begin */
	VMStackPtr ptr = fetch_value(vm, member_ptr->target_ptr);
	ptr            = ((MirConstValueData *)ptr)->v_ptr.data.stack_ptr;
	BL_ASSERT(ptr);

	if (member_ptr->builtin_id == MIR_BUILTIN_ID_NONE) {
		BL_ASSERT(member_ptr->scope_entry &&
		          member_ptr->scope_entry->kind == SCOPE_ENTRY_MEMBER);
		MirMember *member = member_ptr->scope_entry->data.member;
		BL_ASSERT(member);
		const int64_t index = member->index;

		/* let the llvm solve poiner offest */
		const ptrdiff_t ptr_offset =
		    mir_get_struct_elem_offest(vm->assembly, target_type, (uint32_t)index);

		result.v_ptr.data.stack_ptr = ptr + ptr_offset; // pointer shift
	} else {
		/* builtin member */
		if (member_ptr->builtin_id == MIR_BUILTIN_ID_ARR_PTR) {
			/* slice .ptr */
			const ptrdiff_t ptr_offset =
			    mir_get_struct_elem_offest(vm->assembly, target_type, 1);
			result.v_ptr.data.stack_ptr = ptr + ptr_offset; // pointer shift
		} else if (member_ptr->builtin_id == MIR_BUILTIN_ID_ARR_LEN) {
			/* slice .len*/
			const ptrdiff_t len_offset =
			    mir_get_struct_elem_offest(vm->assembly, target_type, 0);
			result.v_ptr.data.stack_ptr = ptr + len_offset; // pointer shift
		} else {
			BL_ABORT("invalid slice member!");
		}
	}

	/* push result address on the stack */
	push_stack(vm, &result, member_ptr->base.value.type);
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
interp_instr_cast(VM *vm, MirInstrCast *cast)
{
	MirType *         src_type  = cast->expr->value.type;
	MirType *         dest_type = cast->base.value.type;
	MirConstValueData tmp       = {0};

	switch (cast->op) {
	case MIR_CAST_NONE:
	case MIR_CAST_BITCAST:
		if (cast->base.comptime)
			memcpy(&cast->base.value.data,
			       &cast->expr->value.data,
			       sizeof(cast->expr->value.data));
		break;

	case MIR_CAST_SEXT: {
		/* src is smaller than dest */
		VMStackPtr from_ptr = fetch_value(vm, cast->expr);
		read_value(&tmp, from_ptr, src_type);

#define sext_case(v, T)                                                                            \
	case sizeof(v.T):                                                                          \
		tmp.v_s64 = (int64_t)tmp.T;                                                        \
		break;

		// clang-format off
		switch (src_type->store_size_bytes)
		{
			sext_case(tmp, v_s8)
				sext_case(tmp, v_s16)
				sext_case(tmp, v_s32)
		default:
			BL_ABORT("Invalid sext cast!");
		}
		// clang-format on

#undef sext_case

		if (cast->base.comptime)
			memcpy(&cast->base.value.data, &tmp, sizeof(tmp));
		else
			push_stack(vm, (VMStackPtr)&tmp, dest_type);

		break;
	}

	case MIR_CAST_FPEXT: {
		/* src is smaller than dest */
		VMStackPtr from_ptr = fetch_value(vm, cast->expr);
		read_value(&tmp, from_ptr, src_type);

		tmp.v_f64 = (double)tmp.v_f32;

		if (cast->base.comptime)
			memcpy(&cast->base.value.data, &tmp, sizeof(tmp));
		else
			push_stack(vm, (VMStackPtr)&tmp, dest_type);
		break;
	}

	case MIR_CAST_FPTRUNC: {
		/* src is bigger than dest */
		VMStackPtr from_ptr = fetch_value(vm, cast->expr);
		read_value(&tmp, from_ptr, src_type);

		tmp.v_f32 = (float)tmp.v_f64;

		if (cast->base.comptime)
			memcpy(&cast->base.value.data, &tmp, sizeof(tmp));
		else
			push_stack(vm, (VMStackPtr)&tmp, dest_type);
		break;
	}

	case MIR_CAST_FPTOSI: {
		/* real to signed integer */
		VMStackPtr from_ptr = fetch_value(vm, cast->expr);
		read_value(&tmp, from_ptr, src_type);

		if (src_type->store_size_bytes == sizeof(float))
			tmp.v_s32 = (int32_t)tmp.v_f32;
		else
			tmp.v_s64 = (int64_t)tmp.v_f64;

		if (cast->base.comptime)
			memcpy(&cast->base.value.data, &tmp, sizeof(tmp));
		else
			push_stack(vm, (VMStackPtr)&tmp, dest_type);
		break;
	}

	case MIR_CAST_FPTOUI: {
		/* real to signed integer */
		VMStackPtr from_ptr = fetch_value(vm, cast->expr);
		read_value(&tmp, from_ptr, src_type);

		if (src_type->store_size_bytes == sizeof(float))
			tmp.v_u64 = (uint64_t)tmp.v_f32;
		else
			tmp.v_u64 = (uint64_t)tmp.v_f64;

		if (cast->base.comptime)
			memcpy(&cast->base.value.data, &tmp, sizeof(tmp));
		else
			push_stack(vm, (VMStackPtr)&tmp, dest_type);
		break;
	}

	case MIR_CAST_SITOFP: {
		VMStackPtr from_ptr = fetch_value(vm, cast->expr);
		read_value(&tmp, from_ptr, src_type);

		if (dest_type->store_size_bytes == sizeof(float)) {
			switch (src_type->store_size_bytes) {
			case sizeof(tmp.v_s8):
				tmp.v_f32 = (float)tmp.v_s8;
				break;
			case sizeof(tmp.v_s16):
				tmp.v_f32 = (float)tmp.v_s16;
				break;
			case sizeof(tmp.v_s32):
				tmp.v_f32 = (float)tmp.v_s32;
				break;
			case sizeof(tmp.v_s64):
				tmp.v_f32 = (float)tmp.v_s64;
				break;
			}
		} else {
			switch (src_type->store_size_bytes) {
			case sizeof(tmp.v_s8):
				tmp.v_f64 = (double)tmp.v_s8;
				break;
			case sizeof(tmp.v_s16):
				tmp.v_f64 = (double)tmp.v_s16;
				break;
			case sizeof(tmp.v_s32):
				tmp.v_f64 = (double)tmp.v_s32;
				break;
			case sizeof(tmp.v_s64):
				tmp.v_f64 = (double)tmp.v_s64;
				break;
			}
		}

		if (cast->base.comptime)
			memcpy(&cast->base.value.data, &tmp, sizeof(tmp));
		else
			push_stack(vm, (VMStackPtr)&tmp, dest_type);
		break;
	}

	case MIR_CAST_UITOFP: {
		VMStackPtr from_ptr = fetch_value(vm, cast->expr);
		read_value(&tmp, from_ptr, src_type);

		if (dest_type->store_size_bytes == sizeof(float))
			tmp.v_f32 = (float)tmp.v_u64;
		else
			tmp.v_f64 = (double)tmp.v_u64;

		if (cast->base.comptime)
			memcpy(&cast->base.value.data, &tmp, sizeof(tmp));
		else
			push_stack(vm, (VMStackPtr)&tmp, dest_type);
		break;
	}

	case MIR_CAST_INTTOPTR:
	case MIR_CAST_PTRTOINT: {
		/* noop for same sizes */
		const size_t src_size  = src_type->store_size_bytes;
		const size_t dest_size = dest_type->store_size_bytes;

		if (src_size != dest_size) {
			/* trunc or zero extend */
			VMStackPtr from_ptr = fetch_value(vm, cast->expr);
			read_value(&tmp, from_ptr, src_type);

			if (cast->base.comptime)
				memcpy(&cast->base.value.data, &tmp, sizeof(tmp));
			else
				push_stack(vm, (VMStackPtr)&tmp, dest_type);
		}

		break;
	}

	case MIR_CAST_ZEXT:
	/* src is smaller than dest and destination is unsigned, src value will
	 * be extended with zeros to dest type size */
	case MIR_CAST_TRUNC: {
		/* src is bigger than dest */
		VMStackPtr from_ptr = fetch_value(vm, cast->expr);
		read_value(&tmp, from_ptr, src_type);

		if (cast->base.comptime)
			memcpy(&cast->base.value.data, &tmp, sizeof(tmp));
		else
			push_stack(vm, (VMStackPtr)&tmp, dest_type);
		break;
	}

	default:
		BL_ABORT("invalid cast operation");
	}
}

void
interp_instr_arg(VM *vm, MirInstrArg *arg)
{
	/* Caller is optional, when we call function implicitly there is no call instruction which
	 * we can use, so we need to handle also this situation. In such case we expect all
	 * arguments to be already pushed on the stack. */
	MirInstrCall *caller = (MirInstrCall *)get_ra(vm)->caller;

	if (caller) {
		SmallArray_InstrPtr *arg_values = caller->args;
		BL_ASSERT(arg_values);
		MirInstr *curr_arg_value = arg_values->data[arg->i];

		if (curr_arg_value->comptime) {
			MirType *  type = curr_arg_value->value.type;
			VMStackPtr dest = push_stack_empty(vm, type);

			copy_comptime_to_stack(vm, dest, &curr_arg_value->value);
		} else {
			/* Arguments are located in reverse order right before return address on the
			 * stack
			 * so we can find them inside loop adjusting address up on the stack. */
			MirInstr *arg_value = NULL;
			/* starting point */
			VMStackPtr arg_ptr = (VMStackPtr)vm->stack->ra;
			for (uint32_t i = 0; i <= arg->i; ++i) {
				arg_value = arg_values->data[i];
				BL_ASSERT(arg_value);
				if (arg_value->comptime) continue;
				arg_ptr -=
				    stack_alloc_size(arg_value->value.type->store_size_bytes);
			}

			push_stack(vm, (VMStackPtr)arg_ptr, arg->base.value.type);
		}

		return;
	}

	/* Caller instruction not specified!!! */
	MirFn *fn = arg->base.owner_block->owner_fn;
	BL_ASSERT(fn && "Arg instruction cannot determinate current function");

	/* All arguments must be already on the stack in reverse order. */
	SmallArray_TypePtr *arg_types = fn->type->data.fn.arg_types;
	BL_ASSERT(arg_types && "Function has no arguments");

	MirType *arg_type;
	/* starting point */
	VMStackPtr arg_ptr = (VMStackPtr)vm->stack->ra;
	for (uint32_t i = 0; i <= arg->i; ++i) {
		arg_type = arg_types->data[i];
		BL_ASSERT(arg_type);
		arg_ptr -= stack_alloc_size(arg_type->store_size_bytes);
	}

	push_stack(vm, (VMStackPtr)arg_ptr, arg->base.value.type);
}

void
interp_instr_cond_br(VM *vm, MirInstrCondBr *br)
{
	BL_ASSERT(br->cond);
	MirType *type = br->cond->value.type;

	/* pop condition from stack */
	VMStackPtr cond = fetch_value(vm, br->cond);
	BL_ASSERT(cond);

	MirConstValueData tmp = {0};
	read_value(&tmp, cond, type);

	/* Set previous block. */
	vm->stack->prev_block = br->base.owner_block;
	if (tmp.v_s64) {
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

		const bool use_static_segment = var->is_in_gscope;
		VMStackPtr real_ptr           = NULL;
		if (var->comptime) {
			real_ptr = (VMStackPtr)&var->value;
		} else {
			real_ptr = read_stack_ptr(vm, var->rel_stack_ptr, use_static_segment);
		}

		ref->base.value.data.v_ptr.data.stack_ptr = real_ptr;
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

	const bool use_static_segment = var->is_in_gscope;
	VMStackPtr real_ptr           = NULL;
	if (var->comptime) {
		real_ptr = (VMStackPtr)&var->value;
	} else {
		real_ptr = read_stack_ptr(vm, var->rel_stack_ptr, use_static_segment);
	}

	ref->base.value.data.v_ptr.data.stack_ptr = real_ptr;
}

void
interp_instr_compound(VM *vm, VMStackPtr tmp_ptr, MirInstrCompound *cmp)
{
	if (cmp->base.comptime) {
		/* non-naked */
		if (tmp_ptr) copy_comptime_to_stack(vm, tmp_ptr, &cmp->base.value);
		return;
	}

	const bool will_push = tmp_ptr == NULL;
	if (will_push) {
		BL_ASSERT(cmp->tmp_var && "Missing temp variable for compound.");
		tmp_ptr =
		    read_stack_ptr(vm, cmp->tmp_var->rel_stack_ptr, cmp->tmp_var->is_in_gscope);
	}

	BL_ASSERT(tmp_ptr);

	MirType *  type = cmp->base.value.type;
	MirType *  elem_type;
	VMStackPtr elem_ptr = tmp_ptr;

	MirInstr *value;
	SARRAY_FOREACH(cmp->values, value)
	{
		elem_type = value->value.type;
		switch (type->kind) {

		case MIR_TYPE_STRING:
		case MIR_TYPE_SLICE:
		case MIR_TYPE_VARGS:
		case MIR_TYPE_STRUCT:
			elem_ptr =
			    tmp_ptr + mir_get_struct_elem_offest(vm->assembly, type, (uint32_t)i);
			break;

		case MIR_TYPE_ARRAY:
			elem_ptr = tmp_ptr + mir_get_array_elem_offset(type, (uint32_t)i);
			break;

		default:
			BL_ASSERT(i == 0 && "Invalid elem count for non-agregate type!!!");
		}

		if (value->comptime) {
			copy_comptime_to_stack(vm, elem_ptr, &value->value);
		} else {
			if (value->kind == MIR_INSTR_COMPOUND) {
				interp_instr_compound(vm, elem_ptr, (MirInstrCompound *)value);
			} else {
				VMStackPtr value_ptr = fetch_value(vm, value);
				memcpy(elem_ptr, value_ptr, elem_type->store_size_bytes);
			}
		}
	}

	if (will_push) push_stack(vm, tmp_ptr, cmp->base.value.type);
}

void
interp_instr_vargs(VM *vm, MirInstrVArgs *vargs)
{
	SmallArray_InstrPtr *values    = vargs->values;
	MirVar *             arr_tmp   = vargs->arr_tmp;
	MirVar *             vargs_tmp = vargs->vargs_tmp;

	BL_ASSERT(vargs_tmp->value.type->kind == MIR_TYPE_VARGS);
	BL_ASSERT(vargs_tmp->rel_stack_ptr && "Unalocated vargs slice!!!");
	BL_ASSERT(values);

	VMStackPtr arr_tmp_ptr = arr_tmp ? read_stack_ptr(vm, arr_tmp->rel_stack_ptr, false) : NULL;

	/* Fill vargs tmp array with values from stack or constants. */
	{
		MirInstr * value;
		VMStackPtr value_ptr;
		SARRAY_FOREACH(values, value)
		{
			const size_t value_size = value->value.type->store_size_bytes;
			VMStackPtr   dest       = arr_tmp_ptr + i * value_size;

			if (value->comptime) {
				copy_comptime_to_stack(vm, dest, &value->value);
			} else {
				value_ptr = fetch_value(vm, value);
				memcpy(dest, value_ptr, value_size);
			}
		}
	}

	/* Push vargs slice on the stack. */
	{
		VMStackPtr vargs_tmp_ptr = read_stack_ptr(vm, vargs_tmp->rel_stack_ptr, false);
		// set len
		{
			MirConstValueData len_tmp = {0};
			VMStackPtr        len_ptr =
			    vargs_tmp_ptr + mir_get_struct_elem_offest(vm->assembly,
			                                               vargs_tmp->value.type,
			                                               MIR_SLICE_LEN_INDEX);

			MirType *len_type =
			    mir_get_struct_elem_type(vargs_tmp->value.type, MIR_SLICE_LEN_INDEX);

			len_tmp.v_s64 = values->size;
			memcpy(len_ptr, &len_tmp, len_type->store_size_bytes);
		}

		// set ptr
		{
			MirConstValueData ptr_tmp = {0};
			VMStackPtr        ptr_ptr =
			    vargs_tmp_ptr + mir_get_struct_elem_offest(vm->assembly,
			                                               vargs_tmp->value.type,
			                                               MIR_SLICE_PTR_INDEX);

			MirType *ptr_type =
			    mir_get_struct_elem_type(vargs_tmp->value.type, MIR_SLICE_PTR_INDEX);

			ptr_tmp.v_ptr.data.any = arr_tmp_ptr;
			memcpy(ptr_ptr, &ptr_tmp, ptr_type->store_size_bytes);
		}

		push_stack(vm, vargs_tmp_ptr, vargs_tmp->value.type);
	}
}

void
interp_instr_decl_var(VM *vm, MirInstrDeclVar *decl)
{
	BL_ASSERT(decl->base.value.type);

	MirVar *var = decl->var;
	BL_ASSERT(var);

	/* compile time known variables cannot be modified and does not need stack
	 * allocated memory, const_value is used instead
	 *
	 * already allocated variables will never be allocated again (in case
	 * declaration is inside loop body!!!)
	 */
	if (var->comptime) return;

	const bool use_static_segment = var->is_in_gscope;

	BL_ASSERT(var->rel_stack_ptr);

	/* initialize variable if there is some init value */
	if (decl->init) {
		VMStackPtr var_ptr = read_stack_ptr(vm, var->rel_stack_ptr, use_static_segment);
		BL_ASSERT(var_ptr);

		if (decl->init->comptime) {
			/* Compile time constants of agregate type are stored in different
			 * way, we need to produce decomposition of those data. */
			copy_comptime_to_stack(vm, var_ptr, &decl->init->value);
		} else {
			if (decl->init->kind == MIR_INSTR_COMPOUND) {
				/* used compound initialization!!! */
				interp_instr_compound(vm, var_ptr, (MirInstrCompound *)decl->init);
			} else {
				/* read initialization value if there is one */
				VMStackPtr init_ptr = fetch_value(vm, decl->init);
				memcpy(var_ptr, init_ptr, var->value.type->store_size_bytes);
			}
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

	VMStackPtr src_ptr = fetch_value(vm, load->src);
	src_ptr            = ((MirConstValueData *)src_ptr)->v_ptr.data.stack_ptr;

	if (!src_ptr) {
		msg_error("Dereferencing null pointer!");
		exec_abort(vm, 0);
	}

	if (load->base.comptime) {
		memcpy(&load->base.value.data, src_ptr, sizeof(load->base.value.data));
	} else {
		push_stack(vm, src_ptr, dest_type);
	}
}

void
interp_instr_store(VM *vm, MirInstrStore *store)
{
	/* loads destination (in case it is not direct reference to declaration) and
	 * source from stack
	 */
	MirType *src_type = store->src->value.type;
	BL_ASSERT(src_type);

	VMStackPtr dest_ptr = fetch_value(vm, store->dest);
	VMStackPtr src_ptr  = fetch_value(vm, store->src);

	dest_ptr = ((MirConstValueData *)dest_ptr)->v_ptr.data.stack_ptr;

	BL_ASSERT(dest_ptr && src_ptr);
	if (store->src->comptime) {
		copy_comptime_to_stack(vm, dest_ptr, &store->src->value);
	} else {
		memcpy(dest_ptr, src_ptr, src_type->store_size_bytes);
	}
}

void
interp_instr_call(VM *vm, MirInstrCall *call)
{
	BL_ASSERT(call->callee && call->base.value.type);
	BL_ASSERT(call->callee->value.type);

	VMStackPtr        callee_ptr = fetch_value(vm, call->callee);
	MirConstValueData callee     = {0};

	read_value(&callee, callee_ptr, call->callee->value.type);

	/* Function called via pointer. */
	if (call->callee->value.type->kind == MIR_TYPE_PTR) {
		BL_ASSERT(mir_deref_type(call->callee->value.type)->kind == MIR_TYPE_FN);
		callee.v_ptr.data.fn =
		    callee.v_ptr.data.any ? callee.v_ptr.data.value->data.v_ptr.data.fn : NULL;
	}

	MirFn *fn = callee.v_ptr.data.fn;
	if (fn == NULL) {
		msg_error("Function pointer not set!");
		exec_abort(vm, 0);
		return;
	}

	BL_ASSERT(fn->type);

	if (IS_FLAG(fn->flags, FLAG_EXTERN)) {
		interp_extern_call(vm, fn, call);
	} else {
		/* Push current frame stack top. (Later poped by ret instruction)*/
		push_ra(vm, &call->base);
		BL_ASSERT(fn->first_block->entry_instr);

		stack_alloc_local_vars(vm, fn);

		/* setup entry instruction */
		set_pc(vm, fn->first_block->entry_instr);
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
		ret_data_ptr = fetch_value(vm, ret->value);
		BL_ASSERT(ret_data_ptr);

		if (caller ? caller->base.ref_count == 1 : false) ret_data_ptr = NULL;
	}

	/* do frame stack rollback */
	MirInstr *pc = (MirInstr *)pop_ra(vm);

	/* clean up all arguments from the stack */
	if (caller) {
		SmallArray_InstrPtr *arg_values = caller->args;
		if (arg_values) {
			MirInstr *arg_value;
			SARRAY_FOREACH(arg_values, arg_value)
			{
				if (arg_value->comptime) continue;
				pop_stack(vm, arg_value->value.type);
			}
		}
	} else {
		/* When caller was not specified we expect all argumenst to be pushed on the stack
		 * so we must clear them all. Remember they were pushed in reverse order, so now we
		 * have to pop them in order they are defined. */

		SmallArray_TypePtr *arg_types = fn->type->data.fn.arg_types;
		if (arg_types) {
			MirType *arg_type;
			SARRAY_FOREACH(arg_types, arg_type)
			{
				pop_stack(vm, arg_type);
			}
		}
	}

	/* push return value on the stack if there is one */
	if (ret_data_ptr) {
		/* Determinate if caller instruction is comptime, if caller does not exist we are
		 * going to push result on the stack. */
		const bool is_caller_comptime = caller ? caller->base.comptime : false;
		if (is_caller_comptime) {
			if (ret->value->comptime) {
				caller->base.value.data = ret->value->value.data;
			} else {
				read_value(&caller->base.value.data, ret_data_ptr, ret_type);
			}
		} else {
			if (ret->value->comptime) {
				VMStackPtr dest = push_stack_empty(vm, ret_type);
				copy_comptime_to_stack(vm, dest, &ret->value->value);
			} else {
				push_stack(vm, ret_data_ptr, ret_type);
			}
		}
	}

	/* set program counter to next instruction */
	pc = pc ? pc->next : NULL;
	set_pc(vm, pc);
}

void
interp_instr_binop(VM *vm, MirInstrBinop *binop)
{
#define _binop_int(_op, _lhs, _rhs, _result, _v_T)                                                 \
	case BINOP_ADD:                                                                            \
		(_result)._v_T = _lhs._v_T + _rhs._v_T;                                            \
		break;                                                                             \
	case BINOP_SUB:                                                                            \
		(_result)._v_T = _lhs._v_T - _rhs._v_T;                                            \
		break;                                                                             \
	case BINOP_MUL:                                                                            \
		(_result)._v_T = _lhs._v_T * _rhs._v_T;                                            \
		break;                                                                             \
	case BINOP_DIV:                                                                            \
		BL_ASSERT(_rhs._v_T != 0 && "divide by zero, this should be an error");            \
		(_result)._v_T = _lhs._v_T / _rhs._v_T;                                            \
		break;                                                                             \
	case BINOP_EQ:                                                                             \
		(_result).v_bool = _lhs._v_T == _rhs._v_T;                                         \
		break;                                                                             \
	case BINOP_NEQ:                                                                            \
		(_result).v_bool = _lhs._v_T != _rhs._v_T;                                         \
		break;                                                                             \
	case BINOP_LESS:                                                                           \
		(_result).v_bool = _lhs._v_T < _rhs._v_T;                                          \
		break;                                                                             \
	case BINOP_LESS_EQ:                                                                        \
		(_result).v_bool = _lhs._v_T == _rhs._v_T;                                         \
		break;                                                                             \
	case BINOP_GREATER:                                                                        \
		(_result).v_bool = _lhs._v_T > _rhs._v_T;                                          \
		break;                                                                             \
	case BINOP_GREATER_EQ:                                                                     \
		(_result).v_bool = _lhs._v_T >= _rhs._v_T;                                         \
		break;

#define binop_case_int(_op, _lhs, _rhs, _result, _v_T)                                             \
	case sizeof(_lhs._v_T): {                                                                  \
		switch (_op) {                                                                     \
			_binop_int(_op, _lhs, _rhs, _result, _v_T);                                \
		case BINOP_SHR:                                                                    \
			(_result)._v_T = _lhs._v_T >> _rhs._v_T;                                   \
			break;                                                                     \
		case BINOP_SHL:                                                                    \
			(_result)._v_T = _lhs._v_T << _rhs._v_T;                                   \
			break;                                                                     \
		case BINOP_MOD:                                                                    \
			(_result)._v_T = _lhs._v_T % _rhs._v_T;                                    \
			break;                                                                     \
		case BINOP_AND:                                                                    \
			(_result)._v_T = _lhs._v_T & _rhs._v_T;                                    \
			break;                                                                     \
		case BINOP_OR:                                                                     \
			(_result)._v_T = _lhs._v_T | _rhs._v_T;                                    \
			break;                                                                     \
		default:                                                                           \
			BL_UNIMPLEMENTED;                                                          \
		}                                                                                  \
	} break;

#define binop_case_real(_op, _lhs, _rhs, _result, _v_T)                                            \
	case sizeof(_lhs._v_T): {                                                                  \
		switch (_op) {                                                                     \
			_binop_int(_op, _lhs, _rhs, _result, _v_T) default : BL_UNIMPLEMENTED;     \
		}                                                                                  \
	} break;
	// clang-format on

	/* binop expects lhs and rhs on stack in exact order and push result again
	 * to the stack */
	MirType *type = binop->lhs->value.type;
	BL_ASSERT(type);

	VMStackPtr lhs_ptr = fetch_value(vm, binop->lhs);
	VMStackPtr rhs_ptr = fetch_value(vm, binop->rhs);
	BL_ASSERT(rhs_ptr && lhs_ptr);

	MirConstValueData result = {0};
	MirConstValueData lhs    = {0};
	MirConstValueData rhs    = {0};

	read_value(&lhs, lhs_ptr, type);
	read_value(&rhs, rhs_ptr, type);

	const size_t s = type->store_size_bytes;

	switch (type->kind) {
	case MIR_TYPE_ENUM:
	case MIR_TYPE_PTR:
	case MIR_TYPE_NULL:
	case MIR_TYPE_BOOL:
	case MIR_TYPE_INT: {
		if (type->data.integer.is_signed) {
			switch (s) {
				binop_case_int(binop->op, lhs, rhs, result, v_s8);
				binop_case_int(binop->op, lhs, rhs, result, v_s16);
				binop_case_int(binop->op, lhs, rhs, result, v_s32);
				binop_case_int(binop->op, lhs, rhs, result, v_s64);
			default:
				BL_ABORT("invalid integer data type");
			}
		} else {
			switch (s) {
				binop_case_int(binop->op, lhs, rhs, result, v_u8);
				binop_case_int(binop->op, lhs, rhs, result, v_u16);
				binop_case_int(binop->op, lhs, rhs, result, v_u32);
				binop_case_int(binop->op, lhs, rhs, result, v_u64);
			default:
				BL_ABORT("invalid integer data type");
			}
		}
		break;
	}

	case MIR_TYPE_REAL: {
		switch (s) {
			binop_case_real(binop->op, lhs, rhs, result, v_f32);
			binop_case_real(binop->op, lhs, rhs, result, v_f64);
		default:
			BL_ABORT("invalid real data type");
		}
		break;
	}

	default:
		BL_ABORT("invalid binop type");
	}

	if (binop->base.comptime)
		memcpy(&binop->base.value.data, &result, sizeof(result));
	else
		push_stack(vm, &result, binop->base.value.type);
#undef binop_case_int
#undef binop_case_real
#undef _binop_int
}

void
interp_instr_unop(VM *vm, MirInstrUnop *unop)
{
#define unop_case(_op, _value, _result, _v_T)                                                      \
	case sizeof(_value._v_T): {                                                                \
		switch (_op) {                                                                     \
		case UNOP_NOT:                                                                     \
			(_result)._v_T = !_value._v_T;                                             \
			break;                                                                     \
		case UNOP_NEG:                                                                     \
			(_result)._v_T = _value._v_T * -1;                                         \
			break;                                                                     \
		case UNOP_POS:                                                                     \
			(_result)._v_T = _value._v_T;                                              \
			break;                                                                     \
		default:                                                                           \
			BL_UNIMPLEMENTED;                                                          \
		}                                                                                  \
	} break;

	BL_ASSERT(unop->base.value.type);
	MirType *  value_type = unop->expr->value.type;
	VMStackPtr value_ptr  = fetch_value(vm, unop->expr);
	BL_ASSERT(value_ptr);

	MirType *type = unop->expr->value.type;
	BL_ASSERT(type);

	MirConstValueData result = {0};
	MirConstValueData value  = {0};
	read_value(&value, value_ptr, type);

	switch (type->kind) {
	case MIR_TYPE_BOOL:
	case MIR_TYPE_INT: {
		const size_t s = type->store_size_bytes;
		if (type->data.integer.is_signed) {
			switch (s) {
				unop_case(unop->op, value, result, v_s8);
				unop_case(unop->op, value, result, v_s16);
				unop_case(unop->op, value, result, v_s32);
				unop_case(unop->op, value, result, v_s64);
			default:
				BL_ABORT("invalid integer data type");
			}
		} else {
			switch (s) {
				unop_case(unop->op, value, result, v_u8);
				unop_case(unop->op, value, result, v_u16);
				unop_case(unop->op, value, result, v_u32);
				unop_case(unop->op, value, result, v_u64);
			default:
				BL_ABORT("invalid integer data type");
			}
		}
		break;
	}

	case MIR_TYPE_REAL: {
		const size_t s = type->store_size_bytes;

		switch (s) {
			unop_case(unop->op, value, result, v_f32);
			unop_case(unop->op, value, result, v_f64);
		default:
			BL_ABORT("invalid real data type");
		}
		break;
	}

	default:
		BL_ABORT("invalid unop type");
	}

	if (unop->expr->comptime) {
		BL_ASSERT(unop->base.comptime);
		memcpy(&unop->base.value.data, &result, sizeof(result));
	} else {
		push_stack(vm, &result, value_type);
	}
#undef unop
}

/* public */
void
vm_init(VM *vm, Assembly *assembly, Builder *builder, size_t stack_size)
{
	if (stack_size == 0) BL_ABORT("invalid frame stack size");

	VMStack *stack = bl_malloc(sizeof(char) * stack_size);
	if (!stack) BL_ABORT("bad alloc");
#if BL_DEBUG
	memset(stack, 0, stack_size);
#endif

	stack->allocated_bytes = stack_size;
	reset_stack(stack);

	vm->stack    = stack;
	vm->assembly = assembly;
	vm->builder  = builder;

	sa_init(&vm->dyncall_sig_tmp);
}

void
vm_terminate(VM *vm)
{
	sa_terminate(&vm->dyncall_sig_tmp);
	bl_free(vm->stack);
}

void
vm_execute_instr(VM *vm, MirInstr *instr)
{
	interp_instr(vm, instr);
}

bool
vm_execute_fn(VM *vm, MirFn *fn, VMStackPtr *out_ptr)
{
	vm->stack->aborted = false;
	return execute_fn_impl_top_level(vm, fn, NULL, out_ptr);
}

bool
vm_execute_instr_top_level_call(VM *vm, MirInstrCall *call)
{
	BL_ASSERT(call && call->base.analyzed);

	assert(call->base.comptime && "Top level call is expected to be comptime.");
	if (call->args) BL_ABORT("exec call top level has not implemented passing of arguments");

	return execute_fn_top_level(vm, &call->base, NULL);
}

VMStackPtr
vm_create_global(VM *vm, struct MirInstrDeclVar *decl)
{
	MirVar *var = decl->var;
	BL_ASSERT(var);
	BL_ASSERT(var->is_in_gscope && "Allocated variable is supposed to be global variable.");

	VMRelativeStackPtr var_ptr = stack_alloc_var(vm, var);
	interp_instr_decl_var(vm, decl);

	/* HACK: we can ignore relative pointers for globals. */
	return (VMStackPtr)var_ptr;
}

VMStackPtr
vm_create_implicit_global(VM *vm, struct MirVar *var)
{
	BL_ASSERT(var);
	BL_ASSERT(var->is_in_gscope && "Allocated variable is supposed to be global variable.");

	/* HACK: we can ignore relative pointers for globals. */
	VMStackPtr var_ptr = (VMStackPtr)stack_alloc_var(vm, var);
	copy_comptime_to_stack(vm, var_ptr, &var->value);
	return var_ptr;
}

void
vm_read_stack_value(MirConstValue *dest, VMStackPtr src)
{
	assert(dest->type);
	memset(&dest->data, 0, sizeof(dest->data));
	read_value(&dest->data, src, dest->type);
}

typedef void (*Fn)(int, bool, bool);

void
test_call(Fn callback)
{
	callback(10, true, false);
}
