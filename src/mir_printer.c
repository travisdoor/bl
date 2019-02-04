//************************************************************************************************
// bl
//
// File:   mir_printer.c
// Author: Martin Dorazil
// Date:   3/15/18
//
// Copyright 2018 Martin Dorazil
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

#include "mir_printer.h"
#include "ast.h"

static inline void
print_type(MirType *type, bool aligned, FILE *stream)
{
  char tmp[256];
  mir_type_to_str(tmp, ARRAY_SIZE(tmp), type);
  if (aligned)
    fprintf(stream, "%16s", tmp);
  else
    fprintf(stream, "%s", tmp);
}

static inline void
print_instr_head(MirInstr *instr, FILE *stream, const char *name)
{
  if (!instr) return;

#if BL_DEBUG
  if (instr->ref_count == -1) {
    fprintf(stream, "  %%%-3u ~%-5llu (-)", instr->id, (unsigned long long)instr->_serial);
  } else {
    fprintf(stream, "  %%%-3u ~%-5llu (%d)", instr->id, (unsigned long long)instr->_serial,
            instr->ref_count);
  }
#else
  fprintf(stream, "  %%%-3u", instr->id);
#endif
  print_type(instr->const_value.type, true, stream);
  fprintf(stream, " %s ", name);
}

static inline void
print_const_value(MirInstr *instr, FILE *stream)
{
  MirConstValue *value = &instr->const_value;
  MirType *      type  = value->type;
  assert(type);

  switch (type->kind) {
  case MIR_TYPE_INT:
    fprintf(stream, "%lld", (long long)value->data.v_int);
    break;
  case MIR_TYPE_REAL:
    if (value->type->store_size_bytes == sizeof(float)) {
      fprintf(stream, "%f", value->data.v_float);
    } else {
      fprintf(stream, "%f", value->data.v_double);
    }
    break;
  case MIR_TYPE_BOOL:
    fprintf(stream, "%s", value->data.v_bool ? "true" : "false");
    break;
  case MIR_TYPE_TYPE:
    print_type(value->data.v_type, false, stream);
    break;
  case MIR_TYPE_PTR:
    fprintf(stream, "%p", value->data.v_void_ptr);
    break;
  case MIR_TYPE_NULL:
    break;
  case MIR_TYPE_ARRAY:
    if (value->kind == MIR_CV_STRING) {
      char *tmp = strdup(value->data.v_str);
      fprintf(stream, "\"%s", strtok(tmp, "\n"));
      char *next = strtok(NULL, "\n");
      if (next && strlen(next)) fprintf(stdout, "...");
      fprintf(stream, "\"");
      free(tmp);
    } else {
      fprintf(stream, "cannot read value");
    }
    break;
  default:
    fprintf(stream, "cannot read value");
  }
}

static inline void
print_comptime_value_or_id(MirInstr *instr, FILE *stream)
{
  if (!instr->comptime || !instr->analyzed) {
    fprintf(stream, "%%%u", instr->id);
    return;
  }

  print_const_value(instr, stream);
}

static void
print_instr_cast(MirInstrCast *cast, FILE *stream);

static void
print_instr_load(MirInstrLoad *load, FILE *stream);

static void
print_instr_addrof(MirInstrAddrOf *addrof, FILE *stream);

static void
print_instr_elem_ptr(MirInstrElemPtr *elem_ptr, FILE *stream);

static void
print_instr_member_ptr(MirInstrMemberPtr *member_ptr, FILE *stream);

static void
print_instr_cond_br(MirInstrCondBr *cond_br, FILE *stream);

static void
print_instr_br(MirInstrBr *br, FILE *stream);

static void
print_instr_unreachable(MirInstrUnreachable *unr, FILE *stream);

static void
print_instr_try_infer(MirInstrTryInfer *infer, FILE *stream);

static void
print_instr_fn_proto(MirInstrFnProto *fn_proto, FILE *stream);

static void
print_instr_type_fn(MirInstrTypeFn *type_fn, FILE *stream);

static void
print_instr_type_ptr(MirInstrTypePtr *type_ptr, FILE *stream);

static void
print_instr_type_array(MirInstrTypeArray *type_array, FILE *stream);

static void
print_instr_type_slice(MirInstrTypeSlice *type_slice, FILE *stream);

static void
print_instr_block(MirInstrBlock *block, FILE *stream);

static void
print_instr_decl_var(MirInstrDeclVar *decl, FILE *stream);

static void
print_instr_const(MirInstrConst *ci, FILE *stream);

static void
print_instr_ret(MirInstrRet *ret, FILE *stream);

static void
print_instr_store(MirInstrStore *store, FILE *stream);

static void
print_instr_binop(MirInstrBinop *binop, FILE *stream);

static void
print_instr_validate_type(MirInstrValidateType *vt, FILE *stream);

static void
print_instr_call(MirInstrCall *call, FILE *stream);

static void
print_instr_decl_ref(MirInstrDeclRef *ref, FILE *stream);

static void
print_instr_unop(MirInstrUnop *unop, FILE *stream);

static void
print_instr_arg(MirInstrArg *arg, FILE *stream);

/* impl */
void
print_instr_type_fn(MirInstrTypeFn *type_fn, FILE *stream)
{
  print_instr_head(&type_fn->base, stream, "fn");
  fprintf(stream, "(");
  if (type_fn->arg_types) {
    MirInstr *tmp;
    barray_foreach(type_fn->arg_types, tmp)
    {
      fprintf(stream, "%%%u", tmp->id);
      if (i + 1 < bo_array_size(type_fn->arg_types)) fprintf(stream, ", ");
    }
  }

  fprintf(stream, ")");

  if (type_fn->ret_type) fprintf(stream, " %%%u", type_fn->ret_type->id);
}

void
print_instr_type_ptr(MirInstrTypePtr *type_ptr, FILE *stream)
{
  print_instr_head(&type_ptr->base, stream, "");
  fprintf(stream, "*%%%u", type_ptr->type->id);
}

void
print_instr_type_array(MirInstrTypeArray *type_array, FILE *stream)
{
  print_instr_head(&type_array->base, stream, "");
  fprintf(stream, "[%%%u]%%%u", type_array->len->id, type_array->elem_type->id);
}

void
print_instr_type_slice(MirInstrTypeSlice *type_slice, FILE *stream)
{
  print_instr_head(&type_slice->base, stream, "");
  fprintf(stream, "[]%%%u", type_slice->elem_type->id);
}

void
print_instr_cast(MirInstrCast *cast, FILE *stream)
{
  switch (cast->op) {
  case MIR_CAST_BITCAST:
    print_instr_head(&cast->base, stream, "bitcast");
    break;
  case MIR_CAST_SEXT:
    print_instr_head(&cast->base, stream, "sext");
    break;
  case MIR_CAST_ZEXT:
    print_instr_head(&cast->base, stream, "zext");
    break;
  case MIR_CAST_TRUNC:
    print_instr_head(&cast->base, stream, "trunc");
    break;
  case MIR_CAST_FPTOSI:
    print_instr_head(&cast->base, stream, "fptosi");
    break;
  case MIR_CAST_FPTOUI:
    print_instr_head(&cast->base, stream, "fptoui");
    break;
  case MIR_CAST_INVALID:
    fprintf(stream, "invalid cast %%%u", cast->next->id);
    break;
  case MIR_CAST_PTRTOINT:
    print_instr_head(&cast->base, stream, "ptrtoint");
    break;
  case MIR_CAST_INTTOPTR:
    print_instr_head(&cast->base, stream, "inttoptr");
    break;
  default:
    bl_unimplemented;
  }

  fprintf(stream, "%%%u", cast->next->id);
}

void
print_instr_elem_ptr(MirInstrElemPtr *elem_ptr, FILE *stream)
{
  print_instr_head(&elem_ptr->base, stream, "elemptr");
  fprintf(stream, "%%%u[%%%u]", elem_ptr->arr_ptr->id, elem_ptr->index->id);
}

void
print_instr_member_ptr(MirInstrMemberPtr *member_ptr, FILE *stream)
{
  print_instr_head(&member_ptr->base, stream, "memberptr");
  if (member_ptr->member_ident) {
    fprintf(stream, "%%%u.%s", member_ptr->target_ptr->id,
            member_ptr->member_ident->data.ident.id.str);
  } else {
    fprintf(stream, "%%%u.%u", member_ptr->target_ptr->id, member_ptr->order);
  }
}

void
print_instr_unop(MirInstrUnop *unop, FILE *stream)
{
  print_instr_head(&unop->base, stream, "unop");

  const char *op = ast_unop_to_str(unop->op);
  fprintf(stream, "%s", op);
  print_comptime_value_or_id(unop->instr, stream);
}

void
print_instr_cond_br(MirInstrCondBr *cond_br, FILE *stream)
{
  print_instr_head(&cond_br->base, stream, "br");
  fprintf(stream, "%%%u ? %s_%u : %s_%u", cond_br->cond->id, cond_br->then_block->name,
          cond_br->then_block->base.id, cond_br->else_block->name, cond_br->else_block->base.id);
}

void
print_instr_arg(MirInstrArg *arg, FILE *stream)
{
  print_instr_head(&arg->base, stream, "arg");
  fprintf(stream, "$%u", arg->i);
}

void
print_instr_unreachable(MirInstrUnreachable *unr, FILE *stream)
{
  print_instr_head(&unr->base, stream, "unreachable");
}

void
print_instr_br(MirInstrBr *br, FILE *stream)
{
  print_instr_head(&br->base, stream, "br");
  fprintf(stream, "%s_%d", br->then_block->name, br->then_block->base.id);
}

void
print_instr_try_infer(MirInstrTryInfer *infer, FILE *stream)
{
  print_instr_head(&infer->base, stream, "tryinfer");
  fprintf(stream, "%%%u -> %%%u", infer->src->id, infer->dest->id);
}

void
print_instr_load(MirInstrLoad *load, FILE *stream)
{
  print_instr_head(&load->base, stream, "load");
  print_comptime_value_or_id(load->src, stream);
}

void
print_instr_addrof(MirInstrAddrOf *addrof, FILE *stream)
{
  print_instr_head(&addrof->base, stream, "addrof");
  fprintf(stream, "%%%u", addrof->src->id);
}

void
print_instr_decl_var(MirInstrDeclVar *decl, FILE *stream)
{
  print_instr_head(&decl->base, stream, "decl");

  MirVar *var = decl->var;
  assert(var);
  fprintf(stream, "%s : ", var->id->str);
  print_type(var->alloc_type, false, stream);
  if (decl->init) {
    fprintf(stream, " %s ", var->is_mutable ? "=" : ":");
    print_comptime_value_or_id(decl->init, stream);
  }
}

void
print_instr_decl_ref(MirInstrDeclRef *ref, FILE *stream)
{
  print_instr_head(&ref->base, stream, "declref");
  const char *name = ref->rid->str;

  fprintf(stream, "%s", name);
}

void
print_instr_const(MirInstrConst *cnst, FILE *stream)
{
  print_instr_head(&cnst->base, stream, "const");
  print_const_value(&cnst->base, stream);
}

void
print_instr_call(MirInstrCall *call, FILE *stream)
{
  print_instr_head(&call->base, stream, "call");

  const char *callee_name =
      call->callee->const_value.data.v_fn ? call->callee->const_value.data.v_fn->llvm_name : NULL;
  if (callee_name)
    fprintf(stream, "@%s", callee_name);
  else
    fprintf(stream, "%%%u", call->callee->id);

  fprintf(stream, "(");
  if (call->args) {
    MirInstr *tmp;
    barray_foreach(call->args, tmp)
    {
      fprintf(stream, "%%%u", tmp->id);
      if (i < bo_array_size(call->args) - 1) fprintf(stream, ", ");
    }
  }
  fprintf(stream, ")");
}

void
print_instr_validate_type(MirInstrValidateType *vt, FILE *stream)
{
  print_instr_head(&vt->base, stream, "validate_type");
  fprintf(stream, "%%%u", vt->src->id);
}

void
print_instr_ret(MirInstrRet *ret, FILE *stream)
{
  print_instr_head(&ret->base, stream, "ret");
  if (ret->value) print_comptime_value_or_id(ret->value, stream);
}

void
print_instr_store(MirInstrStore *store, FILE *stream)
{
  print_instr_head(&store->base, stream, "store");
  assert(store->src && store->src);
  print_comptime_value_or_id(store->src, stream);
  fprintf(stream, " -> %%%u", store->dest->id);
  // print_comptime_value_or_id(store->dest, stream);
}

void
print_instr_binop(MirInstrBinop *binop, FILE *stream)
{
  print_instr_head(&binop->base, stream, "binop");
  assert(binop->lhs && binop->rhs);
  const char *op = ast_binop_to_str(binop->op);
  print_comptime_value_or_id(binop->lhs, stream);
  fprintf(stream, " %s ", op);
  print_comptime_value_or_id(binop->rhs, stream);
}

void
print_instr_block(MirInstrBlock *block, FILE *stream)
{
  if (block->base.prev) fprintf(stream, "\n");
#if BL_DEBUG
  fprintf(stream, "%s_%u (%u):", block->name, block->base.id, block->base.ref_count);
#else
  fprintf(stream, "%s_%u:", block->name, block->base.id);
#endif
  if (!block->base.ref_count)
    fprintf(stream, " // NEVER REACHED\n");
  else
    fprintf(stream, "\n");

  MirInstr *tmp = block->entry_instr;

  while (tmp) {
    mir_print_instr(tmp, stream);
    tmp = tmp->next;
  }
}

void
print_instr_fn_proto(MirInstrFnProto *fn_proto, FILE *stream)
{
  MirFn *fn = fn_proto->base.const_value.data.v_fn;
  assert(fn);

  fprintf(stream, "%s%s\n", fn_proto->base.analyzed ? "// analyzed" : "",
          fn->ref_count ? "" : ", no LLVM representation");

  if (fn->llvm_name)
    fprintf(stream, "@%s ", fn->llvm_name);
  else
    fprintf(stream, "@%u ", fn_proto->base.id);

#if BL_DEBUG
  fprintf(stream, "(%d) ", fn->ref_count);
#endif
  print_type(fn_proto->base.const_value.type, false, stream);

  if (!fn->is_external) {
    if (fn->is_test_case) fprintf(stream, " #test");
    fprintf(stream, " {\n");

    MirInstrBlock *tmp = fn->first_block;
    while (tmp) {
      print_instr_block(tmp, stream);
      tmp = (MirInstrBlock *)tmp->base.next;
    }
    fprintf(stream, "}\n");
  } else {
    fprintf(stream, " #extern\n");
  }
}

/* public */
void
mir_print_instr(MirInstr *instr, FILE *stream)
{
  switch (instr->kind) {
  case MIR_INSTR_INVALID:
    fprintf(stream, RED("INVALID"));
    break;
  case MIR_INSTR_UNREACHABLE:
    print_instr_unreachable((MirInstrUnreachable *)instr, stream);
    break;
  case MIR_INSTR_DECL_VAR:
    print_instr_decl_var((MirInstrDeclVar *)instr, stream);
    break;
  case MIR_INSTR_CONST:
    print_instr_const((MirInstrConst *)instr, stream);
    break;
  case MIR_INSTR_LOAD:
    print_instr_load((MirInstrLoad *)instr, stream);
    break;
  case MIR_INSTR_STORE:
    print_instr_store((MirInstrStore *)instr, stream);
    break;
  case MIR_INSTR_RET:
    print_instr_ret((MirInstrRet *)instr, stream);
    break;
  case MIR_INSTR_BINOP:
    print_instr_binop((MirInstrBinop *)instr, stream);
    break;
  case MIR_INSTR_VALIDATE_TYPE:
    print_instr_validate_type((MirInstrValidateType *)instr, stream);
    break;
  case MIR_INSTR_CALL:
    print_instr_call((MirInstrCall *)instr, stream);
    break;
  case MIR_INSTR_FN_PROTO:
    print_instr_fn_proto((MirInstrFnProto *)instr, stream);
    break;
  case MIR_INSTR_DECL_REF:
    print_instr_decl_ref((MirInstrDeclRef *)instr, stream);
    break;
  case MIR_INSTR_TYPE_FN:
    print_instr_type_fn((MirInstrTypeFn *)instr, stream);
    break;
  case MIR_INSTR_TYPE_ARRAY:
    print_instr_type_array((MirInstrTypeArray *)instr, stream);
    break;
  case MIR_INSTR_TYPE_SLICE:
    print_instr_type_slice((MirInstrTypeSlice *)instr, stream);
    break;
  case MIR_INSTR_TRY_INFER:
    print_instr_try_infer((MirInstrTryInfer *)instr, stream);
    break;
  case MIR_INSTR_COND_BR:
    print_instr_cond_br((MirInstrCondBr *)instr, stream);
    break;
  case MIR_INSTR_BR:
    print_instr_br((MirInstrBr *)instr, stream);
    break;
  case MIR_INSTR_UNOP:
    print_instr_unop((MirInstrUnop *)instr, stream);
    break;
  case MIR_INSTR_ARG:
    print_instr_arg((MirInstrArg *)instr, stream);
    break;
  case MIR_INSTR_ELEM_PTR:
    print_instr_elem_ptr((MirInstrElemPtr *)instr, stream);
    break;
  case MIR_INSTR_TYPE_PTR:
    print_instr_type_ptr((MirInstrTypePtr *)instr, stream);
    break;
  case MIR_INSTR_ADDROF:
    print_instr_addrof((MirInstrAddrOf *)instr, stream);
    break;
  case MIR_INSTR_MEMBER_PTR:
    print_instr_member_ptr((MirInstrMemberPtr *)instr, stream);
    break;
  case MIR_INSTR_CAST:
    print_instr_cast((MirInstrCast *)instr, stream);
    break;
  default:
    break;
  }

  fprintf(stream, "%s\n", instr->comptime ? " // comptime" : "");
}

void
mir_print_module(MirModule *module, FILE *stream)
{
  assert(module);
  MirInstr *instr;
  barray_foreach(module->globals, instr) mir_print_instr(instr, stream);
}
