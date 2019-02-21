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
print_type(MirType *type, bool aligned, FILE *stream, bool prefer_name)
{
  char tmp[256];
  mir_type_to_str(tmp, ARRAY_SIZE(tmp), type, prefer_name);
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
  print_type(instr->const_value.type, true, stream, true);
  fprintf(stream, " %s ", name);
}

static inline void
print_const_value(MirConstValue *value, FILE *stream)
{
  MirType *          type = value->type;
  MirConstValueData *data = &value->data;
  assert(type);

#define print_case(format, T)                                                                      \
  case sizeof(data->T):                                                                            \
    fprintf(stream, format, data->T);                                                              \
    break;

  assert(type);

  switch (type->kind) {
  case MIR_TYPE_INT: {
    const size_t s = type->store_size_bytes;
    if (type->data.integer.is_signed) {
      switch (s) {
        print_case("%d", v_s8);
        print_case("%d", v_s16);
        print_case("%d", v_s32);
        print_case("%ld", v_s64);
      default:
        fprintf(stream, "<cannot read value>");
        break;
      }
    } else {
      switch (s) {
        print_case("%u", v_u8);
        print_case("%u", v_u16);
        print_case("%u", v_u32);
        print_case("%lu", v_u64);
      default:
        fprintf(stream, "<cannot read value>");
        break;
      }
    }

    break;
  }
  case MIR_TYPE_REAL:
    if (type->store_size_bytes == sizeof(float)) {
      fprintf(stream, "%f", data->v_f32);
    } else {
      fprintf(stream, "%f", data->v_f64);
    }
    break;
  case MIR_TYPE_BOOL:
    fprintf(stream, "%s", data->v_bool ? "true" : "false");
    break;
  case MIR_TYPE_TYPE:
    print_type(data->v_type, false, stream, false);
    break;
  case MIR_TYPE_PTR: {
    MirType *deref_type = mir_deref_type(type);
    /* pointers to u8 is printed like strings */
    if (deref_type->kind == MIR_TYPE_INT && deref_type->data.integer.bitcount == 8 &&
        deref_type->data.integer.is_signed == false) {
      if (data->v_str == NULL) {
        fprintf(stream, "<null>");
        break;
      }

      char *tmp = strdup(data->v_str);
      if (strtok(tmp, "\n")) {
        fprintf(stream, "\"%s", strtok(tmp, "\n"));
      } else {
        fprintf(stream, "\"\"");
        break;
      }
      char *next = strtok(NULL, "\n");
      if (next && strlen(next)) fprintf(stdout, "...");
      fprintf(stream, "\"");
      free(tmp);
    } else {
      fprintf(stream, "%p", data->v_void_ptr);
    }
    break;
  }
  case MIR_TYPE_NULL:
    fprintf(stream, "null");
    break;
  case MIR_TYPE_STRUCT: {
    BArray *   members             = data->v_struct.members;
    const bool is_zero_initializer = data->v_struct.is_zero_initializer;

    if (is_zero_initializer) {
      fprintf(stream, "{0}");
    } else if (!members) {
      fprintf(stream, "{<null>}");
    } else {
      fprintf(stream, "{");

      MirConstValue *member;
      const size_t   memc = bo_array_size(members);

      for (size_t i = 0; i < memc; ++i) {
        member = bo_array_at(members, i, MirConstValue *);
        print_const_value(member, stream);
        if (i + 1 < memc) fprintf(stream, ", ");
      }

      fprintf(stream, "}");
    }
    break;
  }
  case MIR_TYPE_ARRAY: {
    BArray *   elems               = data->v_array.elems;
    const bool is_zero_initializer = data->v_array.is_zero_initializer;

    if (is_zero_initializer) {
      fprintf(stream, "{0}");
    } else {
      fprintf(stream, "{");

      if (elems) {
        MirConstValue *elem;
        const size_t   elc = bo_array_size(elems);

        for (size_t i = 0; i < elc; ++i) {
          elem = bo_array_at(elems, i, MirConstValue *);
          print_const_value(elem, stream);
          if (i + 1 < elc) fprintf(stream, ", ");
        }
      } else {
        fprintf(stream, "<cannot read value>");
      }

      fprintf(stream, "}");
    }
    break;
  }
  default:
    fprintf(stream, "<cannot read value>");
  }
}

static inline void
print_comptime_value_or_id(MirInstr *instr, FILE *stream)
{
  if (!instr->comptime || !instr->analyzed) {
    fprintf(stream, "%%%u", instr->id);
    return;
  }

  print_const_value(&instr->const_value, stream);
}

static void
print_instr_cast(MirInstrCast *cast, FILE *stream);

static void
print_instr_sizeof(MirInstrSizeof *szof, FILE *stream);

static void
print_instr_alignof(MirInstrAlignof *szof, FILE *stream);

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
print_instr_init(MirInstrInit *init, FILE *stream);

static void
print_instr_br(MirInstrBr *br, FILE *stream);

static void
print_instr_unreachable(MirInstrUnreachable *unr, FILE *stream);

static void
print_instr_fn_proto(MirInstrFnProto *fn_proto, FILE *stream);

static void
print_instr_type_fn(MirInstrTypeFn *type_fn, FILE *stream);

static void
print_instr_type_struct(MirInstrTypeStruct *type_struct, FILE *stream);

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
print_instr_decl_member(MirInstrDeclMember *decl, FILE *stream);

static void
print_instr_const(MirInstrConst *ci, FILE *stream);

static void
print_instr_ret(MirInstrRet *ret, FILE *stream);

static void
print_instr_store(MirInstrStore *store, FILE *stream);

static void
print_instr_binop(MirInstrBinop *binop, FILE *stream);

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
  print_instr_head(&type_fn->base, stream, "const fn");
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
print_instr_type_struct(MirInstrTypeStruct *type_struct, FILE *stream)
{
  print_instr_head(&type_struct->base, stream, "const struct");
  fprintf(stream, "{");

  BArray *  members = type_struct->members;
  MirInstr *member;
  barray_foreach(members, member)
  {
    print_comptime_value_or_id(member, stream);
    if (i + 1 < bo_array_size(members)) fprintf(stream, ", ");
  }

  fprintf(stream, "}");
}

void
print_instr_type_ptr(MirInstrTypePtr *type_ptr, FILE *stream)
{
  print_instr_head(&type_ptr->base, stream, "const");
  fprintf(stream, "*%%%u", type_ptr->type->id);
}

void
print_instr_type_array(MirInstrTypeArray *type_array, FILE *stream)
{
  print_instr_head(&type_array->base, stream, "const");
  fprintf(stream, "[%%%u]%%%u", type_array->len->id, type_array->elem_type->id);
}

void
print_instr_type_slice(MirInstrTypeSlice *type_slice, FILE *stream)
{
  print_instr_head(&type_slice->base, stream, "const");
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
  case MIR_CAST_PTRTOINT:
    print_instr_head(&cast->base, stream, "ptrtoint");
    break;
  case MIR_CAST_INTTOPTR:
    print_instr_head(&cast->base, stream, "inttoptr");
    break;
  case MIR_CAST_INVALID:
    print_instr_head(&cast->base, stream, "<invalid cast>");
    break;
  default:
    bl_unimplemented;
  }

  fprintf(stream, "%%%u", cast->next->id);
}

void
print_instr_init(MirInstrInit *init, FILE *stream)
{
  print_instr_head(&init->base, stream, "init");
  print_comptime_value_or_id(init->type, stream);

  fprintf(stream, " {");
  BArray *values = init->values;
  if (values) {
    MirInstr *value;
    barray_foreach(values, value)
    {
      print_comptime_value_or_id(value, stream);
      if (i < bo_array_size(values) - 1) fprintf(stream, ", ");
    }
  } else {
    fprintf(stream, "<invalid values>");
  }
  fprintf(stream, "}");
}

void
print_instr_sizeof(MirInstrSizeof *szof, FILE *stream)
{
  print_instr_head(&szof->base, stream, "sizeof");
  fprintf(stream, " ");
  print_comptime_value_or_id(szof->expr, stream);
}

void
print_instr_alignof(MirInstrAlignof *szof, FILE *stream)
{
  print_instr_head(&szof->base, stream, "alignof");
  fprintf(stream, " ");
  print_comptime_value_or_id(szof->expr, stream);
}

void
print_instr_elem_ptr(MirInstrElemPtr *elem_ptr, FILE *stream)
{
  print_instr_head(&elem_ptr->base, stream, "elemptr");
  fprintf(stream, "%%%u[", elem_ptr->arr_ptr->id);
  print_comptime_value_or_id(elem_ptr->index, stream);
  fprintf(stream, "]");
}

void
print_instr_member_ptr(MirInstrMemberPtr *member_ptr, FILE *stream)
{
  print_instr_head(&member_ptr->base, stream, "memberptr");
  if (!member_ptr->target_ptr) {
    fprintf(stream, "<unknown>.");
  }

  if (member_ptr->builtin_id == MIR_BUILTIN_NONE) {
    if (member_ptr->member_ident) {
      fprintf(stream, "%s", member_ptr->member_ident->data.ident.id.str);
    } else {
      fprintf(stream, "<unknown>");
    }
  } else {
    switch (member_ptr->builtin_id) {
    case MIR_BUILTIN_ARR_LEN:
      fprintf(stream, "len");
      break;
    case MIR_BUILTIN_ARR_PTR:
      fprintf(stream, "ptr");
      break;

    default:
      fprintf(stream, "<unknown>");
    }
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
  print_comptime_value_or_id(cond_br->cond, stream);
  fprintf(stream, " ? %%%s_%u : %%%s_%u", cond_br->then_block->name, cond_br->then_block->base.id,
          cond_br->else_block->name, cond_br->else_block->base.id);
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
  fprintf(stream, "%%%s_%d", br->then_block->name, br->then_block->base.id);
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
  MirVar *var = decl->var;
  assert(var);

  if (var->is_in_gscope) {
    /* global scope variable */
    fprintf(stream, "@%s : ", var->id->str);
    print_type(var->alloc_type, false, stream, true);
    fprintf(stream, " %s ", var->is_mutable ? "=" : ":");
    if (decl->init) {
      print_comptime_value_or_id(decl->init, stream);
    } else {
      fprintf(stream, "<uninitialized>");
    }
  } else {
    /* local scope variable */
    print_instr_head(&decl->base, stream, "decl");

    fprintf(stream, "%s : ", var->id->str);
    print_type(var->alloc_type, false, stream, true);
    if (decl->init) {
      fprintf(stream, " %s ", var->is_mutable ? "=" : ":");
      print_comptime_value_or_id(decl->init, stream);
    }
  }
}

void
print_instr_decl_member(MirInstrDeclMember *decl, FILE *stream)
{
  print_instr_head(&decl->base, stream, "declmember");

  MirMember *member = decl->member;
  assert(member);

  fprintf(stream, "%s : ", member->id->str);
  print_comptime_value_or_id(decl->type, stream);
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
  print_const_value(&cnst->base.const_value, stream);
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
      print_comptime_value_or_id(tmp, stream);
      if (i < bo_array_size(call->args) - 1) fprintf(stream, ", ");
    }
  }
  fprintf(stream, ")");
}

void
print_instr_ret(MirInstrRet *ret, FILE *stream)
{
  print_instr_head(&ret->base, stream, "ret");
  if (ret->value) print_comptime_value_or_id(ret->value, stream);
  if (ret->allow_fn_ret_type_override) fprintf(stream, " // can override");
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
  fprintf(stream, "%%%s_%u (%u):", block->name, block->base.id, block->base.ref_count);
#else
  fprintf(stream, "%%%s_%u:", block->name, block->base.id);
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

  if (fn_proto->base.analyzed) fprintf(stream, "// analyzed\n");
  if (fn->ref_count) fprintf(stream, "// no LLVM\n");

  if (fn->llvm_name)
    fprintf(stream, "@%s ", fn->llvm_name);
  else
    fprintf(stream, "@%u ", fn_proto->base.id);

#if BL_DEBUG
  fprintf(stream, "(%d) : ", fn->ref_count);
#else
  fprintf(stream, " : ");
#endif
  print_type(fn_proto->base.const_value.type, false, stream, false);
  fprintf(stream, " :");

  if (fn->flags & FLAG_EXTERN) {
    fprintf(stream, " #extern\n");
  } else {
    if (fn->flags & FLAG_TEST) fprintf(stream, " #test");
    fprintf(stream, " {\n");

    MirInstrBlock *tmp = fn->first_block;
    while (tmp) {
      print_instr_block(tmp, stream);
      tmp = (MirInstrBlock *)tmp->base.next;
    }
    fprintf(stream, "}\n");
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
  case MIR_INSTR_DECL_MEMBER:
    print_instr_decl_member((MirInstrDeclMember *)instr, stream);
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
  case MIR_INSTR_TYPE_STRUCT:
    print_instr_type_struct((MirInstrTypeStruct *)instr, stream);
    break;
  case MIR_INSTR_TYPE_ARRAY:
    print_instr_type_array((MirInstrTypeArray *)instr, stream);
    break;
  case MIR_INSTR_TYPE_SLICE:
    print_instr_type_slice((MirInstrTypeSlice *)instr, stream);
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
  case MIR_INSTR_SIZEOF:
    print_instr_sizeof((MirInstrSizeof *)instr, stream);
    break;
  case MIR_INSTR_ALIGNOF:
    print_instr_alignof((MirInstrAlignof *)instr, stream);
    break;
  case MIR_INSTR_INIT:
    print_instr_init((MirInstrInit *)instr, stream);
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
