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
print_instr_head(MirInstr *instr, FILE *stream)
{
  if (!instr) return;
  fprintf(stream, "  %%%-3u (%d) ", instr->id, instr->ref_count);
  print_type(instr->value.type, true, stream);
  fprintf(stream, " ");
}

static void
print_instr_load(MirInstrLoad *load, FILE *stream);

static void
print_instr_addr_of(MirInstrAddrOf *addr_of, FILE *stream);

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
  print_instr_head(&type_fn->base, stream);
  fprintf(stream, "const fn (");

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
print_instr_unop(MirInstrUnop *unop, FILE *stream)
{
  print_instr_head(&unop->base, stream);

  const char *op = ast_unop_to_str(unop->op);
  fprintf(stream, "%s %%%u", op, unop->instr->id);
}

void
print_instr_cond_br(MirInstrCondBr *cond_br, FILE *stream)
{
  print_instr_head(&cond_br->base, stream);
  fprintf(stream, "br %%%u ? %s_%u : %s_%u", cond_br->cond->id, cond_br->then_block->name,
          cond_br->then_block->base.id, cond_br->else_block->name, cond_br->else_block->base.id);
}

void
print_instr_arg(MirInstrArg *arg, FILE *stream)
{
  print_instr_head(&arg->base, stream);
  fprintf(stream, "arg $%u", arg->i);
}

void
print_instr_unreachable(MirInstrUnreachable *unr, FILE *stream)
{
  print_instr_head(&unr->base, stream);
  fprintf(stream, "unreachable");
}

void
print_instr_br(MirInstrBr *br, FILE *stream)
{
  print_instr_head(&br->base, stream);
  fprintf(stream, "br %s_%d", br->then_block->name, br->then_block->base.id);
}

void
print_instr_try_infer(MirInstrTryInfer *infer, FILE *stream)
{
  print_instr_head(&infer->base, stream);
  fprintf(stream, "tryinfer %%%u -> %%%u", infer->src->id, infer->dest->id);
}

void
print_instr_load(MirInstrLoad *load, FILE *stream)
{
  print_instr_head(&load->base, stream);
  fprintf(stream, "load %%%u", load->src->id);
}

void
print_instr_addr_of(MirInstrAddrOf *addr_of, FILE *stream)
{
  print_instr_head(&addr_of->base, stream);
  fprintf(stream, "&%%%u", addr_of->target->id);
}

void
print_instr_decl_var(MirInstrDeclVar *decl, FILE *stream)
{
  print_instr_head(&decl->base, stream);

  MirVar *var = decl->var;
  assert(var);
  fprintf(stream, "decl %s ", var->name);
  print_type(var->value.type, false, stream);
}

void
print_instr_decl_ref(MirInstrDeclRef *ref, FILE *stream)
{
  print_instr_head(&ref->base, stream);
  const char *name = ref->base.node->data.ident.str;

  fprintf(stream, "declref %s", name);
}

void
print_instr_const(MirInstrConst *cnst, FILE *stream)
{
  print_instr_head(&cnst->base, stream);

  MirValue *value = &cnst->base.value;
  assert(value->type);

  fprintf(stream, "const ");
  switch (value->type->kind) {
  case MIR_TYPE_INT:
    fprintf(stream, "%llu", value->data.v_int);
    break;
  case MIR_TYPE_BOOL:
    fprintf(stream, "%s", value->data.v_bool ? "true" : "false");
    break;
  case MIR_TYPE_TYPE:
    print_type(value->data.v_type, false, stream);
    break;
  default:
    fprintf(stream, "cannot read value");
  }
}

void
print_instr_call(MirInstrCall *call, FILE *stream)
{
  print_instr_head(&call->base, stream);

  fprintf(stream, "call %%%u", call->callee->id);

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
  print_instr_head(&vt->base, stream);
  fprintf(stream, "validate_type %%%u", vt->src->id);
}

void
print_instr_ret(MirInstrRet *ret, FILE *stream)
{
  print_instr_head(&ret->base, stream);
  if (ret->value)
    fprintf(stream, "ret %%%u", ret->value->id);
  else
    fprintf(stream, "ret");
}

void
print_instr_store(MirInstrStore *store, FILE *stream)
{
  print_instr_head(&store->base, stream);
  assert(store->src && store->src);
  fprintf(stream, "store %%%u -> %%%u", store->src->id, store->dest->id);
}

void
print_instr_binop(MirInstrBinop *binop, FILE *stream)
{
  print_instr_head(&binop->base, stream);
  assert(binop->lhs && binop->rhs);
  const char *op = ast_binop_to_str(binop->op);
  fprintf(stream, "binop %%%u %s %%%u", binop->lhs->id, op, binop->rhs->id);
}

void
print_instr_block(MirInstrBlock *block, FILE *stream)
{
  if (block->base.prev) fprintf(stream, "\n");
  fprintf(stream, "%s_%u (%u):", block->name, block->base.id, block->base.ref_count);
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
  MirFn *fn = fn_proto->base.value.data.v_fn;
  assert(fn);

  fprintf(stream, "%s%s\n", fn_proto->base.analyzed ? "// analyzed" : "",
          fn_proto->base.ref_count ? "" : ", no LLVM representation");

  if (fn->name)
    fprintf(stream, "@%s ", fn->name);
  else
    fprintf(stream, "@%u ", fn_proto->base.id);

  fprintf(stream, "(%d) ", fn_proto->base.ref_count);
  print_type(fn_proto->base.value.type, false, stream);

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
  case MIR_INSTR_ADDR_OF:
    print_instr_addr_of((MirInstrAddrOf *)instr, stream);
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
  case MIR_INSTR_BLOCK:
    break;
  }

  fprintf(stream, "\n");
}

void
mir_print_module(MirModule *module, FILE *stream)
{
  assert(module);
  MirInstr *instr;
  barray_foreach(module->globals, instr) mir_print_instr(instr, stream);
}
