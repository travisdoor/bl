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

#define TYPE_COLOR BLUE
#define INSTR_COLOR YELLOW
#define INSTR_ANALYZE_COLOR MAGENTA
#define BLOCK_COLOR GREEN
#define ERROR_COLOR RED

static inline void
print_type(MirType *type, bool aligned)
{
  char tmp[256];
  mir_type_to_str(tmp, ARRAY_SIZE(tmp), type);
  if (aligned)
    fprintf(stdout, TYPE_COLOR("%10s"), tmp);
  else
    fprintf(stdout, TYPE_COLOR("%s"), tmp);
}

static inline void
print_instr_head(MirInstr *instr)
{
  if (!instr) return;
  fprintf(stdout, "  %%%-3u (%d) ", instr->id, instr->ref_count);
  print_type(instr->value.type, true);
  fprintf(stdout, " ");
}

static void
print_instr_load(MirInstrLoad *load);

static void
print_instr_addr_of(MirInstrAddrOf *addr_of);

static void
print_instr_cond_br(MirInstrCondBr *cond_br);

static void
print_instr_br(MirInstrBr *br);

static void
print_instr_try_infer(MirInstrTryInfer *infer);

static void
print_instr_fn_proto(MirInstrFnProto *fn_proto, bool analyzed);

static void
print_instr_type_fn(MirInstrTypeFn *type_fn);

static void
print_block(MirBlock *block);

static void
print_instr_decl_var(MirInstrDeclVar *decl);

static void
print_instr_const(MirInstrConst *ci);

static void
print_instr_ret(MirInstrRet *ret);

static void
print_instr_unreachable(MirInstrUnreachable *instr);

static void
print_instr_store(MirInstrStore *store);

static void
print_instr_binop(MirInstrBinop *binop);

static void
print_instr_validate_type(MirInstrValidateType *vt);

static void
print_instr_call(MirInstrCall *call);

static void
print_instr_decl_ref(MirInstrDeclRef *ref);

/* impl */
void
print_instr_type_fn(MirInstrTypeFn *type_fn)
{
  print_instr_head(&type_fn->base);
  fprintf(stdout, INSTR_COLOR("const"));

  fprintf(stdout, " fn (");

  if (type_fn->arg_types) {
    MirInstr *tmp;
    barray_foreach(type_fn->arg_types, tmp)
    {
      fprintf(stdout, "%%%u", tmp->id);
      if (i + 1 < bo_array_size(type_fn->arg_types)) fprintf(stdout, ", ");
    }
  }

  fprintf(stdout, ")");

  if (type_fn->ret_type) fprintf(stdout, " %%%u", type_fn->ret_type->id);
}

void
print_instr_cond_br(MirInstrCondBr *cond_br)
{
  print_instr_head(&cond_br->base);
  fprintf(stdout, INSTR_COLOR("br") " %%%u ? " BLOCK_COLOR("%s_%u") " : " BLOCK_COLOR("%s_%u"),
          cond_br->cond->id, cond_br->then_block->name, cond_br->then_block->id,
          cond_br->else_block->name, cond_br->else_block->id);
}

void
print_instr_br(MirInstrBr *br)
{
  print_instr_head(&br->base);
  fprintf(stdout, INSTR_COLOR("br") BLOCK_COLOR(" %s_%d"), br->then_block->name,
          br->then_block->id);
}

void
print_instr_try_infer(MirInstrTryInfer *infer)
{
  print_instr_head(&infer->base);
  fprintf(stdout, INSTR_ANALYZE_COLOR("tryinfer") " %%%u -> %%%u", infer->src->id, infer->dest->id);
}

void
print_instr_load(MirInstrLoad *load)
{
  print_instr_head(&load->base);
  fprintf(stdout, INSTR_COLOR("load") " %%%u", load->src->id);
}

void
print_instr_addr_of(MirInstrAddrOf *addr_of)
{
  print_instr_head(&addr_of->base);
  fprintf(stdout, INSTR_COLOR("&") "%%%u", addr_of->target->id);
}

void
print_instr_unreachable(MirInstrUnreachable *instr)
{
  print_instr_head(&instr->base);
  fprintf(stdout, INSTR_COLOR("unreachable"));
}

void
print_instr_decl_var(MirInstrDeclVar *decl)
{
  print_instr_head(&decl->base);

  assert(decl->var);
  const char *name = decl->var->name->data.ident.str;

  fprintf(stdout, INSTR_COLOR("decl") " %s", name);
}

void
print_instr_decl_ref(MirInstrDeclRef *ref)
{
  print_instr_head(&ref->base);
  const char *name = ref->base.node->data.ident.str;

  fprintf(stdout, INSTR_COLOR("declref") " %s", name);
}

void
print_instr_const(MirInstrConst *cnst)
{
  print_instr_head(&cnst->base);

  MirValue *value = &cnst->base.value;
  assert(value->type);

  fprintf(stdout, INSTR_COLOR("const "));
  switch (value->type->kind) {
  case MIR_TYPE_INT:
    fprintf(stdout, "%llu", value->data.v_int);
    break;
  case MIR_TYPE_BOOL:
    fprintf(stdout, "%s", value->data.v_bool ? "true" : "false");
    break;
  case MIR_TYPE_TYPE:
    print_type(value->data.v_type, false);
    break;
  default:
    fprintf(stdout, "cannot read value");
  }
}

void
print_instr_call(MirInstrCall *call)
{
  print_instr_head(&call->base);

  fprintf(stdout, INSTR_COLOR("call "));
  if (call->callee->node) {
    assert(call->callee->node->kind == AST_IDENT);
    fprintf(stdout, "@%s", call->callee->node->data.ident.str);
  } else {
    fprintf(stdout, "@%u", call->callee->id);
  }

  fprintf(stdout, "(");
  if (call->args) {
    MirInstr *tmp;
    barray_foreach(call->args, tmp)
    {
      fprintf(stdout, "%%%u", tmp->id);
      if (i != bo_array_size(call->args)) fprintf(stdout, ", ");
    }
  }
  fprintf(stdout, ")");
}

void
print_instr_validate_type(MirInstrValidateType *vt)
{
  print_instr_head(&vt->base);
  fprintf(stdout, INSTR_ANALYZE_COLOR("validate_type") " %%%u", vt->src->id);
}

void
print_instr_ret(MirInstrRet *ret)
{
  print_instr_head(&ret->base);
  if (ret->value)
    fprintf(stdout, INSTR_COLOR("ret") " %%%u", ret->value->id);
  else
    fprintf(stdout, INSTR_COLOR("ret"));
}

void
print_instr_store(MirInstrStore *store)
{
  print_instr_head(&store->base);
  assert(store->src && store->src);
  fprintf(stdout, INSTR_COLOR("store") " %%%u -> %%%u", store->src->id, store->dest->id);
}

void
print_instr_binop(MirInstrBinop *binop)
{
  print_instr_head(&binop->base);
  assert(binop->lhs && binop->rhs);
  const char *op = ast_binop_to_str(binop->op);
  fprintf(stdout, INSTR_COLOR("op") " %%%u %s %%%u", binop->lhs->id, op, binop->rhs->id);
}

void
print_block(MirBlock *block)
{
  fprintf(stdout, BLOCK_COLOR("%s_%u:\n"), block->name, block->id);

  MirInstr *tmp;
  barray_foreach(block->instructions, tmp) mir_print_instr(tmp, false);
}

void
print_instr_fn_proto(MirInstrFnProto *fn_proto, bool analyzed)
{
  if (fn_proto->base.node)
    fprintf(stdout, "@%s ", fn_proto->base.node->data.ident.str);
  else
    fprintf(stdout, "@%u ", fn_proto->base.id);

  fprintf(stdout, "(%d) ", fn_proto->base.ref_count);
  print_type(fn_proto->base.value.type, false);

  MirFn *fn = fn_proto->base.value.data.v_fn;
  if (fn) {
    MirBlock *tmp;
    MirExec * exec = analyzed ? fn->exec_analyzed : fn->exec;
    fprintf(stdout, " { %s\n", analyzed ? GREEN("// ANALYZED") : "");
    if (exec) {
      barray_foreach(exec->blocks, tmp) print_block(tmp);
    } else {
      fprintf(stdout, RED("MISSING!!!\n"));
    }
    fprintf(stdout, "}\n");
  }
}

/* public */
void
mir_print_instr(MirInstr *instr, bool analyzed)
{
  switch (instr->kind) {
  case MIR_INSTR_INVALID:
    fprintf(stdout, RED("INVALID"));
    break;
  case MIR_INSTR_UNREACHABLE:
    print_instr_unreachable((MirInstrUnreachable *)instr);
    break;
  case MIR_INSTR_DECL_VAR:
    print_instr_decl_var((MirInstrDeclVar *)instr);
    break;
  case MIR_INSTR_CONST:
    print_instr_const((MirInstrConst *)instr);
    break;
  case MIR_INSTR_LOAD:
    print_instr_load((MirInstrLoad *)instr);
    break;
  case MIR_INSTR_STORE:
    print_instr_store((MirInstrStore *)instr);
    break;
  case MIR_INSTR_RET:
    print_instr_ret((MirInstrRet *)instr);
    break;
  case MIR_INSTR_BINOP:
    print_instr_binop((MirInstrBinop *)instr);
    break;
  case MIR_INSTR_VALIDATE_TYPE:
    print_instr_validate_type((MirInstrValidateType *)instr);
    break;
  case MIR_INSTR_CALL:
    print_instr_call((MirInstrCall *)instr);
    break;
  case MIR_INSTR_FN_PROTO:
    print_instr_fn_proto((MirInstrFnProto *)instr, analyzed);
    break;
  case MIR_INSTR_DECL_REF:
    print_instr_decl_ref((MirInstrDeclRef *)instr);
    break;
  case MIR_INSTR_TYPE_FN:
    print_instr_type_fn((MirInstrTypeFn *)instr);
    break;
  case MIR_INSTR_ADDR_OF:
    print_instr_addr_of((MirInstrAddrOf *)instr);
    break;
  case MIR_INSTR_TRY_INFER:
    print_instr_try_infer((MirInstrTryInfer *)instr);
    break;
  case MIR_INSTR_COND_BR:
    print_instr_cond_br((MirInstrCondBr *)instr);
    break;
  case MIR_INSTR_BR:
    print_instr_br((MirInstrBr *)instr);
    break;
  }
  fprintf(stdout, "\n");
}
