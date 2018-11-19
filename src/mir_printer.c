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
#define GOTO_COLOR GREEN
#define ERROR_COLOR RED

static inline void
print_type(MirType *type)
{
  char tmp[256];
  mir_type_to_str(tmp, ARRAY_SIZE(tmp), type);
  fprintf(stdout, TYPE_COLOR("<%s>"), tmp);
}

static inline void
print_instr_head(MirInstr *instr)
{
  if (!instr) return;
  fprintf(stdout, "  %%%u (%d) ", instr->id, instr->ref_count);
  print_type(instr->value.type);
  fprintf(stdout, " = ");
}

static void
print_block(MirBlock *block);

static void
print_instr(MirInstr *instr);

static void
print_instr_decl_var(MirInstrDeclVar *decl);

static void
print_instr_const(MirInstrConst *ci);

static void
print_instr_ret(MirInstrRet *ret);

static void
print_instr_store(MirInstrStore *store);

static void
print_instr_binop(MirInstrBinop *binop);

static void
print_instr_validate_type(MirInstrValidateType *vt);

static void
print_instr_call(MirInstrCall *call);

/* impl */
void
print_instr_decl_var(MirInstrDeclVar *decl)
{
  assert(decl->var);
  const char *name = decl->var->name->data.ident.str;

  fprintf(stdout, INSTR_COLOR("decl") " %s %%%u", name, decl->type->id);
}

void
print_instr_const(MirInstrConst *cnst)
{
  MirValue *value = &cnst->base.value;
  assert(value->type);
  switch (value->type->kind) {
  case MIR_TYPE_INT:
    fprintf(stdout, "%llu", value->data.v_int);
    break;
  case MIR_TYPE_TYPE:
    print_type(value->data.v_type);
    break;
  default:
    fprintf(stdout, "cannot read value");
  }
}

void
print_instr_call(MirInstrCall *call)
{
  fprintf(stdout, INSTR_COLOR("call "));
  if (call->callee->node) {
    assert(call->callee->node->kind == AST_IDENT);
    fprintf(stdout, "%s", call->callee->node->data.ident.str);
  } else {
    fprintf(stdout, "%p", call->callee);
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
  fprintf(stdout, INSTR_COLOR("@validate_type") " %%%u", vt->src->id);
}

void
print_instr_ret(MirInstrRet *ret)
{
  if (ret->value)
    fprintf(stdout, INSTR_COLOR("ret") " %%%u", ret->value->id);
  else
    fprintf(stdout, "ret");
}

void
print_instr_store(MirInstrStore *store)
{
  assert(store->src && store->src);
  fprintf(stdout, INSTR_COLOR("store") " %%%u -> %%%u", store->src->id, store->dest->id);
}

void
print_instr_binop(MirInstrBinop *binop)
{
  assert(binop->lhs && binop->rhs);
  const char *op = ast_binop_to_str(binop->op);
  fprintf(stdout, "%%%u %s %%%u", binop->lhs->id, op, binop->rhs->id);
}

void
print_instr(MirInstr *instr)
{
  print_instr_head(instr);

  switch (instr->kind) {
  case MIR_INSTR_INVALID:
    fprintf(stdout, RED("INVALID"));
    break;
  case MIR_INSTR_DECL_VAR:
    print_instr_decl_var((MirInstrDeclVar *)instr);
    break;
  case MIR_INSTR_CONST:
    print_instr_const((MirInstrConst *)instr);
    break;
  case MIR_INSTR_LOAD:
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
    break;
  }
  fprintf(stdout, "\n");
}

void
print_block(MirBlock *block)
{
  fprintf(stdout, GOTO_COLOR("%s:\n"), block->name);

  MirInstr *tmp;
  barray_foreach(block->instructions, tmp) print_instr(tmp);
}

/* public */
void
mir_printer_fn(MirFn *fn)
{
  assert(fn);
  if (fn->name) {
    fprintf(stdout, "\n%s ", fn->name->data.ident.str);
    print_type(fn->type);
    fprintf(stdout, " {\n");
  } else {
    fprintf(stdout, "\n{\n");
  }

  MirBlock *tmp;
  barray_foreach(fn->exec->blocks, tmp) print_block(tmp);
  fprintf(stdout, "}");

  if (fn->analyzed) {
    fprintf(stdout, " => {\n");
    barray_foreach(fn->exec_analyzed->blocks, tmp) print_block(tmp);
    fprintf(stdout, "}\n");
  } else {
    fprintf(stdout, " => " ERROR_COLOR("MISING\n"));
  }
}
