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
print_type(MirType *type)
{
  char tmp[256];
  mir_type_to_str(tmp, ARRAY_SIZE(tmp), type);
  fprintf(stdout, "%s", tmp);
}

static inline void
print_instr_head(MirInstr *instr)
{
  fprintf(stdout, "  %%%u ", instr->id);
  print_type(instr->type);
  fprintf(stdout, " = ");
}

static void
print_block(MirBlock *block);

static void
print_instr(MirInstr *instr);

static void
print_instr_decl_var(MirInstrDeclVar *decl);

static void
print_instr_const_int(MirInstrConstInt *ci);

static void
print_instr_ret(MirInstrRet *ret);

static void
print_instr_store(MirInstrStore *store);

/* impl */
void
print_instr_decl_var(MirInstrDeclVar *decl)
{
  assert(decl->var);
  const char *name = decl->var->name->data.ident.str;
  
  print_instr_head(&decl->base);
  fprintf(stdout, "'%s' ", name);
  print_type(decl->var->type);
}

void
print_instr_const_int(MirInstrConstInt *ci)
{
  print_instr_head(&ci->base);
  fprintf(stdout, "%llu", ci->value);
}

void
print_instr_ret(MirInstrRet *ret)
{
  print_instr_head(&ret->base);
  if (ret->value)
    fprintf(stdout, "ret %%%u", ret->value->id);
  else
    fprintf(stdout, "ret");
}

void
print_instr_store(MirInstrStore *store)
{
  print_instr_head(&store->base);
  assert(store->src && store->src);
  fprintf(stdout, "store %%%u -> %%%u", store->src->id, store->dest->id);
}

void
print_instr(MirInstr *instr)
{
  switch (instr->kind) {
  case MIR_INSTR_INVALID:
    fprintf(stdout, "INVALID");
    break;
  case MIR_INSTR_DECL_VAR:
    print_instr_decl_var((MirInstrDeclVar *)instr);
    break;
  case MIR_INSTR_CONST_INT:
    print_instr_const_int((MirInstrConstInt *)instr);
    break;
  case MIR_INSTR_LOAD:
    break;
  case MIR_INSTR_STORE:
    print_instr_store((MirInstrStore *)instr);
    break;
  case MIR_INSTR_RET:
    print_instr_ret((MirInstrRet *)instr);
    break;
  }
  fprintf(stdout, "\n");
}

void
print_block(MirBlock *block)
{
  fprintf(stdout, "%s:\n", block->name);

  MirInstr *tmp;
  barray_foreach(block->instructions, tmp) print_instr(tmp);
}

/* public */
void
mir_printer_exec(MirExec *exec)
{
  assert(exec);
  fprintf(stdout, "\n{\n");

  MirBlock *tmp;
  barray_foreach(exec->blocks, tmp) print_block(tmp);

  fprintf(stdout, "}\n");
}
