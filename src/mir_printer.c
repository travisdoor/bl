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

static inline void
print_type(MirType *type)
{
  char tmp[256];
  mir_type_to_str(tmp, ARRAY_SIZE(tmp), type);
  fprintf(stdout, BLUE("<%s>"), tmp);
}

static inline void
print_instr_head(MirInstr *instr)
{
  fprintf(stdout, "  ");
  assert(instr->value);

  if (instr->value->kind != MIR_VALUE_NO) {
    fprintf(stdout, YELLOW("%p") " ", instr->value);
    print_type(instr->value->type);
    fprintf(stdout, " = ");
  }
}

static void
print_var(MirVar *var);

static void
print_fn(MirFn *fn);

static void
print_block(MirBlock *block);

static void
print_instr(MirInstr *instr);

static void
print_ret(MirInstr *instr);

static void
print_call(MirInstr *call);

/* impl */
void
print_instr(MirInstr *instr)
{
  switch (instr->kind) {
  case MIR_INSTR_RET:
    print_ret(instr);
    break;
  case MIR_INSTR_CALL:
    print_call(instr);
    break;
  case MIR_INSTR_INVALID:
    fprintf(stdout, RED("INVALID"));
    break;
  }
};

void
print_call(MirInstr *call)
{
  print_instr_head(call);
  assert(call->data.call.callee);
  fprintf(stdout, "%p()\n", call->data.call.callee);
}

void
print_ret(MirInstr *instr)
{
  print_instr_head(instr);
  fprintf(stdout, "return");
  if (instr->data.ret.value) {
    fprintf(stdout, YELLOW(" %p"), instr->data.ret.value);
  }
  fprintf(stdout, "\n");
}

void
print_var(MirVar *var)
{
  fprintf(stdout, YELLOW("%p") " ", var->value);
  print_type(var->value->type);
  fprintf(stdout, " var %s", var->name);

  if (var->value->has_data) {
    fprintf(stdout, " = %llu", var->value->data.int_value);
  }
  fprintf(stdout, "\n");
}

void
print_fn(MirFn *fn)
{
  fprintf(stdout, YELLOW("%p") " ", fn->value);
  print_type(fn->value->type);
  fprintf(stdout, " fn %s {\n", fn->name);

  MirBlock *tmp;
  barray_foreach(fn->blocks, tmp) print_block(tmp);

  fprintf(stdout, "}\n\n");
}

void
print_block(MirBlock *block)
{
  fprintf(stdout, "%s:\n", block->name);
  MirInstr *tmp;
  barray_foreach(block->instructions, tmp) print_instr(tmp);
}

void
mir_printer_module(MirModule *module)
{
  fprintf(stdout, "mod %s\n\n", module->name);

  /* print globals */
  {
    MirVar *tmp;
    barray_foreach(module->globals, tmp) print_var(tmp);
  }

  fprintf(stdout, "\n");

  {
    MirFn *tmp;
    barray_foreach(module->fns, tmp) print_fn(tmp);
  }
}
