//************************************************************************************************
// bl
//
// File:   ir.c
// Author: Martin Dorazil
// Date:   12/9/18
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

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Linker.h>
#include <llvm-c/DebugInfo.h>
#include "mir.h"
#include "unit.h"
#include "common.h"
#include "builder.h"
#include "assembly.h"

typedef struct
{
  Builder * builder;
  Assembly *assembly;

  LLVMContextRef    llvm_cnt;
  LLVMModuleRef     llvm_module;
  LLVMTargetDataRef llvm_td;
  LLVMBuilderRef    llvm_builder;
} Context;

static void
gen_instr(Context *cnt, MirInstr *instr);

static void
gen_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto);

static void
gen_instr_block(Context *cnt, MirInstrBlock *block);

static void
gen_instr_br(Context *cnt, MirInstrBr *br);

static void
gen_instr_ret(Context *cnt, MirInstrRet *ret);

static void
gen_instr_decl_var(Context *cnt, MirInstrDeclVar *decl);

static void
gen_instr_const(Context *cnt, MirInstrConst *cnst);

static void
gen_instr_load(Context *cnt, MirInstrLoad *load);

void
gen_instr_load(Context *cnt, MirInstrLoad *load)
{
  LLVMValueRef llvm_src = load->src->llvm_value;
  assert(llvm_src);
  load->base.llvm_value = LLVMBuildLoad(cnt->llvm_builder, llvm_src, "tmp");
}

void
gen_instr_const(Context *cnt, MirInstrConst *cnst)
{
  MirValue *value = &cnst->base.value;
  MirType * type  = value->type;
  assert(type);
  LLVMTypeRef llvm_type = type->llvm_type;
  assert(llvm_type);

  switch (type->kind) {
  case MIR_TYPE_INT:
    cnst->base.llvm_value =
        LLVMConstInt(llvm_type, value->data.v_uint, type->data.integer.is_signed);
    break;
  default:
    bl_unimplemented;
  }
}

void
gen_instr_decl_var(Context *cnt, MirInstrDeclVar *decl)
{
  LLVMTypeRef llvm_type = decl->base.value.type->llvm_type;
  assert(llvm_type);
  decl->base.llvm_value = LLVMBuildAlloca(cnt->llvm_builder, llvm_type, decl->var->name);
}

void
gen_instr_ret(Context *cnt, MirInstrRet *ret)
{
  LLVMValueRef llvm_ret;
  if (ret->value) {
    bl_unimplemented;
  } else {
    llvm_ret = LLVMBuildRetVoid(cnt->llvm_builder);
  }

  ret->base.llvm_value = llvm_ret;
}

void
gen_instr_br(Context *cnt, MirInstrBr *br)
{
  MirInstrBlock *then_block = br->then_block;
  assert(then_block);

  LLVMBasicBlockRef llvm_then_block = NULL;
  if (!then_block->base.llvm_value) {
    llvm_then_block = LLVMAppendBasicBlockInContext(cnt->llvm_cnt, then_block->owner_fn->llvm_value,
                                                    then_block->name);
    then_block->base.llvm_value = LLVMBasicBlockAsValue(llvm_then_block);
  } else {
    llvm_then_block = LLVMValueAsBasicBlock(then_block->base.llvm_value);
  }

  assert(llvm_then_block);
  br->base.llvm_value = LLVMBuildBr(cnt->llvm_builder, llvm_then_block);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_then_block);
}

void
gen_instr_block(Context *cnt, MirInstrBlock *block)
{
  assert(block->owner_fn->llvm_value);
  LLVMBasicBlockRef llvm_block = NULL;

  if (!block->base.llvm_value) {
    llvm_block =
        LLVMAppendBasicBlockInContext(cnt->llvm_cnt, block->owner_fn->llvm_value, block->name);
    block->base.llvm_value = LLVMBasicBlockAsValue(llvm_block);
  } else {
    llvm_block = LLVMValueAsBasicBlock(block->base.llvm_value);
  }

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, llvm_block);
  MirInstr *instr = block->entry_instr;
  while (instr) {
    gen_instr(cnt, instr);
    instr = instr->next;
  }
}

void
gen_instr_fn_proto(Context *cnt, MirInstrFnProto *fn_proto)
{
  MirFn *fn = fn_proto->base.value.data.v_fn;
  assert(fn);

  fn->llvm_value = LLVMGetNamedFunction(cnt->llvm_module, fn->name);
  if (!fn->llvm_value) {
    fn->llvm_value =
        LLVMAddFunction(cnt->llvm_module, fn->name, fn_proto->base.value.type->llvm_type);

    if (!fn->is_external) {
      MirInstr *block = (MirInstr *)fn->first_block;

      while (block) {
        gen_instr(cnt, block);
        block = block->next;
      }
    }
  }
}

void
gen_instr(Context *cnt, MirInstr *instr)
{
  if (!instr || !instr->ref_count) return;

  switch (instr->kind) {
  case MIR_INSTR_FN_PROTO:
    gen_instr_fn_proto(cnt, (MirInstrFnProto *)instr);
    break;
  case MIR_INSTR_BLOCK:
    gen_instr_block(cnt, (MirInstrBlock *)instr);
    break;
  case MIR_INSTR_BR:
    gen_instr_br(cnt, (MirInstrBr *)instr);
    break;
  case MIR_INSTR_RET:
    gen_instr_ret(cnt, (MirInstrRet *)instr);
    break;
  case MIR_INSTR_DECL_VAR:
    gen_instr_decl_var(cnt, (MirInstrDeclVar *)instr);
    break;
  case MIR_INSTR_CONST:
    gen_instr_const(cnt, (MirInstrConst *)instr);
    break;
  case MIR_INSTR_LOAD:
    gen_instr_load(cnt, (MirInstrLoad *)instr);
    break;

  default:
    break;
    bl_abort("unimplemented");
  }
}

/* public */
void
ir_run(Builder *builder, Assembly *assembly)
{
  Context cnt;
  memset(&cnt, 0, sizeof(Context));
  cnt.builder      = builder;
  cnt.assembly     = assembly;
  cnt.llvm_cnt     = assembly->mir_module->llvm_cnt;
  cnt.llvm_module  = assembly->mir_module->llvm_module;
  cnt.llvm_td      = assembly->mir_module->llvm_td;
  cnt.llvm_builder = LLVMCreateBuilderInContext(assembly->mir_module->llvm_cnt);

  MirInstr *ginstr;
  barray_foreach(assembly->mir_module->globals, ginstr)
  {
    gen_instr(&cnt, ginstr);
  }

#if BL_DEBUG
  char *error = NULL;
  if (LLVMVerifyModule(cnt.llvm_module, LLVMReturnStatusAction, &error)) {
    msg_error("LLVM module not verified with error: %s", error);
  }
  LLVMDisposeMessage(error);
#endif

  LLVMDisposeBuilder(cnt.llvm_builder);
}
