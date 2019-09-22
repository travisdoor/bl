//************************************************************************************************
// bl
//
// File:   ir_opt.c
// Author: Martin Dorazil
// Date:   5.2.19
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

#include "assembly.h"
#include "bldebug.h"
#include "error.h"
#include "stages.h"
#include "llvm_api.h"

void
ir_opt_run(Assembly *assembly)
{
	LLVMModuleRef        llvm_module = assembly->llvm.module;
	LLVMTargetMachineRef llvm_tm     = assembly->llvm.TM;

	LLVMPassManagerBuilderRef llvm_pm_builder = LLVMPassManagerBuilderCreate();
	LLVMPassManagerBuilderSetOptLevel(llvm_pm_builder, assembly->options.opt_lvl);

	LLVMPassManagerRef llvm_pm = LLVMCreatePassManager();
	LLVMAddAnalysisPasses(llvm_tm, llvm_pm);
	LLVMPassManagerBuilderPopulateModulePassManager(llvm_pm_builder, llvm_pm);
	LLVMPassManagerBuilderPopulateLTOPassManager(llvm_pm_builder, llvm_pm, true, true);

	LLVMRunPassManager(llvm_pm, llvm_module);

	LLVMDisposePassManager(llvm_pm);
	LLVMPassManagerBuilderDispose(llvm_pm_builder);
}
