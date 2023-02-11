// =================================================================================================
// bl
//
// File:   asm_writer.c
// Author: Martin Dorazil
// Date:   30/09/2021
//
// Copyright 2021 Martin Dorazil
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
// =================================================================================================

#include "builder.h"
#include "stb_ds.h"

#define ASM_EXT "s"

// Emit assembly file.
void asm_writer_run(struct assembly *assembly)
{
	zone();
	char                *buf    = tstr();
	const struct target *target = assembly->target;
	const char          *name   = target->name;
	blog("out_dir = %s", target->out_dir);
	blog("name = %s", name);
	strprint(buf, "%s/%s.%s", target->out_dir, name, ASM_EXT);
	char *error_msg = NULL;
	if (LLVMTargetMachineEmitToFile(
			assembly->llvm.TM, assembly->llvm.modules[0], buf, LLVMAssemblyFile, &error_msg)) {
		builder_error("Cannot emit assembly file: %s with error: %s", buf, error_msg);
		LLVMDisposeMessage(error_msg);
	}
	builder_info("Assembly code written into %s", buf);
	put_tstr(buf);
	return_zone();
}
