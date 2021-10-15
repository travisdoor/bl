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

#define ASM_EXT "s"

// Emit assembly file.
void asm_writer_run(struct assembly *assembly)
{
    ZONE();
    TString             *buf    = get_tmpstr();
    const struct target *target = assembly->target;
    const char          *name   = target->name;
    BL_LOG("out_dir = %s", target->out_dir.data);
    BL_LOG("name = %s", name);
    tstring_setf(buf, "%s/%s.%s", target->out_dir.data, name, ASM_EXT);
    char *error_msg = NULL;
    if (LLVMTargetMachineEmitToFile(assembly->llvm.TM,
                                    assembly->llvm.modules[0],
                                    buf->data,
                                    LLVMAssemblyFile,
                                    &error_msg)) {
        builder_error("Cannot emit assembly file: %s with error: %s", buf->data, error_msg);
        LLVMDisposeMessage(error_msg);
    }
    builder_note("Assembly code written into %s", buf->data);
    put_tmpstr(buf);
    RETURN_ZONE();
}
