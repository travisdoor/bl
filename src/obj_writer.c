// =================================================================================================
// bl
//
// File:   obj_writer.c
// Author: Martin Dorazil
// Date:   28/02/2018
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
// =================================================================================================

#include "builder.h"
#include "common.h"
#include "error.h"
#include "llvm_api.h"

// Target specific.
#if BL_PLATFORM_WIN
#define OBJ_EXT "obj"
#else
#define OBJ_EXT "o"
#endif

// Emit assembly object file.
void obj_writer_run(Assembly *assembly)
{
    ZONE();
    TString *     buf    = get_tmpstr();
    const Target *target = assembly->target;
    const char *  name   = target->name;
    BL_LOG("out_dir = %s", target->out_dir.data);
    BL_LOG("name = %s", name);
    tstring_setf(buf, "%s/%s.%s", target->out_dir.data, name, OBJ_EXT);
    char *error_msg = NULL;
    if (LLVMTargetMachineEmitToFile(
            assembly->llvm.TM, assembly->llvm.module, buf->data, LLVMObjectFile, &error_msg)) {
        builder_error("Cannot emit object file: %s with error: %s", buf->data, error_msg);
        LLVMDisposeMessage(error_msg);
    }
    put_tmpstr(buf);
    RETURN_END_ZONE();
}
