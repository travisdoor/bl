//************************************************************************************************
// Biscuit Engine
//
// File:   llvm_native_bin.c
// Author: Martin Dorazil
// Date:   10/03/2018
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

#include "stages_impl.h"
#include "bl/config.h"

bl_error_e
bl_llvm_native_bin_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
#if defined(BL_PLATFORM_LINUX)
  const char *cmd =
      "ld --hash-style=gnu --no-add-needed --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker "
      "/lib64/ld-linux-x86-64.so.2 %s.o -o %s "
      "/usr/lib64/crt1.o "
      "/usr/lib64/crti.o "
      "-L/usr/bin "
      "-L/usr/lib64 "
      "-lc %s "
      "/usr/lib64/crtn.o";
#elif defined(BL_PLATFORM_MACOS)
  const char *cmd = "ld %s.o -o %s -lc -lcrt1.o";
#endif

  // TODO: use dynamic buffer
  char buf[1024];
  sprintf(buf, cmd, assembly->name, assembly->name);

  bo_iterator_t iter = bo_htbl_begin(assembly->link_cache);
  bo_iterator_t end  = bo_htbl_end(assembly->link_cache);
  const char *  lib  = NULL;
  while (!bo_iterator_equal(&iter, &end)) {
    lib = bo_htbl_iter_peek_value(assembly->link_cache, &iter, const char *);
    strcat(&buf[0], " -l");
    strcat(&buf[0], lib);
    bo_htbl_iter_next(assembly->link_cache, &iter);
    bl_log("linking %s", lib);
  }

  bl_log("cmd %s", buf);
  /* TODO: handle error */
  int result = system(buf);
  if (result != 0) {
    return BL_ERR_JIT_RUN_FAILED;
  }

  return BL_NO_ERR;
}
