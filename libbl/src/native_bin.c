//************************************************************************************************
// bl
//
// File:   native_bin.c
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

void
native_bin_run(builder_t *builder, assembly_t *assembly)
{
#if defined(BL_PLATFORM_LINUX)
  const char *cmd =
      "ld --hash-style=gnu --no-add-needed --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker "
      "/lib64/ld-linux-x86-64.so.2 %s.o -o %s "
      "/usr/lib64/crt1.o "
      "/usr/lib64/crti.o "
      "-L/usr/bin "
      "-L/usr/lib64 "
      "/usr/lib64/crtn.o "
      "-lc ";
#elif defined(BL_PLATFORM_MACOS)
  const char *cmd = "ld %s.o -o %s -lc -lcrt1.o";
#elif defined(BL_PLATFORM_WIN)
  const char *cmd =
      "link %s.obj /NOLOGO /INCREMENTAL:NO  /MACHINE:x64 /OUT:%s.exe "
      "/LIBPATH:\"C:\\Program Files (x86)\\Microsoft Visual "
      "Studio\\2017\\Community\\VC\\Tools\\MSVC\\14.14.26428\\lib\\x64\" "
      "/LIBPATH:\"C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.17134.0\\um\\x64\" "
      "/LIBPATH:\"C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.17134.0\\ucrt\\x64\" "
      "kernel32.lib user32.lib gdi32.lib shell32.lib ucrt.lib legacy_stdio_definitions.lib "
      "Msvcrt.lib";
#endif

  // TODO: use dynamic buffer
  char buf[1024];
  sprintf(buf, cmd, assembly->name, assembly->name);

  const char *  lib;
  bo_iterator_t iter;
  bhtbl_foreach(assembly->link_cache, iter)
  {
    lib = bo_htbl_iter_peek_value(assembly->link_cache, &iter, const char *);
    strcat(&buf[0], " -l");
    strcat(&buf[0], lib);
  }

  bl_msg_log("%s", buf);
  /* TODO: handle error */
  int result = system(buf);
  if (result != 0) {
    return;
  }
}
