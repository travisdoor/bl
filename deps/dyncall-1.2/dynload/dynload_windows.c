/*

 Package: dyncall
 Library: dynload
 File: dynload/dynload_windows.c
 Description: 
 License:

   Copyright (c) 2007-2020 Daniel Adler <dadler@uni-goettingen.de>,
                           Tassilo Philipp <tphilipp@potion-studios.com>

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

*/



/*

  dynload_windows.c

  dynload module for .dll files

*/


#include "dynload.h"
#include "dynload_alloc.h"

#include <windows.h>


DLLib* dlLoadLibrary(const char* libPath)
{
  if(libPath == NULL)
    return (DLLib*)GetModuleHandle(NULL);
  else {
    /* convert from UTF-8 to wide chars, so count required size */
    DLLib* pLib;
    wchar_t* ws;
    int r = MultiByteToWideChar(CP_UTF8, 0, libPath, -1, NULL, 0);
    if(!r) {
      return NULL;
    }

    /* Reserve temp space with room for extra '.' suffix (see below) */
    ws = (wchar_t*)dlAllocMem((r+1) * sizeof(wchar_t));
    if(!ws)
      return NULL;

    /* Convert path and add a '.' suffix, needed to tell windows not to add
       .dll to any path that doesn't have it (see MS doc for LoadLibraryW).
       This is to get same behaviour as on other platforms which don't  do any
       magic like this. Library search path behaviour stays unaffected, though */
    pLib = NULL;
    if(MultiByteToWideChar(CP_UTF8, 0, libPath, -1, ws, r) == r) {
        ws[r-1] = '.';
        ws[r] = 0;
        pLib = (DLLib*)LoadLibraryW(ws);
    }

    /* ... free temp space and return handle */
    dlFreeMem(ws);
    return pLib;
  }
}


void* dlFindSymbol(DLLib* pLib, const char* pSymbolName)
{
  return (void*)GetProcAddress((HINSTANCE)pLib, pSymbolName);
}


void dlFreeLibrary(DLLib* pLib)
{
  FreeLibrary((HINSTANCE)pLib);
}


int dlGetLibraryPath(DLLib* pLib, char* sOut, int bufSize)
{
  /* get the path name as wide chars, then convert to UTF-8; we need   */
  /* some trial and error to figure out needed wide char string length */

  wchar_t* ws;
  int r;

  /* num chars to alloc temp space for, and upper limit, must be both power */
  /* of 2s for loop to be precise and to test allow testing up to 32768 chars */
  /* (including \0), which is the extended path ("\\?\...") maximum */
  static const int MAX_EXT_PATH = 1<<15; /* max extended path length (32768) */
  int nc = 1<<6;                         /* guess start buffer size, */

  while(nc <= MAX_EXT_PATH)/*@@@ add testcode for super long paths*/
  {
    ws = (wchar_t*)dlAllocMem(nc * sizeof(wchar_t));
    if(!ws)
      break;

    r = GetModuleFileNameW((HMODULE)pLib, ws, nc);

    /* r == nc if string was truncated, double temp buffer size */
    if(r == nc) {
      nc <<= 1;
      dlFreeMem(ws);
      continue;
    }
    /* error if r is 0 */
    else if(!r) {
      dlFreeMem(ws);
      break;
    }

    /* check if output buffer is big enough */
    r = WideCharToMultiByte(CP_UTF8, 0, ws, -1, NULL, 0, NULL, NULL);
    if(r <= bufSize)
      r = WideCharToMultiByte(CP_UTF8, 0, ws, -1, sOut, bufSize, NULL, NULL);

    /* cleanup and return either size of copied bytes or needed buffer size */
    dlFreeMem(ws);
    return r;
  }

  return 0;
}

