/*

 Package: dyncall
 Library: dyncall
 File: dyncall/dyncall_callvm_ppc64.h
 Description: 
 License:

   Copyright (c) 2014-2015 Masanori Mitsugi <mitsugi@linux.vnet.ibm.com>
                      2020 Tassilo Philipp <tphilipp@potion-studios.com>

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


#ifndef DYNCALL_CALLVM_PPC64_H
#define DYNCALL_CALLVM_PPC64_H

/*

  dyncall callvm for 64bit ppc architectures

  SUPPORTED CALLING CONVENTIONS
  standard and ... (ellipsis) calls

  REVISION
  2014/08/07 initial

*/


#include "dyncall_callvm.h"
#include "dyncall_vector.h"


typedef struct
{
  DClonglong mIntData[8];
  DCdouble   mFloatData[13];
} DCRegData_ppc64;

typedef struct
{
  DCCallVM        mInterface;
  int             mIntRegs;
  int             mFloatRegs;
  DCRegData_ppc64 mRegData;
  DCVecHead       mVecHead;
} DCCallVM_ppc64;


#endif /* DYNCALL_CALLVM_PPC64_H */

