/*

 Package: dyncall
 Library: dyncall
 File: dyncall/dyncall_callvm_mips_eabi.h
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

  dyncall callvm for 32bit MIPS family of processors

  SUPPORTED CALLING CONVENTIONS
  eabi

  REVISION
  2008/01/03 initial

*/


#ifndef DYNCALL_CALLVM_MIPS_EABI_H
#define DYNCALL_CALLVM_MIPS_EABI_H

#include "dyncall_callvm.h"
#include "dyncall_vector.h"


/* Call-kernel register data: 

   Details:
   Two register content buffers for the corresponding register types 
   integer and float are filled from CallVM code and then later at
   call-kernel loaded into the registers.
 */

typedef struct
{
  DCint   mIntData[8];
  DCfloat mSingleData[8];
} DCRegData_mips_eabi;


typedef struct
{
  DCCallVM            mInterface;
  int                 mIntRegs;
  int                 mSingleRegs;
  DCRegData_mips_eabi mRegData;
  DCVecHead           mVecHead;
} DCCallVM_mips_eabi;

#endif /* DYNCALL_CALLVM_MIPS_EABI_H */

