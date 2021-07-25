/*

 Package: dyncall
 Library: dyncall
 File: dyncall/dyncall_callvm_mips_n64.h
 Description: mips "n64" ABI callvm C interface.
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



#ifndef DYNCALL_CALLVM_MIPS_N64_H
#define DYNCALL_CALLVM_MIPS_N64_H

#include "dyncall_callvm.h"
#include "dyncall_vector.h"

#ifdef __cplusplus
extern "C" {
#endif


/*
  two register-files for integer (promoted to 64-bit) and float (not promoted!)
  are used.

  arguments are transfered in a free slot on the corresponding register file.
  the other register-file will be skipped by one.

  float arguments are either loaded from single or double -
  a auto-conversion into double and then loaded as double precision
  turned out to fail for several tests.

  therefore a union for storage of float or double is used instead.
  a bitmask (mUseDouble) records which type is used and will be
  interpreted in the call-kernel.
*/

typedef struct
{
  DClonglong                       mIntData[8];
#if defined(DC__ABI_HARDFLOAT)
  union { DCfloat f; DCdouble d; } mFloatData[8];
  DClonglong                       mUseDouble; /* bitmask: lower 8 bits specify to use float or double from union array. */
#endif /* DC__ABI_HARDFLOAT */
} DCRegData_mips_n64;


typedef struct
{
  DCCallVM           mInterface;
  DCint              mRegCount;
  DCRegData_mips_n64 mRegData;
  DCVecHead          mVecHead;
} DCCallVM_mips_n64;


#ifdef __cplusplus
}
#endif

#endif /* DYNCALL_CALLVM_MIPS_N64_H */

