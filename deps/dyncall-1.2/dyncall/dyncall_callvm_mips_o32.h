/*

 Package: dyncall
 Library: dyncall
 File: dyncall/dyncall_callvm_mips_o32.h
 Description: mips "o32" ABI callvm C interface.
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



#ifndef DYNCALL_CALLVM_MIPS_O32_H
#define DYNCALL_CALLVM_MIPS_O32_H

#include "dyncall_callvm.h"
#include "dyncall_vector.h"


/* Call-kernel register data:

  Details:

  The structure holds the argument data for transfering float/double arguments
  via registers as well.
  The call-kernel on hardfloat platforms implements loads two doubles, which
  involves four 32-bit floating pointer registers. It's unused for softfloat
  platforms.

  Float arguments map as following:
  
    float argument 0 is at u[0][0] for little, u[0][1] for big endian and
    float argument 1 is at u[1][0] for little, u[1][1] for big endian of
	DCRegData_mips_o32 union.

*/

typedef struct
{
#if defined(DC__ABI_HARDFLOAT)
  union {
    double d;
    float  f[2];
  } u[2];
#endif /* DC__ABI_HARDFLOAT */
} DCRegData_mips_o32;


typedef struct
{
  DCCallVM           mInterface;
  int                mArgCount;
  DCRegData_mips_o32 mRegData;
  DCVecHead          mVecHead;
} DCCallVM_mips_o32;


#endif /* DYNCALL_CALLVM_MIPS_O32_H */

