/*

 Package: dyncall
 Library: dyncall
 File: dyncall/dyncall_callvm_mips_n32.h
 Description: mips64 "n32" ABI callvm C interface.
 License:

   Copyright (c) 2007-2018 Daniel Adler <dadler@uni-goettingen.de>,
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



#ifndef DYNCALL_CALLVM_MIPS_N32_H
#define DYNCALL_CALLVM_MIPS_N32_H

#include "dyncall_call_mips_n32.h"
#include "dyncall_callvm.h"
#include "dyncall_vector.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
  DCCallVM                  mInterface;
  DCint                     mRegCount;
  struct DCRegData_mips_n32 mRegData;
  DCVecHead                 mVecHead;
} DCCallVM_mips_n32;
/* @@@ this is the same as n64, combine code */

#ifdef __cplusplus
}
#endif

#endif /* DYNCALL_CALLVM_MIPS_N32_H */

