/*

 Package: dyncall
 Library: test
 File: test/callf/main.c
 Description:
 License:

   Copyright (c) 2007-2021 Daniel Adler <dadler@uni-goettingen.de>,
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



/* test dcCallF API */

#include "../../dyncall/dyncall_callf.h"
#include "../common/platformInit.h"
#include "../common/platformInit.c" /* Impl. for functions only used in this translation unit */

#include <stdarg.h>
#if defined(DC_UNIX) && !defined(DC__OS_BeOS)
#include <sys/syscall.h>
#endif


/* sample void function */

int vf_iii(int x,int y,int z)
{
  int r = (x == 1 && y == 2 && z == 3);
  printf("%d %d %d: %d", x, y, z, r);
  return r;
}

int vf_ffiffiffi(float a, float b, int c, float d, float e, int f, float g, float h, int i)
{
  int r = (a == 1.f && b == 2.f && c == 3 && d == 4.f && e == 5.f && f == 6 && g == 7.f && h == 8.f && i == 9);
  printf("%f %f %d %f %f %d %f %f %d: %d", a, b, c, d, e, f, g, h, i, r);
  return r;
}

int vf_ffiV(float a, float b, int c, ...)
{
  va_list ap;
  double d, e, g, h;
  int f, i;
  int r;

  va_start(ap, c);
  d = va_arg(ap, double);
  e = va_arg(ap, double);
  f = va_arg(ap, int);
  g = va_arg(ap, double);
  h = va_arg(ap, double);
  i = va_arg(ap, int);
  va_end(ap);

  r = (a == 1.f && b == 2.f && c == 3 && d == 4. && e == 5. && f == 6 && g == 7. && h == 8. && i == 9);
  printf("%f %f %d %f %f %d %f %f %d: %d", a, b, c, d, e, f, g, h, i, r);
  return r;
}

/* main */

int main(int argc, char* argv[])
{
  DCCallVM* vm;
  DCValue ret;
  int r = 1;

  dcTest_initPlatform();

  /* allocate call vm */
  vm = dcNewCallVM(4096);


  /* calls using 'formatted' API */
  dcReset(vm);
  printf("callf iii)i:       ");
  dcCallF(vm, &ret, (void*)&vf_iii, "iii)i", 1, 2, 3);
  r = ret.i && r;

  dcReset(vm);
  printf("\ncallf ffiffiffi)i: ");
  dcCallF(vm, &ret, (void*)&vf_ffiffiffi, "ffiffiffi)i", 1.f, 2.f, 3, 4.f, 5.f, 6, 7.f, 8.f, 9);
  r = ret.i && r;

  /* same but with calling convention prefix */
  dcReset(vm);
  printf("\ncallf _:ffiffiffi)i: ");
  dcCallF(vm, &ret, (void*)&vf_ffiffiffi, "_:ffiffiffi)i", 1.f, 2.f, 3, 4.f, 5.f, 6, 7.f, 8.f, 9);
  r = ret.i && r;

  /* vararg call */
  dcReset(vm);
  printf("\ncallf _effi_.ddiddi)i: ");
  dcCallF(vm, &ret, (void*)&vf_ffiV, "_effi_.ddiddi)i", 1.f, 2.f, 3, 4., 5., 6, 7., 8., 9);
  r = ret.i && r;

  /* arg binding then call using 'formatted' API */
  dcReset(vm);
  /* reset calling convention too */
  dcMode(vm, DC_CALL_C_DEFAULT);
  printf("\nargf iii)i       then call: ");
  dcArgF(vm, "iii)i", 1, 2, 3);
  r = r && dcCallInt(vm, (void*)&vf_iii);

  dcReset(vm);
  printf("\nargf iii         then call: ");
  dcArgF(vm, "iii", 1, 2, 3);
  r = r && dcCallInt(vm, (void*)&vf_iii);

  dcReset(vm);
  printf("\nargf ffiffiffi)i then call: ");
  dcArgF(vm, "ffiffiffi)i", 1.f, 2.f, 3, 4.f, 5.f, 6, 7.f, 8.f, 9);
  r = r && dcCallInt(vm, (void*)&vf_ffiffiffi);

  dcReset(vm);
  printf("\nargf ffiffiffi   then call: ");
  dcArgF(vm, "ffiffiffi", 1.f, 2.f, 3, 4.f, 5.f, 6, 7.f, 8.f, 9);
  r = r && dcCallInt(vm, (void*)&vf_ffiffiffi);

#if defined(DC_UNIX) && !defined(DC__OS_MacOSX) && !defined(DC__OS_SunOS) && !defined(DC__OS_BeOS)
  /* testing syscall using calling convention prefix - not available on all platforms */
  dcReset(vm);
  printf("\ncallf _$iZi)i");
  fflush(NULL); /* needed before syscall write as it's immediate, or order might be incorrect */
  dcCallF(vm, &ret, (DCpointer)(ptrdiff_t)SYS_write, "_$iZi)i", 1/*stdout*/, " = syscall: 1", 13);
  r = ret.i == 13 && r;
#endif

  /* free vm */
  dcFree(vm);

  printf("\nresult: callf: %d\n", r);

  dcTest_deInitPlatform();

  return 0;
}

