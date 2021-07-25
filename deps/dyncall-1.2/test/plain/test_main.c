/*

 Package: dyncall
 Library: test
 File: test/plain/test_main.c
 Description:
 License:

   Copyright (c) 2007-2019 Daniel Adler <dadler@uni-goettingen.de>,
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




#include "../../dyncall/dyncall.h"
#include "../common/platformInit.h"
#include "../common/platformInit.c" /* Impl. for functions only used in this translation unit */


/* -------------------------------------------------------------------------
 * test: identity function calls
 * ------------------------------------------------------------------------- */

#define DEF_FUNCS(API,NAME) \
void       API fun_##NAME##_v()             { g_void_testval = 1; } \
DCbool     API fun_##NAME##_b(DCbool x)     { return x; } \
DCint      API fun_##NAME##_i(DCint x)      { return x; } \
DClong     API fun_##NAME##_j(DClong x)     { return x; } \
DClonglong API fun_##NAME##_l(DClonglong x) { return x; } \
DCfloat    API fun_##NAME##_f(DCfloat x)    { return x; } \
DCdouble   API fun_##NAME##_d(DCdouble x)   { return x; } \
DCpointer  API fun_##NAME##_p(DCpointer  x) { return x; }

/* __cdecl */

#if !defined(DC__OS_Win32)
#  define __cdecl
#endif

int g_void_testval;
DEF_FUNCS(__cdecl,c)

int testCallC()
{
  int ret = 1;

  DCCallVM* pc = dcNewCallVM(4096);
  dcMode(pc,DC_CALL_C_DEFAULT);
  /* void */
  dcReset(pc);
  g_void_testval = 0;
  dcCallVoid(pc, (DCpointer) &fun_c_v);
  ret = g_void_testval && ret;
  /* bool */
  {
    DCbool r, val=DC_TRUE;
    dcReset(pc);
    dcArgBool(pc, val);
    r = dcCallBool(pc, (DCpointer) &fun_c_b);
    printf("bt (cdecl): %d\n", (r == val));
	ret = (r == val) && ret;

    val=DC_FALSE;
    dcReset(pc);
    dcArgBool(pc, val);
    r = dcCallBool(pc, (DCpointer) &fun_c_b);
    printf("bf (cdecl): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* int */
  {
    DCint r, val=1234;
    dcReset(pc);
    dcArgInt(pc, val);
    r = dcCallInt(pc, (DCpointer) &fun_c_i);
    printf("i  (cdecl): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* long */
  {
    DClong r, val=(DClong) 0xCAFEBABEL;
    dcReset(pc);
    dcArgLong(pc, val);
    r = dcCallLong(pc, (DCpointer) &fun_c_j);
    printf("l  (cdecl): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* long long */
  {
    DClonglong r, val=(DClonglong) 0xCAFEBABEDEADC0DELL;
    dcReset(pc);
    dcArgLongLong(pc, val);
    r = dcCallLongLong(pc, (DCpointer) &fun_c_l);
    printf("ll (cdecl): %d\n", (r == (DClonglong)val));
	ret = (r == (DClonglong)val) && ret;
  }
  /* float */
  {
    DCfloat r, val=1.234567f;
    dcReset(pc);
    dcArgFloat(pc, val);
    r = dcCallFloat(pc, (DCpointer) &fun_c_f);
    printf("f  (cdecl): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* double */
  {
    DCdouble r, val=1.23456789;
    dcReset(pc);
    dcArgDouble(pc, val);
    r = dcCallDouble(pc, (DCpointer) &fun_c_d);
    printf("d  (cdecl): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* ptr */
  {
    DCpointer r;
    dcReset(pc);
    dcArgPointer(pc, (DCpointer) &fun_c_b);
    r = dcCallPointer(pc, (DCpointer) &fun_c_p);
    printf("p  (cdecl): %d\n", (r == (DCpointer) &fun_c_b));
	ret = (r == (DCpointer) &fun_c_b) && ret;
  }
  dcFree(pc);

  return ret;
}


#if defined(DC__OS_Win32)
/* win32 __stdcall */

DEF_FUNCS(__stdcall,std)

int testCallStd()
{
  int ret = 1;

  DCCallVM* pc = dcNewCallVM(4096);
  dcMode(pc,DC_CALL_C_X86_WIN32_STD);
  /* void */
  dcReset(pc);
  g_void_testval = 0;
  dcCallVoid(pc, (DCpointer) &fun_std_v);
  ret = g_void_testval && ret;
  /* bool */
  {
    DCbool r, val=DC_TRUE;
    dcReset(pc);
    dcArgBool(pc, val);
    r = dcCallBool(pc, (DCpointer) &fun_std_b);
    printf("bt (stdcall): %d\n", (r == val));
	ret = (r == val) && ret;

    val=DC_FALSE;
    dcReset(pc);
    dcArgBool(pc, val);
    r = dcCallBool(pc, (DCpointer) &fun_std_b);
    printf("bf (stdcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* int */
  {
    DCint r, val=1234;
    dcReset(pc);
    dcArgInt(pc, val);
    r = dcCallInt(pc, (DCpointer) &fun_std_i);
    printf("i  (stdcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* long */
  {
    DClong r, val=0xCAFEBABEUL;
    dcReset(pc);
    dcArgLong(pc, val);
    r = dcCallLong(pc, (DCpointer) &fun_std_j);
    printf("l  (stdcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* long long */
  {
    DClonglong r, val=0xCAFEBABEDEADC0DEULL;
    dcReset(pc);
    dcArgLongLong(pc, val);
    r = dcCallLongLong(pc, (DCpointer) &fun_std_l);
    printf("ll (stdcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* float */
  {
    DCfloat r, val=1.234567f;
    dcReset(pc);
    dcArgFloat(pc, val);
    r = dcCallFloat(pc, (DCpointer) &fun_std_f);
    printf("f  (stdcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* double */
  {
    DCdouble r, val=1.23456789;
    dcReset(pc);
    dcArgDouble(pc, val);
    r = dcCallDouble(pc, (DCpointer) &fun_std_d);
    printf("d  (stdcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* ptr */
  {
    DCpointer r;
    dcReset(pc);
    dcArgPointer(pc, (DCpointer) &fun_c_b);
    r = dcCallPointer(pc, (DCpointer) &fun_std_p);
    printf("p  (stdcall): %d\n", (r == &fun_c_b));
	ret = (r == &fun_c_b) && ret;
  }
  dcFree(pc);

  return ret;
}

#endif


#if defined(DC__OS_Win32)
/* win32 __fastcall */

DEF_FUNCS(__fastcall,fast)

int testCallFast()
{
  int ret = 1;

  DCCallVM* pc = dcNewCallVM(4096);
#ifdef DC__C_GNU
# define FT "GNU"
  dcMode(pc,DC_CALL_C_X86_WIN32_FAST_GNU);
#else
# define FT "MS"
  dcMode(pc,DC_CALL_C_X86_WIN32_FAST_MS);
#endif
  /* void */
  dcReset(pc);
  g_void_testval = 0;
  dcCallVoid(pc, (DCpointer) &fun_fast_v);
  ret = g_void_testval && ret;
  /* bool */
  {
    DCbool r, val=DC_TRUE;
    dcReset(pc);
    dcArgBool(pc, val);
    r = dcCallBool(pc, (DCpointer) &fun_fast_b);
    printf("bt ("FT"fastcall): %d\n", (r == val));
	ret = (r == val) && ret;

    val=DC_FALSE;
    dcReset(pc);
    dcArgBool(pc, val);
    r = dcCallBool(pc, (DCpointer) &fun_fast_b);
    printf("bf ("FT"fastcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* int */
  {
    DCint r, val=1234;
    dcReset(pc);
    dcArgInt(pc, val);
    r = dcCallInt(pc, (DCpointer) &fun_fast_i);
    printf("i  ("FT"fastcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* long */
  {
    DClong r, val=0xCAFEBABEUL;
    dcReset(pc);
    dcArgLong(pc, val);
    r = dcCallLong(pc, (DCpointer) &fun_fast_j);
    printf("l  ("FT"fastcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* long long */
  {
    DClonglong r, val=0xCAFEBABEDEADC0DEULL;
    dcReset(pc);
    dcArgLongLong(pc, val);
    r = dcCallLongLong(pc, (DCpointer) &fun_fast_l);
    printf("ll ("FT"fastcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* float */
  {
    DCfloat r, val=1.234567f;
    dcReset(pc);
    dcArgFloat(pc, val);
    r = dcCallFloat(pc, (DCpointer) &fun_fast_f);
    printf("f  ("FT"fastcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* double */
  {
    DCdouble r, val=1.23456789;
    dcReset(pc);
    dcArgDouble(pc, val);
    r = dcCallDouble(pc, (DCpointer) &fun_fast_d);
    printf("d  ("FT"fastcall): %d\n", (r == val));
	ret = (r == val) && ret;
  }
  /* ptr */
  {
    DCpointer r;
    dcReset(pc);
    dcArgPointer(pc, (DCpointer) &fun_c_b);
    r = dcCallPointer(pc, (DCpointer) &fun_fast_p);
    printf("p  ("FT"fastcall): %d\n", (r == &fun_c_b));
	ret = (r == &fun_c_b) && ret;
  }
  dcFree(pc);

  return ret;
}

#endif


int testCallStructs();
int testStructSizes();

int main(int argc, char* argv[])
{
  int r = 1;
  dcTest_initPlatform();

  r = testCallC() && r;
  r = testStructSizes() && r;
  /*r = testCallStructs() && r;*/
#if defined(DC__OS_Win32)
  r = testCallStd() && r;
  r = testCallFast() && r;
#endif

  printf("result: plain: %d\n", r);

  dcTest_deInitPlatform();

  return !r;
}

