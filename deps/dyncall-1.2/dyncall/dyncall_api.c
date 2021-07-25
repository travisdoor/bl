/*

 Package: dyncall
 Library: dyncall
 File: dyncall/dyncall_api.c
 Description: C interface to call vm
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



#include "dyncall.h"
#include "dyncall_callvm.h"

void dcReset(DCCallVM* vm)
{ 
  vm->mVTpointer->reset(vm); 
}

void dcFree(DCCallVM* vm) 
{ 
  vm->mVTpointer->free(vm); 
}

void dcMode(DCCallVM* vm,DCint mode) 
{ 
  vm->mVTpointer->mode(vm,mode);
}

void dcArgBool(DCCallVM* vm,DCbool x) 
{ 
  vm->mVTpointer->argBool(vm, x); 
}

void dcArgChar(DCCallVM* vm,DCchar x)
{ 
  vm->mVTpointer->argChar(vm, x); 
}

void dcArgShort(DCCallVM* vm,DCshort x) 
{ 
  vm->mVTpointer->argShort(vm, x); 
}

void dcArgInt(DCCallVM* vm,DCint x) 
{ 
  vm->mVTpointer->argInt(vm, x); 
}

void dcArgLong(DCCallVM* vm,DClong x) 
{ 
  vm->mVTpointer->argLong(vm, x); 
}

void dcArgLongLong(DCCallVM* vm, DClonglong x) 
{ 
  vm->mVTpointer->argLongLong(vm, x); 
}

void dcArgFloat(DCCallVM* vm, DCfloat x) 
{ 
  vm->mVTpointer->argFloat(vm, x); 
}

void dcArgDouble(DCCallVM* vm, DCdouble x) 
{ 
  vm->mVTpointer->argDouble(vm, x); 
}

void dcArgPointer(DCCallVM* vm, DCpointer x) 
{ 
  vm->mVTpointer->argPointer(vm, x); 
}

void dcArgStruct(DCCallVM* vm, DCstruct* s, DCpointer x) 
{ 
  vm->mVTpointer->argStruct(vm, s, x); 
}


void dcCallVoid(DCCallVM* vm, DCpointer funcptr) 
{        
  vm->mVTpointer->callVoid(vm, funcptr); 
}

DCchar dcCallChar(DCCallVM* vm, DCpointer funcptr) 
{ 
  return vm->mVTpointer->callChar(vm, funcptr); 
}

DCbool dcCallBool(DCCallVM* vm, DCpointer funcptr) 
{ 
  return vm->mVTpointer->callBool(vm, funcptr); 
}

DCshort dcCallShort(DCCallVM* vm, DCpointer funcptr) 
{ 
  return vm->mVTpointer->callShort(vm, funcptr); 
}

DCint dcCallInt(DCCallVM* vm, DCpointer funcptr) 
{ 
  return vm->mVTpointer->callInt(vm, funcptr); 
}

DClong dcCallLong(DCCallVM* vm, DCpointer funcptr) 
{ 
  return vm->mVTpointer->callLong(vm, funcptr); 
}

DClonglong dcCallLongLong(DCCallVM* vm, DCpointer funcptr) 
{ 
  return vm->mVTpointer->callLongLong(vm, funcptr); 
}

DCfloat dcCallFloat(DCCallVM* vm, DCpointer funcptr) 
{ 
  return vm->mVTpointer->callFloat(vm, funcptr); 
}

DCdouble dcCallDouble(DCCallVM* vm, DCpointer funcptr) 
{ 
  return vm->mVTpointer->callDouble(vm, funcptr); 
}

DCpointer dcCallPointer(DCCallVM* vm, DCpointer funcptr) 
{ 
  return vm->mVTpointer->callPointer(vm, funcptr); 
}

void dcCallStruct(DCCallVM* vm, DCpointer funcptr, DCstruct* s, DCpointer x) 
{ 
  vm->mVTpointer->callStruct(vm, funcptr, s, x); 
}

DCint dcGetError(DCCallVM *vm)
{
  return vm->mError;
}

DCint dcGetModeFromCCSigChar(DCsigchar sig_char)
{
  switch(sig_char)
  {
    case DC_SIGCHAR_CC_DEFAULT:          return DC_CALL_C_DEFAULT;
    case DC_SIGCHAR_CC_ELLIPSIS:         return DC_CALL_C_ELLIPSIS;
    case DC_SIGCHAR_CC_ELLIPSIS_VARARGS: return DC_CALL_C_ELLIPSIS_VARARGS;
    case DC_SIGCHAR_CC_CDECL:            return DC_CALL_C_X86_CDECL;
    case DC_SIGCHAR_CC_STDCALL:          return DC_CALL_C_X86_WIN32_STD;
    case DC_SIGCHAR_CC_FASTCALL_MS:      return DC_CALL_C_X86_WIN32_FAST_MS;
    case DC_SIGCHAR_CC_FASTCALL_GNU:     return DC_CALL_C_X86_WIN32_FAST_GNU;
    case DC_SIGCHAR_CC_THISCALL_MS:      return DC_CALL_C_X86_WIN32_THIS_MS;
    case DC_SIGCHAR_CC_THISCALL_GNU:     return DC_CALL_C_X86_WIN32_THIS_GNU;
    case DC_SIGCHAR_CC_ARM_ARM:          return DC_CALL_C_ARM_ARM;
    case DC_SIGCHAR_CC_ARM_THUMB:        return DC_CALL_C_ARM_THUMB;
    case DC_SIGCHAR_CC_SYSCALL:          return DC_CALL_SYS_DEFAULT;
  }
  return DC_ERROR_UNSUPPORTED_MODE;
}

