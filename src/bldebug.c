//************************************************************************************************
// blc
//
// File:   bldebug.c
// Author: Martin Dorazil
// Date:   26.1.18
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

#include "bldebug.h"
#include "common.h"
#include <stdarg.h>

#if defined(BL_PLATFORM_WIN)
// clang-format off
#include <windows.h>
#include <DbgHelp.h>
// clang-format on
#endif

#define MAX_LOG_MSG_SIZE 2048

void _log(bl_log_msg_type_e t, const char *file, s32 line, const char *msg, ...)
{
    char    buffer[MAX_LOG_MSG_SIZE];
    va_list args;
    va_start(args, msg);
    vsnprintf(buffer, MAX_LOG_MSG_SIZE, msg, args);

    switch (t) {
    case LOG_ASSERT:
        color_print(stderr, BL_RED, "assert [%s:%d]: %s", file, line, buffer);
        break;
    case LOG_ABORT:
        color_print(stderr, BL_RED, "abort [%s:%d]: %s", file, line, buffer);
        break;
    case LOG_WARNING:
        color_print(stderr, BL_YELLOW, "warning [%s:%d]: %s", file, line, buffer);
        break;
    case LOG_MSG:
        fprintf(stdout, "log [%s:%d]: %s\n", file, line, buffer);
        break;
    default:
        break;
    }

    va_end(args);
}

void print_trace(void)
{
#if defined(BL_PLATFORM_MACOS) || defined(BL_PLATFORM_LINUX)
#include <execinfo.h>
    void * tmp[32];
    usize  size;
    char **strings;
    usize  i;

    size    = backtrace(tmp, TARRAY_SIZE(tmp));
    strings = backtrace_symbols(tmp, size);

    printf("Obtained stack trace:\n");

    for (i = 1; i < size; i++)
        printf("%s\n", strings[i]);

    free(strings);
#elif defined(BL_PLATFORM_WIN)
    HANDLE process = GetCurrentProcess();
    HANDLE thread  = GetCurrentThread();

    CONTEXT context;
    memset(&context, 0, sizeof(CONTEXT));
    context.ContextFlags = CONTEXT_FULL;
    RtlCaptureContext(&context);

    SymInitialize(process, NULL, TRUE);

    DWORD        image;
    STACKFRAME64 stackframe;
    ZeroMemory(&stackframe, sizeof(STACKFRAME64));

#ifdef _M_IX86
    image                       = IMAGE_FILE_MACHINE_I386;
    stackframe.AddrPC.Offset    = context.Eip;
    stackframe.AddrPC.Mode      = AddrModeFlat;
    stackframe.AddrFrame.Offset = context.Ebp;
    stackframe.AddrFrame.Mode   = AddrModeFlat;
    stackframe.AddrStack.Offset = context.Esp;
    stackframe.AddrStack.Mode   = AddrModeFlat;
#elif _M_X64
    image                       = IMAGE_FILE_MACHINE_AMD64;
    stackframe.AddrPC.Offset    = context.Rip;
    stackframe.AddrPC.Mode      = AddrModeFlat;
    stackframe.AddrFrame.Offset = context.Rsp;
    stackframe.AddrFrame.Mode   = AddrModeFlat;
    stackframe.AddrStack.Offset = context.Rsp;
    stackframe.AddrStack.Mode   = AddrModeFlat;
#elif _M_IA64
    image                        = IMAGE_FILE_MACHINE_IA64;
    stackframe.AddrPC.Offset     = context.StIIP;
    stackframe.AddrPC.Mode       = AddrModeFlat;
    stackframe.AddrFrame.Offset  = context.IntSp;
    stackframe.AddrFrame.Mode    = AddrModeFlat;
    stackframe.AddrBStore.Offset = context.RsBSP;
    stackframe.AddrBStore.Mode   = AddrModeFlat;
    stackframe.AddrStack.Offset  = context.IntSp;
    stackframe.AddrStack.Mode    = AddrModeFlat;
#endif

    printf("Obtained stack trace:\n");
    for (size_t i = 0; i < 25; i++) {

        BOOL result = StackWalk64(image,
                                  process,
                                  thread,
                                  &stackframe,
                                  &context,
                                  NULL,
                                  SymFunctionTableAccess64,
                                  SymGetModuleBase64,
                                  NULL);

        if (!result) {
            break;
        }

        char         buffer[sizeof(SYMBOL_INFO) + MAX_SYM_NAME * sizeof(TCHAR)];
        PSYMBOL_INFO symbol  = (PSYMBOL_INFO)buffer;
        symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
        symbol->MaxNameLen   = MAX_SYM_NAME;

        DWORD64 displacement = 0;
        if (SymFromAddr(process, stackframe.AddrPC.Offset, &displacement, symbol)) {
            printf("    %s\n", symbol->Name);
        } else {
            printf("    ???\n");
        }
    }

    printf("\n");

    SymCleanup(process);
#endif
}

void *_assert_invalid_expr(const char *expr, const char *file, s32 line)
{
    _log(LOG_ASSERT, file, line, "Required pointer '%s' is NULL.", expr);
    print_trace();
    abort();
}
