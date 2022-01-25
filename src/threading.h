// =================================================================================================
// blc
//
// File:   threading.h
// Author: Martin Dorazil
// Date:   25/01/2022
//
// Copyright 2022 Martin Dorazil
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
// =================================================================================================

#ifndef BL_THREADING_H
#define BL_THREADING_H

#include "config.h"
#if BL_PLATFORM_MACOS
#include <pthread.h>
// Apple pthread implementation is missing spinlocks!
#include <stdatomic.h>

typedef atomic_flag pthread_spinlock_t;

static inline int pthread_spin_init(pthread_spinlock_t *l, int pshared)
{
    (void)pshared;
    *l = (pthread_spinlock_t)ATOMIC_FLAG_INIT;
    return 0;
}

static inline int pthread_spin_destroy(pthread_spinlock_t *l)
{
    (void)l;
    return 0;
}

static inline int pthread_spin_lock(pthread_spinlock_t *l)
{
    while (atomic_flag_test_and_set_explicit(l, memory_order_acquire))
        ;
    return 0;
}

static inline int pthread_spin_unlock(pthread_spinlock_t *l)
{
    atomic_flag_clear_explicit(l, memory_order_release);
    return 0;
}
#elif BL_PLATFORM_WIN
// clang-format off
#if BL_COMPILER_CLANG || BL_COMPILER_GNUC
_Pragma("GCC diagnostic push")
_Pragma("GCC diagnostic ignored \"-Wcast-qual\"")
_Pragma("GCC diagnostic ignored \"-Wpedantic\"")
_Pragma("GCC diagnostic ignored \"-Wsign-conversion\"")
#elif BL_COMPILER_MSVC
__pragma(warning(push, 0))
#endif

#include <winpthreads.h>

#if BL_COMPILER_CLANG || BL_COMPILER_GNUC
_Pragma("GCC diagnostic pop")
#elif BL_COMPILER_MSVC
__pragma(warning(pop))
#endif
// clang-format on
#elif BL_PLATFORM_LINUX
#include <pthread.h>
#endif
#endif
