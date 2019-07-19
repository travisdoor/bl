//*****************************************************************************
// Biscuit Object
//
// File:   atomic.h
// Author: Martin Dorazil
// Date:   27/10/2017
//
// Copyright 2017 Martin Dorazil
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
//*****************************************************************************

#ifndef BATOMIC_H_DEL2Y9FQ
#define BATOMIC_H_DEL2Y9FQ

#ifdef _MSC_VER
#include <Windows.h>
#define bo_atomic_add(ex, val) InterlockedExchangeAdd((LONG *)(ex), (val))
#define bo_atomic_sub(ex, val) InterlockedExchangeAdd((LONG *)(ex), -(val))
#define bo_atomic_store(st, val) ((*st) = (val))
#define bo_atomic_load(l) (l)

typedef volatile int bo_atomic_int;
#else
#ifdef __cplusplus
#include <atomic>
using namespace std;
#else
#include <stdatomic.h>
#endif
#define bo_atomic_add(ex, val) atomic_fetch_add((ex), (val))
#define bo_atomic_sub(ex, val) atomic_fetch_sub((ex), (val))
#define bo_atomic_store(st, val) atomic_store((st), (val))
#define bo_atomic_load(l) atomic_load((l))

typedef atomic_int bo_atomic_int;
#endif

#endif /* end of include guard: BATOMIC_H_DEL2Y9FQ */
