//*****************************************************************************
// Biscuit Object
//
// File:   utils.h
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

#ifndef UTILS_H_EYRLMPBK
#define UTILS_H_EYRLMPBK

#ifdef _MSC_VER
#if BO_COMPILING_DLL
#define BO_EXPORT __declspec(dllexport)
#else
#define BO_EXPORT
#endif
#else
#define BO_EXPORT __attribute__((__visibility__("default")))
#endif

#ifdef __cplusplus
#define BO_BEGIN_DECLS extern "C" {
#define BO_END_DECLS }
#else
#define BO_BEGIN_DECLS
#define BO_END_DECLS
#endif

#endif /* end of include guard: UTILS_H_EYRLMPBK */
