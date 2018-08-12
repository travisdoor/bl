//************************************************************************************************
// bl
//
// File:   config.h
// Author: Martin Dorazil
// Date:   3/12/18
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

#ifndef BL_CONFIG_H
#define BL_CONFIG_H

#ifdef _WIN32
#define BL_PLATFORM_WIN
#define BL_NO_COLOR
#elif __APPLE__
#define BL_PLATFORM_MACOS
#elif __linux__
#define BL_PLATFORM_LINUX
#elif __CYGWIN__
#define BL_PLATFORM_WIN
#else
#error "Unknown platform"
#endif

#ifdef __clang__
#define BL_COMPILER_CLANG
#elif __GNUC__
#define BL_COMPILER_GNUC
#elif __MINGW32__
#define BL_COMPILER_GNUC
#elif _MSC_VER
#define BL_COMPILER_MSVC
#endif

#define BL_MAX_FUNC_ARG_COUNT 32
#define BL_MAX_FUNC_NAME_LEN  512

#if BL_DEBUG
#define BL_ASSERT_ON_CMP_ERROR 0
#else
#define BL_ASSERT_ON_CMP_ERROR 0
#endif

#ifdef BL_COMPILER_MSVC 
#if BL_COMPILING_DLL
#define BL_EXPORT __declspec(dllexport)
#else
#define BL_EXPORT __declspec(dllimport)
#endif
#else
#define BL_EXPORT __attribute__((__visibility__("default")))
#endif

#ifdef __cplusplus
#define BL_BEGIN_DECLS extern "C" {
#define BL_END_DECLS }
#else
#define BL_BEGIN_DECLS
#define BL_END_DECLS
#endif

#ifdef BL_COMPILER_MSVC
#include <Windows.h>
#endif

#define ENV_PATH "PATH"
#ifndef PATH_MAX 
#define PATH_MAX 1024
#endif

#ifdef BL_PLATFORM_WIN
#define BL_PATH_SEPARATOR "\\"
#define BL_PATH_SEPARATORC '\\'
#define BL_ENVPATH_SEPARATOR ';'
#else
#define BL_PATH_SEPARATOR "/"
#define BL_PATH_SEPARATORC '/'
#define BL_ENVPATH_SEPARATOR ':'
#endif

#endif // BL_CONFIG_H
