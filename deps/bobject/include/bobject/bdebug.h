//*****************************************************************************
// Biscuit Object
//
// File:   bdebug.h
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

#ifndef BDEBUG_H_RGTSFWXL
#define BDEBUG_H_RGTSFWXL

#include "bobject/utils.h"

BO_BEGIN_DECLS

/* error handling */

/* Error function type. */
typedef void (*bo_err_f)(const char *);

/**
 * Client can specify custom method to print error message via this setter.
 *
 * @param func Log function.
 */
extern BO_EXPORT void
bo_set_err_func(bo_err_f func);

BO_END_DECLS

#endif /* end of include guard: BDEBUG_H_RGTSFWXL */
