//*****************************************************************************
// Biscuit Object
//
// File:   bdebug.c
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
//
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include "bdebug_impl.h"

#define MAX_LOG_MSG_SIZE 256

static void
default_error_handler(const char *msg)
{
  fprintf(stderr, "%s\n", msg);
}

static bo_err_f error = default_error_handler;

void
_error(const char *format, ...)
{
  char buffer[MAX_LOG_MSG_SIZE];
  va_list args;
  va_start(args, format);
  vsnprintf(buffer, MAX_LOG_MSG_SIZE, format, args);

  error(buffer);

  va_end(args);
}

void
bo_set_err_func(bo_err_f func)
{
  error = func;
}
