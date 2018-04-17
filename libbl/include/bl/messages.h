//************************************************************************************************
// bl
//
// File:   messages.h
// Author: Martin Dorazil
// Date:   13/04/2018
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

#ifndef BL_MESSAGES_H
#define BL_MESSAGES_H

#include <bobject/bobject.h>

BO_BEGIN_DECLS

#define bl_msg_log(format, ...)                                                                    \
  {                                                                                                \
    fprintf(stdout, BL_GREEN(format) "\n", ##__VA_ARGS__);                                         \
  }

#define bl_msg_error(format, ...)                                                                  \
  {                                                                                                \
    fprintf(stderr, BL_RED("error: ") format "\n", ##__VA_ARGS__);                                 \
  }

#define bl_msg_warning(format, ...)                                                                \
  {                                                                                                \
    fprintf(stdout, BL_YELLOW("warning: ") format "\n", ##__VA_ARGS__);                            \
  }

BO_END_DECLS

#endif // BL_MESSAGES_H
