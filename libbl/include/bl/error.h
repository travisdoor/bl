//*****************************************************************************
// bl
//
// File:   error.h
// Author: Martin Dorazil
// Date:   03/03/2018
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
//*****************************************************************************

#ifndef BL_ERROR_H
#define BL_ERROR_H

#include <bobject/bobject.h>

BO_BEGIN_DECLS

typedef enum {
  BL_NO_ERR = 0,

  BL_ERR_FILE_NOT_FOUND        = 1,
  BL_ERR_INVALID_SOURCE        = 2,
  BL_ERR_INVALID_TOKEN         = 3,
  BL_ERR_UNTERMINATED_COMMENT  = 4,
  BL_ERR_UNTERMINATED_STRING   = 5,
  BL_ERR_MISSING_SEMICOLON     = 6,
  BL_ERR_MISSING_BRACKET       = 7,
  BL_ERR_UNEXPECTED_DECL       = 8,
  BL_ERR_EXPECTED_EXPR         = 9,
  BL_ERR_MISSING_COMMA         = 10,
  BL_ERR_EXPECTED_BODY         = 11,
  BL_ERR_EXPECTED_BODY_END     = 12,
  BL_ERR_EXPECTED_STMT         = 13,
  BL_ERR_BREAK_OUTSIDE_LOOP    = 14,
  BL_ERR_CONTINUE_OUTSIDE_LOOP = 15,
  BL_ERR_UNEXPECTED_MODIF      = 16,
  BL_ERR_DUPLICATE_SYMBOL      = 17,
  BL_ERR_UNKNOWN_SYMBOL        = 18,
  BL_ERR_EXPECTED_TYPE         = 19,
  BL_ERR_EXPECTED_NAME         = 20,
  BL_ERR_EXPECTED_BINOP        = 21,
  BL_ERR_DUPLICATE_ENTRY       = 22,
  BL_ERR_NOT_VERIFIED          = 23,
  BL_ERR_CANNOT_WRITE_BC       = 24,
  BL_ERR_CANNOT_LINK           = 25,
  BL_ERR_DIFF_KIND_OF_SYMBOL   = 26,
  BL_ERR_INVALID_PARAM_COUNT   = 27,
  BL_ERR_UNKNOWN_TYPE          = 28,
  BL_ERR_PRIVATE               = 29,
  BL_ERR_UNCOMPATIBLE_MODIF    = 30,
  BL_ERR_NO_MAIN_METHOD        = 31,
  BL_ERR_INVALID_RESULT        = 32,
  BL_ERR_INVALID_ARG_COUNT     = 33,
  BL_ERR_INVALID_ARG_TYPE      = 34,
  BL_ERR_INVALID_TYPE          = 35,
} bl_error_e;

BO_END_DECLS

#endif // BL_ERROR_H
