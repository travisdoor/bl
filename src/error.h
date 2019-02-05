//************************************************************************************************
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
//************************************************************************************************

#ifndef BL_ERROR_H
#define BL_ERROR_H

#include "config.h"

typedef enum
{
  NO_ERR = 0,

  ERR_FILE_NOT_FOUND          = 1,
  ERR_INVALID_SOURCE          = 2,
  ERR_INVALID_TOKEN           = 3,
  ERR_UNTERMINATED_COMMENT    = 4,
  ERR_UNTERMINATED_STRING     = 5,
  ERR_MISSING_SEMICOLON       = 6,
  ERR_MISSING_BRACKET         = 7,
  ERR_UNEXPECTED_DECL         = 8,
  ERR_EXPECTED_EXPR           = 9,
  ERR_MISSING_COMMA           = 10,
  ERR_EXPECTED_BODY           = 11,
  ERR_EXPECTED_BODY_END       = 12,
  ERR_EXPECTED_STMT           = 13,
  ERR_BREAK_OUTSIDE_LOOP      = 14,
  ERR_CONTINUE_OUTSIDE_LOOP   = 15,
  ERR_UNEXPECTED_MODIF        = 16,
  ERR_DUPLICATE_SYMBOL        = 17,
  ERR_UNKNOWN_SYMBOL          = 18,
  ERR_EXPECTED_TYPE           = 19,
  ERR_EXPECTED_NAME           = 20,
  ERR_EXPECTED_BINOP          = 21,
  ERR_DUPLICATE_ENTRY         = 22,
  ERR_NOT_VERIFIED            = 23,
  ERR_CANNOT_WRITE_BC         = 24,
  ERR_CANNOT_LINK             = 25,
  ERR_DIFF_KIND_OF_SYMBOL     = 26,
  ERR_INVALID_PARAM_COUNT     = 27,
  ERR_UNKNOWN_TYPE            = 28,
  ERR_PRIVATE                 = 29,
  ERR_UNCOMPATIBLE_MODIF      = 30,
  ERR_NO_MAIN_METHOD          = 31,
  ERR_INVALID_RESULT          = 32,
  ERR_INVALID_ARG_COUNT       = 33,
  ERR_INVALID_ARG_TYPE        = 34,
  ERR_INVALID_TYPE            = 35,
  ERR_INVALID_EXPR            = 36,
  ERR_JIT_RUN_FAILED          = 37,
  ERR_EXPECTED_STRING         = 38,
  ERR_EXPECTED_MODULE         = 39,
  ERR_EXPECTED_FUNC           = 40,
  ERR_MISSING_RETURN          = 41,
  ERR_EMPTY                   = 42,
  ERR_UNEXPECTED_SYMBOL       = 43,
  ERR_MULTIPLE_MAIN           = 44,
  ERR_EXPECTED_INITIALIZATION = 45,
  ERR_EXPECTED_TYPE_REF       = 46,
  ERR_EXPECTED_TYPE_STRUCT    = 47,
  ERR_INVALID_MUTABILITY      = 48,
  ERR_INVALID_NAME            = 49,
  ERR_INVALID_MEMBER_ACCESS   = 50,
  ERR_UNIMPLEMENTED           = 51,
  ERR_EXPECTED_CONST          = 52,
  ERR_EXPECTED_DECL           = 53,
  ERR_FILE_READ               = 54,
  ERR_FILE_EMPTY              = 55,
  ERR_INVALID_INITIALIZER     = 56,
  ERR_INVALID_CAST            = 57,
  ERR_INVALID_ADM             = 58,
  ERR_DIV_BY_ZERO             = 59,
  ERR_LIB_NOT_FOUND           = 60,
  ERR_LIB_SYMBOL_NOT_FOUND    = 61,
  ERR_EXPECTED_TEST_DESC      = 62,
  ERR_NULL_POINTER            = 63,
  ERR_INVALID_ARR_SIZE        = 64,
} Error;

#endif // BL_ERROR_H
