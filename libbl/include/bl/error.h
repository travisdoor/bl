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

typedef enum
{
  BL_NO_ERR = 0,

  BL_ERR_FILE_NOT_FOUND = 100,
  BL_ERR_INVALID_SOURCE = 101,

  BL_ERR_INVALID_TOKEN        = 200,
  BL_ERR_UNTERMINATED_COMMENT = 201,
  BL_ERR_UNTERMINATED_STRING  = 202,

  BL_ERR_MISSING_SEMICOLON     = 300,
  BL_ERR_MISSING_BRACKET       = 301,
  BL_ERR_UNEXPECTED_DECL       = 302,
  BL_ERR_EXPECTED_EXPR         = 303,
  BL_ERR_MISSING_COMMA         = 304,
  BL_ERR_EXPECTED_BODY         = 305,
  BL_ERR_EXPECTED_BODY_END     = 306,
  BL_ERR_EXPECTED_STMT         = 307,
  BL_ERR_BREAK_OUTSIDE_LOOP    = 308,
  BL_ERR_CONTINUE_OUTSIDE_LOOP = 309,
  BL_ERR_UNEXPECTED_MODIF      = 310,
  BL_ERR_DUPLICATE_SYMBOL      = 311,
  BL_ERR_UNKNOWN_SYMBOL        = 312,
  BL_ERR_EXPECTED_TYPE         = 313,
  BL_ERR_EXPECTED_NAME         = 314,

  BL_ERR_NOT_VERIFIED = 400,

  BL_ERR_CANNOT_WRITE_BC = 500,

  BL_ERR_CANNOT_LINK = 600,

  BL_ERR_NO_MAIN_METHOD = 700,
  BL_ERR_INVALID_RESULT = 701

} bl_error_e;

BO_END_DECLS

#endif //BL_ERROR_H
