//*****************************************************************************
// bl
//
// File:   errors.h
// Author: Martin Dorazil
// Date:   26.1.18
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

#ifndef ERRORS_H_IEWPB1RX
#define ERRORS_H_IEWPB1RX

typedef enum _bl_err
{
  BL_ERR_NO_ERROR = 0,
  BL_ERR_FILE_NOT_FOUND,
  BL_ERR_INVALID_SOURCE,

  BL_ERR_UNEXPECTED_SYMBOL,
  BL_ERR_MISSING_SEMICOLON,
  BL_ERR_COUNT
} bl_err;

static const char *bl_err_strings[BL_ERR_COUNT] = {
  "no error",
  "file not found",
  "invalid source file",
  "unexpected symbol",
  "missing semicolon"
};

#endif /* end of include guard: ERRORS_H_IEWPB1RX */
