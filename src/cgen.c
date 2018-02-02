//*****************************************************************************
// bl
//
// File:   cgen.c
// Author: Martin Dorazil
// Date:   31/01/2018
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

#include "cgen.h"
#include "bldebug.h"
#include "token.h"

#define GLOBAL_PREFIX "g"

static const char *header_comment = "/* Biscuit generated file, do not modify. */\n";
static const char *header_defaults = 
  "#include <stdlib.h>\n#include <bobject/bobject.h>\n#include <bobject/containers/string.h>\n";

CSrc *
bl_cgen_generate(Unit *unit,
                 PNode *pnode)
{
  CSrc *csrc = bl_csrc_new();
  return csrc;
}
