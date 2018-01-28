//*****************************************************************************
// bl
//
// File:   snippets.h
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

#ifndef SNIPPETS_H_CE4ZVWQN
#define SNIPPETS_H_CE4ZVWQN

static const char *bl_snipp_inclh =
  "#include <%s>\n";

// static %type %prefix%name;
static const char *bl_snipp_var_decl =
  "%.*s %s%.*s;\n";

// static %type %prefix%name(void);
static const char *bl_snipp_method_decl =
  "%.*s %s%.*s(void);\n";

static const char *bl_snipp_method_impl =
  "%.*s %s%.*s(void) {\n";

// c main method 
static const char *bl_snipp_main =
  "int main (int argc, char **argv) {\n  %s%.*s(); \n  return 0; \n}\n";

#endif /* end of include guard: SNIPPETS_H_CE4ZVWQN */
