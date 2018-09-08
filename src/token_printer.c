//************************************************************************************************
// blc
//
// File:   token_printer.c
// Author: Martin Dorazil
// Date:   6.2.18
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

#include "stages.h"
#include "common.h"

void
token_printer_run(Unit *unit)
{
  BArray *tokens_arr = unit->tokens.buf;

  fprintf(stdout, "Tokens: \n");

  const size_t c = bo_array_size(tokens_arr);
  Token *    tok;
  int          line = -1;
  for (size_t i = 0; i < c; ++i) {
    tok = &bo_array_at(tokens_arr, i, Token);

    if (line == -1) {
      line = tok->src.line;
      fprintf(stdout, "%d: ", line);
    } else if (tok->src.line != line) {
      line = tok->src.line;
      fprintf(stdout, "\n%d: ", line);
    }

    fprintf(stdout, "[" YELLOW("'%s'") " %i:%i], ", sym_strings[tok->sym], tok->src.line,
            tok->src.col);
  }

  fprintf(stdout, "\n");
}
