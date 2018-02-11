//*****************************************************************************
// bl
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
//*****************************************************************************

#include "bl/token_printer.h"
#include "bl/pipeline/stage.h"
#include "bl/bldebug.h"
#include "bl/unit.h"

/* class TokenPrinter */
static bool
run(TokenPrinter *self,
    Unit       *unit);

/* class TokenPrinter constructor params */
bo_decl_params_with_base_begin(TokenPrinter, Stage)
  /* constructor params */
  FILE *out_stream;
bo_end();

/* class TokenPrinter object members */
bo_decl_members_begin(TokenPrinter, Stage)
  /* members */
  FILE *out_stream;
bo_end();

bo_impl_type(TokenPrinter, Stage);

void
TokenPrinterKlass_init(TokenPrinterKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run 
    = (bool (*)(Stage*, Actor *)) run;
}

void
TokenPrinter_ctor(TokenPrinter *self, TokenPrinterParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Stage, p);
  self->out_stream = p->out_stream;
}

void
TokenPrinter_dtor(TokenPrinter *self)
{
}

bo_copy_result
TokenPrinter_copy(TokenPrinter *self, TokenPrinter *other)
{
  return BO_NO_COPY;
}
/* class TokenPrinter end */

bool
run(TokenPrinter *self,
    Unit         *unit)
{
  Tokens *tokens = bl_unit_get_tokens(unit);
  if (tokens == NULL) {
    bl_actor_error((Actor *)unit, "cannot find tokens array in unit %s", bl_unit_get_src_file(unit));
    return false;
  }

  BArray *tokens_arr = bl_tokens_get_all(tokens);

  fprintf(self->out_stream, ANSI_COLOR_YELLOW "Tokens: \n" ANSI_COLOR_RESET);

  const size_t c = bo_array_size(tokens_arr);
  bl_token_t *tok;
  int line = -1;
  for (size_t i = 0; i < c; i++) {
    tok = &bo_array_at(tokens_arr, i, bl_token_t);

    if (line == -1)
      line = tok->line;
    else if (tok->line != line) {
      line = tok->line;
      fprintf(self->out_stream, "\n");
    }

    fprintf(self->out_stream,
            ANSI_COLOR_YELLOW "['%s' %i:%i], " ANSI_COLOR_RESET,
            bl_sym_strings[tok->sym],
            tok->line,
            tok->col
    );
  }

  fprintf(self->out_stream, "\n");

  return true;
}

TokenPrinter *
bl_token_printer_new(FILE              *out_stream,
                     bl_compile_group_e group)
{
  TokenPrinterParams p = {
    .base.group = group,
    .out_stream = out_stream
  };

  return bo_new(TokenPrinter, &p);
}

