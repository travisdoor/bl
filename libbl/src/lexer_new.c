//*****************************************************************************
// bl
//
// File:   lexer_new.c
// Author: Martin Dorazil
// Date:   19/02/2018
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

#include <string.h>
#include "bl/lexer_new.h"
#include "bl/unit.h"
#include "bl/bldebug.h"

/* class LexerNew */
static bool
run(LexerNew *self,
    Unit *unit);

static void
scan(char *c);

/* class LexerNew constructor params */
bo_decl_params_with_base_begin(LexerNew, Stage)
  /* constructor params */
bo_end();

/* class LexerNew object members */
bo_decl_members_begin(LexerNew, Stage)
  /* members */
bo_end();

bo_impl_type(LexerNew, Stage);

void
LexerNewKlass_init(LexerNewKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run =
    (bool (*)(Stage *,
              Actor *)) run;
}

void
LexerNew_ctor(LexerNew *self,
              LexerNewParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Stage, p);

  /* initialize self */
}

void
LexerNew_dtor(LexerNew *self)
{
}

bo_copy_result
LexerNew_copy(LexerNew *self,
              LexerNew *other)
{
  return BO_NO_COPY;
}

/* class LexerNew end */

void
scan(char *c)
{
  size_t len = 0;
  bl_sym_e sym = BL_SYM_NONE;

  /*
   * Scan other symbols described directly in as strings.
   */
  for (int i = BL_SYM_RETURN; i < BL_SYM_NONE; i++) {
    len = strlen(bl_sym_strings[i]);
    if (strncmp(c, bl_sym_strings[i], len) == 0) {
      sym = (bl_sym_e) i;
    }
  }

  switch (sym) {
    case BL_SYM_LCOMMENT:
      /* begin of line comment */
      break;
    case BL_SYM_LBCOMMENT:
      /* begin of block comment */
      break;
    default:
      /* other cases, string, number, identificator, ... */
      break;
  }

  /* When symbol is unknown report error */
}

bool
run(LexerNew *self,
    Unit *unit)
{
  Tokens *tokens = bl_tokens_new();
  bl_unit_set_tokens(unit, tokens);

  char *src = (char *) bl_unit_get_src(unit);

  bl_log(BL_GREEN("processing unit: %s"), bl_unit_get_name(unit));

  if (src == NULL) {
    bl_actor_error((Actor *) unit,
                   "No source loaded for unit " BL_YELLOW("'%s'") ", use builder flag "
                     BL_YELLOW("BL_BUILDER_LOAD_FROM_FILE") " or create unit from loaded source.",
                   bl_unit_get_name(unit));
    return false;
  }

  scan(src);

  return true;
}

/* public */
LexerNew *
bl_lexer_new_new(bl_compile_group_e group)
{
  LexerNewParams p = {.base.group = group};

  return bo_new(LexerNew, &p);
}

