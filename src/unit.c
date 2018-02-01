//*****************************************************************************
// bl
//
// File:   unit.c
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

#include "unit.h"
#include "tokens.h"
#include "lexer.h"
#include "parser.h"
#include "bldebug.h"
#include "cgen.h"

void log_parsed(Pnode *node, int lpad)
{
  switch (node->type) {
    case BL_PT_GSCOPE:
      printf("%*s[gscope]\n", lpad, "");
      break;
    case BL_PT_EXP:
      printf("%*s[exp]\n", lpad, "");
      break;
    case BL_PT_DECL:
      printf("%*s[decl]\n", lpad, "");
      break;
    case BL_PT_END:
      printf("%*s[end]\n", lpad, "");
      break;
    case BL_PT_TYPE:
      printf("%*s[type]\n", lpad, "");
      break;
    case BL_PT_ID:
      printf("%*s[ident]\n", lpad, "");
      break;
    case BL_PT_FUNC:
      printf("%*s[func]\n", lpad, "");
      break;
    case BL_PT_ARGS:
      printf("%*s[args]\n", lpad, "");
      break;
    case BL_PT_ARG:
      printf("%*s[arg]\n", lpad, "");
      break;
    case BL_PT_SCOPE:
      printf("%*s[scope]\n", lpad, "");
      break;
    case BL_PT_NSCOPE:
      printf("%*s[nscope]\n", lpad, "");
      break;
    case BL_PT_NAMESPACE:
      printf("%*s[namespace]\n", lpad, "");
      break;
    case BL_PT_CALL:
      printf("%*s[call]\n", lpad, "");
      break;
    case BL_PT_ASGN:
      printf("%*s[asign]\n", lpad, "");
      break;
    default:
      printf("%*s[UNKNOWN]\n", lpad, "");
  }

  if (node->nodes == NULL)
    return;

  size_t c = bo_array_size(node->nodes);
  Pnode *child;
  lpad+=2;
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(node->nodes, i, Pnode *);
    log_parsed(child, lpad);
  }
}

/* class Unit */
bo_decl_params_begin(Unit)
  const char *filepath;
bo_end();

bo_impl_type(Unit, BObject);

void
UnitKlass_init(UnitKlass *klass)
{
}

void
Unit_ctor(Unit *self, UnitParams *p)
{
  /* constructor */
  self->filepath = bo_string_new_str(p->filepath);
}

void
Unit_dtor(Unit *self)
{
  bo_unref(self->filepath);
  bo_unref(self->src);
}

bo_copy_result
Unit_copy(Unit *self, Unit *other)
{
  return BO_NO_COPY;
}
/* class Unit end */

static void
load_file(Unit *self)
{
  FILE *f = fopen(bo_string_get(self->filepath), "r");
  if (f == NULL)
    bl_exit("file %s not found\n", bo_string_get(self->filepath));

  fseek(f, 0, SEEK_END);
  size_t fsize = (size_t) ftell(f);
  if (fsize == 0) {
    fclose(f);
    bl_exit("invalid source in file %s\n", bo_string_get(self->filepath));
  }

  fseek(f, 0, SEEK_SET);

  self->src = bo_string_new(fsize);
  fread((char *)bo_string_get(self->src), fsize, 1, f);
  fclose(f);
}

/* public */
Unit *
bl_unit_new(const char *filepath)
{
  UnitParams params = {
    .filepath = filepath
  };
  return bo_new(Unit, &params);
}

void
bl_unit_compile(Unit *self)
{
  load_file(self);

  Tokens *tokens = bl_lexer_scan(self);
  Pnode *root = bl_parser_scan(self, tokens);
  /*log_parsed(root, 0);*/

  CSrc *csrc = bl_cgen_generate(self, root);
  BString *impl = bl_csrc_get_impl(csrc);

  printf("%s\n", bo_string_get(impl));

  bo_unref(impl);
  bo_unref(csrc);
  bo_unref(root);
  bo_unref(tokens);
}

