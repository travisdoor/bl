//*****************************************************************************
// bl 
//
// File:   ast_printer.c
// Author: Martin Dorazil
// Date:   04/02/2018
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

#include <stdio.h>
#include "ast_printer_impl.h"
#include "unit_impl.h"
#include "domains_impl.h"
#include "bl/bldebug.h"
#include "ast/node_impl.h"

static bool
run(AstPrinter *self,
    Unit       *unit);

static int
domain(AstPrinter *self);

/* AstPrinter constructor parameters */
bo_decl_params_begin(AstPrinter)
  FILE *out_stream;
bo_end();

bo_impl_type(AstPrinter, Stage);

/* AstPrinter class init */
void
AstPrinterKlass_init(AstPrinterKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run 
    = (bool (*)(Stage*, Actor *)) run;
  bo_vtbl_cl(klass, Stage)->domain
    = (int (*)(Stage*)) domain;
}

/* AstPrinter constructor */
void
AstPrinter_ctor(AstPrinter *self, AstPrinterParams *p)
{
  bo_parent_ctor(Stage, p);
  self->out_stream = p->out_stream;
}

/* AstPrinter destructor */
void
AstPrinter_dtor(AstPrinter *self)
{
}

/* AstPrinter copy constructor */
bo_copy_result
AstPrinter_copy(AstPrinter *self, AstPrinter *other)
{
  return BO_NO_COPY;
}

static void 
print_node(AstPrinter *self,
           Node *node,
           int pad)
{
  if (!node)
    return;

  BString *s = bo_vtbl(node, Node)->to_string(node);
  fprintf(self->out_stream, ANSI_COLOR_YELLOW "%*s%s\n" ANSI_COLOR_RESET, pad, "", bo_string_get(s));
  bo_unref(s);

  if (node->nodes == NULL)
    return;

  size_t c = bo_array_size(node->nodes);
  Node *child;
  pad+=2;
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(node->nodes, i, Node *);
    print_node(self, child, pad);
  }
}

bool
run(AstPrinter *self,
    Unit       *unit)
{
  if (unit->ast == NULL) {
    bl_actor_error((Actor *)unit, "cannot find AST tree in unit %s", unit->filepath);
    return false;
  }

  print_node(self, bl_ast_get_root(unit->ast), 0); 
  return true;
}

int
domain(AstPrinter *self)
{
  return BL_DOMAIN_UNIT;
}

/* public */
AstPrinter *
bl_ast_printer_new(FILE *out_stream)
{
  AstPrinterParams p = {
    .out_stream = out_stream
  };

  return bo_new(AstPrinter, &p);
}

