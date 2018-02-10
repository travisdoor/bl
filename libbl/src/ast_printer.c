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
#include "bl/bldebug.h"
#include "ast/node_impl.h"

static bool
run(AstPrinter *self,
    Unit       *unit);

/* AstPrinter constructor parameters */
bo_decl_params_with_base_begin(AstPrinter, Stage)
  FILE *out_stream;
bo_end();

bo_impl_type(AstPrinter, Stage);

/* AstPrinter class init */
void
AstPrinterKlass_init(AstPrinterKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run 
    = (bool (*)(Stage*, Actor *)) run;
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

  int c = 0;
  Node *child = NULL;

  switch (node->type) {
    case BL_NODE_GLOBAL_STMT:
      c = bl_node_global_stmt_child_count((NodeGlobalStmt *) node);
      pad+=2;
      for (int i = 0; i < c; i++) {
        child = bl_node_global_stmt_child((NodeGlobalStmt *) node, i);
        print_node(self, child, pad);
      }
      break;
    case BL_NODE_FUNC_DECL:
      c = bl_node_func_decl_param_count((NodeFuncDecl *) node);
      pad+=2;
      for (int i = 0; i < c; i++) {
        child = (Node *) bl_node_func_decl_param((NodeFuncDecl *) node, i);
        print_node(self, child, pad);
      }
      print_node(self, (Node *) bl_node_func_decl_get_stmt((NodeFuncDecl *) node), pad);
      break;
    case BL_NODE_PARAM_VAR_DECL:
      break;
    case BL_NODE_EXPR:
      break;
    case BL_NODE_STMT:
      c = bl_node_stmt_child_count((NodeStmt *) node);
      pad+=2;
      for (int i = 0; i < c; i++) {
        child = bl_node_stmt_child((NodeStmt *) node, i);
        print_node(self, child, pad);
      }
      break;
    case BL_NODE_RETURN_STMT:
      pad+=2;
      print_node(self, (Node *) bl_node_return_stmt_expr((NodeReturnStmt *) node), pad);
      break;
    default:
      break;
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

/* public */
AstPrinter *
bl_ast_printer_new(FILE              *out_stream,
                   bl_compile_group_e group)
{
  AstPrinterParams p = {
    .base.group = group,
    .out_stream = out_stream
  };

  return bo_new(AstPrinter, &p);
}

