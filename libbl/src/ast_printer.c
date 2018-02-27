//*****************************************************************************
// blc
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
#include "bl/ast_printer.h"
#include "bl/unit.h"
#include "bl/bldebug.h"
#include "ast/node_impl.h"

static bool
run(AstPrinter *self,
    Unit *unit);

/* AstPrinter members */
bo_decl_members_begin(AstPrinter, Stage)
  FILE *out_stream;
bo_end();

/* AstPrinter constructor parameters */
bo_decl_params_with_base_begin(AstPrinter, Stage)
  FILE *out_stream;
bo_end();

bo_impl_type(AstPrinter, Stage);

/* AstPrinter class init */
void
AstPrinterKlass_init(AstPrinterKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run =
    (bool (*)(Stage *,
              Actor *)) run;
}

/* AstPrinter constructor */
void
AstPrinter_ctor(AstPrinter *self,
                AstPrinterParams *p)
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
AstPrinter_copy(AstPrinter *self,
                AstPrinter *other)
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
  fprintf(
    self->out_stream, "%*s%s\n", pad, "", bo_string_get(s));
  bo_unref(s);

  int c = 0;
  Node *child = NULL;

  switch (node->type) {
    case BL_NODE_GLOBAL_STMT:
      c = bl_node_global_stmt_get_child_count((NodeGlobalStmt *) node);
      pad += 2;
      for (int i = 0; i < c; i++) {
        child = bl_node_global_stmt_get_child((NodeGlobalStmt *) node, i);
        print_node(self, child, pad);
      }
      break;
    case BL_NODE_FUNC_DECL:
      c = bl_node_func_decl_get_param_count((NodeFuncDecl *) node);
      pad += 2;
      for (int i = 0; i < c; i++) {
        child = (Node *) bl_node_func_decl_get_param((NodeFuncDecl *) node, i);
        print_node(self, child, pad);
      }
      print_node(self, (Node *) bl_node_func_decl_get_stmt((NodeFuncDecl *) node), pad);
      break;
    case BL_NODE_CALL:
      c = bl_node_call_get_arg_count((NodeCall *) node);
      pad += 2;
      for (int i = 0; i < c; i++) {
        child = (Node *) bl_node_call_get_arg((NodeCall *) node, i);
        print_node(self, child, pad);
      }
      break;
    case BL_NODE_LOOP_STMT:
      pad += 2;
      print_node(self, (Node *) bl_node_loop_stmt_get_stmt((NodeLoopStmt *) node), pad);
      break;
    case BL_NODE_BINOP:
      pad += 2;
      print_node(self, (Node *) bl_node_binop_get_lhs((NodeBinop *) node), pad);
      print_node(self, (Node *) bl_node_binop_get_rhs((NodeBinop *) node), pad);
      break;
    case BL_NODE_IF_STMT:
      pad += 2;
      print_node(self, (Node *) bl_node_if_stmt_get_cond((NodeIfStmt *) node), pad);
      print_node(self, (Node *) bl_node_if_stmt_get_then((NodeIfStmt *) node), pad);
      break;
    case BL_NODE_DECL_REF:
      break;
    case BL_NODE_CONST:
      break;
    case BL_NODE_STMT:
      c = bl_node_stmt_child_get_count((NodeStmt *) node);
      pad += 2;
      for (int i = 0; i < c; i++) {
        child = bl_node_stmt_get_child((NodeStmt *) node, i);
        print_node(self, child, pad);
      }
      break;
    case BL_NODE_RETURN_STMT:
      pad += 2;
      print_node(self, (Node *) bl_node_return_stmt_get_expr((NodeReturnStmt *) node), pad);
      break;
    case BL_NODE_VAR_DECL:
      pad += 2;
      print_node(self, (Node *) bl_node_var_decl_get_expr((NodeVarDecl *) node), pad);
    default:
      break;
  }
}

bool
run(AstPrinter *self,
    Unit *unit)
{
  if (bl_unit_get_ast(unit) == NULL) {
    bl_actor_error((Actor *) unit, "cannot find AST tree in unit %s", bl_unit_get_src_file(unit));
    return false;
  }

  print_node(self, bl_ast_get_root(bl_unit_get_ast(unit)), 0);
  return true;
}

/* public */
AstPrinter *
bl_ast_printer_new(FILE *out_stream,
                   bl_compile_group_e group)
{
  AstPrinterParams p = {.base.group = group, .out_stream = out_stream};

  return bo_new(AstPrinter, &p);
}

