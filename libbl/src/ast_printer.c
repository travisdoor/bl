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
#include "stages_impl.h"
#include "common_impl.h"
#include "ast/ast2_impl.h"

static void
print_node(bl_node_t *node, int pad)
{
  if (!node)
    return;

  fprintf(stdout, "\n%*s" BL_GREEN("%s ") BL_CYAN("<%d:%d> ") BL_YELLOW("%p "), pad, "",
          bl_ast2_node_to_str(node), node->src.line, node->src.col, node);

  switch (node->t) {
  case BL_NODE_MODULE: {
    pad += 2;
    bl_item_t *item;
    bl_module_t *module = (bl_module_t *)node;
    const size_t c      = bl_ast_module_item_count(module);
    for (size_t i = 0; i < c; i++) {
      item = bl_ast_module_get_item(module, i);
      print_node((bl_node_t *)item, pad);
    }

    break;
  }
  case BL_NODE_FUNC_DECL: {
    pad += 2;
    bl_arg_t *arg;
    bl_func_decl_t *func = (bl_func_decl_t *)node;
    const size_t c       = bl_ast_func_arg_count(func);
    for (size_t i = 0; i < c; i++) {
      arg = bl_ast_func_get_arg(func, i);
      print_node((bl_node_t *)arg, pad);
    }

    print_node((bl_node_t *)func->ret, pad);

    break;
  }
  case BL_NODE_TYPE: {
    bl_type_t *type = (bl_type_t *)node;
    fprintf(stdout, "name: " BL_YELLOW("%s"), type->id.str);
    break;
  }
  case BL_NODE_ARG: {
    pad += 2;
    bl_arg_t *arg = (bl_arg_t *)node;
    fprintf(stdout, "name: " BL_YELLOW("%s"), arg->id.str);
    print_node((bl_node_t *)arg->type, pad);
    break;
  }
  case BL_NODE_ITEM: {
    pad += 2;
    bl_item_t *item = (bl_item_t *)node;
    fprintf(stdout, "name: " BL_YELLOW("%s"), item->id.str);

    switch (item->t) {
    case BL_ITEM_FUNC:
      print_node((bl_node_t *)item->node.func.func_decl, pad);
      print_node((bl_node_t *)item->node.func.block, pad);
      break;
    case BL_ITEM_MODULE:
      break;
    default:
      break;
    }
    break;
  }
  default:
    break;
  }
}

bl_error_e
bl_ast_printer_run(bl_assembly_t *assembly)
{
  const int c     = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    print_node((bl_node_t *)unit->ast.root, 0);
  }

  fprintf(stdout, "\n\n");
  return BL_NO_ERR;
}
