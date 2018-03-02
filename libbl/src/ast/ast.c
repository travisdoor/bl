//*****************************************************************************
// blc
//
// File:   ast.c
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

#include <bobject/containers/array.h>
#include "ast/ast_impl.h"

/* public */
void
bl_ast_init(bl_ast_t *ast)
{
  ast->cache = bo_array_new(sizeof(bl_node_t *));
}

void
bl_ast_terminate(bl_ast_t *ast)
{
  const size_t c = bo_array_size(ast->cache);
  bl_node_t *node;
  for (size_t i = 0; i < c; i++) {
    node = bo_array_at(ast->cache, i, bl_node_t *);
    bl_node_delete(node);
  }

  bo_unref(ast->cache);
}

bl_node_t *
bl_ast_new_node(bl_ast_t *ast,
                bl_node_type_e type,
                const char *generated_from,
                int line,
                int col)
{
  bl_node_t *node = bl_node_new(type, generated_from, line, col);
  bo_array_push_back(ast->cache, node);
  return node;
}

