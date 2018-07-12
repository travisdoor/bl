//************************************************************************************************
// bl
//
// File:   ir.c
// Author: Martin Dorazil
// Date:   12/7/18
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

#include "stages_impl.h"
#include "common_impl.h"
#include "ast_impl.h"

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
} context_t;

void
bl_ir_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  bo_iterator_t it  = bo_list_begin(assembly->ir_queue);
  bo_iterator_t end = bo_list_end(assembly->ir_queue);
  bl_node_t *   node;

  while (!bo_iterator_equal(&it, &end)) {
    node = bo_list_iter_peek(assembly->ir_queue, &it, bl_node_t *);
    assert(bl_node_is(node, BL_NODE_DECL_VALUE));

    if (!bl_peek_decl_value(node)->used) {
      bo_list_erase(assembly->ir_queue, &it);
    } else {
      bl_log("excluded %s", bl_node_name(node));
      bo_list_iter_next(assembly->ir_queue, &it);
    }
  }
}
