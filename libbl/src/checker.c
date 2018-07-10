//************************************************************************************************
// bl
//
// File:   checker.c
// Author: Martin Dorazil
// Date:   3/15/18
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

#include <setjmp.h>
#include "stages_impl.h"
#include "common_impl.h"

#define check_error(cnt, code, tok, pos, format, ...)                                              \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), &(tok)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
  }

#define check_error_node(cnt, code, node, pos, format, ...)                                        \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), (node)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
  }

#define check_warning(cnt, tok, pos, format, ...)                                                  \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, &(tok)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

#define check_warning_node(cnt, node, pos, format, ...)                                            \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, (node)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
  bl_ast_t *     ast;

  bl_node_t *curr_compound;
} context_t;

static void
check_flatten_node(bl_flatten_t *fcache, bl_node_t **node);

static void
check_ublock(context_t *cnt, bl_node_t **node);

static void
check_decl_value(context_t *cnt, bl_node_t **decl);

// impl
void
check_flatten_node(bl_flatten_t *fcache, bl_node_t **node)
{
  if (!*node) return;

#define CASE(_NAME, _name, _stmts)                                                                 \
  case BL_NODE_##_NAME: {                                                                          \
    bl_node_##_name##_t *_##_name = bl_peek_##_name(*node);                                        \
    (_stmts);                                                                                      \
    break;                                                                                         \
  }

#define CASE_IGNORE(_NAME)                                                                         \
  case BL_NODE_##_NAME:                                                                            \
    break;

#define FLATTEN_ALL(_root)                                                                         \
  {                                                                                                \
    bl_node_t **tmp = &(_root);                                                                    \
    while (*tmp) {                                                                                 \
      check_flatten_node(fcache, tmp);                                                                \
      tmp = &(*tmp)->next;                                                                         \
    };                                                                                             \
  }

  switch (bl_node_code(*node)) {
    CASE(IDENT, ident, { check_flatten_node(fcache, &_ident->ref); })

    CASE(DECL_VALUE, decl_value, {
      check_flatten_node(fcache, &_decl_value->type);
      check_flatten_node(fcache, &_decl_value->value);
    })

  default:
    bl_abort("cannot flatten node %s", bl_node_name(*node));
  }
#undef CASE
#undef CASE_IGNORE
#undef FLATTEN_ALL

  bl_log("flatten node %s", bl_node_name(*node));
  bo_array_push_back(fcache->stack, node);
}

void
check_ublock(context_t *cnt, bl_node_t **node)
{
  bl_node_decl_ublock_t *_ublock = bl_peek_decl_ublock(*node);
  _ublock->scope                 = cnt->assembly->gscope;
  cnt->curr_compound             = *node;

  bl_node_t **it;
  while (*it) {
    check_decl_value(cnt, it);
    it = &(*it)->next;
  }
}

void
check_decl_value(context_t *cnt, bl_node_t **decl)
{
  bl_log("check decl-value");
  bl_node_decl_value_t *_decl = bl_peek_decl_value(*decl);

  bl_flatten_t *fcache = &_decl->flatten;
  if (!fcache->stack) {
    // perform flattening
    fcache->stack = bo_array_new(sizeof(bl_node_t **));
  }
}

void
bl_checker_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {
      .builder = builder, .unit = NULL, .assembly = assembly, .ast = NULL, .curr_compound = NULL};

  bl_unit_t *unit;
  bl_barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    cnt.ast  = &unit->ast;
    check_ublock(&cnt, &unit->ast.root);
  }
}
