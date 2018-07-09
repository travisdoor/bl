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
  bl_builder_t *builder;
  bl_unit_t *   unit;
  bl_ast_t *    ast;

  BArray *ast_lin;
} context_t;

static void
check_linearize_ast(context_t *cnt);

static void
check_linearize_node(context_t *cnt, bl_node_t **node);

static void
check(context_t *cnt, bl_node_t **node);

static void
check_decl_value(context_t *cnt, bl_node_t **node);

// impl

void
check_linearize_ast(context_t *cnt)
{
  assert(cnt->ast->root);
  check_linearize_node(cnt, &cnt->ast->root);
}

void
check_linearize_node(context_t *cnt, bl_node_t **node)
{
  if (!*node) return;
  bo_array_push_back(cnt->ast_lin, node);

#define CASE(_NAME, _name, _stmts)                                                                 \
  case BL_NODE_##_NAME: {                                                                          \
    bl_node_##_name##_t *_##_name = bl_peek_##_name(*node);                                        \
    (_stmts);                                                                                      \
    break;                                                                                         \
  }

#define CASE_IGNORE(_NAME)                                                                         \
  case BL_NODE_##_NAME:                                                                            \
    break;

#define LINEARIZE_ALL(_root)                                                                       \
  {                                                                                                \
    bl_node_t **tmp = &(_root);                                                                    \
    while (*tmp) {                                                                                 \
      check_linearize_node(cnt, tmp);                                                              \
      tmp = &(*tmp)->next;                                                                         \
    };                                                                                             \
  }

  switch (bl_node_code(*node)) {
    CASE(UBLOCK, ublock, { LINEARIZE_ALL(_ublock->nodes); })

    CASE(IDENT, ident, { check_linearize_node(cnt, &_ident->ref); })

    CASE(DECL_BLOCK, decl_block, { LINEARIZE_ALL(_decl_block->nodes); })

    CASE(DECL_VALUE, decl_value, {
      // check_linearize_node(cnt, &_decl_value->name);
      check_linearize_node(cnt, &_decl_value->type);
      check_linearize_node(cnt, &_decl_value->value);
    })

    CASE(STMT_RETURN, stmt_return, { check_linearize_node(cnt, &_stmt_return->expr); })

    CASE(STMT_IF, stmt_if, {
      check_linearize_node(cnt, &_stmt_if->test);
      check_linearize_node(cnt, &_stmt_if->true_stmt);
      check_linearize_node(cnt, &_stmt_if->false_stmt);
    })

    CASE(STMT_LOOP, stmt_loop, {
      check_linearize_node(cnt, &_stmt_loop->test);
      check_linearize_node(cnt, &_stmt_loop->true_stmt);
    })

    CASE(TYPE_FN, type_fn, {
      LINEARIZE_ALL(_type_fn->arg_types);
      check_linearize_node(cnt, &_type_fn->ret_type);
    })

    CASE(TYPE_STRUCT, type_struct, { LINEARIZE_ALL(_type_struct->types); })

    CASE(LIT_FN, lit_fn, {
      check_linearize_node(cnt, &_lit_fn->type);
      check_linearize_node(cnt, &_lit_fn->block);
    })

    CASE(LIT, lit, { check_linearize_node(cnt, &_lit->type); })

    CASE(EXPR_BINOP, expr_binop, {
      check_linearize_node(cnt, &_expr_binop->type);
      check_linearize_node(cnt, &_expr_binop->lhs);
      check_linearize_node(cnt, &_expr_binop->rhs);
    })

    CASE(EXPR_CALL, expr_call, {
      check_linearize_node(cnt, &_expr_call->ident);
      check_linearize_node(cnt, &_expr_call->type);
      LINEARIZE_ALL(_expr_call->args);
    })

    CASE_IGNORE(TYPE_FUND)

  default:
    bl_abort("cannot linearize node %s", bl_node_name(*node));
  }
#undef CASE
#undef CASE_IGNORE
#undef LINEARIZE_ALL
}

void
check(context_t *cnt, bl_node_t **node)
{
  assert(*node);
  switch (bl_node_code(*node)) {
  case BL_NODE_UBLOCK:
  case BL_NODE_IDENT:
  case BL_NODE_STMT_BAD:
  case BL_NODE_STMT_RETURN:
  case BL_NODE_STMT_IF:
  case BL_NODE_STMT_LOOP:
  case BL_NODE_DECL_VALUE:
    check_decl_value(cnt, node);
    break;
  case BL_NODE_DECL_BLOCK:
  case BL_NODE_DECL_BAD:
  case BL_NODE_TYPE_FUND:
  case BL_NODE_TYPE_FN:
  case BL_NODE_TYPE_STRUCT:
  case BL_NODE_TYPE_BAD:
  case BL_NODE_LIT_FN:
  case BL_NODE_LIT:
  case BL_NODE_EXPR_BINOP:
  case BL_NODE_EXPR_CALL:
  case BL_NODE_EXPR_BAD:
  case BL_NODE_COUNT:
    break;
  }
}

void
check_decl_value(context_t *cnt, bl_node_t **node)
{
  
}

void
bl_checker_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {
      .builder = builder, .unit = NULL, .ast = NULL, .ast_lin = bo_array_new(sizeof(bl_node_t **))};
  bo_array_reserve(cnt.ast_lin, 2048);

  bl_unit_t *unit;
  bl_barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    cnt.ast  = &unit->ast;
    check_linearize_ast(&cnt);
  }

  bl_log("");
  bl_node_t **node;
  bl_barray_foreach(cnt.ast_lin, node)
  {
    bl_log("check: %s", bl_node_name(*node));
    check(&cnt, node);
  }

  bo_unref(cnt.ast_lin);
}
