//************************************************************************************************
// bl
//
// File:   check.c
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
#include "common_impl.h"
#include "stages_impl.h"
#include "visitor_impl.h"

/* Check perform one pass over AST tree where it solves implicit casting and checking of type
 * compatibility of expression nodes. */

#define TYPE_NAME_TMP_SIZE 512
#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define check_error(cnt, code, node, pos, format, ...)                                             \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), (node)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

#define check_warning(cnt, node, pos, format, ...)                                                 \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, (node)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

typedef struct
{
  char tname_tmp1[TYPE_NAME_TMP_SIZE];
  char tname_tmp2[TYPE_NAME_TMP_SIZE];

  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
  bl_ast_t *     ast;

  /* tmps */
  jmp_buf    jmp_error;
  bl_node_t *exp_type;
} context_t;

static void 
check_call(context_t *cnt, bl_node_t *call);

void
check_call(context_t *cnt, bl_node_t *call)
{
  bl_expr_call_t *_call   = bl_peek_expr_call(call);
  bl_node_t *     callee  = _call->ref;
  bl_decl_func_t *_callee = bl_peek_decl_func(_call->ref);

  if (_callee->modif & BL_MODIF_UTEST) {
    check_error(cnt, BL_ERR_INVALID_EXPR, call, BL_BUILDER_CUR_WORD,
                "calling function " BL_YELLOW(
                    "'%s'") " marked as #test from runtime tree is not possible, because "
                            "function is not included in runtime assembly, declared here: %s:%d:%d",
                _callee->id.str, callee->src->unit->filepath, callee->src->line, callee->src->col);
  }

  if (_call->run_in_compile_time && _call->argsc) {
    check_error(cnt, BL_ERR_INVALID_ARG_COUNT, call, BL_BUILDER_CUR_WORD,
                "calling function " BL_YELLOW(
                    "'%s'") " in compile time with parameters is not supported for now!!!",
                _callee->id.str);
  }

  if (_call->argsc != _callee->argsc) {
    check_error(
        cnt, BL_ERR_INVALID_ARG_COUNT, call, BL_BUILDER_CUR_WORD,
        "invalid argument count in " BL_YELLOW(
            "'%s'") " function call, expected is %d but called with %d, declared here: %s:%d:%d",
        _callee->id.str, _callee->argsc, _call->argsc, callee->src->unit->filepath,
        callee->src->line, callee->src->col);
  }
}

/*************************************************************************************************
 * visitors
 *************************************************************************************************/

static void
visit_expr(bl_visitor_t *visitor, bl_node_t **expr)
{
  context_t *cnt           = peek_cnt(visitor);
  bl_node_t *prev_exp_type = cnt->exp_type;

  if (cnt->exp_type) {
    bl_node_t *expr_type = bl_ast_get_type(*expr);

    if (!bl_ast_type_compatible(cnt->exp_type, expr_type)) {
      bl_type_kind_e expr_kind = bl_ast_type_get_kind(expr_type);
      bl_type_kind_e exp_kind  = bl_ast_type_get_kind(cnt->exp_type);

      if (expr_kind == exp_kind) {
        bl_node_t *icast = bl_ast_add_expr_cast(cnt->ast, NULL, cnt->exp_type, *expr);
        *expr            = icast;
        cnt->exp_type    = NULL;
      } else {
        bl_ast_get_type_name(expr_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
        bl_ast_get_type_name(cnt->exp_type, &cnt->tname_tmp2[0], TYPE_NAME_TMP_SIZE);

        check_error(cnt, BL_ERR_INVALID_TYPE, *expr, BL_BUILDER_CUR_WORD,
                    "cannot implicitly cast type " BL_YELLOW("'%s'") " to type " BL_YELLOW("'%s'"),
                    cnt->tname_tmp1, cnt->tname_tmp2);
      }
    }
  }

  switch (bl_node_code(*expr)) {
  case BL_EXPR_CAST:
    cnt->exp_type = NULL;
    break;

  case BL_EXPR_BINOP:
    cnt->exp_type = bl_peek_expr_binop(*expr)->type;
    break;

  case BL_EXPR_CALL: 
    check_call(cnt, *expr);
    break;

  default:
    break;
  }

  bl_visitor_walk_expr(visitor, expr);
  cnt->exp_type = prev_exp_type;
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/

bl_error_e
bl_check_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder = builder, .assembly = assembly, .exp_type = NULL};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e)error;
  }

  bl_visitor_t visitor;
  bl_visitor_init(&visitor, &cnt);
  bl_visitor_add(&visitor, visit_expr, BL_VISIT_EXPR);

  const int  c    = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; ++i) {
    unit     = bl_assembly_get_unit(assembly, i);
    cnt.unit = unit;
    cnt.ast  = &unit->ast;
    bl_visitor_walk_module(&visitor, &unit->ast.root);
  }

  return BL_NO_ERR;
}
