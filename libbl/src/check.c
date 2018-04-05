//*****************************************************************************
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
//*****************************************************************************

#include <setjmp.h>
#include "common_impl.h"
#include "stages_impl.h"
#include "ast/visitor_impl.h"

#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define check_error(cnt, code, node, format, ...)                                                  \
  {                                                                                                \
    bl_builder_error((cnt)->builder, "%s:%d:%d " format, (node)->src->file, (node)->src->line,     \
                     (node)->src->col, ##__VA_ARGS__);                                             \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

#define check_warning(cnt, node, format, ...)                                                      \
  {                                                                                                \
    bl_builder_warning((cnt)->builder, "%s:%d:%d " format, (node)->src->file, (node)->src->line,   \
                       (node)->src->col, ##__VA_ARGS__);                                           \
  }

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  jmp_buf        jmp_error;
} context_t;

static void
check_expr(context_t *cnt, bl_node_t *expr, bl_node_t *expected_type);

static void
check_call(context_t *cnt, bl_node_t *call, bl_node_t *expected_type);

static void
check_var_ref(context_t *cnt, bl_node_t *var_ref, bl_node_t *expected_type);

static void
check_const(context_t *cnt, bl_node_t *cnst, bl_node_t *expected_type);

void
check_call(context_t *cnt, bl_node_t *call, bl_node_t *expected_type)
{
  bl_expr_call_t *_call  = bl_peek_expr_call(call);
  bl_node_t *     callee = _call->ref;
  bl_assert(callee, "invalid callee in function type check");
  bl_decl_func_t *_callee = bl_peek_decl_func(callee);

  if (expected_type != NULL) {
    if (!bl_type_eq(_callee->ret_type, expected_type)) {
      check_error(
          cnt, BL_ERR_INVALID_ARG_COUNT, call,
          "invalid return type of function " BL_YELLOW(
              "'%s'") " call, expected is %s but function returns %s, declared here: %s:%d:%d",
          _callee->id.str, bl_ast_try_get_type_name(expected_type),
          bl_ast_try_get_type_name(_callee->ret_type), callee->src->file, callee->src->line,
          callee->src->col);
    }
  }

  const size_t call_arg_c   = bl_ast_call_arg_count(call);
  const size_t callee_arg_c = bl_ast_func_arg_count(_callee);

  if (call_arg_c != callee_arg_c) {
    check_error(
        cnt, BL_ERR_INVALID_ARG_COUNT, call,
        "invalid argument count in " BL_YELLOW(
            "'%s'") " function call, expected is %d but called with %d, declared here: %s:%d:%d",
        _callee->id.str, callee_arg_c, call_arg_c, callee->src->file, callee->src->line,
        callee->src->col);
  }

  bl_node_t *callee_arg;
  bl_node_t *call_arg;
  for (size_t i = 0; i < call_arg_c; ++i) {
    callee_arg = bl_ast_func_get_arg(_callee, i);
    call_arg   = bl_ast_call_get_arg(call, i);

    check_expr(cnt, call_arg, bl_peek_decl_arg(callee_arg)->type);
  }
}

void
check_const(context_t *cnt, bl_node_t *cnst, bl_node_t *expected_type)
{
  if (expected_type == NULL)
    return;

  bl_expr_const_t *_cnst = bl_peek_expr_const(cnst);

  if (!bl_type_eq(_cnst->type, expected_type)) {
    check_error(cnt, BL_ERR_INVALID_TYPE, cnst,
                "invalid constant type " BL_YELLOW("'%s'") ", expected is " BL_YELLOW("'%s'"),
                bl_ast_try_get_type_name(_cnst->type), bl_ast_try_get_type_name(expected_type));
  }
}

void
check_var_ref(context_t *cnt, bl_node_t *var_ref, bl_node_t *expected_type)
{
  if (expected_type == NULL)
    return;

  bl_node_t *ref = bl_peek_expr_var_ref(var_ref)->ref;
  bl_node_t *ref_type;
  bl_id_t *  ref_id;

  switch (bl_node_code(ref)) {
  case BL_DECL_VAR:
    ref_type = bl_peek_decl_var(ref)->type;
    ref_id   = &bl_peek_decl_var(ref)->id;
    break;
  case BL_DECL_ARG:
    ref_type = bl_peek_decl_arg(ref)->type;
    ref_id   = &bl_peek_decl_arg(ref)->id;
    break;
  default:
    bl_abort("invalid variable reference");
  }

  if (!bl_type_eq(ref_type, expected_type)) {
    check_error(
        cnt, BL_ERR_INVALID_TYPE, var_ref,
        "invalid type of variable " BL_YELLOW(
            "'%s'") " call, expected is %s but function returns %s, declared here: %s:%d:%d",
        ref_id->str, bl_ast_try_get_type_name(expected_type), bl_ast_try_get_type_name(ref_type),
        ref->src->file, ref->src->line, ref->src->col);
  }
}

void
check_expr(context_t *cnt, bl_node_t *expr, bl_node_t *expected_type)
{
  switch (bl_node_code(expr)) {
  case BL_EXPR_VAR_REF:
    check_var_ref(cnt, expr, expected_type);
    break;

  case BL_EXPR_CALL:
    check_call(cnt, expr, expected_type);
    break;

  case BL_EXPR_BINOP:
    check_expr(cnt, bl_peek_expr_binop(expr)->lhs, expected_type);
    check_expr(cnt, bl_peek_expr_binop(expr)->rhs, expected_type);
    break;

  case BL_EXPR_CONST:
    check_const(cnt, expr, expected_type);
    break;

  default:
    bl_abort("node is not expression");
  }
}

/*************************************************************************************************
 * toplevel visitors
 *************************************************************************************************/

static void
visit_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  context_t *cnt = peek_cnt(visitor);
  check_expr(cnt, expr, NULL);
}

static void
visit_var(bl_visitor_t *visitor, bl_node_t *var)
{
  context_t *    cnt  = peek_cnt(visitor);
  bl_decl_var_t *_var = bl_peek_decl_var(var);
  if (_var->used == 0) {
    check_warning(cnt, var, "variable " BL_YELLOW("'%s'") " is declared but never used",
                  _var->id.str);
  }

  if (_var->init_expr != NULL) {
    check_expr(cnt, _var->init_expr, _var->type);
  }
}

static void
visit_func(bl_visitor_t *visitor, bl_node_t *func)
{
  bl_decl_func_t *_func = bl_peek_decl_func(func);
  context_t *     cnt   = peek_cnt(visitor);
  if (_func->modif == BL_MODIF_NONE && _func->used == 0) {
    check_warning(cnt, func, "function " BL_YELLOW("'%s'") " is declared but never used",
                  _func->id.str);
  }

  bl_visitor_walk_func(visitor, func);
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/

bl_error_e
bl_check_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder = builder, .assembly = assembly};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e)error;
  }

  bl_visitor_t visitor;
  bl_visitor_init(&visitor, &cnt);
  bl_visitor_add(&visitor, visit_expr, BL_VISIT_EXPR);
  bl_visitor_add(&visitor, visit_var, BL_VISIT_VAR);
  bl_visitor_add(&visitor, visit_func, BL_VISIT_FUNC);

  const int  c    = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor, unit->ast.root);
  }

  return BL_NO_ERR;
}
