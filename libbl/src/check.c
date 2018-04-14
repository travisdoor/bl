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

  /* tmps */
  bl_node_t *curr_func;
  jmp_buf    jmp_error;
} context_t;

/* static bool type node used for type checking */
static bl_node_t bool_ftype = {
    .src = NULL, .code = BL_TYPE_FUND, .n.type_fund.type = BL_FTYPE_BOOL};

static bl_node_t *
check_expr(context_t *cnt, bl_node_t *expr, bl_node_t *expected_type);

static bl_node_t *
check_call(context_t *cnt, bl_node_t *call, bl_node_t *expected_type);

static bl_node_t *
check_decl_ref(context_t *cnt, bl_node_t *decl_ref, bl_node_t *expected_type);

static bl_node_t *
check_const(context_t *cnt, bl_node_t *cnst, bl_node_t *expected_type);

static bl_node_t *
check_binop(context_t *cnt, bl_node_t *binop, bl_node_t *expected_type);

bl_node_t *
check_call(context_t *cnt, bl_node_t *call, bl_node_t *expected_type)
{
  bl_expr_call_t *_call  = bl_peek_expr_call(call);
  bl_node_t *     callee = _call->ref;
  bl_assert(callee, "invalid callee in function type check");
  bl_decl_func_t *_callee = bl_peek_decl_func(callee);

  if (expected_type != NULL) {
    if (!bl_type_compatible(_callee->ret_type, expected_type)) {
      check_error(
          cnt, BL_ERR_INVALID_ARG_COUNT, call,
          "incompatible return type of function " BL_YELLOW("'%s'") " call, expected is " BL_YELLOW(
              "'%s'") " but function returns " BL_YELLOW("'%s'") ", declared here: %s:%d:%d",
          _callee->id.str, bl_ast_try_get_type_name(expected_type),
          bl_ast_try_get_type_name(_callee->ret_type), callee->src->file, callee->src->line,
          callee->src->col);
    }
  }

  const size_t call_arg_c   = bl_ast_call_arg_count(_call);
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
    call_arg   = bl_ast_call_get_arg(_call, i);

    check_expr(cnt, call_arg, bl_peek_decl_var(callee_arg)->type);
  }

  return _callee->ret_type;
}

bl_node_t *
check_const(context_t *cnt, bl_node_t *cnst, bl_node_t *expected_type)
{
  bl_expr_const_t *_cnst = bl_peek_expr_const(cnst);

  if (expected_type == NULL)
    return _cnst->type;

  if (!bl_type_compatible(_cnst->type, expected_type)) {
    check_error(cnt, BL_ERR_INVALID_TYPE, cnst,
                "incompatible constant type " BL_YELLOW("'%s'") ", expected is " BL_YELLOW("'%s'"),
                bl_ast_try_get_type_name(_cnst->type), bl_ast_try_get_type_name(expected_type));
  }

  return _cnst->type;
}

bl_node_t *
check_decl_ref(context_t *cnt, bl_node_t *decl_ref, bl_node_t *expected_type)
{
  bl_node_t *ref      = bl_peek_expr_decl_ref(decl_ref)->ref;
  bl_node_t *ref_type = NULL;

  if (expected_type == NULL)
    return ref_type;

  switch (bl_node_code(ref)) {
  case BL_DECL_VAR: {
    ref_type = bl_peek_decl_var(ref)->type;

    if (!bl_type_compatible(ref_type, expected_type)) {
      check_error(
          cnt, BL_ERR_INVALID_TYPE, decl_ref,
          "incompatible type of variable reference " BL_YELLOW("'%s'") ", expected is " BL_YELLOW(
              "'%s'") " but variable is declared " BL_YELLOW("'%s'") ", declared here: %s:%d:%d",
          bl_ast_try_get_id(ref)->str, bl_ast_try_get_type_name(expected_type),
          bl_ast_try_get_type_name(ref_type), ref->src->file, ref->src->line, ref->src->col);
    }
    break;
  }

  case BL_DECL_ENUM_VARIANT: {
    ref_type = bl_peek_decl_enum_variant(ref)->parent;

    if (ref_type != bl_peek_type_ref(expected_type)->ref) {
      check_error(
          cnt, BL_ERR_INVALID_TYPE, decl_ref,
          "incompatible type of enum variant " BL_YELLOW("'%s'") ", expected is " BL_YELLOW(
              "'%s'") " but variable is declared " BL_YELLOW("'%s'") ", declared here: %s:%d:%d",
          bl_ast_try_get_id(ref)->str, bl_ast_try_get_type_name(expected_type),
          bl_ast_try_get_type_name(ref_type), ref->src->file, ref->src->line, ref->src->col);
    }
    break;
  }
  default:
    bl_abort("cannot check reference of type: %s", bl_node_name(ref));
  }

  return ref_type;
}

bl_node_t *
check_binop(context_t *cnt, bl_node_t *binop, bl_node_t *expected_type)
{
  bl_expr_binop_t *_binop = bl_peek_expr_binop(binop);

  if (_binop->type && expected_type) {
    if (!bl_type_compatible(_binop->type, expected_type)) {
      check_error(cnt, BL_ERR_INVALID_TYPE, binop,
                  "binary operation has incompatible type, expected is " BL_YELLOW("'%s'"),
                  bl_ast_try_get_type_name(expected_type));
    }

    expected_type = NULL;
  }

  bl_node_t *lhs_type = check_expr(cnt, _binop->lhs, expected_type);
  check_expr(cnt, _binop->rhs, lhs_type);

  if (_binop->type == NULL)
    return lhs_type;
  else
    return _binop->type;
}

bl_node_t *
check_expr(context_t *cnt, bl_node_t *expr, bl_node_t *expected_type)
{
  switch (bl_node_code(expr)) {
  case BL_EXPR_DECL_REF:
    return check_decl_ref(cnt, expr, expected_type);

  case BL_EXPR_CALL:
    return check_call(cnt, expr, expected_type);

  case BL_EXPR_BINOP:
    return check_binop(cnt, expr, expected_type);

  case BL_EXPR_CONST:
    return check_const(cnt, expr, expected_type);

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

  /* warn about unused expressions in function scope */
  if (bl_node_is(expr, BL_EXPR_BINOP) && bl_peek_expr_binop(expr)->op != BL_SYM_ASIGN) {
    check_warning(cnt, expr, "expression has no effect");
  }

  check_expr(cnt, expr, NULL);
}

static void
visit_var(bl_visitor_t *visitor, bl_node_t *var)
{
  context_t *    cnt  = peek_cnt(visitor);
  bl_decl_var_t *_var = bl_peek_decl_var(var);
  if (_var->used == 0 && _var->modif & BL_MODIF_CONST) {
    check_warning(cnt, var, "constant " BL_YELLOW("'%s'") " is declared but never used",
                  _var->id.str);
  } else if (_var->used == 0) {
    check_warning(cnt, var, "variable " BL_YELLOW("'%s'") " is declared but never used",
                  _var->id.str);
  }

  if (_var->init_expr != NULL) {
    check_expr(cnt, _var->init_expr, _var->type);
  }
}

static void
visit_struct(bl_visitor_t *visitor, bl_node_t *strct)
{
  context_t *       cnt    = peek_cnt(visitor);
  bl_decl_struct_t *_strct = bl_peek_decl_struct(strct);
  if (_strct->modif == BL_MODIF_NONE && _strct->used == 0) {
    check_warning(cnt, strct, "structure " BL_YELLOW("'%s'") " is declared but never used",
                  _strct->id.str);
  }
}

static void
visit_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  context_t *     cnt  = peek_cnt(visitor);
  bl_decl_enum_t *_enm = bl_peek_decl_enum(enm);
  if (_enm->modif == BL_MODIF_NONE && _enm->used == 0) {
    check_warning(cnt, enm, "enumerator " BL_YELLOW("'%s'") " is declared but never used",
                  _enm->id.str);
  }

  if (bl_node_is_not(_enm->type, BL_TYPE_FUND)) {
    check_error(cnt, BL_ERR_INVALID_TYPE, _enm->type,
                "enumerator has invalid type, only fundamental types are supported");
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

  cnt->curr_func = func;
  bl_visitor_walk_func(visitor, func);
}

static void
visit_return(bl_visitor_t *visitor, bl_node_t *ret)
{
  context_t *cnt = peek_cnt(visitor);
  bl_assert(cnt->curr_func, "invalid current function");
  bl_stmt_return_t *_ret          = bl_peek_stmt_return(ret);
  bl_node_t *       expected_type = bl_peek_decl_func(cnt->curr_func)->ret_type;

  if (_ret->expr)
    check_expr(cnt, _ret->expr, expected_type);
}

static void
visit_if(bl_visitor_t *visitor, bl_node_t *if_stmt)
{
  context_t *cnt = peek_cnt(visitor);
  bl_assert(cnt->curr_func, "invalid current function");
  bl_stmt_if_t *_if = bl_peek_stmt_if(if_stmt);
  check_expr(cnt, _if->test, &bool_ftype);

  bl_visitor_walk_if_true(visitor, if_stmt);
  if (_if->false_stmt)
    bl_visitor_walk_if_false(visitor, if_stmt);
}

static void
visit_loop(bl_visitor_t *visitor, bl_node_t *loop)
{
  context_t *cnt = peek_cnt(visitor);
  bl_assert(cnt->curr_func, "invalid current function");
  bl_stmt_loop_t *_loop = bl_peek_stmt_loop(loop);
  check_expr(cnt, _loop->test, &bool_ftype);

  bl_visitor_walk_loop_body(visitor, loop);
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
  bl_visitor_add(&visitor, visit_return, BL_VISIT_RETURN);
  bl_visitor_add(&visitor, visit_if, BL_VISIT_IF);
  bl_visitor_add(&visitor, visit_loop, BL_VISIT_LOOP);
  bl_visitor_add(&visitor, visit_struct, BL_VISIT_STRUCT);
  bl_visitor_add(&visitor, visit_enum, BL_VISIT_ENUM);

  const int  c    = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor, unit->ast.root);
  }

  return BL_NO_ERR;
}
