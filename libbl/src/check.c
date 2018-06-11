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

  bl_node_t      tmp_type;
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    current_unit;

  /* tmps */
  bl_node_t *curr_func;
  jmp_buf    jmp_error;
  bool       fn_has_return;
} context_t;

static inline bl_node_t *
get_tmp_fund(context_t *cnt, bl_fund_type_e t, int is_ptr)
{
  bl_node_t *tmp          = &cnt->tmp_type;
  tmp->code               = BL_TYPE_FUND;
  tmp->n.type_fund.dims   = NULL;
  tmp->n.type_fund.is_ptr = is_ptr;
  tmp->n.type_fund.type   = t;
  return tmp;
}

static inline bl_node_t *
get_tmp_ref(context_t *cnt, bl_node_t *ref, int is_ptr)
{
  bl_node_t *tmp         = &cnt->tmp_type;
  tmp->code              = BL_TYPE_REF;
  tmp->n.type_ref.dims   = NULL;
  tmp->n.type_ref.is_ptr = is_ptr;
  tmp->n.type_ref.ref    = ref;
  return tmp;
}

static inline bl_node_t *
dup_tmp_type(context_t *cnt, bl_node_t *type, int delta_ptr)
{
  if (!type)
    return NULL;

  bl_node_t *tmp = &cnt->tmp_type;
  tmp->code      = type->code;
  if (bl_node_is(type, BL_TYPE_FUND)) {
    tmp->n.type_fund.dims   = bl_peek_type_fund(type)->dims;
    tmp->n.type_fund.type   = bl_peek_type_fund(type)->type;
    tmp->n.type_fund.is_ptr = bl_peek_type_fund(type)->is_ptr + delta_ptr;
  } else if (bl_node_is(type, BL_TYPE_REF)) {
    tmp->n.type_ref.dims   = bl_peek_type_ref(type)->dims;
    tmp->n.type_ref.ref    = bl_peek_type_ref(type)->ref;
    tmp->n.type_ref.is_ptr = bl_peek_type_ref(type)->is_ptr + delta_ptr;
  } else {
    bl_abort("node is: %s not type", bl_node_name(type));
  }
  return tmp;
}

static bl_node_t *
check_expr(context_t *cnt, bl_node_t *expr, bl_node_t *expected_type, bool const_expr);

static bl_node_t *
check_null(context_t *cnt, bl_node_t *nl, bl_node_t *expected_type, bool const_expr);

static bl_node_t *
check_call(context_t *cnt, bl_node_t *call, bl_node_t *expected_type, bool const_expr);

static bl_node_t *
check_decl_ref(context_t *cnt, bl_node_t *decl_ref, bl_node_t *expected_type, bool const_expr);

static bl_node_t *
check_member_ref(context_t *cnt, bl_node_t *member_ref, bl_node_t *expected_type, bool const_expr);

static bl_node_t *
check_const(context_t *cnt, bl_node_t *cnst, bl_node_t *expected_type, bool const_expr);

static bl_node_t *
check_binop(context_t *cnt, bl_node_t *binop, bl_node_t *expected_type, bool const_expr);

static bl_node_t *
check_unary(context_t *cnt, bl_node_t *unary, bl_node_t *expected_type, bool const_expr);

static bl_node_t *
check_cast(context_t *cnt, bl_node_t *cast, bl_node_t *expected_type, bool const_expr);

bl_node_t *
check_unary(context_t *cnt, bl_node_t *unary, bl_node_t *expected_type, bool const_expr)
{
  bl_expr_unary_t *_unary    = bl_peek_expr_unary(unary);
  bl_node_t *      next_type = NULL;

  switch (_unary->op) {
  case BL_SYM_AND: {
    if (expected_type)
      expected_type = dup_tmp_type(cnt, expected_type, -1);

    next_type = check_expr(cnt, _unary->next, expected_type, const_expr);
    next_type = dup_tmp_type(cnt, next_type, 1);
    break;
  }

  case BL_SYM_ASTERISK: {
    if (expected_type)
      expected_type = dup_tmp_type(cnt, expected_type, 1);

    next_type = check_expr(cnt, _unary->next, expected_type, const_expr);
    next_type = dup_tmp_type(cnt, next_type, -1);
    break;
  }

  default:
    next_type = check_expr(cnt, _unary->next, expected_type, const_expr);
    break;
  }

  return next_type;
}

bl_node_t *
check_cast(context_t *cnt, bl_node_t *cast, bl_node_t *expected_type, bool const_expr)
{
  bl_expr_cast_t *_cast = bl_peek_expr_cast(cast);
  if (expected_type && !bl_type_compatible(_cast->to_type, expected_type)) {
    bl_ast_try_get_type_name(expected_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
    bl_ast_try_get_type_name(_cast->to_type, &cnt->tname_tmp2[0], TYPE_NAME_TMP_SIZE);

    check_error(cnt, BL_ERR_INVALID_TYPE, cast, BL_BUILDER_CUR_WORD,
                "incompatible result type of casting, expected is " BL_YELLOW(
                    "'%s'") " but result is " BL_YELLOW("'%s'"),
                cnt->tname_tmp1, cnt->tname_tmp2);
  }
  check_expr(cnt, _cast->next, NULL, const_expr);
  return _cast->to_type;
}

bl_node_t *
check_member_ref(context_t *cnt, bl_node_t *member_ref, bl_node_t *expected_type, bool const_expr)
{
  bl_expr_member_ref_t *_member_ref = bl_peek_expr_member_ref(member_ref);
  bl_node_t *           member_type = bl_peek_decl_struct_member(_member_ref->ref)->type;

  if (expected_type && !bl_type_compatible(member_type, expected_type)) {
    bl_ast_try_get_type_name(expected_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
    bl_ast_try_get_type_name(member_type, &cnt->tname_tmp2[0], TYPE_NAME_TMP_SIZE);

    check_error(cnt, BL_ERR_INVALID_TYPE, member_ref, BL_BUILDER_CUR_WORD,
                "incompatible type of member reference, expected is " BL_YELLOW(
                    "'%s'") " but reference is " BL_YELLOW("'%s'"),
                cnt->tname_tmp1, cnt->tname_tmp2);
  }

  check_expr(cnt, _member_ref->next, NULL, const_expr);
  return bl_peek_decl_struct_member(_member_ref->ref)->type;
}

bl_node_t *
check_call(context_t *cnt, bl_node_t *call, bl_node_t *expected_type, bool const_expr)
{
  if (const_expr) {
    check_error(cnt, BL_ERR_INVALID_EXPR, call, BL_BUILDER_CUR_WORD,
                "expected const-expr, function call cannot be evaluated at compile time");
  }

  bl_expr_call_t *_call  = bl_peek_expr_call(call);
  bl_node_t *     callee = _call->ref;
  bl_assert(callee, "invalid callee in function type check");
  bl_decl_func_t *_callee = bl_peek_decl_func(callee);

  if (_callee->modif & BL_MODIF_UTEST) {
    check_error(cnt, BL_ERR_INVALID_EXPR, call, BL_BUILDER_CUR_WORD,
                "calling function " BL_YELLOW(
                    "'%s'") " marked as #test from runtime tree is not possible, because "
                            "function is not included in runtime assembly, declared here: %s:%d:%d",
                _callee->id.str, callee->src->unit->filepath, callee->src->line, callee->src->col);
  }

  if (expected_type != NULL) {
    if (!bl_type_compatible(_callee->ret_type, expected_type)) {
      bl_ast_try_get_type_name(expected_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
      bl_ast_try_get_type_name(_callee->ret_type, &cnt->tname_tmp2[0], TYPE_NAME_TMP_SIZE);

      check_error(
          cnt, BL_ERR_INVALID_ARG_COUNT, call, BL_BUILDER_CUR_WORD,
          "incompatible return type of function " BL_YELLOW("'%s'") " call, expected is " BL_YELLOW(
              "'%s'") " but function returns " BL_YELLOW("'%s'") ", declared here: %s:%d:%d",
          _callee->id.str, cnt->tname_tmp1, cnt->tname_tmp2, callee->src->unit->filepath,
          callee->src->line, callee->src->col);
    }
  }

  const size_t call_arg_c   = bl_ast_call_arg_count(_call);
  const size_t callee_arg_c = bl_ast_func_arg_count(_callee);

  if (_call->run_in_compile_time && call_arg_c) {
    check_error(cnt, BL_ERR_INVALID_ARG_COUNT, call, BL_BUILDER_CUR_WORD,
                "calling function " BL_YELLOW(
                    "'%s'") " in compile time with parameters is not supported for now!!!",
                _callee->id.str);
  }

  if (call_arg_c != callee_arg_c) {
    check_error(
        cnt, BL_ERR_INVALID_ARG_COUNT, call, BL_BUILDER_CUR_WORD,
        "invalid argument count in " BL_YELLOW(
            "'%s'") " function call, expected is %d but called with %d, declared here: %s:%d:%d",
        _callee->id.str, callee_arg_c, call_arg_c, callee->src->unit->filepath, callee->src->line,
        callee->src->col);
  }

  bl_node_t *callee_arg;
  bl_node_t *call_arg;
  for (size_t i = 0; i < call_arg_c; ++i) {
    callee_arg = bl_ast_func_get_arg(_callee, i);
    call_arg   = bl_ast_call_get_arg(_call, i);

    check_expr(cnt, call_arg, bl_peek_decl_arg(callee_arg)->type, false);
  }

  return _callee->ret_type;
}

bl_node_t *
check_const(context_t *cnt, bl_node_t *cnst, bl_node_t *expected_type, bool const_expr)
{
  bl_expr_const_t *_cnst = bl_peek_expr_const(cnst);

  if (expected_type == NULL)
    return _cnst->type;

  if (!bl_type_compatible(_cnst->type, expected_type)) {
    bl_ast_try_get_type_name(expected_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
    bl_ast_try_get_type_name(_cnst->type, &cnt->tname_tmp2[0], TYPE_NAME_TMP_SIZE);

    check_error(cnt, BL_ERR_INVALID_TYPE, cnst, BL_BUILDER_CUR_WORD,
                "incompatible constant type " BL_YELLOW("'%s'") ", expected is " BL_YELLOW("'%s'"),
                cnt->tname_tmp2, cnt->tname_tmp1);
  }

  return _cnst->type;
}

bl_node_t *
check_decl_ref(context_t *cnt, bl_node_t *decl_ref, bl_node_t *expected_type, bool const_expr)
{
  bl_node_t *ref      = bl_peek_expr_decl_ref(decl_ref)->ref;
  bl_node_t *ref_type = NULL;

  bl_assert(ref, "invalid reference");

  switch (bl_node_code(ref)) {

  case BL_DECL_MUT: {
    ref_type = bl_peek_decl_mut(ref)->type;

    if (expected_type && !bl_type_compatible(ref_type, expected_type)) {
      bl_ast_try_get_type_name(expected_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
      bl_ast_try_get_type_name(ref_type, &cnt->tname_tmp2[0], TYPE_NAME_TMP_SIZE);

      check_error(
          cnt, BL_ERR_INVALID_TYPE, decl_ref, BL_BUILDER_CUR_WORD,
          "incompatible type of variable reference " BL_YELLOW("'%s'") ", expected is " BL_YELLOW(
              "'%s'") " but variable is declared " BL_YELLOW("'%s'") ", declared here: %s:%d:%d",
          bl_ast_try_get_id(ref)->str, cnt->tname_tmp1, cnt->tname_tmp2, ref->src->unit->filepath,
          ref->src->line, ref->src->col);
    }
    break;
  }

  case BL_DECL_ARG: {
    ref_type = bl_peek_decl_arg(ref)->type;

    if (expected_type && !bl_type_compatible(ref_type, expected_type)) {
      bl_ast_try_get_type_name(expected_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
      bl_ast_try_get_type_name(ref_type, &cnt->tname_tmp2[0], TYPE_NAME_TMP_SIZE);

      check_error(
          cnt, BL_ERR_INVALID_TYPE, decl_ref, BL_BUILDER_CUR_WORD,
          "incompatible type of function argument reference " BL_YELLOW(
              "'%s'") ", expected is " BL_YELLOW("'%s'") " but argument is "
                                                         "declared " BL_YELLOW(
                                                             "'%s'") ", declared here: %s:%d:%d",
          bl_ast_try_get_id(ref)->str, cnt->tname_tmp1, cnt->tname_tmp2, ref->src->unit->filepath,
          ref->src->line, ref->src->col);
    }
    break;
  }

  case BL_DECL_CONST: {
    bl_assert(bl_peek_decl_const(ref)->init_expr, "invalid const init expr");
    ref_type = bl_peek_decl_const(ref)->type;

    if (expected_type && !bl_type_compatible(ref_type, expected_type)) {
      bl_ast_try_get_type_name(expected_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
      bl_ast_try_get_type_name(ref_type, &cnt->tname_tmp2[0], TYPE_NAME_TMP_SIZE);

      check_error(
          cnt, BL_ERR_INVALID_TYPE, decl_ref, BL_BUILDER_CUR_WORD,
          "incompatible type of constant reference " BL_YELLOW("'%s'") ", expected is " BL_YELLOW(
              "'%s'") " but constant is declared " BL_YELLOW("'%s'") ", declared here: %s:%d:%d",
          bl_ast_try_get_id(ref)->str, cnt->tname_tmp1, cnt->tname_tmp2, ref->src->unit->filepath,
          ref->src->line, ref->src->col);
    }
    break;
  }

  case BL_DECL_ENUM_VARIANT: {
    ref_type = get_tmp_ref(cnt, bl_peek_decl_enum_variant(ref)->parent, false);
    // ref_type = bl_peek_decl_enum(bl_peek_decl_enum_variant(ref)->parent)->type;

    if (expected_type && !bl_type_compatible(ref_type, expected_type)) {
      bl_ast_try_get_type_name(expected_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
      bl_ast_try_get_type_name(ref_type, &cnt->tname_tmp2[0], TYPE_NAME_TMP_SIZE);
      check_error(
          cnt, BL_ERR_INVALID_TYPE, decl_ref, BL_BUILDER_CUR_WORD,
          "incompatible type of enum variant " BL_YELLOW("'%s'") ", expected is " BL_YELLOW(
              "'%s'") " but variable is declared " BL_YELLOW("'%s'") ", declared here: %s:%d:%d",
          bl_ast_try_get_id(ref)->str, cnt->tname_tmp1, cnt->tname_tmp2, ref->src->unit->filepath,
          ref->src->line, ref->src->col);
    }
    break;
  }

  default:
    bl_abort("cannot check reference of type: %s", bl_node_name(ref));
  }

  return ref_type;
}

bl_node_t *
check_binop(context_t *cnt, bl_node_t *binop, bl_node_t *expected_type, bool const_expr)
{
  bl_expr_binop_t *_binop = bl_peek_expr_binop(binop);

  if (_binop->type && expected_type) {
    if (!bl_type_compatible(_binop->type, expected_type)) {
      bl_ast_try_get_type_name(expected_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
      check_error(cnt, BL_ERR_INVALID_TYPE, binop, BL_BUILDER_CUR_WORD,
                  "binary operation has incompatible type, expected is " BL_YELLOW("'%s'"),
                  cnt->tname_tmp1);
    }

    expected_type = NULL;
  }

  switch (_binop->op) {
  case BL_SYM_ASSIGN:
    if (bl_node_is(_binop->lhs, BL_EXPR_DECL_REF) &&
        bl_node_is(bl_peek_expr_decl_ref(_binop->lhs)->ref, BL_DECL_CONST)) {
      bl_decl_const_t *_cnst = bl_peek_decl_const(bl_peek_expr_decl_ref(_binop->lhs)->ref);
      check_error(cnt, BL_ERR_INVALID_EXPR, binop, BL_BUILDER_CUR_WORD,
                  "constant " BL_YELLOW("'%s'") " is not mutable and it's value cannot be changed ",
                  _cnst->id.str);
    }
    break;
  default:
    break;
  }

  bl_node_t *lhs_type = check_expr(cnt, _binop->lhs, expected_type, const_expr);
  check_expr(cnt, _binop->rhs, lhs_type, const_expr);

  if (_binop->type == NULL)
    return lhs_type;
  else
    return _binop->type;
}

bl_node_t *
check_null(context_t *cnt, bl_node_t *nl, bl_node_t *expected_type, bool const_expr)
{
  if (!expected_type)
    return NULL;

  bl_expr_null_t *_null = bl_peek_expr_null(nl);
  if (bl_type_is_ptr(expected_type) <= 0) {
    check_error(cnt, BL_ERR_INVALID_TYPE, nl, BL_BUILDER_CUR_WORD,
                "only pointers can be set to null value");
  }

  _null->type = expected_type;
  return NULL;
}

bl_node_t *
check_expr(context_t *cnt, bl_node_t *expr, bl_node_t *expected_type, bool const_expr)
{
  switch (bl_node_code(expr)) {
  case BL_EXPR_DECL_REF:
    return check_decl_ref(cnt, expr, expected_type, const_expr);

  case BL_EXPR_CALL:
    return check_call(cnt, expr, expected_type, const_expr);

  case BL_EXPR_BINOP:
    return check_binop(cnt, expr, expected_type, const_expr);

  case BL_EXPR_CONST:
    return check_const(cnt, expr, expected_type, const_expr);

  case BL_EXPR_UNARY:
    return check_unary(cnt, expr, expected_type, const_expr);

  case BL_EXPR_SIZEOF:
    return NULL;

  case BL_EXPR_NULL: {
    return check_null(cnt, expr, expected_type, const_expr);
  }

  case BL_EXPR_MEMBER_REF:
    return check_member_ref(cnt, expr, expected_type, const_expr);

  case BL_EXPR_ARRAY_REF:
    return NULL;

  case BL_EXPR_CAST:
    return check_cast(cnt, expr, expected_type, const_expr);

  case BL_EXPR_INIT:
    return NULL;

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
  if (bl_node_is(expr, BL_EXPR_BINOP) && bl_peek_expr_binop(expr)->op != BL_SYM_ASSIGN) {
    check_warning(cnt, expr, BL_BUILDER_CUR_WORD, "expression has no effect");
  }

  check_expr(cnt, expr, NULL, false);
}

static void
visit_mut(bl_visitor_t *visitor, bl_node_t *mut)
{
  context_t *    cnt  = peek_cnt(visitor);
  bl_decl_mut_t *_mut = bl_peek_decl_mut(mut);

  if (_mut->used == 0) {
    check_warning(cnt, mut, BL_BUILDER_CUR_WORD,
                  "variable " BL_YELLOW("'%s'") " is declared but never used", _mut->id.str);
  }

  if (_mut->init_expr != NULL) {
    check_expr(cnt, _mut->init_expr, _mut->type, false);
  }
}

static void
visit_const(bl_visitor_t *visitor, bl_node_t *cnst)
{
  context_t *      cnt   = peek_cnt(visitor);
  bl_decl_const_t *_cnst = bl_peek_decl_const(cnst);

  if (!(_cnst->modif & BL_MODIF_PUBLIC) && _cnst->used == 0) {
    check_warning(cnt, cnst, BL_BUILDER_CUR_WORD,
                  "constant " BL_YELLOW("'%s'") " is declared but never used", _cnst->id.str);
  }

  if (_cnst->init_expr != NULL) {
    check_expr(cnt, _cnst->init_expr, _cnst->type, true);
  }
}

static void
visit_struct(bl_visitor_t *visitor, bl_node_t *strct)
{
  context_t *       cnt    = peek_cnt(visitor);
  bl_decl_struct_t *_strct = bl_peek_decl_struct(strct);
  if (_strct->modif == BL_MODIF_NONE && _strct->used == 0) {
    check_warning(cnt, strct, BL_BUILDER_CUR_WORD,
                  "structure " BL_YELLOW("'%s'") " is declared but never used", _strct->id.str);
  }

  if (bl_ast_struct_member_count(_strct) == 0) {
    check_warning(cnt, strct, BL_BUILDER_CUR_WORD, "structure " BL_YELLOW("'%s'") " is empty",
                  _strct->id.str);
  }

  /* check all memebrs of structure */
  bl_node_t *              member  = NULL;
  bl_decl_struct_member_t *_member = NULL;
  const size_t             c       = bl_ast_struct_member_count(_strct);

  for (size_t i = 0; i < c; ++i) {
    member  = bl_ast_struct_get_member(_strct, i);
    _member = bl_peek_decl_struct_member(member);

    if (bl_node_is(_member->type, BL_TYPE_REF) && bl_peek_type_ref(_member->type)->ref == strct) {
      check_error(cnt, BL_ERR_INVALID_TYPE, _member->type, BL_BUILDER_CUR_WORD,
                  "structure cannot contains self-typed member " BL_YELLOW("'%s'"),
                  _member->id.str);
    }
  }
}

static void
visit_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  context_t *     cnt  = peek_cnt(visitor);
  bl_decl_enum_t *_enm = bl_peek_decl_enum(enm);
  if (_enm->modif == BL_MODIF_NONE && _enm->used == 0) {
    check_warning(cnt, enm, BL_BUILDER_CUR_WORD,
                  "enumerator " BL_YELLOW("'%s'") " is declared but never used", _enm->id.str);
  }

  if (bl_node_is_not(_enm->type, BL_TYPE_FUND) ||
      (bl_peek_type_fund(_enm->type)->type == BL_FTYPE_BOOL ||
       bl_peek_type_fund(_enm->type)->type == BL_FTYPE_VOID) ||
      bl_peek_type_fund(_enm->type)->is_ptr) {
    check_error(cnt, BL_ERR_INVALID_TYPE, _enm->type, BL_BUILDER_CUR_WORD,
                "enumerator has invalid type, only numerical, string and char types are supported");
  }

  bl_visitor_walk_enum(visitor, enm);
}

static void
visit_enum_variant(bl_visitor_t *visitor, bl_node_t *variant)
{
  context_t *             cnt           = peek_cnt(visitor);
  bl_decl_enum_variant_t *_variant      = bl_peek_decl_enum_variant(variant);
  bl_node_t *             expected_type = bl_peek_decl_enum(_variant->parent)->type;

  if (_variant->expr != NULL) {
    /* check result type of the expression */
    check_expr(cnt, _variant->expr, expected_type, true);
  } else {
    bl_type_fund_t *type = bl_peek_type_fund(expected_type);
    switch (type->type) {
    case BL_FTYPE_CHAR:
    case BL_FTYPE_STRING:
      check_error(cnt, BL_ERR_EXPECTED_EXPR, variant, BL_BUILDER_CUR_WORD,
                  "cannot generate value of enum variant " BL_YELLOW("'%s'") " = ?",
                  _variant->id.str);
      break;
    default:
      break;
    }
  }
}

static void
visit_func(bl_visitor_t *visitor, bl_node_t *func)
{
  bl_decl_func_t *_func = bl_peek_decl_func(func);
  context_t *     cnt   = peek_cnt(visitor);
  if (_func->modif == BL_MODIF_NONE && !_func->used) {
    check_warning(cnt, func, BL_BUILDER_CUR_WORD,
                  "function " BL_YELLOW("'%s'") " is declared but never used", _func->id.str);
  }

  if (_func->modif & BL_MODIF_UTEST) {
    if (bl_ast_func_arg_count(_func)) {
      check_error(
          cnt, BL_ERR_INVALID_ARG_COUNT, func, BL_BUILDER_CUR_WORD,
          "function " BL_YELLOW("'%s'") " is marked as #test, those functions are invoked in "
                                        "compile-time and it cannot take any parameters",
          _func->id.str);
    }

    if (!bl_type_compatible(_func->ret_type, get_tmp_fund(cnt, BL_FTYPE_I32, 0))) {
      check_error(cnt, BL_ERR_EXPECTED_TYPE, func, BL_BUILDER_CUR_WORD,
                  "function " BL_YELLOW("'%s'") " is marked as #test, those functions must return "
                                                "i32 value (0 when test passed without errors)",
                  _func->id.str);
    }
  }

  cnt->curr_func = func;
  bl_visitor_walk_func(visitor, func);

  bool ignore_missing_return = bl_node_is(_func->ret_type, BL_TYPE_FUND) &&
                               bl_peek_type_fund(_func->ret_type)->type == BL_FTYPE_VOID;

  if (!ignore_missing_return && !cnt->fn_has_return && !(_func->modif & BL_MODIF_EXTERN)) {
    // error -> missing return statement
    check_error(cnt, BL_ERR_MISSING_RETURN, func, BL_BUILDER_CUR_WORD,
                "missing return statement in function " BL_YELLOW("'%s'"), _func->id.str);
  }
  cnt->fn_has_return = false;
}

static void
visit_return(bl_visitor_t *visitor, bl_node_t *ret)
{
  context_t *cnt = peek_cnt(visitor);
  bl_assert(cnt->curr_func, "invalid current function");
  bl_stmt_return_t *_ret          = bl_peek_stmt_return(ret);
  bl_node_t *       expected_type = bl_peek_decl_func(cnt->curr_func)->ret_type;

  if (_ret->expr)
    check_expr(cnt, _ret->expr, expected_type, false);

  cnt->fn_has_return = true;
}

static void
visit_if(bl_visitor_t *visitor, bl_node_t *if_stmt)
{
  context_t *cnt = peek_cnt(visitor);
  bl_assert(cnt->curr_func, "invalid current function");
  bl_stmt_if_t *_if = bl_peek_stmt_if(if_stmt);
  check_expr(cnt, _if->test, get_tmp_fund(cnt, BL_FTYPE_BOOL, 0), false);

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
  check_expr(cnt, _loop->test, get_tmp_fund(cnt, BL_FTYPE_BOOL, 0), false);

  bl_visitor_walk_loop_body(visitor, loop);
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/

bl_error_e
bl_check_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder = builder, .assembly = assembly, .fn_has_return = false};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e)error;
  }

  bl_visitor_t visitor;
  bl_visitor_init(&visitor, &cnt);
  bl_visitor_add(&visitor, visit_expr, BL_VISIT_EXPR);
  bl_visitor_add(&visitor, visit_mut, BL_VISIT_MUT);
  bl_visitor_add(&visitor, visit_const, BL_VISIT_CONST);
  bl_visitor_add(&visitor, visit_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor, visit_return, BL_VISIT_RETURN);
  bl_visitor_add(&visitor, visit_if, BL_VISIT_IF);
  bl_visitor_add(&visitor, visit_loop, BL_VISIT_LOOP);
  bl_visitor_add(&visitor, visit_struct, BL_VISIT_STRUCT);
  bl_visitor_add(&visitor, visit_enum, BL_VISIT_ENUM);
  bl_visitor_add(&visitor, visit_enum_variant, BL_VISIT_ENUM_VARIANT);

  const int  c    = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; ++i) {
    unit             = bl_assembly_get_unit(assembly, i);
    cnt.current_unit = unit;
    bl_visitor_walk_module(&visitor, unit->ast.root);
  }

  return BL_NO_ERR;
}
