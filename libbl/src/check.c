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
  bl_node_t *entry_fn;
} context_t;

static void
check_call(context_t *cnt, bl_node_t **call);

static void
check_expr(context_t *cnt, bl_node_t **expr, bl_node_t *exp_type);

static void
check_init(context_t *cnt, bl_node_t **init);

static void
check_member_ref(context_t *cnt, bl_node_t **member_ref);

static void
check_unary(context_t *cnt, bl_node_t **unary);

/* impl */
void
check_init(context_t *cnt, bl_node_t **init)
{
  bl_expr_init_t *_init = bl_peek_expr_init(*init);
  bl_node_t **    expr  = &_init->exprs;

  while (*expr) {
    check_expr(cnt, expr, NULL);
    expr = &(*expr)->next;
  }
}

void
check_unary(context_t *cnt, bl_node_t **unary)
{
  bl_expr_unary_t *_unary = bl_peek_expr_unary(*unary);

  bl_node_t *type = bl_ast_get_type(*unary);
  switch (_unary->op) {
  case BL_SYM_AND: {
    bl_node_t tmp_type;
    bl_ast_dup_node_buf(&tmp_type, type);
    bl_ast_type_deref(&tmp_type);
    check_expr(cnt, &_unary->next, &tmp_type);
    break;
  }

  case BL_SYM_ASTERISK: {
    bl_node_t tmp_type;
    bl_ast_dup_node_buf(&tmp_type, type);
    bl_ast_type_addrof(&tmp_type);
    check_expr(cnt, &_unary->next, &tmp_type);
    break;
  }

  default:
    check_expr(cnt, &_unary->next, type);
    break;
  }
}

void
check_member_ref(context_t *cnt, bl_node_t **member_ref)
{
  bl_expr_member_ref_t *_member_ref = bl_peek_expr_member_ref(*member_ref);
  bl_node_t *           next_type   = bl_ast_get_type(_member_ref->next);

  if (_member_ref->is_ptr_ref && !bl_ast_type_is_ptr(next_type)) {
    check_error(cnt, BL_ERR_INVALID_TYPE, *member_ref, BL_BUILDER_CUR_BEFORE,
                "expected access operator " BL_YELLOW("'.'"));
  }

  if (!_member_ref->is_ptr_ref && bl_ast_type_is_ptr(next_type)) {
    check_error(cnt, BL_ERR_INVALID_TYPE, *member_ref, BL_BUILDER_CUR_BEFORE,
                "expected reference access operator " BL_YELLOW("'->'"));
  }

  check_expr(cnt, &_member_ref->next, NULL);
}

void
check_call(context_t *cnt, bl_node_t **call)
{
  bl_expr_call_t *_call   = bl_peek_expr_call(*call);
  bl_node_t *     callee  = _call->ref;
  bl_decl_func_t *_callee = bl_peek_decl_func(_call->ref);

  if (_callee->modif & BL_MODIF_UTEST) {
    check_error(cnt, BL_ERR_INVALID_EXPR, *call, BL_BUILDER_CUR_WORD,
                "calling function " BL_YELLOW(
                    "'%s'") " marked as #test from runtime tree is not possible, because "
                            "function is not included in runtime assembly, declared here: %s:%d:%d",
                _callee->id.str, callee->src->unit->filepath, callee->src->line, callee->src->col);
  }

  if (_call->run_in_compile_time && _call->argsc) {
    check_error(cnt, BL_ERR_INVALID_ARG_COUNT, *call, BL_BUILDER_CUR_WORD,
                "calling function " BL_YELLOW(
                    "'%s'") " in compile time with parameters is not supported for now!!!",
                _callee->id.str);
  }

  if (_call->argsc != _callee->argsc) {
    check_error(
        cnt, BL_ERR_INVALID_ARG_COUNT, *call, BL_BUILDER_CUR_WORD,
        "invalid argument count in " BL_YELLOW(
            "'%s'") " function call, expected is %d but called with %d, declared here: %s:%d:%d",
        _callee->id.str, _callee->argsc, _call->argsc, callee->src->unit->filepath,
        callee->src->line, callee->src->col);
  }

  bl_node_t **callee_arg = &_callee->args;
  bl_node_t **call_arg   = &_call->args;

  while (*callee_arg) {
    check_expr(cnt, call_arg, bl_ast_get_type(*callee_arg));

    callee_arg = &(*callee_arg)->next;
    call_arg   = &(*call_arg)->next;
  }
}

static void
check_expr(context_t *cnt, bl_node_t **expr, bl_node_t *exp_type)
{
  if (!*expr)
    return;

  bl_node_t *expr_type = bl_ast_get_type(*expr);

  if (!expr_type) {
    switch (bl_node_code(*expr)) {
    case BL_EXPR_NULL:
      bl_peek_expr_null(*expr)->type = bl_ast_dup_node(cnt->ast, exp_type);
      break;

    default:
      break;
    }
  } else if ((exp_type && !bl_ast_type_compatible(exp_type, expr_type))) {
    bl_type_kind_e kind = bl_ast_type_get_kind(exp_type);

    if (bl_ast_type_is_ref(exp_type, BL_DECL_ENUM)) {
      exp_type = bl_peek_decl_enum(bl_peek_type_ref(exp_type)->ref)->type;
    }

    if ((kind == BL_REAL_KIND || kind == BL_SIZE_KIND || kind == BL_UINT_KIND ||
         kind == BL_SINT_KIND) &&
        bl_node_is(*expr, BL_EXPR_CONST)) {
      bl_peek_expr_const(*expr)->type = bl_ast_dup_node(cnt->ast, exp_type);
    } else if (bl_ast_can_implcast(expr_type, exp_type)) {
      bl_node_t *icast = bl_ast_add_expr_cast(cnt->ast, NULL, exp_type, *expr);
      *expr            = icast;
    } else {
      bl_ast_get_type_name(expr_type, &cnt->tname_tmp1[0], TYPE_NAME_TMP_SIZE);
      bl_ast_get_type_name(exp_type, &cnt->tname_tmp2[0], TYPE_NAME_TMP_SIZE);

      check_error(cnt, BL_ERR_INVALID_TYPE, *expr, BL_BUILDER_CUR_WORD,
                  "cannot implicitly cast type " BL_YELLOW("'%s'") " to type " BL_YELLOW("'%s'"),
                  cnt->tname_tmp1, cnt->tname_tmp2);
    }
  }

  exp_type = expr_type;

  switch (bl_node_code(*expr)) {
  case BL_EXPR_BINOP: {
    check_expr(cnt, &bl_peek_expr_binop(*expr)->lhs, NULL);
    exp_type = bl_ast_get_type(bl_peek_expr_binop(*expr)->lhs);
    check_expr(cnt, &bl_peek_expr_binop(*expr)->rhs, exp_type);
    break;
  }

  case BL_EXPR_CALL:
    check_call(cnt, expr);
    break;

  case BL_EXPR_CAST:
    check_expr(cnt, &bl_peek_expr_cast(*expr)->next, NULL);
    break;

  case BL_EXPR_INIT:
    check_init(cnt, expr);
    break;

  case BL_EXPR_MEMBER_REF:
    check_member_ref(cnt, expr);
    break;

  case BL_EXPR_UNARY:
    check_unary(cnt, expr);
    break;

  case BL_EXPR_ARRAY_REF:
  case BL_EXPR_CONST:
  case BL_EXPR_DECL_REF:
  case BL_EXPR_NULL:
  case BL_EXPR_SIZEOF:
    break;

  default:
    bl_msg_warning("unhandled expression check for type %s", bl_node_name(*expr));
  }
}

/*************************************************************************************************
 * visitors
 *************************************************************************************************/

static void
visit_expr(bl_visitor_t *visitor, bl_node_t **expr)
{
  context_t *cnt = peek_cnt(visitor);
  check_expr(cnt, expr, NULL);
}

static void
visit_mut(bl_visitor_t *visitor, bl_node_t **mut)
{
  context_t *cnt = peek_cnt(visitor);
  check_expr(cnt, &bl_peek_decl_mut(*mut)->init_expr, bl_peek_decl_mut(*mut)->type);
}

static void
visit_func(bl_visitor_t *visitor, bl_node_t **func)
{
  bl_decl_func_t *_func = bl_peek_decl_func(*func);
  context_t *     cnt   = peek_cnt(visitor);
  if (_func->modif == BL_MODIF_NONE && !_func->used) {
    check_warning(cnt, *func, BL_BUILDER_CUR_WORD,
                  "function " BL_YELLOW("'%s'") " is declared but never used", _func->id.str);
  }

  if (_func->modif & BL_MODIF_ENTRY) {
    if (cnt->entry_fn) {
      check_error(
          cnt, BL_ERR_MULTIPLE_MAIN, *func, BL_BUILDER_CUR_WORD,
          "assembly can only have one entry method main, previous alredy defined here: %s:%d:%d",
          cnt->entry_fn->src->unit->filepath, cnt->entry_fn->src->line, cnt->entry_fn->src->col);
    } else {
      cnt->entry_fn = *func;
    }
  }

  if (_func->modif & BL_MODIF_UTEST) {
    if (_func->argsc) {
      check_error(
          cnt, BL_ERR_INVALID_ARG_COUNT, *func, BL_BUILDER_CUR_WORD,
          "function " BL_YELLOW("'%s'") " is marked as #test, those functions are invoked in "
                                        "compile-time and it cannot take any parameters",
          _func->id.str);
    }

    if (!bl_ast_type_is_fund(_func->ret_type, BL_FTYPE_I32)) {
      check_error(cnt, BL_ERR_EXPECTED_TYPE, *func, BL_BUILDER_CUR_WORD,
                  "function " BL_YELLOW("'%s'") " is marked as #test, those functions must return "
                                                "i32 value (0 when test passed without errors)",
                  _func->id.str);
    }
  }

  if (_func->block)
    bl_visitor_walk_block(visitor, &_func->block);
}

static void
visit_struct(bl_visitor_t *visitor, bl_node_t **strct)
{
  context_t *       cnt    = peek_cnt(visitor);
  bl_decl_struct_t *_strct = bl_peek_decl_struct(*strct);
  if (_strct->modif == BL_MODIF_NONE && _strct->used == 0) {
    check_warning(cnt, *strct, BL_BUILDER_CUR_WORD,
                  "structure " BL_YELLOW("'%s'") " is declared but never used", _strct->id.str);
  }

  if (!_strct->members) {
    check_warning(cnt, *strct, BL_BUILDER_CUR_WORD, "structure " BL_YELLOW("'%s'") " is empty",
                  _strct->id.str);
  }

  /* check all memebrs of structure */
  bl_node_t *              member  = _strct->members;
  bl_decl_struct_member_t *_member = NULL;

  while (member) {
    _member = bl_peek_decl_struct_member(member);

    if (bl_node_is(_member->type, BL_TYPE_REF) && bl_peek_type_ref(_member->type)->ref == *strct) {
      check_error(cnt, BL_ERR_INVALID_TYPE, _member->type, BL_BUILDER_CUR_WORD,
                  "structure cannot contains self-typed member " BL_YELLOW("'%s'"),
                  _member->id.str);
    }

    member = member->next;
  }
}

static void
visit_enum(bl_visitor_t *visitor, bl_node_t **enm)
{
  context_t *     cnt  = peek_cnt(visitor);
  bl_decl_enum_t *_enm = bl_peek_decl_enum(*enm);
  if (_enm->modif == BL_MODIF_NONE && _enm->used == 0) {
    check_warning(cnt, *enm, BL_BUILDER_CUR_WORD,
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
visit_enum_variant(bl_visitor_t *visitor, bl_node_t **variant)
{
  context_t *             cnt      = peek_cnt(visitor);
  bl_decl_enum_variant_t *_variant = bl_peek_decl_enum_variant(*variant);
  bl_node_t *             enm      = _variant->parent;
  bl_node_t *             exp_type = bl_peek_decl_enum(enm)->type;

  if (_variant->expr != NULL) {
    /* check result type of the expression */
    check_expr(cnt, &_variant->expr, exp_type);
  } else {
    bl_type_fund_t *type = bl_peek_type_fund(exp_type);
    switch (type->type) {
    case BL_FTYPE_CHAR:
    case BL_FTYPE_STRING:
      check_error(cnt, BL_ERR_EXPECTED_EXPR, *variant, BL_BUILDER_CUR_WORD,
                  "cannot generate value of enum variant " BL_YELLOW("'%s'") " = ?",
                  _variant->id.str);
      break;
    default:
      break;
    }
  }
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/

bl_error_e
bl_check_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder = builder, .assembly = assembly, .entry_fn = NULL};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e)error;
  }

  bl_visitor_t visitor;
  bl_visitor_init(&visitor, &cnt);
  bl_visitor_add(&visitor, visit_expr, BL_VISIT_EXPR);
  bl_visitor_add(&visitor, visit_mut, BL_VISIT_MUT);
  bl_visitor_add(&visitor, visit_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor, visit_struct, BL_VISIT_STRUCT);
  bl_visitor_add(&visitor, visit_enum, BL_VISIT_ENUM);
  bl_visitor_add(&visitor, visit_enum_variant, BL_VISIT_ENUM_VARIANT);
  bl_visitor_add(&visitor, BL_SKIP_VISIT, BL_VISIT_ARG);

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
