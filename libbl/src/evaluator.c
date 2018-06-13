//************************************************************************************************
// blc
//
// File:   evaluator.c
// Author: Martin Dorazil
// Date:   26.4.18
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

/*************************************************************************************************
 * Constant expressions in static array declaration and enum variant declaration need
 * to be evaluated due to correct llvm generation. All expresions processed here must
 * be checked in type check already.
 *
 * Ex.: mut arr i32[10 + 2]; is evaluated to mut arr i32[12];
 *************************************************************************************************/

#include <setjmp.h>
#include "common_impl.h"
#include "stages_impl.h"
#include "visitor_impl.h"

#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define eval_error(cnt, code, node, pos, format, ...)                                              \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), (node)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

typedef struct
{
  bl_builder_t *builder;
  bl_ast_t *    ast;
  jmp_buf       jmp_error;
} context_t;

/*************************************************************************************************
 * evaluation
 *************************************************************************************************/

static bl_node_t *
eval_expr(context_t *cnt, bl_node_t *expr);

static bl_node_t *
eval_binop(context_t *cnt, bl_node_t *binop);

static void
eval_enum_variant(context_t *cnt, bl_node_t *mut);

static bl_node_t *
eval_add(context_t *cnt, bl_node_t *lhs, bl_node_t *rhs);

bl_node_t *
eval_add(context_t *cnt, bl_node_t *lhs, bl_node_t *rhs)
{
  bl_node_t *      result  = bl_ast_add_expr_const(cnt->ast, NULL, bl_peek_expr_const(lhs)->type);
  bl_expr_const_t *_result = bl_peek_expr_const(result);

  const long long lhs_val = bl_peek_expr_const(lhs)->value.s;
  const long long rhs_val = bl_peek_expr_const(rhs)->value.s;
  _result->value.s        = lhs_val + rhs_val;
  return result;
}

bl_node_t *
eval_binop(context_t *cnt, bl_node_t *binop)
{
  bl_expr_binop_t *_binop = bl_peek_expr_binop(binop);
  _binop->lhs             = eval_expr(cnt, _binop->lhs);
  _binop->rhs             = eval_expr(cnt, _binop->rhs);

  bl_node_t *result = bl_ast_add_expr_const(cnt->ast, NULL, bl_peek_expr_const(_binop->lhs)->type);
  bl_expr_const_t *_result = bl_peek_expr_const(result);

  const long long lhs = bl_peek_expr_const(_binop->lhs)->value.s;
  const long long rhs = bl_peek_expr_const(_binop->rhs)->value.s;

  switch (_binop->op) {
  case BL_SYM_PLUS:
    result = eval_add(cnt, _binop->lhs, _binop->rhs);
    break;
  case BL_SYM_MINUS:
    _result->value.s = lhs - rhs;
    break;
  case BL_SYM_ASTERISK:
    _result->value.s = lhs * rhs;
    break;
  case BL_SYM_OR:
    _result->value.s = lhs | rhs;
    break;
  case BL_SYM_SLASH:
    if (rhs == 0)
      _result->value.s = 0;
    _result->value.s = lhs / rhs;
    break;

  default:
    bl_abort("unsupported eval operation %s", bl_sym_strings[_binop->op]);
  }

  bl_assert(result, "invalid expression result");
  return result;
}

bl_node_t *
eval_expr(context_t *cnt, bl_node_t *expr)
{
  switch (bl_node_code(expr)) {
  case BL_EXPR_BINOP:
    return eval_binop(cnt, expr);
  case BL_EXPR_CONST:
    return expr;
  case BL_EXPR_CAST: {
    return eval_expr(cnt, bl_peek_expr_cast(expr)->next);
  }
  case BL_EXPR_DECL_REF:
    return eval_expr(cnt, bl_peek_expr_decl_ref(expr)->ref);
  case BL_DECL_ENUM_VARIANT:
    return eval_expr(cnt, bl_peek_decl_enum_variant(expr)->expr);
  case BL_DECL_CONST: {
    bl_decl_const_t *_cnst = bl_peek_decl_const(expr);
    return _cnst->init_expr;
  }
  default:
    bl_abort("expression %s cannot be evaluated", bl_node_name(expr));
  }

  bl_abort("invalid expression");
}

void
eval_enum_variant(context_t *cnt, bl_node_t *variant)
{
  bl_decl_enum_variant_t *_variant = bl_peek_decl_enum_variant(variant);
  _variant->expr                   = eval_expr(cnt, _variant->expr);
}

/*************************************************************************************************
 * visitors
 *************************************************************************************************/

static void
eval_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  bl_decl_enum_t *_enm = bl_peek_decl_enum(enm);
  context_t *     cnt  = peek_cnt(visitor);

  bl_node_t *prev_variant = NULL;
  bl_node_t *curr_variant = _enm->variants;

  while (curr_variant) {

    /*
     * there can be enum variants without explicit init expression declaration spicified in code, in
     * such case we need to generate those expressions automatically as previous variant reference +
     * 1
     */
    if (bl_peek_decl_enum_variant(curr_variant)->expr == NULL) {
      if (prev_variant == NULL) {
        /* create default constant init expression */
        bl_node_t *def_expr                   = bl_ast_add_expr_const(cnt->ast, NULL, _enm->type);
        bl_peek_expr_const(def_expr)->value.u = 0;
        bl_peek_decl_enum_variant(curr_variant)->expr = def_expr;
      } else {
        bl_node_t *def_expr_one        = bl_ast_add_expr_const(cnt->ast, NULL, _enm->type);
        bl_node_t *def_expr_const_prev = bl_peek_decl_enum_variant(prev_variant)->expr;
        bl_node_t *def_expr_add        = bl_ast_add_expr_binop(
            cnt->ast, NULL, BL_SYM_PLUS, def_expr_const_prev, def_expr_one, _enm->type);

        bl_peek_expr_const(def_expr_one)->value.u     = 1;
        bl_peek_decl_enum_variant(curr_variant)->expr = def_expr_add;
      }
    }

    eval_enum_variant(cnt, curr_variant);
    prev_variant = curr_variant;
    curr_variant = curr_variant->next;
  }
}

static void
eval_type(bl_visitor_t *visitor, bl_node_t *type)
{
  context_t *cnt    = peek_cnt(visitor);
  bl_node_t *dim    = NULL;
  bl_node_t *result = NULL;

  switch (bl_node_code(type)) {
  case BL_TYPE_REF:
    dim = bl_peek_type_ref(type)->dim;
    if (dim) {
      result                      = eval_expr(cnt, dim);
      bl_peek_type_ref(type)->dim = result;
    }
    break;
  case BL_TYPE_FUND:
    dim = bl_peek_type_fund(type)->dim;
    if (dim) {
      result                       = eval_expr(cnt, dim);
      bl_peek_type_fund(type)->dim = result;
    }
    break;
  default:
    bl_abort("invalid type");
  }

  if (result && bl_peek_expr_const(result)->value.u == 0) {
    eval_error(cnt, BL_ERR_INVALID_EXPR, type, BL_BUILDER_CUR_WORD,
               "arrays with zero size are not allowed");
  }

  // TODO walk type???
}

static void
eval_decl_const(bl_visitor_t *visitor, bl_node_t *cnst)
{
  context_t *      cnt   = peek_cnt(visitor);
  bl_decl_const_t *_cnst = bl_peek_decl_const(cnst);
  bl_assert(_cnst->init_expr, "constant without init expression");
  _cnst->init_expr = eval_expr(cnt, _cnst->init_expr);

  bl_visitor_walk_const(visitor, cnst);
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/
bl_error_e
bl_evaluator_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt;
  cnt.builder = builder;

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e)error;
  }

  bl_visitor_t visitor_eval;
  bl_visitor_init(&visitor_eval, &cnt);
  bl_visitor_add(&visitor_eval, eval_enum, BL_VISIT_ENUM);
  bl_visitor_add(&visitor_eval, eval_type, BL_VISIT_TYPE);
  bl_visitor_add(&visitor_eval, eval_decl_const, BL_VISIT_CONST);

  const int  c    = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;
  for (int i = 0; i < c; ++i) {
    unit    = bl_assembly_get_unit(assembly, i);
    cnt.ast = &unit->ast;
    bl_visitor_walk_module(&visitor_eval, unit->ast.root);
  }

  return BL_NO_ERR;
}
