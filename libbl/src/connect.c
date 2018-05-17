//************************************************************************************************
// blc
//
// File:   connect.c
// Author: Martin Dorazil
// Date:   14.2.18
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
 * Connect will do 3 partial iterations of AST passed in.
 *
 * 2) connect type tree of custom types (mostly structures) so references to those types can be used
 *    later in compilation
 *    prepare file-global usings
 *
 * 3) connect rest of the source (mostly expressions referencing to some custom types)
 *************************************************************************************************/

#include <setjmp.h>
#include <bobject/containers/htbl.h>
#include "common_impl.h"
#include "stages_impl.h"
#include "ast/visitor_impl.h"

#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define connect_error(cnt, code, loc, format, ...)                                                 \
  {                                                                                                \
    bl_builder_error((cnt)->builder, "%s:%d:%d " format, loc->file, loc->line, loc->col,           \
                     ##__VA_ARGS__);                                                               \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

#define connect_warning(cnt, loc, format, ...)                                                     \
  {                                                                                                \
    bl_builder_warning((cnt)->builder, "%s:%d:%d " format, loc->file, loc->line, loc->col,         \
                       ##__VA_ARGS__);                                                             \
  }

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
  jmp_buf        jmp_error;

  /* Current compound node is pointer to current root for symbol lookup. It is typically block of
   * code inside curly brackets (module, function body, etc.). This node must have scope cache. When
   * needed symbol has not been found in scope of this node we need access to parent of this node
   * also. For example when we looking for type declaration of variable we first search in scope of
   * current compound node and than recursively in its parent nodes when type is declared
   * somewhere in global scope or parent module. */
  bl_node_t *curr_compound;
} context_t;

typedef void (*lookup_elem_valid_f)(context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last);

static bl_node_t *
lookup(context_t *cnt, BArray *path, lookup_elem_valid_f validator);

static bl_node_t *
lookup_in_tree(context_t *cnt, bl_node_t *path_elem, bl_node_t *curr_compound);

static bl_node_t *
lookup_in_scope(context_t *cnt, bl_node_t *path_elem, bl_node_t *curr_compound);

static void
connect_using(bl_visitor_t *visitor, bl_node_t *using);

static void
connect_module(bl_visitor_t *visitor, bl_node_t *module);

static void
connect_block(bl_visitor_t *visitor, bl_node_t *block);

static void
connect_func(bl_visitor_t *visitor, bl_node_t *func);

static void
connect_expr(bl_visitor_t *visitor, bl_node_t *expr);

static void
validate_call_elem(context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last);

static void
validate_using_elem(context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last);

bl_node_t *
lookup(context_t *cnt, BArray *path, lookup_elem_valid_f validator)
{
  const size_t c         = bo_array_size(path);
  bl_node_t *  path_elem = bo_array_at(path, 0, bl_node_t *);
  bl_node_t *  found     = lookup_in_tree(cnt, path_elem, cnt->curr_compound);

  if (validator)
    validator(cnt, path_elem, found, c == 1);

  for (size_t i = 1; i < c; ++i) {
    path_elem = bo_array_at(path, i, bl_node_t *);
    found     = lookup_in_scope(cnt, path_elem, found);

    if (validator)
      validator(cnt, path_elem, found, c == i + 1);
  }

  return found;
}

bl_node_t *
lookup_in_tree(context_t *cnt, bl_node_t *path_elem, bl_node_t *curr_compound)
{
  bl_node_t *         found             = NULL;
  bl_node_t *         linked_by         = NULL;
  bl_scopes_t *tmp_scopes        = NULL;
  bl_node_t *         tmp_curr_compound = curr_compound;

  while (found == NULL && tmp_curr_compound != NULL) {
    tmp_scopes = bl_ast_try_get_scopes(tmp_curr_compound);
    bl_assert(tmp_scopes, "invalid scopes");
    found = bl_scopes_get_node(tmp_scopes, &bl_peek_path_elem(path_elem)->id, &linked_by);
    tmp_curr_compound = bl_ast_try_get_parent(tmp_curr_compound);
  }
  return found;
}

bl_node_t *
lookup_in_scope(context_t *cnt, bl_node_t *path_elem, bl_node_t *curr_compound)
{
  bl_scopes_t *scopes    = bl_ast_try_get_scopes(curr_compound);
  bl_node_t *         linked_by = NULL;
  return bl_scopes_get_node(scopes, &bl_peek_path_elem(path_elem)->id, &linked_by);
}

void
connect_using(bl_visitor_t *visitor, bl_node_t *using)
{
  context_t *cnt   = peek_cnt(visitor);
  bl_node_t *found = lookup(cnt, bl_peek_stmt_using(using)->path, validate_using_elem);
  bl_peek_stmt_using(using)->ref = found;
  /* insert into curent compound scope reference to the scope of found compound block */
}

/* note: same method is used for pre_connect walking too!!! */
void
connect_module(bl_visitor_t *visitor, bl_node_t *module)
{
  context_t *cnt      = peek_cnt(visitor);
  bl_node_t *prev_cmp = cnt->curr_compound;
  cnt->curr_compound  = module;

  bl_visitor_walk_module(visitor, module);

  cnt->curr_compound = prev_cmp;
}

void
connect_block(bl_visitor_t *visitor, bl_node_t *block)
{
  context_t *      cnt      = peek_cnt(visitor);
  bl_decl_block_t *_block   = bl_peek_decl_block(block);
  bl_node_t *      prev_cmp = cnt->curr_compound;

  bl_scope_t *main_scope = bl_scope_new(cnt->assembly->scope_cache);
  bl_scopes_include_main(&_block->scopes, main_scope, NULL);
  cnt->curr_compound = block;

  bl_visitor_walk_block(visitor, block);

  cnt->curr_compound = prev_cmp;
}

void
connect_func(bl_visitor_t *visitor, bl_node_t *func)
{
  context_t *     cnt      = peek_cnt(visitor);
  bl_decl_func_t *_func    = bl_peek_decl_func(func);
  bl_node_t *     prev_cmp = cnt->curr_compound;

  /* IDEA: extern functions without body can be leaved without scope cache becouse they have no body
   * and can be called only */
  bl_scope_t *main_scope = bl_scope_new(cnt->assembly->scope_cache);
  bl_scopes_include_main(&_func->scopes, main_scope, NULL);
  cnt->curr_compound = func;

  bl_visitor_walk_func(visitor, func);

  cnt->curr_compound = prev_cmp;
}

void
connect_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  context_t *cnt = peek_cnt(visitor);
  switch (bl_node_code(expr)) {
  case BL_EXPR_CALL: {
    bl_node_t *found = lookup(cnt, bl_peek_expr_call(expr)->path, validate_call_elem);

    bl_peek_expr_call(expr)->ref = found;
    bl_peek_decl_func(found)->used++;
    break;
  }
  case BL_EXPR_DECL_REF:
    /* if (bl_peek_expr_decl_ref(expr)->ref == NULL) */
    /*   satisfy_decl_ref(cnt, expr); */
    break;
  case BL_EXPR_MEMBER_REF:
    /* member access expression has not been linked yet -> solve it recursivelly */
    /* if (bl_peek_expr_member_ref(expr)->ref == NULL) */
    /*   satisfy_member(cnt, expr); */

    break;
  default:
    break;
  }

  bl_visitor_walk_expr(visitor, expr);
}

void
validate_call_elem(context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last)
{
  if (last) {
    if (found == NULL) {
      connect_error(cnt, BL_ERR_UNKNOWN_SYMBOL, elem->src, "unknown function " BL_YELLOW("'%s'"),
                    bl_peek_path_elem(elem)->id.str);
    }
    if (bl_node_is_not(found, BL_DECL_FUNC))
      connect_error(cnt, BL_ERR_EXPECTED_FUNC, elem->src, "expected function name");
  } else {
    if (found == NULL) {
      connect_error(cnt, BL_ERR_UNKNOWN_SYMBOL, elem->src, "unknown module " BL_YELLOW("'%s'"),
                    bl_peek_path_elem(elem)->id.str);
    }

    if (bl_node_is_not(found, BL_DECL_MODULE)) {
      connect_error(cnt, BL_ERR_EXPECTED_MODULE, elem->src,
                    "expected module name in function call path");
    }
  }
}

void
validate_using_elem(context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last)
{
  if (found == NULL) {
    connect_error(cnt, BL_ERR_UNKNOWN_SYMBOL, elem->src, "unknown module " BL_YELLOW("'%s'"),
                  bl_peek_path_elem(elem)->id.str);
  }

  if (bl_node_is_not(found, BL_DECL_MODULE)) {
    connect_error(cnt, BL_ERR_EXPECTED_MODULE, elem->src, "expected module name in using path");
  }
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/
bl_error_e
bl_connect_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder = builder, .assembly = assembly, .curr_compound = NULL};
  const int c   = bl_assembly_get_unit_count(assembly);

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    /* free allocated memory on error */
    return (bl_error_e)error;
  }

  bl_visitor_t visitor_pre_connect;
  bl_visitor_init(&visitor_pre_connect, &cnt);
  bl_visitor_add(&visitor_pre_connect, connect_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_pre_connect, BL_SKIP_VISIT, BL_VISIT_FUNC);

  for (int i = 0; i < c; ++i) {
    cnt.unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_gscope(&visitor_pre_connect, cnt.unit->ast.root);
  }

  bl_visitor_t visitor_connect;
  bl_visitor_init(&visitor_connect, &cnt);
  bl_visitor_add(&visitor_connect, connect_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_connect, connect_block, BL_VISIT_BLOCK);
  bl_visitor_add(&visitor_connect, connect_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_connect, connect_expr, BL_VISIT_EXPR);
  bl_visitor_add(&visitor_connect, connect_using, BL_VISIT_USING);

  for (int i = 0; i < c; ++i) {
    cnt.unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_gscope(&visitor_connect, cnt.unit->ast.root);
  }

  return BL_NO_ERR;
}
