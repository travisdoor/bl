//*****************************************************************************
// bl
//
// File:   ast2.c
// Author: Martin Dorazil
// Date:   15/03/2018
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

#include "ast/ast2_impl.h"
#include "common_impl.h"

#define CACHE_PREALLOC_ELEM 256

typedef struct
{
  bl_node_t *next;
} next_t;

static bl_node_t *
alloc_node(bl_ast_t *ast)
{
  if (ast->chunk_used >= CACHE_PREALLOC_ELEM) {
    void *new_chunk = bl_calloc(CACHE_PREALLOC_ELEM + 1, sizeof(bl_node_t));
    ((next_t *)&ast->chunk_current[CACHE_PREALLOC_ELEM])->next = new_chunk;
    ast->chunk_current                                         = new_chunk;
    ast->chunk_used                                            = 0;
  }

  return &(ast->chunk_current[ast->chunk_used++]);
}

static void
node_terminate(bl_node_t *node)
{
  switch (node->code) {
  case BL_DECL_MODULE:
    bo_unref(bl_peek_decl_module(node)->nodes);
    break;
  case BL_DECL_FUNC:
    bo_unref(bl_peek_decl_func(node)->args);
    break;
  case BL_DECL_BLOCK:
    bo_unref(bl_peek_decl_block(node)->nodes);
    break;
  case BL_EXPR_CALL:
    bo_unref(bl_peek_expr_call(node)->args);
    break;
  default:
    break;
  }
}

/* public */
void
bl_ast_init(bl_ast_t *ast)
{
  /* one extra element for jump to next chunk */
  ast->chunk_current = bl_calloc(CACHE_PREALLOC_ELEM + 1, sizeof(bl_node_t));
  ast->chunk_used    = 0;

  ((next_t *)&ast->chunk_current[CACHE_PREALLOC_ELEM])->next = NULL;
  ast->cache_begin                                           = ast->chunk_current;
}

void
bl_ast_terminate(bl_ast_t *ast)
{
  bl_node_t *node;
  bl_node_t *chunk = ast->cache_begin;
  int        i     = 0;

  while (chunk != NULL) {
    if (i != 0 && i % CACHE_PREALLOC_ELEM == 0) {
      chunk = ((next_t *)&chunk[i])->next;
      i     = 0;
      continue;
    }

    node = &chunk[i];
    node_terminate(node);
    i++;
  }

  bl_free(ast->cache_begin);
}

bl_node_t *
bl_ast_add_type_fund(bl_ast_t *ast, bl_token_t *tok, bl_fund_type_e t)
{
  bl_node_t *type = alloc_node(ast);
  if (tok)
    type->src = &tok->src;

  type->code                    = BL_TYPE_FUND;
  bl_peek_type_fund(type)->type = t;

  return type;
}

bl_node_t *
bl_ast_add_type_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref)
{
  bl_node_t *type = alloc_node(ast);
  if (tok)
    type->src = &tok->src;

  type->code = BL_TYPE_REF;
  bl_id_init(&bl_peek_type_ref(type)->id, name);
  bl_peek_type_ref(type)->ref = ref;

  return type;
}

bl_node_t *
bl_ast_add_expr_const_char(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, char c)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->code                        = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type    = type;
  bl_peek_expr_const(expr_const)->value.c = c;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_const_bool(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, bool b)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->code                        = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type    = type;
  bl_peek_expr_const(expr_const)->value.b = b;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_const_signed(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, long long s)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->code                        = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type    = type;
  bl_peek_expr_const(expr_const)->value.s = s;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_const_unsigned(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type,
                               unsigned long long u)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->code                        = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type    = type;
  bl_peek_expr_const(expr_const)->value.u = u;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_const_double(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, double f)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->code                        = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type    = type;
  bl_peek_expr_const(expr_const)->value.f = f;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_const_str(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, const char *str)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->code                          = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type      = type;
  bl_peek_expr_const(expr_const)->value.str = str;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_binop(bl_ast_t *ast, bl_token_t *tok, bl_sym_e op, bl_node_t *lhs, bl_node_t *rhs)
{
  bl_node_t *binop = alloc_node(ast);
  if (tok)
    binop->src = &tok->src;

  binop->code                    = BL_EXPR_BINOP;
  bl_peek_expr_binop(binop)->op  = op;
  bl_peek_expr_binop(binop)->lhs = lhs;
  bl_peek_expr_binop(binop)->rhs = rhs;

  return binop;
}

bl_node_t *
bl_ast_add_expr_var_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref)
{
  bl_node_t *var_ref = alloc_node(ast);
  if (tok)
    var_ref->src = &tok->src;

  var_ref->code = BL_EXPR_VAR_REF;
  bl_id_init(&bl_peek_expr_var_ref(var_ref)->id, name);
  bl_peek_expr_var_ref(var_ref)->ref = ref;

  return var_ref;
}

bl_node_t *
bl_ast_add_expr_call(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref)
{
  bl_node_t *call = alloc_node(ast);
  if (tok)
    call->src = &tok->src;

  call->code = BL_EXPR_CALL;
  bl_id_init(&bl_peek_expr_call(call)->id, name);
  bl_peek_expr_call(call)->ref = ref;

  return call;
}

bl_node_t *
bl_ast_add_expr_path(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref,
                     bl_node_t *next)
{
  bl_node_t *path = alloc_node(ast);
  if (tok)
    path->src = &tok->src;

  path->code = BL_EXPR_PATH;
  bl_id_init(&bl_peek_expr_path(path)->id, name);
  bl_peek_expr_path(path)->next = next;
  bl_peek_expr_path(path)->ref  = ref;

  return path;
}

bl_node_t *
bl_ast_add_decl_module(bl_ast_t *ast, bl_node_t *parent, bl_token_t *tok, const char *name)
{
  bl_node_t *module = alloc_node(ast);
  if (tok)
    module->src = &tok->src;

  module->code = BL_DECL_MODULE;
  if (name)
    bl_id_init(&bl_peek_decl_module(module)->id, name);
  bl_peek_decl_module(module)->parent = parent;

  return module;
}

bl_node_t *
bl_ast_add_decl_var(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                    bl_node_t *init_expr)
{
  bl_node_t *var = alloc_node(ast);
  if (tok)
    var->src = &tok->src;

  var->code                        = BL_DECL_VAR;
  bl_peek_decl_var(var)->init_expr = init_expr;
  bl_peek_decl_var(var)->type      = type;
  bl_id_init(&bl_peek_decl_var(var)->id, name);

  return var;
}

bl_node_t *
bl_ast_add_decl_arg(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type)
{
  bl_node_t *arg = alloc_node(ast);
  if (tok)
    arg->src = &tok->src;

  arg->code                   = BL_DECL_ARG;
  bl_peek_decl_arg(arg)->type = type;
  bl_id_init(&bl_peek_decl_arg(arg)->id, name);

  return arg;
}

bl_node_t *
bl_ast_add_decl_func(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *block,
                     bl_node_t *ret_type)
{
  bl_node_t *func = alloc_node(ast);
  if (tok)
    func->src = &tok->src;

  func->code                        = BL_DECL_FUNC;
  bl_peek_decl_func(func)->block    = block;
  bl_peek_decl_func(func)->ret_type = ret_type;
  bl_id_init(&bl_peek_decl_func(func)->id, name);

  return func;
}

bl_node_t *
bl_ast_add_decl_struct(bl_ast_t *ast, bl_token_t *tok, const char *name)
{
  bl_node_t *strct = alloc_node(ast);
  if (tok)
    strct->src = &tok->src;

  strct->code = BL_DECL_STRUCT;
  bl_id_init(&bl_peek_decl_struct(strct)->id, name);

  return strct;
}

bl_node_t *
bl_ast_add_decl_enum(bl_ast_t *ast, bl_token_t *tok, const char *name)
{
  bl_node_t *enm = alloc_node(ast);
  if (tok)
    enm->src = &tok->src;

  enm->code = BL_DECL_ENUM;
  bl_id_init(&bl_peek_decl_enum(enm)->id, name);

  return enm;
}

bl_node_t *
bl_ast_add_decl_block(bl_ast_t *ast, bl_token_t *tok)
{
  bl_node_t *block = alloc_node(ast);
  if (tok)
    block->src = &tok->src;

  block->code = BL_DECL_BLOCK;

  return block;
}

bl_node_t *
bl_ast_add_stmt_if(bl_ast_t *ast, bl_token_t *tok, bl_node_t *test, bl_node_t *true_stmt,
                   bl_node_t *false_stmt)
{
  bl_node_t *if_stmt = alloc_node(ast);
  if (tok)
    if_stmt->src = &tok->src;

  if_stmt->code                        = BL_STMT_IF;
  bl_peek_stmt_if(if_stmt)->test       = test;
  bl_peek_stmt_if(if_stmt)->true_stmt  = true_stmt;
  bl_peek_stmt_if(if_stmt)->false_stmt = false_stmt;

  return if_stmt;
}

bl_node_t *
bl_ast_add_stmt_loop(bl_ast_t *ast, bl_token_t *tok, bl_node_t *true_stmt)
{
  bl_node_t *loop_stmt = alloc_node(ast);
  if (tok)
    loop_stmt->src = &tok->src;

  loop_stmt->code                         = BL_STMT_LOOP;
  bl_peek_stmt_loop(loop_stmt)->true_stmt = true_stmt;

  return loop_stmt;
}

bl_node_t *
bl_ast_add_stmt_while(bl_ast_t *ast, bl_token_t *tok, bl_node_t *test, bl_node_t *true_stmt)
{
  bl_node_t *while_stmt = alloc_node(ast);
  if (tok)
    while_stmt->src = &tok->src;

  while_stmt->code                          = BL_STMT_WHILE;
  bl_peek_stmt_while(while_stmt)->test      = test;
  bl_peek_stmt_while(while_stmt)->true_stmt = true_stmt;

  return while_stmt;
}

bl_node_t *
bl_ast_add_stmt_break(bl_ast_t *ast, bl_token_t *tok)
{
  bl_node_t *break_stmt = alloc_node(ast);
  if (tok)
    break_stmt->src = &tok->src;

  break_stmt->code = BL_STMT_BREAK;
  return break_stmt;
}

bl_node_t *
bl_ast_add_stmt_continue(bl_ast_t *ast, bl_token_t *tok)
{
  bl_node_t *continue_stmt = alloc_node(ast);
  if (tok)
    continue_stmt->src = &tok->src;

  continue_stmt->code = BL_STMT_CONTINUE;
  return continue_stmt;
}

bl_node_t *
bl_ast_add_stmt_return(bl_ast_t *ast, bl_token_t *tok, bl_node_t *expr)
{
  bl_node_t *return_stmt = alloc_node(ast);
  if (tok)
    return_stmt->src = &tok->src;

  return_stmt->code                      = BL_STMT_RETURN;
  bl_peek_stmt_return(return_stmt)->expr = expr;
  return return_stmt;
}

/* module */
/**************************************************************************************************/
bl_node_t *
bl_ast_module_has_node(bl_node_t *module, bl_id_t *id)
{
  bl_assert(bl_node_is(module, BL_DECL_MODULE), "invalid module");
  bl_assert(id, "invalid id");

  bl_decl_module_t *_module = bl_peek_decl_module(module);
  if (_module->nodes == NULL) {
    return NULL;
  }

  if (!bo_htbl_has_key(_module->nodes, id->hash))
    return NULL;

  return bo_htbl_at(_module->nodes, id->hash, bl_node_t *);
}

bl_node_t *
bl_ast_module_insert_node(bl_node_t *module, bl_node_t *node)
{
  bl_assert(bl_node_is(module, BL_DECL_MODULE), "invalid module");

  if (node == NULL)
    return NULL;

  bl_decl_module_t *_module = bl_peek_decl_module(module);

  if (_module->nodes == NULL) {
    _module->nodes = bo_htbl_new(sizeof(bl_node_t *), 512);
  }

  bo_htbl_insert(_module->nodes, bl_ast_try_get_id(node)->hash, node);
  return node;
}

size_t
bl_ast_module_node_count(bl_node_t *module)
{
  bl_assert(bl_node_is(module, BL_DECL_MODULE), "invalid module");
  if (bl_peek_decl_module(module)->nodes == NULL)
    return 0;
  return bo_htbl_size(bl_peek_decl_module(module)->nodes);
}

bl_node_t *
bl_ast_module_get_node(bl_node_t *module, bl_id_t *id)
{
  bl_assert(bl_node_is(module, BL_DECL_MODULE), "invalid module");
  bl_decl_module_t *_module = bl_peek_decl_module(module);

  if (_module->nodes == NULL)
    return NULL;
  if (bo_htbl_has_key(_module->nodes, id->hash))
    return bo_htbl_at(bl_peek_decl_module(module)->nodes, id->hash, bl_node_t *);
  return NULL;
}

bool
bl_ast_module_merge(bl_node_t *dest, bl_node_t *src, bl_node_t **redecl, bl_node_t **orig)
{
  bl_assert(bl_node_is(dest, BL_DECL_MODULE), "invalid module");
  bl_assert(bl_node_is(src, BL_DECL_MODULE), "invalid module");

  bl_decl_module_t *_src       = bl_peek_decl_module(src);
  bl_node_t *       node       = NULL;
  bl_node_t *       conflicted = NULL;

  if (_src->nodes) {
    bo_iterator_t iter = bo_htbl_begin(_src->nodes);
    bo_iterator_t end  = bo_htbl_end(_src->nodes);

    while (!bo_iterator_equal(&iter, &end)) {
      node = bo_htbl_iter_peek_value(_src->nodes, &iter, bl_node_t *);
      bo_htbl_iter_next(_src->nodes, &iter);

      conflicted = bl_ast_module_has_node(dest, bl_ast_try_get_id(node));
      if (conflicted && bl_node_code(conflicted) == BL_DECL_MODULE &&
          bl_ast_module_merge(conflicted, node, redecl, orig)) {
        *orig   = conflicted;
        *redecl = node;
        return false;
      }

      if (*redecl) {
        return false;
      }

      bl_ast_module_insert_node(dest, node);
    }
  }

  *redecl = NULL;
  *orig   = NULL;
  return true;
}
/**************************************************************************************************/

/* function */
/**************************************************************************************************/
bl_node_t *
bl_ast_func_push_arg(bl_node_t *func, bl_node_t *arg)
{
  bl_assert(bl_node_is(func, BL_DECL_FUNC), "invalid func");
  if (arg == NULL)
    return NULL;

  if (bl_peek_decl_func(func)->args == NULL) {
    bl_peek_decl_func(func)->args = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(bl_peek_decl_func(func)->args, arg);
  return arg;
}

size_t
bl_ast_func_arg_count(bl_node_t *func)
{
  bl_assert(bl_node_is(func, BL_DECL_FUNC), "invalid func");
  if (bl_peek_decl_func(func)->args == NULL)
    return 0;
  return bo_array_size(bl_peek_decl_func(func)->args);
}

bl_node_t *
bl_ast_func_get_arg(bl_node_t *func, const size_t i)
{
  bl_assert(bl_node_is(func, BL_DECL_FUNC), "invalid func");
  if (bl_peek_decl_func(func)->args == NULL)
    return NULL;
  return bo_array_at(bl_peek_decl_func(func)->args, i, bl_node_t *);
}
/**************************************************************************************************/

/* block */
/**************************************************************************************************/
bl_node_t *
bl_ast_block_push_node(bl_node_t *block, bl_node_t *node)
{
  bl_assert(bl_node_is(block, BL_DECL_BLOCK), "invalid block");
  if (node == NULL)
    return NULL;

  if (bl_peek_decl_block(block)->nodes == NULL) {
    bl_peek_decl_block(block)->nodes = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(bl_peek_decl_block(block)->nodes, node);
  return node;
}

size_t
bl_ast_block_node_count(bl_node_t *block)
{
  bl_assert(bl_node_is(block, BL_DECL_BLOCK), "invalid block");
  if (bl_peek_decl_block(block)->nodes == NULL)
    return 0;
  return bo_array_size(bl_peek_decl_block(block)->nodes);
}

bl_node_t *
bl_ast_block_get_node(bl_node_t *block, const size_t i)
{
  bl_assert(bl_node_is(block, BL_DECL_BLOCK), "invalid block");
  if (bl_peek_decl_block(block)->nodes == NULL)
    return NULL;
  return bo_array_at(bl_peek_decl_block(block)->nodes, i, bl_node_t *);
}
/**************************************************************************************************/

/* call */
/**************************************************************************************************/
bl_node_t *
bl_ast_call_push_arg(bl_node_t *call, bl_node_t *arg)
{
  bl_assert(bl_node_is(call, BL_EXPR_CALL), "invalid call");
  if (arg == NULL)
    return NULL;

  if (bl_peek_expr_call(call)->args == NULL) {
    bl_peek_expr_call(call)->args = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(bl_peek_expr_call(call)->args, arg);
  return arg;
}

size_t
bl_ast_call_arg_count(bl_node_t *call)
{
  bl_assert(bl_node_is(call, BL_EXPR_CALL), "invalid call");
  if (bl_peek_expr_call(call)->args == NULL)
    return 0;

  return bo_array_size(bl_peek_expr_call(call)->args);
}

bl_node_t *
bl_ast_call_get_arg(bl_node_t *call, const size_t i)
{
  bl_assert(bl_node_is(call, BL_EXPR_CALL), "invalid call");
  if (bl_peek_expr_call(call)->args == NULL)
    return NULL;
  return bo_array_at(bl_peek_expr_call(call)->args, i, bl_node_t *);
}

/* other */
/**************************************************************************************************/
bl_id_t *
bl_ast_try_get_id(bl_node_t *node)
{
  if (node == NULL) {
    return NULL;
  }

  switch (bl_node_code(node)) {
  case BL_DECL_MODULE:
    return &bl_peek_decl_module(node)->id;
  case BL_DECL_VAR:
    return &bl_peek_decl_var(node)->id;
  case BL_DECL_ARG:
    return &bl_peek_decl_arg(node)->id;
  case BL_DECL_FUNC:
    return &bl_peek_decl_func(node)->id;
  case BL_DECL_STRUCT:
    return &bl_peek_decl_struct(node)->id;
  case BL_DECL_ENUM:
    return &bl_peek_decl_enum(node)->id;
  case BL_EXPR_VAR_REF:
    return &bl_peek_expr_var_ref(node)->id;
  case BL_EXPR_CALL:
    return &bl_peek_expr_call(node)->id;
  case BL_EXPR_PATH:
    return &bl_peek_expr_path(node)->id;
  case BL_TYPE_REF:
    return &bl_peek_type_ref(node)->id;
  case BL_TYPE_FUND:
  case BL_EXPR_BINOP:
  case BL_DECL_BLOCK:
  case BL_EXPR_CONST:
  case BL_STMT_IF:
  case BL_STMT_LOOP:
  case BL_STMT_WHILE:
  case BL_STMT_BREAK:
  case BL_STMT_CONTINUE:
  case BL_STMT_RETURN:
    return NULL;
  }
}
/**************************************************************************************************/
