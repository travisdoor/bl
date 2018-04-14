//*****************************************************************************
// bl
//
// File:   ast.c
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

#include "ast/ast_impl.h"

const char *bl_fund_type_strings[] = {
#define ft(tok, str) str,
    BL_FUND_TYPE_LIST
#undef ft
};

const char *bl_node_type_strings[] = {
#define nt(code, name) #name,
    BL_NODE_TYPE_LIST
#undef ft
};

static bl_node_t *
alloc_node(bl_ast_t *ast)
{
  bl_node_t *node = bl_calloc(sizeof(bl_node_t), 1);
  bo_array_push_back(ast->nodes, node);
  return node;
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
    bo_unref(bl_peek_expr_call(node)->path);
    break;
  case BL_EXPR_DECL_REF:
    bo_unref(bl_peek_expr_decl_ref(node)->path);
    break;
  case BL_TYPE_REF:
    bo_unref(bl_peek_type_ref(node)->path);
    break;
  case BL_DECL_ENUM:
    bo_unref(bl_peek_decl_enum(node)->variants);
    break;
  case BL_DECL_STRUCT:
    bo_unref(bl_peek_decl_struct(node)->members);
    break;
  default:
    break;
  }
}

/* public */
void
bl_ast_init(bl_ast_t *ast)
{
  ast->nodes = bo_array_new(sizeof(bl_node_t *));
  bo_array_reserve(ast->nodes, 1024);
}

void
bl_ast_terminate(bl_ast_t *ast)
{
  bl_node_t *  node;
  const size_t c = bo_array_size(ast->nodes);
  for (size_t i = 0; i < c; ++i) {
    node = bo_array_at(ast->nodes, i, bl_node_t *);
    node_terminate(node);
  }

  bo_unref(ast->nodes);
}

/*************************************************************************************************
 * node constructors
 *************************************************************************************************/
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
bl_ast_add_type_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref, BArray *path)
{
  bl_node_t *type = alloc_node(ast);
  if (tok)
    type->src = &tok->src;

  type->code                   = BL_TYPE_REF;
  bl_peek_type_ref(type)->ref  = ref;
  bl_peek_type_ref(type)->path = path;

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
bl_ast_add_expr_binop(bl_ast_t *ast, bl_token_t *tok, bl_sym_e op, bl_node_t *lhs, bl_node_t *rhs,
                      bl_node_t *type)
{
  bl_node_t *binop = alloc_node(ast);
  if (tok)
    binop->src = &tok->src;

  binop->code                     = BL_EXPR_BINOP;
  bl_peek_expr_binop(binop)->op   = op;
  bl_peek_expr_binop(binop)->lhs  = lhs;
  bl_peek_expr_binop(binop)->rhs  = rhs;
  bl_peek_expr_binop(binop)->type = type;

  return binop;
}

bl_node_t *
bl_ast_add_expr_decl_ref(bl_ast_t *ast, bl_token_t *tok, bl_node_t *ref, BArray *path)
{
  bl_node_t *decl_ref = alloc_node(ast);
  if (tok)
    decl_ref->src = &tok->src;

  decl_ref->code                        = BL_EXPR_DECL_REF;
  bl_peek_expr_decl_ref(decl_ref)->ref  = ref;
  bl_peek_expr_decl_ref(decl_ref)->path = path;

  return decl_ref;
}

bl_node_t *
bl_ast_add_expr_call(bl_ast_t *ast, bl_token_t *tok, bl_node_t *ref, BArray *path)
{
  bl_node_t *call = alloc_node(ast);
  if (tok)
    call->src = &tok->src;

  call->code                    = BL_EXPR_CALL;
  bl_peek_expr_call(call)->ref  = ref;
  bl_peek_expr_call(call)->path = path;

  return call;
}

bl_node_t *
bl_ast_add_expr_path(bl_ast_t *ast, bl_token_t *tok, const char *name)
{
  bl_node_t *path = alloc_node(ast);
  if (tok)
    path->src = &tok->src;

  path->code = BL_EXPR_PATH;
  bl_id_init(&bl_peek_expr_path(path)->id, name);

  return path;
}

bl_node_t *
bl_ast_add_decl_module(bl_ast_t *ast, bl_token_t *tok, const char *name, int modif)
{
  bl_node_t *module = alloc_node(ast);
  if (tok)
    module->src = &tok->src;

  module->code = BL_DECL_MODULE;
  if (name)
    bl_id_init(&bl_peek_decl_module(module)->id, name);
  bl_peek_decl_module(module)->modif = modif;

  return module;
}

bl_node_t *
bl_ast_add_decl_var(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                    bl_node_t *init_expr, int modif)
{
  bl_node_t *var = alloc_node(ast);
  if (tok)
    var->src = &tok->src;

  var->code                        = BL_DECL_VAR;
  bl_peek_decl_var(var)->init_expr = init_expr;
  bl_peek_decl_var(var)->type      = type;
  bl_peek_decl_var(var)->modif     = modif;
  bl_id_init(&bl_peek_decl_var(var)->id, name);

  return var;
}

bl_node_t *
bl_ast_add_decl_arg(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type)
{
  bl_node_t *arg = alloc_node(ast);
  if (tok)
    arg->src = &tok->src;

  arg->code                   = BL_DECL_VAR;
  bl_peek_decl_var(arg)->type = type;
  bl_id_init(&bl_peek_decl_var(arg)->id, name);

  return arg;
}

bl_node_t *
bl_ast_add_decl_func(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *block,
                     bl_node_t *ret_type, int modif)
{
  bl_node_t *func = alloc_node(ast);
  if (tok)
    func->src = &tok->src;

  func->code                        = BL_DECL_FUNC;
  bl_peek_decl_func(func)->block    = block;
  bl_peek_decl_func(func)->ret_type = ret_type;
  bl_peek_decl_func(func)->modif    = modif;
  bl_id_init(&bl_peek_decl_func(func)->id, name);

  return func;
}

bl_node_t *
bl_ast_add_decl_struct(bl_ast_t *ast, bl_token_t *tok, const char *name, int modif)
{
  bl_node_t *strct = alloc_node(ast);
  if (tok)
    strct->src = &tok->src;

  strct->code = BL_DECL_STRUCT;
  bl_id_init(&bl_peek_decl_struct(strct)->id, name);
  bl_peek_decl_struct(strct)->modif = modif;

  return strct;
}

bl_node_t *
bl_ast_add_decl_struct_member(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                              int modif)
{
  bl_node_t *member = alloc_node(ast);
  if (tok)
    member->src = &tok->src;

  member->code                     = BL_DECL_STRUCT_MEMBER;
  bl_decl_struct_member_t *_member = bl_peek_decl_struct_member(member);
  _member->type                    = type;
  _member->modif                   = modif;
  bl_id_init(&_member->id, name);

  return member;
}

bl_node_t *
bl_ast_add_decl_enum(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type, int modif)
{
  bl_node_t *enm = alloc_node(ast);
  if (tok)
    enm->src = &tok->src;

  enm->code = BL_DECL_ENUM;
  bl_id_init(&bl_peek_decl_enum(enm)->id, name);
  bl_peek_decl_enum(enm)->modif = modif;
  bl_peek_decl_enum(enm)->type  = type;

  return enm;
}

bl_node_t *
bl_ast_add_decl_enum_variant(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *expr,
                             bl_node_t *parent)
{
  bl_node_t *variant = alloc_node(ast);
  if (tok)
    variant->src = &tok->src;

  variant->code = BL_DECL_ENUM_VARIANT;
  bl_id_init(&bl_peek_decl_enum_variant(variant)->id, name);
  bl_peek_decl_enum_variant(variant)->expr   = expr;
  bl_peek_decl_enum_variant(variant)->parent = parent;

  return variant;
}

bl_node_t *
bl_ast_add_decl_block(bl_ast_t *ast, bl_token_t *tok, bl_node_t *parent)
{
  bl_node_t *block = alloc_node(ast);
  if (tok)
    block->src = &tok->src;

  block->code                       = BL_DECL_BLOCK;
  bl_peek_decl_block(block)->parent = parent;

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
bl_ast_add_stmt_loop(bl_ast_t *ast, bl_token_t *tok, bl_node_t *test, bl_node_t *true_stmt)
{
  bl_node_t *loop_stmt = alloc_node(ast);
  if (tok)
    loop_stmt->src = &tok->src;

  loop_stmt->code                         = BL_STMT_LOOP;
  bl_peek_stmt_loop(loop_stmt)->true_stmt = true_stmt;
  bl_peek_stmt_loop(loop_stmt)->test      = test;

  return loop_stmt;
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

/*************************************************************************************************
 * module
 *************************************************************************************************/
bl_node_t *
bl_ast_module_push_node(bl_decl_module_t *module, bl_node_t *node)
{
  if (node == NULL)
    return NULL;

  if (module->nodes == NULL) {
    module->nodes = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(module->nodes, node);
  return node;
}

size_t
bl_ast_module_node_count(bl_decl_module_t *module)
{
  if (module->nodes == NULL)
    return 0;
  return bo_array_size(module->nodes);
}

bl_node_t *
bl_ast_module_get_node(bl_decl_module_t *module, size_t i)
{
  if (module->nodes == NULL)
    return NULL;
  return bo_array_at(module->nodes, i, bl_node_t *);
}

/*************************************************************************************************
 * function
 *************************************************************************************************/
bl_node_t *
bl_ast_func_push_arg(bl_decl_func_t *func, bl_node_t *arg)
{
  if (arg == NULL)
    return NULL;

  if (func->args == NULL) {
    func->args = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(func->args, arg);
  return arg;
}

size_t
bl_ast_func_arg_count(bl_decl_func_t *func)
{
  if (func->args == NULL)
    return 0;
  return bo_array_size(func->args);
}

bl_node_t *
bl_ast_func_get_arg(bl_decl_func_t *func, const size_t i)
{
  if (func->args == NULL)
    return NULL;
  return bo_array_at(func->args, i, bl_node_t *);
}

/*************************************************************************************************
 * block
 *************************************************************************************************/
bl_node_t *
bl_ast_block_push_node(bl_decl_block_t *block, bl_node_t *node)
{
  if (node == NULL)
    return NULL;

  if (block->nodes == NULL) {
    block->nodes = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(block->nodes, node);
  return node;
}

size_t
bl_ast_block_node_count(bl_decl_block_t *block)
{
  if (block->nodes == NULL)
    return 0;
  return bo_array_size(block->nodes);
}

bl_node_t *
bl_ast_block_get_node(bl_decl_block_t *block, const size_t i)
{
  if (block->nodes == NULL)
    return NULL;
  return bo_array_at(block->nodes, i, bl_node_t *);
}

/*************************************************************************************************
 * call
 *************************************************************************************************/
bl_node_t *
bl_ast_call_push_arg(bl_expr_call_t *call, bl_node_t *arg)
{
  if (arg == NULL)
    return NULL;

  if (call->args == NULL) {
    call->args = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(call->args, arg);
  return arg;
}

size_t
bl_ast_call_arg_count(bl_expr_call_t *call)
{
  if (call->args == NULL)
    return 0;

  return bo_array_size(call->args);
}

bl_node_t *
bl_ast_call_get_arg(bl_expr_call_t *call, const size_t i)
{
  if (call->args == NULL)
    return NULL;
  return bo_array_at(call->args, i, bl_node_t *);
}

/*************************************************************************************************
 * struct
 *************************************************************************************************/
bl_node_t *
bl_ast_struct_push_member(bl_decl_struct_t *strct, bl_node_t *member)
{
  if (member == NULL)
    return NULL;

  if (strct->members == NULL) {
    strct->members = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(strct->members, member);
  return member;
}

size_t
bl_ast_struct_member_count(bl_decl_struct_t *strct)
{
  if (strct->members == NULL)
    return 0;

  return bo_array_size(strct->members);
}

bl_node_t *
bl_ast_struct_get_member(bl_decl_struct_t *strct, const size_t i)
{
  if (strct->members == NULL)
    return NULL;
  return bo_array_at(strct->members, i, bl_node_t *);
}

/*************************************************************************************************
 * enum
 *************************************************************************************************/
bl_node_t *
bl_ast_enum_insert_variant(bl_decl_enum_t *enm, bl_node_t *variant)
{
  if (variant == NULL)
    return NULL;

  if (enm->variants == NULL) {
    enm->variants = bo_htbl_new(sizeof(bl_node_t *), 128);
  }

  bo_htbl_insert(enm->variants, bl_peek_decl_enum_variant(variant)->id.hash, variant);
  return variant;
}

bl_node_t *
bl_ast_enum_get_variant(bl_decl_enum_t *enm, bl_id_t *id)
{
  if (enm->variants == NULL)
    return NULL;

  if (!bo_htbl_has_key(enm->variants, id->hash))
    return NULL;

  return bo_htbl_at(enm->variants, id->hash, bl_node_t *);
}

/*************************************************************************************************
 * other
 *************************************************************************************************/
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
  case BL_DECL_STRUCT_MEMBER:
    return &bl_peek_decl_struct_member(node)->id;
  case BL_DECL_ENUM_VARIANT:
    return &bl_peek_decl_enum_variant(node)->id;
  case BL_DECL_FUNC:
    return &bl_peek_decl_func(node)->id;
  case BL_DECL_STRUCT:
    return &bl_peek_decl_struct(node)->id;
  case BL_DECL_ENUM:
    return &bl_peek_decl_enum(node)->id;
  default:
    return NULL;
  }
}

int
bl_ast_try_get_modif(bl_node_t *node)
{
  if (node == NULL) {
    return BL_MODIF_NONE;
  }

  switch (bl_node_code(node)) {
  case BL_DECL_MODULE:
    return bl_peek_decl_module(node)->modif;
  case BL_DECL_FUNC:
    return bl_peek_decl_func(node)->modif;
  case BL_DECL_STRUCT:
    return bl_peek_decl_struct(node)->modif;
  case BL_DECL_ENUM:
    return bl_peek_decl_enum(node)->modif;
  default:
    return BL_MODIF_NONE;
  }
}

size_t
bl_ast_node_count(bl_ast_t *ast)
{
  return bo_array_size(ast->nodes);
}

bl_node_t *
bl_ast_get_node(bl_ast_t *ast, size_t i)
{
  return bo_array_at(ast->nodes, i, bl_node_t *);
}

bool
bl_type_compatible(bl_node_t *first, bl_node_t *second)
{
  if (!first || !second)
    return false;

  bl_assert(bl_node_is(first, BL_TYPE_REF) || bl_node_is(first, BL_TYPE_FUND), "not type");
  bl_assert(bl_node_is(second, BL_TYPE_REF) || bl_node_is(second, BL_TYPE_FUND), "not type");

  if (first->code != second->code)
    return false;

  if (bl_node_is(first, BL_TYPE_FUND) &&
      bl_peek_type_fund(first)->type == bl_peek_type_fund(second)->type)
    return true;

  if (bl_node_is(first, BL_TYPE_REF) &&
      bl_peek_type_ref(first)->ref == bl_peek_type_ref(second)->ref)
    return true;

  return false;
}

const char *
bl_ast_try_get_type_name(bl_node_t *type)
{
  switch (bl_node_code(type)) {
  case BL_TYPE_FUND:
    return bl_fund_type_strings[bl_peek_type_fund(type)->type];
  case BL_TYPE_REF: {
    bl_node_t *ref = bl_peek_type_ref(type)->ref;
    switch (bl_node_code(ref)) {
    case BL_DECL_ENUM:
      return bl_peek_decl_enum(ref)->id.str;
    case BL_DECL_STRUCT:
      return bl_peek_decl_struct(ref)->id.str;
    default:
      bl_abort("invalid reference to type %s", bl_node_name(ref));
    }

    break;
  }
  default:
    return NULL;
  }
}
/**************************************************************************************************/
