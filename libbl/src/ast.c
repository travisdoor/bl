//************************************************************************************************
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
//************************************************************************************************

#include <bobject/containers/hash.h>
#include "ast_impl.h"

#define CHUNK_SIZE 256
static bool buildin_hashes_initialized = false;

typedef struct chunk
{
  struct chunk *next;
  int           count;
} chunk_t;

const char *bl_fund_type_strings[] = {
#define ft(tok, str) str,
    BL_FUND_TYPE_LIST
#undef ft
};

const char *bl_node_type_strings[] = {
#define nt(code, name) #name,
    BL_NODE_TYPE_LIST
#undef nt
};

const char *bl_buildin_strings[] = {
#define bt(code, name) #name,
    BL_SPEC_BUILINS
#undef nt
};

static uint64_t buildin_hashes[BL_BUILDIN_COUNT] = {0};

static void
node_terminate(bl_node_t *node)
{
  switch (node->code) {
  case BL_DECL_MODULE:
    bl_scopes_terminate(&bl_peek_decl_module(node)->scopes);
    break;
  case BL_DECL_FUNC:
    bl_scopes_terminate(&bl_peek_decl_func(node)->scopes);
    break;
  case BL_DECL_BLOCK:
    bl_scopes_terminate(&bl_peek_decl_block(node)->scopes);
    break;
  case BL_DECL_ENUM:
    bl_scopes_terminate(&bl_peek_decl_enum(node)->scopes);
    break;
  case BL_DECL_STRUCT:
    bl_scopes_terminate(&bl_peek_decl_struct(node)->scopes);
    break;
  default:
    break;
  }
}

static inline bl_node_t *
get_node_in_chunk(chunk_t *chunk, int i)
{
  return (bl_node_t *)((char *)chunk + (i * sizeof(bl_node_t)));
}

static inline chunk_t *
alloc_chunk(void)
{
  const size_t size_in_bytes = sizeof(bl_node_t) * CHUNK_SIZE;
  chunk_t *    chunk         = bl_malloc(size_in_bytes);
  memset(chunk, 0, size_in_bytes);
  chunk->count = 1;
  return chunk;
}

static inline chunk_t *
free_chunk(chunk_t *chunk)
{
  if (!chunk)
    return NULL;

  chunk_t *next = chunk->next;

  for (int i = 0; i < chunk->count - 1; ++i) {
    node_terminate(get_node_in_chunk(chunk, i + 1));
  }
  bl_free(chunk);
  return next;
}

static bl_node_t *
alloc_node(bl_ast_t *ast)
{
  if (!ast->current_chunk) {
    ast->current_chunk = alloc_chunk();
    ast->first_chunk   = ast->current_chunk;
  }

  if (ast->current_chunk->count == CHUNK_SIZE) {
    // last chunk node
    chunk_t *chunk           = alloc_chunk();
    ast->current_chunk->next = chunk;
    ast->current_chunk       = chunk;
  }

  bl_node_t *node = get_node_in_chunk(ast->current_chunk, ast->current_chunk->count);
  ast->current_chunk->count++;

  return node;
}

/* public */
void
bl_ast_init(bl_ast_t *ast)
{
  ast->first_chunk   = NULL;
  ast->current_chunk = NULL;
}

void
bl_ast_terminate(bl_ast_t *ast)
{
  chunk_t *chunk = ast->first_chunk;
  while (chunk) {
    chunk = free_chunk(chunk);
  }
}

/*************************************************************************************************
 * node constructors
 *************************************************************************************************/
bl_node_t *
bl_ast_add_type_fund(bl_ast_t *ast, bl_token_t *tok, bl_fund_type_e t, int is_ptr)
{
  bl_node_t *type = alloc_node(ast);
  if (tok)
    type->src = &tok->src;

  type->code                      = BL_TYPE_FUND;
  bl_peek_type_fund(type)->type   = t;
  bl_peek_type_fund(type)->is_ptr = is_ptr;

  return type;
}

bl_node_t *
bl_ast_add_type_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref,
                    bl_node_t *path, int is_ptr)
{
  bl_node_t *type = alloc_node(ast);
  if (tok)
    type->src = &tok->src;

  type->code                     = BL_TYPE_REF;
  bl_peek_type_ref(type)->ref    = ref;
  bl_peek_type_ref(type)->path   = path;
  bl_peek_type_ref(type)->is_ptr = is_ptr;

  return type;
}

bl_node_t *
bl_ast_add_pre_load(bl_ast_t *ast, bl_token_t *tok, const char *filepath)
{
  bl_node_t *load = alloc_node(ast);
  if (tok)
    load->src = &tok->src;

  load->code                       = BL_PRE_LOAD;
  bl_peek_pre_load(load)->filepath = filepath;
  return load;
}

bl_node_t *
bl_ast_add_pre_link(bl_ast_t *ast, bl_token_t *tok, const char *lib)
{
  bl_node_t *link = alloc_node(ast);
  if (tok)
    link->src = &tok->src;

  link->code                  = BL_PRE_LINK;
  bl_peek_pre_link(link)->lib = lib;
  return link;
}

bl_node_t *
bl_ast_add_expr_sizeof(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type)
{
  bl_node_t *expr_sizeof = alloc_node(ast);
  if (tok)
    expr_sizeof->src = &tok->src;

  expr_sizeof->code                          = BL_EXPR_SIZEOF;
  bl_peek_expr_sizeof(expr_sizeof)->des_type = type;
  bl_peek_expr_sizeof(expr_sizeof)->type     = bl_ast_add_type_fund(ast, NULL, BL_FTYPE_SIZE, 0);
  return expr_sizeof;
}

bl_node_t *
bl_ast_add_expr_cast(bl_ast_t *ast, bl_token_t *tok, bl_node_t *to_type, bl_node_t *next)
{
  bl_node_t *cast = alloc_node(ast);
  if (tok)
    cast->src = &tok->src;

  cast->code            = BL_EXPR_CAST;
  bl_expr_cast_t *_cast = bl_peek_expr_cast(cast);
  _cast->next           = next;
  _cast->type           = to_type;
  return cast;
}

bl_node_t *
bl_ast_add_expr_init(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type)
{
  bl_node_t *init = alloc_node(ast);
  if (tok)
    init->src = &tok->src;

  init->code            = BL_EXPR_INIT;
  bl_expr_init_t *_init = bl_peek_expr_init(init);
  _init->type           = type;
  return init;
}

bl_node_t *
bl_ast_add_expr_null(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type)
{
  bl_node_t *expr_null = alloc_node(ast);
  if (tok)
    expr_null->src = &tok->src;

  expr_null->code                    = BL_EXPR_NULL;
  bl_peek_expr_null(expr_null)->type = type;
  return expr_null;
}

bl_node_t *
bl_ast_add_expr_literal(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type)
{
  bl_node_t *cnst = alloc_node(ast);
  if (tok)
    cnst->src = &tok->src;

  cnst->code                       = BL_EXPR_LITERAL;
  bl_peek_expr_literal(cnst)->type = type;

  return cnst;
}

bl_node_t *
bl_ast_add_expr_literal_char(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, char c)
{
  bl_node_t *expr_literal                     = bl_ast_add_expr_literal(ast, tok, type);
  bl_peek_expr_literal(expr_literal)->value.c = c;

  return expr_literal;
}

bl_node_t *
bl_ast_add_expr_literal_bool(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, bool b)
{
  bl_node_t *expr_literal                     = bl_ast_add_expr_literal(ast, tok, type);
  bl_peek_expr_literal(expr_literal)->value.b = b;

  return expr_literal;
}

bl_node_t *
bl_ast_add_expr_literal_signed(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, long long s)
{
  bl_node_t *expr_literal                     = bl_ast_add_expr_literal(ast, tok, type);
  bl_peek_expr_literal(expr_literal)->value.s = s;

  return expr_literal;
}

bl_node_t *
bl_ast_add_expr_literal_unsigned(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type,
                                 unsigned long long u)
{
  bl_node_t *expr_literal                     = bl_ast_add_expr_literal(ast, tok, type);
  bl_peek_expr_literal(expr_literal)->value.u = u;

  return expr_literal;
}

bl_node_t *
bl_ast_add_expr_literal_double(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, double f)
{
  bl_node_t *expr_literal                     = bl_ast_add_expr_literal(ast, tok, type);
  bl_peek_expr_literal(expr_literal)->value.f = f;

  return expr_literal;
}

bl_node_t *
bl_ast_add_expr_literal_str(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, const char *str)
{
  bl_node_t *expr_literal                       = bl_ast_add_expr_literal(ast, tok, type);
  bl_peek_expr_literal(expr_literal)->value.str = str;

  return expr_literal;
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
bl_ast_add_expr_unary(bl_ast_t *ast, bl_token_t *tok, bl_sym_e op, bl_node_t *next)
{
  bl_node_t *unary = alloc_node(ast);
  if (tok)
    unary->src = &tok->src;

  unary->code                     = BL_EXPR_UNARY;
  bl_peek_expr_unary(unary)->op   = op;
  bl_peek_expr_unary(unary)->next = next;
  return unary;
}

bl_node_t *
bl_ast_add_expr_decl_ref(bl_ast_t *ast, bl_token_t *tok, bl_node_t *ref, bl_node_t *path)
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
bl_ast_add_expr_member_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *next,
                           bl_node_t *ref, bool is_ptr_ref)
{
  bl_node_t *member_ref = alloc_node(ast);
  if (tok)
    member_ref->src = &tok->src;

  member_ref->code                  = BL_EXPR_MEMBER_REF;
  bl_expr_member_ref_t *_member_ref = bl_peek_expr_member_ref(member_ref);
  bl_id_init(&_member_ref->id, name);
  _member_ref->next       = next;
  _member_ref->ref        = ref;
  _member_ref->is_ptr_ref = is_ptr_ref;

  return member_ref;
}

bl_node_t *
bl_ast_add_expr_array_ref(bl_ast_t *ast, bl_token_t *tok, bl_node_t *index, bl_node_t *next)
{
  bl_node_t *array_ref = alloc_node(ast);
  if (tok)
    array_ref->src = &tok->src;

  array_ref->code                 = BL_EXPR_ARRAY_REF;
  bl_expr_array_ref_t *_array_ref = bl_peek_expr_array_ref(array_ref);
  _array_ref->index               = index;
  _array_ref->next                = next;

  return array_ref;
}

bl_node_t *
bl_ast_add_expr_call(bl_ast_t *ast, bl_token_t *tok, bl_node_t *ref, bl_node_t *path,
                     bool run_in_compile_time)
{
  bl_node_t *call = alloc_node(ast);
  if (tok)
    call->src = &tok->src;

  call->code                                   = BL_EXPR_CALL;
  bl_peek_expr_call(call)->ref                 = ref;
  bl_peek_expr_call(call)->path                = path;
  bl_peek_expr_call(call)->run_in_compile_time = run_in_compile_time;

  return call;
}

bl_node_t *
bl_ast_add_path_elem(bl_ast_t *ast, bl_token_t *tok, const char *name)
{
  bl_node_t *path = alloc_node(ast);
  if (tok)
    path->src = &tok->src;

  path->code = BL_PATH_ELEM;
  bl_id_init(&bl_peek_path_elem(path)->id, name);

  return path;
}

bl_node_t *
bl_ast_add_decl_module(bl_ast_t *ast, bl_token_t *tok, const char *name, int modif,
                       bl_node_t *parent)
{
  bl_node_t *module = alloc_node(ast);
  if (tok)
    module->src = &tok->src;

  module->code = BL_DECL_MODULE;
  if (name)
    bl_id_init(&bl_peek_decl_module(module)->id, name);
  bl_peek_decl_module(module)->modif  = modif;
  bl_peek_decl_module(module)->parent = parent;
  bl_scopes_init(&bl_peek_decl_module(module)->scopes);

  return module;
}

bl_node_t *
bl_ast_add_decl_mut(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                    bl_node_t *init_expr, int modif, bool is_anonymous)
{
  bl_node_t *mut = alloc_node(ast);
  if (tok)
    mut->src = &tok->src;

  mut->code                           = BL_DECL_MUT;
  bl_peek_decl_mut(mut)->init_expr    = init_expr;
  bl_peek_decl_mut(mut)->type         = type;
  bl_peek_decl_mut(mut)->modif        = modif;
  bl_peek_decl_mut(mut)->is_anonymous = is_anonymous;
  bl_id_init(&bl_peek_decl_mut(mut)->id, name);

  return mut;
}

bl_node_t *
bl_ast_add_decl_const(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                      bl_node_t *init_expr, int modif)
{
  bl_node_t *cnst = alloc_node(ast);
  if (tok)
    cnst->src = &tok->src;

  cnst->code             = BL_DECL_CONST;
  bl_decl_const_t *_cnst = bl_peek_decl_const(cnst);
  _cnst->init_expr       = init_expr;
  _cnst->type            = type;
  _cnst->modif           = modif;
  bl_id_init(&_cnst->id, name);

  return cnst;
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
                     bl_node_t *ret_type, int modif, bl_node_t *parent, bool gen_in_compiletime)
{
  bl_node_t *func = alloc_node(ast);
  if (tok)
    func->src = &tok->src;

  func->code                = BL_DECL_FUNC;
  bl_decl_func_t *_func     = bl_peek_decl_func(func);
  _func->block              = block;
  _func->ret_type           = ret_type;
  _func->modif              = modif;
  _func->parent             = parent;
  _func->gen_in_compiletime = gen_in_compiletime;
  bl_id_init(&(_func->id), name);
  bl_scopes_init(&(_func->scopes));

  return func;
}

bl_node_t *
bl_ast_add_decl_struct(bl_ast_t *ast, bl_token_t *tok, const char *name, int modif)
{
  bl_node_t *strct = alloc_node(ast);
  if (tok)
    strct->src = &tok->src;

  strct->code              = BL_DECL_STRUCT;
  bl_decl_struct_t *_strct = bl_peek_decl_struct(strct);
  bl_id_init(&_strct->id, name);
  bl_scopes_init(&_strct->scopes);
  _strct->modif = modif;

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
bl_ast_add_decl_enum(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type, int modif,
                     bl_node_t *parent)
{
  bl_node_t *enm = alloc_node(ast);
  if (tok)
    enm->src = &tok->src;

  enm->code            = BL_DECL_ENUM;
  bl_decl_enum_t *_enm = bl_peek_decl_enum(enm);
  bl_id_init(&_enm->id, name);
  bl_scopes_init(&_enm->scopes);
  _enm->modif  = modif;
  _enm->type   = type;
  _enm->parent = parent;

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
  bl_peek_decl_enum_variant(variant)->type   = bl_peek_decl_enum(parent)->type;

  return variant;
}

bl_node_t *
bl_ast_add_decl_block(bl_ast_t *ast, bl_token_t *tok, bl_node_t *parent)
{
  bl_node_t *block = alloc_node(ast);
  if (tok)
    block->src = &tok->src;

  block->code             = BL_DECL_BLOCK;
  bl_decl_block_t *_block = bl_peek_decl_block(block);
  _block->parent          = parent;
  bl_scopes_init(&(_block->scopes));

  return block;
}

bl_node_t *
bl_ast_add_stmt_if(bl_ast_t *ast, bl_token_t *tok, bl_node_t *test, bl_node_t *true_stmt,
                   bl_node_t *false_stmt, bl_node_t *parent)
{
  bl_node_t *if_stmt = alloc_node(ast);
  if (tok)
    if_stmt->src = &tok->src;

  if_stmt->code     = BL_STMT_IF;
  bl_stmt_if_t *_if = bl_peek_stmt_if(if_stmt);
  _if->test         = test;
  _if->true_stmt    = true_stmt;
  _if->false_stmt   = false_stmt;
  _if->parent       = parent;

  return if_stmt;
}

bl_node_t *
bl_ast_add_stmt_loop(bl_ast_t *ast, bl_token_t *tok, bl_node_t *test, bl_node_t *true_stmt,
                     bl_node_t *parent)
{
  bl_node_t *loop_stmt = alloc_node(ast);
  if (tok)
    loop_stmt->src = &tok->src;

  loop_stmt->code       = BL_STMT_LOOP;
  bl_stmt_loop_t *_loop = bl_peek_stmt_loop(loop_stmt);
  _loop->true_stmt      = true_stmt;
  _loop->test           = test;
  _loop->parent         = parent;

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
bl_ast_add_stmt_return(bl_ast_t *ast, bl_token_t *tok, bl_node_t *expr, bl_node_t *func)
{
  bl_node_t *return_stmt = alloc_node(ast);
  if (tok)
    return_stmt->src = &tok->src;

  return_stmt->code                      = BL_STMT_RETURN;
  bl_peek_stmt_return(return_stmt)->expr = expr;
  bl_peek_stmt_return(return_stmt)->func = func;
  return return_stmt;
}

bl_node_t *
bl_ast_add_stmt_using(bl_ast_t *ast, bl_token_t *tok, bl_node_t *path)
{
  bl_node_t *using_stmt = alloc_node(ast);
  if (tok)
    using_stmt->src = &tok->src;

  using_stmt->code                     = BL_STMT_USING;
  bl_peek_stmt_using(using_stmt)->path = path;
  return using_stmt;
}

/*************************************************************************************************
 * other
 *************************************************************************************************/
bl_id_t *
bl_ast_get_id(bl_node_t *node)
{
  if (node == NULL) {
    return NULL;
  }

  switch (bl_node_code(node)) {
  case BL_DECL_MODULE:
    return &bl_peek_decl_module(node)->id;
  case BL_DECL_MUT:
    return &bl_peek_decl_mut(node)->id;
  case BL_DECL_ARG:
    return &bl_peek_decl_arg(node)->id;
  case BL_DECL_CONST:
    return &bl_peek_decl_const(node)->id;
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
  case BL_PATH_ELEM:
    return &bl_peek_path_elem(node)->id;
  case BL_EXPR_MEMBER_REF:
    return &bl_peek_expr_member_ref(node)->id;
  default:
    return NULL;
  }
}

int
bl_ast_get_modif(bl_node_t *node)
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
  case BL_DECL_STRUCT_MEMBER:
    return bl_peek_decl_struct_member(node)->modif;
  case BL_DECL_ENUM:
    return bl_peek_decl_enum(node)->modif;
  case BL_DECL_CONST:
    return bl_peek_decl_const(node)->modif;
  case BL_DECL_ENUM_VARIANT:
    return BL_MODIF_PUBLIC;
  default:
    return BL_MODIF_NONE;
  }
}

bool
bl_ast_type_compatible(bl_node_t *first, bl_node_t *second)
{
  if (!first || !second)
    return false;

  bl_assert(bl_node_is(first, BL_TYPE_REF) || bl_node_is(first, BL_TYPE_FUND), "not type");
  bl_assert(bl_node_is(second, BL_TYPE_REF) || bl_node_is(second, BL_TYPE_FUND), "not type");

  if (first->code != second->code)
    return false;

  if (bl_node_is(first, BL_TYPE_FUND)) {
    bl_type_fund_t *_first  = bl_peek_type_fund(first);
    bl_type_fund_t *_second = bl_peek_type_fund(second);
    return _first->type == _second->type && _first->is_ptr == _second->is_ptr;
  } else if (bl_node_is(first, BL_TYPE_REF)) {
    bl_type_ref_t *_first  = bl_peek_type_ref(first);
    bl_type_ref_t *_second = bl_peek_type_ref(second);
    return _first->ref == _second->ref && _first->is_ptr == _second->is_ptr;
  }

  return false;
}

void
bl_ast_get_type_name(bl_node_t *type, char *out_name, int max_len)
{
  bl_assert(max_len, "invalid max_len of buffer");
  bl_assert(out_name, "invalid out_name buffer");

  const char *tmp = NULL;
  int         is_ptr;

  switch (bl_node_code(type)) {
  case BL_TYPE_FUND:
    tmp    = bl_fund_type_strings[bl_peek_type_fund(type)->type];
    is_ptr = bl_peek_type_fund(type)->is_ptr;
    break;
  case BL_TYPE_REF: {
    bl_node_t *ref = bl_peek_type_ref(type)->ref;
    is_ptr         = bl_peek_type_ref(type)->is_ptr;
    switch (bl_node_code(ref)) {
    case BL_DECL_ENUM:
      tmp = bl_peek_decl_enum(ref)->id.str;
      break;
    case BL_DECL_STRUCT:
      tmp = bl_peek_decl_struct(ref)->id.str;
      break;
    default:
      bl_abort("invalid reference to type %s", bl_node_name(ref));
    }
    break;
  }
  default:
    bl_abort("invalid node %s", bl_node_name(type));
  }

  bl_assert(tmp, "invalid tmp name");
  if (strlen(tmp) + is_ptr + 1 > max_len) {
    snprintf(out_name, max_len, "%s", tmp);
    return;
  }

  memset(out_name, '*', is_ptr);
  out_name[is_ptr] = '\0';
  strcat(out_name, tmp);
}

bl_node_t **
bl_ast_get_type_dim(bl_node_t *type)
{
  switch (bl_node_code(type)) {
  case BL_TYPE_FUND:
    return &bl_peek_type_fund(type)->dim;
  case BL_TYPE_REF: {
    return &bl_peek_type_ref(type)->dim;
  }
  default:
    return NULL;
  }
}

int
bl_ast_type_is_ptr(bl_node_t *type)
{
  switch (bl_node_code(type)) {
  case BL_TYPE_FUND:
    return bl_peek_type_fund(type)->is_ptr;
  case BL_TYPE_REF: {
    return bl_peek_type_ref(type)->is_ptr;
  }
  default:
    return -1;
  }
}

bl_node_t *
bl_ast_path_get_last(bl_node_t *path)
{
  if (!path)
    return NULL;

  bl_node_t *last = path;
  while (true) {
    if (!last->next)
      return last;

    last = last->next;
  }
}

bl_scopes_t *
bl_ast_get_scopes(bl_node_t *node)
{
  switch (bl_node_code(node)) {
  case BL_DECL_MODULE:
    return &bl_peek_decl_module(node)->scopes;
  case BL_DECL_BLOCK:
    return &bl_peek_decl_block(node)->scopes;
  case BL_DECL_FUNC:
    return &bl_peek_decl_func(node)->scopes;
  case BL_DECL_ENUM:
    return &bl_peek_decl_enum(node)->scopes;
  case BL_DECL_STRUCT:
    return &bl_peek_decl_struct(node)->scopes;
  default:
    bl_abort("cannot get scopes of %s", bl_node_name(node));
  }
}

bl_node_t *
bl_ast_get_parent(bl_node_t *node)
{
  switch (bl_node_code(node)) {
  case BL_DECL_MODULE:
    return bl_peek_decl_module(node)->parent;
  case BL_DECL_BLOCK:
    return bl_peek_decl_block(node)->parent;
  case BL_DECL_FUNC:
    return bl_peek_decl_func(node)->parent;
  case BL_DECL_ENUM:
    return bl_peek_decl_enum(node)->parent;
  case BL_STMT_IF:
    return bl_peek_stmt_if(node)->parent;
  case BL_STMT_LOOP:
    return bl_peek_stmt_loop(node)->parent;
  default:
    bl_abort("cannot get parent of %s", bl_node_name(node));
  }
}

bl_node_t *
bl_ast_get_type(bl_node_t *node)
{
  if (!node)
    return NULL;

  switch (bl_node_code(node)) {
  case BL_EXPR_NULL:
    return bl_peek_expr_null(node)->type;
  case BL_EXPR_CAST:
    return bl_peek_expr_cast(node)->type;
  case BL_EXPR_INIT:
    return bl_peek_expr_init(node)->type;
  case BL_EXPR_LITERAL:
    return bl_peek_expr_literal(node)->type;
  case BL_EXPR_BINOP:
    return bl_peek_expr_binop(node)->type;
  case BL_EXPR_DECL_REF:
    return bl_peek_expr_decl_ref(node)->type;
  case BL_EXPR_UNARY:
    return bl_peek_expr_unary(node)->type;
  case BL_EXPR_MEMBER_REF:
    return bl_peek_expr_member_ref(node)->type;
  case BL_EXPR_CALL:
    return bl_peek_expr_call(node)->type;
  case BL_EXPR_ARRAY_REF:
    return bl_peek_expr_array_ref(node)->type;
  case BL_EXPR_SIZEOF:
    return bl_peek_expr_sizeof(node)->type;

  case BL_DECL_MUT:
    return bl_peek_decl_mut(node)->type;
  case BL_DECL_CONST:
    return bl_peek_decl_const(node)->type;
  case BL_DECL_ARG:
    return bl_peek_decl_arg(node)->type;
  case BL_DECL_FUNC:
    return bl_peek_decl_func(node)->ret_type;
  case BL_DECL_STRUCT_MEMBER:
    return bl_peek_decl_struct_member(node)->type;
  case BL_DECL_ENUM:
    return bl_peek_decl_enum(node)->type;
  case BL_DECL_ENUM_VARIANT:
    return bl_peek_decl_enum_variant(node)->type;

  case BL_STMT_RETURN:
    return bl_ast_get_type(bl_peek_stmt_return(node)->func);

  default:
    return NULL;
  }
}

bl_node_t *
bl_ast_dup_node(bl_ast_t *ast, bl_node_t *node)
{
  bl_assert(node, "cannot duplicate node");
  bl_node_t *dup = alloc_node(ast);
  memcpy(dup, node, sizeof(bl_node_t));
  return dup;
}

void
bl_ast_dup_and_insert(bl_ast_t *ast, bl_node_t **dest, bl_node_t *src)
{
  if (!dest || !src)
    return;

  bl_node_t *tmp = *dest;
  *dest = bl_ast_dup_node(ast, src);

  (*dest)->next = tmp->next;
  (*dest)->prev = tmp->prev;
}

bl_type_kind_e
bl_ast_type_get_kind(bl_node_t *type)
{
  if (!type)
    return BL_UNKNOWN_KIND;

  switch (bl_node_code(type)) {
  case BL_TYPE_FUND:
    if (bl_peek_type_fund(type)->is_ptr)
      return BL_PTR_KIND;

    switch (bl_peek_type_fund(type)->type) {
    case BL_FTYPE_I8:
    case BL_FTYPE_I16:
    case BL_FTYPE_I32:
    case BL_FTYPE_I64:
      return BL_SINT_KIND;
    case BL_FTYPE_U8:
    case BL_FTYPE_U16:
    case BL_FTYPE_U32:
    case BL_FTYPE_U64:
      return BL_UINT_KIND;
    case BL_FTYPE_F32:
    case BL_FTYPE_F64:
      return BL_REAL_KIND;
    case BL_FTYPE_STRING:
      return BL_STR_KIND;
    case BL_FTYPE_CHAR:
      return BL_CHAR_KIND;
    case BL_FTYPE_BOOL:
      return BL_BOOL_KIND;
    case BL_FTYPE_VOID:
      return BL_VOID_KIND;
    case BL_FTYPE_SIZE:
      return BL_SIZE_KIND;
    default:
      return BL_UNKNOWN_KIND;
    }
    break;
  case BL_TYPE_REF:
    if (bl_peek_type_ref(type)->is_ptr)
      return BL_PTR_KIND;

    return BL_STRUCT_KIND;
  default:
    return BL_UNKNOWN_KIND;
  }
}

void
bl_ast_type_addrof(bl_node_t *type)
{
  switch (bl_node_code(type)) {
  case BL_TYPE_FUND:
    bl_peek_type_fund(type)->is_ptr++;
    break;
  case BL_TYPE_REF:
    bl_peek_type_ref(type)->is_ptr++;
    break;
  default:
    bl_abort("invalid type");
    break;
  }
}

void
bl_ast_type_deref(bl_node_t *type)
{
  switch (bl_node_code(type)) {
  case BL_TYPE_FUND:
    bl_peek_type_fund(type)->is_ptr--;
    break;
  case BL_TYPE_REF:
    bl_peek_type_ref(type)->is_ptr--;
    break;
  default:
    bl_abort("invalid type");
    break;
  }
}

void
bl_ast_type_remove_dim(bl_node_t *type)
{
  switch (bl_node_code(type)) {
  case BL_TYPE_FUND:
    bl_peek_type_fund(type)->dim = NULL;
    break;
  case BL_TYPE_REF:
    bl_peek_type_ref(type)->dim = NULL;
    break;
  default:
    bl_abort("invalid type");
    break;
  }
}

void
bl_ast_dup_node_buf(bl_node_t *dest, bl_node_t *node)
{
  memcpy(dest, node, sizeof(bl_node_t));
}

bool
bl_ast_type_is_fund(bl_node_t *type, bl_fund_type_e t)
{
  if (!type)
    return false;

  if (bl_node_is_not(type, BL_TYPE_FUND))
    return false;

  return bl_peek_type_fund(type)->type == t;
}

bool
bl_ast_type_is_ref(bl_node_t *type, bl_node_code_e t)
{
  if (!type)
    return false;

  if (bl_node_is_not(type, BL_TYPE_REF))
    return false;

  return bl_node_is(bl_peek_type_ref(type)->ref, t);
}

bool
bl_ast_node_is_const(bl_node_t *node)
{
  if (!node)
    return false;

  switch (bl_node_code(node)) {
  case BL_EXPR_LITERAL:
  case BL_EXPR_BINOP:
  case BL_EXPR_UNARY:
  case BL_DECL_ENUM_VARIANT:
  case BL_EXPR_CAST:
  case BL_EXPR_SIZEOF:
  case BL_DECL_CONST:
    return true;
  case BL_EXPR_DECL_REF:
    return bl_ast_node_is_const(bl_peek_expr_decl_ref(node)->ref);
  case BL_EXPR_CALL:
    return bl_peek_expr_call(node)->run_in_compile_time;
  default:
    return false;
  }
}

bool
bl_ast_can_implcast(bl_node_t *from_type, bl_node_t *to_type)
{
  bl_type_kind_e from_kind = bl_ast_type_get_kind(from_type);
  bl_type_kind_e to_kind   = bl_ast_type_get_kind(to_type);

  if (from_kind <= BL_SIZE_KIND && to_kind <= BL_SIZE_KIND)
    return true;

  if (from_kind != to_kind)
    return false;

  /* ok ... same kind but... */
  if (from_kind == BL_PTR_KIND) {
    /* implicitly cast any pointer to *void */
    if (bl_ast_type_is_fund(from_type, BL_FTYPE_VOID) ||
        bl_ast_type_is_fund(to_type, BL_FTYPE_VOID))
      return true;
    else
      return false;
  }

  return true;
}

uint64_t
bl_ast_buildin_hash(bl_buildin_e t)
{
  if (!buildin_hashes_initialized) {
    for (int i = 0; i < BL_BUILDIN_COUNT; ++i) {
      buildin_hashes[i] = bo_hash_from_str(bl_buildin_strings[i]);
    }
    buildin_hashes_initialized = true;
  }

  return buildin_hashes[t];
}

bool
bl_ast_is_buildin(bl_id_t *id, bl_buildin_e t)
{
  return id->hash == bl_ast_buildin_hash(t);
}

/**************************************************************************************************/
