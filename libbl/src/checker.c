//************************************************************************************************
// bl
//
// File:   checker.c
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

/*
 * Flatten checking structure:
 * Symbols in this language can be defined in any order in global scope, so we need some kind of
 * 'lazy' reference connecting. For example we can call function 'foo' before it is declared, when
 * checker reaches such call it need to be interrupted and resumed later when definition of the
 * function 'foo' apears in current of parent scope. To solve such problem whole AST is divided into
 * smaller flatten queues which are later solved backwards. When compiler gets to unknown symbol we
 * take note about position in queue and push it into waiting cache.
 */

#include "stages_impl.h"
#include "common_impl.h"
#include "ast_impl.h"

#define VERBOSE 0
#define VERBOSE_MULTIPLE_CHECK 0

#define FINISH return true
#define WAIT return false

#define check_error(cnt, code, tok, pos, format, ...)                                              \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), &(tok)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
  }

#define check_error_node(cnt, code, node, pos, format, ...)                                        \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), (node)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
  }

#define check_warning(cnt, tok, pos, format, ...)                                                  \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, &(tok)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

#define check_warning_node(cnt, node, pos, format, ...)                                            \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, (node)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
  bl_ast_t *     ast;

  BHashTable *waiting;
  bl_scope_t *provided_in_gscope;

  BArray *flatten_cache;
} context_t;

typedef struct
{
  BArray *flatten;
  size_t  i;
} fiter_t;

static inline bl_node_t *
lookup(bl_node_t *ident, bl_scope_t **out_scope, bool walk_tree);

static bl_node_t *
_lookup(bl_node_t *compound, bl_node_t *ident, bl_scope_t **out_scope, bool walk_tree);

static bl_node_t *
wait_context(bl_node_t *node);

static bool
infer_type(context_t *cnt, bl_node_t *decl);

static void
provide(bl_node_t *ident, bl_node_t *provided);

static inline void
waiting_push(BHashTable *waiting, bl_node_t *ident, fiter_t fiter);

static void
waiting_resume(context_t *cnt, bl_node_t *ident);

static BArray *
flatten_get(BArray *cache);

static void
flatten_put(BArray *cache, BArray *flatten);

static void
flatten_free_cache(BArray *cache);

static inline void
flatten_push(BArray *flatten, bl_node_t **node);

static void
flatten_node(context_t *cnt, BArray *fbuf, bl_node_t **node);

static bool
implicit_cast(context_t *cnt, bl_node_t **node, bl_node_t *to_type);

static inline void
check_error_invalid_types(context_t *cnt, bl_node_t *first_type, bl_node_t *second_type,
                          bl_node_t *err_pos);

static void
check_flatten(context_t *cnt, bl_node_t **node);

static void
process_flatten(context_t *cnt, fiter_t *fit);

static bool
check_node(context_t *cnt, bl_node_t **node);

static bool
check_ident(context_t *cnt, bl_node_t **ident);

static bool
check_decl(context_t *cnt, bl_node_t **decl);

static bool
check_expr_call(context_t *cnt, bl_node_t **call);

static bool
check_expr_unary(context_t *cnt, bl_node_t **unary);

static bool
check_expr_binop(context_t *cnt, bl_node_t **binop);

static bool
check_expr_cast(context_t *cnt, bl_node_t **cast);

static bool
check_expr_sizeof(context_t *cnt, bl_node_t **szof);

static bool
check_expr_member(context_t *cnt, bl_node_t **member);

static bool
check_expr_elem(context_t *cnt, bl_node_t **elem);

static bool
check_stmt_if(context_t *cnt, bl_node_t **stmt_if);

static bool
check_stmt_return(context_t *cnt, bl_node_t **ret);

static bool
check_type_enum(context_t *cnt, bl_node_t **type);

static void
check_unresolved(context_t *cnt);

// impl
bl_node_t *
lookup(bl_node_t *ident, bl_scope_t **out_scope, bool walk_tree)
{
  assert(ident && "invalid identificator in lookup");
  bl_node_t *compound = bl_peek_ident(ident)->parent_compound;
  assert(compound && "ident compound not set");
  return _lookup(compound, ident, out_scope, walk_tree);
}

bl_node_t *
_lookup(bl_node_t *compound, bl_node_t *ident, bl_scope_t **out_scope, bool walk_tree)
{
  bl_node_t * found = NULL;
  bl_scope_t *scope = NULL;

  while (compound && !found) {
    scope = bl_ast_get_scope(compound);
    assert(scope);

    found = bl_scope_get(scope, ident);
    if (!walk_tree) break;
    compound = bl_ast_get_parent_compound(compound);
  }

  if (out_scope) *out_scope = scope;
  return found;
}

bl_node_t *
wait_context(bl_node_t *node)
{
  assert(node && "invalid wait node");
  switch (bl_node_code(node)) {
  case BL_NODE_IDENT: return node;
  case BL_NODE_EXPR_MEMBER: return bl_peek_expr_member(node)->ident;
  case BL_NODE_DECL: return bl_peek_decl(node)->name;
  case BL_NODE_STMT_RETURN: {
    bl_node_t *decl = bl_peek_stmt_return(node)->fn_decl;
    assert(decl && "return statement without context");
    return bl_peek_decl(decl)->name;
  }
  default: return NULL;
  };
}

void
provide(bl_node_t *ident, bl_node_t *provided)
{
  assert(ident && provided && "trying to provide invalid symbol");
  bl_node_t *compound = bl_peek_ident(ident)->parent_compound;
  assert(compound);
  bl_scope_t *scope = bl_ast_get_scope(compound);
  assert(scope);

#if VERBOSE
  bl_log("providing " BL_MAGENTA("'%s'") " (%d)", bl_peek_ident(ident)->str, ident->_serial);
#endif
  bl_scope_insert(scope, ident, provided);
}

void
waiting_push(BHashTable *waiting, bl_node_t *node, fiter_t fiter)
{
  bl_node_t *ident = wait_context(node);
  assert(ident);
  bl_node_ident_t *_ident = bl_peek_ident(ident);
  BArray *         queue;
  if (bo_htbl_has_key(waiting, _ident->hash)) {
    queue = bo_htbl_at(waiting, _ident->hash, BArray *);
  } else {
    queue = bo_array_new(sizeof(fiter_t));
    bo_htbl_insert(waiting, _ident->hash, queue);
  }
  assert(queue);
  bo_array_push_back(queue, fiter);
}

void
waiting_resume(context_t *cnt, bl_node_t *ident)
{
  bl_node_ident_t *_ident = bl_peek_ident(ident);
  /* is there some flattens waiting for this symbol??? */
  if (!bo_htbl_has_key(cnt->waiting, _ident->hash)) return;

  /* resume all waiting flattens */
  BArray *q = bo_htbl_at(cnt->waiting, _ident->hash, BArray *);
  assert(q && "invalid flattens queue");

  /* NOTE: we need to iterate backwards from last element in 'q' because it can be modified in
   * 'process_flatten' method */
  fiter_t   fit;
  const int c = (int)bo_array_size(q);
  for (int i = c - 1; i >= 0; --i) {
    fit = bo_array_at(q, i, fiter_t);
    bo_array_erase(q, i);
    process_flatten(cnt, &fit);
  }

  if (bo_array_empty(q)) bo_htbl_erase_key(cnt->waiting, _ident->hash);
}

void
check_unresolved(context_t *cnt)
{
  bo_iterator_t iter;
  BArray *      q;
  fiter_t       tmp;
  bl_node_t **  tmp_node;
  bl_node_t *   tmp_ident;

  bl_bhtbl_foreach(cnt->waiting, iter)
  {
    q = bo_htbl_iter_peek_value(cnt->waiting, &iter, BArray *);
    assert(q);

    // bl_log("size %d", bo_array_size(q));
    for (size_t i = 0; i < bo_array_size(q); ++i) {
      tmp = bo_array_at(q, i, fiter_t);
      // bl_log("# %p index: %d", tmp.flatten, i);
      tmp_node = bo_array_at(tmp.flatten, tmp.i, bl_node_t **);
      assert(*tmp_node);
      tmp_ident = wait_context(*tmp_node);
      if (!bl_scope_has_symbol(cnt->provided_in_gscope, tmp_ident))
        check_error_node(cnt, BL_ERR_UNKNOWN_SYMBOL, tmp_ident, BL_BUILDER_CUR_WORD,
                         "unknown symbol");
      flatten_put(cnt->flatten_cache, tmp.flatten);
    }
  }
}

#if BL_DEBUG
static int _flatten = 0;
#endif
BArray *
flatten_get(BArray *cache)
{
  BArray *tmp = NULL;
  if (bo_array_size(cache) == 0) {
    tmp = bo_array_new(sizeof(bl_node_t *));
  } else {
    tmp = bo_array_at(cache, 0, BArray *);
    bo_array_erase(cache, 0);
  }
  assert(tmp);
#if BL_DEBUG
  ++_flatten;
#endif
  return tmp;
}

void
flatten_put(BArray *cache, BArray *flatten)
{
#if BL_DEBUG
  --_flatten;
#endif
  bo_array_clear(flatten);
  bo_array_push_back(cache, flatten);
}

void
flatten_free_cache(BArray *cache)
{
  BArray *it;
  bl_barray_foreach(cache, it)
  {
    bo_unref(it);
  }
}

void
flatten_push(BArray *flatten, bl_node_t **node)
{
  bo_array_push_back(flatten, node);
}

void
flatten_node(context_t *cnt, BArray *fbuf, bl_node_t **node)
{
  if (!*node) return;

#define flatten(_node) flatten_node(cnt, fbuf, (_node))

  switch (bl_node_code(*node)) {
  case BL_NODE_DECL: {
    bl_node_decl_t *_decl = bl_peek_decl(*node);
    /* store declaration for temporary use here, this scope is used only for searching truly
     * undefined symbols later */
    if (_decl->in_gscope && !bl_scope_has_symbol(cnt->provided_in_gscope, _decl->name))
      bl_scope_insert(cnt->provided_in_gscope, _decl->name, *node);

    flatten(&_decl->type);
    flatten(&_decl->value);
    break;
  }

  case BL_NODE_LIT_FN: {
    bl_node_lit_fn_t *_fn = bl_peek_lit_fn(*node);
    flatten(&_fn->type);
    check_flatten(cnt, &_fn->block);
    return;
  }

  case BL_NODE_LIT_STRUCT: {
    bl_node_lit_struct_t *_struct = bl_peek_lit_struct(*node);
    flatten(&_struct->type);
    return;
  }

  case BL_NODE_LIT_ENUM: {
    bl_node_lit_enum_t *_enum = bl_peek_lit_enum(*node);
    flatten(&_enum->type);
    bl_node_t **variant;
    bl_node_foreach_ref(_enum->variants, variant)
    {
      flatten(variant);
    }
    return;
  }

  case BL_NODE_BLOCK: {
    bl_node_block_t *_block = bl_peek_block(*node);

    bl_node_t **tmp;
    bl_node_foreach_ref(_block->nodes, tmp)
    {
      flatten(tmp);
    }
    return;
  }

  case BL_NODE_UBLOCK: {
    bl_node_ublock_t *_ublock = bl_peek_ublock(*node);

    bl_node_t **tmp;
    bl_node_foreach_ref(_ublock->nodes, tmp)
    {
      check_flatten(cnt, tmp);
    }
    return;
  }

  case BL_NODE_STMT_RETURN: {
    bl_node_stmt_return_t *_return = bl_peek_stmt_return(*node);
    flatten(&_return->expr);
    break;
  }

  case BL_NODE_STMT_IF: {
    bl_node_stmt_if_t *_if = bl_peek_stmt_if(*node);
    flatten(&_if->test);
    flatten(&_if->true_stmt);
    flatten(&_if->false_stmt);
    break;
  }

  case BL_NODE_STMT_LOOP: {
    bl_node_stmt_loop_t *_loop = bl_peek_stmt_loop(*node);
    flatten(&_loop->test);
    flatten(&_loop->true_stmt);
    break;
  }

  case BL_NODE_EXPR_MEMBER: {
    bl_node_expr_member_t *_member = bl_peek_expr_member(*node);
    flatten(&_member->next);
    break;
  }

  case BL_NODE_EXPR_ELEM: {
    bl_node_expr_elem_t *_elem = bl_peek_expr_elem(*node);
    flatten(&_elem->next);
    flatten(&_elem->index);
    break;
  }

  case BL_NODE_EXPR_CAST: {
    bl_node_expr_cast_t *_cast = bl_peek_expr_cast(*node);
    flatten(&_cast->type);
    flatten(&_cast->next);
    break;
  }

  case BL_NODE_EXPR_SIZEOF: {
    bl_node_expr_sizeof_t *_sizeof = bl_peek_expr_sizeof(*node);
    flatten(&_sizeof->in);
    flatten(&_sizeof->type);
    break;
  }

  case BL_NODE_EXPR_CALL: {
    bl_node_expr_call_t *_call = bl_peek_expr_call(*node);
    flatten(&_call->ref);

    bl_node_t **tmp;
    bl_node_foreach_ref(_call->args, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case BL_NODE_EXPR_BINOP: {
    bl_node_expr_binop_t *_binop = bl_peek_expr_binop(*node);
    flatten(&_binop->lhs);
    flatten(&_binop->rhs);
    break;
  }

  case BL_NODE_EXPR_UNARY: {
    bl_node_expr_unary_t *_unary = bl_peek_expr_unary(*node);
    flatten(&_unary->next);
    break;
  }

  case BL_NODE_TYPE_STRUCT: {
    bl_node_type_struct_t *_struct_type = bl_peek_type_struct(*node);

    bl_node_t **tmp;
    bl_node_foreach_ref(_struct_type->types, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case BL_NODE_TYPE_ENUM: {
    bl_node_type_enum_t *_enum_type = bl_peek_type_enum(*node);
    flatten(&_enum_type->base_type);
    break;
  }

  case BL_NODE_TYPE_FN: {
    bl_node_type_fn_t *_type_fn = bl_peek_type_fn(*node);
    flatten(&_type_fn->ret_type);
    bl_node_t **sub_type;
    bl_node_foreach_ref(_type_fn->arg_types, sub_type)
    {
      flatten(sub_type);
    }
    break;
  }

  case BL_NODE_IDENT:
  case BL_NODE_EXPR_NULL:
  case BL_NODE_STMT_BREAK:
  case BL_NODE_STMT_CONTINUE:
  case BL_NODE_TYPE_FUND:
  case BL_NODE_LIT:
  case BL_NODE_LOAD:
  case BL_NODE_LINK: break;
  default: bl_warning("missing flattening for node %s", bl_node_name(*node));
  }

  flatten_push(fbuf, node);
#undef flatten
}

void
check_flatten(context_t *cnt, bl_node_t **node)
{
  fiter_t fit;
  fit.flatten = flatten_get(cnt->flatten_cache);
  fit.i       = 0;

  flatten_node(cnt, fit.flatten, node);
  process_flatten(cnt, &fit);
}

void
process_flatten(context_t *cnt, fiter_t *fit)
{
  assert(fit);
  assert(fit->flatten && "invalid flatten");
  bool interrupted = false;

  bl_node_t **tmp;
  for (; fit->i < bo_array_size(fit->flatten); ++fit->i) {
    tmp = bo_array_at(fit->flatten, fit->i, bl_node_t **);
    if (!check_node(cnt, tmp)) {
      waiting_push(cnt->waiting, *tmp, *fit);
      interrupted = true;
      break;
    }
  }

  if (!interrupted) {
    flatten_put(cnt->flatten_cache, fit->flatten);
    fit->flatten = NULL;
  }
}

bool
implicit_cast(context_t *cnt, bl_node_t **node, bl_node_t *to_type)
{
  to_type              = bl_ast_get_type(to_type);
  bl_node_t *from_type = bl_ast_get_type(*node);

  bl_type_kind_e from_kind = bl_ast_get_type_kind(from_type);
  bl_type_kind_e to_kind   = bl_ast_get_type_kind(to_type);
  if (bl_node_is(*node, BL_NODE_LIT) && from_kind != BL_KIND_STRING && from_kind != BL_KIND_CHAR &&
      from_kind != BL_KIND_REAL &&
      (to_kind == BL_KIND_SIZE || to_kind == BL_KIND_UINT || to_kind == BL_KIND_SINT)) {
    bl_peek_lit(*node)->type = bl_ast_node_dup(cnt->ast, to_type);
    return true;
  }

  if (!bl_ast_can_impl_cast(from_type, to_type)) return false;

  bl_node_t *tmp_next = (*node)->next;
  bl_node_t *type_dup = bl_ast_node_dup(cnt->ast, to_type);
  bl_node_t *cast     = bl_ast_expr_cast(cnt->ast, NULL, type_dup, *node);
  cast->next          = tmp_next;
  *node               = cast;

  return true;
}

void
check_error_invalid_types(context_t *cnt, bl_node_t *first_type, bl_node_t *second_type,
                          bl_node_t *err_pos)
{
  char tmp_first[256];
  char tmp_second[256];
  bl_ast_type_to_string(tmp_first, 256, first_type);
  bl_ast_type_to_string(tmp_second, 256, second_type);
  check_error_node(cnt, BL_ERR_INVALID_TYPE, err_pos, BL_BUILDER_CUR_WORD,
                   "no implicit cast for types '%s' and '%s'", tmp_first, tmp_second);
}

bool
check_expr_call(context_t *cnt, bl_node_t **call)
{
  bl_node_expr_call_t *_call = bl_peek_expr_call(*call);

  bl_node_t *ident =
      bl_node_is(_call->ref, BL_NODE_IDENT) ? _call->ref : bl_peek_expr_member(_call->ref)->ident;

  bl_node_t *callee = bl_peek_ident(ident)->ref;
  assert(callee);
  bl_node_decl_t *_callee     = bl_peek_decl(callee);
  bl_node_t *     callee_type = _callee->type;

  if (bl_node_is_not(callee_type, BL_NODE_TYPE_FN)) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, *call, BL_BUILDER_CUR_WORD,
                     "expected function name");
    FINISH;
  }

  bl_node_type_fn_t *_callee_type = bl_peek_type_fn(callee_type);

  _call->type = _callee_type->ret_type;

  if (_call->argsc != _callee_type->argc_types) {
    check_error_node(cnt, BL_ERR_INVALID_ARG_COUNT, *call, BL_BUILDER_CUR_WORD,
                     "expected %d %s, but called with %d", _callee_type->argc_types,
                     _callee_type->argc_types == 1 ? "argument" : "arguments", _call->argsc);
    FINISH;
  }

  bl_node_t **call_arg   = &_call->args;
  bl_node_t * callee_arg = _callee_type->arg_types;

  while (*call_arg) {
    if (bl_node_is(*call_arg, BL_NODE_EXPR_NULL)) {
      bl_peek_expr_null(*call_arg)->type = bl_ast_get_type(callee_arg);
    } else if (!bl_ast_type_cmp(*call_arg, callee_arg) &&
               !implicit_cast(cnt, call_arg, callee_arg)) {
      char tmp1[256];
      char tmp2[256];
      bl_ast_type_to_string(tmp1, 256, bl_ast_get_type(*call_arg));
      bl_ast_type_to_string(tmp2, 256, bl_ast_get_type(callee_arg));

      check_error_node(cnt, BL_ERR_INVALID_ARG_TYPE, *call_arg, BL_BUILDER_CUR_WORD,
                       "invalid call argument type, expected is '%s' but called with '%s'", tmp2,
                       tmp1);

      break;
    }

    call_arg   = &(*call_arg)->next;
    callee_arg = callee_arg->next;
  }

  FINISH;
}

bool
check_expr_unary(context_t *cnt, bl_node_t **unary)
{
  bl_node_expr_unary_t *_unary = bl_peek_expr_unary(*unary);
  assert(_unary->next);

  if (_unary->op == BL_SYM_AND) {
    _unary->type = bl_ast_node_dup(cnt->ast, bl_ast_get_type(_unary->next));
    int ptr      = bl_ast_type_get_ptr(_unary->type) + 1;
    bl_ast_type_set_ptr(_unary->type, ptr);
  } else if (_unary->op == BL_SYM_ASTERISK) {
    _unary->type = bl_ast_node_dup(cnt->ast, bl_ast_get_type(_unary->next));
    int ptr      = bl_ast_type_get_ptr(_unary->type) - 1;

    if (ptr < 0) {
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _unary->next, BL_BUILDER_CUR_WORD,
                       "cannot dereference non-pointer type");
    } else {
      bl_ast_type_set_ptr(_unary->type, ptr);
    }
  } else {
    _unary->type = bl_ast_get_type(_unary->next);
  }

  FINISH;
}

bool
check_node(context_t *cnt, bl_node_t **node)
{
  assert(node);
  bool result = true;
#if defined(BL_DEBUG) && BL_VERBOSE_MUTIPLE_CHECK
  if (node->state == BL_CHECKED)
    bl_msg_warning("unnecessary node check %s (%d)", bl_node_name(node), node->_serial);
#endif

  switch (bl_node_code(*node)) {
  case BL_NODE_IDENT: result = check_ident(cnt, node); break;

  case BL_NODE_DECL: result = check_decl(cnt, node); break;

  case BL_NODE_EXPR_CALL: result = check_expr_call(cnt, node); break;

  case BL_NODE_EXPR_BINOP: result = check_expr_binop(cnt, node); break;

  case BL_NODE_EXPR_CAST: result = check_expr_cast(cnt, node); break;

  case BL_NODE_EXPR_UNARY: result = check_expr_unary(cnt, node); break;

  case BL_NODE_EXPR_SIZEOF: result = check_expr_sizeof(cnt, node); break;

  case BL_NODE_EXPR_MEMBER: result = check_expr_member(cnt, node); break;

  case BL_NODE_EXPR_ELEM: result = check_expr_elem(cnt, node); break;

  case BL_NODE_STMT_IF: result = check_stmt_if(cnt, node); break;

  case BL_NODE_STMT_RETURN: result = check_stmt_return(cnt, node); break;

  case BL_NODE_TYPE_ENUM: result = check_type_enum(cnt, node); break;

  case BL_NODE_EXPR_NULL:
  case BL_NODE_LIT:
  case BL_NODE_STMT_BREAK:
  case BL_NODE_STMT_CONTINUE:
  case BL_NODE_STMT_LOOP:
  case BL_NODE_TYPE_FN:
  case BL_NODE_TYPE_STRUCT:
  case BL_NODE_TYPE_FUND:
  case BL_NODE_LOAD:
  case BL_NODE_LINK: break;

  default: bl_warning("missing check for node type %s", bl_node_name(*node));
  }

#if VERBOSE && defined(BL_DEBUG)
  {
    static int  prev_checked = -1;
    const char *file         = node->src ? node->src->unit->name : "UNKNOWN";
    const int   line         = node->src ? node->src->line : -1;
    bl_node_t * checked      = wait_context(node);
    const char *name         = checked ? bl_peek_ident(checked)->str : "?";
    bl_log("checked [%s] " BL_MAGENTA("'%s'") " (%s, %d) file: " BL_YELLOW("%s") " line: " BL_CYAN(
               "%d"),
           result ? BL_GREEN(" OK ") : BL_RED("WAIT"), name, bl_node_name(node), node->_serial,
           file, line);
    assert(prev_checked != node->_serial && "Looping checker!!!");
    prev_checked = node->_serial;
  }
#endif

  (*node)->state = result ? BL_CHECKED : BL_WAITING;
  return result;
}

bool
check_ident(context_t *cnt, bl_node_t **ident)
{
  bl_node_ident_t *_ident = bl_peek_ident(*ident);
  if (_ident->ref) {
    FINISH;
  }

  bl_node_t *found;
  const int  buildin = bl_ast_is_buildin_type(*ident);
  if (buildin != -1) {
    /* connect buildin fundamental types references */
    found = &bl_ftypes[buildin];
  } else {
    found = lookup(*ident, NULL, true);
    if (!found) WAIT;
  }

  if (bl_node_is(found, BL_NODE_DECL)) {
    bl_peek_decl(found)->used++;
  }

  _ident->ref = bl_ast_unroll_ident(found);
  // assert(bl_node_is(_ident->ref, BL_NODE_DECL));

  if (_ident->ptr || _ident->arr) {
    /* when ident reference is pointer we need to create copy of declaration with different
     * type, maybe there is some better solution ??? */
    _ident->ref     = bl_ast_node_dup(cnt->ast, found);
    bl_node_t *type = bl_ast_get_type(_ident->ref);
    assert(type);
    type = bl_ast_node_dup(cnt->ast, type);
    bl_ast_type_set_ptr(type, _ident->ptr);
    bl_ast_type_set_arr(type, _ident->arr);
    if (bl_ast_is_type(_ident->ref))
      _ident->ref = type;
    else
      bl_ast_set_type(_ident->ref, type);
  }

  FINISH;
}

bool
check_stmt_return(context_t *cnt, bl_node_t **ret)
{
  bl_node_stmt_return_t *_ret = bl_peek_stmt_return(*ret);
  assert(_ret->fn_decl);

  bl_node_decl_t *  _callee_decl = bl_peek_decl(_ret->fn_decl);
  bl_node_lit_fn_t *_callee      = bl_peek_lit_fn(_callee_decl->value);
  bl_node_t *       fn_ret_type  = bl_ast_get_type(bl_peek_type_fn(_callee->type)->ret_type);
  if (fn_ret_type == NULL) WAIT;

  if (_ret->expr) {
    if (bl_node_is(_ret->expr, BL_NODE_EXPR_NULL)) {
      bl_node_expr_null_t *_null = bl_peek_expr_null(_ret->expr);
      _null->type                = fn_ret_type;
      if (bl_ast_get_type_kind(_null->type) != BL_KIND_PTR) {
        check_error_node(
            cnt, BL_ERR_INVALID_TYPE, _ret->expr, BL_BUILDER_CUR_WORD,
            "'null' cannot be used because the function does not return a pointer value");
      }
    } else {
      bl_node_t *expr_type = bl_ast_get_type(_ret->expr);
      if (!bl_ast_type_cmp(expr_type, fn_ret_type) &&
          !implicit_cast(cnt, &_ret->expr, fn_ret_type)) {
        check_error_invalid_types(cnt, expr_type, fn_ret_type, _ret->expr);
      }
    }
  } else {
    if (!bl_ast_type_cmp(&bl_ftypes[BL_FTYPE_VOID], fn_ret_type)) {
      check_error_node(cnt, BL_ERR_EXPECTED_EXPR, *ret, BL_BUILDER_CUR_AFTER,
                       "expected return value");
    }
  }
  FINISH;
}

bool
check_expr_binop(context_t *cnt, bl_node_t **binop)
{
  bl_node_expr_binop_t *_binop = bl_peek_expr_binop(*binop);

  assert(_binop->lhs);
  assert(_binop->rhs);

  if (_binop->op == BL_SYM_ASSIGN) {
    if (bl_node_is_not(_binop->lhs, BL_NODE_IDENT) &&
        bl_node_is_not(_binop->lhs, BL_NODE_EXPR_UNARY) &&
        bl_node_is_not(_binop->lhs, BL_NODE_EXPR_ELEM) &&
        bl_node_is_not(_binop->lhs, BL_NODE_EXPR_MEMBER)) {
      // TODO: temporary solution, what about (some_pointer + 1) = ...
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _binop->lhs, BL_BUILDER_CUR_WORD,
                       "left-hand side of assignment does not refer to any declaration and "
                       "cannot be assigned");
      FINISH;
    }

    if (bl_node_is_not(_binop->lhs, BL_NODE_EXPR_UNARY) &&
        bl_node_is_not(_binop->lhs, BL_NODE_EXPR_ELEM) &&
        bl_node_is_not(_binop->lhs, BL_NODE_EXPR_MEMBER)) {
      bl_node_ident_t *_lhs = bl_peek_ident(_binop->lhs);
      assert(_lhs->ref);
      assert(bl_node_is(_lhs->ref, BL_NODE_DECL));

      if (!bl_peek_decl(_lhs->ref)->mutable) {
        check_error_node(cnt, BL_ERR_INVALID_MUTABILITY, _binop->lhs, BL_BUILDER_CUR_WORD,
                         "declaration is not mutable and cannot be assigned");
      }
    }
  }

  bl_node_t *lhs_type = bl_ast_get_type(_binop->lhs);
  assert(lhs_type);
  bl_type_kind_e lhs_kind = bl_ast_get_type_kind(lhs_type);

  if (bl_node_is(_binop->rhs, BL_NODE_EXPR_NULL)) {
    if (lhs_kind != BL_KIND_PTR) {
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _binop->lhs, BL_BUILDER_CUR_WORD,
                       "expected a pointer type");
    } else {
      bl_peek_expr_null(_binop->rhs)->type = lhs_type;
      if (!_binop->type) _binop->type = lhs_type;
    }

    FINISH;
  }

  bl_node_t *rhs_type = bl_ast_get_type(_binop->rhs);
  assert(rhs_type);
  bl_type_kind_e rhs_kind = bl_ast_get_type_kind(rhs_type);

  if (!bl_ast_type_cmp(lhs_type, rhs_type)) {
    if (bl_node_is(_binop->lhs, BL_NODE_LIT) &&
        (lhs_kind == BL_KIND_SIZE || lhs_kind == BL_KIND_UINT || lhs_kind == BL_KIND_SINT) &&
        (rhs_kind == BL_KIND_SIZE || rhs_kind == BL_KIND_UINT || rhs_kind == BL_KIND_SINT)) {
      bl_peek_lit(_binop->lhs)->type = bl_ast_node_dup(cnt->ast, rhs_type);
    } else if (!implicit_cast(cnt, &_binop->rhs, lhs_type)) {
      check_error_invalid_types(cnt, lhs_type, rhs_type, *binop);
    }
  }

  if (!_binop->type) _binop->type = bl_ast_get_type(_binop->lhs);

  FINISH;
}

bool
check_expr_cast(context_t *cnt, bl_node_t **cast)
{
  bl_node_expr_cast_t *_cast = bl_peek_expr_cast(*cast);
  assert(_cast->type);
  _cast->type = bl_ast_get_type(_cast->type);
  FINISH;
}

bool
check_expr_sizeof(context_t *cnt, bl_node_t **szof)
{
  bl_node_expr_sizeof_t *_sizeof = bl_peek_expr_sizeof(*szof);
  _sizeof->in                    = bl_ast_get_type(_sizeof->in);
  FINISH;
}

bool
check_type_enum(context_t *cnt, bl_node_t **type)
{
  bl_node_type_enum_t *_type = bl_peek_type_enum(*type);
  assert(_type->base_type);
  bl_node_t *tmp = bl_ast_get_type(_type->base_type);

  if (bl_node_is_not(tmp, BL_NODE_TYPE_FUND)) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, _type->base_type, BL_BUILDER_CUR_WORD,
                     "enum base type must be an integer type");
    FINISH;
  }

  switch (bl_peek_type_fund(tmp)->code) {
  case BL_FTYPE_S8:
  case BL_FTYPE_S16:
  case BL_FTYPE_S32:
  case BL_FTYPE_S64:
  case BL_FTYPE_U8:
  case BL_FTYPE_U16:
  case BL_FTYPE_U32:
  case BL_FTYPE_U64:
  case BL_FTYPE_SIZE:
  case BL_FTYPE_CHAR: break;
  default: {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, _type->base_type, BL_BUILDER_CUR_WORD,
                     "enum base type must be an integer type");
  }
  }

  if (bl_ast_type_get_ptr(tmp)) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, _type->base_type, BL_BUILDER_CUR_WORD,
                     "enum base type cannot be a pointer");
  }

  _type->base_type = tmp;

  FINISH;
}

bool
check_expr_member(context_t *cnt, bl_node_t **member)
{
  bl_node_expr_member_t *_member = bl_peek_expr_member(*member);
  bl_node_t *            found   = NULL;
  assert(_member->next);
  assert(_member->ident);

  bl_node_t *lhs_type = bl_ast_get_type(_member->next);
  if (!lhs_type) FINISH;
  if (bl_ast_type_get_arr(lhs_type)) {
    /* is member array 'count'??? */
    if (bl_ast_is_buildin(_member->ident) == BL_BUILDIN_ARR_COUNT) {
      bl_node_t *tmp_next = (*member)->next;
      *member             = bl_ast_node_dup(cnt->ast, bl_ast_type_get_arr(lhs_type));
      (*member)->next     = tmp_next;

      // TODO: set next node???

      FINISH;
    }
  } else if (bl_node_is(lhs_type, BL_NODE_TYPE_STRUCT)) {
    /* structure member */
    _member->kind = BL_MEM_KIND_STRUCT;

    bl_node_type_struct_t *_lhs_type = bl_peek_type_struct(lhs_type);
    /* lhs_type cannot be anonymous structure type (generate error later instead of assert?) */
    assert(_lhs_type->base_decl);

    found = _lookup(bl_peek_decl(_lhs_type->base_decl)->value, _member->ident, NULL, false);
    if (!found) WAIT;

    if (_member->ptr_ref != (_lhs_type->ptr ? true : false)) {
      check_error_node(cnt, BL_ERR_INVALID_MEMBER_ACCESS, *member, BL_BUILDER_CUR_WORD,
                       "invalid member access, use %s",
                       _member->ptr_ref ? "'.' instead of '->'" : "'->' instead of '.'");
    }
  } else if (bl_node_is(lhs_type, BL_NODE_TYPE_ENUM)) {
    /* enum variant */
    _member->kind                  = BL_MEM_KIND_ENUM;
    bl_node_type_enum_t *_lhs_type = bl_peek_type_enum(lhs_type);
    assert(_lhs_type->base_decl);

    found = _lookup(bl_peek_decl(_lhs_type->base_decl)->value, _member->ident, NULL, false);
    if (!found) WAIT;

    if (_member->ptr_ref) {
      check_error_node(cnt, BL_ERR_INVALID_MEMBER_ACCESS, *member, BL_BUILDER_CUR_WORD,
                       "use '.' for access to enum variants");
    }
  } else {
    check_error_node(cnt, BL_ERR_EXPECTED_TYPE_STRUCT, _member->next, BL_BUILDER_CUR_WORD,
                     "expected structure or enum");
  }

  _member->type                      = bl_ast_get_type(found);
  bl_peek_ident(_member->ident)->ref = found;

  FINISH;
}

bool
check_expr_elem(context_t *cnt, bl_node_t **elem)
{
  bl_node_expr_elem_t *_elem = bl_peek_expr_elem(*elem);
  assert(_elem->index);
  assert(_elem->next);

  _elem->type = bl_ast_get_type(_elem->next);
  if (!bl_ast_type_get_arr(_elem->type)) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, *elem, BL_BUILDER_CUR_WORD, "expected array");
  }

  _elem->type = bl_ast_node_dup(cnt->ast, _elem->type);
  bl_ast_type_set_arr(_elem->type, NULL);

  bl_node_t *index_type = bl_ast_get_type(_elem->index);

  if (!implicit_cast(cnt, &_elem->index, &bl_ftypes[BL_FTYPE_SIZE])) {
    check_error_invalid_types(cnt, index_type, &bl_ftypes[BL_FTYPE_SIZE], _elem->index);
  }

  FINISH;
}

bool
check_stmt_if(context_t *cnt, bl_node_t **stmt_if)
{
  bl_node_stmt_if_t *_if = bl_peek_stmt_if(*stmt_if);
  assert(_if->test);
  assert(_if->true_stmt);

  bl_node_t *test_type = bl_ast_get_type(_if->test);
  if (!bl_ast_type_cmp(test_type, &bl_ftypes[BL_FTYPE_BOOL])) {
    check_error_invalid_types(cnt, test_type, &bl_ftypes[BL_FTYPE_BOOL], _if->test);
  }
  FINISH;
}

bool
infer_type(context_t *cnt, bl_node_t *decl)
{
  bl_node_decl_t *_decl = bl_peek_decl(decl);
  if (!_decl->value) return false;
  bl_node_t *inferred_type = bl_ast_get_type(_decl->value);
  if (!inferred_type) return false;

  if (_decl->type && !bl_ast_type_cmp(inferred_type, _decl->type) &&
      !implicit_cast(cnt, &_decl->value, _decl->type)) {
    check_error_invalid_types(cnt, _decl->type, inferred_type, _decl->value);
    return false;
  }

  /* infer type from value */
  _decl->type = bl_ast_get_type(_decl->value);
  return true;
}

bool
check_decl(context_t *cnt, bl_node_t **decl)
{
  bl_node_decl_t *_decl          = bl_peek_decl(*decl);
  bool            lookup_in_tree = true;

  assert(_decl->name);
  infer_type(cnt, *decl);

  _decl->in_gscope =
      bl_ast_get_scope(bl_peek_ident(_decl->name)->parent_compound) == cnt->assembly->gscope;

  if (_decl->flags & BL_FLAG_MAIN && _decl->kind != BL_DECL_KIND_FN) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, *decl, BL_BUILDER_CUR_WORD,
                     "main is expected to be function");
  }

  switch (_decl->kind) {
  case BL_DECL_KIND_STRUCT: {
    bl_node_t *value_type                      = bl_ast_get_type(_decl->value);
    bl_peek_type_struct(value_type)->base_decl = *decl;

    if (_decl->mutable) {
      check_error_node(cnt, BL_ERR_INVALID_MUTABILITY, *decl, BL_BUILDER_CUR_WORD,
                       "structure declaration cannot be mutable");
    }

    if (bl_peek_type_struct(value_type)->typesc == 0) {
      check_error_node(cnt, BL_ERR_EMPTY, _decl->name, BL_BUILDER_CUR_WORD, "empty structure");
    }

    break;
  }

  case BL_DECL_KIND_MEMBER: {
    /* Structure members cannot be initialized with default value (foo s32 := 10), when implicit
     * structure initialization will be implemented in future, mutablility checking will be
     * required. In this case assignment of any kind will cause error */
    if (_decl->value) {
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->value, BL_BUILDER_CUR_WORD,
                       "struct member cannot have value binding");
    }
    lookup_in_tree = false;
    break;
  }

  case BL_DECL_KIND_FIELD: {
    assert(_decl->mutable);

    if (bl_ast_get_type_kind(bl_ast_get_type(_decl->type)) == BL_KIND_FN) {
      char tmp[256];
      bl_ast_type_to_string(tmp, 256, bl_ast_get_type(_decl->type));
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->name, BL_BUILDER_CUR_WORD,
                       "invalid type of variable '%s'", tmp);
    }

    if (_decl->in_gscope && !_decl->value) {
      check_error_node(cnt, BL_ERR_EXPECTED_EXPR, _decl->type, BL_BUILDER_CUR_AFTER,
                       "global variables needs to be initialized");
    }
    break;
  }

  case BL_DECL_KIND_CONSTANT: {
    assert(!_decl->mutable);

    if (bl_ast_get_type_kind(bl_ast_get_type(_decl->type)) == BL_KIND_FN ||
        bl_ast_get_type_kind(bl_ast_get_type(_decl->type)) == BL_KIND_STRUCT) {
      char tmp[256];
      bl_ast_type_to_string(tmp, 256, bl_ast_get_type(_decl->type));
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->name, BL_BUILDER_CUR_WORD,
                       "invalid type of constant '%s'", tmp);
    }
    break;
  }

  case BL_DECL_KIND_ARG: {
    if (_decl->value) {
      check_error_node(cnt, BL_ERR_INVALID_ARG_TYPE, *decl, BL_BUILDER_CUR_WORD,
                       "function arguments cannot have value binding");
    }
    break;
  }

  case BL_DECL_KIND_VARIANT: {
    if (_decl->mutable) {
      check_error_node(cnt, BL_ERR_INVALID_MUTABILITY, *decl, BL_BUILDER_CUR_WORD,
                       "an enum variant cannot be mutable");
    }
    break;
  }

  case BL_DECL_KIND_ENUM: {
    bl_node_t *value_type                    = bl_ast_get_type(_decl->value);
    bl_peek_type_enum(value_type)->base_decl = *decl;
    break;
  }

  case BL_DECL_KIND_FN: break;
  case BL_DECL_KIND_TYPE: bl_abort("unimplemented");
  case BL_DECL_KIND_UNKNOWN: bl_abort("unknown declaration kind");
  }

  assert(_decl->type);
  bl_type_kind_e type_kind = bl_ast_get_type_kind(bl_ast_get_type(_decl->type));
  if (type_kind == BL_KIND_VOID) {
    char tmp[256];
    bl_ast_type_to_string(tmp, 256, bl_ast_get_type(_decl->type));
    check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->name, BL_BUILDER_CUR_WORD,
                     "declaration has invalid type '%s'", tmp);
  }

  if (type_kind == BL_KIND_FN && _decl->mutable) {
    char tmp[256];
    bl_ast_type_to_string(tmp, 256, bl_ast_get_type(_decl->type));
    check_error_node(
        cnt, BL_ERR_INVALID_TYPE, _decl->name, BL_BUILDER_CUR_WORD,
        "declaration has invalid type '%s', a function mutable must be referenced by pointer", tmp);
  }
  _decl->type = bl_ast_get_type(_decl->type);

  /* infer type for 'null' value */
  if (_decl->value && bl_node_is(_decl->value, BL_NODE_EXPR_NULL)) {
    if (type_kind == BL_KIND_PTR) {
      bl_peek_expr_null(_decl->value)->type = _decl->type;
    } else {
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->value, BL_BUILDER_CUR_WORD,
                       "'null' cannot be used because the declaration is not a pointer");
    }
  }

  /* provide symbol into scope if there is no conflict */
  bl_node_t *conflict = lookup(_decl->name, NULL, lookup_in_tree);
  if (conflict) {
    check_error_node(cnt, BL_ERR_DUPLICATE_SYMBOL, *decl, BL_BUILDER_CUR_WORD,
                     "symbol with same name already declared here: %s:%d",
                     conflict->src->unit->filepath, conflict->src->line);
  } else {
    provide(_decl->name, *decl);

    /* insert into ir queue */
    if (_decl->in_gscope && (_decl->kind != BL_DECL_KIND_CONSTANT)) {
      // bl_log("generate %s", bl_peek_ident(_decl->name)->str);
      bl_assembly_add_into_ir(cnt->assembly, *decl);
    }

    waiting_resume(cnt, _decl->name);
  }

  FINISH;
}

void
bl_checker_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {
      .builder            = builder,
      .assembly           = assembly,
      .unit               = NULL,
      .ast                = NULL,
      .waiting            = bo_htbl_new_bo(bo_typeof(BArray), true, 2048),
      .flatten_cache      = bo_array_new(sizeof(BArray *)),
      .provided_in_gscope = bl_scope_new(assembly->scope_cache, 4092),
  };

  bl_unit_t *unit;
  bl_barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    cnt.ast  = &unit->ast;
    check_flatten(&cnt, &unit->ast.root);
  }

  check_unresolved(&cnt);

  flatten_free_cache(cnt.flatten_cache);

  bo_unref(cnt.waiting);
  bo_unref(cnt.flatten_cache);
#if BL_DEBUG
  if (_flatten != 0) bl_log(BL_RED("leaking flatten cache: %d"), _flatten);
#endif
}
