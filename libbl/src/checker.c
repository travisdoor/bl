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
    builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), &(tok)->src, (pos), (format),            \
                ##__VA_ARGS__);                                                                    \
  }

#define check_error_node(cnt, code, node, pos, format, ...)                                        \
  {                                                                                                \
    builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), (node)->src, (pos), (format),            \
                ##__VA_ARGS__);                                                                    \
  }

#define check_warning(cnt, tok, pos, format, ...)                                                  \
  {                                                                                                \
    builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, &(tok)->src, (pos), (format),               \
                ##__VA_ARGS__);                                                                    \
  }

#define check_warning_node(cnt, node, pos, format, ...)                                            \
  {                                                                                                \
    builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, (node)->src, (pos), (format),               \
                ##__VA_ARGS__);                                                                    \
  }

typedef struct
{
  builder_t * builder;
  assembly_t *assembly;
  unit_t *    unit;
  ast_t *     ast;

  BHashTable *waiting;
  scope_t *   provided_in_gscope;

  BArray *flatten_cache;
} context_t;

typedef struct
{
  BArray *flatten;
  size_t  i;
} fiter_t;

static inline node_t *
lookup(node_t *ident, scope_t **out_scope, bool walk_tree);

static node_t *
_lookup(node_t *compound, node_t *ident, scope_t **out_scope, bool walk_tree);

static node_t *
wait_context(node_t *node);

static bool
infer_type(context_t *cnt, node_t *decl);

static void
provide(node_t *ident, node_t *provided);

static inline void
waiting_push(BHashTable *waiting, node_t *ident, fiter_t fiter);

static void
waiting_resume(context_t *cnt, node_t *ident);

static BArray *
flatten_get(BArray *cache);

static void
flatten_put(BArray *cache, BArray *flatten);

static void
flatten_free_cache(BArray *cache);

static inline void
flatten_push(BArray *flatten, node_t **node);

static void
flatten_node(context_t *cnt, BArray *fbuf, node_t **node);

static bool
implicit_cast(context_t *cnt, node_t **node, node_t *to_type);

static inline void
check_error_invalid_types(context_t *cnt, node_t *first_type, node_t *second_type, node_t *err_pos);

static void
check_flatten(context_t *cnt, node_t **node);

static void
process_flatten(context_t *cnt, fiter_t *fit);

static bool
check_node(context_t *cnt, node_t **node);

static bool
check_ident(context_t *cnt, node_t **ident);

static bool
check_decl(context_t *cnt, node_t **decl);

static bool
check_expr_call(context_t *cnt, node_t **call);

static bool
check_expr_unary(context_t *cnt, node_t **unary);

static bool
check_expr_binop(context_t *cnt, node_t **binop);

static bool
check_expr_cast(context_t *cnt, node_t **cast);

static bool
check_expr_sizeof(context_t *cnt, node_t **szof);

static bool
check_expr_member(context_t *cnt, node_t **member);

static bool
check_expr_elem(context_t *cnt, node_t **elem);

static bool
check_stmt_if(context_t *cnt, node_t **stmt_if);

static bool
check_stmt_return(context_t *cnt, node_t **ret);

static bool
check_type_enum(context_t *cnt, node_t **type);

static void
check_unresolved(context_t *cnt);

// impl
node_t *
lookup(node_t *ident, scope_t **out_scope, bool walk_tree)
{
  assert(ident && "invalid identificator in lookup");
  node_t *compound = peek_ident(ident)->parent_compound;
  assert(compound && "ident compound not set");
  return _lookup(compound, ident, out_scope, walk_tree);
}

node_t *
_lookup(node_t *compound, node_t *ident, scope_t **out_scope, bool walk_tree)
{
  node_t * found = NULL;
  scope_t *scope = NULL;

  while (compound && !found) {
    scope = ast_get_scope(compound);
    assert(scope);

    found = scope_get(scope, ident);
    if (!walk_tree) break;
    compound = ast_get_parent_compound(compound);
  }

  if (out_scope) *out_scope = scope;
  return found;
}

node_t *
wait_context(node_t *node)
{
  assert(node && "invalid wait node");
  switch (node_code(node)) {
  case NODE_IDENT: return node;
  case NODE_EXPR_MEMBER: return peek_expr_member(node)->ident;
  case NODE_DECL: return peek_decl(node)->name;
  case NODE_STMT_RETURN: {
    node_t *decl = peek_stmt_return(node)->fn_decl;
    assert(decl && "return statement without context");
    return peek_decl(decl)->name;
  }
  default: return NULL;
  };
}

void
provide(node_t *ident, node_t *provided)
{
  assert(ident && provided && "trying to provide invalid symbol");
  node_t *compound = peek_ident(ident)->parent_compound;
  assert(compound);
  scope_t *scope = ast_get_scope(compound);
  assert(scope);

#if VERBOSE
  bl_log("providing " BL_MAGENTA("'%s'") " (%d)", peek_ident(ident)->str, ident->_serial);
#endif
  scope_insert(scope, ident, provided);
}

void
waiting_push(BHashTable *waiting, node_t *node, fiter_t fiter)
{
  node_t *ident = wait_context(node);
  assert(ident);
  node_ident_t *_ident = peek_ident(ident);
  BArray *      queue;
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
waiting_resume(context_t *cnt, node_t *ident)
{
  node_ident_t *_ident = peek_ident(ident);
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
  node_t **     tmp_node;
  node_t *      tmp_ident;

  bhtbl_foreach(cnt->waiting, iter)
  {
    q = bo_htbl_iter_peek_value(cnt->waiting, &iter, BArray *);
    assert(q);

    // bl_log("size %d", bo_array_size(q));
    for (size_t i = 0; i < bo_array_size(q); ++i) {
      tmp = bo_array_at(q, i, fiter_t);
      // bl_log("# %p index: %d", tmp.flatten, i);
      tmp_node = bo_array_at(tmp.flatten, tmp.i, node_t **);
      assert(*tmp_node);
      tmp_ident = wait_context(*tmp_node);
      if (!scope_has_symbol(cnt->provided_in_gscope, tmp_ident))
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
    tmp = bo_array_new(sizeof(node_t *));
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
  barray_foreach(cache, it)
  {
    bo_unref(it);
  }
}

void
flatten_push(BArray *flatten, node_t **node)
{
  bo_array_push_back(flatten, node);
}

void
flatten_node(context_t *cnt, BArray *fbuf, node_t **node)
{
  if (!*node) return;

#define flatten(_node) flatten_node(cnt, fbuf, (_node))

  switch (node_code(*node)) {
  case NODE_DECL: {
    node_decl_t *_decl = peek_decl(*node);
    /* store declaration for temporary use here, this scope is used only for searching truly
     * undefined symbols later */
    if (_decl->in_gscope && !scope_has_symbol(cnt->provided_in_gscope, _decl->name))
      scope_insert(cnt->provided_in_gscope, _decl->name, *node);

    flatten(&_decl->type);
    flatten(&_decl->value);
    break;
  }

  case NODE_LIT_FN: {
    node_lit_fn_t *_fn = peek_lit_fn(*node);
    flatten(&_fn->type);
    check_flatten(cnt, &_fn->block);
    return;
  }

  case NODE_LIT_STRUCT: {
    node_lit_struct_t *_struct = peek_lit_struct(*node);
    flatten(&_struct->type);
    return;
  }

  case NODE_LIT_ENUM: {
    node_lit_enum_t *_enum = peek_lit_enum(*node);
    flatten(&_enum->type);
    node_t **variant;
    node_foreach_ref(_enum->variants, variant)
    {
      flatten(variant);
    }
    return;
  }

  case NODE_BLOCK: {
    node_block_t *_block = peek_block(*node);

    node_t **tmp;
    node_foreach_ref(_block->nodes, tmp)
    {
      flatten(tmp);
    }
    return;
  }

  case NODE_UBLOCK: {
    node_ublock_t *_ublock = peek_ublock(*node);

    node_t **tmp;
    node_foreach_ref(_ublock->nodes, tmp)
    {
      check_flatten(cnt, tmp);
    }
    return;
  }

  case NODE_STMT_RETURN: {
    node_stmt_return_t *_return = peek_stmt_return(*node);
    flatten(&_return->expr);
    break;
  }

  case NODE_STMT_IF: {
    node_stmt_if_t *_if = peek_stmt_if(*node);
    flatten(&_if->test);
    flatten(&_if->true_stmt);
    flatten(&_if->false_stmt);
    break;
  }

  case NODE_STMT_LOOP: {
    node_stmt_loop_t *_loop = peek_stmt_loop(*node);
    flatten(&_loop->test);
    flatten(&_loop->true_stmt);
    break;
  }

  case NODE_EXPR_MEMBER: {
    node_expr_member_t *_member = peek_expr_member(*node);
    flatten(&_member->next);
    break;
  }

  case NODE_EXPR_ELEM: {
    node_expr_elem_t *_elem = peek_expr_elem(*node);
    flatten(&_elem->next);
    flatten(&_elem->index);
    break;
  }

  case NODE_EXPR_CAST: {
    node_expr_cast_t *_cast = peek_expr_cast(*node);
    flatten(&_cast->type);
    flatten(&_cast->next);
    break;
  }

  case NODE_EXPR_SIZEOF: {
    node_expr_sizeof_t *_sizeof = peek_expr_sizeof(*node);
    flatten(&_sizeof->in);
    flatten(&_sizeof->type);
    break;
  }

  case NODE_EXPR_CALL: {
    node_expr_call_t *_call = peek_expr_call(*node);
    flatten(&_call->ref);

    node_t **tmp;
    node_foreach_ref(_call->args, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case NODE_EXPR_BINOP: {
    node_expr_binop_t *_binop = peek_expr_binop(*node);
    flatten(&_binop->lhs);
    flatten(&_binop->rhs);
    break;
  }

  case NODE_EXPR_UNARY: {
    node_expr_unary_t *_unary = peek_expr_unary(*node);
    flatten(&_unary->next);
    break;
  }

  case NODE_TYPE_STRUCT: {
    node_type_struct_t *_struct_type = peek_type_struct(*node);

    node_t **tmp;
    node_foreach_ref(_struct_type->types, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case NODE_TYPE_ENUM: {
    node_type_enum_t *_enum_type = peek_type_enum(*node);
    flatten(&_enum_type->base_type);
    break;
  }

  case NODE_TYPE_FN: {
    node_type_fn_t *_type_fn = peek_type_fn(*node);
    flatten(&_type_fn->ret_type);
    node_t **sub_type;
    node_foreach_ref(_type_fn->arg_types, sub_type)
    {
      flatten(sub_type);
    }
    break;
  }

  case NODE_IDENT:
  case NODE_EXPR_NULL:
  case NODE_STMT_BREAK:
  case NODE_STMT_CONTINUE:
  case NODE_TYPE_FUND:
  case NODE_LIT:
  case NODE_LOAD:
  case NODE_LINK: break;
  default: bl_warning("missing flattening for node %s", node_name(*node));
  }

  flatten_push(fbuf, node);
#undef flatten
}

void
check_flatten(context_t *cnt, node_t **node)
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

  node_t **tmp;
  for (; fit->i < bo_array_size(fit->flatten); ++fit->i) {
    tmp = bo_array_at(fit->flatten, fit->i, node_t **);
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
implicit_cast(context_t *cnt, node_t **node, node_t *to_type)
{
  to_type           = ast_get_type(to_type);
  node_t *from_type = ast_get_type(*node);

  type_kind_e from_kind = ast_get_type_kind(from_type);
  type_kind_e to_kind   = ast_get_type_kind(to_type);
  if (node_is(*node, NODE_LIT) && from_kind != KIND_STRING && from_kind != KIND_CHAR &&
      from_kind != KIND_REAL &&
      (to_kind == KIND_SIZE || to_kind == KIND_UINT || to_kind == KIND_SINT)) {
    peek_lit(*node)->type = ast_node_dup(cnt->ast, to_type);
    return true;
  }

  if (!ast_can_impl_cast(from_type, to_type)) return false;

  node_t *tmp_next = (*node)->next;
  node_t *type_dup = ast_node_dup(cnt->ast, to_type);
  node_t *cast     = ast_expr_cast(cnt->ast, NULL, type_dup, *node);
  cast->next       = tmp_next;
  *node            = cast;

  return true;
}

void
check_error_invalid_types(context_t *cnt, node_t *first_type, node_t *second_type, node_t *err_pos)
{
  char tmp_first[256];
  char tmp_second[256];
  ast_type_to_string(tmp_first, 256, first_type);
  ast_type_to_string(tmp_second, 256, second_type);
  check_error_node(cnt, BL_ERR_INVALID_TYPE, err_pos, BL_BUILDER_CUR_WORD,
                   "no implicit cast for types '%s' and '%s'", tmp_first, tmp_second);
}

bool
check_expr_call(context_t *cnt, node_t **call)
{
  node_expr_call_t *_call = peek_expr_call(*call);

  node_t *ident =
      node_is(_call->ref, NODE_IDENT) ? _call->ref : peek_expr_member(_call->ref)->ident;

  node_t *callee = peek_ident(ident)->ref;
  assert(callee);
  node_decl_t *_callee     = peek_decl(callee);
  node_t *     callee_type = _callee->type;

  if (node_is_not(callee_type, NODE_TYPE_FN)) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, *call, BL_BUILDER_CUR_WORD,
                     "expected function name");
    FINISH;
  }

  node_type_fn_t *_callee_type = peek_type_fn(callee_type);

  _call->type = _callee_type->ret_type;

  if (_call->argsc != _callee_type->argc_types) {
    check_error_node(cnt, BL_ERR_INVALID_ARG_COUNT, *call, BL_BUILDER_CUR_WORD,
                     "expected %d %s, but called with %d", _callee_type->argc_types,
                     _callee_type->argc_types == 1 ? "argument" : "arguments", _call->argsc);
    FINISH;
  }

  node_t **call_arg   = &_call->args;
  node_t * callee_arg = _callee_type->arg_types;

  while (*call_arg) {
    if (node_is(*call_arg, NODE_EXPR_NULL)) {
      peek_expr_null(*call_arg)->type = ast_get_type(callee_arg);
    } else if (!ast_type_cmp(*call_arg, callee_arg) && !implicit_cast(cnt, call_arg, callee_arg)) {
      char tmp1[256];
      char tmp2[256];
      ast_type_to_string(tmp1, 256, ast_get_type(*call_arg));
      ast_type_to_string(tmp2, 256, ast_get_type(callee_arg));

      check_error_node(cnt, BL_ERR_INVALID_ARG_TYPE, *call_arg, BL_BUILDER_CUR_WORD,
                       "invalid call argument type, expected is '%s' but called with '%s'", tmp2,
                       tmp1);

      break;
    }

    call_arg   = &(*call_arg)->next;
    callee_arg = callee_arg->next;
  }

  if (_call->run) {
    type_kind_e callee_ret_tkind = ast_get_type_kind(ast_unroll_ident(_callee_type->ret_type));
    switch (callee_ret_tkind) {
    case KIND_FN:
    case KIND_PTR:
    case KIND_STRING:
    case KIND_STRUCT:
      check_error_node(cnt, BL_ERR_INVALID_TYPE, *call, BL_BUILDER_CUR_WORD,
                       "method called in compile time can return fundamental types only");
    default: break;
    }

    if (_call->argsc) {
      check_error_node(cnt, BL_ERR_INVALID_ARG_COUNT, *call, BL_BUILDER_CUR_WORD,
                       "method called in compile time cannot take arguments, remove '#run'?");
    }
  }

  FINISH;
}

bool
check_expr_unary(context_t *cnt, node_t **unary)
{
  node_expr_unary_t *_unary = peek_expr_unary(*unary);
  assert(_unary->next);

  if (_unary->op == BL_SYM_AND) {
    _unary->type = ast_node_dup(cnt->ast, ast_get_type(_unary->next));
    int ptr      = ast_type_get_ptr(_unary->type) + 1;
    ast_type_set_ptr(_unary->type, ptr);
  } else if (_unary->op == BL_SYM_ASTERISK) {
    _unary->type = ast_node_dup(cnt->ast, ast_get_type(_unary->next));
    int ptr      = ast_type_get_ptr(_unary->type) - 1;

    if (ptr < 0) {
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _unary->next, BL_BUILDER_CUR_WORD,
                       "cannot dereference non-pointer type");
    } else {
      ast_type_set_ptr(_unary->type, ptr);
    }
  } else {
    _unary->type = ast_get_type(_unary->next);
  }

  FINISH;
}

bool
check_node(context_t *cnt, node_t **node)
{
  assert(node);
  bool result = true;
#if defined(BL_DEBUG) && BL_VERBOSE_MUTIPLE_CHECK
  if (node->state == BL_CHECKED)
    bl_msg_warning("unnecessary node check %s (%d)", node_name(node), node->_serial);
#endif

  switch (node_code(*node)) {
  case NODE_IDENT: result = check_ident(cnt, node); break;
  case NODE_DECL: result = check_decl(cnt, node); break;
  case NODE_EXPR_CALL: result = check_expr_call(cnt, node); break;
  case NODE_EXPR_BINOP: result = check_expr_binop(cnt, node); break;
  case NODE_EXPR_CAST: result = check_expr_cast(cnt, node); break;
  case NODE_EXPR_UNARY: result = check_expr_unary(cnt, node); break;
  case NODE_EXPR_SIZEOF: result = check_expr_sizeof(cnt, node); break;
  case NODE_EXPR_MEMBER: result = check_expr_member(cnt, node); break;
  case NODE_EXPR_ELEM: result = check_expr_elem(cnt, node); break;
  case NODE_STMT_IF: result = check_stmt_if(cnt, node); break;
  case NODE_STMT_RETURN: result = check_stmt_return(cnt, node); break;
  case NODE_TYPE_ENUM: result = check_type_enum(cnt, node); break;
  case NODE_EXPR_NULL:
  case NODE_LIT:
  case NODE_STMT_BREAK:
  case NODE_STMT_CONTINUE:
  case NODE_STMT_LOOP:
  case NODE_TYPE_FN:
  case NODE_TYPE_STRUCT:
  case NODE_TYPE_FUND:
  case NODE_LOAD:
  case NODE_LINK: break;

  default: bl_warning("missing check for node type %s", node_name(*node));
  }

#if VERBOSE && defined(BL_DEBUG)
  {
    static int  prev_checked = -1;
    const char *file         = node->src ? node->src->unit->name : "UNKNOWN";
    const int   line         = node->src ? node->src->line : -1;
    node_t *    checked      = wait_context(node);
    const char *name         = checked ? peek_ident(checked)->str : "?";
    bl_log("checked [%s] " BL_MAGENTA("'%s'") " (%s, %d) file: " BL_YELLOW("%s") " line: " BL_CYAN(
               "%d"),
           result ? BL_GREEN(" OK ") : BL_RED("WAIT"), name, node_name(node), node->_serial, file,
           line);
    assert(prev_checked != node->_serial && "Looping checker!!!");
    prev_checked = node->_serial;
  }
#endif

  (*node)->state = result ? CHECKED : WAITING;
  return result;
}

bool
check_ident(context_t *cnt, node_t **ident)
{
  node_ident_t *_ident = peek_ident(*ident);
  if (_ident->ref) {
    FINISH;
  }

  node_t *  found;
  const int buildin = ast_is_buildin_type(*ident);
  if (buildin != -1) {
    /* connect buildin fundamental types references */
    found = &bl_ftypes[buildin];
  } else {
    found = lookup(*ident, NULL, true);
    if (!found) WAIT;
  }

  if (node_is(found, NODE_DECL)) {
    peek_decl(found)->used++;
  }

  _ident->ref = ast_unroll_ident(found);
  // assert(node_is(_ident->ref, NODE_DECL));

  if (_ident->ptr || _ident->arr) {
    /* when ident reference is pointer we need to create copy of declaration with different
     * type, maybe there is some better solution ??? */
    _ident->ref  = ast_node_dup(cnt->ast, found);
    node_t *type = ast_get_type(_ident->ref);
    assert(type);
    type = ast_node_dup(cnt->ast, type);
    ast_type_set_ptr(type, _ident->ptr);
    ast_type_set_arr(type, _ident->arr);
    if (ast_is_type(_ident->ref))
      _ident->ref = type;
    else
      ast_set_type(_ident->ref, type);
  }

  FINISH;
}

bool
check_stmt_return(context_t *cnt, node_t **ret)
{
  node_stmt_return_t *_ret = peek_stmt_return(*ret);
  assert(_ret->fn_decl);

  node_decl_t *  _callee_decl = peek_decl(_ret->fn_decl);
  node_lit_fn_t *_callee      = peek_lit_fn(_callee_decl->value);
  node_t *       fn_ret_type  = ast_get_type(peek_type_fn(_callee->type)->ret_type);
  if (fn_ret_type == NULL) WAIT;

  if (_ret->expr) {
    if (node_is(_ret->expr, NODE_EXPR_NULL)) {
      node_expr_null_t *_null = peek_expr_null(_ret->expr);
      _null->type             = fn_ret_type;
      if (ast_get_type_kind(_null->type) != KIND_PTR) {
        check_error_node(
            cnt, BL_ERR_INVALID_TYPE, _ret->expr, BL_BUILDER_CUR_WORD,
            "'null' cannot be used because the function does not return a pointer value");
      }
    } else {
      node_t *expr_type = ast_get_type(_ret->expr);
      if (!ast_type_cmp(expr_type, fn_ret_type) && !implicit_cast(cnt, &_ret->expr, fn_ret_type)) {
        check_error_invalid_types(cnt, expr_type, fn_ret_type, _ret->expr);
      }
    }
  } else {
    if (!ast_type_cmp(&bl_ftypes[BL_FTYPE_VOID], fn_ret_type)) {
      check_error_node(cnt, BL_ERR_EXPECTED_EXPR, *ret, BL_BUILDER_CUR_AFTER,
                       "expected return value");
    }
  }
  FINISH;
}

bool
check_expr_binop(context_t *cnt, node_t **binop)
{
  node_expr_binop_t *_binop = peek_expr_binop(*binop);

  assert(_binop->lhs);
  assert(_binop->rhs);

  if (_binop->op == BL_SYM_ASSIGN) {
    if (node_is_not(_binop->lhs, NODE_IDENT) && node_is_not(_binop->lhs, NODE_EXPR_UNARY) &&
        node_is_not(_binop->lhs, NODE_EXPR_ELEM) && node_is_not(_binop->lhs, NODE_EXPR_MEMBER)) {
      // TODO: temporary solution, what about (some_pointer + 1) = ...
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _binop->lhs, BL_BUILDER_CUR_WORD,
                       "left-hand side of assignment does not refer to any declaration and "
                       "cannot be assigned");
      FINISH;
    }

    if (node_is_not(_binop->lhs, NODE_EXPR_UNARY) && node_is_not(_binop->lhs, NODE_EXPR_ELEM) &&
        node_is_not(_binop->lhs, NODE_EXPR_MEMBER)) {
      node_ident_t *_lhs = peek_ident(_binop->lhs);
      assert(_lhs->ref);
      assert(node_is(_lhs->ref, NODE_DECL));

      if (!peek_decl(_lhs->ref)->mutable) {
        check_error_node(cnt, BL_ERR_INVALID_MUTABILITY, _binop->lhs, BL_BUILDER_CUR_WORD,
                         "declaration is not mutable and cannot be assigned");
      }
    }
  }

  node_t *lhs_type = ast_get_type(_binop->lhs);
  assert(lhs_type);
  type_kind_e lhs_kind = ast_get_type_kind(lhs_type);

  if (node_is(_binop->rhs, NODE_EXPR_NULL)) {
    if (lhs_kind != KIND_PTR) {
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _binop->lhs, BL_BUILDER_CUR_WORD,
                       "expected a pointer type");
    } else {
      peek_expr_null(_binop->rhs)->type = lhs_type;
      if (!_binop->type) _binop->type = lhs_type;
    }

    FINISH;
  }

  node_t *rhs_type = ast_get_type(_binop->rhs);
  assert(rhs_type);
  type_kind_e rhs_kind = ast_get_type_kind(rhs_type);

  if (!ast_type_cmp(lhs_type, rhs_type)) {
    if (node_is(_binop->lhs, NODE_LIT) &&
        (lhs_kind == KIND_SIZE || lhs_kind == KIND_UINT || lhs_kind == KIND_SINT) &&
        (rhs_kind == KIND_SIZE || rhs_kind == KIND_UINT || rhs_kind == KIND_SINT)) {
      peek_lit(_binop->lhs)->type = ast_node_dup(cnt->ast, rhs_type);
    } else if (!implicit_cast(cnt, &_binop->rhs, lhs_type)) {
      check_error_invalid_types(cnt, lhs_type, rhs_type, *binop);
    }
  }

  if (!_binop->type) _binop->type = ast_get_type(_binop->lhs);

  FINISH;
}

bool
check_expr_cast(context_t *cnt, node_t **cast)
{
  node_expr_cast_t *_cast = peek_expr_cast(*cast);
  assert(_cast->type);
  _cast->type = ast_get_type(_cast->type);
  FINISH;
}

bool
check_expr_sizeof(context_t *cnt, node_t **szof)
{
  node_expr_sizeof_t *_sizeof = peek_expr_sizeof(*szof);
  _sizeof->in                 = ast_get_type(_sizeof->in);
  FINISH;
}

bool
check_type_enum(context_t *cnt, node_t **type)
{
  node_type_enum_t *_type = peek_type_enum(*type);
  assert(_type->base_type);
  node_t *tmp = ast_get_type(_type->base_type);

  if (node_is_not(tmp, NODE_TYPE_FUND)) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, _type->base_type, BL_BUILDER_CUR_WORD,
                     "enum base type must be an integer type");
    FINISH;
  }

  switch (peek_type_fund(tmp)->code) {
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

  if (ast_type_get_ptr(tmp)) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, _type->base_type, BL_BUILDER_CUR_WORD,
                     "enum base type cannot be a pointer");
  }

  _type->base_type = tmp;

  FINISH;
}

bool
check_expr_member(context_t *cnt, node_t **member)
{
  node_expr_member_t *_member = peek_expr_member(*member);
  node_t *            found   = NULL;
  assert(_member->next);
  assert(_member->ident);

  node_t *lhs_type = ast_get_type(_member->next);
  if (!lhs_type) FINISH;
  if (ast_type_get_arr(lhs_type)) {
    /* is member array 'count'??? */
    if (ast_is_buildin(_member->ident) == BL_BUILDIN_ARR_COUNT) {
      node_t *tmp_next = (*member)->next;
      *member          = ast_node_dup(cnt->ast, ast_type_get_arr(lhs_type));
      (*member)->next  = tmp_next;

      // TODO: set next node???

      FINISH;
    }
  } else if (node_is(lhs_type, NODE_TYPE_STRUCT)) {
    /* structure member */
    _member->kind = MEM_KIND_STRUCT;

    node_type_struct_t *_lhs_type = peek_type_struct(lhs_type);
    /* lhs_type cannot be anonymous structure type (generate error later instead of assert?) */
    assert(_lhs_type->base_decl);

    found = _lookup(peek_decl(_lhs_type->base_decl)->value, _member->ident, NULL, false);
    if (!found) WAIT;

    if (_member->ptr_ref != (_lhs_type->ptr ? true : false)) {
      check_error_node(cnt, BL_ERR_INVALID_MEMBER_ACCESS, *member, BL_BUILDER_CUR_WORD,
                       "invalid member access, use %s",
                       _member->ptr_ref ? "'.' instead of '->'" : "'->' instead of '.'");
    }
  } else if (node_is(lhs_type, NODE_TYPE_ENUM)) {
    /* enum variant */
    _member->kind               = MEM_KIND_ENUM;
    node_type_enum_t *_lhs_type = peek_type_enum(lhs_type);
    assert(_lhs_type->base_decl);

    found = _lookup(peek_decl(_lhs_type->base_decl)->value, _member->ident, NULL, false);
    if (!found) WAIT;

    if (_member->ptr_ref) {
      check_error_node(cnt, BL_ERR_INVALID_MEMBER_ACCESS, *member, BL_BUILDER_CUR_WORD,
                       "use '.' for access to enum variants");
    }
  } else {
    check_error_node(cnt, BL_ERR_EXPECTED_TYPE_STRUCT, _member->next, BL_BUILDER_CUR_WORD,
                     "expected structure or enum");
  }

  _member->type                   = ast_get_type(found);
  peek_ident(_member->ident)->ref = found;

  FINISH;
}

bool
check_expr_elem(context_t *cnt, node_t **elem)
{
  node_expr_elem_t *_elem = peek_expr_elem(*elem);
  assert(_elem->index);
  assert(_elem->next);

  _elem->type = ast_get_type(_elem->next);
  if (!ast_type_get_arr(_elem->type)) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, *elem, BL_BUILDER_CUR_WORD, "expected array");
  }

  _elem->type = ast_node_dup(cnt->ast, _elem->type);
  ast_type_set_arr(_elem->type, NULL);

  node_t *index_type = ast_get_type(_elem->index);

  if (!implicit_cast(cnt, &_elem->index, &bl_ftypes[BL_FTYPE_SIZE])) {
    check_error_invalid_types(cnt, index_type, &bl_ftypes[BL_FTYPE_SIZE], _elem->index);
  }

  FINISH;
}

bool
check_stmt_if(context_t *cnt, node_t **stmt_if)
{
  node_stmt_if_t *_if = peek_stmt_if(*stmt_if);
  assert(_if->test);
  assert(_if->true_stmt);

  node_t *test_type = ast_get_type(_if->test);
  if (!ast_type_cmp(test_type, &bl_ftypes[BL_FTYPE_BOOL])) {
    check_error_invalid_types(cnt, test_type, &bl_ftypes[BL_FTYPE_BOOL], _if->test);
  }
  FINISH;
}

bool
infer_type(context_t *cnt, node_t *decl)
{
  node_decl_t *_decl = peek_decl(decl);
  if (!_decl->value) return false;
  node_t *inferred_type = ast_get_type(_decl->value);
  if (!inferred_type) return false;

  if (_decl->type && !ast_type_cmp(inferred_type, _decl->type) &&
      !implicit_cast(cnt, &_decl->value, _decl->type)) {
    check_error_invalid_types(cnt, _decl->type, inferred_type, _decl->value);
    return false;
  }

  /* infer type from value */
  _decl->type = ast_get_type(_decl->value);
  return true;
}

bool
check_decl(context_t *cnt, node_t **decl)
{
  node_decl_t *_decl          = peek_decl(*decl);
  bool         lookup_in_tree = true;

  assert(_decl->name);
  infer_type(cnt, *decl);

  _decl->in_gscope =
      ast_get_scope(peek_ident(_decl->name)->parent_compound) == cnt->assembly->gscope;

  if (_decl->flags & FLAG_MAIN && _decl->kind != DECL_KIND_FN) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, *decl, BL_BUILDER_CUR_WORD,
                     "main is expected to be function");
  }

  switch (_decl->kind) {
  case DECL_KIND_STRUCT: {
    node_t *value_type                      = ast_get_type(_decl->value);
    peek_type_struct(value_type)->base_decl = *decl;

    if (_decl->mutable) {
      check_error_node(cnt, BL_ERR_INVALID_MUTABILITY, *decl, BL_BUILDER_CUR_WORD,
                       "structure declaration cannot be mutable");
    }

    if (peek_type_struct(value_type)->typesc == 0) {
      check_error_node(cnt, BL_ERR_EMPTY, _decl->name, BL_BUILDER_CUR_WORD, "empty structure");
    }

    break;
  }

  case DECL_KIND_MEMBER: {
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

  case DECL_KIND_FIELD: {
    assert(_decl->mutable);

    if (ast_get_type_kind(ast_get_type(_decl->type)) == KIND_FN) {
      char tmp[256];
      ast_type_to_string(tmp, 256, ast_get_type(_decl->type));
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->name, BL_BUILDER_CUR_WORD,
                       "invalid type of variable '%s'", tmp);
    }

    if (_decl->in_gscope && !_decl->value) {
      check_error_node(cnt, BL_ERR_EXPECTED_EXPR, _decl->type, BL_BUILDER_CUR_AFTER,
                       "global variables needs to be initialized");
    }
    break;
  }

  case DECL_KIND_CONSTANT: {
    assert(!_decl->mutable);

    if (ast_get_type_kind(ast_get_type(_decl->type)) == KIND_FN ||
        ast_get_type_kind(ast_get_type(_decl->type)) == KIND_STRUCT) {
      char tmp[256];
      ast_type_to_string(tmp, 256, ast_get_type(_decl->type));
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->name, BL_BUILDER_CUR_WORD,
                       "invalid type of constant '%s'", tmp);
    }
    break;
  }

  case DECL_KIND_ARG: {
    if (_decl->value) {
      check_error_node(cnt, BL_ERR_INVALID_ARG_TYPE, *decl, BL_BUILDER_CUR_WORD,
                       "function arguments cannot have value binding");
    }
    break;
  }

  case DECL_KIND_VARIANT: {
    if (_decl->mutable) {
      check_error_node(cnt, BL_ERR_INVALID_MUTABILITY, *decl, BL_BUILDER_CUR_WORD,
                       "an enum variant cannot be mutable");
    }
    break;
  }

  case DECL_KIND_ENUM: {
    node_t *value_type                    = ast_get_type(_decl->value);
    peek_type_enum(value_type)->base_decl = *decl;
    break;
  }

  case DECL_KIND_FN: break;
  case DECL_KIND_TYPE: bl_abort("unimplemented");
  case DECL_KIND_UNKNOWN: bl_abort("unknown declaration kind");
  }

  assert(_decl->type);
  type_kind_e type_kind = ast_get_type_kind(ast_get_type(_decl->type));
  if (type_kind == KIND_VOID) {
    char tmp[256];
    ast_type_to_string(tmp, 256, ast_get_type(_decl->type));
    check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->name, BL_BUILDER_CUR_WORD,
                     "declaration has invalid type '%s'", tmp);
  }

  if (type_kind == KIND_FN && _decl->mutable) {
    char tmp[256];
    ast_type_to_string(tmp, 256, ast_get_type(_decl->type));
    check_error_node(
        cnt, BL_ERR_INVALID_TYPE, _decl->name, BL_BUILDER_CUR_WORD,
        "declaration has invalid type '%s', a function mutable must be referenced by pointer", tmp);
  }
  _decl->type = ast_get_type(_decl->type);

  /* infer type for 'null' value */
  if (_decl->value && node_is(_decl->value, NODE_EXPR_NULL)) {
    if (type_kind == KIND_PTR) {
      peek_expr_null(_decl->value)->type = _decl->type;
    } else {
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->value, BL_BUILDER_CUR_WORD,
                       "'null' cannot be used because the declaration is not a pointer");
    }
  }

  /* provide symbol into scope if there is no conflict */
  node_t *conflict = lookup(_decl->name, NULL, lookup_in_tree);
  if (conflict) {
    check_error_node(cnt, BL_ERR_DUPLICATE_SYMBOL, *decl, BL_BUILDER_CUR_WORD,
                     "symbol with same name already declared here: %s:%d",
                     conflict->src->unit->filepath, conflict->src->line);
  } else {
    provide(_decl->name, *decl);
    waiting_resume(cnt, _decl->name);
  }

  FINISH;
}

void
checker_run(builder_t *builder, assembly_t *assembly)
{
  context_t cnt = {
      .builder            = builder,
      .assembly           = assembly,
      .unit               = NULL,
      .ast                = NULL,
      .waiting            = bo_htbl_new_bo(bo_typeof(BArray), true, 2048),
      .flatten_cache      = bo_array_new(sizeof(BArray *)),
      .provided_in_gscope = scope_new(assembly->scope_cache, 4092),
  };

  unit_t *unit;
  barray_foreach(assembly->units, unit)
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
