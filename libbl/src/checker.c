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

#include "stages_impl.h"
#include "common_impl.h"
#include "ast_impl.h"

#define VERBOSE 0

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

  BArray *    waiting_resumed;
  BHashTable *waiting;
  bl_scope_t *provided_in_gscope;
} context_t;

typedef struct
{
  BArray *flatten;
  size_t     i;
} fiter_t;

static inline bl_node_t *
lookup(bl_node_t *ident, bl_scope_t **out_scope, bool walk_tree);

static bl_node_t *
_lookup(bl_node_t *compound, bl_node_t *ident, bl_scope_t **out_scope, bool walk_tree);

static bool
infer_type(context_t *cnt, bl_node_t *decl);

static void
provide(bl_node_t *ident, bl_node_t *provided);

static inline void
waiting_push(BHashTable *waiting, uint64_t hash, fiter_t fiter);

static void
waiting_resume(context_t *cnt, uint64_t hash);

static void
waiting_resume_all(context_t *cnt);

static BArray *
flatten_new(void);

static void
flatten_delete(BArray *flatten);

static inline void
flatten_push(BArray *flatten, bl_node_t *node);

static void
flatten_node(context_t *cnt, BArray *fbuf, bl_node_t *node);

static bool
implicit_cast(context_t *cnt, bl_node_t **node, bl_node_t *to_type);

static inline void
check_error_invalid_types(context_t *cnt, bl_node_t *first_type, bl_node_t *second_type,
                          bl_node_t *err_pos);

static void
check_flatten(context_t *cnt, bl_node_t *node);

static bool
check_node(context_t *cnt, bl_node_t *node);

static bool
check_ident(context_t *cnt, bl_node_t *ident);

static bool
check_decl_value(context_t *cnt, bl_node_t *decl);

static bool
check_expr_call(context_t *cnt, bl_node_t *call);

static bool
check_expr_unary(context_t *cnt, bl_node_t *unary);

static bool
check_expr_binop(context_t *cnt, bl_node_t *binop);

static bool
check_expr_cast(context_t *cnt, bl_node_t *cast);

static bool
check_expr_sizeof(context_t *cnt, bl_node_t *szof);

static bool
check_expr_member(context_t *cnt, bl_node_t *member);

static bool
check_stmt_if(context_t *cnt, bl_node_t *stmt_if);

static bool
check_stmt_return(context_t *cnt, bl_node_t *ret);

static bool
check_type_enum(context_t *cnt, bl_node_t *type);

static void
check_unresolved(context_t *cnt);

static void
_check_unused(context_t *cnt, bl_node_t *node);

static void
check_unused(context_t *cnt);

// impl
bl_node_t *
lookup(bl_node_t *ident, bl_scope_t **out_scope, bool walk_tree)
{
  assert(ident);
  bl_node_t *compound = bl_peek_ident(ident)->parent_compound;
  assert(compound);
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

void
provide(bl_node_t *ident, bl_node_t *provided)
{
  assert(ident && provided);
  bl_node_t *compound = bl_peek_ident(ident)->parent_compound;
  assert(compound);
  bl_scope_t *scope = bl_ast_get_scope(compound);
  assert(scope);

  /* bl_log("providing: %s (%d)", bl_peek_ident(ident)->str, ident->_serial) */
  bl_scope_insert(scope, ident, provided);
}

void
waiting_push(BHashTable *waiting, uint64_t hash, fiter_t fiter)
{
  BArray *queue = NULL;
  if (bo_htbl_has_key(waiting, hash)) {
    queue = bo_htbl_at(waiting, hash, BArray *);
  } else {
    queue = bo_array_new(sizeof(fiter_t));
    bo_htbl_insert(waiting, hash, queue);
  }

  bo_array_push_back(queue, fiter);
}

void
waiting_resume(context_t *cnt, uint64_t hash)
{
  if (!bo_htbl_has_key(cnt->waiting, hash)) return;
  bo_array_push_back(cnt->waiting_resumed, hash);
}

void
waiting_resume_all(context_t *cnt)
{
  bo_iterator_t it = bo_array_begin(cnt->waiting_resumed);
  for (; !bo_array_empty(cnt->waiting_resumed); it = bo_array_begin(cnt->waiting_resumed)) {
    uint64_t hash = bo_array_iter_peek(cnt->waiting_resumed, &it, uint64_t);
    bo_array_erase(cnt->waiting_resumed, 0);

    BArray *q = bo_htbl_at(cnt->waiting, hash, BArray *);
    assert(q);

    //bl_log("q.len = %d", bo_array_size(q));
    fiter_t fit;
    for (size_t i = 0; i < bo_array_size(q);) {
      fit = bo_array_at(q, i, fiter_t);
      bo_array_erase(q, i);

      bool interrupted = false;
      /* resume here */
      bl_node_t *tmp;
      for (; fit.i < bo_array_size(fit.flatten); ++fit.i) {
        tmp = bo_array_at(fit.flatten, fit.i, bl_node_t *);
        if (!check_node(cnt, tmp)) {
          bl_node_ident_t *_ident = bl_peek_ident(tmp);
          waiting_push(cnt->waiting, _ident->hash, fit);
          interrupted = true;
          break;
        }
      }

      if (!interrupted) {
        flatten_delete(fit.flatten);
      } else {
        //++i;
      }
    }
    if (!bo_array_size(q)) {
      bo_htbl_erase_key(cnt->waiting, hash);
    }
  }
}

void
check_unresolved(context_t *cnt)
{
  bo_iterator_t    iter;
  BArray *q;
  fiter_t          tmp;
  bl_node_t *      tmp_node;

  bl_bhtbl_foreach(cnt->waiting, iter)
  {
    q = bo_htbl_iter_peek_value(cnt->waiting, &iter, BArray *);
    assert(q);

    bl_log("size %d", bo_array_size(q));
    for (size_t i = 0; i < bo_array_size(q); ++i) {
      tmp = bo_array_at(q, i, fiter_t);
      bl_log("# %p index: %d", tmp.flatten, i);
      tmp_node = bo_array_at(tmp.flatten, tmp.i, bl_node_t *);
      if (!bl_scope_has_symbol(cnt->provided_in_gscope, tmp_node))
        check_error_node(cnt, BL_ERR_UNKNOWN_SYMBOL, tmp_node, BL_BUILDER_CUR_WORD,
                         "unknown symbol");
      flatten_delete(tmp.flatten);
    }
  }
}

#if BL_DEBUG
static int _flatten = 0;
#endif
BArray *
flatten_new(void)
{
  BArray *tmp = bo_array_new(sizeof(bl_node_t *));
#if BL_DEBUG
  ++_flatten;
#endif
  return tmp;
}

void
flatten_delete(BArray *flatten)
{
#if BL_DEBUG
  --_flatten;
#endif
  bo_unref(flatten);
}

void
flatten_push(BArray *flatten, bl_node_t *node)
{
  bo_array_push_back(flatten, node);
}

void
flatten_node(context_t *cnt, BArray *fbuf, bl_node_t *node)
{
  if (!node) return;

#define flatten(_node) flatten_node(cnt, fbuf, (_node))

  switch (bl_node_code(node)) {
  case BL_NODE_DECL_VALUE: {
    bl_node_decl_value_t *_decl = bl_peek_decl_value(node);

    {
      /* store declaration for temporary use here, this scope is used only for searching truly
       * undefined symbols later */
      const bool is_in_gscope =
          bl_ast_get_scope(bl_peek_ident(_decl->name)->parent_compound) == cnt->assembly->gscope;

      if (is_in_gscope && !bl_scope_has_symbol(cnt->provided_in_gscope, _decl->name))
        bl_scope_insert(cnt->provided_in_gscope, _decl->name, node);
    }

    flatten(_decl->type);
    flatten(_decl->value);
    break;
  }

  case BL_NODE_TYPE_FN: {
    bl_node_type_fn_t *_type_fn = bl_peek_type_fn(node);
    flatten(_type_fn->ret_type);
    bl_node_t *sub_type;
    bl_node_foreach(_type_fn->arg_types, sub_type)
    {
      flatten(sub_type);
    }
    break;
  }

  case BL_NODE_LIT_FN: {
    bl_node_lit_fn_t *_fn = bl_peek_lit_fn(node);
    check_flatten(cnt, _fn->type);
    check_flatten(cnt, _fn->block);
    return;
  }

  case BL_NODE_LIT_STRUCT: {
    bl_node_lit_struct_t *_struct = bl_peek_lit_struct(node);
    flatten(_struct->type);
    return;
  }

  case BL_NODE_LIT_ENUM: {
    bl_node_lit_enum_t *_enum = bl_peek_lit_enum(node);
    flatten(_enum->type);
    bl_node_t *variant;
    bl_node_foreach(_enum->variants, variant)
    {
      flatten(variant);
    }
    return;
  }

  case BL_NODE_DECL_BLOCK: {
    bl_node_decl_block_t *_block = bl_peek_decl_block(node);
    bl_node_t *           tmp;
    bl_node_foreach(_block->nodes, tmp)
    {
      flatten(tmp);
    }
    return;
  }

  case BL_NODE_DECL_UBLOCK: {
    bl_node_decl_ublock_t *_ublock = bl_peek_decl_ublock(node);
    bl_node_t *            tmp;
    bl_node_foreach(_ublock->nodes, tmp)
    {
      check_flatten(cnt, tmp);
    }
    return;
  }

  case BL_NODE_STMT_RETURN: {
    bl_node_stmt_return_t *_return = bl_peek_stmt_return(node);
    flatten(_return->expr);
    break;
  }

  case BL_NODE_STMT_IF: {
    bl_node_stmt_if_t *_if = bl_peek_stmt_if(node);
    flatten(_if->test);
    flatten(_if->true_stmt);
    flatten(_if->false_stmt);
    break;
  }

  case BL_NODE_STMT_LOOP: {
    bl_node_stmt_loop_t *_loop = bl_peek_stmt_loop(node);
    flatten(_loop->test);
    flatten(_loop->true_stmt);
    break;
  }

  case BL_NODE_EXPR_MEMBER: {
    bl_node_expr_member_t *_member = bl_peek_expr_member(node);
    flatten(_member->next);
    break;
  }

  case BL_NODE_EXPR_CAST: {
    bl_node_expr_cast_t *_cast = bl_peek_expr_cast(node);
    flatten(_cast->type);
    flatten(_cast->next);
    break;
  }

  case BL_NODE_EXPR_SIZEOF: {
    bl_node_expr_sizeof_t *_sizeof = bl_peek_expr_sizeof(node);
    flatten(_sizeof->in);
    flatten(_sizeof->type);
    break;
  }

  case BL_NODE_EXPR_CALL: {
    bl_node_expr_call_t *_call = bl_peek_expr_call(node);
    flatten(_call->ident);

    bl_node_t *tmp;
    bl_node_foreach(_call->args, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case BL_NODE_EXPR_BINOP: {
    bl_node_expr_binop_t *_binop = bl_peek_expr_binop(node);
    flatten(_binop->lhs);
    flatten(_binop->rhs);
    break;
  }

  case BL_NODE_EXPR_UNARY: {
    bl_node_expr_unary_t *_unary = bl_peek_expr_unary(node);
    flatten(_unary->next);
    break;
  }

  case BL_NODE_TYPE_STRUCT: {
    bl_node_type_struct_t *_struct_type = bl_peek_type_struct(node);
    bl_node_t *            tmp;
    bl_node_foreach(_struct_type->types, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case BL_NODE_TYPE_ENUM: {
    bl_node_type_enum_t *_enum_type = bl_peek_type_enum(node);
    flatten(_enum_type->base_type);
    break;
  }

  case BL_NODE_EXPR_NULL:
  case BL_NODE_STMT_BREAK:
  case BL_NODE_STMT_CONTINUE:
  case BL_NODE_IDENT:
  case BL_NODE_TYPE_FUND:
  case BL_NODE_LIT:
  case BL_NODE_LOAD:
    break;
  default:
    bl_warning("missing flattening for node %s", bl_node_name(node));
  }

  flatten_push(fbuf, node);
#undef flatten
}

void
check_flatten(context_t *cnt, bl_node_t *node)
{
  fiter_t fit;
  fit.flatten = flatten_new();
  fit.i       = 0;

  flatten_node(cnt, fit.flatten, node);
  bool interrupted = false;

  bl_node_t *tmp;
  for (; fit.i < bo_array_size(fit.flatten); ++fit.i) {
    tmp = bo_array_at(fit.flatten, fit.i, bl_node_t *);
    if (!check_node(cnt, tmp)) {
      /* node has not been satisfied and need to be checked later when all it's references become
       * available */
      bl_node_ident_t *_ident = bl_peek_ident(tmp);
      waiting_push(cnt->waiting, _ident->hash, fit);
      interrupted = true;
      break;
    }
  }

  if (!interrupted) flatten_delete(fit.flatten);
}

bool
implicit_cast(context_t *cnt, bl_node_t **node, bl_node_t *to_type)
{
  to_type              = bl_ast_get_type(to_type);
  bl_node_t *from_type = bl_ast_get_type(*node);

  bl_type_kind_e to_kind = bl_ast_get_type_kind(to_type);
  if (bl_node_is(*node, BL_NODE_LIT) &&
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
check_expr_call(context_t *cnt, bl_node_t *call)
{
  bl_node_expr_call_t *_call = bl_peek_expr_call(call);

  bl_node_t *callee = bl_peek_ident(_call->ident)->ref;
  assert(callee);
  bl_node_t *callee_type = bl_peek_decl_value(callee)->type;

  if (bl_node_is_not(callee_type, BL_NODE_TYPE_FN)) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, call, BL_BUILDER_CUR_WORD, "expected function name");
    FINISH;
  }

  bl_node_type_fn_t *_callee_type = bl_peek_type_fn(callee_type);

  _call->type = _callee_type->ret_type;

  if (_call->argsc != _callee_type->argc_types) {
    check_error_node(cnt, BL_ERR_INVALID_ARG_COUNT, call, BL_BUILDER_CUR_WORD,
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
check_expr_unary(context_t *cnt, bl_node_t *unary)
{
  bl_node_expr_unary_t *_unary = bl_peek_expr_unary(unary);
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
check_node(context_t *cnt, bl_node_t *node)
{
  assert(node);
  bool result = true;

  switch (bl_node_code(node)) {
  case BL_NODE_IDENT:
    result = check_ident(cnt, node);
    break;

  case BL_NODE_DECL_VALUE:
    result = check_decl_value(cnt, node);
    break;

  case BL_NODE_EXPR_CALL:
    result = check_expr_call(cnt, node);
    break;

  case BL_NODE_EXPR_BINOP:
    result = check_expr_binop(cnt, node);
    break;

  case BL_NODE_EXPR_CAST:
    result = check_expr_cast(cnt, node);
    break;

  case BL_NODE_EXPR_UNARY:
    result = check_expr_unary(cnt, node);
    break;

  case BL_NODE_EXPR_SIZEOF:
    result = check_expr_sizeof(cnt, node);
    break;

  case BL_NODE_EXPR_MEMBER:
    result = check_expr_member(cnt, node);
    break;

  case BL_NODE_STMT_IF:
    result = check_stmt_if(cnt, node);
    break;

  case BL_NODE_STMT_RETURN:
    result = check_stmt_return(cnt, node);
    break;

  case BL_NODE_TYPE_ENUM:
    result = check_type_enum(cnt, node);
    break;

  case BL_NODE_EXPR_NULL:
  case BL_NODE_LIT:
  case BL_NODE_STMT_BREAK:
  case BL_NODE_STMT_CONTINUE:
  case BL_NODE_STMT_LOOP:
  case BL_NODE_TYPE_FN:
  case BL_NODE_TYPE_STRUCT:
  case BL_NODE_TYPE_FUND:
  case BL_NODE_LOAD:
    break;

  default:
    bl_warning("missing check for node type %s", bl_node_name(node));
  }

#if VERBOSE
  bl_log("checked [%s] %s (%d) on line: %d", result ? BL_GREEN(" OK ") : BL_RED("WAIT"),
         bl_node_is(node, BL_NODE_IDENT) ? bl_peek_ident(node)->str : bl_node_name(node),
         node->_serial, node->src ? node->src->line : 0);
#endif

  return result;
}

bool
check_ident(context_t *cnt, bl_node_t *ident)
{
  bl_node_ident_t *_ident = bl_peek_ident(ident);
  if (_ident->ref) FINISH;

  bl_node_t *found;
  const int  buildin = bl_ast_is_buildin_type(ident);
  if (buildin != -1) {
    found = &bl_ftypes[buildin];
  } else {
    found = lookup(ident, NULL, true);
    if (!found) WAIT;
  }

  if (bl_node_is(found, BL_NODE_DECL_VALUE)) {
    bl_peek_decl_value(found)->used++;
  }

  _ident->ref = bl_ast_unroll_ident(found);
  assert(bl_node_is(_ident->ref, BL_NODE_DECL_VALUE));

  if (_ident->ptr) {
    /* when ident reference is pointer we need to create copy of declaration with different
     * type, maybe there is some better solution ??? */
    _ident->ref     = bl_ast_node_dup(cnt->ast, found);
    bl_node_t *type = bl_ast_get_type(_ident->ref);
    assert(type);
    type = bl_ast_node_dup(cnt->ast, type);
    bl_ast_type_set_ptr(type, _ident->ptr);
    bl_ast_set_type(_ident->ref, type);
  }

  FINISH;
}

bool
check_stmt_return(context_t *cnt, bl_node_t *ret)
{
  bl_node_stmt_return_t *_ret = bl_peek_stmt_return(ret);
  assert(_ret->fn);

  bl_node_lit_fn_t *_callee     = bl_peek_lit_fn(_ret->fn);
  bl_node_t *       fn_ret_type = bl_ast_get_type(bl_peek_type_fn(_callee->type)->ret_type);

  if (_ret->expr) {
    bl_node_t *expr_type = bl_ast_get_type(_ret->expr);

    if (!bl_ast_type_cmp(expr_type, fn_ret_type) && !implicit_cast(cnt, &_ret->expr, fn_ret_type)) {
      check_error_invalid_types(cnt, expr_type, fn_ret_type, _ret->expr);
    }
  } else {
    if (!bl_ast_type_cmp(&bl_ftypes[BL_FTYPE_VOID], fn_ret_type)) {
      check_error_node(cnt, BL_ERR_EXPECTED_EXPR, ret, BL_BUILDER_CUR_AFTER,
                       "expected return value");
    }
  }
  FINISH;
}

bool
check_expr_binop(context_t *cnt, bl_node_t *binop)
{
  bl_node_expr_binop_t *_binop = bl_peek_expr_binop(binop);

  assert(_binop->lhs);
  assert(_binop->rhs);

  if (_binop->op == BL_SYM_ASSIGN) {
    if (bl_node_is_not(_binop->lhs, BL_NODE_IDENT) &&
        bl_node_is_not(_binop->lhs, BL_NODE_EXPR_UNARY) &&
        bl_node_is_not(_binop->lhs, BL_NODE_EXPR_MEMBER)) {
      // TODO: temporary solution, what about (some_pointer + 1) = ...
      check_error_node(cnt, BL_ERR_INVALID_TYPE, _binop->lhs, BL_BUILDER_CUR_WORD,
                       "left-hand side of assignment does not refer to any declaration and "
                       "cannot be assigned");
      FINISH;
    }

    if (bl_node_is_not(_binop->lhs, BL_NODE_EXPR_UNARY) &&
        bl_node_is_not(_binop->lhs, BL_NODE_EXPR_MEMBER)) {
      bl_node_ident_t *_lhs = bl_peek_ident(_binop->lhs);
      assert(_lhs->ref);
      assert(bl_node_is(_lhs->ref, BL_NODE_DECL_VALUE));

      if (!bl_peek_decl_value(_lhs->ref)->mutable) {
        check_error_node(cnt, BL_ERR_INVALID_MUTABILITY, _binop->lhs, BL_BUILDER_CUR_WORD,
                         "declaration is not mutable and cannot be assigned");
      }
    }
  }

  bl_node_t *lhs_type = bl_ast_get_type(_binop->lhs);
  assert(lhs_type);
  if (bl_node_is(_binop->rhs, BL_NODE_EXPR_NULL)) {
    bl_peek_expr_null(_binop->rhs)->type = lhs_type;
    if (!_binop->type) _binop->type = lhs_type;
    FINISH;
  }

  bl_node_t *rhs_type = bl_ast_get_type(_binop->rhs);

  if (!bl_ast_type_cmp(lhs_type, rhs_type) && !implicit_cast(cnt, &_binop->rhs, lhs_type)) {
    check_error_invalid_types(cnt, lhs_type, rhs_type, binop);
  }

  if (!_binop->type) _binop->type = lhs_type;

  FINISH;
}

bool
check_expr_cast(context_t *cnt, bl_node_t *cast)
{
  bl_node_expr_cast_t *_cast = bl_peek_expr_cast(cast);
  assert(_cast->type);
  _cast->type = bl_ast_get_type(_cast->type);
  FINISH;
}

bool
check_expr_sizeof(context_t *cnt, bl_node_t *szof)
{
  bl_node_expr_sizeof_t *_sizeof = bl_peek_expr_sizeof(szof);
  _sizeof->in                    = bl_ast_get_type(_sizeof->in);
  FINISH;
}

bool
check_type_enum(context_t *cnt, bl_node_t *type)
{
  bl_node_type_enum_t *_type = bl_peek_type_enum(type);
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
  case BL_FTYPE_CHAR:
    break;
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
check_expr_member(context_t *cnt, bl_node_t *member)
{
  bl_node_expr_member_t *_member = bl_peek_expr_member(member);
  bl_node_t *            found   = NULL;
  assert(_member->next);
  assert(_member->ident);

  bl_node_t *lhs_type = bl_ast_get_type(_member->next);
  if (!lhs_type) FINISH;

  if (bl_node_is(lhs_type, BL_NODE_TYPE_STRUCT)) {
    /* structure member */
    _member->kind = BL_MEM_KIND_STRUCT;

    bl_node_type_struct_t *_lhs_type = bl_peek_type_struct(lhs_type);
    /* lhs_type cannot be anonymous structure type (generate error later instead of assert?) */
    assert(_lhs_type->base_decl);

    found = _lookup(bl_peek_decl_value(_lhs_type->base_decl)->value, _member->ident, NULL, false);
    if (!found) {
      check_error_node(cnt, BL_ERR_UNKNOWN_SYMBOL, _member->ident, BL_BUILDER_CUR_WORD,
                       "unknown structure member");
      FINISH;
    }

    if (_member->ptr_ref != (_lhs_type->ptr ? true : false)) {
      check_error_node(cnt, BL_ERR_INVALID_MEMBER_ACCESS, member, BL_BUILDER_CUR_WORD,
                       "invalid member access, use %s",
                       _member->ptr_ref ? "'.' instead of '->'" : "'->' instead of '.'");
    }
  } else if (bl_node_is(lhs_type, BL_NODE_TYPE_ENUM)) {
    /* enum variant */
    _member->kind                  = BL_MEM_KIND_ENUM;
    bl_node_type_enum_t *_lhs_type = bl_peek_type_enum(lhs_type);
    assert(_lhs_type->base_decl);

    found = _lookup(bl_peek_decl_value(_lhs_type->base_decl)->value, _member->ident, NULL, false);
    if (!found) {
      check_error_node(cnt, BL_ERR_UNKNOWN_SYMBOL, _member->ident, BL_BUILDER_CUR_WORD,
                       "unknown enum variant");
      FINISH;
    }

    if (_member->ptr_ref) {
      check_error_node(cnt, BL_ERR_INVALID_MEMBER_ACCESS, member, BL_BUILDER_CUR_WORD,
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
check_stmt_if(context_t *cnt, bl_node_t *stmt_if)
{
  bl_node_stmt_if_t *_if = bl_peek_stmt_if(stmt_if);
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
  bl_node_decl_value_t *_decl = bl_peek_decl_value(decl);
  if (!_decl->value) return false;
  bl_node_t *inferred_type = bl_ast_get_type(_decl->value);
  assert(inferred_type);

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
check_decl_value(context_t *cnt, bl_node_t *decl)
{
  bl_node_decl_value_t *_decl          = bl_peek_decl_value(decl);
  bool                  lookup_in_tree = true;

  assert(_decl->name);
  infer_type(cnt, decl);

  if (_decl->flags & BL_FLAG_MAIN && _decl->kind != BL_DECL_KIND_FN) {
    check_error_node(cnt, BL_ERR_INVALID_TYPE, decl, BL_BUILDER_CUR_WORD,
                     "main is expected to be function");
  }

  switch (_decl->kind) {
  case BL_DECL_KIND_STRUCT: {
    bl_node_t *value_type                      = bl_ast_get_type(_decl->value);
    bl_peek_type_struct(value_type)->base_decl = decl;

    if (_decl->mutable) {
      check_error_node(cnt, BL_ERR_INVALID_MUTABILITY, decl, BL_BUILDER_CUR_WORD,
                       "structure declaration cannot be mutable");
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
      check_error_node(cnt, BL_ERR_INVALID_ARG_TYPE, decl, BL_BUILDER_CUR_WORD,
                       "function arguments cannot have value binding");
    }
    break;
  }

  case BL_DECL_KIND_VARIANT: {
    if (_decl->mutable) {
      check_error_node(cnt, BL_ERR_INVALID_MUTABILITY, decl, BL_BUILDER_CUR_WORD,
                       "an enum variant cannot be mutable");
    }
    break;
  }

  case BL_DECL_KIND_ENUM: {
    bl_node_t *value_type                    = bl_ast_get_type(_decl->value);
    bl_peek_type_enum(value_type)->base_decl = decl;
    break;
  }

  case BL_DECL_KIND_FN:
    break;
  case BL_DECL_KIND_TYPE:
    bl_abort("unimplemented");
  case BL_DECL_KIND_UNKNOWN:
    bl_abort("unknown declaration kind");
  }

  assert(_decl->type);

  if (bl_ast_get_type_kind(bl_ast_get_type(_decl->type)) == BL_KIND_VOID) {
    char tmp[256];
    bl_ast_type_to_string(tmp, 256, bl_ast_get_type(_decl->type));
    check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->name, BL_BUILDER_CUR_WORD,
                     "declaration has invalid type '%s'", tmp);
  }

  _decl->type = bl_ast_get_type(_decl->type);

  /* provide symbol into scope if there is no conflict */
  bl_node_t *conflict = lookup(_decl->name, NULL, lookup_in_tree);
  if (conflict) {
    check_error_node(cnt, BL_ERR_DUPLICATE_SYMBOL, decl, BL_BUILDER_CUR_WORD,
                     "symbol with same name already declared here: %s:%d",
                     conflict->src->unit->filepath, conflict->src->line);
  } else {
    provide(_decl->name, decl);

    _decl->in_gscope =
        bl_ast_get_scope(bl_peek_ident(_decl->name)->parent_compound) == cnt->assembly->gscope;

    /* insert into ir queue */
    if (_decl->in_gscope && (_decl->kind != BL_DECL_KIND_CONSTANT)) {
      // bl_log("generate %s", bl_peek_ident(_decl->name)->str);
      bl_assembly_add_into_ir(cnt->assembly, decl);
    }

    waiting_resume(cnt, bl_peek_ident(_decl->name)->hash);
  }

  FINISH;
}

void
_check_unused(context_t *cnt, bl_node_t *node)
{
  assert(node);
  switch (bl_node_code(node)) {
  case BL_NODE_DECL_VALUE: {
    bl_node_decl_value_t *_decl = bl_peek_decl_value(node);
    if (!_decl->in_gscope && !_decl->used && !(_decl->flags & BL_FLAG_EXTERN) &&
        !(_decl->flags & BL_FLAG_MAIN)) {
      check_warning_node(cnt, _decl->name, BL_BUILDER_CUR_WORD,
                         "symbol is declared but never used");
    }
    break;
  }
  default:
    break;
  }
}

void
check_unused(context_t *cnt)
{
  /* for unused declaration check we need to do additional pass after general checking pass,
   * unordered linear iteration is used on AST */
  bl_ast_visit_every_node(cnt->ast, (bl_visit_f)_check_unused, cnt);
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
      .waiting_resumed    = bo_array_new(sizeof(uint64_t)),
      .provided_in_gscope = bl_scope_new(assembly->scope_cache, 4092),
  };

  bl_unit_t *unit;
  bl_barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    cnt.ast  = &unit->ast;
    check_flatten(&cnt, unit->ast.root);
  }

  waiting_resume_all(&cnt);
  check_unresolved(&cnt);
  check_unused(&cnt);
  bo_unref(cnt.waiting);
  bo_unref(cnt.waiting_resumed);
#if BL_DEBUG
  if (_flatten != 0) bl_log(BL_RED("leaking flatten cache: %d"), _flatten);
#endif
}
