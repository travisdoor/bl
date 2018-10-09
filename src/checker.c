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

#include "stages.h"
#include "common.h"
#include "ast.h"
#include "eval.h"

#define VERBOSE 0
#define VERBOSE_MULTIPLE_CHECK 0

#define finish() return NULL
#define wait(_n) return (_n)

#define FN_ARR_COUNT_NAME "count@"

#define check_error_node(cnt, code, node, pos, format, ...)                                        \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_ERROR, (code), (node)->src, (pos), (format),           \
                ##__VA_ARGS__);                                                                    \
  }

#define check_note_node(cnt, node, pos, format, ...)                                               \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_NOTE, 0, (node)->src, (pos), (format), ##__VA_ARGS__); \
  }

typedef struct
{
  Eval        evaluator;
  Builder *   builder;
  Assembly *  assembly;
  Unit *      unit;
  Ast *       ast;
  BHashTable *waiting;
  Scope *     provided_in_gscope;
  BArray *    flatten_cache;
} Context;

typedef struct
{
  BArray *flatten;
  Node *  waitfor;
  size_t  i;
} FIter;

static inline Node *
lookup(Node *ident, Scope **out_scope, bool walk_tree);

static Node *
_lookup(Node *compound, Node *ident, Scope **out_scope, bool walk_tree);

static void
to_Any(Context *cnt, Node **node, Node *any_type);

static bool
infer_type(Context *cnt, Node *decl);

static void
provide(Node *ident, Node *provided);

static inline void
waiting_push(BHashTable *waiting, Node *node, FIter fiter, Node *waitfor);

static void
waiting_resume(Context *cnt, Node *ident);

static BArray *
flatten_get(BArray *cache);

static void
flatten_put(BArray *cache, BArray *flatten);

static void
flatten_free_cache(BArray *cache);

static inline void
flatten_push(BArray *flatten, Node **node);

static void
flatten_node(Context *cnt, BArray *fbuf, Node **node);

static bool
implicit_cast(Context *cnt, Node **node, Node *to_type);

static inline void
check_error_invalid_types(Context *cnt, Node *first_type, Node *second_type, Node *err_pos);

static void
check_flatten(Context *cnt, Node **node);

static void
process_flatten(Context *cnt, FIter *fit);

/* perform checking on node of any type, return NULL when node was sucessfully checked or ponter to
 * waiting-for node */
static Node *
check_node(Context *cnt, Node **node);

static Node *
check_ident(Context *cnt, Node **ident);

static Node *
check_decl(Context *cnt, Node **decl);

static Node *
check_lit_cmp(Context *cnt, Node **cmp);

static Node *
check_expr_call(Context *cnt, Node **call);

static Node *
check_expr_unary(Context *cnt, Node **unary);

static Node *
check_expr_binop(Context *cnt, Node **binop);

static Node *
check_expr_cast(Context *cnt, Node **cast);

static Node *
check_expr_sizeof(Context *cnt, Node **szof);

static Node *
check_expr_typeof(Context *cnt, Node **tpof);

static Node *
check_expr_member(Context *cnt, Node **member);

static Node *
check_expr_elem(Context *cnt, Node **elem);

static Node *
check_stmt_if(Context *cnt, Node **stmt_if);

static Node *
check_stmt_return(Context *cnt, Node **ret);

static Node *
check_type_enum(Context *cnt, Node **type);

static void
check_unresolved(Context *cnt);

// impl
Node *
lookup(Node *ident, Scope **out_scope, bool walk_tree)
{
  assert(ident && "invalid identificator in lookup");
  Node *compound = peek_ident(ident)->parent_compound;
  assert(compound && "ident compound not set");
  return _lookup(compound, ident, out_scope, walk_tree);
}

Node *
_lookup(Node *compound, Node *ident, Scope **out_scope, bool walk_tree)
{
  Node * found = NULL;
  Scope *scope = NULL;

  while (compound && !found) {
    scope = ast_get_scope(compound);
    if (scope) {
      found = scope_get(scope, ident);
    }

    if (!walk_tree) break;
    compound = ast_get_parent_compound(compound);
  }

  if (out_scope) *out_scope = scope;
  return found;
}

void
to_Any(Context *cnt, Node **node, Node *any_type)
{
  /* INITIALIZER BOILERPLATE for Any type */
  /* Here we convert anything to 'Any { .value = cast(*u8) (&foo), .type = typeof(foo) }' */
  Node *from_type = ast_get_type(*node);

  Node *tmp           = *node;
  Node *val_base_type = ast_node_dup(cnt->ast, from_type);
  ast_type_set_ptr(val_base_type, 1);

  Node *u8_ptr = ast_type_fund(cnt->ast, NULL, FTYPE_U8, 1, NULL);
  Node *value  = ast_expr_unary(cnt->ast, NULL, SYM_AND, tmp, val_base_type);
  value        = ast_expr_cast(cnt->ast, NULL, u8_ptr, value);

  TokenValue type;
  type.u      = (unsigned long long)ast_type_kind(from_type);
  value->next = ast_lit(cnt->ast, NULL, &ftypes[FTYPE_S32], type);
  *node       = ast_lit_cmp(cnt->ast, NULL, any_type, value, 2, NULL);
}

void
provide(Node *ident, Node *provided)
{
  assert(ident && provided && "trying to provide invalid symbol");
  Node *compound = peek_ident(ident)->parent_compound;
  assert(compound);
  Scope *scope = ast_get_scope(compound);
  assert(scope);

#if VERBOSE
  bl_log("providing " MAGENTA("'%s'") " (%d)", peek_ident(ident)->str, ident->_serial);
#endif
  scope_insert(scope, ident, provided);
}

void
waiting_push(BHashTable *waiting, Node *node, FIter fiter, Node *waitfor)
{
  NodeIdent *_waitfor = peek_ident(waitfor);
  BArray *   queue;
  if (bo_htbl_has_key(waiting, _waitfor->hash)) {
    queue = bo_htbl_at(waiting, _waitfor->hash, BArray *);
  } else {
    queue = bo_array_new(sizeof(FIter));
    bo_htbl_insert(waiting, _waitfor->hash, queue);
  }
  assert(queue);
  fiter.waitfor = waitfor;
  bo_array_push_back(queue, fiter);
}

void
waiting_resume(Context *cnt, Node *ident)
{
  NodeIdent *_ident = peek_ident(ident);
  /* is there some flattens waiting for this symbol??? */
  if (!bo_htbl_has_key(cnt->waiting, _ident->hash)) return;

  /* resume all waiting flattens */
  BArray *q = bo_htbl_at(cnt->waiting, _ident->hash, BArray *);
  assert(q && "invalid flattens queue");

  /* NOTE: we need to iterate backwards from last element in 'q' because it can be modified in
   * 'process_flatten' method */
  FIter     fit;
  const int c = (int)bo_array_size(q);
  for (int i = c - 1; i >= 0; --i) {
    fit = bo_array_at(q, i, FIter);
    bo_array_erase(q, i);
    process_flatten(cnt, &fit);
  }

  if (bo_array_empty(q)) bo_htbl_erase_key(cnt->waiting, _ident->hash);
}

void
check_unresolved(Context *cnt)
{
  bo_iterator_t iter;
  BArray *      q;
  FIter         tmp;

  bhtbl_foreach(cnt->waiting, iter)
  {
    q = bo_htbl_iter_peek_value(cnt->waiting, &iter, BArray *);
    assert(q);

    // bl_log("size %d", bo_array_size(q));
    for (size_t i = 0; i < bo_array_size(q); ++i) {
      tmp = bo_array_at(q, i, FIter);
      assert(tmp.waitfor);
      if (!scope_has_symbol(cnt->provided_in_gscope, tmp.waitfor))
        check_error_node(cnt, ERR_UNKNOWN_SYMBOL, tmp.waitfor, BUILDER_CUR_WORD, "unknown symbol");
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
    tmp = bo_array_new(sizeof(Node *));
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
flatten_push(BArray *flatten, Node **node)
{
  bo_array_push_back(flatten, node);
}

void
flatten_node(Context *cnt, BArray *fbuf, Node **node)
{
  if (!*node) return;

#define flatten(_node) flatten_node(cnt, fbuf, (_node))

  switch (node_code(*node)) {
  case NODE_DECL: {
    NodeDecl *_decl = peek_decl(*node);
    /* store declaration for temporary use here, this scope is used only for searching truly
     * undefined symbols later */
    if (_decl->in_gscope && !scope_has_symbol(cnt->provided_in_gscope, _decl->name))
      scope_insert(cnt->provided_in_gscope, _decl->name, *node);

    _decl->name->state = CHECKED;

    flatten(&_decl->type);
    flatten(&_decl->value);
    break;
  }

  case NODE_LIT_FN: {
    NodeLitFn *_fn = peek_lit_fn(*node);
    flatten(&_fn->type);
    check_flatten(cnt, &_fn->block);
    break;
  }

  case NODE_LIT_STRUCT: {
    NodeLitStruct *_struct = peek_lit_struct(*node);
    // flatten(&_struct->type);
    check_flatten(cnt, &_struct->type);
    break;
  }

  case NODE_LIT_CMP: {
    NodeLitCmp *_cmp = peek_lit_cmp(*node);
    flatten(&_cmp->type);
    Node **field;
    node_foreach_ref(_cmp->fields, field)
    {
      flatten(field);
    }
    break;
  }

  case NODE_LIT_ENUM: {
    NodeLitEnum *_enum = peek_lit_enum(*node);
    flatten(&_enum->type);
    Node **variant;
    node_foreach_ref(_enum->variants, variant)
    {
      flatten(variant);
    }
    break;
  }

  case NODE_BLOCK: {
    NodeBlock *_block = peek_block(*node);

    Node **tmp;
    node_foreach_ref(_block->nodes, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case NODE_UBLOCK: {
    NodeUBlock *_ublock = peek_ublock(*node);

    Node **tmp;
    node_foreach_ref(_ublock->nodes, tmp)
    {
      check_flatten(cnt, tmp);
    }
    break;
  }

  case NODE_STMT_RETURN: {
    NodeStmtReturn *_return = peek_stmt_return(*node);
    flatten(&_return->expr);
    break;
  }

  case NODE_STMT_IF: {
    NodeStmtIf *_if = peek_stmt_if(*node);
    flatten(&_if->test);
    flatten(&_if->true_stmt);
    flatten(&_if->false_stmt);
    break;
  }

  case NODE_STMT_LOOP: {
    NodeStmtLoop *_loop = peek_stmt_loop(*node);
    flatten(&_loop->init);
    flatten(&_loop->condition);
    flatten(&_loop->increment);
    flatten(&_loop->block);
    break;
  }

  case NODE_EXPR_MEMBER: {
    NodeExprMember *_member = peek_expr_member(*node);
    flatten(&_member->next);
    break;
  }

  case NODE_EXPR_ELEM: {
    NodeExprElem *_elem = peek_expr_elem(*node);
    flatten(&_elem->next);
    flatten(&_elem->index);
    break;
  }

  case NODE_EXPR_CAST: {
    NodeExprCast *_cast = peek_expr_cast(*node);
    flatten(&_cast->type);
    flatten(&_cast->next);
    break;
  }

  case NODE_EXPR_SIZEOF: {
    NodeExprSizeof *_sizeof = peek_expr_sizeof(*node);
    flatten(&_sizeof->in);
    flatten(&_sizeof->type);
    break;
  }

  case NODE_EXPR_TYPEOF: {
    NodeExprTypeof *_typeof = peek_expr_typeof(*node);
    flatten(&_typeof->in);
    flatten(&_typeof->type);
    break;
  }

  case NODE_EXPR_CALL: {
    NodeExprCall *_call = peek_expr_call(*node);
    flatten(&_call->ref);

    Node **tmp;
    node_foreach_ref(_call->args, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case NODE_EXPR_BINOP: {
    NodeExprBinop *_binop = peek_expr_binop(*node);
    flatten(&_binop->lhs);
    flatten(&_binop->rhs);
    break;
  }

  case NODE_EXPR_UNARY: {
    NodeExprUnary *_unary = peek_expr_unary(*node);
    flatten(&_unary->next);
    break;
  }

  case NODE_TYPE_STRUCT: {
    NodeTypeStruct *_struct_type = peek_type_struct(*node);
    Node **         tmp;
    node_foreach_ref(_struct_type->types, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case NODE_TYPE_ENUM: {
    NodeTypeEnum *_enum_type = peek_type_enum(*node);
    flatten(&_enum_type->base_type);
    break;
  }

  case NODE_TYPE_FN: {
    NodeTypeFn *_type_fn = peek_type_fn(*node);
    flatten(&_type_fn->ret_type);
    Node **sub_type;
    node_foreach_ref(_type_fn->arg_types, sub_type)
    {
      flatten(sub_type);
    }
    break;
  }

  case NODE_IDENT: {
    NodeIdent *_ident = peek_ident(*node);
    flatten(&_ident->arr);
    break;
  }

  case NODE_TYPE_FUND:
  case NODE_TYPE_VARGS:
  case NODE_EXPR_NULL:
  case NODE_STMT_BREAK:
  case NODE_STMT_CONTINUE:
  case NODE_LIT:
  case NODE_LOAD:
  case NODE_LINK:
  case NODE_BAD:
  case NODE_COUNT:
    break;
  }

  flatten_push(fbuf, node);
#undef flatten
}

void
check_flatten(Context *cnt, Node **node)
{
  FIter fit;
  fit.flatten = flatten_get(cnt->flatten_cache);
  fit.i       = 0;
  fit.waitfor = NULL;

  flatten_node(cnt, fit.flatten, node);
  process_flatten(cnt, &fit);
}

void
process_flatten(Context *cnt, FIter *fit)
{
  assert(fit);
  assert(fit->flatten && "invalid flatten");
  bool interrupted = false;

  Node * waitfor;
  Node **tmp;
  for (; fit->i < bo_array_size(fit->flatten); ++fit->i) {
    tmp = bo_array_at(fit->flatten, fit->i, Node **);
    assert(*tmp);
    /* NULL means successfull check */
    waitfor = check_node(cnt, tmp);
    if (waitfor) {
      waiting_push(cnt->waiting, *tmp, *fit, waitfor);
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
implicit_cast(Context *cnt, Node **node, Node *to_type)
{
  assert(to_type);
  assert(*node);
  to_type         = ast_get_type(to_type);
  Node *from_type = ast_get_type(*node);

  TypeKind from_kind = ast_type_kind(from_type);
  TypeKind to_kind   = ast_type_kind(to_type);
  if (node_is(*node, NODE_LIT) && from_kind != TYPE_KIND_STRING && from_kind != TYPE_KIND_CHAR &&
      from_kind != TYPE_KIND_REAL &&
      (to_kind == TYPE_KIND_SIZE || to_kind == TYPE_KIND_UINT || to_kind == TYPE_KIND_SINT)) {
    peek_lit(*node)->type = ast_node_dup(cnt->ast, to_type);
    return true;
  }

  if (!ast_can_impl_cast(from_type, to_type)) return false;

  if (to_kind == TYPE_KIND_ANY) {
    to_Any(cnt, node, to_type);
    return true;
  }

  Node *tmp_next = (*node)->next;
  Node *type_dup = ast_node_dup(cnt->ast, to_type);
  Node *cast     = ast_expr_cast(cnt->ast, NULL, type_dup, *node);
  cast->next     = tmp_next;
  *node          = cast;

  return true;
}

void
check_error_invalid_types(Context *cnt, Node *first_type, Node *second_type, Node *err_pos)
{
  char tmp_first[256];
  char tmp_second[256];
  ast_type_to_string(tmp_first, 256, first_type);
  ast_type_to_string(tmp_second, 256, second_type);
  check_error_node(cnt, ERR_INVALID_TYPE, err_pos, BUILDER_CUR_WORD,
                   "no implicit cast for types '%s' and '%s'", tmp_first, tmp_second);
}

Node *
check_expr_call(Context *cnt, Node **call)
{
  NodeExprCall *_call = peek_expr_call(*call);

  Node *ident = node_is(_call->ref, NODE_IDENT) ? _call->ref : peek_expr_member(_call->ref)->ident;

  Node *callee = peek_ident(ident)->ref;
  assert(callee);
  NodeDecl *_callee     = peek_decl(callee);
  Node *    callee_type = _callee->type;

  if (node_is_not(callee_type, NODE_TYPE_FN)) {
    check_error_node(cnt, ERR_INVALID_TYPE, *call, BUILDER_CUR_WORD, "expected function name");
    finish();
  }

  NodeTypeFn *_callee_type = peek_type_fn(callee_type);

  _call->type = _callee_type->ret_type;

  if (_call->argsc != _callee_type->argc_types) {
    check_error_node(cnt, ERR_INVALID_ARG_COUNT, *call, BUILDER_CUR_WORD,
                     "expected %d %s, but called with %d", _callee_type->argc_types,
                     _callee_type->argc_types == 1 ? "argument" : "arguments", _call->argsc);
    finish();
  }

  Node **call_arg   = &_call->args;
  Node * callee_arg = _callee_type->arg_types;

  while (*call_arg) {
    if (node_is(*call_arg, NODE_EXPR_NULL)) {
      peek_expr_null(*call_arg)->type = ast_get_type(callee_arg);
    } else if (!ast_type_cmp(*call_arg, callee_arg) && !implicit_cast(cnt, call_arg, callee_arg)) {
      char tmp1[256];
      char tmp2[256];
      ast_type_to_string(tmp1, 256, ast_get_type(*call_arg));
      ast_type_to_string(tmp2, 256, ast_get_type(callee_arg));

      check_error_node(cnt, ERR_INVALID_ARG_TYPE, *call_arg, BUILDER_CUR_WORD,
                       "invalid call argument type, expected is '%s' but called with '%s'", tmp2,
                       tmp1);

      break;
    }

    call_arg   = &(*call_arg)->next;
    callee_arg = callee_arg->next;
  }

  if (_call->run) {
    TypeKind callee_ret_tkind = ast_type_kind(ast_unroll_ident(_callee_type->ret_type));
    switch (callee_ret_tkind) {
    case TYPE_KIND_FN:
    case TYPE_KIND_PTR:
    case TYPE_KIND_STRING:
    case TYPE_KIND_STRUCT:
      check_error_node(cnt, ERR_INVALID_TYPE, *call, BUILDER_CUR_WORD,
                       "method called in compile time can return fundamental types only");
    default:
      break;
    }

    if (_call->argsc) {
      check_error_node(cnt, ERR_INVALID_ARG_COUNT, *call, BUILDER_CUR_WORD,
                       "method called in compile time cannot take arguments, remove '#run'?");
    }
  }

  finish();
}

Node *
check_expr_unary(Context *cnt, Node **unary)
{
  NodeExprUnary *_unary = peek_expr_unary(*unary);
  assert(_unary->next);

  if (_unary->op == SYM_AND) {
    _unary->type = ast_node_dup(cnt->ast, ast_get_type(_unary->next));
    int ptr      = ast_type_get_ptr(_unary->type) + 1;
    ast_type_set_ptr(_unary->type, ptr);
  } else if (_unary->op == SYM_ASTERISK) {
    _unary->type = ast_node_dup(cnt->ast, ast_get_type(_unary->next));
    int ptr      = ast_type_get_ptr(_unary->type) - 1;

    if (ptr < 0) {
      check_error_node(cnt, ERR_INVALID_TYPE, _unary->next, BUILDER_CUR_WORD,
                       "cannot dereference non-pointer type");
    } else {
      ast_type_set_ptr(_unary->type, ptr);
    }
  } else {
    _unary->type = ast_get_type(_unary->next);
  }

  finish();
}

Node *
check_node(Context *cnt, Node **node)
{
  assert(node);
  Node *result = NULL;
#if defined(BL_DEBUG) && BL_VERBOSE_MUTIPLE_CHECK
  if (node->state == BL_CHECKED)
    bl_msg_warning("unnecessary node check %s (%d)", node_name(node), node->_serial);
#endif

  switch (node_code(*node)) {
  case NODE_IDENT:
    result = check_ident(cnt, node);
    break;
  case NODE_DECL:
    result = check_decl(cnt, node);
    break;
  case NODE_EXPR_CALL:
    result = check_expr_call(cnt, node);
    break;
  case NODE_EXPR_BINOP:
    result = check_expr_binop(cnt, node);
    break;
  case NODE_EXPR_CAST:
    result = check_expr_cast(cnt, node);
    break;
  case NODE_EXPR_UNARY:
    result = check_expr_unary(cnt, node);
    break;
  case NODE_EXPR_SIZEOF:
    result = check_expr_sizeof(cnt, node);
    break;
  case NODE_EXPR_TYPEOF:
    result = check_expr_typeof(cnt, node);
    break;
  case NODE_EXPR_MEMBER:
    result = check_expr_member(cnt, node);
    break;
  case NODE_EXPR_ELEM:
    result = check_expr_elem(cnt, node);
    break;
  case NODE_STMT_IF:
    result = check_stmt_if(cnt, node);
    break;
  case NODE_STMT_RETURN:
    result = check_stmt_return(cnt, node);
    break;
  case NODE_TYPE_ENUM:
    result = check_type_enum(cnt, node);
    break;
  case NODE_LIT_CMP:
    result = check_lit_cmp(cnt, node);
    break;
  case NODE_TYPE_FUND:
  case NODE_TYPE_FN:
  case NODE_TYPE_STRUCT:
  case NODE_TYPE_VARGS:
  case NODE_EXPR_NULL:
  case NODE_LIT:
  case NODE_STMT_BREAK:
  case NODE_STMT_CONTINUE:
  case NODE_STMT_LOOP:
  case NODE_LOAD:
  case NODE_LINK:
  case NODE_BAD:
  case NODE_UBLOCK:
  case NODE_BLOCK:
  case NODE_LIT_STRUCT:
  case NODE_LIT_ENUM:
  case NODE_LIT_FN:
  case NODE_COUNT:
    break;
  }

#if VERBOSE
  {
    const char *file = (*node)->src ? (*node)->src->unit->name : "implicit";
    const int   line = (*node)->src ? (*node)->src->line : 0;
    const int   col  = (*node)->src ? (*node)->src->col : 0;
    if (result == NULL) {
      bl_log("checked [" GREEN(" OK ") "] (%s) " CYAN("%s:%d:%d"), node_name(*node), file, line,
             col);
    } else {
      bl_log("checked [" RED("WAIT") "] (%s) " CYAN("%s:%d:%d") " -> " RED("%s"), node_name(*node),
             file, line, col, peek_ident(result)->str);
    }
  }
#endif

  (*node)->state = result ? WAITING : CHECKED;
  return result;
}

Node *
check_ident(Context *cnt, Node **ident)
{
  NodeIdent *_ident = peek_ident(*ident);
  if (_ident->ref) {
    finish();
  }

  Node *    found   = NULL;
  const int buildin = ast_is_buildin_type(*ident);
  if (buildin != -1) {
    /* connect buildin fundamental types references */
    found = &ftypes[buildin];
  } else {
    found = lookup(*ident, NULL, true);
    if (!found) wait(*ident);
  }

  if (node_is(found, NODE_DECL)) {
    peek_decl(found)->used++;
  }

  _ident->ref = ast_unroll_ident(found);
  // assert(node_is(_ident->ref, NODE_DECL));

  if (_ident->ptr || _ident->arr) {
    /* when ident reference is pointer we need to create copy of declaration with different
     * type, maybe there is some better solution ??? */
    if (_ident->arr) {
      /* check valid type of array size */

      if (!implicit_cast(cnt, &_ident->arr, &ftypes[FTYPE_SIZE])) {
        check_error_invalid_types(cnt, ast_get_type(_ident->arr), &ftypes[FTYPE_SIZE], _ident->arr);
      }

      if (node_is_not(_ident->arr, NODE_LIT)) {
        TokenValue value;
        Node *     eval_err_node;
        value.u = (unsigned long long int)eval_expr(&cnt->evaluator, _ident->arr, &eval_err_node);

        if (eval_err_node) {
          /* unable to evaluate value */
          if (node_is(eval_err_node, NODE_EXPR_CALL) && peek_expr_call(eval_err_node)->run) {
            if (eval_err_node != _ident->arr) {
              /* un-evaluated compile-time call is not a root of the arr expression so we need to
               * put whole expression into implicit function and replace arr with call to this
               * functon */

              /* generate unique name */
              const char *uname = builder_get_unique_name(cnt->builder, FN_ARR_COUNT_NAME);

              /* FUNCTION BOILERPLATE */
              Node *gscope   = cnt->unit->ast.root;
              Node *fn_name  = ast_ident(cnt->ast, NULL, uname, NULL, gscope, 0, NULL);
              Node *fn_type  = ast_type_fn(cnt->ast, NULL, NULL, 0, &ftypes[FTYPE_SIZE], 0);
              Node *fn_block = ast_block(cnt->ast, NULL, NULL, NULL, NULL);
              Node *fn_value = ast_lit_fn(cnt->ast, NULL, fn_type, fn_block, gscope, NULL);
              Node *fn_decl  = ast_decl(cnt->ast, NULL, DECL_KIND_FN, fn_name, fn_type, fn_value,
                                       false, 0, 0, true);

              /* fill body */
              Node *ret                   = ast_stmt_return(cnt->ast, NULL, _ident->arr, fn_decl);
              peek_block(fn_block)->nodes = ret;

              /* insert node into top of the global scope */
              NodeUBlock *_gscope = peek_ublock(gscope);
              fn_decl->next       = _gscope->nodes;
              _gscope->nodes      = fn_decl;

              /* genetate call to created function */
              _ident->arr =
                  ast_expr_call(cnt->ast, NULL, fn_decl, NULL, 0, &ftypes[FTYPE_SIZE], true);
              peek_decl(fn_decl)->used = 1;
            }
          } else {
            check_error_node(cnt, ERR_EXPECTED_CONST, eval_err_node, BUILDER_CUR_WORD,
                             "expected constant known in compile time");
            if (node_is(eval_err_node, NODE_EXPR_CALL)) {
              check_note_node(cnt, eval_err_node, BUILDER_CUR_BEFORE,
                              "#run directive can be used here");
            }
          }
        } else {
          /* evalueted value */
          _ident->arr = ast_lit(cnt->ast, NULL, &ftypes[FTYPE_SIZE], value);
        }
      }
    }

    _ident->ref = ast_node_dup(cnt->ast, found);
    Node *type  = ast_get_type(_ident->ref);
    assert(type);
    type = ast_node_dup(cnt->ast, type);
    ast_type_set_ptr(type, _ident->ptr);
    ast_type_set_arr(type, _ident->arr);

    if (ast_is_type(_ident->ref)) {
      _ident->ref = type;
    } else {
      ast_set_type(_ident->ref, type);
    }
  }

  finish();
}

Node *
check_stmt_return(Context *cnt, Node **ret)
{
  NodeStmtReturn *_ret = peek_stmt_return(*ret);
  assert(_ret->fn_decl);

  NodeDecl * _callee_decl = peek_decl(_ret->fn_decl);
  NodeLitFn *_callee      = peek_lit_fn(_callee_decl->value);
  Node *     fn_ret_type  = ast_get_type(peek_type_fn(_callee->type)->ret_type);
  if (fn_ret_type == NULL) wait(peek_decl(_ret->fn_decl)->name);

  if (_ret->expr) {
    if (node_is(_ret->expr, NODE_EXPR_NULL)) {
      NodeExprNull *_null = peek_expr_null(_ret->expr);
      _null->type         = fn_ret_type;
      if (ast_type_kind(_null->type) != TYPE_KIND_PTR) {
        check_error_node(
            cnt, ERR_INVALID_TYPE, _ret->expr, BUILDER_CUR_WORD,
            "'null' cannot be used because the function does not return a pointer value");
      }
    } else {
      Node *expr_type = ast_get_type(_ret->expr);
      if (!ast_type_cmp(expr_type, fn_ret_type) && !implicit_cast(cnt, &_ret->expr, fn_ret_type)) {
        check_error_invalid_types(cnt, expr_type, fn_ret_type, _ret->expr);
      }
    }
  } else {
    if (!ast_type_cmp(&ftypes[FTYPE_VOID], fn_ret_type)) {
      check_error_node(cnt, ERR_EXPECTED_EXPR, *ret, BUILDER_CUR_AFTER, "expected return value");
    }
  }
  finish();
}

Node *
check_expr_binop(Context *cnt, Node **binop)
{
  NodeExprBinop *_binop = peek_expr_binop(*binop);

  assert(_binop->lhs);
  assert(_binop->rhs);

  Sym op = _binop->op;
  if (op == SYM_ASSIGN || op == SYM_PLUS_ASSIGN || op == SYM_MINUS_ASSIGN || op == SYM_MUL_ASSIGN ||
      op == SYM_DIV_ASSIGN || op == SYM_MOD_ASSIGN) {
    if (node_is_not(_binop->lhs, NODE_IDENT) && node_is_not(_binop->lhs, NODE_EXPR_UNARY) &&
        node_is_not(_binop->lhs, NODE_EXPR_ELEM) && node_is_not(_binop->lhs, NODE_EXPR_MEMBER)) {
      // TODO: temporary solution, what about (some_pointer + 1) = ...
      check_error_node(cnt, ERR_INVALID_TYPE, _binop->lhs, BUILDER_CUR_WORD,
                       "left-hand side of assignment does not refer to any declaration and "
                       "cannot be assigned");
      finish();
    }

    if (node_is_not(_binop->lhs, NODE_EXPR_UNARY) && node_is_not(_binop->lhs, NODE_EXPR_ELEM) &&
        node_is_not(_binop->lhs, NODE_EXPR_MEMBER)) {
      NodeIdent *_lhs = peek_ident(_binop->lhs);
      assert(_lhs->ref);
      assert(node_is(_lhs->ref, NODE_DECL));

      if (!peek_decl(_lhs->ref)->mutable) {
        check_error_node(cnt, ERR_INVALID_MUTABILITY, *binop, BUILDER_CUR_WORD,
                         "left-hand side declaration is not mutable and cannot be assigned");
        check_note_node(cnt, _lhs->ref, BUILDER_CUR_WORD, "declared here");
      }
    }
  }

  Node *lhs_type = ast_get_type(_binop->lhs);
  assert(lhs_type);
  TypeKind lhs_kind = ast_type_kind(lhs_type);

  if (node_is(_binop->rhs, NODE_EXPR_NULL)) {
    if (lhs_kind != TYPE_KIND_PTR) {
      check_error_node(cnt, ERR_INVALID_TYPE, _binop->lhs, BUILDER_CUR_WORD,
                       "expected a pointer type");
    } else {
      peek_expr_null(_binop->rhs)->type = lhs_type;
      if (!_binop->type) _binop->type = lhs_type;
    }

    finish();
  }

  Node *rhs_type = ast_get_type(_binop->rhs);
  assert(rhs_type);
  TypeKind rhs_kind = ast_type_kind(rhs_type);

  if (!ast_type_cmp(lhs_type, rhs_type)) {
    if (node_is(_binop->lhs, NODE_LIT) &&
        (lhs_kind == TYPE_KIND_SIZE || lhs_kind == TYPE_KIND_UINT || lhs_kind == TYPE_KIND_SINT) &&
        (rhs_kind == TYPE_KIND_SIZE || rhs_kind == TYPE_KIND_UINT || rhs_kind == TYPE_KIND_SINT)) {
      peek_lit(_binop->lhs)->type = ast_node_dup(cnt->ast, rhs_type);
    } else if (!implicit_cast(cnt, &_binop->rhs, lhs_type)) {
      check_error_invalid_types(cnt, lhs_type, rhs_type, *binop);
    }
  }

  if (!_binop->type) _binop->type = ast_get_type(_binop->lhs);

  finish();
}

Node *
check_expr_cast(Context *cnt, Node **cast)
{
  NodeExprCast *_cast = peek_expr_cast(*cast);
  assert(_cast->type);
  _cast->type = ast_get_type(_cast->type);

  if (ast_type_cmp(_cast->type, ast_get_type(_cast->next))) {
    /* unnecessary cast -> so remove them */
    *cast = _cast->next;
  }

  TypeKind src_kind  = ast_type_kind(ast_get_type(_cast->next));
  TypeKind dest_kind = ast_type_kind(_cast->type);

  bool valid = src_kind != TYPE_KIND_FN && src_kind != TYPE_KIND_STRUCT &&
                     src_kind != TYPE_KIND_VOID && dest_kind != TYPE_KIND_FN &&
                     dest_kind != TYPE_KIND_STRUCT && dest_kind != TYPE_KIND_VOID;

  /* invalid cast ptr -> real and vice versa */
  valid = valid && !(src_kind == TYPE_KIND_REAL && dest_kind == TYPE_KIND_PTR) &&
    !(src_kind == TYPE_KIND_PTR && dest_kind == TYPE_KIND_REAL);

  valid = valid && !(src_kind == TYPE_KIND_REAL && dest_kind == TYPE_KIND_STRING) &&
    !(src_kind == TYPE_KIND_STRING && dest_kind == TYPE_KIND_REAL);

  if (!valid) {
    char tmp_first[256];
    char tmp_second[256];
    ast_type_to_string(tmp_first, 256, ast_get_type(_cast->next));
    ast_type_to_string(tmp_second, 256, _cast->type);
    check_error_node(cnt, ERR_INVALID_CAST, *cast, BUILDER_CUR_WORD,
                     "cannot build cast from '%s' to '%s'", tmp_first, tmp_second);
  }

  finish();
}

Node *
check_expr_sizeof(Context *cnt, Node **szof)
{
  NodeExprSizeof *_sizeof = peek_expr_sizeof(*szof);
  _sizeof->in             = ast_get_type(_sizeof->in);
  finish();
}

Node *
check_expr_typeof(Context *cnt, Node **tpof)
{
  NodeExprTypeof *_typeof = peek_expr_typeof(*tpof);
  assert(_typeof->in);

  Node *   type = ast_get_type(_typeof->in);
  TypeKind kind = ast_type_kind(type);
  assert(kind != TYPE_KIND_UNKNOWN);

  TokenValue value;
  value.u = (unsigned long long)kind;
  *tpof   = ast_lit(cnt->ast, NULL, &ftypes[FTYPE_S32], value);

  finish();
}

Node *
check_type_enum(Context *cnt, Node **type)
{
  NodeTypeEnum *_type = peek_type_enum(*type);
  assert(_type->base_type);
  Node *tmp = ast_get_type(_type->base_type);

  if (node_is_not(tmp, NODE_TYPE_FUND)) {
    check_error_node(cnt, ERR_INVALID_TYPE, _type->base_type, BUILDER_CUR_WORD,
                     "enum base type must be an integer type");
    finish();
  }

  switch (peek_type_fund(tmp)->code) {
  case FTYPE_S8:
  case FTYPE_S16:
  case FTYPE_S32:
  case FTYPE_S64:
  case FTYPE_U8:
  case FTYPE_U16:
  case FTYPE_U32:
  case FTYPE_U64:
  case FTYPE_SIZE:
  case FTYPE_CHAR:
    break;
  default: {
    check_error_node(cnt, ERR_INVALID_TYPE, _type->base_type, BUILDER_CUR_WORD,
                     "enum base type must be an integer type");
  }
  }

  if (ast_type_get_ptr(tmp)) {
    check_error_node(cnt, ERR_INVALID_TYPE, _type->base_type, BUILDER_CUR_WORD,
                     "enum base type cannot be a pointer");
  }

  _type->base_type = tmp;

  finish();
}

Node *
check_expr_member(Context *cnt, Node **member)
{
  NodeExprMember *_member = peek_expr_member(*member);
  Node *          found   = NULL;
  assert(_member->ident);
  Node *lhs_type = ast_get_type(_member->next);

  if (_member->next) {
    lhs_type = ast_get_type(_member->next);
  } else {
    lhs_type = peek_ident(_member->ident)->parent_compound;
    assert(lhs_type);
    /* TODO: valid only for lit_cmp!!! */
    lhs_type = peek_lit_cmp(lhs_type)->type;
    lhs_type = ast_get_type(lhs_type);
  }

  if (!lhs_type) finish();
  if (ast_type_get_arr(lhs_type)) {
    /* is member array 'count'??? */
    if (ast_is_buildin(_member->ident) == BUILDIN_ARR_COUNT) {
      Node *tmp_next  = (*member)->next;
      *member         = ast_node_dup(cnt->ast, ast_type_get_arr(lhs_type));
      (*member)->next = tmp_next;

      finish();
    }
  } else if (node_is(lhs_type, NODE_TYPE_STRUCT)) {
    /* structure member */
    _member->kind = MEM_KIND_STRUCT;

    NodeTypeStruct *_lhs_type = peek_type_struct(lhs_type);
    /* lhs_type cannot be anonymous structure type (generate error later instead of assert?) */
    assert(_lhs_type->base_decl);

    found = _lookup(peek_decl(_lhs_type->base_decl)->value, _member->ident, NULL, false);
    if (!found) wait(_member->ident);

    if (_member->ptr_ref != (_lhs_type->ptr ? true : false)) {
      check_error_node(cnt, ERR_INVALID_MEMBER_ACCESS, *member, BUILDER_CUR_WORD,
                       "invalid member access, use %s",
                       _member->ptr_ref ? "'.' instead of '->'" : "'->' instead of '.'");
    }
  } else if (node_is(lhs_type, NODE_TYPE_ENUM)) {
    /* enum variant */
    _member->kind           = MEM_KIND_ENUM;
    NodeTypeEnum *_lhs_type = peek_type_enum(lhs_type);
    assert(_lhs_type->base_decl);

    found = _lookup(peek_decl(_lhs_type->base_decl)->value, _member->ident, NULL, false);
    if (!found) wait(_member->ident);

    if (_member->ptr_ref) {
      check_error_node(cnt, ERR_INVALID_MEMBER_ACCESS, *member, BUILDER_CUR_WORD,
                       "use '.' for access to enum variants");
    }
  } else {
    check_error_node(cnt, ERR_EXPECTED_TYPE_STRUCT, _member->next, BUILDER_CUR_WORD,
                     "expected structure or enum");
  }

  _member->type                   = ast_get_type(found);
  peek_ident(_member->ident)->ref = found;

  finish();
}

Node *
check_expr_elem(Context *cnt, Node **elem)
{
  NodeExprElem *_elem = peek_expr_elem(*elem);
  assert(_elem->index);
  assert(_elem->next);

  _elem->type = ast_get_type(_elem->next);
  if (!ast_type_get_arr(_elem->type)) {
    check_error_node(cnt, ERR_INVALID_TYPE, *elem, BUILDER_CUR_WORD, "expected array");
  }

  _elem->type = ast_node_dup(cnt->ast, _elem->type);
  ast_type_set_arr(_elem->type, NULL);

  Node *index_type = ast_get_type(_elem->index);

  if (!implicit_cast(cnt, &_elem->index, &ftypes[FTYPE_SIZE])) {
    check_error_invalid_types(cnt, index_type, &ftypes[FTYPE_SIZE], _elem->index);
  }

  finish();
}

Node *
check_stmt_if(Context *cnt, Node **stmt_if)
{
  NodeStmtIf *_if = peek_stmt_if(*stmt_if);
  assert(_if->test);
  assert(_if->true_stmt);

  Node *test_type = ast_get_type(_if->test);
  if (!ast_type_cmp(test_type, &ftypes[FTYPE_BOOL])) {
    check_error_invalid_types(cnt, test_type, &ftypes[FTYPE_BOOL], _if->test);
  }
  finish();
}

bool
infer_type(Context *cnt, Node *decl)
{
  NodeDecl *_decl = peek_decl(decl);
  if (!_decl->value) return false;
  Node *inferred_type = ast_get_type(_decl->value);
  if (!inferred_type) return false;

  if (_decl->type && !ast_type_cmp(inferred_type, _decl->type)) {
    if (!implicit_cast(cnt, &_decl->value, _decl->type)) {
      check_error_invalid_types(cnt, _decl->type, inferred_type, _decl->value);
      return false;
    }
  }

  /* infer type from value */
  _decl->type = ast_get_type(_decl->value);
  return true;
}

Node *
check_decl(Context *cnt, Node **decl)
{
  NodeDecl *_decl          = peek_decl(*decl);
  bool      lookup_in_tree = true;

  assert(_decl->name);
  if (!infer_type(cnt, *decl) && !_decl->type) {
    check_error_node(cnt, ERR_INVALID_TYPE, *decl, BUILDER_CUR_WORD,
                     "cannot infer declaration type");
    finish();
  }

  _decl->in_gscope =
      ast_get_scope(peek_ident(_decl->name)->parent_compound) == cnt->assembly->gscope;

  if (_decl->flags & FLAG_MAIN && _decl->kind != DECL_KIND_FN) {
    check_error_node(cnt, ERR_INVALID_TYPE, *decl, BUILDER_CUR_WORD,
                     "main is expected to be function");
  }

  switch (_decl->kind) {
  case DECL_KIND_STRUCT: {
    Node *value_type = ast_get_type(_decl->value);

    /* what about anonymous types ??? */
    assert(peek_type_struct(value_type)->base_decl);
    // peek_type_struct(value_type)->base_decl = *decl;

    if (_decl->mutable) {
      check_error_node(cnt, ERR_INVALID_MUTABILITY, *decl, BUILDER_CUR_WORD,
                       "structure declaration cannot be mutable");
    }

    if (peek_type_struct(value_type)->typesc == 0) {
      check_error_node(cnt, ERR_EMPTY, _decl->name, BUILDER_CUR_WORD, "empty structure");
    }

    break;
  }

  case DECL_KIND_MEMBER: {
    /* Structure members cannot be initialized with default value (foo s32 := 10), when implicit
     * structure initialization will be implemented in future, mutablility checking will be
     * required. In this case assignment of any kind will cause error */
    if (_decl->value) {
      check_error_node(cnt, ERR_INVALID_TYPE, _decl->value, BUILDER_CUR_WORD,
                       "struct member cannot have value binding");
    }
    lookup_in_tree = false;
    break;
  }

  case DECL_KIND_FIELD: {
    if (ast_type_kind(ast_get_type(_decl->type)) == TYPE_KIND_FN) {
      char tmp[256];
      ast_type_to_string(tmp, 256, ast_get_type(_decl->type));
      check_error_node(cnt, ERR_INVALID_TYPE, _decl->name, BUILDER_CUR_WORD,
                       "invalid type of variable '%s'", tmp);
    }

    if (_decl->in_gscope && !_decl->value) {
      check_error_node(cnt, ERR_EXPECTED_EXPR, _decl->type, BUILDER_CUR_AFTER,
                       "global variables needs to be initialized");
    }
    break;
  }

  case DECL_KIND_ARG: {
    if (_decl->value) {
      check_error_node(cnt, ERR_INVALID_ARG_TYPE, *decl, BUILDER_CUR_WORD,
                       "function arguments cannot have value binding");
    }
    break;
  }

  case DECL_KIND_VARIANT: {
    if (_decl->mutable) {
      check_error_node(cnt, ERR_INVALID_MUTABILITY, *decl, BUILDER_CUR_WORD,
                       "an enum variant cannot be mutable");
    }
    break;
  }

  case DECL_KIND_ENUM: {
    Node *value_type = ast_get_type(_decl->value);
    /* what about anonymous types ??? */
    assert(peek_type_enum(value_type)->base_decl);
    // peek_type_enum(value_type)->base_decl = *decl;
    break;
  }

  case DECL_KIND_FN:
    break;
  case DECL_KIND_TYPE:
    bl_abort("unimplemented");
  case DECL_KIND_UNKNOWN:
    bl_abort("unknown declaration kind");
  }

  assert(_decl->type);
  TypeKind type_kind = ast_type_kind(ast_get_type(_decl->type));
  if (type_kind == TYPE_KIND_VOID) {
    char tmp[256];
    ast_type_to_string(tmp, 256, ast_get_type(_decl->type));
    check_error_node(cnt, ERR_INVALID_TYPE, _decl->name, BUILDER_CUR_WORD,
                     "declaration has invalid type '%s'", tmp);
  }

  if (type_kind == TYPE_KIND_FN && _decl->mutable) {
    char tmp[256];
    ast_type_to_string(tmp, 256, ast_get_type(_decl->type));
    check_error_node(
        cnt, ERR_INVALID_TYPE, _decl->name, BUILDER_CUR_WORD,
        "declaration has invalid type '%s', a function mutable must be referenced by pointer", tmp);
  }
  _decl->type = ast_get_type(_decl->type);

  /* infer type for 'null' value */
  if (_decl->value && node_is(_decl->value, NODE_EXPR_NULL)) {
    if (type_kind == TYPE_KIND_PTR) {
      peek_expr_null(_decl->value)->type = _decl->type;
    } else {
      check_error_node(cnt, ERR_INVALID_TYPE, _decl->value, BUILDER_CUR_WORD,
                       "'null' cannot be used because the declaration is not a pointer");
    }
  }

  /* skip registration of test cases into symbol table */
  if (_decl->flags & FLAG_TEST) {
    finish();
  }

  /* provide symbol into scope if there is no conflict */
  Node *conflict = lookup(_decl->name, NULL, lookup_in_tree);
  if (conflict) {
    check_error_node(cnt, ERR_DUPLICATE_SYMBOL, *decl, BUILDER_CUR_WORD,
                     "symbol with same name is already declared");

    check_note_node(cnt, conflict, BUILDER_CUR_WORD, "previous declaration found here");
  } else {
    provide(_decl->name, *decl);
    waiting_resume(cnt, _decl->name);
  }

  finish();
}

Node *
check_lit_cmp(Context *cnt, Node **cmp)
{
  // TODO
  // Node *      tmp  = NULL;
  NodeLitCmp *_cmp = peek_lit_cmp(*cmp);
  Node *      type = ast_get_type(_cmp->type);
  assert(type);
  type               = ast_unroll_ident(type);
  _cmp->type         = ast_get_type(type);
  TypeKind type_kind = ast_type_kind(_cmp->type);

  if (_cmp->fieldc == 0) {
    check_error_node(cnt, ERR_EXPECTED_EXPR, *cmp, BUILDER_CUR_AFTER,
                     "expected initialization fields");
    finish();
  }

  if (type_kind == TYPE_KIND_VOID) {
    char tmp[256];
    ast_type_to_string(tmp, 256, type);
    check_error_node(cnt, ERR_INVALID_TYPE, _cmp->type, BUILDER_CUR_WORD,
                     "initialization has invalid type '%s'", tmp);
    finish();
  }

  switch (node_code(type)) {
  case NODE_TYPE_FUND: {
    if (_cmp->fieldc != 1) {
      check_error_node(cnt, ERR_EXPECTED_EXPR, *cmp, BUILDER_CUR_AFTER,
                       "expected one initialization field for fundamental types");
      finish();
    }

    if (!ast_type_cmp(_cmp->fields, type) && !implicit_cast(cnt, &_cmp->fields, type)) {
      check_error_invalid_types(cnt, ast_get_type(_cmp->fields), type, _cmp->fields);
      finish();
    }

    /* convert simple initialization compound to expression */
    *cmp = _cmp->fields;
    break;
  }

  case NODE_TYPE_STRUCT: {
    /* struct-field initializers need to be sorted and only left-hand side of expression can be used
     * for ir generation
     */
    NodeTypeStruct *_type = peek_type_struct(type);
    Node **         tmp   = bl_calloc((size_t)_type->typesc, sizeof(Node *));
    if (!tmp) bl_abort("bad alloc");

    Node *field;
    bool  noerr = true;
    node_foreach(_cmp->fields, field)
    {
      assert(field);
      if (node_is_not(field, NODE_EXPR_BINOP)) {
        check_error_node(cnt, ERR_INVALID_INITIALIZER, field, BUILDER_CUR_WORD,
                         "expected member initialization");
        noerr = false;
        continue;
      }

      /* is binop */
      NodeExprBinop *_binop = peek_expr_binop(field);
      assert(_binop->lhs);
      if (node_is_not(_binop->lhs, NODE_EXPR_MEMBER)) {
        check_error_node(cnt, ERR_INVALID_INITIALIZER, field, BUILDER_CUR_WORD,
                         "expected member initialization");
        noerr = false;
        continue;
      }

      if (_binop->op != SYM_ASSIGN) {
        check_error_node(cnt, ERR_INVALID_INITIALIZER, field, BUILDER_CUR_WORD,
                         "expected member initialization");
        noerr = false;
        continue;
      }

      NodeExprMember *_member = peek_expr_member(_binop->lhs);
      if (_member->kind != MEM_KIND_STRUCT) {
        check_error_node(cnt, ERR_INVALID_INITIALIZER, field, BUILDER_CUR_WORD,
                         "expected member initialization");
        noerr = false;
        continue;
      }

      assert(_member->ident);
      NodeIdent *_ident       = peek_ident(_member->ident);
      NodeDecl * _decl_member = peek_decl(_ident->ref);
      assert(_decl_member->order != -1);
      assert(_decl_member->order < _type->typesc);

      if (tmp[_decl_member->order]) {
        check_error_node(cnt, ERR_INVALID_INITIALIZER, field, BUILDER_CUR_WORD,
                         "structure member alrady initialized");

        check_note_node(cnt, tmp[_decl_member->order], BUILDER_CUR_WORD,
                        "previous initialization here");
        noerr = false;
        continue;
      }

      tmp[_decl_member->order] = field;
    }

    /* store initializers in right order and use only right side of field binop */
    if (noerr) {
      _cmp->fieldc  = _type->typesc;
      Node **fields = &_cmp->fields;
      for (int i = 0; i < _type->typesc; ++i) {
        if (!tmp[i]) {
          check_error_node(cnt, ERR_INVALID_INITIALIZER, *cmp, BUILDER_CUR_WORD,
                           "all structure members must be initialized");
          break;
        }

        tmp[i]       = peek_expr_binop(tmp[i])->rhs;
        tmp[i]->next = NULL;
        *fields      = tmp[i];
        fields       = &(*fields)->next;
      }
    }

    bl_free(tmp);
    tmp = NULL;

    break;
  }

  default:
    bl_abort("unsupported compound literal type");
  }

  finish();
}

void
checker_run(Builder *builder, Assembly *assembly)
{
  Context cnt = {
      .builder            = builder,
      .assembly           = assembly,
      .unit               = NULL,
      .ast                = NULL,
      .waiting            = bo_htbl_new_bo(bo_typeof(BArray), true, 2048),
      .flatten_cache      = bo_array_new(sizeof(BArray *)),
      .provided_in_gscope = scope_new(assembly->scope_cache, 4092),
  };

  /* TODO: stack size */
  eval_init(&cnt.evaluator, 1024);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    cnt.ast  = &unit->ast;
    check_flatten(&cnt, &unit->ast.root);
  }

  check_unresolved(&cnt);

  flatten_free_cache(cnt.flatten_cache);

  if (!assembly->has_main && (!(builder->flags & (BUILDER_SYNTAX_ONLY | BUILDER_NO_BIN)))) {
    builder_msg(builder, BUILDER_MSG_ERROR, ERR_NO_MAIN_METHOD, NULL, BUILDER_CUR_WORD,
                "assembly has no 'main' method");
  }

  bo_unref(cnt.waiting);
  bo_unref(cnt.flatten_cache);
#if BL_DEBUG
  if (_flatten != 0) bl_log(RED("leaking flatten cache: %d"), _flatten);
#endif
  eval_terminate(&cnt.evaluator);
}
