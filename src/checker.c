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

#define FN_ARR_LEN_NAME "len@"
#define FN_ARR_TMP_NAME "arr@"

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
to_any(Context *cnt, Node **node, Node *any_type);

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

static Node *
check_type_arr(Context *cnt, Node **type);

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
to_any(Context *cnt, Node **node, Node *any_type)
{
  /* INITIALIZER BOILERPLATE for any type */
  /* Here we convert anything to 'any { .value = cast(*u8) (&foo), .type = typeof(foo) }' */
  /*Node *from_type = ast_get_type(*node);

  Node *tmp           = *node;
  Node *val_base_type = ast_node_dup(cnt->ast, from_type);
  ast_type_set_ptr(val_base_type, 1);

  Node *u8_ptr = ast_type_fund(cnt->ast, NULL, FTYPE_U8, 1);
  Node *value  = ast_expr_unary(cnt->ast, NULL, SYM_AND, tmp, val_base_type);
  value        = ast_expr_cast(cnt->ast, NULL, u8_ptr, value);

  TokenValue type;
  type.u      = (unsigned long long)ast_type_kind(from_type);
  value->next = ast_lit(cnt->ast, NULL, &ftypes[FTYPE_S32], type);
  *node       = ast_lit_cmp(cnt->ast, NULL, any_type, value, 2, NULL);*/
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

  case NODE_MEMBER: {
    NodeMember *_mem = peek_member(*node);
    flatten(&_mem->type);
    break;
  }

  case NODE_LIT_FN: {
    NodeLitFn *_fn = peek_lit_fn(*node);
    flatten(&_fn->type);
    check_flatten(cnt, &_fn->block);
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
    NodeTypeStruct *_type_struct = peek_type_struct(*node);
    Node **         tmp;
    node_foreach_ref(_type_struct->members, tmp)
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

  case NODE_TYPE_ARR: {
    NodeTypeArr *_arr = peek_type_arr(*node);
    flatten(&_arr->elem_type);
    flatten(&_arr->len);
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
  /*assert(to_type);
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
    to_any(cnt, node, to_type);
    return true;
  }

  Node *tmp_next = (*node)->next;
  Node *type_dup = ast_node_dup(cnt->ast, to_type);
  Node *cast     = ast_expr_cast(cnt->ast, NULL, type_dup, *node);
  cast->next     = tmp_next;
  *node          = cast;
  */
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
  case NODE_TYPE_ARR:
    result = check_type_arr(cnt, node);
    break;
  case NODE_LIT_CMP:
    result = check_lit_cmp(cnt, node);
    break;

  case NODE_MEMBER:
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
check_expr_call(Context *cnt, Node **call)
{
  finish();
}

Node *
check_expr_unary(Context *cnt, Node **unary)
{
  finish();
}

static inline void
reduce_type_refs(Node **found)
{
  /* TODO */
}

Node *
check_ident(Context *cnt, Node **ident)
{
  NodeIdent *_ident = peek_ident(*ident);
  if (_ident->ref) {
    finish();
  }

  const int buildin = ast_is_buildin_type(*ident);
  if (buildin != -1) {
    /* connect buildin fundamental types references */
    _ident->ref = &ftypes[buildin];
    finish();
  }

  Node *found = lookup(*ident, NULL, true);
  if (!found) wait(*ident);

  assert(node_is(found, NODE_DECL) && "not declaration");
  peek_decl(found)->used++;
  reduce_type_refs(&found);

  _ident->ref = found;

  finish();
}

Node *
check_stmt_return(Context *cnt, Node **ret)
{
  finish();
}

Node *
check_expr_binop(Context *cnt, Node **binop)
{
  finish();
}

Node *
check_expr_cast(Context *cnt, Node **cast)
{
  finish();
}

Node *
check_expr_sizeof(Context *cnt, Node **szof)
{
  finish();
}

Node *
check_expr_typeof(Context *cnt, Node **tpof)
{
  finish();
}

Node *
check_type_enum(Context *cnt, Node **type)
{
  finish();
}

Node *
check_type_arr(Context *cnt, Node **type)
{
  finish();
}

Node *
check_expr_member(Context *cnt, Node **member)
{
  finish();
}

Node *
check_expr_elem(Context *cnt, Node **elem)
{
  finish();
}

Node *
check_stmt_if(Context *cnt, Node **stmt_if)
{
  finish();
}

bool
infer_type(Context *cnt, Node *decl)
{
  NodeDecl *_decl = peek_decl(decl);
  if (!_decl->value) return false;

  Node *infered = ast_typeof(_decl->value);
  _decl->type   = infered;

  return true;
}

Node *
check_decl(Context *cnt, Node **decl)
{
  NodeDecl *_decl          = peek_decl(*decl);
  bool      lookup_in_tree = true;

  _decl->in_gscope =
      ast_get_scope(peek_ident(_decl->name)->parent_compound) == cnt->assembly->gscope;

  if (_decl->flags & FLAG_MAIN && _decl->kind != DECL_KIND_FN) {
    check_error_node(cnt, ERR_INVALID_TYPE, *decl, BUILDER_CUR_WORD,
                     "main is expected to be function");
  }

  infer_type(cnt, *decl);

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
