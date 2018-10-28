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

#define FLATTEN_ARENA_CHUNK_COUNT 128

#define VERBOSE 0
#define VERBOSE_MULTIPLE_CHECK 0

#define finish() return NULL
#define wait(_n) return (_n)

#define FN_ARR_LEN_NAME "len@"
#define FN_ARR_TMP_NAME "arr@"

#define check_error_node(cnt, kind, node, pos, format, ...)                                        \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_ERROR, (kind), (node)->src, (pos), (format),           \
                ##__VA_ARGS__);                                                                    \
  }

#define check_note_node(cnt, node, pos, format, ...)                                               \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_NOTE, 0, (node)->src, (pos), (format), ##__VA_ARGS__); \
  }

typedef struct
{
  Eval        evaluator;
  Arena       flatten_arena;
  Builder *   builder;
  Assembly *  assembly;
  Unit *      unit;
  Buildin *   buildin;
  Arena *     ast_arena;
  Arena *     type_arena;
  BHashTable *waiting;
  Scope *     provided_in_gscope;
  BArray *    flatten_cache;
} Context;

typedef struct
{
  BArray *  stack;
  AstIdent *waitfor;
  size_t    i;
} Flatten;

void
provide(Context *cnt, AstIdent *name, AstDecl *decl);

static inline void
waiting_push(BHashTable *waiting, Flatten *flatten);

void
waiting_resume(Context *cnt, AstIdent *ident);

static Flatten *
flatten_get(Context *cnt);

static void
flatten_put(Context *cnt, Flatten *flatten);

static void
flatten_dtor(Flatten *flatten);

static inline void
flatten_push(Flatten *flatten, Ast **node);

static void
flatten_node(Context *cnt, Flatten *fbuf, Ast **node);

static void
flatten_check(Context *cnt, Ast **node);

static void
flatten_process(Context *cnt, Flatten *flatten);

static bool
cmp_type(AstType *first, AstType *second);

/* perform checking on node of any type, return NULL when node was sucessfully checked or ponter to
 * waiting-for node */
static AstIdent *
check_node(Context *cnt, Ast **node);

static AstIdent *
check_expr(Context *cnt, AstExpr **expr);

static AstIdent *
check_type(Context *cnt, AstType **type);

static AstIdent *
check_expr_ref(Context *cnt, AstExprRef **expr_ref);

static AstIdent *
check_type_ref(Context *cnt, AstTypeRef **type_ref);

static AstIdent *
check_type_fn(Context *cnt, AstTypeFn **type_fn);

static AstIdent *
check_decl(Context *cnt, AstDecl **decl);

static AstIdent *
check_expr_lit_int(Context *cnt, AstExprLitInt **lit);

void
provide(Context *cnt, AstIdent *name, AstDecl *decl)
{
  Scope *scope = name->scope;
  assert(scope);

  AstDecl *conflict = scope_lookup(scope, name, true);
  if (conflict) {
    /* symbol collision !!! */
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_DUPLICATE_SYMBOL, ((Ast *)name)->src,
                BUILDER_CUR_WORD, "symbol with same name is already declared");

    builder_msg(cnt->builder, BUILDER_MSG_NOTE, 0, ((Ast *)conflict)->src, BUILDER_CUR_WORD,
                "previous declaration found here");
  } else {
    scope_insert(scope, name->hash, decl);
    waiting_resume(cnt, name);
  }
}

void
waiting_push(BHashTable *waiting, Flatten *flatten)
{
  BArray *queue;
  if (bo_htbl_has_key(waiting, flatten->waitfor->hash)) {
    queue = bo_htbl_at(waiting, flatten->waitfor->hash, BArray *);
  } else {
    queue = bo_array_new(sizeof(Flatten *));
    bo_htbl_insert(waiting, flatten->waitfor->hash, queue);
  }
  assert(queue);
  bo_array_push_back(queue, flatten);
}

void
waiting_resume(Context *cnt, AstIdent *ident)
{
  /* is there some flattens waiting for this symbol??? */
  if (!bo_htbl_has_key(cnt->waiting, ident->hash)) return;

#if VERBOSE
  bl_log("checker: resume " RED("'%s'"), ident->str);
#endif

  /* resume all waiting flattens */
  BArray *q = bo_htbl_at(cnt->waiting, ident->hash, BArray *);
  assert(q && "invalid flattens queue");

  /* NOTE: we need to iterate backwards from last element in 'q' because it can be modified in
   * 'flatten_process' method */
  Flatten * flatten;
  const int c = (int)bo_array_size(q);
  for (int i = c - 1; i >= 0; --i) {
    flatten = bo_array_at(q, i, Flatten *);
    bo_array_erase(q, i);
    flatten_process(cnt, flatten);
  }

  if (bo_array_empty(q)) bo_htbl_erase_key(cnt->waiting, ident->hash);
}

void
check_unresolved(Context *cnt)
{
  bo_iterator_t iter;
  BArray *      q;
  Flatten *     flatten;

  bhtbl_foreach(cnt->waiting, iter)
  {
    q = bo_htbl_iter_peek_value(cnt->waiting, &iter, BArray *);
    assert(q);

    for (size_t i = 0; i < bo_array_size(q); ++i) {
      flatten = bo_array_at(q, i, Flatten *);
      assert(flatten->waitfor);
      if (!scope_lookup(cnt->provided_in_gscope, flatten->waitfor, false))
        check_error_node(cnt, ERR_UNKNOWN_SYMBOL, (Ast *)flatten->waitfor, BUILDER_CUR_WORD,
                         "unknown symbol");
      flatten_put(cnt, flatten);
    }
  }
}

Flatten *
flatten_get(Context *cnt)
{
  Flatten *tmp = NULL;
  if (bo_array_size(cnt->flatten_cache) == 0) {
    tmp          = arena_alloc(&cnt->flatten_arena);
    tmp->stack   = bo_array_new(sizeof(Ast *));
    tmp->i       = 0;
    tmp->waitfor = NULL;
  } else {
    tmp = bo_array_at(cnt->flatten_cache, 0, Flatten *);
    bo_array_erase(cnt->flatten_cache, 0);
  }

  assert(tmp);
  return tmp;
}

void
flatten_put(Context *cnt, Flatten *flatten)
{
  bo_array_clear(flatten->stack);
  flatten->i       = 0;
  flatten->waitfor = NULL;
  bo_array_push_back(cnt->flatten_cache, flatten);
}

void
flatten_dtor(Flatten *flatten)
{
  bo_unref(flatten->stack);
}

void
flatten_push(Flatten *flatten, Ast **node)
{
  bo_array_push_back(flatten->stack, node);
}

void
flatten_process(Context *cnt, Flatten *flatten)
{
  assert(flatten);
  bool interrupted = false;

  AstIdent *waitfor;
  Ast **    tmp;
  for (; flatten->i < bo_array_size(flatten->stack); ++flatten->i) {
    tmp = bo_array_at(flatten->stack, flatten->i, Ast **);
    assert(*tmp);

    /* NULL means successfull check */
    waitfor = check_node(cnt, tmp);
    if (waitfor) {
      flatten->waitfor = waitfor;
      waiting_push(cnt->waiting, flatten);
      interrupted = true;
      break;
    }
  }

  if (!interrupted) {
    flatten_put(cnt, flatten);
  }
}

void
flatten_check(Context *cnt, Ast **node)
{
  Flatten *flatten = flatten_get(cnt);

  flatten_node(cnt, flatten, node);
  flatten_process(cnt, flatten);
}

void
flatten_node(Context *cnt, Flatten *fbuf, Ast **node)
{
  if (!*node) return;

#define flatten(_node) flatten_node(cnt, fbuf, (Ast **)(_node))

  switch (ast_kind(*node)) {
  case AST_DECL: {
    AstDecl *_decl = (AstDecl *)(*node);
    /* store declaration for temporary use here, this scope is used only for searching truly
     * undefined symbols later */
    /*if (_decl->in_gscope && !scope_lookup(cnt->provided_in_gscope, _decl->name, false))
      scope_insert(cnt->provided_in_gscope, _decl->name, *node);*/

    flatten(&_decl->type);
    flatten(&_decl->value);
    break;
  }

  case AST_MEMBER: {
    AstMember *_mem = (AstMember *)(*node);
    flatten(&_mem->type);
    break;
  }

  case AST_ARG: {
    AstArg *_arg = (AstArg *)(*node);
    flatten(&_arg->type);
    break;
  }

  case AST_VARIANT: {
    AstVariant *_variant = (AstVariant *)(*node);
    flatten(&_variant->type);
    flatten(&_variant->value);
    break;
  }

  case AST_BLOCK: {
    AstBlock *_block = (AstBlock *)(*node);

    Ast **tmp;
    node_foreach_ref(_block->nodes, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case AST_UBLOCK: {
    AstUBlock *_ublock = (AstUBlock *)(*node);

    Ast **tmp;
    node_foreach_ref(_ublock->nodes, tmp)
    {
      flatten_check(cnt, tmp);
    }
    break;
  }

  case AST_STMT_RETURN: {
    AstStmtReturn *_return = (AstStmtReturn *)(*node);
    flatten(&_return->expr);
    break;
  }

  case AST_STMT_IF: {
    AstStmtIf *_if = (AstStmtIf *)(*node);
    flatten(&_if->test);
    flatten(&_if->true_stmt);
    flatten(&_if->false_stmt);
    break;
  }

  case AST_STMT_LOOP: {
    AstStmtLoop *_loop = (AstStmtLoop *)(*node);
    flatten(&_loop->init);
    flatten(&_loop->condition);
    flatten(&_loop->increment);
    flatten(&_loop->block);
    break;
  }

  case AST_TYPE: {
    AstType **type = (AstType **)node;
    switch (ast_type_kind(*type)) {
    case AST_TYPE_FN: {
      AstTypeFn *_fn = (AstTypeFn *)(*type);
      Ast **     arg;
      node_foreach_ref(_fn->args, arg) flatten(arg);
      flatten(&_fn->ret_type);
      break;
    }
    default:
      break;
    }

    break;
  }

  case AST_EXPR: {
    AstExpr **expr = (AstExpr **)node;
    switch (ast_expr_kind(*expr)) {
    case AST_EXPR_LIT_FN: {
      AstExprLitFn *_fn = (AstExprLitFn *)(*expr);
      flatten(&(*expr)->type);
      flatten_check(cnt, &_fn->block);
      break;
    }
    default:
      break;
    }

    break;
  }

  default:
    break;
  }

  flatten_push(fbuf, node);
#undef flatten
}

AstIdent *
check_node(Context *cnt, Ast **node)
{
  assert(node);
  AstIdent *result = NULL;
#if defined(BL_DEBUG) && BL_VERBOSE_MUTIPLE_CHECK
  if (node->_state == BL_CHECKED)
    bl_msg_warning("unnecessary node check %s (%d)", node_name(node), node->_serial);
#endif

  switch (ast_kind(*node)) {

  case AST_DECL:
    result = check_decl(cnt, (AstDecl **)node);
    break;

  case AST_EXPR:
    result = check_expr(cnt, (AstExpr **)node);
    break;

  case AST_TYPE:
    result = check_type(cnt, (AstType **)node);
    break;

  default:
    break;
  }

#if VERBOSE
  {
    const char *file = (*node)->src ? (*node)->src->unit->name : "implicit";
    const int   line = (*node)->src ? (*node)->src->line : 0;
    const int   col  = (*node)->src ? (*node)->src->col : 0;
    if (result == NULL) {
      bl_log("checker: [" GREEN(" OK ") "] (%s) " CYAN("%s:%d:%d"), ast_get_name(*node), file, line,
             col);
    } else {
      bl_log("checker: [" RED("WAIT") "] (%s) " CYAN("%s:%d:%d") " -> " RED("%s"),
             ast_get_name(*node), file, line, col, result->str);
    }
  }
#endif

#ifdef BL_DEBUG
  (*node)->_state = result ? WAITING : CHECKED;
#endif
  return result;
}

AstIdent *
check_expr(Context *cnt, AstExpr **expr)
{
  AstIdent *result = NULL;
  switch (ast_expr_kind(*expr)) {
  case AST_EXPR_LIT_INT:
    result = check_expr_lit_int(cnt, (AstExprLitInt **)expr);
    break;

  case AST_EXPR_REF:
    result = check_expr_ref(cnt, (AstExprRef **)expr);
    break;
  default:
    break;
  }

  assert(result || (*expr)->type);
  return result;
}

AstIdent *
check_type(Context *cnt, AstType **type)
{
  AstIdent *result = NULL;

  switch (ast_type_kind(*type)) {

  case AST_TYPE_REF:
    result = check_type_ref(cnt, (AstTypeRef **)type);
    break;

  case AST_TYPE_FN:
    result = check_type_fn(cnt, (AstTypeFn **)type);
    break;

  default:
    break;
  }

  return result;
}

bool
cmp_type(AstType *first, AstType *second)
{
  assert(first && second);

  if (first == second) return true;

  AstTypeKind fc = ast_type_kind(first);
  AstTypeKind sc = ast_type_kind(second);
  if (fc != sc) return false;

  switch (fc) {
  case AST_TYPE_TYPE: {
    return true;
  }

  case AST_TYPE_FN: {
    AstTypeFn *_f = &first->fn;
    AstTypeFn *_s = &second->fn;

    if (_f->argc != _s->argc) return false;
    if (!cmp_type(_f->ret_type, _s->ret_type)) return false;

    Ast *argt1 = _f->args;
    Ast *argt2 = _s->args;
    while (argt1 && argt2) {
      if (!cmp_type(argt1->arg.type, argt2->arg.type)) return false;

      argt1 = argt1->next;
      argt2 = argt2->next;
    }
    return true;
  }

  case AST_TYPE_BAD:
  case AST_TYPE_REF:
  case AST_TYPE_INT:
  case AST_TYPE_VARGS:
  case AST_TYPE_ARR:
  case AST_TYPE_STRUCT:
  case AST_TYPE_ENUM:
  case AST_TYPE_PTR:
  case AST_TYPE_VOID:
    bl_abort("unimplemented %s", ast_get_name((Ast *)first));
  }

  return false;
}

static inline void
check_error_invalid_types(Context *cnt, AstType *first, AstType *second, Ast *err_pos)
{
  char tmp_first[256];
  char tmp_second[256];
  ast_type_to_str(tmp_first, 256, first);
  ast_type_to_str(tmp_second, 256, second);
  builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, err_pos->src, BUILDER_CUR_WORD,
              "no implicit cast for types '%s' and '%s'", tmp_first, tmp_second);
}

AstIdent *
check_expr_ref(Context *cnt, AstExprRef **ref)
{
  AstExprRef *_ref = *ref;
  assert(_ref->ident);

  Scope *scope = _ref->ident->scope;
  assert(scope && "missing scope for identificator");

  /* TODO: not only declarations are registred??? */
  AstDecl *found = scope_lookup(scope, _ref->ident, true);
  if (!found) wait(_ref->ident);
  found->used++;

  assert(found->type);
  ((AstExpr *)_ref)->type = found->type;

  finish();
}

AstIdent *
check_type_ref(Context *cnt, AstTypeRef **type_ref)
{
  AstTypeRef *_ref = *type_ref;
  assert(_ref->ident);

  Scope *scope = _ref->ident->scope;
  assert(scope && "missing scope for identificator");

  /* TODO: not only declarations are registred??? */
  AstDecl *found = scope_lookup(scope, _ref->ident, true);
  if (!found) wait(_ref->ident);
  found->used++;

  assert(found->type);
  if (found->type->kind != AST_TYPE_TYPE) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_TYPE, ((Ast *)_ref)->src,
                BUILDER_CUR_WORD, "expected type");
    _ref->type = ast_create_type(cnt->ast_arena, AST_TYPE_BAD, NULL, AstType *);
    finish();
  }

  *type_ref = (AstTypeRef *)found->type->type.spec;
  finish();
}

AstIdent *
check_type_fn(Context *cnt, AstTypeFn **type_fn)
{
  AstTypeFn *_fn = *type_fn;
  if (!_fn->ret_type)
    _fn->ret_type = (AstType *)buildin_get(cnt->buildin, cnt->buildin->hashes[BUILDIN_VOID]);
  finish();
}

static inline bool
infer_decl_type(Context *cnt, AstDecl *decl)
{
  if (!decl->value) return false;
  AstType *inferred = ast_get_type(decl->value);
  assert(inferred);

  if (inferred->kind == AST_TYPE_TYPE) {
    AstTypeType *tmp = ast_create_type(cnt->ast_arena, AST_TYPE_TYPE, NULL, AstTypeType *);
    tmp->name        = decl->name->str;
    tmp->spec        = inferred->type.spec;
    inferred         = (AstType *)tmp;
  }

  if (decl->type && !cmp_type(inferred, decl->type)) {
    check_error_invalid_types(cnt, decl->type, inferred, (Ast *)decl->value);
    return false;
  }

  decl->type = inferred;
  return true;
}

static bool
check_buildin_decl(Context *cnt, AstDecl *decl)
{
  if (!(decl->flags & FLAG_COMPILER)) return false;
  AstType *tmp = (AstType *)buildin_get(cnt->buildin, decl->name->hash);
  if (!tmp) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNCOMPATIBLE_MODIF, ((Ast *)decl)->src,
                BUILDER_CUR_WORD, "unknown compiler internal");
    return false;
  }

  decl->value       = ast_create_expr(cnt->ast_arena, AST_EXPR_TYPE, NULL, AstExpr *);
  decl->value->type = tmp;

  AstTypeType *type = ast_create_type(cnt->ast_arena, AST_TYPE_TYPE, NULL, AstTypeType *);
  type->name        = decl->name->str;
  type->spec        = decl->value->type;
  decl->type        = (AstType *)type;
  decl->kind        = DECL_KIND_TYPE;

  provide(cnt, decl->name, decl);
  return true;
}

static inline void
setup_decl_kind(AstDecl *decl)
{
  switch (ast_type_kind(decl->type)) {
  case AST_TYPE_VOID:
  case AST_TYPE_REF:
  case AST_TYPE_BAD:
  case AST_TYPE_VARGS:
    decl->kind = DECL_KIND_INVALID;
    break;
  case AST_TYPE_TYPE:
    decl->kind = DECL_KIND_TYPE;
    break;
  case AST_TYPE_FN:
    decl->kind = DECL_KIND_FN;
    break;
  case AST_TYPE_ENUM:
    decl->kind = DECL_KIND_ENUM;
    break;
  case AST_TYPE_INT:
  case AST_TYPE_STRUCT:
  case AST_TYPE_ARR:
  case AST_TYPE_PTR:
    decl->kind = DECL_KIND_FIELD;
    break;
  }
}

AstIdent *
check_decl(Context *cnt, AstDecl **decl)
{
  AstDecl *_decl = *decl;
  assert(_decl->name);

  /* solve buildins */
  if (check_buildin_decl(cnt, _decl)) finish();

  /* infer declaration type */
  infer_decl_type(cnt, _decl);
  assert(_decl->type);

  /* declaration kind based on type */
  setup_decl_kind(_decl);

  switch (_decl->kind) {
  case DECL_KIND_INVALID:
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, ((Ast *)_decl->type)->src,
                BUILDER_CUR_WORD, "declaration has invalid type");
    break;
  case DECL_KIND_FIELD:
    break;
  case DECL_KIND_TYPE:
    if (_decl->mutable) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_MUTABILITY, ((Ast *)_decl)->src,
                  BUILDER_CUR_WORD, "type declaration cannot be mutable");
    }
    break;
  case DECL_KIND_FN:
    if (_decl->mutable) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_MUTABILITY, ((Ast *)_decl)->src,
                  BUILDER_CUR_WORD, "function declaration cannot be mutable");
    }
    break;
  case DECL_KIND_ENUM:
    break;
  }

  provide(cnt, _decl->name, _decl);

  finish();
}

AstIdent *
check_expr_lit_int(Context *cnt, AstExprLitInt **lit)
{
  AstExpr *_lit = (AstExpr *)*lit;
  _lit->type    = (AstType *)buildin_get(cnt->buildin, cnt->buildin->hashes[BUILDIN_S32]);
  finish();
}

void
checker_run(Builder *builder, Assembly *assembly)
{
  Context cnt = {
      .builder            = builder,
      .assembly           = assembly,
      .unit               = NULL,
      .buildin            = &assembly->buildin,
      .ast_arena          = &assembly->ast_arena,
      .waiting            = bo_htbl_new_bo(bo_typeof(BArray), true, 2048),
      .flatten_cache      = bo_array_new(sizeof(BArray *)),
      .provided_in_gscope = scope_create(&assembly->scope_arena, NULL, 4092),
  };

  arena_init(&cnt.flatten_arena, sizeof(Flatten), FLATTEN_ARENA_CHUNK_COUNT,
             (ArenaElemDtor)flatten_dtor);

  /* TODO: stack size */
  eval_init(&cnt.evaluator, 1024);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    flatten_check(&cnt, (Ast **)&unit->ast);
  }

  check_unresolved(&cnt);

  if (!assembly->has_main && (!(builder->flags & (BUILDER_SYNTAX_ONLY | BUILDER_NO_BIN)))) {
    builder_msg(builder, BUILDER_MSG_ERROR, ERR_NO_MAIN_METHOD, NULL, BUILDER_CUR_WORD,
                "assembly has no 'main' method");
  }

  bo_unref(cnt.waiting);
  bo_unref(cnt.flatten_cache);
  eval_terminate(&cnt.evaluator);
  arena_terminate(&cnt.flatten_arena);
}
