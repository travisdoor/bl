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

#define VERBOSE 1
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
  Arena       flatten_arena;
  Builder *   builder;
  Assembly *  assembly;
  Unit *      unit;
  Arena *     ast_arena;
  Arena *     type_arena;
  Arena *     scope_entry_arena;
  BHashTable *waiting;
  Scope *     provided_in_gscope;
  BArray *    flatten_cache;
} Context;

typedef struct
{
  BArray *stack;
  Ast *  waitfor;
  size_t  i;
} Flatten;

static void
provide(Context *cnt, Ast *ident, Ast *provided);

static inline void
waiting_push(BHashTable *waiting, Flatten *flatten);

static void
waiting_resume(Context *cnt, Ast *ident);

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

/* perform checking on node of any type, return NULL when node was sucessfully checked or ponter to
 * waiting-for node */
static Ast *
check_node(Context *cnt, Ast **node);

static Ast *
check_ident(Context *cnt, Ast **ident);

static Ast *
check_lit(Context *cnt, Ast **lit);

void
provide(Context *cnt, Ast *ident, Ast *provided)
{
  /* TODO */
#if VERBOSE
  bl_log("providing " MAGENTA("'%s'") " (%d)", ast_peek_ident(ident)->str, ident->_serial);
#endif
}

void
waiting_push(BHashTable *waiting, Flatten *flatten)
{
  AstIdent *_waitfor = ast_peek_ident(flatten->waitfor);
  BArray *   queue;
  if (bo_htbl_has_key(waiting, _waitfor->hash)) {
    queue = bo_htbl_at(waiting, _waitfor->hash, BArray *);
  } else {
    queue = bo_array_new(sizeof(Flatten *));
    bo_htbl_insert(waiting, _waitfor->hash, queue);
  }
  assert(queue);
  bo_array_push_back(queue, flatten);
}

void
waiting_resume(Context *cnt, Ast *ident)
{
  AstIdent *_ident = ast_peek_ident(ident);
  /* is there some flattens waiting for this symbol??? */
  if (!bo_htbl_has_key(cnt->waiting, _ident->hash)) return;

  /* resume all waiting flattens */
  BArray *q = bo_htbl_at(cnt->waiting, _ident->hash, BArray *);
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

  if (bo_array_empty(q)) bo_htbl_erase_key(cnt->waiting, _ident->hash);
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

    // bl_log("size %d", bo_array_size(q));
    for (size_t i = 0; i < bo_array_size(q); ++i) {
      flatten = bo_array_at(q, i, Flatten *);
      assert(flatten->waitfor);
      if (!scope_lookup(cnt->provided_in_gscope, flatten->waitfor, false))
        check_error_node(cnt, ERR_UNKNOWN_SYMBOL, flatten->waitfor, BUILDER_CUR_WORD,
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

  Ast * waitfor;
  Ast **tmp;
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

#define flatten(_node) flatten_node(cnt, fbuf, (_node))

  switch (ast_node_code(*node)) {
  case AST_DECL: {
    AstDecl *_decl = ast_peek_decl(*node);
    /* store declaration for temporary use here, this scope is used only for searching truly
     * undefined symbols later */
    /*if (_decl->in_gscope && !scope_lookup(cnt->provided_in_gscope, _decl->name, false))
      scope_insert(cnt->provided_in_gscope, _decl->name, *node);*/

    flatten(&_decl->type);
    flatten(&_decl->value);
    break;
  }

  case AST_MEMBER: {
    AstMember *_mem = ast_peek_member(*node);
    flatten(&_mem->type);
    break;
  }

  case AST_ARG: {
    AstArg *_arg = ast_peek_arg(*node);
    flatten(&_arg->type);
    break;
  }

  case AST_VARIANT: {
    AstVariant *_variant = ast_peek_variant(*node);
    flatten(&_variant->type);
    flatten(&_variant->value);
    break;
  }

  case AST_LIT_FN: {
    AstLitFn *_fn = ast_peek_lit_fn(*node);
    flatten(&_fn->type);
    flatten_check(cnt, &_fn->block);
    break;
  }

  case AST_LIT_CMP: {
    AstLitCmp *_cmp = ast_peek_lit_cmp(*node);
    flatten(&_cmp->type);
    Ast **field;
    node_foreach_ref(_cmp->fields, field)
    {
      flatten(field);
    }
    break;
  }

  case AST_TYPE_ENUM: {
    AstTypeEnum *_enum = ast_peek_type_enum(*node);
    flatten(&_enum->type);
    Ast **variant;
    node_foreach_ref(_enum->variants, variant)
    {
      flatten(variant);
    }
    break;
  }

  case AST_BLOCK: {
    AstBlock *_block = ast_peek_block(*node);

    Ast **tmp;
    node_foreach_ref(_block->nodes, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case AST_UBLOCK: {
    AstUBlock *_ublock = ast_peek_ublock(*node);

    Ast **tmp;
    node_foreach_ref(_ublock->nodes, tmp)
    {
      flatten_check(cnt, tmp);
    }
    break;
  }

  case AST_STMT_RETURN: {
    AstStmtReturn *_return = ast_peek_stmt_return(*node);
    flatten(&_return->expr);
    break;
  }

  case AST_STMT_IF: {
    AstStmtIf *_if = ast_peek_stmt_if(*node);
    flatten(&_if->test);
    flatten(&_if->true_stmt);
    flatten(&_if->false_stmt);
    break;
  }

  case AST_STMT_LOOP: {
    AstStmtLoop *_loop = ast_peek_stmt_loop(*node);
    flatten(&_loop->init);
    flatten(&_loop->condition);
    flatten(&_loop->increment);
    flatten(&_loop->block);
    break;
  }

  case AST_EXPR_MEMBER: {
    AstExprMember *_member = ast_peek_expr_member(*node);
    flatten(&_member->next);
    break;
  }

  case AST_EXPR_ELEM: {
    AstExprElem *_elem = ast_peek_expr_elem(*node);
    flatten(&_elem->next);
    flatten(&_elem->index);
    break;
  }

  case AST_EXPR_CAST: {
    AstExprCast *_cast = ast_peek_expr_cast(*node);
    flatten(&_cast->type);
    flatten(&_cast->next);
    break;
  }

  case AST_EXPR_SIZEOF: {
    AstExprSizeof *_sizeof = ast_peek_expr_sizeof(*node);
    flatten(&_sizeof->in);
    break;
  }

  case AST_EXPR_TYPEOF: {
    AstExprTypeof *_typeof = ast_peek_expr_typeof(*node);
    flatten(&_typeof->in);
    break;
  }

  case AST_EXPR_CALL: {
    AstExprCall *_call = ast_peek_expr_call(*node);
    flatten(&_call->ref);

    Ast **tmp;
    node_foreach_ref(_call->args, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case AST_EXPR_BINOP: {
    AstExprBinop *_binop = ast_peek_expr_binop(*node);
    flatten(&_binop->lhs);
    flatten(&_binop->rhs);
    break;
  }

  case AST_EXPR_UNARY: {
    AstExprUnary *_unary = ast_peek_expr_unary(*node);
    flatten(&_unary->next);
    break;
  }

  case AST_TYPE_STRUCT: {
    AstTypeStruct *_type_struct = ast_peek_type_struct(*node);
    Ast **         tmp;
    node_foreach_ref(_type_struct->members, tmp)
    {
      flatten(tmp);
    }
    break;
  }

  case AST_TYPE_FN: {
    AstTypeFn *_type_fn = ast_peek_type_fn(*node);
    flatten(&_type_fn->ret_type);
    Ast **sub_type;
    node_foreach_ref(_type_fn->arg_types, sub_type)
    {
      flatten(sub_type);
    }
    break;
  }

  case AST_TYPE_ARR: {
    AstTypeArr *_arr = ast_peek_type_arr(*node);
    flatten(&_arr->elem_type);
    flatten(&_arr->len);
    break;
  }

  case AST_TYPE_PTR: {
    AstTypePtr *_ptr = ast_peek_type_ptr(*node);
    flatten(&_ptr->type);
    break;
  }

  default:
    break;
  }

  flatten_push(fbuf, node);
#undef flatten
}

Ast *
check_node(Context *cnt, Ast **node)
{
  assert(node);
  Ast *result = NULL;
#if defined(BL_DEBUG) && BL_VERBOSE_MUTIPLE_CHECK
  if (node->_state == BL_CHECKED)
    bl_msg_warning("unnecessary node check %s (%d)", node_name(node), node->_serial);
#endif

  switch (ast_node_code(*node)) {
  case AST_IDENT:
    result = check_ident(cnt, node);
    break;
  case AST_LIT_BOOL:
  case AST_LIT_CHAR:
  case AST_LIT_FLOAT:
  case AST_LIT_INT:
  case AST_LIT_STRING:
    result = check_lit(cnt, node);
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
      bl_log("checked [" GREEN(" OK ") "] (%s) " CYAN("%s:%d:%d"), ast_node_name(*node), file, line,
             col);
    } else {
      bl_log("checked [" RED("WAIT") "] (%s) " CYAN("%s:%d:%d") " -> " RED("%s"),
             ast_node_name(*node), file, line, col, ast_peek_ident(result)->str);
    }
  }
#endif

#ifdef BL_DEBUG
  (*node)->_state = result ? WAITING : CHECKED;
#endif
  return result;
}

Ast *
check_ident(Context *cnt, Ast **ident)
{
  finish();
}

Ast *
check_lit(Context *cnt, Ast **lit)
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
      .ast_arena          = &assembly->ast_arena,
      .type_arena         = &assembly->type_arena,
      .scope_entry_arena  = &assembly->scope_entry_arena,
      .waiting            = bo_htbl_new_bo(bo_typeof(BArray), true, 2048),
      .flatten_cache      = bo_array_new(sizeof(BArray *)),
      .provided_in_gscope = scope_create(&assembly->scope_arena, NULL, 4092),
  };

  arena_init(&cnt.flatten_arena, sizeof(Flatten), FLATTEN_ARENA_CHUNK_COUNT,
             (ArenaElemDtor)flatten_dtor);

  /* add buildin symbols to global scope table */
  types_add_builinds(assembly->gscope, &assembly->scope_entry_arena);

  /* TODO: stack size */
  eval_init(&cnt.evaluator, 1024);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    flatten_check(&cnt, &unit->ast);
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
