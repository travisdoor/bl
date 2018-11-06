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
 * function 'foo' apears in current of parent scope. To solve such problem whole AST is divided
into
 * smaller flatten queues which are later solved backwards. When compiler gets to unknown symbol we
 * take note about position in queue and push it into waiting cache.
 */

#include "stages.h"
#include "common.h"
#include "ast.h"

#define LOG_TAG GREEN("CHECK")
#define FLATTEN_ARENA_CHUNK_COUNT 128

#define VERBOSE_MULTIPLE_CHECK 0

#define finish() return NULL
#define wait(_n) return (_n)

#define FN_ARR_LEN_NAME "len@"
#define FN_ARR_TMP_NAME "arr@"

typedef struct
{
  Arena       flatten_arena;
  Builder *   builder;
  Assembly *  assembly;
  Unit *      unit;
  Arena *     ast_arena;
  Arena *     type_arena;
  BHashTable *waiting;
  Scope *     provided_in_gscope;
  BArray *    flatten_cache;
  BArray *    stack;
  bool        verbose;
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
flatten_decl(Context *cnt, Flatten *fbuf, AstDecl **decl);

static void
flatten_expr(Context *cnt, Flatten *fbuf, AstExpr **expr);

static void
flatten_type(Context *cnt, Flatten *fbuf, AstType **type);

static void
flatten_process(Context *cnt, Flatten *flatten);

static void
schedule_check(Context *cnt, Ast **node);

static void
do_check(Context *cnt);

static bool
cmp_type(AstType *first, AstType *second);

static AstType *
lookup_buildin_type(Context *cnt, AstIdent *ident);

/* perform checking on node of any type, return NULL when node was sucessfully checked or ponter to
 * waiting-for node */
static AstIdent *
check_node(Context *cnt, Ast **node);

static AstIdent *
check_expr(Context *cnt, AstExpr **expr);

static AstIdent *
check_type(Context *cnt, AstType **type);

static AstIdent *
check_type_ref(Context *cnt, AstTypeRef **type_ref);

static AstIdent *
check_decl(Context *cnt, AstDecl **decl);

static AstIdent *
check_decl_entity(Context *cnt, AstDeclEntity **decl);

static AstIdent *
check_decl_arg(Context *cnt, AstDeclArg **decl);

static AstIdent *
check_stmt_return(Context *cnt, AstStmtReturn **stmt);

static AstIdent *
check_expr_lit_int(Context *cnt, AstExprLitInt **lit);

static AstIdent *
check_expr_lit_fn(Context *cnt, AstExprLitFn **fn);

static AstIdent *
check_expr_binop(Context *cnt, AstExprBinop **expr);

static AstIdent *
check_expr_ref(Context *cnt, AstExprRef **expr);

static AstIdent *
check_expr_call(Context *cnt, AstExprCall **expr);

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

    if (cnt->verbose) msg_log(LOG_TAG ": new entry '%s'", name->str);
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

  if (cnt->verbose) msg_log(LOG_TAG ": resume " RED("'%s'"), ident->str);

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
        builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNKNOWN_SYMBOL,
                    ((Ast *)flatten->waitfor)->src, BUILDER_CUR_WORD, "unknown symbol");
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
schedule_check(Context *cnt, Ast **node)
{
  bo_array_push_back(cnt->stack, node);
}

#define flatten(_node) flatten_node(cnt, fbuf, (Ast **)(_node))

void
flatten_node(Context *cnt, Flatten *fbuf, Ast **node)
{
  if (!*node) return;

  switch ((*node)->kind) {
  case AST_DECL: {
    flatten_decl(cnt, fbuf, (AstDecl **)node);
    break;
  }

  case AST_BLOCK: {
    AstBlock *_block = (AstBlock *)(*node);

    Ast **    tmp;
    const int c = bo_array_size(_block->nodes);
    for (int i = 0; i < c; ++i) {
      tmp = &bo_array_at(_block->nodes, i, Ast *);
      flatten(tmp);
    }
    break;
  }

  case AST_UBLOCK: {
    AstUBlock *_ublock = (AstUBlock *)(*node);

    Ast **    tmp;
    const int c = bo_array_size(_ublock->nodes);
    for (int i = 0; i < c; ++i) {
      tmp = &bo_array_at(_ublock->nodes, i, Ast *);
      schedule_check(cnt, tmp);
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
    flatten_type(cnt, fbuf, (AstType **)node);
    break;
  }

  case AST_EXPR: {
    flatten_expr(cnt, fbuf, (AstExpr **)node);
    break;
  }

  default:
    msg_warning("missing flattening for %s", ast_get_name((Ast *)*node));
    break;
  }

  flatten_push(fbuf, node);
}

void
flatten_decl(Context *cnt, Flatten *fbuf, AstDecl **decl)
{
  switch ((*decl)->kind) {
  case AST_DECL_ENTITY: {
    AstDeclEntity *entity = (AstDeclEntity *)*decl;
    /* store declaration for temporary use here, this scope is used only for searching truly
     * undefined symbols later */
    /*if (_decl->in_gscope && !scope_lookup(cnt->provided_in_gscope, _decl->name, false))
      scope_insert(cnt->provided_in_gscope, _decl->name, *node);*/

    flatten(&entity->base.type);
    flatten(&entity->value);
    break;
  }

  case AST_DECL_MEMBER: {
    AstDeclMember *member = (AstDeclMember *)*decl;
    flatten(&member->base.type);
    break;
  }

  case AST_DECL_ARG: {
    AstDeclArg *arg = (AstDeclArg *)*decl;
    flatten(&arg->base.type);
    break;
  }

  case AST_DECL_VARIANT: {
    AstDeclVariant *var = (AstDeclVariant *)*decl;
    flatten(&var->base.type);
    flatten(&var->value);
    break;
  }

  default:
    msg_warning("missing flattening for %s", ast_get_name((Ast *)*decl));
    break;
  }
}

void
flatten_expr(Context *cnt, Flatten *fbuf, AstExpr **expr)
{
  switch ((*expr)->kind) {

  case AST_EXPR_LIT_FN: {
    AstExprLitFn *_fn = (AstExprLitFn *)(*expr);
    flatten(&(*expr)->type);
    schedule_check(cnt, &_fn->block);
    break;
  }

  case AST_EXPR_BINOP: {
    AstExprBinop *_binop = (AstExprBinop *)(*expr);
    flatten(&_binop->lhs);
    flatten(&_binop->rhs);
    break;
  }

  case AST_EXPR_CALL: {
    AstExprCall *_call = (AstExprCall *)(*expr);
    flatten(&_call->ref);

    Ast **    tmp;
    const int c = bo_array_size(_call->args);
    for (int i = 0; i < c; ++i) {
      tmp = &bo_array_at(_call->args, i, Ast *);
      flatten(tmp);
    }
    break;
  }

  case AST_EXPR_LIT_INT:
  case AST_EXPR_REF:
    break;
  default:
    msg_warning("missing flattening for %s", ast_get_name((Ast *)*expr));
    break;
  }
}

void
flatten_type(Context *cnt, Flatten *fbuf, AstType **type)
{
  switch ((*type)->kind) {
  case AST_TYPE_FN: {
    AstTypeFn *_fn = (AstTypeFn *)(*type);

    Ast **    tmp;
    const int c = bo_array_size(_fn->args);
    for (int i = 0; i < c; ++i) {
      tmp = &bo_array_at(_fn->args, i, Ast *);
      flatten(tmp);
    }

    flatten(&_fn->ret_type);
    break;
  }

  case AST_TYPE_REF:
  case AST_TYPE_VOID:
    break;

  default:
    msg_warning("missing flattening for %s", ast_get_name((Ast *)*type));
    break;
  }
}

#undef flatten

AstIdent *
check_node(Context *cnt, Ast **node)
{
  assert(node);
  AstIdent *result = NULL;
#if defined(BL_DEBUG) && BL_VERBOSE_MUTIPLE_CHECK
  if (node->_state == BL_CHECKED)
    bl_msg_warning("unnecessary node check %s (%d)", node_name(node), node->_serial);
#endif

  switch ((*node)->kind) {

  case AST_DECL:
    result = check_decl(cnt, (AstDecl **)node);
    break;

  case AST_EXPR:
    result = check_expr(cnt, (AstExpr **)node);
    break;

  case AST_TYPE:
    result = check_type(cnt, (AstType **)node);
    break;

  case AST_STMT_RETURN:
    result = check_stmt_return(cnt, (AstStmtReturn **)node);
    break;

  case AST_UBLOCK:
  case AST_BLOCK:
    break;

  default:
    msg_warning("missing checking for %s", ast_get_name((Ast *)*node));
    break;
  }

  if (cnt->verbose) {
    const char *file = (*node)->src ? (*node)->src->unit->name : "implicit";
    const int   line = (*node)->src ? (*node)->src->line : 0;
    const int   col  = (*node)->src ? (*node)->src->col : 0;
    const char *name =
        (*node)->kind == AST_DECL ? ((AstDecl *)*node)->name->str : ast_get_name(*node);
    if (result == NULL) {
      msg_log(LOG_TAG ": [" GREEN(" OK ") "] <%s:%d:%d> '%s' ", file, line, col, name);
    } else {
      msg_log(LOG_TAG ": [" RED("WAIT") "] <%s:%d:%d> '%s' -> %s", file, line, col, name,
              result->str);
    }
  }

#ifdef BL_DEBUG
  (*node)->_state = result ? WAITING : CHECKED;
#endif
  return result;
}

AstIdent *
check_decl(Context *cnt, AstDecl **decl)
{
  AstIdent *result = NULL;
  switch ((*decl)->kind) {
  case AST_DECL_ENTITY:
    result = check_decl_entity(cnt, (AstDeclEntity **)decl);
    break;
  case AST_DECL_ARG:
    result = check_decl_arg(cnt, (AstDeclArg **)decl);
    break;

  default:
    msg_warning("missing checking for %s", ast_get_name((Ast *)*decl));
    break;
  }

  return result;
}

AstIdent *
check_expr(Context *cnt, AstExpr **expr)
{
  AstIdent *result = NULL;
  switch ((*expr)->kind) {
  case AST_EXPR_LIT_INT:
    result = check_expr_lit_int(cnt, (AstExprLitInt **)expr);
    break;

  case AST_EXPR_LIT_FN:
    result = check_expr_lit_fn(cnt, (AstExprLitFn **)expr);
    break;

  case AST_EXPR_REF:
    result = check_expr_ref(cnt, (AstExprRef **)expr);
    break;

  case AST_EXPR_CALL:
    result = check_expr_call(cnt, (AstExprCall **)expr);
    break;

  case AST_EXPR_BINOP:
    result = check_expr_binop(cnt, (AstExprBinop **)expr);
    break;

  default:
    msg_warning("missing checking for %s", ast_get_name((Ast *)*expr));
    break;
  }

  assert(result || (*expr)->type);
  assert(result || ((*expr)->adr_mode != ADR_MODE_INVALID));
  return result;
}

AstIdent *
check_type(Context *cnt, AstType **type)
{
  AstIdent *result = NULL;

  switch ((*type)->kind) {

  case AST_TYPE_REF:
    result = check_type_ref(cnt, (AstTypeRef **)type);
    break;

  case AST_TYPE_FN:
  case AST_TYPE_VOID:
    break;

  default:
    msg_warning("missing checking for %s", ast_get_name((Ast *)*type));
    break;
  }

  return result;
}

bool
cmp_type(AstType *first, AstType *second)
{
  assert(first && second);

  if (first == second) return true;

  AstTypeKind fc = first->kind;
  AstTypeKind sc = second->kind;
  if (fc != sc) return false;

  switch (fc) {
  case AST_TYPE_TYPE: {
    return true;
  }

  case AST_TYPE_FN: {
    AstTypeFn *_f = (AstTypeFn *)first;
    AstTypeFn *_s = (AstTypeFn *)second;

    const int fargc = bo_array_size(_f->args);
    const int sargc = bo_array_size(_f->args);
    if (fargc != sargc) return false;
    if (!cmp_type(_f->ret_type, _s->ret_type)) return false;

    AstDeclArg *farg;
    AstDeclArg *sarg;
    for (int i = 0; i < fargc; ++i) {
      farg = bo_array_at(_f->args, i, AstDeclArg *);
      sarg = bo_array_at(_s->args, i, AstDeclArg *);

      if (!cmp_type(farg->base.type, sarg->base.type)) return false;
    }
    return true;
  }

  case AST_TYPE_INT: {
    AstTypeInt *_f = (AstTypeInt *)first;
    AstTypeInt *_s = (AstTypeInt *)second;

    return _f->bitcount == _s->bitcount && _f->is_signed == _s->is_signed;
  }

  case AST_TYPE_VOID:
    break;

  case AST_TYPE_BAD:
  case AST_TYPE_REF:
  case AST_TYPE_VARGS:
  case AST_TYPE_ARR:
  case AST_TYPE_STRUCT:
  case AST_TYPE_ENUM:
  case AST_TYPE_PTR:
    bl_abort("unimplemented %s", ast_get_name((Ast *)first));
  }

  return false;
}

AstType *
lookup_buildin_type(Context *cnt, AstIdent *ident)
{
  assert(ident);
  int id = builder_is_reserved(cnt->builder, ident->hash);
  if (id == -1) return NULL;
  switch ((ReservedNames)id) {
  case RESERVED_U8:
    return cnt->builder->buildin.entry_u8;
  case RESERVED_U16:
    return cnt->builder->buildin.entry_u16;
  case RESERVED_U32:
    return cnt->builder->buildin.entry_u32;
  case RESERVED_U64:
    return cnt->builder->buildin.entry_u64;
  case RESERVED_USIZE:
    return cnt->builder->buildin.entry_usize;
  case RESERVED_S8:
    return cnt->builder->buildin.entry_s8;
  case RESERVED_S16:
    return cnt->builder->buildin.entry_s16;
  case RESERVED_S32:
    return cnt->builder->buildin.entry_s32;
  case RESERVED_S64:
    return cnt->builder->buildin.entry_s64;
  default:
    return NULL;
  }
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
check_type_ref(Context *cnt, AstTypeRef **type_ref)
{
  AstTypeRef *_ref = *type_ref;
  assert(_ref->ident);

  Scope *scope = _ref->ident->scope;
  assert(scope && "missing scope for identificator");

  /* TODO: not only declarations are registred??? */
  AstDecl *found = scope_lookup(scope, _ref->ident, true);
  if (!found) wait(_ref->ident);

  if (found->kind == AST_DECL_ENTITY) ((AstDeclEntity *)found)->used++;

  assert(found->type);
  if (found->type->kind != AST_TYPE_TYPE) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_TYPE, ((Ast *)_ref)->src,
                BUILDER_CUR_WORD, "expected type");
    _ref->type = ast_create_type(cnt->ast_arena, AST_TYPE_BAD, NULL, AstType *);
    finish();
  }

  AstTypeType *type = (AstTypeType *)found->type;
  *type_ref         = (AstTypeRef *)type->spec;

  finish();
}

AstIdent *
check_stmt_return(Context *cnt, AstStmtReturn **stmt)
{
  AstStmtReturn *ret = *stmt;
  assert(ret->fn_decl && ret->fn_decl->kind == DECL_ENTITY_FN);
  AstDeclEntity *fn = (AstDeclEntity *)ret->fn_decl;

  assert(fn->base.type->kind == AST_TYPE_FN);
  AstType *expected_type = ((AstTypeFn *)fn->base.type)->ret_type;
  assert(expected_type);

  if (ret->expr) {
    if (!cmp_type(ret->expr->type, expected_type)) {
      check_error_invalid_types(cnt, ret->expr->type, expected_type, (Ast *)ret);
    }
  } else {
    if (!cmp_type(cnt->builder->buildin.entry_void, expected_type)) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_EXPR, ret->base.src,
                  BUILDER_CUR_WORD, "expected return value");
    }
  }

  finish();
}

static inline bool
infer_decl_type(Context *cnt, AstDeclEntity *decl)
{
  if (!decl->value) return false;
  AstType *inferred = decl->value->type;
  assert(inferred);

  if (inferred->kind == AST_TYPE_TYPE) {
    AstTypeType *tmp = ast_create_type(cnt->ast_arena, AST_TYPE_TYPE, NULL, AstTypeType *);
    tmp->name        = decl->base.name->str;
    tmp->spec        = ((AstTypeType *)inferred)->spec;
    inferred         = (AstType *)tmp;
  }

  if (decl->base.type && !cmp_type(inferred, decl->base.type)) {
    check_error_invalid_types(cnt, decl->base.type, inferred, (Ast *)decl->value);
    return false;
  }

  decl->base.type = inferred;
  return true;
}

static bool
check_buildin_decl(Context *cnt, AstDeclEntity *decl)
{
  if (!(decl->flags & FLAG_COMPILER)) return false;
  AstType *tmp = lookup_buildin_type(cnt, decl->base.name);
  if (!tmp) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNCOMPATIBLE_MODIF, ((Ast *)decl)->src,
                BUILDER_CUR_WORD, "unknown compiler internal");
    return false;
  }

  decl->value       = ast_create_expr(cnt->ast_arena, AST_EXPR_TYPE, NULL, AstExpr *);
  decl->value->type = tmp;

  AstTypeType *type = ast_create_type(cnt->ast_arena, AST_TYPE_TYPE, NULL, AstTypeType *);
  type->name        = decl->base.name->str;
  type->spec        = decl->value->type;
  decl->base.type   = (AstType *)type;
  decl->kind        = DECL_ENTITY_TYPE;

  provide(cnt, decl->base.name, &decl->base);
  return true;
}

static inline void
setup_decl_kind(AstDeclEntity *decl)
{
  switch (decl->base.type->kind) {
  case AST_TYPE_REF:
  case AST_TYPE_BAD:
  case AST_TYPE_VARGS:
    decl->kind = DECL_ENTITY_INVALID;
    break;
  case AST_TYPE_TYPE:
    decl->kind = DECL_ENTITY_TYPE;
    break;
  case AST_TYPE_FN:
    decl->kind = DECL_ENTITY_FN;
    break;
  case AST_TYPE_ENUM:
    decl->kind = DECL_ENTITY_ENUM;
    break;
  case AST_TYPE_INT:
  case AST_TYPE_STRUCT:
  case AST_TYPE_ARR:
  case AST_TYPE_PTR:
    decl->kind = DECL_ENTITY_FIELD;
    break;

  case AST_TYPE_VOID:
    decl->kind = DECL_ENTITY_INVALID;
  }
}

AstIdent *
check_decl_entity(Context *cnt, AstDeclEntity **decl)
{
  AstDeclEntity *entity = *decl;
  assert(entity->base.name);

  /* solve buildins */
  if (check_buildin_decl(cnt, entity)) finish();

  /* infer declaration type */
  infer_decl_type(cnt, entity);
  if (!entity->base.type) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_TYPE, entity->base.base.src,
                BUILDER_CUR_WORD, "declaration of unknown type");
    entity->base.type = ast_create_type(cnt->ast_arena, AST_TYPE_BAD, NULL, AstType *) finish();
  }

  /* declaration kind based on type */
  setup_decl_kind(entity);

  {
    const int id = builder_is_reserved(cnt->builder, entity->base.name->hash);

    /* check main method */
    if (id == RESERVED_MAIN) {
      cnt->assembly->entry_node = entity;
      entity->used++;

      if (entity->kind != DECL_ENTITY_FN) {
        builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_FUNC, entity->base.base.src,
                    BUILDER_CUR_WORD, "'main' is expected to be a function");
      }

      if (entity->flags != 0) {
        builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_UNEXPECTED_MODIF, entity->base.base.src,
                    BUILDER_CUR_WORD, "'main' method declared with invalid flags");
      }

    } else if (id != -1) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_NAME, entity->base.base.src,
                  BUILDER_CUR_WORD, "'%s' is compiler reserved name", entity->base.name->str);
    }
  }

  /* all globals need explicit initialization value */
  if (entity->in_gscope && !entity->value) {
    builder_msg(
        cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_INITIALIZATION, entity->base.base.src,
        BUILDER_CUR_WORD,
        "missing initializer, all global declarations must have explicit initialization value");
  }

  switch (entity->kind) {
  case DECL_ENTITY_INVALID: {
    char tmp[256];
    ast_type_to_str(tmp, 256, entity->base.type);
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_TYPE, entity->base.base.src,
                BUILDER_CUR_WORD, "declaration has invalid type '%s'", tmp);
    break;
  }

  case DECL_ENTITY_FIELD:
    break;
  case DECL_ENTITY_TYPE:
    if (entity->mutable) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_MUTABILITY, entity->base.base.src,
                  BUILDER_CUR_WORD, "type declaration cannot be mutable");
    }
    break;
  case DECL_ENTITY_FN:
    if (entity->mutable) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_MUTABILITY, entity->base.base.src,
                  BUILDER_CUR_WORD, "function declaration cannot be mutable");
    }
    break;
  case DECL_ENTITY_ENUM:
    break;
  }

  provide(cnt, entity->base.name, &entity->base);

  finish();
}

AstIdent *
check_decl_arg(Context *cnt, AstDeclArg **decl)
{
  AstDeclArg *arg = *decl;
  assert(arg->base.name && arg->base.type);

  {
    const int id = builder_is_reserved(cnt->builder, arg->base.name->hash);

    /* check main method */
    if (id != -1) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_NAME, arg->base.base.src,
                  BUILDER_CUR_WORD,
                  "'%s' is compiler reserved name and cannot be used as name of function argument",
                  arg->base.name->str);
    }
  }

  Scope *scope = arg->base.name->scope;
  assert(scope);

  AstDecl *conflict = scope_lookup(scope, arg->base.name, false);
  if (conflict) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_DUPLICATE_SYMBOL, arg->base.name->base.src,
                BUILDER_CUR_WORD, "argument with same name is already declared");

    builder_msg(cnt->builder, BUILDER_MSG_NOTE, 0, ((Ast *)conflict)->src, BUILDER_CUR_WORD,
                "previous declaration found here");
  } else {
    provide(cnt, arg->base.name, &arg->base);
  }

  finish();
}

AstIdent *
check_expr_lit_int(Context *cnt, AstExprLitInt **lit)
{
  AstExpr *_lit  = (AstExpr *)*lit;
  _lit->type     = cnt->builder->buildin.entry_s32;
  _lit->adr_mode = ADR_MODE_CONST;

  finish();
}

AstIdent *
check_expr_lit_fn(Context *cnt, AstExprLitFn **fn)
{
  (*fn)->base.adr_mode = ADR_MODE_NO_VALUE;
  finish();
}

AstIdent *
check_expr_ref(Context *cnt, AstExprRef **expr)
{
  AstExprRef *ref = *expr;
  assert(ref->ident);

  Scope *scope = ref->ident->scope;
  assert(scope && "missing scope for identificator");

  AstDecl *found = scope_lookup(scope, ref->ident, true);
  if (!found) wait(ref->ident);

  ref->base.type = found->type;

  if (found->kind == AST_DECL_ENTITY) {
    ref->base.adr_mode = ((AstDeclEntity *)found)->mutable ? ADR_MODE_MUT : ADR_MODE_IMMUT;
    ((AstDeclEntity *)found)->used++;
  } else {
    ref->base.adr_mode = ADR_MODE_MUT;
  }

  ref->ref = found;
  finish();
}

AstIdent *
check_expr_binop(Context *cnt, AstExprBinop **expr)
{
  AstExprBinop *binop = *expr;
  assert(binop->kind != BINOP_INVALID);
  assert(binop->lhs && binop->rhs);

  AdrMode  ladrm = binop->lhs->adr_mode;
  AdrMode  radrm = binop->rhs->adr_mode;
  AstType *ltype = binop->lhs->type;
  AstType *rtype = binop->rhs->type;
  assert(ladrm != ADR_MODE_INVALID && "invalid address mode of lhs expression!!!");
  assert(radrm != ADR_MODE_INVALID && "invalid address mode of rhs expression!!!");
  assert(ltype && rtype);

  if (ast_binop_is_assign(binop->kind)) {
    /* assignment */
    if (ladrm != ADR_MODE_MUT) {
      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_MUTABILITY, binop->base.base.src,
                  BUILDER_CUR_WORD, "left-hand side of assign expression cannot be assigned");
    }
  }

  binop->base.type     = ltype;
  binop->base.adr_mode = ladrm;

  if (!cmp_type(ltype, rtype)) {
    check_error_invalid_types(cnt, ltype, rtype, (Ast *)binop);
  }

  finish();
}

AstIdent *
check_expr_call(Context *cnt, AstExprCall **expr)
{
  AstExprCall *call = *expr;
  assert(call->ref);

  call->base.adr_mode = ADR_MODE_CONST;

  AstType *tmp_type = call->ref->type;
  if (tmp_type->kind != AST_TYPE_FN) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_EXPECTED_FUNC, ((Ast *)call->ref)->src,
                BUILDER_CUR_WORD, "expected function name before expr operator");
    call->base.type = tmp_type;
    finish();
  }

  AstTypeFn *callee_type = (AstTypeFn *)tmp_type;

  const int call_argc   = bo_array_size(call->args);
  const int callee_argc = bo_array_size(callee_type->args);

  if (call_argc != callee_argc) {
    builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_ARG_COUNT, ((Ast *)call)->src,
                BUILDER_CUR_WORD, "expected %d %s, but called with %d", callee_argc,
                callee_argc == 1 ? "argument" : "arguments", call_argc);

    call->base.type = callee_type->ret_type;
    finish();
  }

  AstExpr *   call_arg;
  AstDeclArg *callee_arg;

  for (int i = 0; i < call_argc; ++i) {
    call_arg   = bo_array_at(call->args, i, AstExpr *);
    callee_arg = bo_array_at(callee_type->args, i, AstDeclArg *);

    if (!cmp_type(call_arg->type, callee_arg->base.type)) {
      char tmp1[256];
      char tmp2[256];
      ast_type_to_str(tmp1, 256, call_arg->type);
      ast_type_to_str(tmp2, 256, callee_arg->base.type);

      builder_msg(cnt->builder, BUILDER_MSG_ERROR, ERR_INVALID_ARG_TYPE, call_arg->base.src,
                  BUILDER_CUR_WORD,
                  "invalid expr argument type, expected is '%s' but called with '%s'", tmp2, tmp1);

      break;
    }
  }

  call->base.type = callee_type->ret_type;
  finish();
}

void
do_check(Context *cnt)
{
  Ast **tmp;
  for (size_t i = 0; i < bo_array_size(cnt->stack); ++i) {
    tmp = bo_array_at(cnt->stack, i, Ast **);

    Flatten *flatten = flatten_get(cnt);

    flatten_node(cnt, flatten, tmp);
    flatten_process(cnt, flatten);
  }
}

void
checker_run(Builder *builder, Assembly *assembly)
{
  Context cnt = {
      .builder            = builder,
      .assembly           = assembly,
      .unit               = NULL,
      .ast_arena          = &assembly->ast_arena,
      .waiting            = bo_htbl_new_bo(bo_typeof(BArray), true, 2048),
      .flatten_cache      = bo_array_new(sizeof(BArray *)),
      .stack              = bo_array_new(sizeof(Ast **)),
      .provided_in_gscope = scope_create(&assembly->scope_arena, NULL, 4092),
      .verbose            = (bool)(builder->flags & BUILDER_VERBOSE),
  };

  arena_init(&cnt.flatten_arena, sizeof(Flatten), FLATTEN_ARENA_CHUNK_COUNT,
             (ArenaElemDtor)flatten_dtor);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    schedule_check(&cnt, (Ast **)&unit->ast);
  }

  do_check(&cnt);
  check_unresolved(&cnt);

  if (!assembly->entry_node && (!(builder->flags & (BUILDER_SYNTAX_ONLY | BUILDER_NO_BIN)))) {
    builder_msg(builder, BUILDER_MSG_ERROR, ERR_NO_MAIN_METHOD, NULL, BUILDER_CUR_WORD,
                "assembly has no 'main' entry method defined");
  }

  bo_unref(cnt.waiting);
  bo_unref(cnt.flatten_cache);
  bo_unref(cnt.stack);
  arena_terminate(&cnt.flatten_arena);
}
