//************************************************************************************************
// bl
//
// File:   eval.c
// Author: Martin Dorazil
// Date:   9/10/18
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

#include "eval.h"
#include "common.h"
#include "ast.h"

#define obj_new_s64(value)                                                                         \
  {                                                                                                \
    .type = OBJ_S64, .s64 = (value)                                                                \
  }

typedef enum
{
  OBJ_U64,
  OBJ_S64,
  // TODO: Real types and pointers???
} ObjType;

typedef struct Object
{
  ObjType type;

  union
  {
    uint64_t u64;
    int64_t  s64;
  };
} Object;

static inline void
push(Eval *eval, Object obj)
{
  if (eval->i == eval->size) bl_abort("stack overflow");
  eval->stack[eval->i++] = obj;
}

static inline Object
pop(Eval *eval)
{
  assert(eval->i != 0);
  return eval->stack[--(eval->i)];
}

static inline void
reset(Eval *eval)
{
  eval->i        = 0;
  eval->err_node = NULL;
}

static bool
eval_node(Eval *eval, Ast *node);

static bool
eval_lit(Eval *eval, Ast *lit);

static bool
eval_ident(Eval *eval, Ast *ident);

static bool
eval_binop(Eval *eval, Ast *binop);

static bool
eval_decl(Eval *eval, Ast *decl);

static bool
eval_cast(Eval *eval, Ast *cast);

bool
eval_lit(Eval *eval, Ast *lit)
{
  AstLitInt *_lit = ast_peek_lit_int(lit);
  /* TODO: support only signed integers for now! */
  //assert(ast_type_kind(_lit->type) == TYPE_KIND_SINT);

  Object tmp = obj_new_s64((int64_t) _lit->i);
  push(eval, tmp);

  return true;
}

bool
eval_cast(Eval *eval, Ast *cast)
{
  AstExprCast *_cast = ast_peek_expr_cast(cast);
  assert(_cast->next);
  /* TODO */
 
  return eval_node(eval, _cast->next);
}

bool
eval_ident(Eval *eval, Ast *ident)
{
  return false;
}

bool
eval_decl(Eval *eval, Ast *decl)
{
  AstDecl *_decl = ast_peek_decl(decl);
  assert(_decl->value);
  //if (_decl->kind != DECL_KIND_CONSTANT) return false;
  return eval_node(eval, _decl->value);
}

bool
eval_binop(Eval *eval, Ast *binop)
{
  AstExprBinop *_binop = ast_peek_expr_binop(binop);

  if (!eval_node(eval, _binop->lhs)) return false;
  if (!eval_node(eval, _binop->rhs)) return false;

  Object b = pop(eval);
  Object a = pop(eval);
  assert(a.type == b.type);
  Object result = {.type = a.type, .u64 = 0};

#define operate(op)                                                                                \
  if (result.type == OBJ_S64) {                                                                    \
    result.s64 = a.s64 op b.s64;                                                                   \
  }

  switch (_binop->kind) {
  case BINOP_ADD:
    operate(+);
    break;
  case BINOP_SUB:
    operate(-);
    break;
  case BINOP_MUL:
    operate(*);
    break;
  case BINOP_DIV:
    operate(/);
    break;
  default:
    bl_abort("unimplemented binary operation");
  }

  push(eval, result);
  return true;
#undef operate
}

bool
eval_node(Eval *eval, Ast *node)
{
  if (!node) return false;

  switch (ast_code(node)) {
  case AST_LIT_INT:
    return eval_lit(eval, node);
  case AST_EXPR_BINOP:
    return eval_binop(eval, node);
  case AST_EXPR_CAST:
    return eval_cast(eval, node);
  case AST_IDENT:
    return eval_ident(eval, node);
  case AST_DECL:
    return eval_decl(eval, node);
  default:
    eval->err_node = node;
  };

  return false;
}

/* public */
void
eval_init(Eval *eval, int stack_size)
{
  assert(stack_size > 0);
  eval->i        = 0;
  eval->size     = stack_size;
  eval->err_node = NULL;

  eval->stack    = bl_malloc(sizeof(Object) * stack_size);
  if (!eval->stack) bl_abort("bad allocation");
}

void
eval_terminate(Eval *eval)
{
  bl_free(eval->stack);
  eval->stack = NULL;
}

int
eval_expr(Eval *eval, Ast *node, Ast **err_node)
{
  assert(node);
  reset(eval);
  eval_node(eval, node);
  *err_node = eval->err_node;
  if (eval->err_node)
    return 0;

  return (int) pop(eval).s64;
}
