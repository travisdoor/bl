//************************************************************************************************
// bl
//
// File:   interp.c
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

#include "interp.h"
#include "common.h"

#define VERBOSE 1

// clang-format off
#define _CMD_LIST \
    cmd(LOAD_CONST) \
    cmd(BINARY_ADD) \
    cmd(BINARY_SUB) \
    cmd(BINARY_MUL) \
    cmd(BINARY_DIV)

// clang-format on

typedef enum
{
#define cmd(name) CMD_##name,
  _CMD_LIST
#undef cmd
      CMD_COUNT
} cmd_e;

static const char *cmd_strings[] = {
#define cmd(name) #name,
    _CMD_LIST
#undef cmd
};

static inline void
push(interp_t *interp, size_t value)
{
  if (interp->pc >= INTERP_STACK_SIZE) bl_abort("stack overflow");
  interp->stack[interp->pc++] = value;
}

static inline size_t
pop(interp_t *interp)
{
  if (interp->pc >= INTERP_STACK_SIZE) bl_abort("stack underflow");
  return interp->stack[--interp->pc];
}

static void
reset(interp_t *interp);

static void
emit(interp_t *interp, int cmd, size_t value);

static void
cmp_node(interp_t *interp, node_t *node);

static void
cmp_lit(interp_t *interp, node_t *lit);

static void
cmp_binop(interp_t *interp, node_t *binop);

void
reset(interp_t *interp)
{
  interp->pc = 0;
}

void
emit(interp_t *interp, int cmd, size_t value)
{
#if VERBOSE
  bl_log("%s(%d)", cmd_strings[cmd], value);
#endif

  switch (cmd) {
  case CMD_LOAD_CONST: {
    push(interp, value);
    break;
  }

  case CMD_BINARY_ADD: {
    size_t a = pop(interp);
    size_t b = pop(interp);
    push(interp, b + a);
    break;
  }

  case CMD_BINARY_SUB: {
    size_t a = pop(interp);
    size_t b = pop(interp);
    push(interp, b - a);
    break;
  }

  case CMD_BINARY_MUL: {
    size_t a = pop(interp);
    size_t b = pop(interp);
    push(interp, b * a);
    break;
  }

  case CMD_BINARY_DIV: {
    size_t a = pop(interp);
    size_t b = pop(interp);
    push(interp, b / a);
    break;
  }

  default:
    break;
  }
}

void
cmp_node(interp_t *interp, node_t *node)
{
  if (!node) return;
  switch (node_code(node)) {
  case NODE_LIT:
    cmp_lit(interp, node);
    break;

  case NODE_EXPR_BINOP:
    cmp_binop(interp, node);
    break;

  default:
    break;
  }
}

void
cmp_lit(interp_t *interp, node_t *lit)
{
  assert(lit);
  node_lit_t *_lit = peek_lit(lit);

  /* just for signed numbers now */
  assert(ast_get_type_kind(_lit->type) == TYPE_KIND_SINT);
  emit(interp, CMD_LOAD_CONST, _lit->value.u);
}

void
cmp_binop(interp_t *interp, node_t *binop)
{
  assert(binop);
  node_expr_binop_t *_binop = peek_expr_binop(binop);

  cmp_node(interp, _binop->lhs);
  cmp_node(interp, _binop->rhs);

  switch (_binop->op) {
  case SYM_PLUS:
    emit(interp, CMD_BINARY_ADD, 0);
    break;

  case SYM_MINUS:
    emit(interp, CMD_BINARY_SUB, 0);
    break;

  case SYM_ASTERISK:
    emit(interp, CMD_BINARY_MUL, 0);
    break;

  case SYM_SLASH:
    emit(interp, CMD_BINARY_DIV, 0);
    break;

  default:
    break;
  };
}

/* public */
void
interp_init(interp_t *interp)
{
  /* nothing??? */
}

unsigned long long
interp_node(interp_t *interp, node_t *node)
{
#if VERBOSE
  bl_log("run interpreter:");
#endif
  reset(interp);
  cmp_node(interp, node);

  return pop(interp);
}
