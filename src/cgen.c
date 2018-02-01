//*****************************************************************************
// bl
//
// File:   cgen.c
// Author: Martin Dorazil
// Date:   31/01/2018
//
// Copyright 2017 Martin Dorazil
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
//*****************************************************************************

#include "cgen.h"
#include "bldebug.h"
#include "token.h"

#define GLOBAL_PREFIX "g"

static const char *header_comment = "/* Biscuit generated file, do not modify. */\n";
static const char *header_defaults = 
  "#include <stdlib.h>\n#include <bobject/bobject.h>\n#include <bobject/containers/string.h>\n";

static void
gen_decl(Unit *unit,
         Pnode *pnode,
         CSrc *src,
         int nesting,
         bool is_local,
         const char *prefix);

static void
gen_gscope(Unit *unit,
           Pnode *pnode,
           CSrc *src);

static void
gen_nscope(Unit *unit,
           Pnode *pnode,
           CSrc *src,
           int nesting);

static CSrc *
gen_scope(Unit *unit,
          Pnode *pnode,
          int nesting);

static void
gen_func(Unit *unit,
         Pnode *pnode,
         CSrc *src,
         int nesting,
         const char *prefix);

static void
gen_call(Unit *unit,
         Pnode *pnode,
         CSrc *src,
         int nesting,
         const char *prefix);

static void
gen_fargs(Unit *unit,
         Pnode *pnode,
         CSrc *src,
         bl_csrc_impl_layers l);

static void
gen_farg(Unit *unit,
        Pnode *pnode,
        CSrc *src,
        bl_csrc_impl_layers l);

static void
gen_args(Unit *unit,
         Pnode *pnode,
         CSrc *src,
         bl_csrc_impl_layers l);

static void
gen_arg(Unit *unit,
        Pnode *pnode,
        CSrc *src,
        bl_csrc_impl_layers l);

/* impl */
void
gen_decl(Unit *unit,
         Pnode *pnode,
         CSrc *src,
         int nesting,
         bool is_local,
         const char *prefix)
{
  Pnode *type = bo_array_at(pnode->nodes, 0, Pnode *);
  Pnode *ident = bo_array_at(pnode->nodes, 1, Pnode *);
  Pnode *exp = NULL;

  if (bo_array_size(pnode->nodes) == 3)
    exp = bo_array_at(pnode->nodes, 2, Pnode *);

  char buf[256];

  if (is_local) {
    sprintf(buf,
            "%*s%s %s;\n",
            nesting,
            "",
            type->tok->content.as_string,
            ident->tok->content.as_string);
    bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_DVAR], &buf[0]);

    if (exp) {
      sprintf(buf,
              "%*s%s = %d;\n",
              nesting,
              "",
              ident->tok->content.as_string,
              exp->tok->content.as_int);

      bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_IVAR], &buf[0]);
    }
  } else {
    sprintf(buf,
            "%*s%s %s %s_%s;\n",
            nesting,
            "",
            "extern",
            type->tok->content.as_string,
            prefix, 
            ident->tok->content.as_string);
    bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_DVAR], &buf[0]);

    if (exp) {
      sprintf(buf,
              "%*s%s %s_%s = %d;\n",
              nesting,
              "",
              type->tok->content.as_string,
              prefix,
              ident->tok->content.as_string,
              exp->tok->content.as_int);

      bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_IVAR], &buf[0]);
    }
  }
}

void
gen_func(Unit *unit,
         Pnode *pnode,
         CSrc *src,
         int nesting,
         const char *prefix)
{
  Pnode *type = bo_array_at(pnode->nodes, 0, Pnode *);
  Pnode *ident = bo_array_at(pnode->nodes, 1, Pnode *);
  Pnode *args = bo_array_at(pnode->nodes, 2, Pnode *);
  Pnode *scope = bo_array_at(pnode->nodes, 3, Pnode *);
  char buf[256];

  sprintf(buf,
          "%s %s_%s(",
          type->tok->content.as_string,
          prefix,
          ident->tok->content.as_string);

  bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_DFUNC], &buf[0]);

  gen_args(unit, args, src, BL_CSRC_IMPL_LAYER_DFUNC);
  bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_DFUNC], ");\n");

  sprintf(buf,
          "%s %s_%s(",
          type->tok->content.as_string,
          prefix,
          ident->tok->content.as_string);

  bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_IFUNC], &buf[0]);
  gen_args(unit, args, src, BL_CSRC_IMPL_LAYER_IFUNC);
  bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_IFUNC], ") {\n");

  CSrc *scope_src = gen_scope(unit, scope, nesting);
  bl_csrc_merge(src, scope_src, BL_CSRC_IMPL_LAYER_IFUNC);
  bo_unref(scope_src);

  bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_IFUNC], "}\n\n");
}

void
gen_call(Unit *unit,
         Pnode *pnode,
         CSrc *src,
         int nesting,
         const char *prefix)
{
  Pnode *ident = bo_array_at(pnode->nodes, 0, Pnode *);
  Pnode *args = bo_array_at(pnode->nodes, 1, Pnode *);

  char buf[256];

  sprintf(buf,
          "%*s%s_%s(",
          nesting, "",
          prefix,
          ident->tok->content.as_string);

  bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_IFUNC], &buf[0]);

  gen_fargs(unit, args, src, BL_CSRC_IMPL_LAYER_IFUNC);
  bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_IFUNC], ");\n");
}

void
gen_fargs(Unit *unit,
         Pnode *pnode,
         CSrc *src,
         bl_csrc_impl_layers l)
{
  size_t c = bo_array_size(pnode->nodes);
  Pnode *child = NULL;
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(pnode->nodes, i, Pnode *);
    gen_farg(unit, child, src, l);

    if (i + 1 < c)
      bo_string_append(src->impl[l], ", ");
  }
}

void
gen_farg(Unit *unit,
        Pnode *pnode,
        CSrc *src,
        bl_csrc_impl_layers l)
{
  char buf[256];
  Pnode *ident = NULL;

  ident = bo_array_at(pnode->nodes, 0, Pnode *);

  sprintf(buf,
          "%s",
          ident->tok->content.as_string);

  bo_string_append(src->impl[l], &buf[0]);
}

void
gen_args(Unit *unit,
         Pnode *pnode,
         CSrc *src,
         bl_csrc_impl_layers l)
{
  size_t c = bo_array_size(pnode->nodes);
  if (c == 0)
    bo_string_append(src->impl[l], "void");

  Pnode *child = NULL;
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(pnode->nodes, i, Pnode *);
    gen_arg(unit, child, src, l);

    if (i + 1 < c)
      bo_string_append(src->impl[l], ", ");
  }
}

void
gen_arg(Unit *unit,
        Pnode *pnode,
        CSrc *src,
        bl_csrc_impl_layers l)
{
  char buf[256];
  Pnode *type = NULL;
  Pnode *ident = NULL;

  type = bo_array_at(pnode->nodes, 0, Pnode *);
  ident = bo_array_at(pnode->nodes, 1, Pnode *);

  sprintf(buf,
          "%s %s",
          type->tok->content.as_string,
          ident->tok->content.as_string);

  bo_string_append(src->impl[l], &buf[0]);
}

CSrc *
gen_scope(Unit *unit,
          Pnode *pnode,
          int nesting)
{
  CSrc *scope_src = bl_csrc_new();
  nesting += 2;
  size_t c = bo_array_size(pnode->nodes);
  Pnode *child = NULL;
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(pnode->nodes, i, Pnode *);
    switch (child->type) {
      case BL_PT_DECL:
        gen_decl(unit, child, scope_src, nesting, true, "");
        break;
      case BL_PT_CALL:
        gen_call(unit, child, scope_src, nesting, "");
        break;
      default:
        /* TODO: handle error */
        break;
    }
  }
  return scope_src;
}

void
gen_nscope(Unit *unit,
           Pnode *pnode,
           CSrc *src,
           int nesting)
{

  Pnode *ident = bo_array_at(pnode->nodes, 0, Pnode*);
  Pnode *nscope = bo_array_at(pnode->nodes, 1, Pnode*);
  const char *prefix = ident->tok->content.as_string;

  Pnode *child = NULL;
  size_t c = bo_array_size(nscope->nodes);
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(nscope->nodes, i, Pnode *);
    switch (child->type) {
      case BL_PT_DECL:
        gen_decl(unit, child, src, nesting, false, prefix);
        break;
      case BL_PT_FUNC:
        gen_func(unit, child, src, 0, prefix);
        break;
      default:
        /* TODO: handle error */
        break;
    }
  }
}

void
gen_gscope(Unit *unit,
           Pnode *pnode,
           CSrc *src)
{
  bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_INCLUDE], header_comment);
  bo_string_append(src->impl[BL_CSRC_IMPL_LAYER_INCLUDE], header_defaults);

  size_t c = bo_array_size(pnode->nodes);
  Pnode *child = NULL;
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(pnode->nodes, i, Pnode *);
    switch (child->type) {
      case BL_PT_NAMESPACE:
        gen_nscope(unit, child, src, 0);
        break;
      case BL_PT_DECL:
        gen_decl(unit, child, src, 0, false, GLOBAL_PREFIX);
        break;
      case BL_PT_FUNC:
        gen_func(unit, child, src, 0, GLOBAL_PREFIX);
        break;
      default:
        /* TODO: handle error */
        break;
    }
  }
}

CSrc *
bl_cgen_generate(Unit *unit,
                 Pnode *pnode)
{
  bl_assert(pnode->type == BL_PT_GSCOPE, "expected global scope\n");
  CSrc *csrc = bl_csrc_new();
  gen_gscope(unit, pnode, csrc);

  return csrc;
}
