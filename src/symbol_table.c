//*****************************************************************************
// bl 
//
// File:   symbol_table.c
// Author: Martin Dorazil
// Date:   01/02/2018
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
//*****************************************************************************

#include <bobject/containers/htbl.h>
#include <bobject/containers/hash.h>
#include "symbol_table.h"
#include "bldebug.h"

/* SymbolTable members */
bo_decl_members_begin(SymbolTable, BObject)
  BHashTable *symbols;
bo_end();

/* SymbolTable constructor parameters */
bo_decl_params_begin(SymbolTable)
bo_end();

bo_impl_type(SymbolTable, BObject);

/* SymbolTable class init */
void
SymbolTableKlass_init(SymbolTableKlass *klass)
{
}

/* SymbolTable constructor */
void
SymbolTable_ctor(SymbolTable *self, SymbolTableParams *p)
{
  self->symbols = bo_htbl_new_bo(bo_typeof(PNode), false, 512);
}

/* SymbolTable destructor */
void
SymbolTable_dtor(SymbolTable *self)
{
  bo_unref(self->symbols);
}

/* SymbolTable copy constructor */
bo_copy_result
SymbolTable_copy(SymbolTable *self, SymbolTable *other)
{
  return BO_NO_COPY;
}

/* public */
SymbolTable *
bl_symbol_table_new(void)
{
  return bo_new(SymbolTable, NULL);
}

bool
bl_symbol_table_geristrate(SymbolTable *self,
                           PNode       *node)
{
  const char *id = node->tok->content.as_string;
  uint64_t hash = bo_hash_from_str(id);
  if (bo_htbl_has_key(self->symbols, hash))
    return false;

  bo_htbl_insert(self->symbols, hash, node);
  return true;
}

PNode *
bl_symbol_table_get(SymbolTable *self,
                    const char  *id)
{
  uint64_t hash = bo_hash_from_str(id);
  bo_iterator_t found = bo_htbl_find(self->symbols, hash); 
  bo_iterator_t end = bo_htbl_end(self->symbols);
  if (bo_iterator_equal(&found, &end))
    return NULL;

  return bo_htbl_iter_peek_value(self->symbols, &found, PNode *);
}

void
bl_symbol_table_print(SymbolTable *self,
                      FILE        *out)
{
  bo_iterator_t iter = bo_htbl_begin(self->symbols); 
  bo_iterator_t end = bo_htbl_end(self->symbols);
  PNode *node = NULL;
  while (!bo_iterator_equal(&iter, &end)) {
    node = bo_htbl_iter_peek_value(self->symbols, &iter, PNode *); 
    fprintf(out, "symbol %s\n", node->tok->content.as_string);
    bo_htbl_iter_next(self->symbols, &iter);
  }
}

