//*****************************************************************************
// Biscuit Engine
//
// File:   parser.c
// Author: Martin Dorazil
// Date:   03/02/2018
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

#include "parser.h"

/* Parser members */
bo_decl_members_begin(Parser, BObject)
  Lexer *lexer;
bo_end();

/* Parser constructor parameters */
bo_decl_params_begin(Parser)
  Lexer *lexer;
bo_end();

bo_impl_type(Parser, BObject);

/* Parser class init */
void
ParserKlass_init(ParserKlass *klass)
{
}

/* Parser constructor */
void
Parser_ctor(Parser *self, ParserParams *p)
{
  self->lexer = bo_ref(p->lexer);
}

/* Parser destructor */
void
Parser_dtor(Parser *self)
{
  bo_unref(self->lexer);
}

/* Parser copy constructor */
bo_copy_result
Parser_copy(Parser *self, Parser *other)
{
  return BO_NO_COPY;
}

/* public */
Parser *
bl_parser_new(Lexer *lexer)
{
  ParserParams p = {
    .lexer = lexer 
  };
  
  return bo_new(Parser, &p);
}

/* public */
void
bl_parser_scan(Parser *self)
{
}
