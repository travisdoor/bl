//*****************************************************************************
// bl
//
// File:   lexer_test.cpp
// Author: Martin Dorazil
// Date:   8.2.18
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

#include <gtest/gtest.h>
#include "bl/bl.h"

const char *src =
  "// \n"
  "identifier "
  "\"string\" "
  "0123456789 "
  "return "
  "if "
  "else "
  "extern "
  "namespace "
  "class "
  "struct "
  "{}[](),;=/";

class LexerTest : public ::testing::Test
{
protected:
  void
  SetUp()
  {
    module = (Actor *)bl_module_new();
    pipeline = bl_pipeline_new();

    auto *lexer = (Stage *)bl_lexer_new();
    bl_pipeline_add_stage(pipeline, lexer);
  }

  void
  TearDown()
  {
    bo_unref(module);
    bo_unref(pipeline);
  }

  Pipeline *pipeline;
  Actor *module;
};

TEST_F(LexerTest, symbol_parsing)
{
  auto *unit = (Actor *) bl_unit_new_str("_test_", src);
  bl_actor_add(module, unit);

  ASSERT_TRUE(bl_pipeline_run(pipeline, module));

  Tokens *tokens = bl_unit_tokens((Unit *)unit);
  bl_token_t *t;

  t = bl_tokens_consume(tokens);
  ASSERT_EQ(t->sym, BL_SYM_IDENT);

  t = bl_tokens_consume(tokens);
  ASSERT_EQ(t->sym, BL_SYM_STRING);
  ASSERT_FALSE(strncmp(t->content.as_string, "string", 6));

  t = bl_tokens_consume(tokens);
  ASSERT_EQ(t->sym, BL_SYM_NUM);
  ASSERT_EQ(t->content.as_int, 123456789);

  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_RETURN);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_IF);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_ELSE);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_EXTERN);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_NAMESPACE);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_CLASS);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_STRUCT);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_LBLOCK);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_RBLOCK);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_LBRACKET);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_RBRACKET);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_LPAREN);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_RPAREN);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_COMMA);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_SEMICOLON);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_ASIGN);
  ASSERT_EQ(bl_tokens_consume(tokens)->sym, BL_SYM_SLASH);
}

