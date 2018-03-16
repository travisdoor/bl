//*****************************************************************************
// blc
//
// File:   type.c
// Author: Martin Dorazil
// Date:   11/02/2018
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

#include <bobject/containers/hash.h>
#include <string.h>
#include "type_impl.h"
#include "ast/node_impl.h"

static const char *bl_type_strings[] = {
#define tp(tok, str) str,
    BL_TYPE_LIST
#undef tp
};

/* public */
void
bl_type_init(bl_type_t *type, const char *name)
{
  type->name = name;

  for (uint32_t i = 0; i < BL_TYPE_COUNT; i++) {
    if (strcmp(bl_type_strings[i], name) == 0) {
      type->hash = i;
      break;
    }
  }

  if (type->hash == BL_TYPE_NONE) {
    type->hash = bo_hash_from_str(name);
  }
}

bool
bl_type_is_fundamental(bl_type_t *type)
{
  return type->hash < BL_TYPE_COUNT;
}

bool
bl_type_is_user_defined(bl_type_t *type)
{
  return !bl_type_is_fundamental(type);
}
