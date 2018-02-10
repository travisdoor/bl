//*****************************************************************************
// bl 
//
// File:   node_global_stmt.h
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

#ifndef BISCUIT_NODE_GLOBAL_STMT_H
#define BISCUIT_NODE_GLOBAL_STMT_H

#include "bl/ast/node_global_stmt.h"
#include "node_impl.h"
#include "bl/token.h"

/* NodeGlobalStmt members */
bo_decl_members_begin(NodeGlobalStmt, Node)
  BArray *nodes;
bo_end();

/* NodeGlobalStmt constructor parameters */
bo_decl_params_with_base_begin(NodeGlobalStmt, Node)
bo_end();

#endif /* end of include guard: BISCUIT_NODE_GLOBAL_STMT_H */

