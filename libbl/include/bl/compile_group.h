//*****************************************************************************
// blc
//
// File:   compile_group.h
// Author: Martin Dorazil
// Date:   09/02/2018
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

#ifndef BL_COMPILE_GROUP_H
#define BL_COMPILE_GROUP_H

typedef enum _bl_compile_group
{
  BL_CGROUP_PRE_ANALYZE = 0,
  BL_CGROUP_ANALYZE,
  BL_CGROUP_GENERATE,
  BL_CGROUP_POST_GENERATE,
  BL_CGROUP_ASSEMBLY_BUILD,
  BL_CGROUP_COUNT
} bl_compile_group_e;

#endif //BL_COMPILE_GROUP_H
