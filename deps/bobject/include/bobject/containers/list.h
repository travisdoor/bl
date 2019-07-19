//*****************************************************************************
// Biscuit Object
//
// File:   list.h
// Author: Martin Dorazil
// Date:   04/12/2017
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

#ifndef BOBJECT_LIST_H
#define BOBJECT_LIST_H

#include <bobject/bobject.h>
#include "bobject/utils.h"
#include "bobject/containers/iterator.h"

BO_BEGIN_DECLS

/* class BList declaration */
bo_decl_type_begin(BList, BObject)
  /* virtual functions */
bo_end();

extern BO_EXPORT BList *
bo_list_new(size_t data_size);

extern BO_EXPORT BList *
bo_list_new_bo(BType type,
               bool  managed);

#define bo_list_push_back(self, data) \
  _bo_list_push_back((self), &(data))

extern BO_EXPORT void
_bo_list_push_back(BList *self,
                   void  *data);

extern BO_EXPORT void
bo_list_pop_back(BList *self);

#define bo_list_push_front(self, data) \
  _bo_list_push_front((self), &(data))

extern BO_EXPORT void
bo_list_pop_front(BList *self);

extern BO_EXPORT void
_bo_list_push_front(BList *self,
                   void  *data);

extern BO_EXPORT void
bo_list_erase(BList         *self,
              bo_iterator_t *iter);

extern BO_EXPORT void
bo_list_iter_next(BList         *self,
                  bo_iterator_t *iter);

#define bo_list_iter_peek(self, iter, type) \
  (*(type *)_bo_list_iter_peek((self), (iter)))

extern BO_EXPORT void *
_bo_list_iter_peek(BList         *self,
                   bo_iterator_t *iter);

extern BO_EXPORT void
_bo_list_pop_back(BList *self);

extern BO_EXPORT bo_iterator_t
bo_list_begin(BList *self);

extern BO_EXPORT bo_iterator_t
bo_list_end(BList *self);

#define bo_list_front(self, type) \
  (*(type *)_bo_list_front((self)))

extern BO_EXPORT void*
_bo_list_front(BList *self);

#define bo_list_back(self, type) \
  (*(type *)_bo_list_back((self)))

extern BO_EXPORT void*
_bo_list_back(BList *self);

extern BO_EXPORT bool
bo_list_empty(BList *self);

extern BO_EXPORT void
bo_list_clear(BList *self);

extern BO_EXPORT size_t
bo_list_size(BList *self);

extern BO_EXPORT size_t
bo_list_data_size(BList *self);

extern BO_EXPORT bool
bo_list_managed(BList *self);

BO_END_DECLS
#endif //BOBJECT_LIST_H
