#include "mock.h"
#include <assert.h>
#include <stdio.h>

bo_impl_type(Mock, BObject);

/* virtuals */
static void
method_one(Mock *self);

static void
method_two(Mock *self);

/* class Mock */
void
MockKlass_init(MockKlass *klass)
{
  bo_vtbl_cl(klass, Mock)->method_one = method_one;
  bo_vtbl_cl(klass, Mock)->method_two = method_two;
  ++mock_init_called;
}

void
Mock_ctor(Mock *self, MockParams *p)
{
  assert(p);
  bo_parent_ctor(BObject, p);
  self->first = p->first;
  self->second = p->second;
  self->third = p->third;
  self->data = p->data;

  ++mock_ctor_called;
}

void
Mock_dtor(Mock *self)
{
  ++mock_dtor_called;
}

bo_copy_result
Mock_copy(Mock *self, Mock *other)
{
  ++mock_copy_called;
  return BO_COPY_DEFAULT;
}
/* class Mock end */

void
method_one(Mock *self)
{
  ++mock_method_one_called;
}

void
method_two(Mock *self)
{
  ++mock_method_two_called;
}

/* UNIT TEST STATS */
int mock_ctor_called = 0;
int mock_dtor_called = 0;
int mock_init_called = 0;
int mock_copy_called = 0;
int mock_method_one_called = 0;
int mock_method_two_called = 0;

void
mock_reset_utest_stats(void)
{
  mock_ctor_called = 0;
  mock_dtor_called = 0;
  mock_init_called = 0;
  mock_copy_called = 0;
  mock_method_one_called = 0;
  mock_method_two_called = 0;
}
