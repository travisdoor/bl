#include <stdio.h>
#include "mock_child.h"

/* virtuals */
static void
method_one(MockChild *self);

/* class MockChild */
bo_impl_type(MockChild, Mock);

/* MockChild type initialization (static constructor) */
void
MockChildKlass_init(MockChildKlass *klass)
{
  bo_vtbl_cl(klass, Mock)->method_one = (void (*)(Mock *)) method_one;
  ++mock_child_init_called;
}

/* MockChild constructor */
void
MockChild_ctor(MockChild *self, MockChildParams *p)
{
  bo_parent_ctor(Mock, (&(MockParams) {.first = 1, .second = 2, .third = 3}));
  ++mock_child_ctor_called;
}

/* MockChild destructor */
void
MockChild_dtor(MockChild *self)
{
  ++mock_child_dtor_called;
}

bo_copy_result
MockChild_copy(MockChild *self, MockChild *other)
{
  ++mock_child_copy_called;
  return BO_COPY_DEFAULT;
}

/* class MockChild end */

void
method_one(MockChild *self)
{
  ++mock_child_method_one_called;
}

/* UNIT TEST STATS */
int mock_child_ctor_called = 0;
int mock_child_dtor_called = 0;
int mock_child_init_called = 0;
int mock_child_copy_called = 0;
int mock_child_method_one_called = 0;

void
mock_child_reset_utest_stats(void)
{
  mock_child_ctor_called = 0;
  mock_child_dtor_called = 0;
  mock_child_init_called = 0;
  mock_child_copy_called = 0;
  mock_child_method_one_called = 0;
}
