#include <stdio.h>
#include "my_object.h"

/* class MyObject */
bo_impl_type(MyObject, BObject);

void
MyObjectKlass_init(MyObjectKlass *klass)
{
  /*
   * Class initialization is called once per type when it is needed
   * by derived type or this type when bo_new or bo_init is called.
   * Override virtual functions here.
   */
  printf("class init\n");
}

void
MyObject_ctor(MyObject *self, MyObjectParams *p)
{
  /*
   * Constructor
   *
   * Constructor is called for every new object. Set your object
   * data here.
   */

  self->i = p->i;
  printf("construction\n");
}

void
MyObject_dtor(MyObject *self)
{
  /*
   * Destructor
   *
   * Destructor is called when object is going to be destroyed.
   * Free all allocated resources here.
   */
  self->i = 0;
  printf("destruction\n");
}

bo_copy_result
MyObject_copy(MyObject *self, MyObject *other)
{
  /*
   * Copy is called when we're creating this object from another
   * with bo_duplicate method.
   *
   * Possible return values:
   *  BO_COPY_SUCCESS - copying is done without error
   *  BO_COPY_FAIL    - on error
   *  BO_COPY_DEFAULT - memcpy is used by default for copying other data to self
   *  BO_NO_COPY      - object is non-copyable
   */
  return BO_COPY_DEFAULT;
}
/* class MyObject end */