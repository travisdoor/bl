#include <stdio.h>
#include <stdlib.h>
#include "my_object.h"

int main(int argc, const char *argv[]) {
  /* create object parameters passed into constructor */
  MyObjectParams p;
  p.i = 666;

  /* create new object instance on heap */
  MyObject *obj = bo_new(MyObject, &p);
  printf("object (heap) data: %i\n", obj->i);

  /* delete object */
  bo_unref(obj);

  return EXIT_SUCCESS;
}
