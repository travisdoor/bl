#include "deleteme.h"

void
foo(struct Foo foo, int i)
{
  struct Foo tmp = bar();
  tmp.i = tmp.j;
}

struct Foo
bar(void) {
  return (struct Foo){10, 20, 30};
};
