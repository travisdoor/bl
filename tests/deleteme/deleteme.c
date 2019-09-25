#include "deleteme.h"

struct Foo
foo(struct Foo foo, int i)
{
  struct Foo tmp = bar();
  return tmp;
}

struct Foo
bar(void) {
  return (struct Foo){10};
};
