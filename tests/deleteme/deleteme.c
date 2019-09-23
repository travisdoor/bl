#include "deleteme.h"
#include <stdio.h>

int
foo(struct Foo foo, int i)
{
	printf("foo.i = %lli, foo.j = %i\n", foo.i, foo.j);
	printf("i = %d\n", i);
	return 10;
}
