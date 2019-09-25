#ifndef DELETEME_H
#define DELETEME_H

#include "deleteme_global.h"

struct Foo {
	long j;
	long i;
	long k;
};


DELETEME_EXPORT struct Foo foo(struct Foo foo, int i);
DELETEME_EXPORT struct Foo bar(void);

#endif // DELETEME_H
