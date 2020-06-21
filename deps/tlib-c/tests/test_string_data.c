#include "tests.h"

int
main(void)
{
	TString *str = tstring_new();

	for (int i = 0; i < 1000; ++i) {
		tstring_append(str, "hello");
	}

	ASSERT(str->len == 1000 * strlen("hello"));
	ASSERT(strlen(str->data) == 1000 * strlen("hello"));

	tstring_clear(str);

	ASSERT(str->len == 0);
	ASSERT(strlen(str->data) == 0);

	tstring_delete(str);
	return 0;
}
