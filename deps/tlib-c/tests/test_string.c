#include "tests.h"

int
main(void)
{
	TString *str = tstring_new();
	ASSERT(str->data[0] == '\0');

	tstring_append(str, "hello ");
	tstring_append(str, "world");

	ASSERT(str->len == strlen("hello world"));
	ASSERT(str->data[str->len] == '\0');

	tstring_terminate(str);
	ASSERT(str->len == 0);
	ASSERT(strlen(str->data) == 0);
	
	tstring_delete(str);
	return 0;
}
