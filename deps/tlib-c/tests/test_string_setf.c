#include "tests.h"

int
main(void)
{
	TString *str = tstring_new();

	tstring_setf(str, "I am %s and I am %s years old.", "Travis", "29");
	ASSERT(strcmp(str->data, "I am Travis and I am 29 years old.") == 0);
	tstring_delete(str);
	return 0;
}
