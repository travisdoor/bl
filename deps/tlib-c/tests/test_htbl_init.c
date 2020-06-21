#include "tests.h"

int
main(void)
{
	THashTable *tbl = thtbl_new(sizeof(s32), 256);
	ASSERT(tbl->data_size == sizeof(s32));
	ASSERT(tbl->size == 0);
	ASSERT(&tbl->end == tbl->begin);
	ASSERT(TITERATOR_EQUAL(thtbl_begin(tbl), thtbl_end(tbl)));

	thtbl_delete(tbl);
	return 0;
}
