#include "tests.h"

int
main(void)
{
	TArray *arr = tarray_new(sizeof(s32));
	ASSERT(arr->elem_size == sizeof(s32));

	tarray_reserve(arr, 128);
	ASSERT(arr->allocated == 128);

	tarray_terminate(arr);
	ASSERT(arr->elem_size == sizeof(s32));
	ASSERT(arr->size == 0);
	ASSERT(arr->allocated == 0);

	tarray_delete(arr);
	return 0;
}
