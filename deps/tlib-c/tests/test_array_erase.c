#include "tests.h"

int
main(void)
{
	TArray *arr = tarray_new(sizeof(s32));

	for (s32 i = 0; i < 100; ++i) {
		tarray_push(arr, i);
	}

	ASSERT(arr->size == 100);

	for (s32 i = 0; i < 100; ++i) {
		tarray_pop(arr);
	}

	ASSERT(arr->size == 0);

	for (s32 i = 0; i < 100; ++i) {
		tarray_push(arr, i);
	}

	tarray_erase(arr, 99);
	ASSERT(arr->size == 99);

	tarray_erase(arr, 0);
	ASSERT(arr->size == 98);

	tarray_erase(arr, 10);
	ASSERT(arr->size == 97);

	tarray_delete(arr);
	return 0;
}
