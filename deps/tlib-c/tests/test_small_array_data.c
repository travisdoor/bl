#include "tests.h"

TSMALL_ARRAY_TYPE(s32, s32, 16)

int
main(void)
{
	TSmallArray_s32 arr;
	tsa_init(&arr);

	ASSERT(arr.size == 0);
	ASSERT(arr.data);

	for (s32 i = 0; i < 256; ++i) {
		tsa_push_s32(&arr, i);
	}

	ASSERT(arr.size == 256);
	ASSERT(arr.allocated > 0);

	for (s32 i = 0; i < 256; ++i) {
	  ASSERT(arr.data[i] == i);
	}

	for (s32 i = 0; i < 256; ++i) {
		tsa_pop_s32(&arr);
	}

	ASSERT(arr.size == 0);

	tsa_terminate(&arr);
	return 0;
}
