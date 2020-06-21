#include "tests.h"

int
main(void)
{
	TList *list = tlist_new(sizeof(s32));
	ASSERT(list->data_size == sizeof(s32));
	ASSERT(list->begin == &list->end);
	ASSERT(list->size == 0);

	for (s32 i = 0; i < 100; ++i) {
		tlist_push_back(list, i);
	}

	ASSERT(list->size == 100);

	s32       visited = 0;
	TIterator it;
	TLIST_FOREACH(list, it)
	{
		++visited;
	}

	ASSERT(visited == 100);

	for (s32 i = 0; i < 100; ++i) {
		tlist_pop_back(list);
	}

	ASSERT(list->size == 0);

	for (s32 i = 0; i < 100; ++i) {
		tlist_push_front(list, i);
	}

	ASSERT(list->size == 100);

	for (s32 i = 0; i < 100; ++i) {
		tlist_pop_front(list);
	}

	ASSERT(list->size == 0);
	ASSERT(tlist_empty(list));

	BENCH_START("List push_back of 100000 elems. ")
	for (s32 i = 0; i < 100000; ++i) {
		tlist_push_back(list, i);
	}
	BENCH_END;

	tlist_delete(list);
	return 0;
}
