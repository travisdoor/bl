#include "tlib/tlib.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define ASSERT(e)                                                                                  \
	if (!(e)) {                                                                                \
		fprintf(stderr, "Assertion failed on line %d: '" #e "'\n", __LINE__);              \
		abort();                                                                           \
	}

#define BENCH_START(_MSG)                                                                          \
	{                                                                                          \
		clock_t     _start = clock();                                                      \
		const char *_msg   = _MSG " executed in: %fms.";
#define BENCH_END                                                                                  \
	clock_t _end = clock();                                                                    \
	fprintf(stdout, _msg, ((double)(_end - _start)) / CLOCKS_PER_SEC * 1000.f);                \
	}
