#include <stdio.h>
#include <assert.h>

void my_function(int a, int b, int c, int d, int e, int f) {
	printf("a = %d\nb = %d\nc = %d\nd = %d\ne = %d\nf = %d\n", a, b, c, d, e, f);
	assert(a == 10);
	assert(b == 20);
	assert(c == 30);
	assert(d == 40);
	assert(e == 50);
	assert(f == 60);
}

int add_numbers(int a, int b) {
	return a + b;
}

void print_number(int n) {
	printf("Number is: %d\n", n);
}