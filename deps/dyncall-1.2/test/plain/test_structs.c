/*

 Package: dyncall
 Library: test
 File: test/plain/test_structs.c
 Description:
 License:

   Copyright (c) 2010-2015 Olivier Chafik <olivier.chafik@gmail.com>
                      2019 Tassilo Philipp <tphilipp@potion-studios.com>

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

*/




#include "../../dyncall/dyncall.h"
#include "../../dyncall/dyncall_signature.h"
#include "../../dyncall/dyncall_struct.h"
#include "../common/platformInit.h"

#define DC_TEST_STRUCT_SIZE(sig, type, s) { \
	DCsize expected = sizeof(type), computed = dcStructSize(s);\
	printf("struct_%s size: expected = %d, computed = %d: %d\n", sig, (int)expected, (int)computed, (expected == computed)); \
	ret = (expected == computed) && ret; \
}

/* @@@ incomplete and should be makde generally available in dyncall once struct support will make it in */
#if defined(DC__OS_Plan9)
#  define DEFAULT_STRUCT_ALIGNMENT 4
#else
#  define DEFAULT_STRUCT_ALIGNMENT DEFAULT_ALIGNMENT
#endif

int testStructSizes()
{
	int ret = 1;

	{
		typedef struct {
			char a, b;
		} S;

		size_t size;
		DCstruct* s = dcNewStruct(2, DEFAULT_STRUCT_ALIGNMENT);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcCloseStruct(s);

		DC_TEST_STRUCT_SIZE("cc", S, s);
		dcFreeStruct(s);
	}
	{
		typedef struct {
			char a, b, c;
		} S;

		size_t size;
		DCstruct* s = dcNewStruct(3, DEFAULT_STRUCT_ALIGNMENT);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcCloseStruct(s);

		DC_TEST_STRUCT_SIZE("ccc", S, s);
		dcFreeStruct(s);
	}
	{
		typedef struct {
			char a;
			short b;
		} S;

		size_t size;
		DCstruct* s = dcNewStruct(2, DEFAULT_STRUCT_ALIGNMENT);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_SHORT, DEFAULT_ALIGNMENT, 1);
		dcCloseStruct(s);

		DC_TEST_STRUCT_SIZE("cs", S, s);
		dcFreeStruct(s);
	}
	{
		typedef struct {
			double a, b, c, d;
		} S;

		size_t size;
		DCstruct* s = dcNewStruct(4, DEFAULT_STRUCT_ALIGNMENT);
		dcStructField(s, DC_SIGCHAR_DOUBLE, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_DOUBLE, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_DOUBLE, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_DOUBLE, DEFAULT_ALIGNMENT, 1);
		dcCloseStruct(s);

		DC_TEST_STRUCT_SIZE("dddd", S, s);
		dcFreeStruct(s);
	}
	{
		typedef struct {
			char a, b;
			void* p[3];
		} S;

		size_t size;
		DCstruct* s = dcNewStruct(3, DEFAULT_STRUCT_ALIGNMENT);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_POINTER, DEFAULT_ALIGNMENT, 3);
		dcCloseStruct(s);

		DC_TEST_STRUCT_SIZE("cc[ppp]", S, s);
		dcFreeStruct(s);
	}
	{
		typedef struct {
			short a;
			struct {
			char a, b;
			void* p[3];
			} sub;
			short b;
		} S;
	 	
		size_t size;
		DCstruct* s = dcNewStruct(3, DEFAULT_STRUCT_ALIGNMENT);
		dcStructField(s, DC_SIGCHAR_SHORT, DEFAULT_ALIGNMENT, 1);
		dcSubStruct(s, 3, DEFAULT_STRUCT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_POINTER, DEFAULT_ALIGNMENT, 3);
		dcCloseStruct(s);
		dcStructField(s, DC_SIGCHAR_SHORT, DEFAULT_ALIGNMENT, 1);
		dcCloseStruct(s);

		DC_TEST_STRUCT_SIZE("s{cc[ppp]}s", S, s);
		dcFreeStruct(s);
	}

#define TEST_MONO_STRUCT(sig, type, sigchar) \
	{ \
		typedef struct { \
			type v; \
		} S; \
		 \
		DCstruct* s = dcNewStruct(1, DEFAULT_STRUCT_ALIGNMENT); \
		dcStructField(s, sigchar, DEFAULT_ALIGNMENT, 1); \
		dcCloseStruct(s); \
		 \
		DC_TEST_STRUCT_SIZE(sig, S, s); \
		dcFreeStruct(s); \
	}

	TEST_MONO_STRUCT("c", char,               DC_SIGCHAR_CHAR);      // 4 on plan 9 |
	TEST_MONO_STRUCT("C", unsigned char,      DC_SIGCHAR_UCHAR);     // 4 on plan 9 |
	TEST_MONO_STRUCT("s", short,              DC_SIGCHAR_SHORT);     // 4 on plan 9 |  minimal size of a struct, period?
	TEST_MONO_STRUCT("S", unsigned short,     DC_SIGCHAR_USHORT);    // 4 on plan 9 |
	TEST_MONO_STRUCT("i", int,                DC_SIGCHAR_INT);
	TEST_MONO_STRUCT("I", unsigned int,       DC_SIGCHAR_UINT);
	TEST_MONO_STRUCT("j", long,               DC_SIGCHAR_LONG);
	TEST_MONO_STRUCT("J", unsigned long,      DC_SIGCHAR_ULONG);
	TEST_MONO_STRUCT("l", long long,          DC_SIGCHAR_LONGLONG);
	TEST_MONO_STRUCT("L", unsigned long long, DC_SIGCHAR_ULONGLONG);
	TEST_MONO_STRUCT("p", void*,              DC_SIGCHAR_POINTER);
	TEST_MONO_STRUCT("f", float,              DC_SIGCHAR_FLOAT);
	TEST_MONO_STRUCT("d", double,             DC_SIGCHAR_DOUBLE);

	return ret;
}



typedef struct
{
	char a, b, c;
} FewValues;

double sum_FewValues(FewValues values)
{
	printf("sum_FewValues(a = %d, b = %d, c = %d)\n", (int)values.a, (int)values.b, (int)values.c);
	return ((double)values.a) + ((double)values.b) + ((double)values.c);
}


typedef struct
{
	char a, b;
	double p[10];
} SomeValues;

double sum_SomeValues(SomeValues values)
{
	return ((double)values.a) + ((double)values.b) + values.p[0] + values.p[1] + values.p[2];
}


/*int testCallStructs()
{
	int ret = 1;

	DCCallVM* pc = dcNewCallVM(4096);
	{
		FewValues values;
		double calledSum, expectedSum;
		DCstruct* s = dcNewStruct(3, DEFAULT_STRUCT_ALIGNMENT);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcCloseStruct(s);

		DC_TEST_STRUCT_SIZE("ccc", FewValues, s);

		values.a = 1;
		values.b = 2;
		values.c = 3;

		dcMode(pc, DC_CALL_C_DEFAULT);
		dcReset(pc);
		printf("BEFORE dcArgStruct\n");
		dcArgStruct(pc, s, &values);
		printf("AFTER dcArgStruct\n");
		calledSum = dcCallDouble(pc, (DCpointer)&sum_FewValues);
		expectedSum = sum_FewValues(values);

		DC_TEST_INT_EQUAL(expectedSum, calledSum);
		dcFreeStruct(s);
	}
	{
		SomeValues values;
		double calledSum, expectedSum;
		DCstruct* s = dcNewStruct(3, DEFAULT_STRUCT_ALIGNMENT);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_CHAR, DEFAULT_ALIGNMENT, 1);
		dcStructField(s, DC_SIGCHAR_DOUBLE, DEFAULT_ALIGNMENT, 10);
		dcCloseStruct(s);

		DC_TEST_STRUCT_SIZE("ccd", SomeValues, s);

		values.a = 1;
		values.b = 2;
		values.p[0] = 10;
		values.p[1] = 11;
		values.p[2] = 12;

		dcMode(pc, DC_CALL_C_DEFAULT);
		dcReset(pc);
		dcArgStruct(pc, s, &values);
		calledSum = dcCallDouble(pc, (DCpointer) &sum_SomeValues);
		expectedSum = sum_SomeValues(values);

		DC_TEST_INT_EQUAL(expectedSum, calledSum);
		dcFreeStruct(s);
	}

	dcFree(pc);

	return ret;
}*/

