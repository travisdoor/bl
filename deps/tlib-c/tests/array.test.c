#include "tests.h"

static void init_test(void)
{
    TArray *arr = tarray_new(sizeof(s32));
    ASSERT(arr);
    ASSERT(arr->elem_size == sizeof(s32));

    tarray_terminate(arr);
    ASSERT(arr->elem_size == sizeof(s32));
    ASSERT(arr->size == 0);

    tarray_delete(arr);
}

static void s32_test(void)
{
    TArray *arr = tarray_new(sizeof(s32));

    BENCH_START("Array push_back of 100000 elems. ")
    for (s32 i = 0; i < 100000; ++i) {
        tarray_push(arr, i);
    }
    BENCH_END;

    ASSERT(arr->size == 100000);

    s32 it;
    TARRAY_FOREACH(s32, arr, it)
    {
        ASSERT(it == i);
    }

    tarray_delete(arr);
}

static void s32_ptr_test(void)
{
    s32     tmp[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    TArray *arr   = tarray_new(sizeof(s32 *));

    for (s32 i = 0; i < 10; ++i) {
        s32 *v = &tmp[i];
        tarray_push(arr, v);
    }

    ASSERT(arr->size == 10);

    s32 *it;
    TARRAY_FOREACH(s32 *, arr, it)
    {
        ASSERT(*it == i);
    }

    tarray_delete(arr);
}

static void struct_test(void)
{
    struct Foo {
        s32 i, j, k, l;
    };

    TArray *arr = tarray_new(sizeof(struct Foo));

    for (s32 i = 0; i < 100; ++i) {
        struct Foo v = (struct Foo){.i = i, .j = i + 1, .k = i + 2, .l = i + 3};
        tarray_push(arr, v);
    }

    ASSERT(arr->size == 100);

    for (s32 i = 0; i < 100; ++i) {
        ASSERT(tarray_at(struct Foo, arr, i).i == i);
        ASSERT(tarray_at(struct Foo, arr, i).j == i + 1);
        ASSERT(tarray_at(struct Foo, arr, i).k == i + 2);
        ASSERT(tarray_at(struct Foo, arr, i).l == i + 3);
    }

    tarray_delete(arr);
}

static void front_back_test(void)
{
    s32     front = 666;
    s32     back  = 777;
    TArray *arr   = tarray_new(sizeof(s32));
    tarray_push(arr, front);
    ASSERT(tarray_front(s32, arr) == tarray_back(s32, arr));
    tarray_push(arr, back);
    ASSERT(tarray_front(s32, arr) != tarray_back(s32, arr));
    ASSERT(tarray_front(s32, arr) == 666);
    ASSERT(tarray_back(s32, arr) == 777);
    tarray_delete(arr);
}

static void erase_test(void)
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
}

int main(void)
{
    init_test();
    s32_test();
    s32_ptr_test();
    struct_test();
    front_back_test();
    erase_test();
    return 0;
}
