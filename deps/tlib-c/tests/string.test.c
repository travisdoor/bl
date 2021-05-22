#include "tests.h"

static void init_test(void)
{
    TString *str = tstring_new();
    ASSERT(str->data[0] == '\0');

    tstring_append(str, "hello ");
    tstring_append(str, "world");

    ASSERT(str->len == strlen("hello world"));
    ASSERT(str->data[str->len] == '\0');

    tstring_terminate(str);
    ASSERT(str->len == 0);
    ASSERT(strlen(str->data) == 0);

    tstring_delete(str);
}

static void data_test(void)
{
    TString *str = tstring_new();
    for (int i = 0; i < 1000; ++i) {
        tstring_append(str, "hello");
    }
    ASSERT(str->len == 1000 * strlen("hello"));
    ASSERT(strlen(str->data) == 1000 * strlen("hello"));
    tstring_clear(str);
    ASSERT(str->len == 0);
    ASSERT(strlen(str->data) == 0);
    tstring_delete(str);
}

static void setf_test(void)
{
    TString *str = tstring_new();
    tstring_setf(str, "I am %s and I am %s years old.", "Travis", "29");
    ASSERT(strcmp(str->data, "I am Travis and I am 29 years old.") == 0);
    tstring_delete(str);
}


s32 main(void) {
    init_test();
    data_test();
    setf_test();
    return 0;
}