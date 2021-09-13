#include "tests.h"

static init_test_empty(void)
{
    TArrayTable *tbl = tatbl_new(sizeof(s32), 0);
    ASSERT(tatbl_size(tbl) == 0);
    tatbl_delete(tbl);
}

static init_test_reserved(void)
{
    TArrayTable *tbl = tatbl_new(sizeof(s32), 256);
    ASSERT(tatbl_size(tbl) == 0);
    tatbl_delete(tbl);
}

static insert(void)
{
    TArrayTable *tbl = tatbl_new(sizeof(s32), 256);

    for (s32 i = 0; i < 1000; ++i) {
        tatbl_insert(tbl, (u64)i, i);
    }
    ASSERT(tatbl_size(tbl) == 1000);
    u64 key;
    s32 value;
    TATBL_FOREACH(s32, tbl, key, value)
    {
        ASSERT(key == value);
    }
    tatbl_clear(tbl);
    ASSERT(tatbl_size(tbl) == 0);
    tatbl_delete(tbl);
}

static find(void)
{
    TArrayTable *tbl = tatbl_new(sizeof(s32), 256);
    for (s32 i = 0; i < 1000; ++i) {
        tatbl_insert(tbl, (u64)i, i);
    }
    ASSERT(tatbl_size(tbl) == 1000);
    TIterator begin = tatbl_begin(tbl);
    TIterator end   = tatbl_end(tbl);
    ASSERT(!TITERATOR_EQUAL(begin, end));
    for (s32 i = 0; i < 1000; ++i) {
        TIterator it = tatbl_find(tbl, (u64)i);
        ASSERT(!TITERATOR_EQUAL(it, end));
        u64 key   = tatbl_iter_peek_key(tbl, it);
        s32 value = tatbl_iter_peek_value(s32, tbl, it);
        ASSERT(key == value);
    }
    tatbl_delete(tbl);
}

int main(void)
{
    init_test_empty();
    init_test_reserved();
    insert();
    find();
    return 0;
}
