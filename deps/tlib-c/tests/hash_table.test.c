#include "tests.h"

static init_test(void)
{
    THashTable *tbl = thtbl_new(sizeof(s32), 256);
    ASSERT(tbl->data_size == sizeof(s32));
    ASSERT(tbl->size == 0);
    ASSERT(&tbl->end == tbl->begin);
    ASSERT(TITERATOR_EQUAL(thtbl_begin(tbl), thtbl_end(tbl)));

    thtbl_delete(tbl);
}

static data_test(void)
{
    THashTable *tbl = thtbl_new(sizeof(s32), 256);
    for (s32 i = 0; i < 1024; ++i) {
        thtbl_insert(tbl, i, i);
    }

    ASSERT(tbl->size == 1024);
    ASSERT(!TITERATOR_EQUAL(thtbl_begin(tbl), thtbl_end(tbl)));

    TIterator it;
    s32       i = 0;
    THTBL_FOREACH(tbl, it)
    {
        ASSERT(thtbl_has_key(tbl, i++));
        ASSERT(thtbl_iter_peek_value(s32, it) == thtbl_iter_peek_key(it));
    }

    it = thtbl_find(tbl, 66);
    ASSERT(thtbl_iter_peek_value(s32, it) == thtbl_iter_peek_key(it));

    thtbl_erase(tbl, it);
    ASSERT(!thtbl_has_key(tbl, 66));

    thtbl_erase_key(tbl, 10);
    ASSERT(!thtbl_has_key(tbl, 10));

    thtbl_clear(tbl);
    ASSERT(tbl->size == 0);

    thtbl_delete(tbl);
}

int main(void)
{
    init_test();
    data_test();
    return 0;
}
