#ifndef BOBJECT_TEST_MOCK_CHILD
#define BOBJECT_TEST_MOCK_CHILD

#include <bobject/bobject.h>
#include "mock.h"

#ifdef __cplusplus
extern "C" {
#endif

/* class declaration */
bo_decl_type_begin(MockChild, Mock)
  /* virtual methods goes here */
bo_end();

/* class constructor parameters */
bo_decl_params_begin(MockChild)
  /* constructor parameters */
bo_end();

/* class members */
bo_decl_members_begin(MockChild, Mock)
  /* member data goes here */
bo_end();

/* UNIT TEST STATS */
extern int mock_child_ctor_called;
extern int mock_child_dtor_called;
extern int mock_child_init_called;
extern int mock_child_copy_called;
extern int mock_child_method_one_called;

void
mock_child_reset_utest_stats(void);

#ifdef __cplusplus
}
#endif
#endif //BOBJECT_TEST_MOCK_CHILD
