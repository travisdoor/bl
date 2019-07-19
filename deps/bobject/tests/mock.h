#ifndef BOBJECT_TEST_MOCK
#define BOBJECT_TEST_MOCK

#include <bobject/bobject.h>

BO_BEGIN_DECLS

/* class declaration */
bo_decl_type_begin(Mock, BObject)
  /* virtual methods goes here */
  void
  (*method_one)(Mock *);

  void
  (*method_two)(Mock *);
bo_end();

/* class constructor parameters */
bo_decl_params_begin(Mock)
  /* constructor parameters */
  int first;
  int second;
  int third;
  int data;
bo_end();

/* class members */
bo_decl_members_begin(Mock, BObject)
  /* member data goes here */
  int first;
  int second;
  int third;
  int data;
bo_end();

/* UNIT TEST STATS */
extern int mock_ctor_called;
extern int mock_dtor_called;
extern int mock_init_called;
extern int mock_copy_called;
extern int mock_method_one_called;
extern int mock_method_two_called;

void
mock_reset_utest_stats(void);

BO_END_DECLS

#endif //BOBJECT_TEST_MOCK
