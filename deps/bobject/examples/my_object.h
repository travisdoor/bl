#ifndef BOBJECT_EXAMPLES_MY_OBJECT_H
#define BOBJECT_EXAMPLES_MY_OBJECT_H

#include <bobject/bobject.h>

/* class MyObject declaration */
/* This will create new class type table and virtual table */
bo_decl_type_begin(MyObject, BObject)
  /* list your virtual functions here */
bo_end();

/* class MyObject constructor params */
bo_decl_params_begin(MyObject)
  /* constructor params goes here */
  int i;
bo_end();

/* class MyObject object data members */
bo_decl_members_begin(MyObject, BObject)
  /* member data */
  int i;
bo_end();

#endif //BOBJECT_EXAMPLES_MY_OBJECT_H