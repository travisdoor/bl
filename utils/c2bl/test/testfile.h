#include "nested/testfile2.h"
#define HOMO 1


int foo(int number, char c);

const char *hello(void);

void homo();

void homo2(void *homo);

typedef void (*printer_t)(int number);
typedef printer_t my_printer_t, his_printer_t;

enum MyEnum {
    A, B
};

typedef enum MyEnum HisEnum, HisSecondEnum;

void use_enum(enum MyEnum foo);

typedef enum {
    C, D
} Foo, Foo2, Foo3;

typedef Foo3 Homo3;

typedef enum AnotherOne Bar;
enum AnotherOne {
    E, F
};


struct S_One {
    int i;
    const char *str;
};

typedef struct S_One S_Two, S_Three;

typedef struct S_Four {
    int i;
    const char *str;
} S_Four, S_Five, S_Six;

void cool(S_Two data);

typedef union FooFoo {
    int i;
    char c;
} FooFoo2;


void get_module(MyRef m);
