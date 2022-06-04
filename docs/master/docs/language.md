# Language Reference

Basically, every construct in bl follows the same rules of declaration syntax. We define the name of the
entity, type and optionally some initial value. Name can usually be used to reference the entity
later in the code and type describes layout of data represented by the entity. It could be a number, 
text or more complex types.

Possible declarations:

```text
<name>: <type>;              // mutable declaration
<name>: [type] = <value>;    // mutable declaration
<name>: [type] : <value>;    // immutable declaration (value can be set only once)
```

```c
foo: s32;                // integer variable without initial value
name: string_view = "Martin"; // string variable
name: string_view : "Martin"; // string constant
```

When we decide to explicitly specify initial value, data type can be inferred from this value. In
such case, the type is optional.

```c
name := "Martin"; // string variable
name :: "Martin"; // string constant
```

Comment lines will be ignored by compiler.

```c
// this is line comment
/*
 this
 is
 multi line
 comment
*/
```

## Basic data types

Basic types are atomic basic types builtin into BL compiler.

| Name        | Description                   |
|-------------|-------------------------------|
| s8          | Signed 8-bit number.          |
| s16         | Signed 16-bit number.         |
| s32         | Signed 32-bit number.         |
| s64         | Signed 64-bit number.         |
| u8          | Unsigned 8-bit number.        |
| u16         | Unsigned 16-bit number.       |
| u32         | Unsigned 32-bit number.       |
| u64         | Unsigned 64-bit number.       |
| usize       | Unsigned 64-bit size.         |
| bool        | Boolean. (true/false)         |
| f32         | 32-bit floating point number. |
| f64         | 64-bit floating point number. |
| string_view | String slice.                 |

## Pointer

Represents the address of some allocated data.

**Example:**

```c
#import "std/test"

pointers :: fn () #test {
    i := 666;
    i_ptr : *s32 = &i; // taking the address of 'i' variable and set 'i_ptr'
    j := @i_ptr;       // pointer dereferencing

    test_true(j == i);
};
```

## Array

The array is an aggregate type of multiple values of the same type. Size value must be known in compile
time. Arrays can be inline initialized with compound block; type is required. Zero initializers can
be used for zero initializations of whole array storage, otherwise we must specify value for every
element in an array.

**Example:**

```c
array_type :: fn () #test {
    arr1 : [10] s32; // declare zero initialized array variable
    arr1[0] = 666;

    arr1.len; // yields array element count (s64)
    arr1.ptr; // yields pointer to first element '&arr[0]'

    // inline initialization of array type
    arr2 := [10].s32{};            // Initialize all elements to 0. 
    arr3 := [4]s32.{ 1, 2, 3, 4 }; // Initialize array to the sequence 1, 2, 3, 4
};
```

Arrays can be implicitly converted to slice:

```c
array_to_slice :: fn () #test {
    arr : [10] s32;
    slice : []s32 = arr;
};
```

## String

String type in Biscuit aka `string_view` is slice containing a pointer to string data and string length. String literals
are always zero terminated. The `string_view` represents a string of fixed length. In case you want dynamically allocated
string use `string` type and its associated methods. Values of `string` can be implicitly converted to `string_view`.

**Example:**

```c
string_type :: fn () #test {
    msg : string_view = "Hello world\n";
    msg.len; // character count of the string
    msg.ptr; // pointer to the string content
};
```

## Slice

The array slice consists of a pointer to the first array element and array length.

Slice layout:

```c
Slice :: struct {
    len: s64;
    ptr: *T
};
```

**Example:**

```c
array_slice :: fn () #test {
    arr :: [4]s32.{1, 2, 3, 4};
    slice : []s32 = arr;
    loop i := 0; i < slice.len; i += 1 {
        print("%\n", slice[i]);
    }
};
```

!!! note
    `slice_init` can be used to allocate slice on the heap using context allocator.

## Structure

The structure is a composite type representing a group of data as a single type. The structure is as an array
another way to define user data type, but types of structure members could be different. It can be
used in situations when it's better to group data into one unit instead of interacting with separate
units.

Structure can be declared with use of struct keyword.

```c
Person :: struct {
    id: s32;
    name: string_view;
    age: s32;
}
```

Structure Person in example consists of id, name and age. Now we can create variable of this type
and fill it with data. To access person's member fields use `.` operator.

```c
main :: fn () s32 {
    my_person: Person; // Create instance of type Person
    my_person.id = 1;
    my_person.age = 20;
    my_person.name = "Martin";

    return 0;
}
```

Inline initialization is also possible. We can use compound expression to set all members at once.

```c
main :: fn () s32 {
    // Set all data in person to 0
    my_person1 := Person.{};
   
    // Initialize all members.
    my_person2 := Person.{ 1, "Martin", 20 };
    
    // We can explicitly name the members we want to initialize.
    my_person3 := Person.{ id = 1, name = "Martin", age = 20 };

    // We can change the order. 
    my_person4 := Person.{ name = "Martin", age = 20, id = 1 };
    
    // Or initialize only someting. In such a case the rest is initialized to 0 by default.
    my_person5 := Person.{ name = "Martin" };
    
    return 0;
}
```

Structure content can be printed by print function.

```c
main :: fn () s32 {
    my_person := Person.{ 1, "Martin", 20 };
    print("%\n", my_person);

    return 0;
}
```

```c
Person {id = 1, name = Martin, age = 20}
```

Due to lack of OOP support, we cannot declare member functions in structures and there is no class or
object concept in the language. Common way to manipulate with data is passing them into the
function as an argument.

```c
person_add_age :: fn (person: *Person, add: s32) {
    person.age += add;
}
```

Structure can extend any type with use of `#base <T>`. This is kind of inheritance similar to the C
style where inheritance can be simulated by composition. The `#base <T>` basically insert `base: T`;
as the first member into the structure. The compiler can use this information later to provide more
inheritance related features like merging of scopes to enable direct access to base-type members via
`.` operator or implicit cast from child to parent type.

Example of struct extension:

```c
Entity :: struct {
    id: s32
}

// Player has base type Entity
Player :: struct #base Entity {
    // base: Entity; is implicitly inserted as first member
    name: string_view;
};

Wall :: struct #base Entity {
    height: s32
};

Enemy :: struct #base Entity {
    health: s32
};

// Multi-level extension Boss -> Enemy -> Entity
Boss :: struct #base Enemy {
    // Extended struct can be empty.
};

struct_extending :: fn () #test {
    p: Player;
    p.id = 10; // direct access to base-type members
    p.name = "Travis";
    assert(p.base.id == 10); // access via .base

    w: Wall;
    w.id = 11;
    w.height = 666;

    e: Enemy;
    e.id = 12;
    e.health = 100;

    b: Boss;
    b.id = 13;

    // implicit down cast to entity
    update(&p);
    update(&w);
    update(&e);
    update(&b);
}

update :: fn (e: *Entity) {
    print("id = %\n", e.id);
}
```

## Union

The union is a special composite type representing value of multiple types. Union size is always equal to
size of the biggest member type and memory offset of all members is the same. Union is usually
associated with some enum providing information about stored type.

**Example:**

```c
Token :: union {
    as_string: string_view;
    as_int: s32;
}

Kind :: enum {
    String;
    Int;
}

test_union :: fn () #test {
    token1: Token;
    token2: Token;

    // Token has total size of the biggest member.
    assert(sizeof(token1 == sizeof(string_view));

    token1.as_string = "This is string";
    consumer(&token, Kind.String);

    token2.as_int = 666;
    consumer(&token, Kind.Int);
}

consumer :: fn (token: *Token, kind: TokenKind) {
    switch kind {
        Kind.String { print("%\n", token.as_string); }
        Kind.Int    { print("%\n", token.as_int); }
        default { panic(); }
    }
}
```

## Any

Any type is a special builtin structure containing a pointer to TypeInfo and pointer to data. Any
value can be implicitly cast to this type on function call.

Any type layout:

```c
Any :: struct #compiler {
    type_info: *TypeInfo;
    data: *u8
};
```

Remember that Any instance does not contain a copy of the value but only pointer to already stack
or heap allocated data. Any instance never owns pointed data and should not be responsible for
memory free.

Since Any contains a pointer to data, we need to generate temporary storage on stack for constant
literals converted to Any.

```c
...
foo(10); // temp for '10' is created here
...

foo :: fn (v: Any) {}
```

For types converted to the Any compiler implicitly sets `type_info` field to a pointer to the TypeType
type-info and data field to the pointer to actual type-info of the converted type.

```c
...
foo(s32); // Type passed
...

foo :: fn (v: Any) {
    assert(v.type_info.kind == TypeKind.TYPE);

    data_info := cast(*TypeInfo) v.data;
    assert(data_info.kind == TypeKind.INT);
}
```

Any can be combined with vargs; good example of this use case is print function where args argument
type is vargs of Any (\... is the same as \...Any). The print function can take values of any type
passed in args.

```c
print :: fn (format: string_view, args: ...) {
    ...
};
```

## Enum

The enum allows the creation of type representing one of the listed variants. Biscuit enums can
represent variants of any integer type (`s32` by default). All variants are grouped into enum's
namespace.

**Example:**

```c
// Enum declaration (base type is by default s32)
Color : type : enum {
    Red;    // default value 0
    Green;  // default value 1
    Blue    // default value 2
};

simple_enumerator :: fn () #test {
    assert(cast(s32) Color.Red == 0);
    assert(cast(s32) Color.Green == 1);
    assert(cast(s32) Color.Blue == 2);

    // Base type is s32
    assert(sizeof(Color) == 4);

    // Declare variable of type Color with value Red
    color := Color.Red;
    assert(cast(s32) color == 0);
};

// Enum declaration (base type is u8)
Day :: enum u8 {
    Sat = 1; // first value explicitly set to 1
    Sun;     // implicitly set to previous value + 1 -> 2
    Mon;     // 3
    Tue;     // ...
    Wed;
    Thu;
    Fri
};

test_enumerator :: fn () #test {
    /* Day */
    assert(cast(s32) Day.Sat == 1);
    assert(cast(s32) Day.Sun == 2);
    assert(cast(s32) Day.Mon == 3);

    // Base type is u8
    assert(sizeof(Day) == 1);
};
```

## Enum flags

An enumerator can be used as a definition of bit flags by adding #flags directive to the type
definition. This directive slightly changes the way how the enumerator values are generated. By
default, the enumerator starts with zero variant (if it's not explicitly changed by the programmer)
and every following enumerator variant has a value set to the previous one plus one. The flags
enumerator start with the first variant set to 1 and the following variants are set to the
left-bit-shifted value of the previous one.

Enumerators marked as flags are also serialized as a combination of atomic flags instead of just one
value.

!!! note
     Flags enumerators must use unsigned number type as a base type (`u32` by default).

!!! note
     It's possible to do implicit casting of flags enumerators to it's base type.

**Example:**

```c
OpenMode :: enum #flags {
    Read;   // 1
    Write;  // 2
    Append; // 4
    Create; // 8
    WriteAppend = Write | Append; // Combination of multiple variants.
    WriteCreate = Write | Create;
}

main :: fn () s32 {
    mode: OpenMode; // Set to OpenMode.Read by default
    mode = OpenMode.WriteCreate;
    print("mode = %\n", mode);

    // Set flag
    set_flag(&mode, OpenMode.Append);
    print("mode = %\n", mode);

    // Check flag
    if is_flag(mode, OpenMode.Append) { print("Append is enabled!\n"); }

    // Clear flag
    clr_flag(&mode, OpenMode.Append);

    return 0;
}
```

!!! note
    Since flags enumerators starts implicitly with value 1, you can explicitly define `NoFlag
    = 0;` variant at the beginning of the variant list.

## Type aliasing

It's possible to create alias to any data type except function types, those can be referenced only
by pointers.

```text
<alias name> :: <type>;
```

**Example:**

```c
alias :: fn () #test {
    T :: s32;
    i : T;
    i = 10;
    print("%\n", i);
};
```

## Function type

Type of function.

```text
fn ([arguments]) [T|(T1, T2)]
```

```c
// type of function without arguments and without return value
fn ()

// type of function without arguments, returning value of 's32' type
fn () s32

// type of function with two arguments, returning value of 's32' type
fn (s32, bool) s32
```

## Type casting

Change type of value to the other type. Conventions between integer types, from pointer to `bool`
and from array to slice are generated implicitly by the compiler.

```text
cast(<T>) <expr>
```

**Example:**

```c
type_cast :: fn () #test {
    // default type of integer literal is 's32'
    i := 666;

    // type of the integer literal is changed to u64
    j : u16 = 666;

    // implicit cast on function call
    fn (num: u64) {
    } (j);

    // explicit cast of 'f32' type to 's32'
    l := 1.5f;
    m := cast(s32) l;
};
```

Biscuit type casting rules are more strict compared to C or C++, there are no void pointers or
implicit conversion between integers and enums etc. Despite this fact an explicit cast can be in
some cases replaced by auto cast. The auto cast operator does not need explicit destination type
notation, it will automatically detect destination type based on expression if possible. When auto
operator cannot detect type, it will keep expression's type untouched. In such case auto does not
generate any instructions into IR.

```text
auto <expr>
```

**Example:**

```c
type_auto_cast :: fn () #test {
    s32_ptr : *s32;
    u32_ptr : *u32;

    // auto cast from *u32 to *s32
    s32_ptr = auto u32_ptr;

    // keep expession type s32
    i := auto 10;
};
```

## Type Decomposition

Type decomposition can be used on composit types to get type of any member for later use.

**Example:**

```c
Person :: struct {
    name: string_view;
    age: s32;
}

main :: fn () s32 {
    name: Person.name; // string_view type
    age: Person.age; // s32 type
    return 0;
}

```

This can be extremely useful when generic structures are used in polymorphic functions and we
don't know internal member types in advance.

**Example:**
```c
MyContainer :: fn (TValue: type) type #comptime {
    return struct {
        value: TValue;
    };
}

// Return type is type of TContainer member value.
get_value :: fn (container: *?TContainer) *TContainer.value {
    return &container.value;
}

main :: fn () s32 {
    container: MyContainer(u64);
    value :: get_value(&container);
    return 0;
}
```

Pointer type dereference is also possible.

**Example:**
```c
Person :: struct {
    name: string_view;
    age: s32;
    parent: *Person;
}

main :: fn () s32 {
    parent_by_value: @Person.parent; // Person type.
    return 0;
}
```

## Simple literals

```c
b :: true;         // bool true literal
b :: false;        // bool false literal
ptr : *s32 = null; // *s32 null pointer literal
```

## Integer literals

Biscuit language provides constant integer literals written in various formats showed in example
section. Integer literals has volatile type, when desired type is not specified compiler will choose
best type to hold the value. Numbers requiring less space than 32 bits will be implicitly set to
s32, numbers requiring more space than 31 bits and less space than 64 bits will be set to s64 and
numbers requiring 64 bits will be set to u64 type. Bigger numbers are not supported and compiler
will complain. When we specify type explicitly (ex.: foo : u8 : 10;), integer literal will inherit
that type.

**Example:**

```c
i     :: 10;      // s32 literal
i_u8  : u8 : 10;  // u8 literal
i_hex :: 0x10;    // s32 literal
i_bin :: 0b1011;  // s32 literal
f     :: 13.43f;  // f32 literal
d     :: 13.43;   // f64 literal
char  :: 'i';     // u8 literal
```

## Binary Operators

| Symbol | Description                    |
| ------ | ------------------------------ |
| \+     | Addition.                      |
| \-     | Subtraction.                   |
| \*     | Multiplication.                |
| /      | Division.                      |
| %      | Remainder division.            |
| +=     | Addition and assign.           |
| -=     | Subtraction and assign.        |
| \*=    | Multiplication and assign.     |
| /=     | Division and assign.           |
| %=     | Remainder division and assign. |
| <      | Less.                          |
| \>     | Greater.                       |
| <=     | Less or equals.                |
| \>=    | Greater or equals.             |
| ==     | Equals.                        |
| &&     | Logical AND.                   |
| \|\|   | Logical OR.                    |
| &      | Bit AND.                       |
| \|     | Bit OR.                        |
| ^      | Bit XOR.                       |
| &=     | Bit AND and assign.            |
| \|=    | Bit OR and assign.             |
| ^=     | Bit XOR and assign.            |
| <<     | Bitshift left.                 |
| \>\>   | Bitshift right.                |

## Unary Operators

| Symbol | Description          |
| ------ | -------------------- |
| \+     | Positive value.      |
| \-     | Negative value.      |
| @      | Pointer dereference. |
| &      | Address of.          |
| !      | Logical not.         |
| ~      | Bit flip.            |

## Special Operators

| Symbol         | Relevant for types | Description                          |
| -------------- | ------------------ | -------------------------------      |
| sizeof(expr)   | Any                | Determinates size in bytes.          |
| alignof(expr)  | Any                | Determinates alignment of epression. |
| typeinfo(expr) | Any                | Determinates TypeInfo of expression. |
| typeof(expr)   | Any                | Determinates type of expression.     |

## Type Info

Biscuit language provides type reflection allowing access to the type structure of the code. Pointer
to the type information structure can be yielded by `typeinfo(<T>)` builtin operator call. Type
information can be yielded in compile time and also in runtime, with low additional overhead for
runtime (only pointer to the TypeInfo constant is pushed on the stack).

**Example:**

```c
RTTI :: fn () #test {
    // yields pointer to TypeInfo constant structure
    info := typeinfo(s32);

    if info.kind == TypeKind.INT {
        // safe cast to *TypeInfoInt
        info_int := cast(*TypeInfoInt) info;

        print("bit_count = %\n", info_int.bit_count);

        if info_int.is_signed {
            print("signed\n");
        } else {
            print("unsigned\n");
        }
    }
};
```

By calling the `typeinfo` operator compiler will automatically include desired type information into
output binary.

## Hash directive

Hash directives specify special compile-time information used by compiler. They are introduced by
`#` character followed by directive name and optionally some other information.

### #load

Load source file into the current assembly. Every file is included into the assembly only once even
if we load it from multiple locations.

Lookup order:

- Current file parent directory.
- BL API directory set in install location/etc/bl.conf.
- System PATH environment variable.

```text
#load "<bl file>"
```

### #import

Import module into current assembly.

```c
#import "<bl module>"
```

### #private

Creates private (file scope) block in the file. Everything after this is going to be private and
visible only inside the current file.

**Example:**

```c
// main is public
main :: fn () s32 {
    foo(); // can be called only inside this file.
    return 0;
};

#private

// private function can be called only inside this file
foo :: fn () {
};

// private constant
bar :: 10;
```

### #scope

Creates new named scope i.e. `#scope std`. Every symbol written after the `scope` tag lives in
named scope (aka namespace). This prevents possible symbol collisions and makes local names shorter.
Named scope cannot be nested in another one and can be specified only once per file unit. Scopes
with the same name defined in multiple units are merged into one.

To refer to public symbols from the outside of the named scope use the scope name followed by the
dot operator. (i.e. `std.compare`)

### #extern

Used for marking entities as an external (imported from dynamic library). Custom linkage name can be
specified since version 0.5.2 as a string `#extern "malloc"`, when linkage name is not explicitly
specified compiler will use name of the entity as linkage name.

**Example:**

```c
// libc functions
malloc :: fn (size: usize) *u8 #extern;
// since 0.5.2
my_free :: fn (ptr: *u8) #extern "free";
```

### #export

Mark symbol to be exported when compile into library. This can be used only for functions for now.

**Example:**

```c
// libc functions
my_func :: fn () #export {
    print("Hello!\n");
}
```

### #compiler

Used for marking entities as an compiler internals.

!!! warning
     This directive is compiler internal.

### #test

Introduce test case function. The test case function is supposed not to take any arguments and
return always `void`. All function with `test` hash directive are automatically stored into builtin
implicit array and can be acquired by `testcases()` function call. Every test case is stored as
`TestCase` type.

**Example:**

```c
this_is_my_test :: fn () #test {
   ...
}
```

### #line

Fetch current line in source code as s32.

### #file

Fetch current source file name string_view.

### #noinit

Disable variable default initialization. This directive cannot be used with global variables (those
must be initialized every time).

**Example:**

```c
test_no_init :: fn () #test {
    my_large_array: [1024]u8 #noinit;
}
```

### #call_location

This directive yields pointer to static `CodeLocation` structure generated by compiler containing
call-side location in code. The `call_location` can be used only as function argument default value.
It's useful in cases we want to know from where function was called.

**Example:**

```c
test_call_location :: fn () #test {
    print_location();
}

print_location :: fn (loc := #call_location) {
    print("%\n", loc);
}
```

### #inline and #no_inline

Function related directives giving the compiler information about possibility of inlining marked
function during optimization pass.

**Example:**

```c
my_inline_function :: fn () #inline {
   ...
}
```

### #base

Specify base type of structure.

**Example:**

```c
Type :: struct #base s32 {
   ...
}
```

### #entry

Specify executable entry function.

!!! warning
     This directive is compiler internal.

### #build_entry

Specify build system entry function.

### #tag

Specify struct member tag. This value can be evaluated by type info.

**Example:**

```c
NO_SERIALIZE :: 1;
NO_GUI :: 2;

Type :: struct {
    i: s32 #tag NO_SERIALIZE | NO_GUI;
}
```

### #intrinsic

Mark external function as compiler specific intrinsic function.

!!! warning
     This directive is compiler internal.

### #maybe_unused

Suppress usage checking for declaration. This can be helpful i.e.: in case the polymorphic function is 
using some private global variables and the function is not used (the body is never generated). Such
variable should not be reported as unused.

This is workaround to get rid of unwanted warning for now.

### #comptime

Mark the function as compile-time executed function.

## Variable

Variable associate name with value of some type. Variables in BL can be declared as mutable or
immutable, value of immutable variable cannot be changed and can be set only by variable
initializer. Type of variable is optional when value is specified. Variables can be declared in
local or global scope, local variable lives only in particular function during function execution,
      global variables lives during whole execution.

Variables without explicit initialization value are `zero initialized` (set to default value). We
can suppress this behaviour by `#noinit` directive. Global variables must be initialized every time
(explicitly or zero initialized) so `#noinit` cannot be used.

**Example:**

```c
mutable_variables :: fn () #test {
    i : s32 = 666;
    j := 666; // type is optional here
    i = 0; // value can be changed
};

immutable_variables :: fn () #test {
    i : s32 : 666;
    j :: 666; // type is optional here
    // value cannot be changed
};

variable_initialization :: fn () #test {
    i: s32; // implicitly initialized to 0
    arr: [1024]u8 #noinit; // not initialized
}
```

!!! note
    Prefer immutable variables as possible, immutable value can be effectively optimized by
    compiler and could be evaluated in compile time in some cases.


## Compound expression

Compound expression can be used for inline initialization of variables or directly as value.
Implicit temporary variable is created as needed.  Zero initializer can be used as short for
memset(0) call.

**Example:**

```c
array_compound :: fn () #test {
    // print out all array values
    print_arr :: fn (v: [2]s32) {
        loop i := 0; i < v.len; i += 1 {
            print("v[%] = %\n", i, v[i]);
        }
    };

    // create array of 2 elements directly in call
    print_arr([2]s32.{10, 20});

    // create zero initialized array
    print_arr([2]s32.{0});
};

struct_compound :: fn () #test {
    Foo :: struct {
        i: s32;
        j: s32
    };

    print_strct :: fn (v: Foo) {
        print("v.i = %\n", v.i);
        print("v.j = %\n", v.j);
    };

    // create structure in call
    print_strct(Foo.{10, 20});

    // create zero initialized structure
    print_strct(Foo.{}});
};
```

## Function

Function is chunk of code representing specific piece of program functionality. Function can be
called with call operator `()`, we can provide any number of arguments into function and get return
value back on call-side.

Functions can be declared in global or local scope (one function can be nested in other).

### Named function

Function associated with name can be later called by this name. In this case we treat function like
immutable variable.

**Example:**

```c
// named function
my_function :: fn () {
    print("Hello!!!\n");
};

my_function_with_return_value :: fn () s32 {
    return 10;
};

my_function_with_arguments :: fn (i: s32, j: s32) s32 {
    return i + j;
};

test_fn :: fn () #test {
    // call function by name
    my_function();
    result1 :: my_function_with_return_value();
    result2 :: my_function_with_arguments(10, 20);
}
```

### Anonymous function

Functions can be used without explicit name defined and can be directly called.

**Example:**

```c
test_anonymous_function :: fn () #test {
    i := fn (i: s32) s32 {
        return i;
    } (666);
    print("%\n", i);
}
```

### Function pointer

Functions can be called via pointer. Call on null pointer will produce error in interpreter.

**Example:**

```c
test_fn_pointers :: fn () #test {
    foo :: fn () {
        print("Hello from foo!!!\n");
    };

    bar :: fn () {
        print("Hello from bar!!!\n");
    };

    // Grab the pointer of 'foo'
    fn_ptr := &foo;

    // Call via pointer reference.
    fn_ptr();

    fn_ptr = &bar;
    fn_ptr();
};
```

### Function with variable argument count

Biscuit supports functions with variable argument count of the same type. VArgs type must be last in
function argument list. Compiler internally creates temporary array of all arguments passed in
vargs.  Inside function body variable argument list acts like regular array slice.

**Example:**

```c
sum :: fn (nums: ...s32) s32 {
    // nums is slice of s32
    result := 0;
    loop i := 0; i < nums.len; i += 1 {
        result += nums[i];
    }

    return result;
};

test_vargs :: fn () #test {
    s := sum(10, 20, 30);
    assert(s == 60);

    s = sum(10, 20);
    assert(s == 30);

    s = sum();
    assert(s == 0);
};
```

### Local function

Function can be declared even in local scope of another function.  Local-scoped functions does not
capture variables from parent scope (scope of the upper_func in example), this leads to some
restrictions.  You cannot access i variable declared in upper_func from the inner_func.

**Example:**

```c
upper_func :: fn () {
    i := 10; // local for upper_func

    inner_func :: fn () {
        i := 20; // local for inner_func (no capture)
    };
}
```

### Default argument value

Function arguments can use default value if value is not provided on call side. Default value must
be known in compile time.

**Example:**

```c
foo :: fn (i: s32, j := 10) {}

test_foo :: fn () #test {
    // here we call foo only with one argument so j will
    // use default value 10
    foo(10);
}
```

### Explicit function overloading

More functions can be associated with one name with explicit function overloading groups. Call to
group of functions is replaced with proper function call during compilation, based on provided
arguments.

!!! note
     There is no additional runtime overhead caused by function overloading. 

!!! note
     Ordering of functions inside the group is arbitrary. 

**Example:**

```c
group :: fn { s32_add; f32_add; }

s32_add :: fn (a: s32, b: s32) s32 {
    return a + b;
}

f32_add :: fn (a: f32, b: f32) f32 {
    return a + b;
}

test_group :: fn () #test {
    i :: group(10, 20);
    j :: group(0.2f, 13.534f);
    print("i = %\n", i);
    print("j = %\n", j);
}
```

Functions can be declared directly inside the overload group:

```c
group :: fn { 
    fn (a: s32, b: s32) s32 {
        return a + b;
    };

    fn (a: f32, b: f32) f32 {
        return a + b;
    };
}
```

**Overload Resolution**

When function group is called the function overload resolution takes into account multiple options to
sort all possible call candidates by its priority. Candidate function with the highest priority is
used. In case there are multiple functions with the same priority found in the group, compiler complains
about ambiguous function call. In case there is no call candidate, the first one is used. This usually
leads to an error later if the function interface is not compatible.

**The overload resolution is based on:**

1. Argument count.
2. Argument types.
3. Type casting.
4. Conversion to slice.
5. Conversion to any.

!!! note
     The return type has no effect on choosing the best call candidate.

**Resolving of the best call candidate is done in two passes:**

1. Pick all possible candidates based on call-side argument count when:
   - Argument count is exactly matching the count of arguments required by the function interface. 
   - All arguments up to the first defaulted or variadic argument in the function interface are
     provided.
     
2. Iterate over previously picked functions and rank them comparing call-side arguments with the
   each function's interface arguments one by one:
   - Type is exactly the same. (Rank +3)
   - Can be implicitly casted. (Rank +2) 
   - Can be implicitly converted. (Rank +2) 
   - Can be implicitly converted `Any`. (Rank +1) 
   - Can be added into vargs. (Rank +1) 
   
3. Use function with highest rank.

**Examples:**
```c
a :: fn (_: []u8)                {}
b :: fn (_: string)              {}
c :: fn (_: Any)                 {}
d :: fn (_: s32, _: bool = true) {}

group :: fn { a; b; c; d; }

// a: rank = 3 <- used 
// b: rank = 0  
// c: rank = 1
// d: rank = 0
group("hello");

// a: rank = 2 (can be implicitly converted to []u8)
// b: rank = 3 <- used 
// c: rank = 1
// d: rank = 0
str: string;
group(str);

// a: rank = 0 
// b: rank = 0 
// c: rank = 1
// d: rank = 3 <- used
group(10);

// a: rank = 0 
// b: rank = 0 
// c: rank = 0
// d: rank = 6 <- used
group(10, false);

// a: rank = 0 
// b: rank = 0 
// c: rank = 1
// d: rank = 2 <- used (implicitly casted s8 to s32)
i: s8;
group(10);
```

### Polymorphic functions

[How-To](examples/polymorph.bl)

Polymorphic function (aka templated function or generic function) is a well-known concept in many
programming languages. It's a sort of meta-programming method providing good type safety and proper
error reporting. The basic idea is the automatic generation of functions doing the same operation
using multiple types, instead of rewriting the function for every type needed, we just specify it as
a "recipe" for later generations.

Consider the following function doing an addition of two values, when we want to use the function
with multiple different types, we must explicitly rewrite the same function for every type needed:

```c
add_s32 :: fn (a: s32, b: s32) s32 {
    return a + b;
}

// Same for floats.
add_f32 :: fn (a: f32, b: f32) f32 {
    return a + b;
}
```

In this case, we can use polymorph instead:

```c
add :: fn (a: ?T, b: T) T {
    return a + b;
}
```

Value of `T` represents any type, in this case, chosen based on usage of the function. The question
mark before `T` says the first `T` is the master polymorph type. The compiler tries to replace all
master types with the type of argument on the call side and register the new type alias `T` in the
function scope.

Example of usage:

```c
main :: fn () s32 {
    result_1 : s32 = add(10, 20);
    result_2 : f32 = add(1.4f, 42.5f);

    return 0;
}
```

Notice that we call the same function, first with integers and second with floats. The type of `T`
is based on the first argument type (because the master type is defined as the type of the first
argument).  The second argument type, in this case, must be the same type as the master
because `b` use, as its type, alias `T`. The same alias is used also as a return type.

So two functions are generated internally:

```c
.add.1 :: fn (a: s32, b: s32) s32 {
    return a + b;
}

.add.2 :: fn (a: f32, b: f32) f32 {
    return a + b;
}
```

!!! note
    Content of polymorphic function is semantically analyzed only when
    function is used.

#### Nested master type

Polymorph master type replacement can be used also as nested member in more complex types.

**Example:**

```c
sum :: fn (slice: []?T) T {
    result: T; // We can use T inside the function as well
    loop i := 0; i < slice.len; i += 1 {
        result += slice[i];
    }
    return result;
}

// Sum function accepts any slice as an input and any array is implicitly convertible
// to slice.

// Use the function with static array
arr_static := [3]s32.{10, 20, 30};
sum(arr_static); // T = s32

// Use the function with dynamic array
arr_dynamic: [..]f32;
defer array_terminate(&arr_dynamic);
array_push(&arr_dynamic, 10.f);
array_push(&arr_dynamic, 20.f);
array_push(&arr_dynamic, 30.f);

sum(arr_dynamic); // T = f32
```

#### Multiple polymorph masters

More than one polymorphic masters can be declared inside the function
argument list:

```c
table_insert :: fn (table: *Table, key: ?TKey, value: ?TValue) {
    // ...
}
```

#### Specify implementation for type

In some cases we want to specify explicitly what implementation should be used for some specific
type, i.e. in a function doing comparison of two values, we can provide specific handling for
string:

```c
is_equal :: fn { // function group
    // Implementation used for strings only.
    fn (a: string_view, b: string_view) bool {
        return std.str_compare(a, b);
    };

    // Implementation used for all other types.
    fn (a: ?T, b: T) bool {
        return a == b;
    };
}
```

!!! note
    Compiler error is reported in case content of the polymorph generated for some type
    specification is not semantically valid. (i.e. we can't compare strings directly by `==` operator)

!!! warning
    Getting address (by `&` operator) of polymorphic function recipe is not possible,
    polymorphic recipe as it is does not represent any allocated memory in program binary.

### Multiple Return Values

Function in BL can return more than one value, this can be useful i.e.  in cases we want to return
value and error code. There is no explicit limitation of returned value count. Return value can be
also named to make the function interface more readable.

Returned values are implicitly converted to anonymous structure instances with possibility to
implicitly unroll results on caller side.

Use unnamed identifier to ignore some of returned values on caller side.

Example of multiple return:

```c
foo :: fn () (s32, bool) {
    return 666, true;
}

main :: fn () s32 {
    int1, boolean1 := foo();

    // no all values must be captured
    int2 := foo();
}
```

Example of multiple return with named values:

```c
foo :: fn () (number: s32, boolean: bool) {
    return 666, true;
}

main :: fn () s32 {
    int1, boolean1 := foo();
    _, b := foo(); // Ignore first returned value.
}
```

### Comptime Function

The `comptime` function is every function marked by `#comptime` hash directive in a declaration. 
Every call to such a function is going to be evaluated in compile-time and replaced by constant 
eventually.

**Example:**
```c
hash_string :: fn (s: string_view) u32 #comptime {
    return std.str_hash(s);
}
```

The `hash` function can be later called as any other regular function, but the call itself is 
replaced by constant result in the final binary.

```
main :: fn () s32 {
    hash := hash_string("Hello!");
    print("%\n", hash);
    return 0;
}
```

So the comptime function has no runtime overhead.

**List of cons:**

1. Every argument passed, must be known in compile-time.
2. All arguments inside the function are constant (we cannot change it's values).
3. Returning pointers from comptime functions is not a good idea in general (i.e. addresses of functions
   in compile-time are not the same in runtime).

**List of pros:**

1. We can pass any type as a value to the comptime function.
2. We can return any type as a value.
3. Since all comptime functions are evaluated in compile-time, there is no runtime overhead. 

## Block

Block can limit scope of the variable.

**Example:**

```c
#import "std/test"

blocks :: fn () #test {
    a := 10;

    {
    // this variable lives only in this scope
       i := a;
       assert(i == 10);
    }

    i := 20;
    assert(i == 20);
};
```

## If - Else

If represents condition statement which can change program flow. If executes following code block
only if passed condition is `true`, otherwise skip the block and continue on next statement after
the block.  We can specify `else` block which is executed only if condition is `false`.

**Example:**

```c
test_ifs :: fn () #test {
    b := true;
    if b {
        print("b is true!\n");
    } else {
        print("b is false!\n");
    }
};
```

If statement can be static. In this case the condition must be known in compile time. Static ifs are
evaluated during compilation and can be used i.e. for including/excluding some parts of code based
on some constant expression.

**Example:**

```c
test_static_ifs :: fn () #test {
    #if IS_DEBUG {
        print("This is compiled in debug mode!\n");
    } else {
        print("This is compiled in release mode!\n");
    }
};
```

!!! note
    The excluded branch is removed completely from compilation, but it still has to be
    semantically valid.

## Loop

**Example:**

```c
simple_loops :: fn () #test {
    count :: 10;
    i := 0;

    loop {
        i += 1;
        if i == count { break; }
    }

    i = 0;
    loop i < count {
        i += 1;
    }

    loop j := 0; j < count; j += 1 {
        // do something amazing here
    }
};
```

## Break and continue

Break/continue statements can be used in loops to control execution flow.

**Example:**

```c
break_and_continue :: fn () #test {
    i := 0;
    loop {
        i += 1;
        if i == 10 {
            break;
        } else {
            continue;
        }
    }
};
```

## Switch

Switch can compare one numeric value against multiple values and switch execution flow to matching
case. The `default` case can be used for all other values we don't explicitly specify case for.

**Example:**

```c
test_switch :: fn () #test {
    i := 1;
    switch i {
        0 { print("Zero!\n"); }
        1 { print("One!\n"); }
        default { print("Other!\n"); }
    }
}
```

Switch can be also used with enumerators, in such case we have to specify cases for all enumerator
variations or specify `default` one.

**Example:**

```c
Color :: enum {
    Red;
    Green;
    Blue;
}

test_switch :: fn () #test {
    c := Color.Blue;
    switch c {
        Color.Red   { print("Red!\n");   }
        Color.Green { print("Green!\n"); }
        Color.Blue  { print("Blue!\n");  }
    // default is not needed here, we covered all variants.
    }
}
```

It's also possible to define one execution block for multiple cases.

**Example:**

```c
Color :: enum {
    Red;
    Green;
    Blue;
}

test_switch :: fn () #test {
    c := Color.Blue;
    switch c {
        Color.Red,
        Color.Green { print("Red or green!\n"); }
        Color.Blue  { print("Blue!\n");  }
    }
}
```

## Defer statement

The defer statement can be used for defering execution of some expression. All deferred expressions
will be executed at the end of the current scope in reverse order. This is usually useful for
calling cleanup functions. When scope is terminated by return all previous defers up the scope tree
will be called after evaluation of return value.

**Example:**

```c
test_defer_example :: fn () #test {
    defer print("1\n");

    {
        defer print("2 ");
        defer print("3 ");
        defer print("4 ");
    } // defer 4, 3, 2

    defer_with_return();

    defer print("5 ");
} // defer 5, 1

defer_with_return :: fn () s32 {
    defer print("6 ");
    defer print("7 ");

    if true {
        defer print("8 ");
        return 1;
    } // defer 8, 7, 6

    defer print("9 "); // never reached
    return 0;
};
```

Output:

```bash
4 3 2 8 7 6 5 1
```

## Using statement

The using statement can be used to allow direct access to another scope's or enum's members. 

**Example:**

```c
main :: fn () s32 {
    using my_scope;
    // Directly call function from my_scope.
    print_kind(Kind.PLAYER); 
    print_kind(Kind.ENEMY);

    using Kind;
    // Directly use enum variants from my_scope.Kind.
    print_kind(PLAYER); 
    print_kind(ENEMY);
    return 0;
}

#scope my_scope

Kind :: enum { PLAYER; ENEMY; }

print_kind :: fn (k: Kind) {
    print("Kind is = %\n", k);
}

```

Be careful with the `using` statement, ambiguous declaration references can be introduced easily. Scope 
lookup rules are following in case the `using` is present:

* Symbols declared in the function scope and its child scopes are preferred during lookup. In case there
is a symbol of the same name in the used scope it's ignored and a local one is used without warnings/errors.
* In case a symbol with the same name is found in multiple used scopes, it's reported as ambiguous.
* In case a symbol from the used scope collides with a symbol in global/private scope, it's also reported as ambiguous.

!!! note
    The `using` statement placement is limited to local scopes due to explicitness and readability of the
    code.

## Main function

The `main` function is mandatory entry function which should be defined in every program. It's
basically entry point of your application. Main function must return `s32` execution state, zero in
this case indicates successful execution.

**Example:**

```c
main :: fn () s32 {
    // some useful stuff goes here.
    return 0;
}
```

!!! note
    Command line arguments are not passed directly as parameter in BL. Use
    `command_line_arguments` builtin array.

## Modules and import

The module system can be used to split source into chunks (modules) which can be later imported into
assembly by `#import` directive. Modules can distinguish between platforms and load different
sources on them during the compilation process.

A module is basically directory containing `module.yaml` configuration file, the name of the directory
is used as module name during import process.

See `ModuleImportPolicy` for more information about module import policy.

!!! note
    Module root directory usually contains all source files, libraries and unit tests related
    to the module.

**Example of the module file structure:**

```text
thread/
  module.yaml       - module config
  _thread.win32.bl  - windows implementation
  _thread.posix.bl  - posix implementation
  thread.bl         - interface
  thread.test.bl    - unit tests
```

**Example of the module config:**

```yaml
version: 4

x86_64-pc-windows-msvc:
  src: "_thread.win32.bl"

x86_64-pc-linux-gnu:
  src: "_thread.posix.bl"

x86_64-apple-darwin:
  src: "_thread.posix.bl"

arm64-apple-darwin:
  src: "_thread.posix.bl"
```

To import the `thread` module use:

```c
#import "path/to/module/thread"
```

### List of module config entries

**Global options:**
- `version: <N>` - Module version number used during import to distinguish various versions of same
module, see also `ModuleImportPolicy` for more information.

**Global or platform specific options:**

- `src: "<FILE1[;FILE2;...]>"` - List of source file paths relative to the module *root* directory
separated by **platform specific** separator (`:` on Windows and `;` on Unix).
- `linker_opt: "<OPTIONS>"` - Additional linker options.
- `linker_lib_path: "<DIR1;[DIR2;...]>"` - Additional linker lookup directories relative to the module
*root* directory.
- `link: "<LIB1[;LIB2;...]>` - Libraries to link.

**Global vs local options**

Module platform specific options can be set in sub-groups starting with platform target triple name
(use `blc --target-host` to get your current default target and `blc --target-supported` to get list
of all supported targets).

```yaml
version: 1
src: "my_file_imported_everytime.bl"

x86_64-pc-windows-msvc:
  src: "my_file_only_for_windows.bl"

x86_64-pc-linux-gnu:
  src: "my_file_only_for_linux.bl"
  linker_opt: "-lc -lm" # link these only on linux
```

## Unit testing

Biscuit compiler provides unit testing by default.

Create unit test case:

```c
#import "std/test"

// function to be tested
add :: fn (a: s32, b: s32) s32 {
    return a + b;
};

this_is_OK :: fn () #test {
    assert(add(10, 20) == 30);
};

this_is_not_OK :: fn () #test {
    assert(add(10, 20) != 30);
};

main :: fn () s32 {
    test_run();
    return 0;
}
```

Run tests:

```text
$ blc -rt test.bl
Compiler version: 0.7.0, LLVM: 10
Compile assembly: out [DEBUG]
Target: x86_64-pc-windows-msvc

Testing start in compile time
--------------------------------------------------------------------------------
[ PASS |      ] this_is_OK (0.021000 ms)
assert [test.bl:21]: Assertion failed!
execution reached unreachable code
C:/Develop/bl/lib/bl/api/std/debug.bl:113:5
  112 |     if IS_DEBUG { _os_debug_break(); }
> 113 |     unreachable;
      |     ^^^^^^^^^^^
  114 | };
called from:
C:/Develop/bl/tests/test.bl:21:11
  20 | this_is_not_OK :: fn () #test {
> 21 |     assert(add(10, 20) != 30);
     |           ^
  22 | };
[      | FAIL ] this_is_not_OK (1.630000 ms)

Results:
--------------------------------------------------------------------------------
[      | FAIL ] this_is_not_OK (1.630000 ms)
--------------------------------------------------------------------------------
Executed: 2, passed 50%.
--------------------------------------------------------------------------------
```

## Use BL code from C/C++

Since BL compiler supports compilation into shared library, ABI compatible with C, BL code can be
easily called from C/C++ program. Use `#export` to mark function to be exported from library and
`--shared` flag to create shared object (`so` on Linux). See example bellow on Linux.

```c
// Content of my-lib.bl
my_bl_function :: fn (count: s32) #export {
    loop i := 0; i < count; i += 1 {
        print("Hello from foo library!!!\n");
    }
}
```

```c
// Content of main.c
void my_bl_function(int count);

int main(int argc, char *argv[]) {
    my_bl_function(100);
    return 0;
}
```

```bash
$ blc --shared my-lib.bl
$ gcc -L. -o test main.c -lout
$ export LD_LIBRARY_PATH=. && ./test
```

## Builtin variables

List of builtin variables set by compiler.

- `IS_DEBUG` Is bool immutable variable set to true when assembly is running in debug mode.
- `IS_COMPTIME_RUN` Is bool immutable variable set to true when assembly is executed in compile
time.

