# Variables

## Syntax And Mutability

Now let's talk about the language syntax. In BL, the name comes first followed by a colon separating the name of the symbol from its type. Most type specifications in BL can be left empty if the type is obvious from the value assigned to the symbol. The second colon separates the type from the actual value of the symbol. This may be a bit confusing so let's take a look at the simplest case; the variable.

```rust
number : s32;

```

In this case, we declare a variable named *number* of type *s32* and we do not specify any value of the number (the second colon and value are missing). In this case, the type *s32* is mandatory (there is no value to guess the type from). Also, one very important thing is that if we do not specify the value of the variable explicitly, it's implicitly set to the default value (0 in this case). *Note: This is not a case in C for example where the variable stays uninitialized.* 

```rust
number : s32 : 10;

```

Now our variable *number* is explicitly initialized to 10 using the literal value. All numeric literals are by default of *s32* type so we can make the same declaration like this:

```rust
number :: 10;

```

One important note about the second colon is that it affects the variable mutablitly. Variables can be declared as *mutable* the variable that can be changed at any time, or *immutable* we must initialize the variable with some value, and the value cannot be changed later. When the colon is used, we say the variable is *immutable* or *constant*.

```rust
number :: 10;
number = 20; // This is an error. Variable is immutable.

```

```text
hello.bl:3:12: error(0036): Cannot assign to immutable constant.
   2 |  number :: 10;
>  3 |  number = 20;
     |            ^
   4 |  return 0;

error: Compilation of target 'hello' failed.
```


To declare the variable as *mutable* we have to do the following:

```rust
number := 10; // '=' means the variable is mutable.
number = 20;  // This is OK now.

```

And in case there is no value specified, the variable is every time considered to be mutable.

```rust
number: s32; // Default value 0. 
number = 20; // This is also OK.

```

## Mutability Of Structure Members

Mutability introduced to variables of *struct* type is preserved also to all members unless a dereference is introduced explicitly or implicitly. That means the direct member modification is not allowed for immutable variables of *struct* type, however, if accessed via a pointer, the value can be changed.

```rust
Person :: struct {
    name: string_view;
    age: s32;
}

main :: fn () s32 {
    person :: Person.{};
    person.age = 10; // ERROR

    person_ptr :: &person;
    person_ptr.name = "foo"; // OK, access via pointer.

    person_ptr = null; // ERROR, the pointer itself is immutable.
    return 0;
}
```

## Initialization

As already mentioned, all variables can be initialized to some value in the declaration. In case the initialization value is not specified, the variable is implicitly "zero" initialized. 

```rust
number: s32;   // Default value 0. 
boolean: bool; // This is also false.

```

In case we want to disable the implicit initialization for some reason, we can use `#noinit` directive after the variable type.

```rust
number: s32 #noinit; 
boolean: bool #noinit;

```

Both variables are left uninitialized (containing some random data from the stack). Uninitialized variables should not be used before they are set to some meaningful value.

This can be useful in some cases when the default initialization may be expensive; i.e. The initialized variable is a large struct, and we explicitly initialize all its members later.

```rust
Data :: struct {
	a: s32;
	b: bool;
	// ... lot of other members
}

data: Data #noinit; // Disable default initialization.
data.a = 10;
data.b = true;
// Initialize all other members...

```

## Local 

Local variables have their lifetime limited to the current block (code usually surrounded by curly braces). They are allocated on the stack frame of an owning function and become invalid when a function returns.

```rust
main :: fn () s32 {
	my_local_integer: s32;

	{
		another_one: s32;
	}
	return 0;
}

```

The first local variable in the previous function is declared in the function block, so it's available in the whole function since it's declared. However, the second variable is declared in the anonymous block, it's available only inside this block.

```rust
main :: fn () s32 {
	my_local_integer: s32;

	{
		my_local_integer = 1; // This is OK.
		another_one: s32;
		another_one = 2; // This is also OK.
	}
	
	another_one = 2; // Error!
	return 0;
}

```

```rust
hello.bl:10:5: error(0018): Unknown symbol 'another_one'.
    9 |
>  10 |         another_one = 2; // Error!
      |         ^^^^^^^^^^^
   11 |         return 0;

error: Compilation of target 'hello' failed.
```

## Global

Global variables are in general, variables declared outside of any function. Its lifetime is not limited in any way and they are available anywhere in the program. Since there is no required ordering of symbols declared in global scope, the global variable can be used even before its declaration appears in the file. Due to this, BL does not require any header files or any kind of forward declarations.

All global variables must be initialized; either to a default value or explicitly, so we cannot use `#noinit` directive the same way as we did with local variables.

```rust
// Mutable initialized to 0.
number: s32;
// Mutable initialized to 'true'.
boolean := true;

main :: fn () s32 {
    print("number  = %\n", number);
    print("boolean = %\n", boolean);
    print("text    = %\n", text);
    return 0;
}

// Immutable, declared after use.
text :: "Hello";
```

The lifetime of global variables can be explicitly limited to the thread by `#thread_local` directive added after the variable declaration. The thread local variable is later instantiated for each thread separately, so it's safe to use them without any locks. But keep in mind that each instance points to a different memory location (data are not shared between threads).

```rust
{% include "../examples/thread_local_variable.bl" %}
```

## Usage Checks

The compiler will check if all declared variables are used and produce warnings eventually. However, variables declared in *non-private* global scope may be part of an API and are not checked for usage.

```rust
name := "Martin";

main :: fn () s32 {
    age :: 30;
    return 0;
}

#private
nationality := "CZ";
```

```text
test2.bl:9:1: warning: Unused symbol 'nationality'. Mark the symbol as '#maybe_unused' 
                       if it's intentional.
   8 | #private
>  9 | nationality := "CZ";
     | ^^^^^^^^^^^
  10 |

test2.bl:4:5: warning: Unused symbol 'age'. Use blank identificator '_' if it's  
                       intentional, or mark the symbol as '#maybe_unused'. If it's 
                       used only in some conditional or generated code.
   3 | main :: fn () s32 {
>  4 |     age :: 30;
     |     ^^^
   5 |     return 0;
```

As you can see, the compiler gives you some possible options on how to disable these warnings.

```rust
name := "Martin";

main :: fn () s32 {
    _ :: 30; // Blank identificator.
    return 0;
}

#private
nationality := "CZ" #maybe_unused;
```

## Shadowing

It's possible to declare a new variable with the same name in nested scopes, the previous variable is *shadowed* by the new one. In general, it's not a good idea; however, there are currently no restrictions or limitations. This may change in the future.

```rust
name := "Martin";

main :: fn () s32 {
    name := "Travis";
    {
        name := "George";
        print("name = %\n", name);
    }
    print("name = %\n", name);
    return 0;
}
```


## Comptime Variables

Compile-time known variable is any immutable variable with a value known in compile-time. In some cases, it's required to do compile-time evaluations of the value. For example, the array type definition requires the element count to be known in compile time.

```rust
main :: fn () s32 {
    N := 10;
    array: [N]s32;
    return 0;
}
```

```text
test2.bl:3:13: error(0052): Array size must be compile-time constant.
   2 |     N := 10;
>  3 |     array: [N]s32;
     |             ^
   4 |     return 0;
```

To fix this, it's enough to make the N variable immutable:

```rust
main :: fn () s32 {
    N :: 10; // Changed from = to :.
    array: [N]s32;
    return 0;
}
```

The `N` variable is initialized with compile-time known integer literal, however, if the initialization value is not known in compile-time, we'll get the same error again even if `N` is immutable.

```rust
foo := 10;

main :: fn () s32 {
    N :: foo;
    array: [N]s32;
    return 0;
}
```

```text
test2.bl:5:13: error(0052): Array size must be compile-time constant.
   4 |     N :: foo;
>  5 |     array: [N]s32;
     |             ^
   6 |     return 0;
```