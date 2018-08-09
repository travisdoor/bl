# ![alt text](biscuit_logo.png "logo") The Biscuit Language - basics

## Comments

	// this is line comment
	/* this
	   is
	   multiline
	   comment */

## Types
### Fundamental types
Basic data types integrated into compiler.

| Name | Size | Value           |
|------|------|-----------------|
| char | 1B   | character |
| s8   | 1B   | signed number   |
| s16  | 2B   | signed number   |
| s32  | 4B   | signed number   |
| s64  | 8B   | signed number   |
| u8   | 1B   | unsigned number |
| u16  | 2B   | unsigned number |
| u32  | 4B   | unsigned number |
| u64  | 8B   | unsigned number |
| f32  | 4B   | floating-point number |
| f64  | 8B   | floating-point number |
| string | 8B   | pointer to string |
| bool | 1b   | true/false |
| size_t | arch-depend (4/8B)   | unsigned number |

### Base rules
Declarations and mutability.
		
	<name> <type>;              // mutable declaration
	<name> [type] := <value>;   // mutable declaration
	<name> [type] : <value>;    // immutable declaration

Mutability of declared entity is described by binding symbols after declaration type. Type is optional in case we specify value.

Examples:
		
	// simple variables
	foo i32;            // declare mutable integer
	foo i32 := 0;       // declare and initialize to 0
	foo i32 : 20; 	    // declare immutable constant
	foo : 20;           // same like previous

	// struct
	Foo : struct {
	  i i32,
	  j i32
	};

	// function
	main : fn () i32 {
	};
		
		
### Immutables

Immutables or constants can be created with `:` operator. Initial values must be known in compile time and value of immutable cannot be changed later in code. Immutables will cause no allocation on stack.

Example:

	<name> <type>;              // mutable declaration
	<name> [type] := <value>;   // mutable declaration
	
	Number s32 : 666;    // immutable with explicit type s32
	Number : 666;        // type s32 is based on value '666'
		
### Mutables

Mutables or variables are allocated on stack.

Example:

	Number s32;        // mutable without initialization (not set to 0!!!)
	Number s32 := 666; // mutable set to 666
	Number := 666;     // same as previous
		

### Functions

Functions are special case of immutable declaration with type `fn ([args]) [return type]`. Return type of the function is also optional as the function arguments, when we don't specify return type `void` will be used implicitly.

Example:

	<name> [type] : <value>; 	// immutable declaration
	
	// function declaration with explicit type after name
	add fn (s32, s32) s32 : fn (a s32, b s32) s32 {
	  return a + b; 
	};

	// same as previous
	add : fn (a s32, b s32) s32 {
	  return a + b; 
	};
		
		
### Structs

Structures are same like in C for example, they are also immutable with type `struct {[members]}`.

Example:

	<name> [type] : <value>; 	// immutable declaration
	
	Foo struct {s32, u8} : struct {
	  foo s32,
	  bar u8
	};
	
	Foo : struct {
	  foo s32,
	  bar u8
    };
