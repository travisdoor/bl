# ![alt text](biscuit_logo.png "logo") The Biscuit Language - basics

## Comments

	// this is line comment
	/* this
	   is
	   multiline
	   comment */

## Modules
Modules are similar to C++ namespaces.

	module foo {
	  ...
	  module bar {
	    ...
	  }
	}
	
## Fundamental types

| Name | Size | Value           |
|------|------|-----------------|
| char | 1B   | character |
| i8   | 1B   | signed number   |
| i32  | 4B   | signed number   |
| i64  | 8B   | signed number   |
| u8   | 1B   | unsigned number |
| u32  | 4B   | unsigned number |
| u64  | 8B   | unsigned number |
| f32  | 4B   | floating-point number |
| f64  | 8B   | floating-point number |
| ptr | 8B   | pointer |
| string | 8B   | pointer to string |
| bool | 1b   | true/false |
	
## Functions
Functions are declared using 'fn' keyword fallowed by name of the function, 
params and return type. Return type is optional and can be 'void' or simply
nothing when function does not return any value.

    // C printf
    module c {
      public extern fn printf(s string, i i32) i32;
    }

    // main function declaration
    fn main() {
      c.printf("2 + 3 = %d\n", add(2, 3));
      print_addition();
    }
    
    // function add takes two numbers and return addition
    fn add(a i32, b i32) i32 {
      return a + b;
    }

    // function without parameters returning no value
    fn print_addition() {
      c.printf("2 + 3 = %d\n", add(2, 3));
    }
    
	
## Variables
    
	var foo i32;

## Constants 
	
	const foo i32 = 10;
	
## If - else

	if (true) {
	  ...
	} else {
	  ...
	}
	
## Loop
Use 'break' to interrupt iteration and 'continue' to jump to another cycle.

	var i i32 = 0;
	loop {
	  i = i + 1;
	  if (i > 10)
	    break;
	}
	
	
## While
Use 'break' to interrupt iteration and 'continue' to jump to another cycle.
 
	while (true) {
	  ...
	}
	
## Enum

	enum foo {
	  A = 0,
	  B,
	  C,
	  D = 10,
	  E,
	  F
	}
	
## Struct

	struct foo {
	  i32 a,
	  string b,
	  f64 c
	}
