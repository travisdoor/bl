 #![alt text](biscuit_logo.png "logo") The Biscuit Language - basics

## Comments

	// this is line comment
	/* this
	   is
	   multiline
	   comment */

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

## Modules
Modules are similar to C++ namespaces.

    /* EXAMPLE: Module */

    module c {
      public extern fn printf(s string, i i32) i32;
    }

    // declaration of module A
    module A {
      // public module B
      public module B {
        // public function add (can be called from the outside)
        public fn add(a i32, b i32) i32 {
          return priv_add(a, b);
        }

        // private function priv_add (can be called only inside module 'A')
        fn priv_add(a i32, b i32) i32 {
          return a + b;
        }
      }

      public fn print_addition() {
        c.printf("2 + 3 = %d\n", B.add(2, 3));
      }
    }

    fn main() {
      c.printf("2 + 3 = %d\n", A.B.add(2, 3));
      A.print_addition();
      // A.B.priv_add(2, 3) generates error (method is private for module A.B)
    }
	
## Variables
    
    /* EXAMPLE: Var */

    module c {
      public extern fn printf(s string, i i32) i32;
    }

    fn main() {
      // declaration of variable i of i32 type (automatically set to default value 0)
      var a i32;
      var b i32 = 10; 

      c.printf("a + b = %d\n", a + b);
    }

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
