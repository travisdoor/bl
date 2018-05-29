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
| i8   | 1B   | signed number   |
| i16  | 2B   | signed number   |
| i32  | 4B   | signed number   |
| i64  | 8B   | signed number   |
| u8   | 1B   | unsigned number |
| u16  | 2B   | unsigned number |
| u32  | 4B   | unsigned number |
| u64  | 8B   | unsigned number |
| f32  | 4B   | floating-point number |
| f64  | 8B   | floating-point number |
| string | 8B   | pointer to string |
| bool | 1b   | true/false |
| size_t | arch-depend (4/8B)   | unsigned number |

### Struct
Structures in BL are similar to C structures with a few exceptions: members are separated by comma
and all members are private by default. Private members are visible only in current module in
which is structure defined. Public members are visible for reading and writing from anywhere.
Whole structure can be public also.

    /* EXAMPLE: Struct */

    module c {
      public extern fn printf(s string, i i32) i32;
    }

    module data {
      public struct user_t {
        public name string,
        public age i32,
        id i32 // private struct member is visible only inside current module
      }

      public fn new_user(name string, age i32) user_t {
        var user user_t;
        user.name = name;
        user.age = age; 
        user.id = 666; // id is visible inside current module
        return user;
      }

      public fn get_id(user user_t) i32 {
        return user.id;
      }
    }

    fn main() {
      var user data::user_t = data::new_user("Tereza", 24);
      c::printf("Tereza is %d years old\n", user.age);
      c::printf("Tereza has id %d\n", data::get_id(user));
    }

### Pointer types
Pointer to any type can be defined with asterix prefix.

	var i32_ptr *i32;
	
Pointer variables are set no null by default.

### Type casting

	
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
      c::printf("2 + 3 = %d\n", add(2, 3));
      print_addition();
    }
    
    // function add takes two numbers and return addition
    fn add(a i32, b i32) i32 {
      return a + b;
    }

    // function without parameters returning no value
    fn print_addition() {
      c::printf("2 + 3 = %d\n", add(2, 3));
    }

## Modules
### Module declaration
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
        c::printf("2 + 3 = %d\n", B::add(2, 3));
      }
    }

    fn main() {
      c::printf("2 + 3 = %d\n", A::B::add(2, 3));
      A::print_addition();
      // A::B::priv_add(2, 3) generates error (method is private for module A::B)
    }

### Using
Using can be used in global scope or local scope to symplify path to symbols inside other modules. Only public modules are exposed via using.

	/* EXAMPLE: Using */
	
	module A {
	  public module B {
        public extern fn printf(s string) i32;
      }
	}
	
	fn main() {
      {
        {
           // using affect symbol lookup only in current block
           using A::B; 
           printf("hello from nested block\n");
        }
      }

      using A::B; 
      printf("hello from local scope\n");
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

      c::printf("a + b = %d\n", a + b);
    }

## Constants 
Constants can be defined in local scope of function or in global scope of module. Constant
value cannot be changed and initialization const-expression is requred. Constants in global
scope are private by default and can be made public with 'public' keyword.

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
Enumerators are by default of type i32 but they can be: i8, i32, i64, u8, u32, u64, f32, f64, string or char.
String and char typed enumerators must have explicit const-expr value set for every variant. 

    // by default i32
	enum foo {
	  A = 0,               // 0
	  B,                   // 1
	  C,                   // 2
	  D = 10,              // 10
	  E = cast(i32) D + 1, // 11
	  F                    // 12
	}

    // string enum (all variants must be explicitly set)
	enum foo string {
	  A = "A",     
	  B = "B",    
	  C = "C"      
	}

## Array


    /* EXAMPLE: Array */

    module c {
      public extern fn printf(s string, i i32) i32;
    }

    fn main() {
      var arr i32[256];
      var i i32;

      while (i < 256) {
        arr[i] = i;
        i = i + 1;
      }

      i = 0;
      while (i < 256) {
        c::printf("i = %d\n", i);
        i = i + 1;
      }
    }


## Preprocessor directives
### Load
Load preprocessor directive can be used for loading other source files into current assembly.
You can use relative path to files in current folder or in PATH environment variable.

	#load "some/my/file.bl" // in current directory
	#load "std/debug.bl" // in PATH system variable
	
	fn main() i32 {
	  return 0;
	}
