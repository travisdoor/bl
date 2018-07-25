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

### Base rules
Declarations and mutability.
		
		<name> <type>; 				// mutable declaration
		<name> [type] := <value>; 	// mutable declaration
		<name> [type] : <value>; 	// immutable declaration

Mutability of declared entity is described by binding symbols after declaration type. Type is optional in case we specify value.

Examples:
		
		// simple variables
		foo i32; 			// declare mutable integer
		foo i32 := 0;		// declare and initialize to 0
		foo i32 : 20; 	// declare immutable constant
		foo : 20;			// same like previous
		
		// struct
		foo_t : struct {
			i i32,
			j i32
		};
		
		// function
		main : fn () i32 {
		}
		
		
		