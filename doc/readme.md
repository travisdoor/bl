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

| Name | Size | Value         |
|------|------|---------------|
| i8   | 1B   | signed number |
| i32  | 4B   | signed number |
| i64  | 8B   | signed number |
	
## Functions

	fn add(a i32, b i32) i32 {
	  return a + b;
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
