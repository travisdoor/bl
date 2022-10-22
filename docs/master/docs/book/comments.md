# Comments

Simple documentation can be written directly into the code the same way as in other programming languages, simply by adding comments. The BL comments use the same syntax as in C. You can write a single-line comment or multi-line comment as needed. You can also write documentation directly into the code and let the compiler generate *markdown* files for you.

## Single-line Comments
You can use single-line comments introduced by `//` anywhere in the code. Everything up to the end of the line is considered a comment.

```c
// This is the main function.
main :: fn () s32 {
	return 0; // We return 0 here.
}
```

## Multi-line Comments
Multi-line comments are also supported:

```c
/*
	The main function.

	This is the entry point of our program.
*/
main :: fn () s32 /* the signed number is returned from the main */ {
	return 0; 
}
```

## Documentation Comments 

Since the BL compiler supports documentation generation out of the box, you can use two types of comments which are supposed to be a part of a generated document.

1. `//!` Documentation of the file.
2. `///` Documentation of the following symbol. 

See also [Self-Documentation](/book/documentation).