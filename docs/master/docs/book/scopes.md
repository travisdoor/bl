# Code Structure

A BL code is organized into scopes similar to C++ namespaces. Each scope groups entities inside, and can limit their visibility from the outside. Scopes can be used also to avoid naming collisions. They are organized into a tree structure, where one scope may contain a bunch of other scopes. In this chapter, we describe all possible scope kinds in detail.

## Global Scope

The global scope is created implicitly and encloses all symbols and other scopes in the program. It's the top scope without any parent scope. You can use `#load` or `#import` directives to add symbols from other files in the global scope of our program. Consider we have two files, one called *utils.bl* containing the *my_print_log* function declared in global scope, and the other file containing the *main* function. We can use a `#load` followed by the file name to make the *my_print_log* function available in the *main*. The `#load` should be followed by the *filepath* relative to the file we're loading from, but it can be any *absolute* existing path to the other file. The `#import` is meant to be used with [modules](/book/modules).

```rust
// utils.bl
my_print_log :: fn (text: string_view) {}
```

```rust
// main.bl
#load "utils.bl"

main :: fn () s32 {
	my_print_log("Hello");
	return 0;
}
```

### Load

```rust
#load "<path/to/your/file.bl>"
```

Loads source code in a single file into the current global scope of your project. Every file is loaded only once even if we load it from multiple locations. The *filepath* may be any existing absolute or relative path, in case the path is relative, the compiler lookup the file using the following lookup order (the first hit is used):

** Lookup order: **

- Current file parent directory.
- BL API directory set in `install-location/etc/bl.yaml`.
- System *PATH* environment variable.

### Import

```rust
#import "<path/to/your/module>"
```

The import is supposed to be used with BL [modules](/book/modules).

See also [Module Import Policy](/modules/build/#moduleimportpolicy).

## Named Scope

The named scope may be introduced in a source file if we want to prevent possible name collisions with other symbols in the global scope. In the previous example, we've introduced `my_print_log`  function, but we probably don't want to call it like this in a production code; we prefer only `print_log` name. The problem is, the `print_log` function already exists in the *standard library* and is imported by default. To fix this we may enclose our function into named scope called *utils*.

```rust
// utils.bl
#scope utils

print_log :: fn (text: string_view) {}
```

```rust
// main.bl
#load "utils.bl"

main :: fn () s32 {
	// Print is now nested in 'utils' scope.
	utils.print_log("Hello");
	return 0;
}
```
The named scope in *utils.bl* file is now introduced by `#scope` directive followed by the scope name. Everything in the file after the `#scope` directive is added into *utils* named scope. There is currently no possibility to create named scope nested in another named scope; this restriction mainly exists only to keep the scope structure relatively flat.

As you can see, the `print_log` function is now accessible only through `.` operator and it's nested in the named scope called *utils*.

## Private Scope

The *private* scope may be created by `#private` directive in a source file, everything declared after this directive is visible only inside the file. The *private* scope may exist only once in each file. Note there is no *public* scope available in the BL everything outside the *private* scope is *public* and we cannot switch back to *public* scope once we're in the *private* one.

The main purpose of a *private* scope is to hide some internal implementations which should not be accessible from the outside world.

```rust
// utils.bl
#scope utils

print_log :: fn (text: string_view) {
	// We can access the private stuff since it's in the same file.
	set_output_color(Color.BLUE);
	defer set_output_color(Color.NORMAL);
	// Use default print to print the message.
	print(text);
}

#private
// All following code is visible only inside the current file.

Color :: enum {
	NORMAL;
	RED;
	BLUE;
}

set_output_color :: fn (color: Color) {
	switch color {
		Color.NORMAL { ... }
		Color.RED { ... }
		Color.BLUE { ... }
	}
}
```

## Local Scope

Local scopes are created implicitly for each function since the usage of symbols, declared in the function, is strictly limited to be used only inside that function. The compiler creates local scopes also for structures, unions and enums, where the content of those is again accessible using the `.` operator.

## Using

The *using* statement may be added in local scopes to reduce the amount of code you have to write. It makes the content of "used" scope directly available in the scope the *using* is living in. Be careful using this feature, it may introduce symbol ambiguity and the compiler may complain.

Currently, the *using* supports named scopes and enums, it's not allowed for structures and unions, since it may make the code less readable.

```rust
// main.bl
#load "utils.bl"

main :: fn () s32 {
	using utils;

	// This is not valid, compiler don't known if you mean 'print_log' from the
	// standard library or the one from utils.bl.
	print_log("Hello");
	return 0;
}
```

```text
main.bl:9:5: error(0078): Symbol is ambiguous.
   8 |     // standard library or the one from utils.bl.
>  9 |     print_log("Hello");
     |     ^^^^^^^^^
  10 |     return 0;

debug.bl:38:1: First declaration found here.
   37 | /// Print debug log using current application_context `print_log_fn` function.
>  38 | print_log :: fn (format: string_view, args: ...) #inline {
      | ^^^^^^^^^
   39 |     application_context.print_log_fn(PrintLogKind.MESSAGE, "", 0, format, args);

utils.bl:4:1: Another declaration found here.
   3 |
>  4 | print_log :: fn (text: string_view) {
     | ^^^^^^^^^
   5 |     // We can access the private stuff since it's in the same file.
```

However, we can use this feature in *utils.bl* like this:

```rust
set_output_color :: fn (color: Color) {
    using Color;
	// Content of Color is now available in current scope.

    switch color {
    	// No need to write 'Color.' before each variant.
        NORMAL {}
        RED {}
        BLUE {}
    }
}
```