# Literals

## Simple Literals

```rust
b :: true;         // bool true literal
b :: false;        // bool false literal
ptr : *s32 = null; // *s32 null pointer literal
```

## Integer Literals

Biscuit language provides constant integer literals written in various formats showed in the example bellow. Integer literals have a volatile type, when the desired type is not specified compiler will choose the best type to hold the value. Numbers requiring less space than 32 bits will be implicitly set to *s32*, numbers requiring more space than 31 bits and less space than 64 bits will be set to *s64* and numbers requiring 64 bits will be set to *u64* type. Bigger numbers are not supported and the compiler will complain. When we specify type explicitly (ex.: `foo : u8 : 10;`), an integer literal will inherit the type of the variable (same for function calls where the type is known).

```rust
i     :: 10;      // s32 literal
i_u8  : u8 : 10;  // u8 literal
i_hex :: 0x10;    // s32 literal
i_bin :: 0b1011;  // s32 literal
f     :: 13.43f;  // f32 literal
d     :: 13.43;   // f64 literal
char  :: 'i';     // u8 character literal
```

## String Literals

The string literal is represented as a constant _utf8_ array of bytes stored in the builtin value of *string_view* type.  The content of the array is allocated on the *stack* and cannot be changed in runtime.

```rust
the_name :: "Martin Dorazil";
```

A longer string value may be split into multiple lines, the result value is evaluated as a single string. 

```rust
text :: 
    "This "
    "is "
    "long "
    "text on multiple "
    "lines.";
```
