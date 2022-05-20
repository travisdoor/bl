# Print

`#import "std/print"`

Printing functions collection.

## PRINT_MAX_LENGTH

```c
PRINT_MAX_LENGTH :: 4096
```

Size of buffer used by `print` function, this is maximum text length which can be printed at once.



*File: print.bl*


## print

```c
print :: fn (format: string_view, args: ...) s32
```

Write string to the standart output (stdout). Format string can include format specifiers `%` 
which are replaced by corresponding argument value passed in `args`. Value-string conversion is 
done automatically, we can pass values of any type as an arguments, even structures or arrays.

The `print` function accepts C-like escape sequences as `\n`, `\t`, `\r`, etc.

Pointers to `Error` are dereferenced automatically; so the `print` function can print out errors 
directly.

Count of printed bytes is returned.

### Example
```
main :: fn () s32 {
    print("Hello world!\n");
    print("My name is '%'.\n", "Travis");
    print("Number: %\n", 10);

    Foo :: struct {
        i: s32;
        j: s32;
    };

    foo := Foo.{10, 20};
    print("foo = '%'\n", foo);

    return 0;
}
```




*File: print.bl*


## eprint

```c
eprint :: fn (format: string_view, args: ...) s32
```

Write string to the error output (stderr).
See also [print](#print).




*File: print.bl*


## bprint

```c
bprint :: fn (buf: []u8, format: string_view, args: ...) s32
```

Write formatted input to the buffer. When passed buffer has not enough space to handle whole
resulting string and terminal character, function will print just part tting into the buffer.

Returns count of characters written into buffer, this count does not include terminal character 
written at the end of the result string.

**note**: String written into the buffer is always zero terminated.

See also [print](#print).




*File: print.bl*


## tprint

```c
tprint :: fn (format: string_view, args: ...) string #inline
```

Write formatted input to the new string using `tmp_allocator`.

**note**: There is no need to explicitly release memory used by temporary string.

**note**: Created string is always zero terminated.

See also [print](#print).




*File: print.bl*


## sprint

```c
sprint :: fn (format: string_view, args: ...) string
```

Write formatted input to the new heap-allocated string. 

**note**: Use `std.str_delete` to free memory used by string.

**note**: Created string is always zero terminated.

See also [print](#print).




*File: print.bl*


## FmtReal

```c
FmtReal :: struct {
    trailing: s8;
    v: Any;
}
```

Structure to hold information about custom real print formatting. Use [fmt_real](#fmt_real) 
function to create formatted printable value.



### Members
* `trailing` - Count of trailing zeros. When this value is less than
zero, default (6) trailing will be used.

* `v` - Value.


*File: print.bl*


## fmt_real

```c
fmt_real :: fn (v: Any, trailing: s8) FmtReal #inline
```

Create formatted printable object for real number. Created [FmtReal](#fmtreal) object is valid 



*File: print.bl*


## FmtIntBase

```c
FmtIntBase :: enum u8 {
    BIN;
    DEC;
    OCT;
    HEX;
}
```

Number base used for formatted printing.


### Variants
* `BIN` - Format as binary number.
* `DEC` - Format as decimal number.
* `OCT` - Format as octal number.
* `HEX` - Format as haxadecimal number.


*File: print.bl*


## FmtInt

```c
FmtInt :: struct {
    base: FmtIntBase;
    print_prefix: bool;
    v: Any;
}
```

Specify number printing format. Use [fmt_int](#fmt_int) helper function to create instance of 
this type.



### Members
* `base` - Numeric base.
* `print_prefix` - Prints prefix based on desired numeric base.

  * `0b` for binary.
  * `0` for octal.
  * `0x` for hexadecimal.


* `v` - Printed value.


*File: print.bl*


## fmt_int

```c
fmt_int :: fn (v: Any, base: FmtIntBase, print_prefix :: ) FmtInt #inline
```

Create formatted printable object for number. Created [FmtInt](#fmtint) object is valid for 
direct printing.




*File: print.bl*


## FmtChar

```c
FmtChar :: struct {
    v: u8;
}
```

Simple wrapper used for format `u8` value as character.



*File: print.bl*


## fmt_char

```c
fmt_char :: fn (v: u8) FmtChar #inline
```

Create formatter for `u8` number to be later printed as character.

### Example
```
print("% = %\n", 'c', fmt_char('c'));
```




*File: print.bl*

