# Builtin

Compiler builtins are automatically loaded into every assembly.

## Current running platform
```c
Platform :: enum s32 {
    UNKNOWN;
    WINDOWS;
    DARWIN;
    LINUX;
}

PLATFORM :: <SET_BY_COMPILER>;
```

## Current running architecture
```c
Arch :: enum s32 {
    UNKNOWN;
    X86_64;
    AARCH64;
    ARM64;
}

ARCH :: <SET_BY_COMPILER>;
```

## Current running environment
```c
Env :: enum s32 {
    UNKNOWN;
    MSVC;
    GNU;
    MUSL;
}

ENV :: <SET_BY_COMPILER>;
```
## Compiler version
```c
BLC_VER_MAJOR : s32 : <SET_BY_COMPILER>;
BLC_VER_MINOR : s32 : <SET_BY_COMPILER>;
BLC_VER_PATCH : s32 : <SET_BY_COMPILER>;
```

## string_view

```c
string_view :: []u8
```

Builtin string slice.



*File: a.bl*


## TypeKind

```c
TypeKind :: enum {
    TYPE :: 1;
    VOID :: 2;
    INT :: 3;
    REAL :: 4;
    FN :: 5;
    PTR :: 6;
    BOOL :: 7;
    ARRAY :: 8;
    STRUCT :: 9;
    ENUM :: 10;
    NULL :: 11;
    STRING :: 12;
    FN_GROUP :: 16;
}
```

TypeKind describes kind of BL type returned in `TypeInfo` structure. This value can be used for 
safe casting.



### Variants
* `TYPE` - Base type of all types in type system.
* `VOID` - Void type. (Implicitly used for functions without return value)
* `INT` - Any integer type: `s8`, `s16`, `s32`, `s64`, `u8`, `u16`, `u32`, `u64`, `usize`.
* `REAL` - Any real type: `f32`, `f64`.
* `FN` - Function type. 
* `PTR` - Pointer type. 
* `BOOL` - Boolean type. 
* `ARRAY` - Array type. 
* `STRUCT` - Structure type. 
* `ENUM` - Enumerator type. 
* `NULL` - Null-value type. 
* `STRING` - String type. @Cleanup: remove this!
* `FN_GROUP` - Function group type. 


*File: a.bl*


## TypeInfo

```c
TypeInfo :: struct {
    kind: TypeKind;
    size_bytes: usize;
}
```

Base `TypeInfo` structure returned by `typeinfo` operator. This structure pointer can be casted to 
child type to get more descriptive information about the type.



### Members
* `kind` - Type info kind.
* `size_bytes` - Size of type in bytes.


*File: a.bl*


## TypeInfoInt

```c
TypeInfoInt :: struct {
    bit_count: s32;
    is_signed: bool;
}
```

Detailed information about integer types: `s8`, `s16`, `s32`, `s64`, `u8`, `u16`, `u32`, `u64`, `usize`.


### Members
* `bit_count` - Size of type in bits.
* `is_signed` - True when type is signed integer type.


*File: a.bl*


## TypeInfoReal

```c
TypeInfoReal :: struct {
    bit_count: s32;
}
```

Detailed information about real types: `f32`, `f64`.


### Members
* `bit_count` - Size of type in bits.


*File: a.bl*


## TypeInfoFn

```c
TypeInfoFn :: struct {
    args: []TypeInfoFnArg;
    ret_type: *TypeInfo;
    is_vargs: bool;
}
```

Detailed information about function types.


### Members
* `args` - Slice of argument type infos.
* `ret_type` - Return type info.
* `is_vargs` - True when function has variable argument list.


*File: a.bl*


## TypeInfoFnGroup

```c
TypeInfoFnGroup :: struct {
    variants: []*TypeInfoFn;
}
```

Detailed information about function group and contained possible overloads.


### Members
* `variants` - Group content. 


*File: a.bl*


## TypeInfoPtr

```c
TypeInfoPtr :: struct {
    pointee_type: *TypeInfo;
}
```

Detailed information about pointer types.


### Members
* `pointee_type` - Underlaying pointed type info.


*File: a.bl*


## TypeInfoArray

```c
TypeInfoArray :: struct {
    name: string_view;
    elem_type: *TypeInfo;
    len: s64;
}
```

Detailed information about array types.


### Members
* `name` - Array name.
* `elem_type` - Array element type info.
* `len` - Array element count.


*File: a.bl*


## TypeInfoStruct

```c
TypeInfoStruct :: struct {
    name: string_view;
    members: []TypeInfoStructMember;
    is_slice: bool;
    is_union: bool;
    is_dynamic_array: bool;
}
```

Detailed information about structure types.


### Members
* `name` - Structure type name.
* `members` - Slice of structure member infos.
* `is_slice` - True when structure is slice.
* `is_union` - True when structure is union.
* `is_dynamic_array` - True when structure is dynamic array.


*File: a.bl*


## TypeInfoEnum

```c
TypeInfoEnum :: struct {
    name: string_view;
    base_type: *TypeInfo;
    variants: []TypeInfoEnumVariant;
    is_flags: bool;
}
```

Detailed information about enumerator types.


### Members
* `name` - Enumerator type name.
* `base_type` - Base type info.
* `variants` - Slice of all enumerator variants.
* `is_flags` - True when enumerator was defined with #flags directive.


*File: a.bl*


## TypeInfoVoid

```c
TypeInfoVoid :: struct {
}
```

Placeholder for information about `void` type.



*File: a.bl*


## TypeInfoNull

```c
TypeInfoNull :: struct {
}
```

Placeholder for information about `null` type.



*File: a.bl*


## TypeInfoString

```c
TypeInfoString :: struct {
}
```

Placeholder for information about `string` type.



*File: a.bl*


## TypeInfoType

```c
TypeInfoType :: struct {
}
```

Placeholder for information about `type` type.



*File: a.bl*


## TypeInfoBool

```c
TypeInfoBool :: struct {
}
```

Placeholder for information about `bool` type.



*File: a.bl*


## TypeInfoStructMember

```c
TypeInfoStructMember :: struct {
    name: string_view;
    base_type: *TypeInfo;
    offset_bytes: s32;
    index: s32;
    tag: u64;
    is_base: bool;
}
```

Detailed information about structure member.


### Members
* `name` - Member type name.
* `base_type` - Member type info.
* `offset_bytes` - Byte-offset of member inside structure ABI.
* `index` - Order in structure.
* `tag` - User defined member tag can be used to store any custom user data up to 8 bytes into the type
info structure when structure member is defined.

```
Foo :: struct {
    i: s32 #tag 123;
    j: s32 #tag 321;
};
```

* `is_base` - True when member is inherrited base of the parent structure type.


*File: a.bl*


## TypeInfoEnumVariant

```c
TypeInfoEnumVariant :: struct {
    name: string_view;
    value: s64;
}
```

Detailed information about enumerator variant.


### Members
* `name` - Member type name.
* `value` - Variant value.


*File: a.bl*


## TypeInfoFnArg

```c
TypeInfoFnArg :: struct {
    name: string_view;
    base_type: *TypeInfo;
}
```

Detailed information about function's argument.


### Members
* `name` - Member type name.
* `base_type` - Argument type info.


*File: a.bl*


## Any

```c
Any :: struct {
    type_info: *TypeInfo;
    data: *u8;
}
```

Any type is special builtin type used for passing value of "any" type as function argument. 


### Members
* `type_info` - Type info associated to data type.
* `data` - Data pointer. (not owner!)


*File: a.bl*


## TestCase

```c
TestCase :: struct {
    func: *fn () ;
    name: string_view;
    file: string_view;
    line: s32;
}
```

Type of test case record found during compilation.


### Members
* `func` - Pointer to test case function.
* `name` - Name of the test case. 
* `file` - File where the test case is declared.
* `line` - Line in the file where the test case is declared.


*File: a.bl*


## CodeLocation

```c
CodeLocation :: struct {
    file: string_view;
    line: s32;
    hash: u32;
}
```

Type of source code location used by `#call_location` directive.

### Example
```
foo :: fn (loc: *CodeLocation = #call_location) {
    print("%\n", @loc);
}

main :: fn () s32 {
    foo();
    return 0;
}
```



### Members
* `file` - Full source file path.
* `line` - Line in the source file.
* `hash` - File and line combination hash.


*File: a.bl*


## PrintLogFn

```c
PrintLogFn :: *fn (kind: PrintLogKind, file: string_view, line: s32, format: string_view, args: ...) 
```

Type of print log function.

### Arguments
* `kind` Kind of report message. 
* `format` Format string. 
* `args` Additional arguments. 
* `file` Call side filename. 
* `line` Call side line in source file. 




*File: a.bl*


## AbortFn

```c
AbortFn :: *fn () 
```



*File: a.bl*


## Context

```c
Context :: struct {
    print_log_fn: PrintLogFn;
    abort_fn: AbortFn;
    allocator: *Allocator;
    temporary_allocator: *Allocator;
}
```

Default implicit context type. Implicit context is compiler internal global variable 
containing basic context for whole assembly. This variable is mutable and can 
be modified by user code. 



### Members
* `print_log_fn` - Pointer to log print function. 
* `abort_fn` - Pointer to abort handler function. 
* `allocator` - Default memory allocator used across the application.


*File: a.bl*


## application_context

```c
application_context := 
```

Default application context. Implicit context is compiler internal global variable
containing basic context for the whole assembly. This variable is mutable and can
be modified by user code. For example we can replace default memory allocator
with custom one, this will affect all memory allocations made after.




*File: a.bl*


## command_line_arguments

```c
command_line_arguments : []string_view = 
```

Contains all arguments passed from command line. First argument is executable name.



*File: a.bl*


## enum_count

```c
enum_count :: fn (T: type) s64
```

Compile-time helper function resolving count of enumerator variants, passed type 'T' must be an
enumerator type (checked by assert).




*File: a.bl*


## is_number

```c
is_number :: fn (T: type) bool
```

Returns `true` if the `T` type is an integer type.



*File: a.bl*


## is_real_or_number

```c
is_real_or_number :: fn (T: type) bool
```

Returns `true` if the `T` type is an integer or floating point number type (f32 or f64).



*File: a.bl*


## is_signed_number

```c
is_signed_number :: fn (T: type) bool
```

Returns true if the `T` type is a signed integer type.



*File: a.bl*


## is_pointer

```c
is_pointer :: fn (T: type) bool
```

Returns true if the `T` type is a pointer type.



*File: a.bl*


## is_function

```c
is_function :: fn (T: type) bool
```

Returns true if the `T` type is a function type.



*File: a.bl*


## is_struct

```c
is_struct :: fn (T: type) bool
```

Returns true if the `T` type is a struct type.



*File: a.bl*


## typeid

```c
typeid :: fn (v: type) u64
```

Returns unique type identificator in compile-time.



*File: a.bl*


## is_enum

```c
is_enum :: fn (T: type) bool
```

Returns `true` if the `T` type is an enumerator type.



*File: a.bl*


## number_type

```c
number_type :: fn (size_bytes: usize, is_signed: bool) type
```

Returns signed or unsigned builtin number type of the requested size.
The `size_bytes` must be 1, 2, 4 or 8 Bytes.




*File: a.bl*

