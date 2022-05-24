# String

`#import "std/string"`

Builtin dynami string container. 

```
string :: struct {
    len: s64;
    ptr: *u8;
}


// Compile-time string literal
my_string :: "This is my string!"; 


// Dynamic string allocated on heap.
// New dynamic string can be created from string literal. 
my_string :: std.str_new("This is my string!");
defer std.str_delete(&my_string);
``` 

BL strings are zero terminated except of sub-string view function (terminator cannot be added 
because we cannot modify original string).

**note**: String manipulation functions are related only to dynamic strings and cannot be used 
with string literals since those are constants allocated on stack.

## std.str_new

```c
str_new :: fn { fn (allocator : *Allocator: ) string; fn (size: s64, allocator : *Allocator: ) string; fn (v: string_view, allocator : *Allocator: ) string; fn (cstr: *u8, allocator : *Allocator: ) string; }
```

Overloaded function creating new dynamic string instance. Created string is guaranteed to be 
zero terminated.

**Overloads:**
```c
fn (allocator: *Allocator = null) string 
fn (size: s64, allocator: *Allocator = null) string 
fn (v: string_view, allocator: *Allocator = null) string
fn (cstr: *u8, allocator: *Allocator = null) string
```




*File: string.bl*


## std.str_delete

```c
str_delete :: fn (v: *string) 
```

Delete dynamic string.



*File: string.bl*


## std.str_reserve

```c
str_reserve :: fn (v: *string, count: s64) 
```

Ensure that string's underlying buffer is capable to hold `count` of characters and eventually 
do preallocation if buffer is too small. The reserve does not take in account if the string's 
buffer is full or not. The function does nothing if buffer size is enough.




*File: string.bl*


## std.str_get_allocated_bytes

```c
str_get_allocated_bytes :: fn (v: string) usize #inline
```

Get currently allocated memory used by the string in bytes.



*File: string.bl*


## std.str_clear

```c
str_clear :: fn (v: *string) 
```

Clear dynamic string but keep allocated storage.



*File: string.bl*


## std.str_append

```c
str_append :: fn { _str_append_str; _str_append_any; }
```

Append dynamic string with any value. Allocation is done in case there is not enough space 
reminding in string. The `v` string is supposed to be allocated on heap.

Returns count of characters appended to the string excluding zero terminator.




*File: string.bl*


## std.str_concat

```c
str_concat :: fn (v: *string, args: ...) string
```

Append string with multiple values passed as `args` and return `@v`. New allocation is done in 
case there is space no left in currently allocated string's memory to append `args`.




*File: string.bl*


## std.str_clear_concat

```c
str_clear_concat :: fn (v: *string, args: ...) string
```

Clear the `v` string and append multiple values passed as `args`. Returned value is dereference 
of the original `v` string after new values are appended. New allocation is done in case there 
is space no left in currently allocated
string's memory to append `args`.

**hint**: Use this function in case you need create only temporary string i.e. file path.

```
VERSION :: "1.0.0";

path := std.str_new();
defer std.str_delete(path);
std.set_cwd(std.str_clear_concat(&path, "my-dir/project-", VERSION));
```




*File: string.bl*


## std.str_match

```c
str_match :: fn (first: string_view, second: string_view, n :: -1) bool
```

Compare `first` and `second` strings in specified range `n` and return `true` if they are the same 
otherwise return `false`.

Range value `n` is optional and ignored when it's less than 0.




*File: string.bl*


## std.str_compare

```c
str_compare :: fn (first: string_view, second: string_view) s32
```



*File: string.bl*


## std.str_to_f32

```c
str_to_f32 :: fn (str: string_view, count :: -1) f32
```

Convert first `count` characters of `str` to `f32`.



*File: string.bl*


## std.str_to_s64

```c
str_to_s64 :: fn (str: string_view, count :: -1) s64
```

Convert first `count` characters from `str` to `s64`.



*File: string.bl*


## std.str_split_by_last

```c
str_split_by_last :: fn (str: string_view, delimiter: u8, lhs: *string_view, rhs : *string_view: , di : *s32: ) bool
```

Split input string `str` into two tokens based on the last occurrence of `delimiter`. Delimiter 
is not included in resulting tokens. Result tokens only points into original memory of the 
`str`, they are not supposed to be freed.

When delimiter is not present in the input string function return `false`, `lhs` is set to the
original `str` string, `rhs` value is unchanged.

Token destination pointers `lhs` and `rhs` are optional. The `di` output variable is set to 
index of the split position when it's not `null`.

**warning**: `lhs` and `rhs` sub strings are not guaranteed to be zero terminated and they are 
not supposed to be freed. 

### Example

```
main :: fn () s32 {
    lhs: string_view;
    rhs: string_view;
    if std.str_split_by_last("this/is/my/epic/path", '/', &lhs, &rhs) {
        print("lhs = %\n", lhs);
        print("rhs = %\n", rhs);
    }

    return 0;
}
```




*File: string.bl*


## std.str_split_at_index

```c
str_split_at_index :: fn (str: string_view, index: s32, lhs : *string_view: , rhs : *string_view: ) bool
```

Split input string `str` at index position and return true when split was done. Result tokens 
only points into original memory of the `str`, they are not supposed to be freed. When index is 
out of `str` range function return `false`, `lhs` and `rhs` buffers are not modified.

Token destination pointers `lhs` and `rhs` are optional. 

**warning**: `lhs` and `rhs` sub strings are not guaranteed to be zero terminated and they are 
not supposed to be freed. 
 
### Example

```
main :: fn () s32 {
    lhs: string_view;
    rhs: string_view;
    if std.str_split_at_index("foobar", 3, &lhs, &rhs) {
        print("lhs = %\n", lhs);
        print("rhs = %\n", rhs);
    }

    return 0;
}
```




*File: string.bl*


## std.str_split_by_first

```c
str_split_by_first :: fn (str: string_view, delimiter: u8, lhs: *string_view, rhs : *string_view: , di : *s32: ) bool
```

Split input string `str` into two tokens based on the first occurrence of `delimiter`. Delimiter 
is not included in resulting tokens. Result tokens only points into original memory of the 
`str`, they are not supposed to be freed.

When delimiter is not present in the input string function return `false`, `lhs` is set to the
original `str` string, `rhs` value is unchanged.

Token destination pointers `lhs` and `rhs` are optional. 

**warning**: `lhs` and `rhs` sub strings are not guaranteed to be zero terminated and they are 
not supposed to be freed. 
 
### Example

```
main :: fn () s32 {
    lhs: string_view;
    rhs: string_view;
    if std.str_split_by_first("this/is/my/epic/path", '/', &lhs, &rhs) {
        print("lhs = %\n", lhs);
        print("rhs = %\n", rhs);
    }

    return 0;
}
```




*File: string.bl*


## std.str_insert

```c
str_insert :: fn { _string_insert; _char_insert; }
```

Overloaded function inserting one character or other string at desired position.
 
**Overloads:**
```c
fn (str: *string, index: s32, v: u8) bool #inline
fn (str: *string, index: s32, v: string_view) bool
```

**note**: Function does nothing (return `false`) when `v` string is empty.




*File: string.bl*


## std.str_erase

```c
str_erase :: fn (str: *string, index: s32) bool
```

Erase one character at `index` position and return true when character was erased. The '`index` 
value is checked to fit in string bounds.




*File: string.bl*


## std.str_split_by

```c
str_split_by :: fn (str: string_view, delimiter: u8) []string_view
```

Split the `str` input string by delimiter and return new slice containing all found sub-strings. 

**warning**: String slice should be terminated by `slice_terminate` call.

**warning**: Slice elements are not guaranteed to be zero terminated and they are not supposed 
to be freed. 




*File: string.bl*


## std.str_count_of

```c
str_count_of :: fn (str: string_view, c: u8) s32 #inline
```

Counts desired character occurrence in the input string.



*File: string.bl*


## std.str_to_lower

```c
str_to_lower :: fn (str: string_view)  #inline
```

Converts input string to lower case.



*File: string.bl*


## std.str_to_upper

```c
str_to_upper :: fn (str: string_view)  #inline
```

Converts input string to upper case.



*File: string.bl*


## std.str_replace_all

```c
str_replace_all :: fn (str: *string, c: u8, with :: ) s32
```

Replace all found occurrences of character `c` in the input string with `with` character and 
return count of replacements made. This function cannot be used with constant string literals 
as input.

If `with` replacement is 0 character, all `c` occurrences will be erased from the string.

Function return count of replaced characters or zero.




*File: string.bl*


## std.str_hash

```c
str_hash :: fn (str: string_view) u32
```

Calculates string `u32` hash.



*File: string.bl*


## std.str_is_null

```c
str_is_null :: fn (s: string_view) bool #inline
```

Helper inline function returning `true` when string is null. In such case string `len` could be 
any value.




*File: string.bl*


## std.str_is_empty

```c
str_is_empty :: fn (s: string_view) bool #inline
```

Helper inline function returning `true` when string is empty. In such case string `ptr` could 
be any pointer.




*File: string.bl*


## std.str_empty

```c
str_empty :: 
```



*File: string.bl*


## std.str_view_empty

```c
str_view_empty :: 
```



*File: string.bl*


## std.str_is_null_or_empty

```c
str_is_null_or_empty :: fn (s: string_view) bool #inline
```

Helper inline function returning `true` when string is empty and null. 



*File: string.bl*


## std.str_sub

```c
str_sub :: fn (s: string_view, start: s64, len : s64: -1) string_view #inline
```

Creates substring from passed string starting at `start` index of input string and ending at 
`start` + `len` index.

Starting index `start` must be greater than 0 and less than `str.len`. `len` specifies optional 
length of substring. When not specified, length from `start` to the end of the `str` is used.

**warning**: Result sub-string is not guaranteed to be zero terminated and it's not supposed to
be freed.




*File: string.bl*


## std.str_is_zero_terminated

```c
str_is_zero_terminated :: fn (str: string_view) bool
```

Checks whether string is zero terminated.



*File: string.bl*


## std.strtoc

```c
strtoc :: fn (str: string_view) *C.char #inline
```

Converts string view into C string representation. The original string `str` must be zero-terminated
(checked by assert).




*File: string.bl*


## std.ctostr

```c
ctostr :: fn (cstr: *C.char, len : s64: -1) string_view #inline
```

Converts C string into string view. The `len` argument is optional and is used as string length if
it's greater than -1, otherwise the strlen is used.

Empty string view is returned when the input C string is null.




*File: string.bl*

