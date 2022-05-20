# Utils

`#load "std/utils.bl"`

Set of various utility functions.

## set_flag

```c
set_flag :: fn (flags: *?T, flag: T) T #inline
```

Set `flag` in `flags` input. This function is valid for numeric and enum types (checked by assert).



*File: utils.bl*


## clr_flag

```c
clr_flag :: fn (flags: *?T, flag: T) T #inline
```

Clear `flag` in `flags` input. This function is valid for numeric and enum types (checked by assert).



*File: utils.bl*


## is_flag

```c
is_flag :: fn (flags: ?T, flag: T) bool #inline
```

Check whether `flag` is set in `flags`. This function is valid for numeric and enum types 
(checked by assert).




*File: utils.bl*


## make_flags

```c
make_flags :: fn (f1: ?T, other: ...T) T
```



*File: utils.bl*


## ptr_shift_bytes

```c
ptr_shift_bytes :: fn (ptr: *?T, bytes: s64) *u8 #inline
```

Produce right-shift of input `ptr` by count of `bytes`.



*File: utils.bl*


## ptr_diff

```c
ptr_diff :: fn (a: *?T1, b: *?T2) s64 #inline
```

Calculates pointer difference `a` - `b`.



*File: utils.bl*


## env_get

```c
env_get :: fn (var: string_view) string
```

Reads environment variable specified by `var` name. Result is empty in case no such variable was 
found or has no content. It's caller responsibility to delete result string.




*File: utils.bl*


## env_set

```c
env_set :: fn (var: string_view, value: string_view)  #inline
```

Sets environment variable.



*File: utils.bl*


## random_seed_time

```c
random_seed_time :: fn ()  #inline
```

Sets seed for `std.rand` or utility function [random_number](#random_number) based on current 
system tick time.




*File: utils.bl*


## random_number

```c
random_number :: fn (min :: 0, max :: 1) s32 #inline
```

Generates random number in specified range <min, max> using standard libc rand generator.
Random number generator seed is supposed to be set by :ref:`random_seed_time` or by `std.srand` 
call.




*File: utils.bl*


## sort

```c
sort :: fn (list: []?T, cmp: *fn (a: *T, b: *T) bool) 
```

Polymorph slice sorting utility.
@INCOMPLETE




*File: utils.bl*


## find_if

```c
find_if :: fn (arr: []?T, func: *fn (: *T) bool) (value: *T, index: s64
) #inline
```

Iterate over `arr` slice and return pointer to the value and it's index if `func` validator
function returs true.

The `func` is called for every element in the `arr` slice and pointer to the current element
is passed into this function.

In case no element was found, function returns null pointer and -1 index.




*File: utils.bl*


## hash_combine

```c
hash_combine :: fn (first: ?T, second: T, more: ...T) T #inline
```

Combine two or more hashes into one, T is expected to be an integer type (checked by static
assert).




*File: utils.bl*


## is_power_of_two

```c
is_power_of_two :: fn (n: usize) bool #inline
```



*File: utils.bl*

