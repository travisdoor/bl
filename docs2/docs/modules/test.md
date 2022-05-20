# Test

`#load "std/test.bl"`

Integrated unit testing API.

## test_eq

```c
test_eq :: fn { fn (got: string_view, expected: string_view, loc :: ) ; fn (got: string, expected: string_view, loc :: ) ; fn (got: string_view, expected: string, loc :: ) ; fn (got: string, expected: string, loc :: ) ; fn (got: f32, expected: f32, epsilon : f32: , loc :: ) ; fn (got: f64, expected: f64, epsilon : f64: , loc :: ) ; fn (got: ?T, expected: T, loc :: ) ; }
```

### Overloads

```
fn (got: string_view, expected: string_view, loc := #call_location)
fn (got: string, expected: string_view, loc := #call_location)
fn (got: string_view, expected: string, loc := #call_location)
fn (got: string, expected: string, loc := #call_location)
fn (got: f32, expected: f32, epsilon: f32 = std.F32_EPSILON, loc := #call_location)
fn (got: f64, expected: f64, epsilon: f64 = std.F64_EPSILON, loc := #call_location)
fn (got: ?T, expected: T, loc := #call_location)
```

Test whether `got` value is equal to `expected` value.




*File: test.bl*


## test_neq

```c
test_neq :: fn { fn (got: string_view, expected: string_view, loc :: ) ; fn (got: string, expected: string_view, loc :: ) ; fn (got: string_view, expected: string, loc :: ) ; fn (got: string, expected: string, loc :: ) ; fn (got: f32, not_expected: f32, epsilon : f32: , loc :: ) ; fn (got: f64, not_expected: f64, epsilon : f64: , loc :: ) ; fn (got: ?T, not_expected: T, loc :: ) ; }
```

### Overloads

```
fn (got: string, not_expected: string, loc := #call_location)
fn (got: string, expected: string_view, loc := #call_location)
fn (got: string_view, expected: string, loc := #call_location)
fn (got: string, expected: string, loc := #call_location)
fn (got: f32, not_expected: f32, epsilon: f32 = std.F32_EPSILON, loc := #call_location)
fn (got: f64, not_expected: f64, epsilon: f64 = std.F64_EPSILON, loc := #call_location)
fn (got: ?T, not_expected: T, loc := #call_location)
```

Test whether `got` value is not equal to `not_expected` value.




*File: test.bl*


## test_gt

```c
test_gt :: fn (v1: ?T, v2: T, loc :: ) 
```

Test whether `v1` is greater than `v2`



*File: test.bl*


## test_lt

```c
test_lt :: fn (v1: ?T, v2: T, loc :: ) 
```

Test whether `v1` is less than `v2`



*File: test.bl*


## test_true

```c
test_true :: fn (v: bool, loc :: ) 
```

Test whether `v` value is `true`.



*File: test.bl*


## test_false

```c
test_false :: fn (v: bool, loc :: ) 
```

Test whether `v` value is `false`.



*File: test.bl*


## test_null

```c
test_null :: fn (ptr: *?T, loc :: ) 
```

Test whether `ptr` pointer value is `null`.



*File: test.bl*


## test_not_null

```c
test_not_null :: fn (ptr: *?T, loc :: ) 
```

Test whether `ptr` pointer value is not `null`.



*File: test.bl*


## test_ok

```c
test_ok :: fn (err: Error, loc :: )  #inline
```

Test whether `err` is OK.



*File: test.bl*


## test_not_ok

```c
test_not_ok :: fn (err: Error, loc :: )  #inline
```

Test whether `err` is not OK.



*File: test.bl*


## test_is_error

```c
test_is_error :: fn (got_err: Error, expected_code: s32, loc :: )  #inline
```

Test whether `got_err` code is `expected_code`.



*File: test.bl*


## test_run

```c
test_run :: fn (print_results :: ) s32
```

Execute all registered test cases in current assembly. The `test_run` function uses compiler
builtin `testcases()` function returning slice of all registered test cases in the current 
assembly (all functions with hash directive `#test`).

Test case execution can be used in runtime and compile time, in both cases function remap 
default behaviour of `panic()` function call and try to report all failing tests without 
termination.

Formatted output containing information about every test run status and summary report is 
produced during execution of this function. In case of test failure all needed information 
about fail source location is reported into standard output.

Returns number of failed tests.

### Example
    
```
#load "std/test.bl"

my_test :: fn () #test {
    print("Hello from test case!!!\n");     
}

main :: fn () s32 {
    return test_run(); 
} 
```




*File: test.bl*

