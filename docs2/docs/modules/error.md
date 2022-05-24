# Error handling

`#load "std/error.bl"`

```
_Error :: struct {
    msg: string_view;
    code: s32;
}

Error :: *_Error;
```

Universal error state representation used across the API.

### Example

```
div :: fn (a: s32, b: s32) (s32, Error) {
    if b == 0 {
        // Return error with message.
        return 0, error("Divide by zero '%/%'!", a, b);
    }
    // Return result and OK state.
    return a / b, ok();
}

main :: fn () s32 {
    loop i := -5; i <= 5; i += 1 {
        result, error :: div(10, i);
        if is_ok(error) {
            print("Result is %.\n", result);
        } else {
            print("Error: %.\n", error);
        }
    }
    return 0;
}
```

## Error

```c
Error :: *_Error
```



*File: error.bl*


## is_ok

```c
is_ok :: fn (err: Error) bool #inline
```

Check whether `err` is no-error valid state. Returns `false` when `err` is `null`.



*File: error.bl*


## is_error

```c
is_error :: fn (err: Error, code: s32) bool #inline
```

Check whether `err` is representing error `code`. Returns `false` when `err` is `null`.



*File: error.bl*


## ok

```c
ok :: fn () Error #inline
```

Creates no-error :ref:`Error`.



*File: error.bl*


## error

```c
error :: fn { error1; error2; error3; }
```

No error.
Overloaded function setting up error state. Error state is global variable holding :ref:`Error` 
instance, `error` function sets desired values and return pointer to this global. That means
the `Error` must be handled immediately after it's returned from failing function since every
`Error` state points to the same memory.

Error creating does not require any HEAP memory alocations.

```
fn (code: s32) Error #inline
```

Sets error code.

***

```
fn (format: string, args: ...) Error #inline
```

Sets error state with `ERR_UNKNOWN` code and formatted message.

***

```
fn (code: s32, format: string, args: ...) Error
```

Sets error state with `code` and formatted message.




*File: error.bl*

