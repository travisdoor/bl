# Debug

`#load "std/debug.bl"`

Set of common debugging tools.
@Incomplete: documentation of `debugbreak` builtin.

## print_log

```c
print_log :: fn (format: string_view, args: ...)  #inline
```

Print debug log using current application_context `print_log_fn` function.



*File: debug.bl*


## print_warn

```c
print_warn :: fn (format: string_view, args: ...)  #inline
```

Print debug warning using current application_context `print_log_fn` function.



*File: debug.bl*


## print_err

```c
print_err :: fn { print_err_e; print_err_msg; }
```

Print debug error using current application_context.print_log_fn function.



*File: debug.bl*


## assert

```c
assert :: fn (cond: bool, loc :: ) 
```

Interrupt execution when `cond` is false. Assert use current application_context `print_log_fn` to print out error
message containing assert location in source code.
**important**: All calls to the assert function are removed from release builds by default.

**warning**: There is no argument validation in case the assert call is removed from compilation.




*File: debug.bl*


## static_assert

```c
static_assert :: fn (expr: bool) 
```

Assertion check evaluated in compile time.



*File: debug.bl*


## panic

```c
panic :: fn { panic_empty; panic_error; panic_msg; }
```

Abort execution and eventually print panic message if there is one specified. First passed 
argument in 'args' will act like format string and can be eventually followed by any additional 
values required.

### Example

```
panic(); // abort without any messages
panic(error); // abort with :ref:`Error`.
panic("oops!"); // abort with message prited out.
panic("Failed with error: %", errn); // first argument passed acts like formating string
```




*File: debug.bl*


## PrintLogKind

```c
PrintLogKind :: enum u8 {
    MESSAGE;
    WARNING;
    ERROR;
    ASSERT;
    PANIC;
}
```

Kinds of log messages.


### Variants
* `MESSAGE` - Ordinary debug log message.
* `WARNING` - Warning log message.
* `ERROR` - Error log message.
* `ASSERT` - Assert log message.
* `PANIC` - Panic log message.


*File: debug.bl*


## measure_elapsed_ms_begin

```c
measure_elapsed_ms_begin :: fn ()  #inline
```

Start measure elapsed milliseconds in the current scope. This function call will push the current
time into the thread-local queue. Measurement must be ended by :ref:`measure_elapsed_ms_end` call.

### Example

```
measure_elapsed_ms_begin();
defer measure_elapsed_ms_end("Name");
```

**warning**: Every `measure_elapsed_ms_begin` must have corresponding 
[measure_elapsed_ms_end](#measure_elapsed_ms_end) call.




*File: debug.bl*


## measure_elapsed_ms_end

```c
measure_elapsed_ms_end :: fn (name :: ) f64 #inline
```

Pop the last start time from the runtime measurement queue and log the result time difference when 
the name is specified. Function return time difference between the current time and last start time.




*File: debug.bl*

