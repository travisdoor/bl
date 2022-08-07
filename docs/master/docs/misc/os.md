# System

Collection of operating system interface imported implicitly.

## Standart IO

Standart input and output is implemented using the builtin [Stream](/modules/io) abstraction, so all common stream manipulation methods
like `read` and `write` can be used. Use following methods to obtain the stream handle.

```c
OsStdIoStream :: struct #base std.Stream {
    handle: win32.HANDLE;
}

// Standart input stream
os_stdin :: fn () *OsStdIoStream
// Standart output stream.
os_stdout :: fn () *OsStdIoStream
// Standart output error stream.
os_stderr :: fn () *OsStdIoStream
```

In general, you can use i.e. Standart Output Stream for printing into the console, however using [print](/modules/print) function is
more elegant in most situations.

## os_execute

```c
os_execute :: fn (command: string_view) s32
```

Execute shell `command` and return the command output state as an integer.

## os_get_last_error

```c
os_get_last_error :: fn () (s32, string_view) #inline
```

Return last known operating system dependent error code.

## os_get_exec_path

```c
os_get_exec_path :: fn () string
```

Returns a full path to the currently running executable; internally a new string is allocated and must be deleted after use.
The path may be empty in case of an error.

