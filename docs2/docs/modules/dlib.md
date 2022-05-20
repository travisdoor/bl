# Dynamic Library Tools

`#import "std/dlib"`

Dynamic library tools for runtime library loading and symbol resolving.

## std.Library

```c
Library :: _dlib_impl.Library
```

Platform specific library handle.



*File: dlib.bl*


## std.library_open

```c
library_open :: fn (lib_name :: ) (_0: Library, _1: Error
) #inline
```

Dynamically loads library specified by `lib_name` and return it's handle or null with error. 
When `lib_name` is empty, current binary will be loaded. Use `library_close` call to close 
library when it's not needed anymore.




*File: dlib.bl*


## std.library_get_symbol

```c
library_get_symbol :: fn (lib: Library, sym_name: string_view) (_0: *u8, _1: Error
) #inline
```

Resolve named symbol in library and return pointer to it.



*File: dlib.bl*


## std.library_close

```c
library_close :: fn (lib: Library)  #inline
```

Close library opened by `lirary_open` call.



*File: dlib.bl*

