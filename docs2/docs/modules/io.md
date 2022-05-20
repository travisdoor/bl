# Input/Ouput

`#import "std/io"`


## std.read

```c
read :: fn (handle: std.File, dest: *u8, size: s64) (_0: s64, _1: Error
) #inline
```

Read `size` bytes from file into raw `dest` buffer. Return count of bytes written into buffer 
and `OK` status when there was no error. `dest` buffer must be allocated to handle at least 
`size` bytes. There is no overflow check.




*File: io.bl*


## std.read_string

```c
read_string :: fn (handle: std.File) (_0: string, _1: Error
)
```

Read whole file content into string. Return new string instance containing file data and `OK` 
status on success, otherwise return empty string and error. Returned string is expected to be 
released by `std.str_delete` call if there was no error reported by function. Result string is 
zero terminated even if file is empty.

### Example

```
#import "std/fs"

main :: fn () s32 {
    // Open this file.
    file, open_err :: std.file_open(#file, std.FileOpenMode.READ);

    // Always check for errors.
    if !is_ok(open_err) {
        panic("Cannot open file with error: '%'!", open_err);
    }
    // Close file at the end of scope.
    defer std.file_close(file);

    // Read it's content.
    content, read_err :: std.read_string(file);

    // Check for errors.
    if !is_ok(read_err) {
        panic("Cannot read file with error: '%'!", read_err);
    }
    // Delete content string at the end of scope.
    defer std.str_delete(&content);

    // Print file content to stdout.
    print("%\n", content);
    return 0;
}
```




*File: io.bl*


## std.read_slice

```c
read_slice :: fn (handle: std.File) (_0: string_view, _1: Error
)
```

Read whole file content into slice array. Return new slice instance containting file data and 
`OK` status on success, otherwise return empty slice and error. Returned slice is expected to 
be released by :ref:`slice_terminate` call if there was no error reported by function.




*File: io.bl*


## std.write

```c
write :: fn (handle: std.File, src: *u8, size: s64) (_0: s64, _1: Error
) #inline
```

Write `size` bytes from raw buffer `src` into the file. Return count of bytes written into the 
buffer and `OK` on success, otherwise return error. The `src` buffer size must be at least 
`size` bytes. No overflow checking is done.




*File: io.bl*


## std.write_string

```c
write_string :: fn (handle: std.File, str: string_view) (_0: s64, _1: Error
) #inline
```

Write content of `str` into the file, return count of written bytes and `OK` if there is no 
error, otherwise return 0 and error.




*File: io.bl*


## std.write_fmt

```c
write_fmt :: fn { fn (buf_size: s32, handle: std.File, fmt: string_view, args: ...) (_0: s64, _1: Error
); fn (handle: std.File, fmt: string_view, args: ...) (_0: s64, _1: Error
); } #inline
```



*File: io.bl*


## std.write_slice

```c
write_slice :: fn (handle: std.File, v: string_view, count : s64: -1) (_0: s64, _1: Error
) #inline
```

Write content of `v` slice into the file, return count of written bytes and `OK` if there is no 
error, otherwise return 0 and error. Optional argument `count` specify count of bytes from `v`
to be written, negative value means whole content. When passed count is greater than `v.len`, 
count value is set to `v.len`.
Does nothing in case the `v.len == 0` (slice is empty) and return 0 and ok.




*File: io.bl*

