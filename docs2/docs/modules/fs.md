# File System

`#import "std/fs"`

File system module for manipulation with files and directories. This module provides an 
abstraction over operating system APIs such as creating files and directories, reading and 
writing, and scanning the filesystem.

## std.File

```c
File :: _fs_impl.File
```

File handle type.



*File: fs.bl*


## std.FILE_INVALID

```c
FILE_INVALID :: _fs_impl.FILE_INVALID
```

Invalid file handle. This can be used for file handle validation.



*File: fs.bl*


## std.FileOpenMode

```c
FileOpenMode :: enum {
    READ;
    WRITE;
    APPEND;
    CREATE;
}
```

Specify operation with opened file.


### Variants
* `READ` - Open file for reading.
* `WRITE` - Open file for writing and truncate its content.
* `APPEND` - Open file for appending (keeps current content).
* `CREATE` - Create file if it does not exist.


*File: fs.bl*


## std.file_open

```c
file_open :: fn (filepath: string_view, mode : FileOpenMode: ) (_0: File, _1: Error
) #inline
```

Open an file specified by `filepath`. Function return file handle and `OK` status when file was 
opened, otherwise return invalid handle and proper error. File must be closed by [close](#close) 
call.

File open `mode` is optional, any combination of `FileOpenMode` can be used. When `Create` 
`mode` is specified, new file is created on `filepath` only if it does not exist, otherwise 
already existing file is used. `Read` `mode` is used as default when neither `Read`, `Write` or 
`Append` is specified.

### Example

```
#import "std/fs"

main :: fn () s32 {
    file, err :: std.file_open(#file);
    defer std.file_close(file);
    if !is_ok(err) {
        print_err("%", err);
        return 1;
    }
    return 0;
}
```

**todo*: Mode should be passed as flags in the future.




*File: fs.bl*


## std.file_close

```c
file_close :: fn (handle: File)  #inline
```

Close previously openned file. Does nothing when `handle` is not valid.



*File: fs.bl*


## std.file_remove

```c
file_remove :: fn (filepath: string_view) Error #inline
```

Try to remove file specified by `filepath` and return `OK` on success, otherwise return error.



*File: fs.bl*


## std.file_copy

```c
file_copy :: fn (src: string_view, dest: string_view, override :: ) Error #inline
```

Copy existing file from `src` to `dest` and override existing file in destination if `override` 
is true. Return `ok` or `error`.

**note**: `src` and `dest` path can be relative path to current working path set by 
[set_cwd](#set_cwd).




*File: fs.bl*


## std.file_size

```c
file_size :: fn (handle: File) (_0: usize, _1: Error
) #inline
```

Return file content size in bytes and `OK` status on success, otherwise return zero and proper 
error.




*File: fs.bl*


## std.file_uid

```c
file_uid :: fn { _fs_impl.get_uid; _fs_impl.get_uid_by_name; }
```

Return file id.



*File: fs.bl*


## std.dir_create

```c
dir_create :: fn (dirpath: string_view) Error #inline
```

Create new directory and return `OK` on success. This function does not create directories 
recursively.




*File: fs.bl*


## std.dir_create_all

```c
dir_create_all :: fn (dirpath: string_view) Error
```



*File: fs.bl*


## std.dir_remove

```c
dir_remove :: fn (dirpath: string_view) Error #inline
```

Remove directory specified by `dirpath` and return `OK` on success, otherwise return an error.



*File: fs.bl*


## std.InfoKind

```c
InfoKind :: enum {
    FILE;
    DIRECTORY;
}
```

Specify kind of file system entry.



*File: fs.bl*


## std.Info

```c
Info :: struct {
    kind: InfoKind;
    name: string;
}
```

Helper container to hold information about file system entry.



*File: fs.bl*


## std.InfoList

```c
InfoList :: []Info
```



*File: fs.bl*


## std.info_list_delete

```c
info_list_delete :: fn (list: InfoList)  #inline
```

Release allocated file system entry info list.



*File: fs.bl*


## std.dir_remove_all

```c
dir_remove_all :: fn (dirpath: string_view, remove_root :: , filter : DirScanFilterFn: ) Error
```

Remove non-empty directory specified by `dirpath` and return `OK` on success, otherwise return 
an error. Root directory is removed if `remove_root` is `true`. Custom file `filter` function 
can be specified as needed.

**note**: This function recursively remove all nested folders and files in specified sub tree 
so it can be expensive.




*File: fs.bl*


## std.DirScanFilterFn

```c
DirScanFilterFn :: *fn (info: *Info) bool
```

Type of `dir_scan` filter function.



*File: fs.bl*


## std.dir_scan

```c
dir_scan :: fn (dirpath: string_view, filter : DirScanFilterFn: ) (_0: InfoList, _1: Error
) #inline
```

Scan `dirpath` directory and return list of information for every file system entry found on 
success. Otherwise return empty list and error. Use [info_list_delete](#info_list_delete) to 
release list when there was no error reported by this function.

Optional `filter` funtion [DirScanFilterFn](#dirscanfilterfn) can be used to filter scan results
directly during scanning, it's called for every found entry and only those for whose filter yields 
true are added into the output list.




*File: fs.bl*


## std.DirCopyOpt

```c
DirCopyOpt :: struct {
    recursive: bool;
    override: bool;
    skip_existing: bool;
}
```

Specify behavior of `dir_copy`.


### Members
* `recursive` - Copy all directories and sub-directories.
* `override` - Override existing entries.
* `skip_existing` - Skip already existing entries.


*File: fs.bl*


## std.dir_copy

```c
dir_copy :: fn (src: string_view, dest: string_view, opt : *DirCopyOpt: , filter : DirScanFilterFn: ) (_0: s64, _1: Error
)
```

Copy from `src` path to `dest` path with specified `opt` options and return  count of processed 
files or error.

### Example:

```
#import "std/fs"

main :: fn () s32 {
    // Copy options
    opt: std.DirCopyOpt;
    // Create destination directory if not exist.
    opt.recursive = true;
    // Override all existing entries.
    opt.override = true;

    // Copy content of 'foo' into 'bar'
    c, err :: std.dir_copy("foo", "bar", &opt, &fn (item: *std.Info) bool {
        // Filter only txt files
        _, ext :: std.path_splitext(item.name);
        if std.str_match(ext, ".txt") { return true; } 
        return false;
    });
    if !is_ok(err) { print("%\n", err); }
    else { print("Copied % files!\n", c); }
    return 0;
}
```

@INCOIMPLETE Is using current dir + better descriptions.




*File: fs.bl*


## std.is_directory

```c
is_directory :: fn (path: string_view) (_0: bool, _1: Error
) #inline
```

Check whether `path` points to valid directory and return true with `OK` state, otherwise return 
`false` and error.




*File: fs.bl*


## std.validate_filename

```c
validate_filename :: fn (name: string_view) bool #inline
```

Checks whether `name` is valid file name on current platform.



*File: fs.bl*


## std.path_exist

```c
path_exist :: fn (filepath: string_view) bool #inline
```

Check whether file or directory exists.



*File: fs.bl*


## std.path_normalize

```c
path_normalize :: fn (filepath: *string) Error #inline
```

Try to normalize `filepath`, basically try to remove all relative path nodes `..` and `.`. Path 
must be valid path (existing) on system. Original `filepath` is extended with current working 
directory. Function return `OK` on success or proper error on fail. Original string is not 
modified when error occurs.




*File: fs.bl*


## std.path_split

```c
path_split :: fn (filepath: string_view) (head: string_view, tail: string_view
)
```

Split input `filepath` into head and tail components. The `tail` is the last `filepath` component
and the `head` is everything before. Only unix `/` path delimiters are supported. The `tail`
component never contains path separators.

### Example

| filepath                | head        | tail         |
|-------------------------|-------------|--------------|
| C:/foo/bar/file.txt     | C:/foo/bar/ | file.txt     |
| file.txt                | -           | file.txt     |
| /usr/local/.hidden.file | /usr/local/ | .hidden.file |
| /                       | /           | -            |
| C:/                     | C:/         | -            |

**note**: Returned components are just "views" of the original `filepath` and should not be explicitly
freed.

**note**: Returned components may not be zero terminated.




*File: fs.bl*


## std.path_splitext

```c
path_splitext :: fn (filepath: string_view) (head: string_view, ext: string_view
)
```

Split input `filepath` into `head` and `ext` components. The `ext` is the file extension  and the `head` is
everything before. 

### Example

| filepath                | head               | ext          |
|-------------------------|--------------------|--------------|
| C:/foo/bar/file.txt     | C:/foo/bar/file    | .txt         |
| file.txt                | file               | .txt         |
| /usr/local/.hidden.file | /usr/local/.hidden | .file        |
| /                       | /                  | -            |
| C:/                     | C:/                | -            |

**note**: Returned components are just "views" of the original `filepath` and should not be explicitly
freed.

**note**: Returned components may not be zero terminated.




*File: fs.bl*


## std.get_cwd

```c
get_cwd :: fn () string #inline
```

Try to obtain current working directory, result must be released by `std.str_delete`. Path does 
not contain last path separator.




*File: fs.bl*


## std.set_cwd

```c
set_cwd :: fn (path: string_view) Error #inline
```

Sets current working directory and return `OK` on success, otherwise return error.



*File: fs.bl*


## std.get_home

```c
get_home :: fn () string #inline
```

Try to obtain system home directory, result must be released by `std.str_delete`. Path does not 
contain last path separator.




*File: fs.bl*


## std.get_tmp

```c
get_tmp :: fn () string #inline
```

Try to obtain system temporary directory, result must be released by `std.str_delete`. Path does 
not contain last path separator.




*File: fs.bl*

