# Thread

`#import "std/thread"`

This module contains tools for managing multithreading. It's basically wrapper for Windows thread 
system and pthread on posix systems.

**warning**: This module is experimental and not fully supported across all platforms.

## std.ThreadEntryFn

```c
ThreadEntryFn :: *fn (ctx: *u8) s32
```

Thread entry function type.



*File: thread.bl*


## std.Thread

```c
Thread :: _thread_impl.Handle
```

Thread handle.



*File: thread.bl*


## std.thread_create

```c
thread_create :: fn (entry: ThreadEntryFn, ctx : *u8: ) (_0: Thread, _1: Error
) #inline
```

Create and start new thread with specified `entry` function. Pointer to custom `ctx` can be 
optionally passed to the entry function here.




*File: thread.bl*


## std.thread_join

```c
thread_join :: fn (thread: Thread) (exit_code: s32, err: Error
) #inline
```

Blocks until `thread` exits. Return value from worker function is returned as `exit_code`. When 
execution is successful all thread resources are released.




*File: thread.bl*


## std.thread_join_all

```c
thread_join_all :: fn (threads: []Thread) Error #inline
```

Blocks until all `threads` exits. 



*File: thread.bl*


## std.thread_exit

```c
thread_exit :: fn (exit_code : s32: 0) Error #inline
```

Exit current thread with optional `exit_code` return value.



*File: thread.bl*


## std.thread_current

```c
thread_current :: fn () Thread #inline
```

Gets current thread.



*File: thread.bl*

