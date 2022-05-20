# Debug Allocator

`#import "std/debug_allocator"`

Debug allocator can be used to analyze memory usage of program and eventually analyze possible 
memory leaks. By [init](#init) call the global `context` allocator is replaced by 
debug allocator, every following allocations are recorded and analyzed in runtime since then. 
Call [terminate](#terminate) to swap default context allocator back to previous 
one.

### Example

```
#import "std/debug_allocator"

main :: fn () s32 {
    std.debug_allocator_init();
    defer std.debug_allocator_terminate();

    // leaking allocation
    alloc(64);
    return 0;
}
```


```text
$ ./out.exe
******************* MEMORY REPORT ******************
* Allocated 64 Bytes.
* Count of allocations 1.
****************************************************
Dump memory leaks begin:
    [1] - test.bl:10 (64 bytes)
Dump memory leaks end.
```

**note**: Debug allocator is thread safe. Init and terminate must be called from main thread.

## std.DebugAllocator

```c
DebugAllocator :: struct {
    mutex: std.Mutex;
    allocator: *Allocator;
    total_allocated: s64;
    alloc_table: ;
    serial: u64;
    break_on: u64;
}
```



*File: debug_allocator.bl*


## std.debug_allocator_make

```c
debug_allocator_make :: fn (allocator: *Allocator) DebugAllocator
```

Initialize Debug Allocator. This function internally swap current global context allocator to
debug one. Deinitialization must be done by [debug_allocator_terminate](#debug_allocator_terminate) call.




*File: debug_allocator.bl*


## std.debug_allocator_terminate

```c
debug_allocator_terminate :: fn (dbgalloc: *DebugAllocator) 
```

Terminate Debug Allocator. Prints current memory report when 
[print_report](#print_report) is `true`.




*File: debug_allocator.bl*


## std.debug_allocator_break

```c
debug_allocator_break :: fn (dbgalloc: *DebugAllocator, serial: u64)  #inline
```

Invoke [debug_break](#debug_break) before allocation with defined serial ID.
**note**: See also [print_memory_report](#print_memory_report)




*File: debug_allocator.bl*


## std.debug_allocator_allocated

```c
debug_allocator_allocated :: fn (dbgalloc: *DebugAllocator) usize #inline
```

Return currently allocated memory in bytes.



*File: debug_allocator.bl*


## std.print_memory_report

```c
print_memory_report :: fn (dbgalloc: *DebugAllocator, dump_leaks :: ) 
```

Print memory report. First block contains currently allocated bytes and current count
of allocations. Optional memory leak dump block (enabled by `dump_leaks` argument) contains:

```text
[allocation serial ID] - <file>:<line> (allocation size in bytes) 
``` 

```text
$ ./out.exe
******************* MEMORY REPORT ******************
* Allocated 64 Bytes.
* Count of allocations 1.
****************************************************
Dump memory leaks begin:
    [1] - test.bl:10 (64 bytes)
Dump memory leaks end.
``` 

**note**: Printed report contains all remaining (not freed) allocations in time when function was
called. Memory leaks can contain false-positives when function is called before execution end.

**hint**: Allocation serail ID can be used by [debug_allocator_break](#debug_allocator_break) to interrupt 
execution before memory is allocated and eventually localize allocation in debbuger.




*File: debug_allocator.bl*


## std.dump_memory_leaks

```c
dump_memory_leaks :: fn (dbgalloc: *DebugAllocator) s64
```



*File: debug_allocator.bl*

