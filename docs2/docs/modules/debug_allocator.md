# Debug Allocator

`#import "std/debug_allocator"`

Debug allocator can be used to analyze memory usage and eventually investigate possible memory leaks.
Every allocation done via debug allocator is internally recorded with some meta-data included to each
allocation. Debug allocator can later assert in situations like double-free, freeing of non-allocated
memory; or just report memory usage or memory leaks.

Each allocation takes more space due to meta data beeing stored. This allocator also should not be
used in production code since it's way much slower than regular allocators.

### Example

```
#import "std/debug_allocator"

debug_allocator: std.DebugAllocator;

main :: fn () s32 {
    using std;
    // Initialize debug allocator with current context allocator.
    debug_allocator = debug_allocator_make(application_context.allocator);
    // Always cleanup at the end of the scope.
    defer debug_allocator_terminate(&debug_allocator);

    // Some leaking memory.
    allocate_memory(&debug_allocator, 128);

    // Do some other stuff...

    dump_memory_leaks(&debug_allocator);
    return 0;
}
```


```text
.\out.exe
Dump memory leaks begin:
    [1] - C:/Develop/bl/tests/test.bl:13 (128 bytes)
Dump memory leaks end.
```

!!! note
    Debug allocator can be used as global application allocator to catch all possible memory leaks
    and other issues.

!!! note
    Debug allocator is thread safe.

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

Create new debug allocator instace using `allocator` to allocate memory.



*File: debug_allocator.bl*


## std.debug_allocator_terminate

```c
debug_allocator_terminate :: fn (dbgalloc: *DebugAllocator) 
```

Release debug allocator resources.



*File: debug_allocator.bl*


## std.debug_allocator_break

```c
debug_allocator_break :: fn (dbgalloc: *DebugAllocator, serial: u64)  #inline
```

Invoke `debug_break` before allocation with defined serial ID.



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

!!! note
    Printed report contains all remaining (not freed) allocations in time when function was
    called. Memory leaks can contain false-positives when function is called before execution end.

!!! note
    Allocation serail ID can be used by [debug_allocator_break](#debug_allocator_break) to interrupt
    execution before memory is allocated and eventually localize allocation in debbuger.




*File: debug_allocator.bl*


## std.dump_memory_leaks

```c
dump_memory_leaks :: fn (dbgalloc: *DebugAllocator) s64
```

Print only leaking memory if any and returs count of leaking allocations. Please see the
[print_memory_report](debug_allocator.md#stdprint_memory_report) for further details.




*File: debug_allocator.bl*

