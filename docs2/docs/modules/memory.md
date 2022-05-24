# Memory

```
#load "std/memory.bl"
```

Toolset for manipulation with system memory.

Memory allocator
----------------

Memory allocators are used across the entire program to manage allocated memory resources used 
in runtime. Since memory allocation can be an expensive operation in some cases, it's good to 
provide an explicit API, giving information enough to fix  bugs, memory leaks and performance 
issues.

Memory allocator in BL world is just some context :ref:`Allocator` used by an allocator function, 
and allocator function itself [AllocFn](#allocfn). The allocator context can hold some data needed by 
allocator function and the allocator function is later used to make memory allocations and 
deallocations.

Functions like [alloc](#alloc) and [free](#free) internally use allocator set in global executable
context `application_context` variable. Global context allocator is by default set to `default_allocator`
and can be changed as needed. Main goal of internal use of  the allocator set in the global 
context (used across all modules) is to have full control over what's going on and also give
some advanced options to programmer (i.e. create his/her own allocator to have truly full 
control over memory management of the program).

## DEFAULT_ALIGNMENT

```c
DEFAULT_ALIGNMENT : usize : 
```



*File: memory.bl*


## AllocOp

```c
AllocOp :: enum {
    ALLOCATE;
    REALLOCATE;
    FREE;
}
```

Specify allocator opratation.


### Variants
* `ALLOCATE` - Allocation of new memory block is required.
* `REALLOCATE` - Reallocate previously allocated memory.
* `FREE` - Free of previously allocated memory. This operation is optional (i.e. for pools).


*File: memory.bl*


## AllocFn

```c
AllocFn :: *fn (ctx: *Allocator, operation: AllocOp, size: usize, old_size: usize, ptr: *u8, alignment: usize, file: string_view, line: s32) (mem: *u8, err: Error
)
```

Allocator handle function type.



*File: memory.bl*


## Allocator

```c
Allocator :: struct {
    handler: AllocFn;
}
```

Default allocator context base.



*File: memory.bl*


## alloc

```c
alloc :: fn (size: usize, alignment :: , loc :: ) *u8 #inline
```

Allocates `size` of bytes on heap using default allocator. Use `free` to free allocated memory 
when it's no longer needed. Cause panic when allocation is not possible or `size` is  zero. This 
function use allocator set in `application_context.allocator`




*File: memory.bl*


## realloc

```c
realloc :: fn (ptr: *u8, old_size: usize, new_size: usize, alignment :: , loc :: ) *u8 #inline
```



*File: memory.bl*


## free

```c
free :: fn (ptr: *u8, loc :: )  #inline
```

Free memory using current context allocator.

**note**: The `ptr` can be `null`.




*File: memory.bl*


## allocate_memory

```c
allocate_memory :: fn (allocator: *Allocator, size: usize, alignment :: , loc :: ) (mem: *u8, err: Error
) #inline
```

Allocate memory directly using specified `allocator`.



*File: memory.bl*


## reallocate_memory

```c
reallocate_memory :: fn (allocator: *Allocator, ptr: *u8, old_size: usize, size: usize, alignment :: , loc :: ) (mem: *u8, err: Error
) #inline
```



*File: memory.bl*


## free_memory

```c
free_memory :: fn (allocator: *Allocator, ptr: *u8, loc :: )  #inline
```

Free memory previously allocated by specific `allocator`. The `ptr` can be null.



*File: memory.bl*


## memcpy

```c
memcpy :: fn (dest: *u8, src: *u8, size: usize) 
```

Copy memory of defined `size` from `src` to `dest`. Destination and source size must be at least
`size` bytes.




*File: memory.bl*


## memset

```c
memset :: fn (_dest: *u8, v: u8, size: usize) *u8
```

Set memory to desired value and return `dest` pointer. Destination size must be at least `size` bytes.



*File: memory.bl*


## zeromem

```c
zeromem :: fn (dest: *u8, size: usize) *u8
```

Zero out `dest` memory of `size` and return the `dest` pointer.



*File: memory.bl*


## init_default

```c
init_default :: fn (dest: *?T) *T #inline
```



*File: memory.bl*


## swap

```c
swap :: fn (first: *?T, second: *T)  #inline
```

Swaps content of memory at address `first` and `second`.



*File: memory.bl*


## is_aligned

```c
is_aligned :: fn (ptr: *?T, alignment: usize) bool #inline
```

Checks whether passed pointer `ptr` is properly aligned by `alignment`.



*File: memory.bl*


## align_ptr_up

```c
align_ptr_up :: fn (p: *u8, alignment: usize) (p: *u8, adjustment: usize
)
```

Align pointer `p` to `alignment` and return adjusted pointer and number of bytes needed for 
adjustment.

**warning**: Cause panic when alignment is not power of two.




*File: memory.bl*


## slice_init

```c
slice_init :: fn (slice: *[]?T, n: s64, zero_initialized :: , allocator : *Allocator: , loc :: ) 
```

Allocate heap memory for `n` elements in `_v` slice. Allocated block is set to zero default value. 
Allocated memory must be released by :ref:`slice_terminate` call.

Zero initialization of allocated memory block can be expensive in case of large allocation.

### Example

```
main :: fn () s32 {
    // Allocate slice of 10 numbers
    sl: []s32;
    slice_init(&sl, 10);

    loop i := 0; i < sl.len; i += 1 {
        sl[i] = i;
    }

    // release memory allocated by init
    slice_terminate(&sl);
    return 0;
}
```




*File: memory.bl*


## slice_terminate

```c
slice_terminate :: fn (slice: *[]?T, allocator : *Allocator: , loc :: ) 
```

Release slice memory allocated by [slice_init](#slice_init) call. 



*File: memory.bl*


## slice_range

```c
slice_range :: fn (slice: []?T, start: s64, end : s64: -1) []T #inline
```

Create slice subset defined as range `<start-index, end-index)`. Notice that `end` index
is excluded from range, so ``slice_range(other, 0, other.len)`` is valid and returns
new slice pointing to the same data as `other` slice, and with same size.

Indexing rules:

```
start >= 0
start < slice.len
end >= 0
end <= slice.len
```

**warning**: Function cause panic in case combination of `start` and `end` is out of `slice` range.




*File: memory.bl*


## temporary_reset

```c
temporary_reset :: fn ()  #inline
```

Reduce allocated memory, but keeps biggest allocated chunk for later use.

**warning**:  All resources previously allocated by this allocator became invalid after reset.

**hint**: Call this method i.e. in every event loop (update) iteration.




*File: memory.bl*


## temporary_release

```c
temporary_release :: fn ()  #inline
```

Release all memory allocated by temporary allocator, this method is supposed to be called
at exit of a program or thread.

**warning**:  All resources previously allocated by this allocator became invalid after release.

**hint**: Call this method before main returns.




*File: memory.bl*


## default_allocator

```c
default_allocator :: 
```

Default memory allocator using libc malloc internally.



*File: memory.bl*


## default_temporary_allocator

```c
default_temporary_allocator :: 
```

Default temporary allocator instance using pool internally.



*File: memory.bl*

