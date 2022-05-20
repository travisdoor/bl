# Temporary Allocator

Temporary allocator is memory allocator supposed to be used in case just temporary memory 
allocation is needed; i.e. to build up file path from individual path components. The allocated 
memory is valid until [tmp_reset](#tmp_reset) or [tmp_release](#tmp_release)
is called.

The allocator makes all allocations in bigger preallocated block; i.e. when allocation is 
requested for the first time, new block is allocated with predefined size ``DEFAULT_BLOCK_SIZE``, 
all following allocations use the preallocated block if there is enough space. If the block is 
full and new requested allocation does not fit, another block is allocated (with size slightly 
bigger then previous one). When [tmp_reset](#tmp_reset) is called, allocator will 
release all smaller allocated blocks and keeps only the biggest one for later use.

### Example

```
#import "std/tmp_allocator"

main :: fn () s32 {
    // Release allocated memory at the end of the scope.
    defer std.tmp_release();
    
    loop i := 0; i < 1000; i += 1 {
         // Reset the allocator here (all previous alocation are invalid since now).
        defer std.tmp_reset();
        
        // Allocate memory using temporary allocator.
        int_ptr := cast(*s32) allocate_memory(&std.tmp_allocator, sizeof(s32));
        
        // Do something here with allocated memory.
        @int_ptr = i;

        // There is no need to free allocated memory here.
    }
    return 0;
}
```

**note**: Temporary allocator has it's internal context allocated per-thread, so the explicit 
release of the allocated memory at the thread worker exit is mandatory.


## std.default_temporary_allocator

```c
default_temporary_allocator :: pool_default
```

Temporary allocator instance.



*File: tmp_allocator.bl*


## std.temporary_reset

```c
temporary_reset :: fn ()  #inline
```

Reduce allocated memory, but keeps biggest allocated chunk for later use.

**warning**:  All resources previously allocated by this allocator became invalid after reset.

**hint**: Call this method i.e. in every event loop (update) iteration.




*File: tmp_allocator.bl*


## std.temporary_release

```c
temporary_release :: fn ()  #inline
```

Release all memory allocated by temporary allocator, this method is supposed to be called
at exit of a program.

**warning**:  All resources previously allocated by this allocator became invalid after release.

**hint**: Call this method before main returns.




*File: tmp_allocator.bl*

