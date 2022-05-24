# Pool

`#import "std/pool"`

Pool allocator is simple growing allocator grouping all allocated memory into memory blocks
of `POOL_DEFAULT_SIZE` bytes by default.

- In case there is no space left in the current block new one is allocated.
- Allocated data remains valid until the `reset` or `release` is called.
- If required allocation size is greater then the current block size, the new block is allocated
  with size `N * current_block_size` where `N` is the minimum multiplier needed to store the required
  data size.
- The pool allocator never frees data when `free_memory` is called, you should call `reset` instead
  when all previously allocated data can be freed.
- All preallocated blocks are kept (even after the `reset` is called) for later use.

## std.POOL_DEFAULT_SIZE

```c
POOL_DEFAULT_SIZE : usize : 65536
```



*File: pool.bl*


## std.PoolAllocator

```c
PoolAllocator :: struct {
    block_size: usize;
    allocator: *Allocator;
    used_blocks: [..]*u8;
    unused_blocks: [..]*u8;
    obsolete_blocks: [..]*u8;
    current_block: *u8;
    top: *u8;
    space: usize;
}
```

Pool allocator type.


### Members
* `block_size` - Size of the current block.
* `allocator` - Base allocator used to allocate block memory.


*File: pool.bl*


## std.pool_default

```c
pool_default :: 
```

Default pool allocator instance using the current allocator in application context and `POOL_DEFAULT_SIZE`.



*File: pool.bl*


## std.pool_make

```c
pool_make :: fn (allocator: *Allocator, block_size :: ) PoolAllocator #inline
```

Make new pool allocator instance. The `allocator` is the allocator used internally to allocate
block memory. The `block_size` is size of each preallocated block in Bytes (note that the block
size can grow over time when bigger continuous memory allocations are required).




*File: pool.bl*


## std.pool_release

```c
pool_release :: fn (pool: *PoolAllocator) 
```

Release all allocated memory used by `pool` (allocated in `pool.allocator`).



*File: pool.bl*


## std.pool_reset

```c
pool_reset :: fn (pool: *PoolAllocator)  #inline
```

Invalidate all allocations but keep preallocated memory block for later use.



*File: pool.bl*

