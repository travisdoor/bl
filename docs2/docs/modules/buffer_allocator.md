# Buffer Allocator

`#import "std/buffer_allocator"`

The buffer allocator can be used to allocate memory in preallocated blocks on the stack or heap 
in case we know the size need. It is perfectly fit for temporary allocation in case we need better
control over the memory resource.

The preallocated memory block can have an arbitrary size greater than zero.

The block is not supposed to grow and the allocator does not own it. Used buffer is basically 
appended on every allocation and free takes no effect in this case. In case there is no more space 
left in the block, the default context allocator is used as a fallback. The buffer can be used 
multiple times but it should be managed only by one buffer allocator at a time.

Use the standard `allocate_memory` and `free_memory` to use the allocator.

### Example

```c
#import "std/buffer_allocator"

buffer: [64]u8;

main :: fn () s32 {
    allocator: std.BufferAllocator;
    std.buffer_allocator_init(&allocator, buffer);

    arr: [..]u8;
    // Initialize the array with our custom allocator.
    array_init(&arr, buffer.len, &allocator);

    loop i := 0; i < 10; i += 1 {
        // We're appending the array using our allocator.
        array_push(&arr, auto i);
    }
   
    // Print out the array content;
    print("arr = %\n", arr);

    // All allocations are done on stack inside the 'buffer'. To test it, we can
    // print out the buffer content. Keep in mind that the array preallocates
    // slightly more memory than needed on the first 'array_push` call (for i.e.
    // 32 elements).
    print("buf = %\n", buffer);

    // Also the address of the first element in the array should point to the first
    // element of the buffer.
    print("%\n", buffer.ptr == arr.ptr);

    return 0;
}
```


## std.BufferAllocator

```c
BufferAllocator :: struct {
    mem: []u8;
    used_bytes: usize;
}
```


### Members
* `mem` - Memory buffer slice.
* `used_bytes` - Used byte count in the `mem` slice. (This count does not contain allocations done in case
the `mem` is full and allocator produce fallback allocation using the context allocator.)



*File: buffer_allocator.bl*


## std.buffer_allocator_make

```c
buffer_allocator_make :: fn (buffer: []u8) BufferAllocator #inline
```

Use this function to initialize the `allocator` with an external preallocated `buffer`. The 
allocator **does not own** the buffer resource so there is no need to terminate the `allocator` 
instance when it's no longer needed.




*File: buffer_allocator.bl*


## std.buffer_allocator_reset

```c
buffer_allocator_reset :: fn (buf: *BufferAllocator) 
```



*File: buffer_allocator.bl*

