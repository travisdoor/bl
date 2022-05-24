# Array

`#import "std/array"`

Compiler builtin dynamic array is automatically growing memory storage allocated on heap. The
array allocated memory block groves over time when we push new values to the array. It's
guaranteed that all pushed elements are stored in single continuous block of memory. When there
is no space reminding in currently allocated block, new one is allocated and all already pushed
data are copied from old block to the new one.

### Example

```
main :: fn () s32 {
    arr: [..]s32; // dynamic array
    loop i := 0; i < 10; i += 1 {
        array_push(&arr, i);
    }
    print("arr = %\n", arr);
    return 0;
}
```

## array_init

```c
array_init :: fn (_arr: *[..]?T, n : s64: 0, allocator : *Allocator: )  #inline
```

Sets dynamic array to default state. If array contains already allocated memory `array_terminate`
must be called first. 

Initial capacity can be specified by `n` (count of elements). If `n` is greater than 0, needed
memory is preallocated using specified `allocator`; otherwise no allocation is done.

It's usually enough to rely on default implicit initialization in case we don't need specify
custom allocator or preallocate storage to specified `n` of entries.




*File: array.bl*


## array_terminate

```c
array_terminate :: fn (_arr: *[..]?T)  #inline
```

Release memory allocated by dynamic array. This function is supposed to be called when dynamic
array is no longer needed. Passed dynamic array is set to default state also.




*File: array.bl*


## array_push

```c
array_push :: fn { impl_push_empty; impl_push; }
```

Append new value at the dynamic array end. New heap allocation is done here only in cases when
there is not enough memory allocated to store all values.
Returns pointer to the new appended element.

**note:** When there is no element to push provided, function will just allocate memory for
a new empty element without any initialization.




*File: array.bl*


## array_pop

```c
array_pop :: fn (_arr: *[..]?T, out : *T: ) bool #inline
```

Duplicate the last array element into `out` argument (if not null) and reduce the array length by
one. Returs `true` in case the array is not empty.




*File: array.bl*


## array_reserve

```c
array_reserve :: fn (_arr: *[..]?T, n: s64)  #inline
```

Reserve heap memory for `n` elements in array. Does nothing in case the already allocaed block is
large enough to hold `n` elements.




*File: array.bl*


## array_erase

```c
array_erase :: fn (_arr: *[..]?T, i: s64) 
```

Erase element on index `i`. Call to this function can cause reorderingof an dynamic array.
Allocated memory is kept even if we erase entire dynamic array. Function invoke panic in case
of index overflow.




*File: array.bl*


## array_clear

```c
array_clear :: fn (_arr: *[..]?T)  #inline
```

Erase all elements in dynamic array but keep allocated memory.



*File: array.bl*

