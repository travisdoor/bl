# Table

`#import "std/table"`

Tables are generic data containers aka hash tables or hash maps that store data elements
as pairs of Keys and Values. Table provides fast lookup on large amount of data.

Internally, the table is implemented as double-linked list with metadata stored as buckets.
A bucket represents begin of sub-list of data associated with hash value calculated from the
key value. Table entries are wrapped into Nodes containing key-value data and some metadata
required by the table.

Every table insertion cause memory allocation, this can be a performance bottle-neck in some
cases, however, already inserted data never change its address (does not move in memory).
Consider using a simple dynamic array in cases data set is not too-big (simple iteration over
an array can be faster and more memory efficient than using a table.

No memory is allocated until the first call of insert method. After the first insertion, table
operates on pre-allocated chunk of memory used to store buckets (`DEFAULT_BUCKET_COUNT`). The
bucket storage is reallocated to double size in case the maximum load factor
(table.len / table.buckets.len) is exceeded (maximum load factor is defined as `MAX_LF`) and
whole table is reorganized to achieve optimal performance (rehashing). This operation can be
very expensive on large data sets.

Current table implementation is not heavily optimized, this should be improved later, consider
using a custom implementation in performance-critical cases.

**warning:** Table elements are not sorted in any particular way so the iteration over the
table does not correspond to the order in which elements were inserted.

**warning:** The key types are limited to numbers and strings (this may change in the future).

### Example
```c
#import "std/table"

main :: fn () s32 {
    table: std.Table(string, s32);
    defer std.tbl_terminate(&table);

    // Insert some data into the table.
    std.tbl_insert(&table, "Martin", 32);
    std.tbl_insert(&table, "Lisa", 29);
    std.tbl_insert(&table, "John", 40);

    // Lookup data by key.
    found :: std.tbl_lookup(&table, "Martin");
    if found != std.tbl_end(&table) {
        print("[%] %\n", found.key, found.value);
    }

    // Iterate over the table
    loop it := std.tbl_begin(&table); it != std.tbl_end(&table); std.tbl_iter_next(&it) {
        print("[%] %\n", it.key, it.value);
    }

    return 0;
}
```

```c
Table :: struct {
    len: s64;
}
```

## std.Table

```c
Table :: fn (TKey: type, TValue: type) type
```

Create a new **Table** type. The `TKey` must be a number or string type.





*File: table.bl*


## std.TableIter

```c
TableIter :: fn (TTable: type) type
```

Create iterator type from **Table** type.



*File: table.bl*


## std.tbl_init

```c
tbl_init :: fn (tbl: *?T, elem_count :: , allocator : *Allocator: )  #inline
```

Initialize the `tbl` table. It's not necessary to call this method in case the table is
already zero-initialized. The `elem_count` can be specified as hint telling the table how many
elements we're going to store. Memory to hold an internal meta-data is allocated here. The custom
`allocator` can be set.




*File: table.bl*


## std.tbl_terminate

```c
tbl_terminate :: fn (tbl: *?T) 
```

Release all memory resources used by the table and set the `tbl` instance to the default state.



*File: table.bl*


## std.tbl_insert

```c
tbl_insert :: fn { impl_insert_empty; impl_insert; }
```

Overloaded table insert function adding new element into the table associated with the `key`.
The `key` value must be unique (not already existing in the table), this is checked in debug
mode and panic is invoked in case of collision.

**Overloads:**
```c
fn (tbl: *?T, key: tbl_typeof_key(T), value: tbl_typeof_value(T)) TableIter(T) #inline
fn (tbl: *?T, key: tbl_typeof_key(T)) TableIter(T) #inline
```

Function returns iterator pointing to the new element. Every new element is allocated using
table allocator (can be specified using `tbl_init` function). Rehashing of the table can be
eventually done in case the table's load factor goes over `MAX_LF`.

Iterator's value in case the `value` is not specified is NOT zero initialized.




*File: table.bl*


## std.tbl_erase

```c
tbl_erase :: fn (tbl: *?T, key: )  #inline
```

Erase table entry associated with `key`. In case there is no such `key` found function does nothing.

Returns iterator pointing to the next element or to the `tbl_end`.




*File: table.bl*


## std.tbl_erase_it

```c
tbl_erase_it :: fn (tbl: *?T, it: )  #inline
```

Erase table entry associated with `it` iterator. In case the `it` points to the table end, the
function does nothing. 

Returns iterator pointing to the next element or to the `tbl_end`.




*File: table.bl*


## std.tbl_lookup

```c
tbl_lookup :: fn (tbl: *?T, key: )  #inline
```

Lookup element associated with the `key` value in the table and return an iterator pointing to
the key-value pair. In case there is no such element in the table, the function returns an iterator
pointing to the `tbl_end()`.




*File: table.bl*


## std.tbl_contains

```c
tbl_contains :: fn (tbl: *?T, key: ) bool #inline
```

Checks whether the table `tbl` contains the `key`.



*File: table.bl*


## std.tbl_clear

```c
tbl_clear :: fn (tbl: *?T)  #inline
```

Clears the table and deletes all inserted entries.



*File: table.bl*


## std.tbl_begin

```c
tbl_begin :: fn (tbl: *?T)  #inline
```

Return an iterator pointing to the table beginning.



*File: table.bl*


## std.tbl_end

```c
tbl_end :: fn (tbl: *?T)  #inline
```

Return an iterator pointing to the table ending (one element past the last in the table).



*File: table.bl*


## std.tbl_iter_next

```c
tbl_iter_next :: fn (it: *?T)  #inline
```

Move the iterator `it` to the next element.



*File: table.bl*


## std.tbl_iter_prev

```c
tbl_iter_prev :: fn (it: *?T)  #inline
```

Move the iterator `it` to the previous element.



*File: table.bl*


## std.tbl_typeof_key

```c
tbl_typeof_key :: fn (T: type) type
```

Resolve type of key from table type in compiletime.



*File: table.bl*


## std.tbl_typeof_value

```c
tbl_typeof_value :: fn (T: type) type
```

Resolve type of value from table type in compiletime.



*File: table.bl*

