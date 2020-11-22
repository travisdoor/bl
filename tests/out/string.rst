======
String
======

::

    #load "std/string.bl"

:: 

    string :: struct {
        len: s64;
        ptr: *u8;
    }

Builtin string storage. `string` type could represent dynamic and compile time static strings. 

::

    // Compile-time string literal
    my_string :: "This is my string!"; 

:: 

    // Dynamic string allocated on heap.
    // New dynamic string can be created from string literal. 
    my_string :: string_new("This is my string!");
    defer string_delete(my_string);

BL strings are zero terminated except of sub-string view function (terminator cannot be added 
because we cannot modify original string).

.. note:: String manipulation functions are related only to dynamic strings and cannot be used with
          string literals since those are constants allocated on stack.

.. toctree::
    :glob:
    :titlesonly:

    string/*