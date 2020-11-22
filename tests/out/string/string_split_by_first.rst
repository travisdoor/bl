.. _string_split_by_first:

string_split_by_first
=====================
.. code-block:: bl

    string_split_by_first :: fn (str: string, delimiter: u8, lhs: *string, rhs: *string, di: *s32) bool

Split input string `str` into two tokens based on the first occurrence of `delimiter`.
Delimiter is not included in resulting tokens. Result tokens only points into original
memory of the `str`, they are not supposed to be freed.

When delimiter is not present in the input string function return `false`, `lhs` and
`rhs` buffers are not modified, otherwise function return `true` and sets `lhs` and `rhs`
to found values.

Token destination pointers `lhs` and `rhs` are optional. 

.. warning:: `lhs` and `rhs` sub strings are not guaranteed to be zero terminated and they are not
             supposed to be freed. 
 
Example
-------
.. literalinclude:: /examples/docs/012.bl
   :language: bl



*Declared in: string.bl*
