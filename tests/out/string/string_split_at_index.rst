.. _string_split_at_index:

string_split_at_index
=====================
.. code-block:: bl

    string_split_at_index :: fn (str: string, index: s32, lhs: *string, rhs: *string) bool

Split input string `str` at index position and return true when split was done. Result tokens 
only points into original memory of the `str`, they are not supposed to be freed. When index is 
out of `str` range function return `false`, `lhs` and `rhs` buffers are not modified.

Token destination pointers `lhs` and `rhs` are optional. 

.. warning:: `lhs` and `rhs` sub strings are not guaranteed to be zero terminated and they are not
             supposed to be freed. 
 
Example
-------
.. literalinclude:: /examples/docs/011.bl
   :language: bl



*Declared in: string.bl*
