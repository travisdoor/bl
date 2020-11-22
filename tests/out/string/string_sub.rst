.. _string_sub:

string_sub
==========
.. code-block:: bl

    string_sub :: fn (s: string, start: s64, len: s64) string #inline

Creates substring from passed string starting at `start` index of input string and ending at `start` + `len`
index.

Starting index `start` must be greated than 0 and less than `str.len`. `len` specifies optional length of substring.
When not specified, length from `start` to the end of the `str` is used.

.. warning:: Result sub-string is not guaranteed to be zero terminated and it's not supposed to
             be freed.



*Declared in: string.bl*
