.. _string_split_by:

string_split_by
===============
.. code-block:: bl

    string_split_by :: fn (str: string, delimiter: u8) 

Split the `str` input string by delimiter and return new slice containing
all found sub-strings. 

.. warning:: String slice should be terminated by :ref:`slice_terminate` call.
.. warning:: Slice elements are not guaranteed to be zero terminated and they are not
             supposed to be freed. 



*Declared in: string.bl*
