.. _string_insert:

string_insert
=============
.. code-block:: bl

    string_insert :: 

Overloaded function inserting one character or other string at desired position.

::

    fn (str: *string, index: s32, v: u8) bool #inline

Insert one character into `str` at `index` and return `true` when character was inserted.

.. note:: The input `str` can be reallocated when `index` is equal to input string `len` and more
          space is needed.

:: 

    fn (str: *string, index: s32, v: string) bool

Insert other string into `str` at `index` and return `true` when string was inserted.

.. note:: The input `str` can be reallocated when `index` is equal to input string `len` and
          more space is needed.

.. note:: Function does nothing (return `false`) when `v` string is empty.



*Declared in: string.bl*
