.. _string_new:

string_new
==========
.. code-block:: bl

    string_new :: 

Overloaded function creating new dynamic string instance. Created string is 
guaranteed to be zero terminated.

.. warning:: Every new string must be deleted by :ref:`string_delete` call.

::

    fn () string

Create new empty string object.

----

::

    fn (size: usize) string

Create new string with preallocated space. This type of string
initialization can reduce count of allocations made later by extending
string content. String with length up to `size` require only one memory
allocation.

.. hint:: This initialization should be preferred if string length can be predicted.

Example
-------

.. literalinclude:: /examples/docs/009.bl
   :language: bl

----

:: 

    fn (v: string) string

Create copy of `v` string. Allocates memory to hold exactly size of `v` string data.

----

::

    fn (cstr: *u8) string

Create copy of C zero terminated string.



*Declared in: string.bl*
