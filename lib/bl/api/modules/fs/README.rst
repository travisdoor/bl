File System
===========
.. toctree::
   :hidden:

File system module for manipulation with files and directories.

.. warning:: This module is experimental and not fully supported across all platforms.



.. _FSFileOpenMode:

FSFileOpenMode
--------------

Declaration
^^^^^^^^^^^

::

    FSFileOpenMode :: enum {
        Read;
        Write;
    };

Description
^^^^^^^^^^^
Specify operation with opened file.

Variants
^^^^^^^^
* `Read` Open file for reading.
* `Write` Open file for writing.

.. _FSError:

FSError
-------

Declaration
^^^^^^^^^^^

::

    FSError :: enum {
	OK;
	InvalidInput;
	NotFound;
	AlreadyExist;
	AccessDenied;
	Unknown;
    }

Description
^^^^^^^^^^^
Specify all possible errors in module.

Variants
^^^^^^^^
* `OK` No error.
* `InvalidInput` Function input is invalid.
* `NotFound` File or directory not found.
* `AlreadyExist` File or directory already exist.
* `AccessDenied` File or directory cannot be manipulated.
* `Unknown` All other unspecified errors.

.. _fs_open:

fs_open
-------

Declaration
^^^^^^^^^^^
::

    fs_file_open :: fn (handle: *FSFile, filepath: string, mode: ...FSFileOpenMode) FSError

Description
^^^^^^^^^^^
Open an existing file specified by `filepath`. File `handle` is set to valid file handle in case function return
`FSError.OK` state, otherwise handle's value does not change. File must be closed by :ref:`fs_close` call.
 
Arguments
^^^^^^^^^
* `handle` File handle to be set.
* `filepath` File path.
* `mode` Open mode. :ref:`FSFileOpenMode` When no mode is specified, `Read` and `Write` is used.

Result
^^^^^^
Resulting error code :ref:`FSError`.

Example
^^^^^^^

.. code-block:: c

    main :: fn () s32 {
        file: FSFile;
	if fs_file_open(&file, #file) != FSError.OK {
            print_err("Cannot open file!");
	    return 1;
	}
	defer fs_file_close(&file);
        return 0;
    }

.. _fs_close:

fs_file_close
-------------

Declaration
^^^^^^^^^^^

::

    fs_file_close :: fn (handle: *File)

Description
^^^^^^^^^^^
Close opened file.
 
Arguments
^^^^^^^^^
* `handle` File handle.

.. _fs_exist:

fs_exist
--------

Declaration
^^^^^^^^^^^
::

    fs_exist :: fn (filepath: string) bool 

Description
^^^^^^^^^^^
Check whether file exists.
 
Arguments
^^^^^^^^^
* `filepath` File path.

Result
^^^^^^
True when file exists.
