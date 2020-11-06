===========
File System
===========

::

   #import "fs"

File system module for manipulation with files and directories.

.. warning:: This module is experimental and not fully supported across all platforms.

.. contents::
   :local:
   :depth: 1

----

.. _FSFileOpenMode:

FSFileOpenMode
==============

Declaration
-----------

::

    FSFileOpenMode :: enum {
        Read;
        Write;
    };

Description
-----------
Specify operation with opened file.

Variants
--------
* `Read` Open file for reading.
* `Write` Open file for writing.

----

.. _FSError:

FSError
=======

Declaration
-----------

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
-----------
Specify all possible errors in module.

Variants
--------
* `OK` No error.
* `InvalidInput` Function input is invalid.
* `NotFound` File or directory not found.
* `AlreadyExist` File or directory already exist.
* `AccessDenied` File or directory cannot be manipulated.
* `Unknown` All other unspecified errors.

----

.. _fs_file_open:

fs_file_open
============

Declaration
-----------
::

    fs_file_open :: fn (handle: *FSFile, filepath: string, mode: ...FSFileOpenMode) FSError

Description
-----------
Open an existing file specified by `filepath`. File `handle` is set to valid file handle in case function return
`FSError.OK` state, otherwise handle's value does not change. File must be closed by :ref:`fs_close` call.
 
Arguments
---------
* `handle` File handle to be set.
* `filepath` File path.
* `mode` Open mode. :ref:`FSFileOpenMode` When no mode is specified, `Read` and `Write` is used.

Result
------
Resulting error code :ref:`FSError`.

Example
-------

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

----

.. _fs_file_create:

fs_file_create
==============

Declaration
-----------
::

    fs_file_create :: fn (handle: *FSFile, filepath: string, mode: ...FSFileOpenMode) FSError

Description
-----------
Create new file specified by `filepath`. File `handle` is set to valid file handle in case function return
`FSError.OK` state, otherwise handle's value does not change. File must be closed by :ref:`fs_close` call.
 
Arguments
---------
* `handle` File handle to be set.
* `filepath` File path.
* `mode` Open mode. :ref:`FSFileOpenMode` When no mode is specified, `Read` and `Write` is used.

Result
------
Resulting error code :ref:`FSError`.

----

.. _fs_file_delete:

fs_file_delete
==============

Declaration
-----------
::

    fs_file_delete :: fn (filepath: string) bool #inline

Description
-----------
Delete file specified by `filepath`.
 
Arguments
---------
* `filepath` File path.

Result
------
True when file was deleted, otherwise return false. When `filepath` is invalid or empty string function also
return `false` and doesn't produce any file system operation.

----

.. _fs_close:

fs_file_close
=============

Declaration
-----------

::

    fs_file_close :: fn (handle: *File)

Description
-----------
Close opened file.
 
Arguments
---------
* `handle` File handle.

----

.. _fs_exist:

fs_exist
========

Declaration
-----------
::

    fs_exist :: fn (filepath: string) bool 

Description
-----------
Check whether file or directory exists.
 
Arguments
---------
* `filepath` File path.

Result
------
True when file of directory exists.

----

.. _fs_home:

fs_home
=======

Declaration
-----------
::

    fs_home :: fn () string #inline

Description
-----------
Get path to `home` directory. Use :ref:`string_delete` to delete result string.
 
Result
------
Path to `home` directory or empty string.

----

.. _fs_tmp:

fs_tmp
======

Declaration
-----------
::

    fs_home :: fn () string #inline

Description
-----------
Get path to `temp` directory. Use :ref:`string_delete` to delete result string.
 
Result
------
Path to `temp` directory or empty string.

----

.. _fs_remove_extension:

fs_remove_extension
===================

Declaration
-----------
::

    fs_remove_extension :: fn (filename: string) string #inline 

Description
-----------
Remove file extension (first after dot separator) from file name. In case dot separator is first character
in the string we expect it's hidden file.
 
Arguments
---------
* `filename` File name.

Result
------
File name without extension (not including dot separator) or empty string.

----

.. _fs_get_extension:

fs_get_extension
================

Declaration
-----------
::

    fs_get_extension :: fn (filename: string) string #inline

Description
-----------
Get file extension from file name. This function just split input `filename` by first occourence of
dot character if it's not first one.
 
Arguments
---------
* `filename` File name.

Result
------
File extension not including dot separator. In case no extension was found, function return empty string. 
Returned string is not copy and should not be deleted. 
