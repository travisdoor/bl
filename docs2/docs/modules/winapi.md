
# winapi.bl

## win32.TRUE

```c
TRUE :: 1
```



*File: winapi.bl*


## win32.FALSE

```c
FALSE :: 0
```



*File: winapi.bl*


## win32.MAX_PATH

```c
MAX_PATH :: 260
```



*File: winapi.bl*


## win32.INVALID_HANDLE_VALUE

```c
INVALID_HANDLE_VALUE :: 
```



*File: winapi.bl*


## win32.INFINITE

```c
INFINITE : u32 : 4294967295
```



*File: winapi.bl*


## win32.ERROR_FILE_NOT_FOUND

```c
ERROR_FILE_NOT_FOUND :: 2
```



*File: winapi.bl*


## win32.ERROR_FILE_EXISTS

```c
ERROR_FILE_EXISTS :: 80
```



*File: winapi.bl*


## win32.ERROR_ACCESS_DENIED

```c
ERROR_ACCESS_DENIED :: 5
```



*File: winapi.bl*


## win32.ERROR_INVALID_HANDLE

```c
ERROR_INVALID_HANDLE :: 6
```



*File: winapi.bl*


## win32.ERROR_DIRECTORY

```c
ERROR_DIRECTORY :: 267
```



*File: winapi.bl*


## win32.CP_UTF8

```c
CP_UTF8 : u32 : 65001
```



*File: winapi.bl*


## win32.WCHAR

```c
WCHAR :: *C.ushort
```



*File: winapi.bl*


## win32.LPWSTR

```c
LPWSTR :: *C.ushort
```



*File: winapi.bl*


## win32.LPCWSTR

```c
LPCWSTR :: *C.ushort
```



*File: winapi.bl*


## win32.LPCSTR

```c
LPCSTR :: *C.char
```



*File: winapi.bl*


## win32.LPSTR

```c
LPSTR :: *C.char
```



*File: winapi.bl*


## win32.LPCTSTR

```c
LPCTSTR :: *C.uchar
```



*File: winapi.bl*


## win32.HLOCAL

```c
HLOCAL :: *C.uchar
```



*File: winapi.bl*


## win32.UINT

```c
UINT :: C.uint
```



*File: winapi.bl*


## win32.DWORD

```c
DWORD :: C.uint
```



*File: winapi.bl*


## win32.LPCCH

```c
LPCCH :: C.uchar
```



*File: winapi.bl*


## win32.LPCWCH

```c
LPCWCH :: C.ushort
```



*File: winapi.bl*


## win32.LPBOOL

```c
LPBOOL :: C.int
```



*File: winapi.bl*


## win32.HMODULE

```c
HMODULE :: *C.uchar
```



*File: winapi.bl*


## win32.BOOL

```c
BOOL :: C.int
```



*File: winapi.bl*


## win32.PBOOL

```c
PBOOL :: *BOOL
```



*File: winapi.bl*


## win32.FARPROC

```c
FARPROC :: *C.uchar
```



*File: winapi.bl*


## win32.LPVOID

```c
LPVOID :: *C.uchar
```



*File: winapi.bl*


## win32.LPOVERLAPPED

```c
LPOVERLAPPED :: *OVERLAPPED
```



*File: winapi.bl*


## win32.LPDWORD

```c
LPDWORD :: *DWORD
```



*File: winapi.bl*


## win32.HANDLE

```c
HANDLE :: *C.uchar
```



*File: winapi.bl*


## win32.HCURSOR

```c
HCURSOR :: C.void_ptr
```



*File: winapi.bl*


## win32.HINSTANCE

```c
HINSTANCE :: C.void_ptr
```



*File: winapi.bl*


## win32.ULONG_PTR

```c
ULONG_PTR :: *C.ulong
```



*File: winapi.bl*


## win32.SIZE_T

```c
SIZE_T :: C.ulong
```



*File: winapi.bl*


## win32.PVOID

```c
PVOID :: *C.uchar
```



*File: winapi.bl*


## win32.CHAR

```c
CHAR :: C.uchar
```



*File: winapi.bl*


## win32.LONG_PTR

```c
LONG_PTR :: *C.long
```



*File: winapi.bl*


## win32.UINT_PTR

```c
UINT_PTR :: *C.uint
```



*File: winapi.bl*


## win32.LARGE_INTEGER

```c
LARGE_INTEGER :: C.longlong
```



*File: winapi.bl*


## win32.PLARGE_INTEGER

```c
PLARGE_INTEGER :: *LARGE_INTEGER
```



*File: winapi.bl*


## win32.HGLOBAL

```c
HGLOBAL :: C.void_ptr
```



*File: winapi.bl*


## win32.HWND

```c
HWND :: C.void_ptr
```



*File: winapi.bl*


## win32.LONG

```c
LONG :: C.long
```



*File: winapi.bl*


## win32.PLONG

```c
PLONG :: *LONG
```



*File: winapi.bl*


## win32.WORD

```c
WORD :: C.ushort
```



*File: winapi.bl*


## win32.LPARAM

```c
LPARAM :: LONG_PTR
```



*File: winapi.bl*


## win32.WPARAM

```c
WPARAM :: UINT_PTR
```



*File: winapi.bl*


## win32.GENERIC_READ

```c
GENERIC_READ : u32 : 2147483648
```



*File: winapi.bl*


## win32.GENERIC_WRITE

```c
GENERIC_WRITE : u32 : 1073741824
```



*File: winapi.bl*


## win32.FILE_BEGIN

```c
FILE_BEGIN : u32 : 0
```



*File: winapi.bl*


## win32.FILE_CURRENT

```c
FILE_CURRENT : u32 : 1
```



*File: winapi.bl*


## win32.FILE_END

```c
FILE_END : u32 : 2
```



*File: winapi.bl*


## win32.CREATE_NEW

```c
CREATE_NEW : u32 : 1
```



*File: winapi.bl*


## win32.CREATE_ALWAYS

```c
CREATE_ALWAYS : u32 : 2
```



*File: winapi.bl*


## win32.OPEN_EXISTING

```c
OPEN_EXISTING : u32 : 3
```



*File: winapi.bl*


## win32.OPEN_ALWAYS

```c
OPEN_ALWAYS : u32 : 4
```



*File: winapi.bl*


## win32.TRUNCATE_EXISTING

```c
TRUNCATE_EXISTING : u32 : 5
```



*File: winapi.bl*


## win32.LPOVERLAPPED_COMPLETION_ROUTINE

```c
LPOVERLAPPED_COMPLETION_ROUTINE :: *fn (dwErrorCode: DWORD, dwNumberOfBytesTransfered: DWORD, lpOverlapped: LPOVERLAPPED) 
```



*File: winapi.bl*


## win32.LPOFNHOOKPROC

```c
LPOFNHOOKPROC :: *fn (_1: HWND, _2: UINT, _3: WPARAM, _4: LPARAM) UINT_PTR
```



*File: winapi.bl*


## win32.FILE_ADD_FILE

```c
FILE_ADD_FILE : u32 : 2
```



*File: winapi.bl*


## win32.FILE_ADD_SUBDIRECTORY

```c
FILE_ADD_SUBDIRECTORY : u32 : 4
```



*File: winapi.bl*


## win32.FILE_APPEND_DATA

```c
FILE_APPEND_DATA : u32 : 4
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_ARCHIVE

```c
FILE_ATTRIBUTE_ARCHIVE : u32 : 32
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_COMPRESSED

```c
FILE_ATTRIBUTE_COMPRESSED : u32 : 2048
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_DEVICE

```c
FILE_ATTRIBUTE_DEVICE : u32 : 64
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_DIRECTORY

```c
FILE_ATTRIBUTE_DIRECTORY : u32 : 16
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_ENCRYPTED

```c
FILE_ATTRIBUTE_ENCRYPTED : u32 : 16384
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_HIDDEN

```c
FILE_ATTRIBUTE_HIDDEN : u32 : 2
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_NORMAL

```c
FILE_ATTRIBUTE_NORMAL : u32 : 128
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_NOT_CONTENT_INDEXED

```c
FILE_ATTRIBUTE_NOT_CONTENT_INDEXED : u32 : 8192
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_OFFLINE

```c
FILE_ATTRIBUTE_OFFLINE : u32 : 4096
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_READONLY

```c
FILE_ATTRIBUTE_READONLY : u32 : 1
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_REPARSE_POINT

```c
FILE_ATTRIBUTE_REPARSE_POINT : u32 : 1024
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_SPARSE_FILE

```c
FILE_ATTRIBUTE_SPARSE_FILE : u32 : 512
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_SYSTEM

```c
FILE_ATTRIBUTE_SYSTEM : u32 : 4
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_TEMPORARY

```c
FILE_ATTRIBUTE_TEMPORARY : u32 : 256
```



*File: winapi.bl*


## win32.FILE_ATTRIBUTE_VIRTUAL

```c
FILE_ATTRIBUTE_VIRTUAL : u32 : 65536
```



*File: winapi.bl*


## win32.FILE_CREATE_PIPE_INSTANCE

```c
FILE_CREATE_PIPE_INSTANCE : u32 : 4
```



*File: winapi.bl*


## win32.FILE_FLAG_BACKUP_SEMANTICS

```c
FILE_FLAG_BACKUP_SEMANTICS : u32 : 33554432
```



*File: winapi.bl*


## win32.FILE_FLAG_DELETE_ON_CLOSE

```c
FILE_FLAG_DELETE_ON_CLOSE : u32 : 67108864
```



*File: winapi.bl*


## win32.FILE_FLAG_FIRST_PIPE_INSTANCE

```c
FILE_FLAG_FIRST_PIPE_INSTANCE : u32 : 524288
```



*File: winapi.bl*


## win32.FILE_FLAG_NO_BUFFERING

```c
FILE_FLAG_NO_BUFFERING : u32 : 536870912
```



*File: winapi.bl*


## win32.FILE_FLAG_OPEN_NO_RECALL

```c
FILE_FLAG_OPEN_NO_RECALL : u32 : 1048576
```



*File: winapi.bl*


## win32.FILE_FLAG_OPEN_REPARSE_POINT

```c
FILE_FLAG_OPEN_REPARSE_POINT : u32 : 2097152
```



*File: winapi.bl*


## win32.FILE_FLAG_OVERLAPPED

```c
FILE_FLAG_OVERLAPPED : u32 : 1073741824
```



*File: winapi.bl*


## win32.FILE_FLAG_POSIX_SEMANTICS

```c
FILE_FLAG_POSIX_SEMANTICS : u32 : 16777216
```



*File: winapi.bl*


## win32.FILE_FLAG_RANDOM_ACCESS

```c
FILE_FLAG_RANDOM_ACCESS : u32 : 268435456
```



*File: winapi.bl*


## win32.FILE_FLAG_SEQUENTIAL_SCAN

```c
FILE_FLAG_SEQUENTIAL_SCAN : u32 : 134217728
```



*File: winapi.bl*


## win32.FILE_FLAG_SESSION_AWARE

```c
FILE_FLAG_SESSION_AWARE : u32 : 8388608
```



*File: winapi.bl*


## win32.FILE_FLAG_WRITE_THROUGH

```c
FILE_FLAG_WRITE_THROUGH : u32 : 2147483648
```



*File: winapi.bl*


## win32.FILE_LIST_DIRECTORY

```c
FILE_LIST_DIRECTORY : u32 : 1
```



*File: winapi.bl*


## win32.FILE_READ_DATA

```c
FILE_READ_DATA : u32 : 1
```



*File: winapi.bl*


## win32.FILE_SHARE_DELETE

```c
FILE_SHARE_DELETE : u32 : 4
```



*File: winapi.bl*


## win32.FILE_SHARE_READ

```c
FILE_SHARE_READ : u32 : 1
```



*File: winapi.bl*


## win32.FILE_SHARE_WRITE

```c
FILE_SHARE_WRITE : u32 : 2
```



*File: winapi.bl*


## win32.FILE_WRITE_DATA

```c
FILE_WRITE_DATA : u32 : 2
```



*File: winapi.bl*


## win32.GHND

```c
GHND : u32 : 66
```



*File: winapi.bl*


## win32.GMEM_FIXED

```c
GMEM_FIXED : u32 : 0
```



*File: winapi.bl*


## win32.GMEM_MOVEABLE

```c
GMEM_MOVEABLE : u32 : 2
```



*File: winapi.bl*


## win32.GMEM_ZEROINIT

```c
GMEM_ZEROINIT : u32 : 64
```



*File: winapi.bl*


## win32.GPTR

```c
GPTR : u32 : 64
```



*File: winapi.bl*


## win32.CF_TEXT

```c
CF_TEXT : u32 : 1
```



*File: winapi.bl*


## win32.FILE_NOTIFY_CHANGE_FILE_NAME

```c
FILE_NOTIFY_CHANGE_FILE_NAME : u32 : 1
```



*File: winapi.bl*


## win32.FILE_NOTIFY_CHANGE_DIR_NAME

```c
FILE_NOTIFY_CHANGE_DIR_NAME : u32 : 2
```



*File: winapi.bl*


## win32.FILE_NOTIFY_CHANGE_ATTRIBUTES

```c
FILE_NOTIFY_CHANGE_ATTRIBUTES : u32 : 4
```



*File: winapi.bl*


## win32.FILE_NOTIFY_CHANGE_SIZE

```c
FILE_NOTIFY_CHANGE_SIZE : u32 : 8
```



*File: winapi.bl*


## win32.FILE_NOTIFY_CHANGE_LAST_WRITE

```c
FILE_NOTIFY_CHANGE_LAST_WRITE : u32 : 16
```



*File: winapi.bl*


## win32.FILE_NOTIFY_CHANGE_LAST_ACCESS

```c
FILE_NOTIFY_CHANGE_LAST_ACCESS : u32 : 32
```



*File: winapi.bl*


## win32.FILE_NOTIFY_CHANGE_CREATION

```c
FILE_NOTIFY_CHANGE_CREATION : u32 : 64
```



*File: winapi.bl*


## win32.FILE_NOTIFY_CHANGE_SECURITY

```c
FILE_NOTIFY_CHANGE_SECURITY : u32 : 256
```



*File: winapi.bl*


## win32.OFN_READONLY

```c
OFN_READONLY : u32 : 1
```



*File: winapi.bl*


## win32.OFN_OVERWRITEPROMPT

```c
OFN_OVERWRITEPROMPT : u32 : 2
```



*File: winapi.bl*


## win32.OFN_HIDEREADONLY

```c
OFN_HIDEREADONLY : u32 : 4
```



*File: winapi.bl*


## win32.OFN_NOCHANGEDIR

```c
OFN_NOCHANGEDIR : u32 : 8
```



*File: winapi.bl*


## win32.OFN_SHOWHELP

```c
OFN_SHOWHELP : u32 : 16
```



*File: winapi.bl*


## win32.OFN_ENABLEHOOK

```c
OFN_ENABLEHOOK : u32 : 32
```



*File: winapi.bl*


## win32.OFN_ENABLETEMPLATE

```c
OFN_ENABLETEMPLATE : u32 : 64
```



*File: winapi.bl*


## win32.OFN_ENABLETEMPLATEHANDLE

```c
OFN_ENABLETEMPLATEHANDLE : u32 : 128
```



*File: winapi.bl*


## win32.OFN_NOVALIDATE

```c
OFN_NOVALIDATE : u32 : 256
```



*File: winapi.bl*


## win32.OFN_ALLOWMULTISELECT

```c
OFN_ALLOWMULTISELECT : u32 : 512
```



*File: winapi.bl*


## win32.OFN_EXTENSIONDIFFERENT

```c
OFN_EXTENSIONDIFFERENT : u32 : 1024
```



*File: winapi.bl*


## win32.OFN_PATHMUSTEXIST

```c
OFN_PATHMUSTEXIST : u32 : 2048
```



*File: winapi.bl*


## win32.OFN_FILEMUSTEXIST

```c
OFN_FILEMUSTEXIST : u32 : 4096
```



*File: winapi.bl*


## win32.OFN_CREATEPROMPT

```c
OFN_CREATEPROMPT : u32 : 8192
```



*File: winapi.bl*


## win32.OFN_SHAREAWARE

```c
OFN_SHAREAWARE : u32 : 16384
```



*File: winapi.bl*


## win32.OFN_NOREADONLYRETURN

```c
OFN_NOREADONLYRETURN : u32 : 32768
```



*File: winapi.bl*


## win32.OFN_NOTESTFILECREATE

```c
OFN_NOTESTFILECREATE : u32 : 65536
```



*File: winapi.bl*


## win32.OFN_NONETWORKBUTTON

```c
OFN_NONETWORKBUTTON : u32 : 131072
```



*File: winapi.bl*


## win32.OFN_NOLONGNAMES

```c
OFN_NOLONGNAMES : u32 : 262144
```



*File: winapi.bl*


## win32.OFN_EXPLORER

```c
OFN_EXPLORER : u32 : 524288
```



*File: winapi.bl*


## win32.OFN_NODEREFERENCELINKS

```c
OFN_NODEREFERENCELINKS : u32 : 1048576
```



*File: winapi.bl*


## win32.OFN_LONGNAMES

```c
OFN_LONGNAMES : u32 : 2097152
```



*File: winapi.bl*


## win32.OFN_ENABLEINCLUDENOTIFY

```c
OFN_ENABLEINCLUDENOTIFY : u32 : 4194304
```



*File: winapi.bl*


## win32.OFN_ENABLESIZING

```c
OFN_ENABLESIZING : u32 : 8388608
```



*File: winapi.bl*


## win32.OFN_DONTADDTORECENT

```c
OFN_DONTADDTORECENT : u32 : 33554432
```



*File: winapi.bl*


## win32.OFN_FORCESHOWHIDDEN

```c
OFN_FORCESHOWHIDDEN : u32 : 268435456
```



*File: winapi.bl*


## win32.IDC_ARROW

```c
IDC_ARROW :: 
```



*File: winapi.bl*


## win32.IDC_IBEAM

```c
IDC_IBEAM :: 
```



*File: winapi.bl*


## win32.IDC_WAIT

```c
IDC_WAIT :: 
```



*File: winapi.bl*


## win32.IDC_CROSS

```c
IDC_CROSS :: 
```



*File: winapi.bl*


## win32.IDC_UPARROW

```c
IDC_UPARROW :: 
```



*File: winapi.bl*


## win32.IDC_SIZE

```c
IDC_SIZE :: 
```



*File: winapi.bl*


## win32.IDC_ICON

```c
IDC_ICON :: 
```



*File: winapi.bl*


## win32.IDC_SIZENWSE

```c
IDC_SIZENWSE :: 
```



*File: winapi.bl*


## win32.IDC_SIZENESW

```c
IDC_SIZENESW :: 
```



*File: winapi.bl*


## win32.IDC_SIZEWE

```c
IDC_SIZEWE :: 
```



*File: winapi.bl*


## win32.IDC_SIZENS

```c
IDC_SIZENS :: 
```



*File: winapi.bl*


## win32.IDC_SIZEALL

```c
IDC_SIZEALL :: 
```



*File: winapi.bl*


## win32.IDC_NO

```c
IDC_NO :: 
```



*File: winapi.bl*


## win32.OPENFILENAMEA

```c
OPENFILENAMEA :: struct {
    lStructSize: DWORD;
    hwndOwner: HWND;
    hInstance: HINSTANCE;
    lpstrFilter: LPCSTR;
    lpstrCustomFilter: LPSTR;
    nMaxCustFilter: DWORD;
    nFilterIndex: DWORD;
    lpstrFile: LPSTR;
    nMaxFile: DWORD;
    lpstrFileTitle: LPSTR;
    nMaxFileTitle: DWORD;
    lpstrInitialDir: LPCSTR;
    lpstrTitle: LPCSTR;
    Flags: DWORD;
    nFileOffset: WORD;
    nFileExtension: WORD;
    lpstrDefExt: LPCSTR;
    lCustData: LPARAM;
    lpfnHook: LPOFNHOOKPROC;
    lpTemplateName: LPCSTR;
    pvReserved: *u8;
    dwReserved: DWORD;
    FlagsEx: DWORD;
}
```



*File: winapi.bl*


## win32.LPOPENFILENAMEA

```c
LPOPENFILENAMEA :: *OPENFILENAMEA
```



*File: winapi.bl*


## win32.FILE_NOTIFY_INFORMATION

```c
FILE_NOTIFY_INFORMATION :: struct {
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: ;
}
```



*File: winapi.bl*


## win32.OVERLAPPED

```c
OVERLAPPED :: struct {
    Internal: ULONG_PTR;
    InternalHigh: ULONG_PTR;
    Pointer: PVOID;
    hEvent: HANDLE;
}
```



*File: winapi.bl*


## win32.FILETIME

```c
FILETIME :: struct {
    dwLowDateTime: DWORD;
    dwHighDateTime: DWORD;
}
```



*File: winapi.bl*


## win32.PFILETIME

```c
PFILETIME :: FILETIME
```



*File: winapi.bl*


## win32.LPFILETIME

```c
LPFILETIME :: *FILETIME
```



*File: winapi.bl*


## win32.FIND_DATA

```c
FIND_DATA :: struct {
    dwFileAttributes: DWORD;
    ftCreationTime: FILETIME;
    ftLastAccessTime: FILETIME;
    ftLastWriteTime: FILETIME;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    cFileName: ;
    cAlternateFileName: ;
}
```



*File: winapi.bl*


## win32.BY_HANDLE_FILE_INFORMATION

```c
BY_HANDLE_FILE_INFORMATION :: struct {
    dwFileAttributes: DWORD;
    ftCreationTime: FILETIME;
    ftLastAccessTime: FILETIME;
    ftLastWriteTime: FILETIME;
    dwVolumeSerialNumber: DWORD;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    nNumberOfLinks: DWORD;
    nFileIndexHigh: DWORD;
    nFileIndexLow: DWORD;
}
```



*File: winapi.bl*


## win32.LPBY_HANDLE_FILE_INFORMATION

```c
LPBY_HANDLE_FILE_INFORMATION :: *BY_HANDLE_FILE_INFORMATION
```



*File: winapi.bl*


## win32.FIND_DATAA

```c
FIND_DATAA :: FIND_DATA
```



*File: winapi.bl*


## win32.P_FIND_DATAA

```c
P_FIND_DATAA :: *FIND_DATA
```



*File: winapi.bl*


## win32.LP_FIND_DATAA

```c
LP_FIND_DATAA :: *FIND_DATA
```



*File: winapi.bl*


## win32.INVALID_HANDLE

```c
INVALID_HANDLE :: 
```



*File: winapi.bl*


## win32.INVALID_FILE_ATTRIBUTES

```c
INVALID_FILE_ATTRIBUTES :: 
```



*File: winapi.bl*


## win32.WAIT_TIMEOUT

```c
WAIT_TIMEOUT :: 258
```



*File: winapi.bl*


## win32.WAIT_FAILED

```c
WAIT_FAILED : DWORD : 4294967295
```



*File: winapi.bl*


## win32.WAIT_OBJECT_0

```c
WAIT_OBJECT_0 :: 0
```



*File: winapi.bl*


## win32.LIST_ENTRY

```c
LIST_ENTRY :: struct {
    Flink: *LIST_ENTRY;
    Blink: *LIST_ENTRY;
}
```



*File: winapi.bl*


## win32.RTL_CRITICAL_SECTION_DEBUG

```c
RTL_CRITICAL_SECTION_DEBUG :: struct {
    Type: WORD;
    CreatorBackTraceIndex: WORD;
    CriticalSection: *RTL_CRITICAL_SECTION;
    ProcessLocksList: LIST_ENTRY;
    EntryCount: DWORD;
    ContentionCount: DWORD;
    Flags: DWORD;
    CreatorBackTraceIndexHigh: WORD;
    SpareWORD: WORD;
}
```



*File: winapi.bl*


## win32.PRTL_CRITICAL_SECTION_DEBUG

```c
PRTL_CRITICAL_SECTION_DEBUG :: *RTL_CRITICAL_SECTION_DEBUG
```



*File: winapi.bl*


## win32.RTL_CRITICAL_SECTION

```c
RTL_CRITICAL_SECTION :: struct {
    DebugInfo: PRTL_CRITICAL_SECTION_DEBUG;
    LockCount: LONG;
    RecursionCount: LONG;
    OwningThread: HANDLE;
    LockSemaphore: HANDLE;
    SpinCount: ULONG_PTR;
}
```



*File: winapi.bl*


## win32.LPCRITICAL_SECTION

```c
LPCRITICAL_SECTION :: *RTL_CRITICAL_SECTION
```



*File: winapi.bl*


## win32.POINT

```c
POINT :: struct {
    x: LONG;
    y: LONG;
}
```



*File: winapi.bl*


## win32.LPPOINT

```c
LPPOINT :: *POINT
```



*File: winapi.bl*


## win32.FORMAT_MESSAGE_IGNORE_INSERTS

```c
FORMAT_MESSAGE_IGNORE_INSERTS : u32 : 512
```



*File: winapi.bl*


## win32.FORMAT_MESSAGE_FROM_STRING

```c
FORMAT_MESSAGE_FROM_STRING : u32 : 1024
```



*File: winapi.bl*


## win32.FORMAT_MESSAGE_FROM_HMODULE

```c
FORMAT_MESSAGE_FROM_HMODULE : u32 : 2048
```



*File: winapi.bl*


## win32.FORMAT_MESSAGE_FROM_SYSTEM

```c
FORMAT_MESSAGE_FROM_SYSTEM : u32 : 4096
```



*File: winapi.bl*


## win32.FORMAT_MESSAGE_ARGUMENT_ARRAY

```c
FORMAT_MESSAGE_ARGUMENT_ARRAY : u32 : 8192
```



*File: winapi.bl*


## win32.FORMAT_MESSAGE_MAX_WIDTH_MASK

```c
FORMAT_MESSAGE_MAX_WIDTH_MASK : u32 : 255
```



*File: winapi.bl*


## win32.FindFirstFile

```c
FindFirstFile :: fn (lpFileName: LPCSTR, lpFindFileData: LP_FIND_DATAA) HANDLE #extern
```



*File: winapi.bl*


## win32.FindClose

```c
FindClose :: fn (hFindFile: HANDLE) BOOL #extern
```



*File: winapi.bl*


## win32.FindNextFile

```c
FindNextFile :: fn (hFindFile: HANDLE, lpFindFileData: LP_FIND_DATAA) BOOL #extern
```



*File: winapi.bl*


## win32.GetFileAttributesA

```c
GetFileAttributesA :: fn (lpFileName: LPCSTR) DWORD #extern
```



*File: winapi.bl*


## win32.CreateFile

```c
CreateFile :: fn (lpFileName: LPCSTR, dwDesiredAccess: DWORD, dwShareMode: DWORD, lpSecurityAttributes: LPSECURITY_ATTRIBUTES, dwCreationDisposition: DWORD, dwFlagsAndAttributes: DWORD, hTemplateFile: HANDLE) *u8 #extern
```



*File: winapi.bl*


## win32.CreateDirectoryA

```c
CreateDirectoryA :: fn (lpPathName: LPCSTR, lpSecurityAttributes: LPSECURITY_ATTRIBUTES) BOOL #extern
```



*File: winapi.bl*


## win32.ReadDirectoryChanges

```c
ReadDirectoryChanges :: fn (hDirectory: HANDLE, lpBuffer: LPVOID, nBufferLength: DWORD, bWatchSubtree: BOOL, dwNotifyFilter: DWORD, lpBytesReturned: LPDWORD, lpOverlapped: LPOVERLAPPED, lpCompletionRoutine: LPOVERLAPPED_COMPLETION_ROUTINE) s32 #extern
```



*File: winapi.bl*


## win32.WideCharToMultiByte

```c
WideCharToMultiByte :: fn (CodePage: UINT, dwFlags: *DWORD, lpWideCharStr: *LPCWCH, cchWideChar: s32, lpMultiByteStr: LPSTR, cbMultiByte: s32, lpDefaultChar: *LPCCH, lpUsedDefaultChar: *LPBOOL) s32 #extern
```



*File: winapi.bl*


## win32.CreateEvent

```c
CreateEvent :: fn (lpEventAttributes: LPSECURITY_ATTRIBUTES, bManualReset: BOOL, bInitialState: BOOL, lpName: LPCSTR) HANDLE #extern
```



*File: winapi.bl*


## win32.SetEvent

```c
SetEvent :: fn (hEvent: HANDLE) BOOL #extern
```



*File: winapi.bl*


## win32.FindFirstChangeNotification

```c
FindFirstChangeNotification :: fn (lpPathName: LPCSTR, bWatchSubtree: BOOL, dwNotifyFilter: DWORD) HANDLE #extern
```



*File: winapi.bl*


## win32.FindNextChangeNotification

```c
FindNextChangeNotification :: fn (hChangeHandle: HANDLE) BOOL #extern
```



*File: winapi.bl*


## win32.FindCloseChangeNotification

```c
FindCloseChangeNotification :: fn (hChangeHandle: HANDLE) BOOL #extern
```



*File: winapi.bl*


## win32.WaitForSingleObject

```c
WaitForSingleObject :: fn (hHandle: HANDLE, dwMilliseconds: DWORD) DWORD #extern
```



*File: winapi.bl*


## win32.WaitForMultipleObjects

```c
WaitForMultipleObjects :: fn (nCount: DWORD, lpHandles: *HANDLE, bWaitAll: BOOL, dwMilliseconds: DWORD) DWORD #extern
```



*File: winapi.bl*


## win32.GetCommandLineW

```c
GetCommandLineW :: fn () LPWSTR #extern
```



*File: winapi.bl*


## win32.CommandLineToArgvW

```c
CommandLineToArgvW :: fn (lpCmdLine: LPCWSTR, pNumArgs: *s32) *LPWSTR #extern
```



*File: winapi.bl*


## win32.LocalFree

```c
LocalFree :: fn (hMem: HLOCAL) HLOCAL #extern
```



*File: winapi.bl*


## win32.Sleep

```c
Sleep :: fn (dwMilliseconds: DWORD)  #extern
```



*File: winapi.bl*


## win32.SleepEx

```c
SleepEx :: fn (dwMilliseconds: DWORD, bAlertable: BOOL) DWORD #extern
```



*File: winapi.bl*


## win32.GetFullPathNameA

```c
GetFullPathNameA :: fn (lpFileName: LPCSTR, nBufferLength: DWORD, lpBuffer: LPSTR, lpFilePart: *LPSTR) DWORD #extern
```



*File: winapi.bl*


## win32.PathFileExistsA

```c
PathFileExistsA :: fn (pszPath: LPCSTR) BOOL #extern
```



*File: winapi.bl*


## win32.LoadLibraryA

```c
LoadLibraryA :: fn (lpLibFileName: LPCSTR) HMODULE #extern
```



*File: winapi.bl*


## win32.FreeLibrary

```c
FreeLibrary :: fn (hLibModule: HMODULE) BOOL #extern
```



*File: winapi.bl*


## win32.GetModuleHandleA

```c
GetModuleHandleA :: fn (lpModuleName: LPCSTR) HMODULE #extern
```



*File: winapi.bl*


## win32.GetProcAddress

```c
GetProcAddress :: fn (hModule: HMODULE, lpProcName: LPCSTR) FARPROC #extern
```



*File: winapi.bl*


## win32.DebugBreak

```c
DebugBreak :: fn ()  #extern
```



*File: winapi.bl*


## win32.IsDebuggerPresent

```c
IsDebuggerPresent :: fn () BOOL #extern
```



*File: winapi.bl*


## win32.CheckRemoteDebuggerPresent

```c
CheckRemoteDebuggerPresent :: fn (hProcess: HANDLE, pbDebuggerPresent: PBOOL) BOOL #extern
```



*File: winapi.bl*


## win32.GetModuleFileNameA

```c
GetModuleFileNameA :: fn (hModule: HMODULE, lpFilename: LPSTR, nSize: DWORD) DWORD #extern
```



*File: winapi.bl*


## win32.SECURITY_ATTRIBUTES

```c
SECURITY_ATTRIBUTES :: struct {
    nLength: DWORD;
    lpSecurityDescriptor: LPVOID;
    bInheritHandle: BOOL;
}
```



*File: winapi.bl*


## win32.PSECURITY_ATTRIBUTES

```c
PSECURITY_ATTRIBUTES :: *SECURITY_ATTRIBUTES
```



*File: winapi.bl*


## win32.LPSECURITY_ATTRIBUTES

```c
LPSECURITY_ATTRIBUTES :: *SECURITY_ATTRIBUTES
```



*File: winapi.bl*


## win32.LPTHREAD_START_ROUTINE

```c
LPTHREAD_START_ROUTINE :: *fn (args: LPVOID) DWORD
```



*File: winapi.bl*


## win32.CreateThread

```c
CreateThread :: fn (lpThreadAttributes: LPSECURITY_ATTRIBUTES, dwStackSize: SIZE_T, lpStartAddress: LPTHREAD_START_ROUTINE, lpParameter: LPVOID, dwCreationFlags: DWORD, lpThreadId: LPDWORD) HANDLE #extern
```



*File: winapi.bl*


## win32.GetCurrentThread

```c
GetCurrentThread :: fn () HANDLE #extern
```



*File: winapi.bl*


## win32.TerminateThread

```c
TerminateThread :: fn (handle: HANDLE, exit_code: DWORD) BOOL #extern
```



*File: winapi.bl*


## win32.GetExitCodeThread

```c
GetExitCodeThread :: fn (hThread: HANDLE, lpExitCode: LPDWORD) BOOL #extern
```



*File: winapi.bl*


## win32.CreateMutexA

```c
CreateMutexA :: fn (lpMutexAttributes: LPSECURITY_ATTRIBUTES, bInitialOwner: BOOL, lpName: LPCSTR) HANDLE #extern
```



*File: winapi.bl*


## win32.ReleaseMutex

```c
ReleaseMutex :: fn (hMutex: HANDLE) BOOL #extern
```



*File: winapi.bl*


## win32.GetTickCount

```c
GetTickCount :: fn () DWORD #extern
```



*File: winapi.bl*


## win32.QueryPerformanceCounter

```c
QueryPerformanceCounter :: fn (lpPerformanceCount: *LARGE_INTEGER) BOOL #extern
```



*File: winapi.bl*


## win32.QueryPerformanceFrequency

```c
QueryPerformanceFrequency :: fn (lpPerformanceCount: *LARGE_INTEGER) BOOL #extern
```



*File: winapi.bl*


## win32.GlobalAlloc

```c
GlobalAlloc :: fn (uFlags: UINT, dwBytes: SIZE_T) HGLOBAL #extern
```



*File: winapi.bl*


## win32.GlobalFree

```c
GlobalFree :: fn (hMem: HGLOBAL) HGLOBAL #extern
```



*File: winapi.bl*


## win32.GlobalLock

```c
GlobalLock :: fn (hMem: HGLOBAL) LPVOID #extern
```



*File: winapi.bl*


## win32.GlobalUnlock

```c
GlobalUnlock :: fn (hMem: HGLOBAL) BOOL #extern
```



*File: winapi.bl*


## win32.OpenClipboard

```c
OpenClipboard :: fn (hWndNewOwner: HWND) BOOL #extern
```



*File: winapi.bl*


## win32.EmptyClipboard

```c
EmptyClipboard :: fn () BOOL #extern
```



*File: winapi.bl*


## win32.SetClipboardData

```c
SetClipboardData :: fn (uFormat: UINT, hMem: HANDLE) HANDLE #extern
```



*File: winapi.bl*


## win32.GetClipboardData

```c
GetClipboardData :: fn (uFormat: UINT) HANDLE #extern
```



*File: winapi.bl*


## win32.CloseClipboard

```c
CloseClipboard :: fn () BOOL #extern
```



*File: winapi.bl*


## win32.GetLastError

```c
GetLastError :: fn () DWORD #extern
```



*File: winapi.bl*


## win32.CloseHandle

```c
CloseHandle :: fn (hObject: HANDLE) BOOL #extern
```



*File: winapi.bl*


## win32.GetTempPathA

```c
GetTempPathA :: fn (nBufferLength: DWORD, lpBuffer: LPSTR) DWORD #extern
```



*File: winapi.bl*


## win32.DeleteFileA

```c
DeleteFileA :: fn (lpFileName: LPCSTR) BOOL #extern
```



*File: winapi.bl*


## win32.RemoveDirectoryA

```c
RemoveDirectoryA :: fn (lpPathName: LPCSTR) BOOL #extern
```



*File: winapi.bl*


## win32.GetCurrentDirectoryA

```c
GetCurrentDirectoryA :: fn (nBufferLength: DWORD, lpBuffer: LPSTR) DWORD #extern
```



*File: winapi.bl*


## win32.SetCurrentDirectoryA

```c
SetCurrentDirectoryA :: fn (lpPathName: LPSTR) BOOL #extern
```



*File: winapi.bl*


## win32.GetFileSizeEx

```c
GetFileSizeEx :: fn (hFile: HANDLE, lpFileSize: PLARGE_INTEGER) BOOL #extern
```



*File: winapi.bl*


## win32.ReadFile

```c
ReadFile :: fn (hFile: HANDLE, lpBuffer: LPVOID, nNumberOfBytesToRead: DWORD, lpNumberOfBytesRead: LPDWORD, lpOverlapped: LPOVERLAPPED) BOOL #extern
```



*File: winapi.bl*


## win32.WriteFile

```c
WriteFile :: fn (hFile: HANDLE, lpBuffer: LPVOID, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: LPDWORD, lpOverlapped: LPOVERLAPPED) BOOL #extern
```



*File: winapi.bl*


## win32.CopyFileA

```c
CopyFileA :: fn (lpExistingFileName: LPCSTR, lpNewFileName: LPCSTR, bFailIfExists: BOOL) BOOL #extern
```



*File: winapi.bl*


## win32.SetEndOfFile

```c
SetEndOfFile :: fn (hFile: HANDLE) BOOL #extern
```



*File: winapi.bl*


## win32.SetFilePointer

```c
SetFilePointer :: fn (hFile: HANDLE, lDistanceToMove: LONG, lpDistanceToMoveHigh: PLONG, dwMoveMethod: DWORD) DWORD #extern
```



*File: winapi.bl*


## win32.InitializeCriticalSection

```c
InitializeCriticalSection :: fn (lpCriticalSection: LPCRITICAL_SECTION)  #extern
```



*File: winapi.bl*


## win32.DeleteCriticalSection

```c
DeleteCriticalSection :: fn (lpCriticalSection: LPCRITICAL_SECTION)  #extern
```



*File: winapi.bl*


## win32.EnterCriticalSection

```c
EnterCriticalSection :: fn (lpCriticalSection: LPCRITICAL_SECTION)  #extern
```



*File: winapi.bl*


## win32.LeaveCriticalSection

```c
LeaveCriticalSection :: fn (lpCriticalSection: LPCRITICAL_SECTION)  #extern
```



*File: winapi.bl*


## win32.TryEnterCriticalSection

```c
TryEnterCriticalSection :: fn (lpCriticalSection: LPCRITICAL_SECTION) BOOL #extern
```



*File: winapi.bl*


## win32.GetFileInformationByHandle

```c
GetFileInformationByHandle :: fn (hFile: HANDLE, lpFileInformation: LPBY_HANDLE_FILE_INFORMATION) BOOL #extern
```



*File: winapi.bl*


## win32.GetOverlappedResult

```c
GetOverlappedResult :: fn (hFile: HANDLE, lpOverlapped: LPOVERLAPPED, lpNumberOfBytesTransferred: LPDWORD, bWait: BOOL) BOOL #extern
```



*File: winapi.bl*


## win32.ResetEvent

```c
ResetEvent :: fn (hEvent: HANDLE) BOOL #extern
```



*File: winapi.bl*


## win32.FormatMessageA

```c
FormatMessageA :: fn (dwFlags: DWORD, lpSource: LPVOID, dwMessageId: DWORD, dwLanguageId: DWORD, lpBuffer: LPSTR, nSize: DWORD, Arguments: *u8) DWORD #extern
```



*File: winapi.bl*


## win32.GetCursorPos

```c
GetCursorPos :: fn (lpPoint: LPPOINT) BOOL #extern
```



*File: winapi.bl*


## win32.LoadCursorA

```c
LoadCursorA :: fn (hInstance: HINSTANCE, lpCursorName: LPCSTR) HCURSOR #extern
```



*File: winapi.bl*


## win32.SetCursor

```c
SetCursor :: fn (hCursor: HCURSOR) HCURSOR #extern
```



*File: winapi.bl*


## win32.GetSaveFileNameA

```c
GetSaveFileNameA :: fn (param1: LPOPENFILENAMEA) BOOL #extern
```



*File: winapi.bl*


## win32.GetOpenFileNameA

```c
GetOpenFileNameA :: fn (param1: LPOPENFILENAMEA) BOOL #extern
```



*File: winapi.bl*

