
# libc.bl

## C.char

```c
char :: s8
```



*File: libc.bl*


## C.uchar

```c
uchar :: u8
```



*File: libc.bl*


## C.short

```c
short :: s16
```



*File: libc.bl*


## C.ushort

```c
ushort :: u16
```



*File: libc.bl*


## C.int

```c
int :: s32
```



*File: libc.bl*


## C.uint

```c
uint :: u32
```



*File: libc.bl*


## C.long

```c
long :: s32
```



*File: libc.bl*


## C.ulong

```c
ulong :: u32
```



*File: libc.bl*


## C.longlong

```c
longlong :: s64
```



*File: libc.bl*


## C.ulonglong

```c
ulonglong :: u64
```



*File: libc.bl*


## C.void_ptr

```c
void_ptr :: *u8
```



*File: libc.bl*


## C.size_t

```c
size_t :: u64
```



*File: libc.bl*


## C.ssize_t

```c
ssize_t :: s64
```



*File: libc.bl*


## C.STDIN

```c
STDIN :: 0
```



*File: libc.bl*


## C.STDOUT

```c
STDOUT :: 1
```



*File: libc.bl*


## C.STDERR

```c
STDERR :: 2
```



*File: libc.bl*


## C.RAND_MAX

```c
RAND_MAX :: 2147483647
```



*File: libc.bl*


## C.SIGHUP

```c
SIGHUP :: 1
```



*File: libc.bl*


## C.SIGINT

```c
SIGINT :: 2
```



*File: libc.bl*


## C.SIGQUIT

```c
SIGQUIT :: 3
```



*File: libc.bl*


## C.SIGILL

```c
SIGILL :: 4
```



*File: libc.bl*


## C.SIGTRAP

```c
SIGTRAP :: 5
```



*File: libc.bl*


## C.SIGABRT

```c
SIGABRT :: 6
```



*File: libc.bl*


## C.putenv

```c
putenv :: fn (var: *char) int #extern
```



*File: libc.bl*


## C.popen

```c
popen :: fn (f: *char, m: *char) void_ptr #extern
```



*File: libc.bl*


## C.pclose

```c
pclose :: fn (f: void_ptr)  #extern
```



*File: libc.bl*


## C.write

```c
write :: fn (fd: int, buf: *char, count: size_t) ssize_t #extern
```



*File: libc.bl*


## C.read

```c
read :: fn (fd: int, buf: *char, count: size_t) ssize_t #extern
```



*File: libc.bl*


## C.exit

```c
exit :: fn (v: int)  #extern
```



*File: libc.bl*


## C.realpath

```c
realpath :: fn (path: *char, resolved_path: *char) *char #extern
```



*File: libc.bl*


## C.raise

```c
raise :: fn (sig: int) int #extern
```



*File: libc.bl*


## C.signal

```c
signal :: fn (signum: int, handler: *fn (v: int) ) *fn (v: int)  #extern
```



*File: libc.bl*


## C.malloc

```c
malloc :: fn (size: size_t) void_ptr #extern
```



*File: libc.bl*


## C.realloc

```c
realloc :: fn (ptr: void_ptr, new_size: size_t) void_ptr #extern
```



*File: libc.bl*


## C.free

```c
free :: fn (ptr: void_ptr)  #extern
```



*File: libc.bl*


## C.aligned_malloc

```c
aligned_malloc :: fn (size: size_t, alignment: size_t) void_ptr #extern
```



*File: libc.bl*


## C.aligned_realloc

```c
aligned_realloc :: fn (memblock: void_ptr, size: size_t, alignment: size_t) void_ptr #extern
```



*File: libc.bl*


## C.aligned_free

```c
aligned_free :: fn (memblock: void_ptr)  #extern
```



*File: libc.bl*


## C.getenv

```c
getenv :: fn (name: *char) *char #extern
```



*File: libc.bl*


## C.system

```c
system :: fn (command: *char) int #extern
```



*File: libc.bl*


## C.strlen

```c
strlen :: fn (str: *char) size_t #extern
```



*File: libc.bl*


## C.strcmp

```c
strcmp :: fn (first: *char, second: *char) int #extern
```



*File: libc.bl*


## C.wcslen

```c
wcslen :: fn (str: *u16) usize #extern
```



*File: libc.bl*


## C.strerror

```c
strerror :: fn (errnum: int) *char #extern
```



*File: libc.bl*


## C.fgets

```c
fgets :: fn (buf: void_ptr, size: int, f: void_ptr) void_ptr #extern
```



*File: libc.bl*


## C.getchar

```c
getchar :: fn () int #extern
```



*File: libc.bl*


## C.rand

```c
rand :: fn () s32 #extern
```



*File: libc.bl*


## C.srand

```c
srand :: fn (seed: u32)  #extern
```



*File: libc.bl*


## C.qsort

```c
qsort :: fn (base: void_ptr, nitems: size_t, size: size_t, compar: *fn (: void_ptr, : void_ptr) int)  #extern
```



*File: libc.bl*


## C.asinf

```c
asinf :: fn (arg: f32) f32 #extern
```



*File: libc.bl*


## C.asin

```c
asin :: fn (arg: f64) f64 #extern
```



*File: libc.bl*


## C.acosf

```c
acosf :: fn (n: f32) f32 #extern
```



*File: libc.bl*


## C.acos

```c
acos :: fn (n: f64) f64 #extern
```



*File: libc.bl*


## C.tanf

```c
tanf :: fn (x: f32) f32 #extern
```



*File: libc.bl*


## C.tan

```c
tan :: fn (x: f64) f64 #extern
```



*File: libc.bl*


## C.atan2f

```c
atan2f :: fn (y: f32, x: f32) f32 #extern
```



*File: libc.bl*


## C.atan2

```c
atan2 :: fn (y: f64, x: f64) f64 #extern
```



*File: libc.bl*


## C.copysignf

```c
copysignf :: fn (x: f32, y: f32) f32 #extern
```



*File: libc.bl*


## C.copysign

```c
copysign :: fn (x: f64, y: f64) f64 #extern
```



*File: libc.bl*


## C.tolower

```c
tolower :: fn (c: int) int #extern
```



*File: libc.bl*


## C.toupper

```c
toupper :: fn (c: int) int #extern
```



*File: libc.bl*

