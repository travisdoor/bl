
# arm64.bl

## C.opendir

```c
opendir :: fn (name: *char) *DIR #extern
```



*File: arm64.bl*


## C.fdopendir

```c
fdopendir :: fn (fd: int) *DIR #extern
```



*File: arm64.bl*


## C.readdir

```c
readdir :: fn (dirp: *DIR) *dirent_t #extern
```



*File: arm64.bl*


## C.rewinddir

```c
rewinddir :: fn (dirp: *DIR)  #extern
```



*File: arm64.bl*


## C.stat

```c
stat :: fn (path: *char, buf: *stat_t) int #extern
```



*File: arm64.bl*


## C.fstat

```c
fstat :: fn (fd: int, stat: *stat_t) int #extern
```



*File: arm64.bl*

