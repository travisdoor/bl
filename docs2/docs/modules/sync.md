# Thread synchronization

`#import "std/sync"`

Multiplatform implementation of various thread synchronization tools.

**warning**: This module is experimental and not fully supported across all platforms.

## std.Mutex

```c
Mutex :: _sync_impl.Mutex
```

Mutex type.



*File: sync.bl*


## std.mutex_init

```c
mutex_init :: fn (mutex: *Mutex)  #inline
```

Initialize mutex. Call :ref:`terminate` when mutex is no longer needed.



*File: sync.bl*


## std.mutex_terminate

```c
mutex_terminate :: fn (mutex: *Mutex)  #inline
```

Terminate mutex.



*File: sync.bl*


## std.mutex_lock

```c
mutex_lock :: fn (mutex: *Mutex)  #inline
```

Lock mutex and enter critical section.



*File: sync.bl*


## std.mutex_try_lock

```c
mutex_try_lock :: fn (mutex: *Mutex) bool #inline
```



*File: sync.bl*


## std.mutex_unlock

```c
mutex_unlock :: fn (mutex: *Mutex)  #inline
```

Unlock previously locked mutex and leave critical section.



*File: sync.bl*

