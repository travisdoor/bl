
# pthread.bl

## pthread_mutex_t

```c
pthread_mutex_t :: struct {
    __sig: C.long;
    __opaque: ;
}
```



*File: pthread.bl*


## pthread_t

```c
pthread_t :: *u8
```



*File: pthread.bl*


## pthread_attr_t

```c
pthread_attr_t :: *u8
```



*File: pthread.bl*


## pthread_start_routine_t

```c
pthread_start_routine_t :: *fn (args: C.void_ptr) C.void_ptr
```



*File: pthread.bl*


## pthread_create

```c
pthread_create :: fn (thread: *pthread_t, attr: pthread_attr_t, start_routine: pthread_start_routine_t, args: C.void_ptr) C.int #extern
```



*File: pthread.bl*


## pthread_join

```c
pthread_join :: fn (thread: pthread_t, retval: *C.void_ptr) C.int #extern
```



*File: pthread.bl*


## pthread_exit

```c
pthread_exit :: fn (retval: C.void_ptr)  #extern
```



*File: pthread.bl*


## pthread_self

```c
pthread_self :: fn () pthread_t #extern
```



*File: pthread.bl*


## pthread_mutex_lock

```c
pthread_mutex_lock :: fn (mutex: *pthread_mutex_t) C.int #extern
```



*File: pthread.bl*


## pthread_mutex_trylock

```c
pthread_mutex_trylock :: fn (mutex: *pthread_mutex_t) C.int #extern
```



*File: pthread.bl*


## pthread_mutex_unlock

```c
pthread_mutex_unlock :: fn (mutex: *pthread_mutex_t) C.int #extern
```



*File: pthread.bl*


## pthread_mutex_init

```c
pthread_mutex_init :: fn (mutex: *pthread_mutex_t, attr: C.void_ptr) C.int #extern
```



*File: pthread.bl*


## pthread_mutex_destroy

```c
pthread_mutex_destroy :: fn (mutex: *pthread_mutex_t) C.int #extern
```



*File: pthread.bl*

