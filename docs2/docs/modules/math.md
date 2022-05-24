# Math

`#load "std/math.bl"`

## std.PI

```c
PI :: 
```



*File: math.bl*


## std.TWO_PI

```c
TWO_PI :: 
```



*File: math.bl*


## std.HALF_PI

```c
HALF_PI :: 
```



*File: math.bl*


## std.F32_EPSILON

```c
F32_EPSILON :: 
```



*File: math.bl*


## std.F64_EPSILON

```c
F64_EPSILON :: 
```



*File: math.bl*


## std.S8_MIN

```c
S8_MIN : s8 : 
```



*File: math.bl*


## std.S8_MAX

```c
S8_MAX : s8 : 127
```



*File: math.bl*


## std.S16_MIN

```c
S16_MIN : s16 : 
```



*File: math.bl*


## std.S16_MAX

```c
S16_MAX : s16 : 32767
```



*File: math.bl*


## std.S32_MIN

```c
S32_MIN : s32 : 
```



*File: math.bl*


## std.S32_MAX

```c
S32_MAX : s32 : 2147483647
```



*File: math.bl*


## std.S64_MIN

```c
S64_MIN : s64 : 
```



*File: math.bl*


## std.S64_MAX

```c
S64_MAX : s64 : 9223372036854775807
```



*File: math.bl*


## std.U8_MIN

```c
U8_MIN : u8 : 0
```



*File: math.bl*


## std.U8_MAX

```c
U8_MAX : u8 : 255
```



*File: math.bl*


## std.U16_MIN

```c
U16_MIN : u16 : 0
```



*File: math.bl*


## std.U16_MAX

```c
U16_MAX : u16 : 65535
```



*File: math.bl*


## std.U32_MIN

```c
U32_MIN : u32 : 0
```



*File: math.bl*


## std.U32_MAX

```c
U32_MAX : u32 : 4294967295
```



*File: math.bl*


## std.U64_MIN

```c
U64_MIN : u64 : 0
```



*File: math.bl*


## std.U64_MAX

```c
U64_MAX : u64 : 18446744073709551615
```



*File: math.bl*


## std.sin

```c
sin :: fn { f32_sin; f64_sin; }
```



*File: math.bl*


## std.cos

```c
cos :: fn { f32_cos; f64_cos; }
```



*File: math.bl*


## std.floor

```c
floor :: fn { f32_floor; f64_floor; }
```



*File: math.bl*


## std.round

```c
round :: fn { f32_round; f64_round; }
```



*File: math.bl*


## std.pow

```c
pow :: fn { f32_pow; f64_pow; }
```



*File: math.bl*


## std.log

```c
log :: fn { f32_log; f64_log; }
```



*File: math.bl*


## std.log2

```c
log2 :: fn { f32_log2; f64_log2; }
```



*File: math.bl*


## std.log10

```c
log10 :: fn { f32_log10; f64_log10; }
```



*File: math.bl*


## std.sqrt

```c
sqrt :: fn { f32_sqrt; f64_sqrt; }
```



*File: math.bl*


## std.ceil

```c
ceil :: fn { f32_ceil; f64_ceil; }
```



*File: math.bl*


## std.atan2

```c
atan2 :: fn { C.atan2f; C.atan2; }
```



*File: math.bl*


## std.copysign

```c
copysign :: fn { C.copysignf; C.copysign; }
```



*File: math.bl*


## std.asin

```c
asin :: fn { C.asinf; C.asin; }
```



*File: math.bl*


## std.acos

```c
acos :: fn { C.acosf; C.acos; }
```



*File: math.bl*


## std.tan

```c
tan :: fn { C.tanf; C.tan; }
```



*File: math.bl*


## std.abs

```c
abs :: fn { f32_abs; f64_abs; }
```



*File: math.bl*


## std.compare

```c
compare :: fn { f32_compare; f64_compare; }
```



*File: math.bl*


## std.degtorad

```c
degtorad :: fn { f32_degtorad; f64_degtorad; }
```



*File: math.bl*


## std.radtodeg

```c
radtodeg :: fn { f32_radtodeg; f64_radtodeg; }
```



*File: math.bl*


## std.max

```c
max :: fn (a: ?T, b: T) T #inline
```



*File: math.bl*


## std.min

```c
min :: fn (a: ?T, b: T) T #inline
```



*File: math.bl*


## std.lerp

```c
lerp :: fn (a: ?T, b: T, fraction: T) T #inline
```



*File: math.bl*


## std.clamp

```c
clamp :: fn (v: ?T, minimum: T, maximum: T) T
```



*File: math.bl*

