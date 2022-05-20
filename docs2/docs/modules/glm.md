# GLM

`#import "std/glm"`

3D math tools.

## glm.v2

```c
v2 :: struct {
    x: f32;
    y: f32;
}
```



*File: glm.bl*


## glm.v2_one

```c
v2_one :: 
```



*File: glm.bl*


## glm.v2_zero

```c
v2_zero :: 
```



*File: glm.bl*


## glm.v3

```c
v3 :: struct {
    x: f32;
    y: f32;
    z: f32;
}
```



*File: glm.bl*


## glm.v3_right

```c
v3_right :: 
```



*File: glm.bl*


## glm.v3_up

```c
v3_up :: 
```



*File: glm.bl*


## glm.v3_forward

```c
v3_forward :: 
```



*File: glm.bl*


## glm.v3_one

```c
v3_one :: 
```



*File: glm.bl*


## glm.v3_zero

```c
v3_zero :: 
```



*File: glm.bl*


## glm.v4

```c
v4 :: struct {
    x: f32;
    y: f32;
    z: f32;
    w: f32;
}
```



*File: glm.bl*


## glm.v4_one

```c
v4_one :: 
```



*File: glm.bl*


## glm.v4_zero

```c
v4_zero :: 
```



*File: glm.bl*


## glm.color_white

```c
color_white :: 
```



*File: glm.bl*


## glm.color_black

```c
color_black :: 
```



*File: glm.bl*


## glm.color_red

```c
color_red :: 
```



*File: glm.bl*


## glm.color_green

```c
color_green :: 
```



*File: glm.bl*


## glm.color_blue

```c
color_blue :: 
```



*File: glm.bl*


## glm.color_yellow

```c
color_yellow :: 
```



*File: glm.bl*


## glm.iv2

```c
iv2 :: struct {
    x: s32;
    y: s32;
}
```



*File: glm.bl*


## glm.iv3

```c
iv3 :: struct {
    x: s32;
    y: s32;
    z: s32;
}
```



*File: glm.bl*


## glm.mat4

```c
mat4 :: 
```



*File: glm.bl*


## glm.quat

```c
quat :: v4
```



*File: glm.bl*


## glm.quat_identity

```c
quat_identity :: 
```



*File: glm.bl*


## glm.sub

```c
sub :: fn { v2_sub; v2_sub_s; v3_sub; v3_sub_s; v4_sub; v4_sub_s; }
```



*File: glm.bl*


## glm.add

```c
add :: fn { v2_add; v2_add_s; v3_add; v3_add_s; v4_add; v4_add_s; }
```



*File: glm.bl*


## glm.mul

```c
mul :: fn { v2_mul; v2_mul_s; v3_mul; v3_mul_s; v4_mul; v4_mul_s; }
```



*File: glm.bl*


## glm.div

```c
div :: fn { v2_div; v2_div_s; v3_div; v3_div_s; v4_div; v4_div_s; }
```



*File: glm.bl*


## glm.length

```c
length :: fn { v2_length; v3_length; v4_length; }
```



*File: glm.bl*


## glm.sqr_length

```c
sqr_length :: fn { v2_sqr_length; v3_sqr_length; v4_sqr_length; }
```



*File: glm.bl*


## glm.dot

```c
dot :: fn { v2_dot; v3_dot; v4_dot; }
```



*File: glm.bl*


## glm.normalize

```c
normalize :: fn { v2_normalize; v3_normalize; v4_normalize; }
```



*File: glm.bl*


## glm.compare

```c
compare :: fn { v2_compare; v3_compare; v4_compare; }
```



*File: glm.bl*


## glm.negate

```c
negate :: fn { v2_negate; v3_negate; v4_negate; }
```



*File: glm.bl*


## glm.cross

```c
cross :: fn { v3_cross; }
```



*File: glm.bl*


## glm.round

```c
round :: fn { v2_round; v3_round; v4_round; }
```



*File: glm.bl*


## glm.floor

```c
floor :: fn { v2_floor; v3_floor; v4_floor; }
```



*File: glm.bl*


## glm.ceil

```c
ceil :: fn { v2_ceil; v3_ceil; v4_ceil; }
```



*File: glm.bl*


## glm.mat4_init

```c
mat4_init :: fn (m00: f32, m10: f32, m20: f32, m30: f32, m01: f32, m11: f32, m21: f32, m31: f32, m02: f32, m12: f32, m22: f32, m32: f32, m03: f32, m13: f32, m23: f32, m33: f32, out_mat: *mat4)  #inline
```



*File: glm.bl*


## glm.mat4_identity

```c
mat4_identity :: fn (out_mat: *mat4)  #inline
```



*File: glm.bl*


## glm.mat4_zero

```c
mat4_zero :: fn (out_mat: *mat4)  #inline
```



*File: glm.bl*


## glm.mat4_perspective

```c
mat4_perspective :: fn (fov_rad: f32, aspect: f32, near: f32, far: f32, out_mat: *mat4)  #inline
```



*File: glm.bl*


## glm.mat4_ortho

```c
mat4_ortho :: fn (left: f32, right: f32, bottom: f32, top: f32, near: f32, far: f32, out_mat: *mat4) 
```



*File: glm.bl*


## glm.mat4_look_at

```c
mat4_look_at :: fn (eye: v3, target: v3, up: v3, out_mat: *mat4)  #inline
```



*File: glm.bl*


## glm.mat4_mul

```c
mat4_mul :: fn (a: *mat4, b: *mat4, out_mat: *mat4) 
```



*File: glm.bl*


## glm.mat4_inverse

```c
mat4_inverse :: fn (src_mat: *mat4, out_mat: *mat4)  #inline
```



*File: glm.bl*


## glm.mat4_scale_s

```c
mat4_scale_s :: fn (s: f32, out_mat: *mat4)  #inline
```



*File: glm.bl*


## glm.mat4_mul_v4

```c
mat4_mul_v4 :: fn (mat: *mat4, v: v4) v4 #inline
```



*File: glm.bl*


## glm.mat4_translation

```c
mat4_translation :: fn (translation: v3, out_mat: *mat4)  #inline
```



*File: glm.bl*


## glm.mat4_scale

```c
mat4_scale :: fn (s: v3, out_mat: *mat4)  #inline
```



*File: glm.bl*


## glm.mat4_get_scale

```c
mat4_get_scale :: fn (mat: *mat4) v3
```



*File: glm.bl*


## glm.mat4_get_translation

```c
mat4_get_translation :: fn (mat: *mat4) v3 #inline
```



*File: glm.bl*


## glm.mat4_get_quat

```c
mat4_get_quat :: fn (mat: *mat4) quat
```



*File: glm.bl*


## glm.quat_normalize

```c
quat_normalize :: fn (q: quat) quat #inline
```



*File: glm.bl*


## glm.quat_to_mat4

```c
quat_to_mat4 :: fn (q: quat, out_mat: *mat4) 
```



*File: glm.bl*


## glm.quat_look

```c
quat_look :: fn (eye: v3, ori: quat, out_mat: *mat4) 
```



*File: glm.bl*


## glm.quat_look_for

```c
quat_look_for :: fn (dir: v3, fwd: v3, up: v3) quat
```



*File: glm.bl*


## glm.quat_axis_angle

```c
quat_axis_angle :: fn (axis: v3, angle_rad: f32) quat
```



*File: glm.bl*


## glm.quat_imag

```c
quat_imag :: fn (q: quat) v3 #inline
```



*File: glm.bl*


## glm.quat_real

```c
quat_real :: fn (q: quat) f32 #inline
```



*File: glm.bl*


## glm.quat_mul

```c
quat_mul :: fn (q1: quat, q2: quat) quat
```



*File: glm.bl*


## glm.quat_rotate_v3

```c
quat_rotate_v3 :: fn (q: quat, v: v3) v3
```



*File: glm.bl*


## glm.quat_forward

```c
quat_forward :: fn (q: quat) v3
```



*File: glm.bl*

