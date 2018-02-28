# Biscuit Desing

## Modules

Source is divided into separate modules. Everything inside module
is public for this module and private for other modules.

Module definition:
```
module my_module {
  ...
}
```

## Functions
### Static
Static functions are not associated with any data. It must be
defined in module body.

```
void my_function(i32 arg1, f32 arg2) {
  ...
}
```

### Extern
External functions allows calling of plain C function.
```
extern i32 puts(string s);
```

## Data
### Fundamental

```
void    - void type
char    - 1 byte character
bool    - 1 byte boolean type
i32     - 4 byte signed number
i64     - 8 byte signed number
u32     - 4 byte unsigned number
u64     - 8 byte unsigned number
f32     - 4 byte float number
f64     - 8 byte float number

string  - 1 byte pointer to string array
```

### Enum
```
public enum state : i32 {
  Init = 0,
  Read,
  Write,
  Success,
  Fail,
  Done 
}

state s;
s.Init;
```

### Struct
```
struct vector {
  i32 x,
  i32 y
}

vector vec;
vec.x = 10;
```

## Behavior

Possible manipulation with _vector_ data:

```
struct vector {
  i32 x,
  i32 y
}

impl vector {
  public i32 sum() {
    return x + y;
  }
}
```

### Abstract behavior

We can design behavior applicable on any data. 

```
behavior print {
  void print();
}
```

And assign int to any data type.

```
impl vector : print {
  public i32 sum() {
    return x + y;
  }  
  
  public void print() {
    printf("%d %d\n", x, y);
  }  
}
```

From now every vector can calculate it's sum and it can be printed.

```
void main() {
  vector my_vector;
  
  i32 sum = my_vector.sum();
  my_vector.print();
}
```

## Flow control
### If statement

```
if (expr == true) {
  ...
} else if (expr == true) {
  ...
} else {
```

### Loop

Simple endless loop:
```
loop {
  break;
  continue;
}
```



