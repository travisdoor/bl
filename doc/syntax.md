# ![alt text](biscuit_logo.png "logo") The Biscuit Language

## Data types
```
void 
bool   - 1b
char   - 1B
i32    - 4B
i64    - 8B
string - 1B
```

## Variable
```
i32 my_integer;
i32 my_integer = [expr];
```

## Function

C extern function declaration in global scope:
```
extern i32 printf(string s);
```

Function declaration in global scope:
```
i32 add(i32 a, i32 b) {
  return a + b;
}
```

Main function
```
i32 main() {
  return 0;
}
```

## Expressions 
### Operators
```
*
/
+
-
<
>
<=
>=
==
!=
&
^
|
&&
||
=
```

### Method call
```
i32 result = add(10, 10);
```

## Flow control
### If - else
```
if (expr) {
} else {
}
```

### Loop
```
i32 i;
loop {
  i = i + 1;
  if (i == 100) {
    break; // break loop
  }

  continue; // next loop iteration
  // never reached code
}
```
