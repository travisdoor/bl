## TODO
* dont use context in stages, use self instead.
* skipping empty source files
* expression binop priority
* analyze - compare call arguments with callee
* namespaces

## Features 
* classes
* structs

## Syntax hints
### Arrays
    int[n] int_array;
    
### Loops
    break;
    continue;
    
    loop {
    }
    
    while (expr) {
    }
    
    for (x = 0; x < 10; x++) {
    }


### Casting
    bool b;
    int i;
    
    i = b as int;
    
### Types

    bool
    char
    i32
    string
    
### Objects

```c
public struct obj {
  private int a;
  private int b;
  
  public int add() {
    return a + b;
  }
} 

pub struct obj {
  int a; // private by default
  int b;
  
  pub int add() {
    return a + b;
  }
} 
```
