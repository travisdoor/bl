# Compile-time Debugger

> **Warning:**
This feature is not complete, it's supposed to be used by compiler developers.

Since the compile-time execution is one of the most powerful things on BL and i.e. command-line utility scripts are executed using the interpreter almost every time, we have to provide a proper way how to debug them. In the case of the compile-time execution, no binary file is produced, so we cannot use external debuggers as gdb or Visual Studio, however, the compiler can be used for debugging directly.

Consider the following example program:

```c
my_function :: fn () {
    ptr: *s32 = null;
    @ptr = 10; // error
}

main :: fn () s32 {
    my_function();
    return 0;
}
```

The mistake here is obvious, we're dereferencing the null pointer and the program will just crash during the execution with the  following error:

```text
$blc -run test.bl

Executing 'main' in compile time...                                             
error: Dereferencing null pointer!                                              
                                                                                
================================================================================
Obtained backtrace:                                                             
================================================================================
test.bl:3:5: Last called:                                                       
   2 |     ptr: *s32 = null;                                                    
>  3 |     @ptr = 10; // error                                                  
   4 | }                                                                        
                                                                                
test.bl:8:5: Called from:                                                       
   7 |     debugbreak;                                                          
>  8 |     my_function();                                                       
   9 |     return 0;                                                            
```

To track down the error we can use the compiler built-in function `debugbreak`, causing the execution to be stopped when the interpreter reaches this call.

```c
my_function :: fn () {
    ptr: *s32 = null;
    @ptr = 10; // error
}

main :: fn () s32 {
    debugbreak; // break here
    my_function();
    return 0;
}
```

When the breakpoint is specified we must execute the program with the compile-time debugger attached to get the actual break. This can be done by the `--vmdbg-attach` command-line argument. 

```text
$blc --vmdbg-attach -run test.bl   
                                    
Executing 'main' in compile time... 
                                    
Hit breakpoint in assembly 'out'.   
                                    
   6 | main :: fn () s32 {          
>  7 |     debugbreak;              
   8 |     my_function();
```

As you can see, the execution breaks on `debugbreak` call, and the debugger waits for user input, type `h` and hit `return` to get all available commands.

```text
: h                                                                                    
  h, help                             = Show this help.                                
  q, quit                             = Stop debugging.                                
  n, next                             = Step to next instruction.                      
  c, continue                         = Continue execution.                            
  p, print                            = Print current instruction.                     
  bt, backtrace                       = Print current backtrace.                       
  vs=<on|off>, verbose-stack=<on|off> = Log stack operations.                          
  mir=<on|off>, mir-mode=<on|off>     = Enable/disable MIR instruction level debugging.
```

Now we can step through our code, get some stack-related information, print stack traces on the user-code level, but also on MIR instruction level. Printing values of variables is supported only partially.


