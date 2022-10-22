# Hello World

In this chapter, we try to create a simple BL program printing out "Hello world!" message into the command line.

## The Main Function

To start a new project, we simply need to create a file containing the *main* function used as an application entry point. So let's call our file *hello.bl*. Every BL executable requires the *main* function to be present in the program. Every *main* function is supposed to return *s32* (32-bit signed number) as its result state.

The content of *hello.bl* can look like this:

```rust
main :: fn () s32 {
	print("Hello world!\n");
	return 0;
}
```

### Print

Inside the function body, we call *print* function to print the passed string "Hello world!" into the standard output. The declaration of the *print* function looks like this:

```rust
print :: fn (format: string_view, args: ...) s32 { 
	// ...
}
```

It's declared inside *std/print* module and it's available by default. From the declaration you can see it takes *format* and *args* as input arguments. The type of the *format* argument is compiler internal type *string_view* (representation of any string in the language); *args* argument type is *...* which allows any number of additional values to be passed into the function.

### Return

The last statement in the *main* function body is the `return` statement. The `return` statement, as in other languages, terminates the execution of the current function and provides some return value. In our case, it's just a 0.

## Compile & Run

To compile our simple program, just type `blc hello.bl` into the terminal (in the directory containing `hello.bl` file). The new `out.exe` executable should be created next to your source file.

This way the BL compiler (called `blc`) can produce native executable binary for the platform you're running, however, since the BL code can be also interpreted, we can directly execute our program using the same `blc` command by adding `-run` parameter before the source file name: `blc -run hello.bl`. The *main* function from our example is executed without any native compiled binary creation needed. 

The output should look like this:

```bash
blc -run hello.bl

Executing 'main' in compile time...
Hello world!
Execution finished with state: 0

Finished in 0.031 seconds.
```  
