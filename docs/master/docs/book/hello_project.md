# Hello Project

As you maybe noticed from the previous example, our executable file was called *out.exe*, so what if we want to change the name without needing to do it manually after each recompilation? To fix this we need to feed the compiler with some additional settings; we need to create a configuration build file to do so.

One common approach to adjusting the build settings configuration is having some kind of build system involved in the language ecosystem. If you're coming from the C/C++ world, you may know *CMake* or similar tools to manage the project configuration and compilation. However, instead of introducing a whole new system and language, programmers must know to build something, we decided to do it using BL itself, and integrate such a feature directly into the compiler.

## Configuration

To use the BL compiler-integrated build pipeline, we have to create the configuration file first. The compiler expects the project's configuration file to be called `build.bl`. We can reuse our "Hello World" program from the previous chapter, and create `build.bl` file next to it. The most basic build configuration looks like this:

```rust
build :: fn () #build_entry {
    exe := add_executable("hello");
	add_unit(exe, "hello.bl");    
    compile(exe);
}
```

The function *build* marked as a *build_entry* is later automatically executed by the compiler. Inside the function body, we create a new executable target "hello" by *add_executable* function call. Then we add our `hello.bl` source file into it, and finally, we compile the target.

## Compilation

To use our new build script, simply type `blc -build` into the terminal (in the folder where the `build.bl` is located). The newly created executable `hello.exe` should be generated next to the build file.

## How It Works

The build script file is nothing else than the regular BL program executed directly when `-build` compiler flag is used. That means we can do any advanced programming here, you can use `fs` module API to create new files, and directories or to generate any advanced build output. 

See also [Build System](/modules/build)