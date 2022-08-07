# Compiler usage

Biscuit language compiler is standalone terminal application called blc.  It can be compiled from
source code found on GitHub repository or downloaded from home page as binary executable. All three
major operating systems (Windows, macOS and Linux) are supported, but current active development is
done on Windows and it usually takes some time to port latest changes to the other platforms.
Compiler executable can be found in bin directory it's usually good idea to add executable location
to system PATH to be accessible from other locations.

There are several options which can be passed to the compiler.

## Compiler options:

```text
blc --help
Usage:
  blc [options] [source-files]

Alternative usage:
  blc [options] <-build> [build-arguments]
  blc [options] <-run> <source-file> [arguments] [forwarded-arguments]

Options:
  -build                   Invoke project build pipeline. All following arguments are
                           forwarded into the build script and ignored by compiler itself.
                           Use as '-build [arguments]'.
  -doc                     Generate documentation and exit.
  -opt=<debug|release-fast|release-small>
                           Specify binary optimization mode (use 'debug' by default).
  -release                 Specify binary optimization mode to release. (same as
                           '-opt=release-fast')
  -run                     Execute BL program using interpreter and exit. The compiler
                           expects <source-file> after '-run' flag, the file name and
                           all following command line arguments are passed into the
                           executed program and ignored by compiler itself. Use as
                           '-run <source-file> [arguments]'.
  -shared                  Compile shared library.
  --about                  Print compiler info and exit
  --assert=<default|on|off>
                           Set assert mode ('default' option sets assert 'on' in debug
                           and 'off' in release mode).
  --ast-dump               Print AST.
  --configure              Generate configuration file and exit.
  --di=<dwarf|codeview>    Set debug info format.
  --doc-out-dir=<STRING>   Set documentation output directory. (Use 'out' in current
                           working directory by default.)
  --emit-asm               Write assembly to file.
  --emit-llvm              Write LLVM-IR to file.
  --emit-mir               Write MIR to file.
  --error-limit=<N>        Set maximum reported error count.
  --full-path              Report full file paths.
  --help                   Print usage information and exit.
  --lex-dump               Print tokens.
  --no-analyze             Disable analyze pass, only parse and exit.
  --no-api                 Don't load internal API.
  --no-bin                 Don't write binary to disk.
  --no-color               Disable colored output.
  --no-jobs                Enable single-thread mode.
  --no-llvm                Disable LLVM back-end.
  --no-usage-check         Disable checking of unused symbols.
  --no-warning             Ignore all warnings.
  --reg-split=<off|on>     Enable/disable splitting of structures passed into the
                           function by value into registers.
  --run-tests              Execute all unit tests in compile time.
  --silent                 Disable compiler console logging.
  --syntax-only            Check syntax and exit.
  --time-report            Print compilation time report.
  --verbose                Enable verbose mode.
  --verify-llvm            Verify LLVM IR after generation.
  --vmdbg-attach           Attach compile-time execution debugger.
  --vmdbg-break-on=<N>     Attach compile-time execution debugger and sets break
                           point to the MIR instruction with <N> id.
  --where-is-api           Return path to API folder and exit.
```

## Configuration

Use `bl-config` tool to change compiler configuration. This tool generates configuration file
`etc/bl.conf` containing all required information needed by compiler during compilation.

## Execution status

- After regular compilation process `blc` return 0 on success or numeric maximum error code on fail.
- When `-run` flag is specified `blc` return status returned by executed `main` function on success
or numeric maximum error code on fail (compilation error or compile time execution error).
- When `--run-tests` flag is specified `blc` return count of failed tests on success or numeric
maximum error code on fail.

# Compile-time debugger

!!! warning
    This feature is not complete, it's supposed to be used by compiler developers to
    simplify compiler debugging.

Since the compile-time execution is one of the most powerful things on BL and i.e. command-line
utility scripts are executed using the interpreter almost every time, we have to provide a proper
way how to debug them. In the case of the compile-time execution, no binary file is produced, so we
cannot use external debuggers as gdb or Visual Studio, however, the compiler can be used for
debugging directly.

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

The mistake here is obvious, we're dereferencing the null pointer and the program will just crash
during the execution with following error:

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

To track down the error we can use the compiler built-in function `debugbreak`, causing the
execution to be stopped when the interpreter reaches this call.

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

When the breakpoint is specified we must execute the program with the compile-time debugger attached
to get the actual break. This can be done by the `--vmdbg-attach` command-line argument. 

```text
$blc --vmdbg-attach -run test.bl   
                                    
Executing 'main' in compile time... 
                                    
Hit breakpoint in assembly 'out'.   
                                    
   6 | main :: fn () s32 {          
>  7 |     debugbreak;              
   8 |     my_function();
```

As you can see, the execution breaks on `debugbreak` call, and the debugger waits for user input,
type `h` and hit `return` to get all available commands.

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

Now we can step through our code, get some stack-related information, print stack traces on
user-code level, but also on MIR instruction level. Printing values of variables is not supported
yet.

# Build System

Biscuit has an integrated build system replacing CMake or similar tools.  Main advantage is
integration of the build system directly into the compiler. All you need is `build.bl` file
containing `#build_entry` function. Setup file is nothing more than a simple BL program executed in
compile time with some special features enabled. See `Build_System` for more information.

# Script mode

Programs written in Biscuit can easily act like shell scripts on UNIX systems due to support of
`shebang` character sequence specified at the first line of the entry file. The `--silent -run`
options passed to the compiler reduces all compiler diagnostic output and executes the `main`
function in compile time. No output binary is produced in such a case. Following example can be
directly executed in `bash` as it was executable.

```c
#!/usr/local/bin/blc --silent -run

main :: fn () s32 {
    print("Hello!!!\n");
    return 0;
}
```

```text
$ chmod +x main.bl
$ ./main.bl
```

All additional arguments after `-run` option are automatically forwarded into the executed script
and can be accessed via `command_line_arguments` builtin variable during compile-time execution.
First argument (index 0) contains the script name every time.

# Automatic Documentation

Integrated self-documentation tool can be used to generate
[Markdown](https://en.wikipedia.org/wiki/Markdown) files from Biscuit source files automatically.
Documentation text can be attached to file by `//!` comment prefix or to declaration by `///`
comment prefix.  Such comments will be recognised by the compiler and attached to proper declaration
or file compilation unit. Use `-doc` compiler flag followed by a list of files you want to generate
documentation for. Documentation output will be written to `out` directory into the current working
directory if `--doc-out-dir` is not specified.

Use marker `@INCOMPLETE` in documentation comment to mark it as incomplete. Compiler will warn you
about symbols with incomplete documentation during generation.

Documentation rules:

- Only files listed in compiler input are used as generation input (no loaded or imported files are
included).
- Documentation is generated from AST; only parsing is required, after that compiler exits.
- When out directory directory already exists, compiler will only append new files and override old
in case of collision.
- Only global and non-private declarations can be documented.
- Declaration name and declaration itself are included automatically.
- Single `md` file is produced for every input source file.

Example of documented `print` function:

```c
/// Write string to the standard output (stdout). Format string can include format specifiers `%`
/// which are replaced by corresponding argument value passed in `args`. Value-string conversion is
/// done automatically, we can pass values of any type as an arguments, even structures or arrays.
///
/// The `print` function accepts C-like escape sequences as `\n`, `\t`, `\r`, etc.
///
/// Pointers to `Error` are dereferenced automatically; so the `print` function can print out errors
/// directly.
///
/// Count of printed bytes is returned.
print :: fn (format: string, args: ...) s32 {
    buf: [PRINT_MAX_LENGTH]u8 #noinit;
    w := _print_impl(buf, format, args);
    __os_write(OS_STDOUT, buf.ptr, auto w);
    return w;
}
```

Execution of `blc -doc print.bl` will produce following output:

````md
## print

```
print :: fn (format: string, args: ...) s32
```

Write string to the standard output (stdout). Format string can include format specifiers `%`
which are replaced by corresponding argument value passed in `args`. Value-string conversion is
done automatically, we can pass values of any type as an arguments, even structures or arrays.

The `print` function accepts C-like escape sequences as `\n`, `\t`, `\r`, etc.

Pointers to `Error` are dereferenced automatically; so the `print` function can print out errors
directly.

Count of printed bytes is returned.
````


