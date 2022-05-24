# Build system

`#load "build/build.bl"`

Compiler integrated build pipeline. Build pipline can be used to manage whole project compilation 
process directly in BL. All you need is to create the build file called `build.bl` and specify the
`build_entry` function inside. When `-b` flag is used without need of specifying any files, the
compiler will lookup `build.bl` file in the current directory and execute it in compile time. All 
compilation steps, target input and output can be specified here. 

Example of minimal build.bl:
```
build :: fn () #build_entry {
    // create new executable target
    exe :: add_executable("MyProgram");

    // add 'main.bl' file into the target 'exe'
    add_unit(exe, "main.bl");

    // Start compilation
    compile(exe);
}
```

Start build pipeline using our build.bl file:
``` 
$ blc -build
``` 

Compiler will automatically use build.bl file as build script and execute build function in compile time.
SDK file build/build.bl containing compiler API for build pipeline manipulation is loaded implicitly.

**warning**: Build API is available only in compile-time.

## Basics

### Create new executable target

Target is a single build target defined as consisting of build Units representing source files
needed for compilation. It`s basically a target compiled into an executable or binary file. Use
[add_executable](#add_executable) function to specify your target. There are several options 
related to target, described later  in this documentation.

```
exe :: add_executable("MyProgram");
```

### Add file into target

Use [add_unit](#add_unit) function to add source files into the target. There is no need to add 
all files you want to use, general `load` and `import` will do so automatically. Only `main` or 
entry files must be included.

```
add_unit(exe, "main.bl");
```

### Specify output directory

Output directory is a directory where all compiler-produced files will be written (i.e. native 
executables). Use [set_output_dir](#set_output_dir) function to specify this directory, current 
directory is used by default.

```
set_output_dir(exe, "bin");
```

### Compile target
```
compile(exe);
```

### Command line argumets
All argumets passed after ``-build`` compiler flag are automatically forwarded into
`command_line_arguments` global variable.

## Target

```c
Target :: struct {
    kind: TargetKind;
    build_mode: BuildMode;
    debug_info_kind: DebugInfo;
    register_split: bool;
    verify_llvm: bool;
    run_tests: bool;
    tests_minimal_output: bool;
    no_api: bool;
    copy_dependencies: bool;
    run: bool;
    print_tokens: bool;
    print_ast: bool;
    emit_llvm: bool;
    emit_asm: bool;
    emit_mir: bool;
    no_bin: bool;
    no_llvm: bool;
    no_analyze: bool;
    assert_mode: AssertMode;
    syntax_only: bool;
    vmdbg_enabled: bool;
    vmdbg_break_on: s32;
}
```

Target is representation of whole program workspace, it's a consist of Units, every unit 
represents one source file.



### Members
* `kind` - See :ref:`TargetKind`.
* `build_mode` - Specify build mode of the target. See [BuildMode](#buildmode)
* `debug_info_kind` - Specify debug information format used for target in debug mode. See [DebugInfo](#debuginfo).
* `register_split` - Enable split of function arguments and return value into registers.
* `verify_llvm` - Verify LLVM module.
* `run_tests` - Execute compile time tests.
* `tests_minimal_output` - Reduce compile-time tests output (remove results section).
* `no_api` - Disable default API import.
* `copy_dependencies` - Copy all known dependencies into output folder.
* `run` - Execute main function in compile time.
* `print_tokens` - Print out lexer output.
* `print_ast` - Print out AST.
* `emit_llvm` - Emit LLVM IR code into file.
* `emit_asm` - Emit asm code into file.
* `emit_mir` - Emit MIR code into file.
* `no_bin` - Disable generation of native binary.
* `no_llvm` - Disable LLVM backend.
* `no_analyze` - Disable analyze pass of code generation.
* `assert_mode` - See :ref:`AssertMode`.
* `syntax_only` - Check only code syntax.
* `vmdbg_enabled` - Enable virtual machine debugger.
* `vmdbg_break_on` - Specify MIR instruction ID to break on if virtual machine debugger is attached.


*File: build.bl*


## TargetKind

```c
TargetKind :: enum {
    EXECUTABLE :: 0;
    SHARED_LIBRARY :: 1;
}
```

Specification of compiler output binary kind.



*File: build.bl*


## BuildMode

```c
BuildMode :: enum s32 {
    DEBUG :: 0;
    RELEASE_FAST :: 1;
    RELEASE_SMALL :: 2;
}
```

Specify target build mode. Every [Target](#target) can be compiled with various configuration 
options. The `BuildMode` can specify which set of options compiler should use.



### Variants
* `DEBUG` - Generates debug symbols and produce binary without any optimizations.
* `RELEASE_FAST` - Fast release mode; no debug symbols are produced, all possible optimizations
are applied to produce binary as fast as possible.
The `assert` is disabled unless `Target.assert_mode` is `ALWAYS_ENABLED`.

* `RELEASE_SMALL` - Small release mode; no debug symbols are produced, optimizations are applied
to produce binary reasonably fast and as small as possible.
The `assert` is disabled unless `Target.assert_mode` is `ALWAYS_ENABLED`.



*File: build.bl*


## DebugInfo

```c
DebugInfo :: enum s32 {
    DWARF :: 0;
    CODE_VIEW :: 1;
}
```

Debug information format.



*File: build.bl*


## AssertMode

```c
AssertMode :: enum s32 {
    DEFAULT :: 0;
    ALWAYS_ENABLED :: 1;
    ALWAYS_DISABLED :: 2;
}
```

Specification of `assert` mode used for `Target`.


### Variants
* `DEFAULT` - By default compiler emits  all assertions in [BuildMode](#buildmode). Debug and skips all 
assertions in all optimized release modes.

* `ALWAYS_ENABLED` - Force-enable assertion in all build modes.
* `ALWAYS_DISABLED` - Force-disable assertion in all build modes.


*File: build.bl*


## add_executable

```c
add_executable :: fn (name: string_view) *Target
```

Add new executable target into the current compilation queue. Target with specified name is 
compiled into binary or it can be just executed in compile-time without any output created. 
Assemblies are compiled after `build_entry` function execution in order they are added.

### Example
```
build :: fn () #build_entry {
    exe :: add_executable("MyProgram");
    add_unit(exe, "src/main.bl");

    mode :: get_build_mode(exe);
    switch mode {
        BuildMode.DEBUG {
            set_output_dir(exe, "build/debug");
        }

        BuildMode.RELEASE_SMALL,
        BuildMode.RELEASE_FAST {
            set_output_dir(exe, "build/release");
        }
    }
    compile(exe);
}
```




*File: build.bl*


## add_library

```c
add_library :: fn (name: string_view) *Target
```

Add new shared library target target into the current compilation queue.



*File: build.bl*


## add_unit

```c
add_unit :: fn (target: *Target, filepath: string_view) 
```

Add new source file into the `target`. Function does nothing when `filepath` is already present 
in the `target` assembly.




*File: build.bl*


## compile

```c
compile :: fn (target: *Target) Error
```

Start compilation of the `target` assembly and return `ok` or `error` in case compilation failed.



*File: build.bl*


## compile_all

```c
compile_all :: fn () Error
```

Compile all created targets one by one in order they were created. See also [compile](#compile).



*File: build.bl*


## add_lib_path

```c
add_lib_path :: fn (target: *Target, path: string_view) 
```

Add path for linker library lookup.



*File: build.bl*


## link_library

```c
link_library :: fn (target: *Target, name: string_view) 
```

Add system library. Only name is required (without extension and prefix). Compiler will lookup 
for this library in working directory, system `PATH` and `LINKER_LIB_PATH` variable specified 
in `bl.conf` file. Linked library can be used also during compile-time execution, in such case 
all needed symbols are loaded in compile-time.

In general there is no need to link libraries manually, all needed dependencies should be 
handled by module import mechanism, however there is still an option do it manually.

Library name platform specific rules:

* On Linux name will be extended by 'lib' prefix and '.so' extension.
* On MacOS name will be extended by 'lib' prefix and '.dylib' extension.
* On Windows name will be extended only by '.dll' extension.

### Example
```
build :: fn () #build_entry {
    exe :: add_executable("MyGame");
    add_unit(exe, "src/main.bl");

    switch PLATFORM {
        Platform.WINDOWS { target_windows(exe); }
        default          { panic("Unknown build target!"); }
    }
    compile(exe);
}

target_windows :: fn (exe: *Target) {
    link_library(exe, "freetype");
    link_library(exe, "zlib");
    link_library(exe, "png");
}
```




*File: build.bl*


## append_linker_options

```c
append_linker_options :: fn (target: *Target, option: string_view) 
```

Appends raw string data directly to linker command. Passed option is added without any 
processing and compatibility validation. 




*File: build.bl*


## set_output_dir

```c
set_output_dir :: fn (target: *Target, dir: string_view) 
```

Set build output directory. This is directory where all output files will be written. For 
example different output directory can be set for any build mode.

Directory path `dir`. This can contain non-existing directories separated by `/`, compiler will 
create all missing directories in passed path.

Specified directory will be used also for build temporary files.




*File: build.bl*


## get_output_dir

```c
get_output_dir :: fn (target: *Target) string_view
```

Get output directory specified by [set_output_dir](#set_output_dir) or empty string.



*File: build.bl*


## ModuleImportPolicy

```c
ModuleImportPolicy :: enum s32 {
    SYSTEM :: 0;
    BUNDLE :: 1;
    BUNDLE_LATEST :: 2;
}
```

Specify import module policy in [set_module_dir](#set_module_dir) function call. Module 
dependencies of any target can be treated in different ways depending on use case and needs of
programmer. Sometimes program stability and maintainability is more important than use of 
latest versions of modules. These flags can specify how local modules should be updated.



### Variants
* `SYSTEM` - Use system modules but prefer local ones in module directory. This option disables any 
copying and version check. Local module directory can contain custom project related 
modules you don't want to expose to the whole system.

* `BUNDLE` - Bundle only missing modules from system into the local module folder and ignore if there is 
newer version available. Individual module updates can be done by renaming old one and 
re-run compilation. Custom modules (not coming from main API folder are kept untouched.)

* `BUNDLE_LATEST` - Bundle all missing modules from system into the local module folder and update old ones 
also. Custom modules (not coming from main API folder are kept untouched.) Compiler will 
create `.bak` backup directory for every module before any updates.



*File: build.bl*


## set_module_dir

```c
set_module_dir :: fn (target: *Target, dir: string_view, policy :: ) 
```

Sets module directory `dir` for `target`. All imported modules will be copied into this 
directory according to chosen [ModuleImportPolicy](#moduleimportpolicy). Module policy is set 
to `SYSTEM` by default even if this function has not been called.




*File: build.bl*


## get_module_dir

```c
get_module_dir :: fn (target: *Target) string_view
```

Get module directory specified by [set_module_dir](#set_module_dir) or default one.



*File: build.bl*


## get_module_import_policy

```c
get_module_import_policy :: fn (target: *Target) ModuleImportPolicy
```

Get module import policy specified by [set_module_dir](#set_module_dir) or default one.



*File: build.bl*

