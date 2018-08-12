# ![alt text](doc/biscuit_logo.png "logo") The Biscuit Language

## About
The Biscuit is programming language inspired by C and Rust.

## Change log
* 0.3.0 - syntax changes
* 0.2.0 - pre alpha basic language with lot of bugs :)

## Platforms
* MacOS
* Linux
* Windows

## Compilation

### Requirements
* CMake
* LLVM
* [bobject](https://github.com/travisdoor/bobject)
* GCC/CLANG/Visual Studio compiler

### MacOS and Linux
1) Install bobject from [here](https://github.com/travisdoor/bobject).
2) Install LLVM with `brew install llvm`.
3) Download and compile bl.

```
git clone https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake ..
make
```

4) Add `bl/api` and `bl/bin` into the system PATH.

### Windows
1) Install bobject from [here](https://github.com/travisdoor/bobject).
2) Compile LLVM ([instructions](https://llvm.org/docs/GettingStartedVS.html)).
3) Download and compile bl.

```
git clone https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake .. -G "Visual Studio 15 Win64" -DLLVM_PATH:STRING="path/to/LLVM"
cmake --build . --config Release
```

4) Add `bl/api` and `bl/bin/Release` into the system PATH.

## Language basics
[here](https://github.com/travisdoor/bl/blob/master/doc/readme.md "here")

## Compiler params
```
-ast-dump     Print out ast.
-lex-dump     Print tokens.
-syntax-only  Check syntax only. 
-emit-llvm    Write bytecode to disk.
-run          Run during compilation.
-no-bin       No binary will be created.
```
