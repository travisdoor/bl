# Biscuit Object
## About
Biscuit object is OO system for pure C providing simple type class definition and single
inheritance with a virtual table implemented.

## Change log
- 2.1.0 - add iterator operation for array 
- 2.0.0 - release version with added cmake installation scripts and containers.
- 0.1.0 - initial version with unit testing

## Features
- Dynamic container classes.
- Class definition
- Single parent inheritance
- Automatic construction and destruction of an object
- Virtual table and method overriding
- Runtime virtual method linking
- New and delete
- Automatic parent destructor call
## Platforms
* MacOS
* Linux
* Windows

## Compilation

### Requirements
* git
* CMake
* GCC/CLANG/Visual Studio compiler

### MacOS and Linux
```
git clone https://github.com/travisdoor/bobject.git
cd bobject
mkdir build
cd build
cmake ..
make install
```

### Windows
```
git clone https://github.com/travisdoor/bobject.git
cd bobject
mkdir build
cd build
cmake .. -G "Visual Studio 15 Win64"
cmake --build . --config Release --target Install
```



