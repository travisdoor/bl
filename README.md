# About

The Biscuit Language (BL) is simple imperative programming language using LLVM backend implemented in C. 
Language syntax and all it's features are still in development and not ready for "real" use yet. Biscuit 
is designed to be simple, fast and explicit.

Project homepage: [biscuitlang.org](http://biscuitlang.org)  
Contact email: [biscuitlang@gmail.com](mailto:biscuitlang@gmail.com)

# Features & descriptions

* Simple small language.
* Manual memory management.
* ABI compatibility with C libraries.
* Opensource
* Game development oriented.
* Compilation to native binary.
* Integrated interpreter.
* Offer testing tools out of the box.
* Rich type info in runtime.
* Debuggable in gdb, lldb and Visual Studio.


# Example

```c
main :: fn () s32 {
    return fib(10);
}

fib :: fn (n: s32) s32 {
    if n == 0 || n == 1 {
        return n;
    } else {
        return fib(n-1) + fib(n-2);
    }
    
    return -1;
}
```

# Compilation 
## Requirements

* git
* CMake
* LLVM 10
* GCC/CLANG/Visual Studio compiler

    
## Windows
You will need Visual Studio 2019 installed on your machine. 

* Run shell as an administrator.
* Download and compile bl.

```bash
git clone https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake .. -G "Visual Studio 16 2019" -Thost=x64 -DCMAKE_BUILD_TYPE=Release
```

* Now you should be able to compile the bl target from the Visual Studio or from the terminal with following command.

```bash
cmake --build .
```

* Use this for installation into Program Files

```bash
cmake --build . --target Install
```

* Run bl.conf file generation

```bash
blc.exe -configure
```

## macOS
* Install command line tools.

```bash
xcode-select --install
```

* Install LLVM dev packages with your favourite package manager. Brew:

```bash
brew install llvm
```

* Download and compile bl.

```bash
git clone --recurse-submodules https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake ..
make
```

* For installation use:

```bash
[sudo] make install
```

* Run bl.conf file generation

```bash
[sudo] blc -configure
```


## Linux
* Install LLVM dev packages with your favourite package manager.

```bash
sudo apt install llvm-dev 
```

* Download and compile bl.

```bash
git clone --recurse-submodules https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake ..
make
```

* For installation use:

```bash
[sudo] make install
```

* Run bl.conf file generation

```bash
[sudo] blc -configure
```

