<div style="text-align:center"><img src="http://biscuitlang.org/versions/0.9.0/_static/header.png" /></div>

# About
![Windows](https://github.com/travisdoor/bl/workflows/Windows/badge.svg?branch=master)
![Linux](https://github.com/travisdoor/bl/workflows/Linux/badge.svg?branch=master)
![macOS](https://github.com/travisdoor/bl/workflows/macOS/badge.svg?branch=master)

[![Twitter URL](https://img.shields.io/twitter/url/https/twitter.com/MTravisDoor.svg?style=social&label=Follow%20%40MTravisDoor)](https://twitter.com/MTravisDoor)

The Biscuit Language (BL) is simple imperative programming language using LLVM backend implemented in C. 
Language syntax and all it's features are still in development. Biscuit is designed to be simple, fast and explicit.

Project homepage: [biscuitlang.org](http://biscuitlang.org)  
Contact email: [biscuitlang@gmail.com](mailto:biscuitlang@gmail.com)

# Features & descriptions
* Simple small language.
* ABI compatibility with C libraries.
* Opensource.
* Game development oriented.
* Compilation to native binary.
* Integrated interpreter.
* Offer testing tools out of the box.
* Rich type info in runtime.
* Debuggable in gdb, lldb and Visual Studio.
* Explicit function overload.
* Integrated build system.
* Integrated module import system.
* Multiplatform (Windows, macOS, Linux)
* Fast compilation.
* Integrated tools for automatic documentation.

# Installation
See installation guide [here](http://biscuitlang.org/versions/1.0.0/index.html).

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

See more examples [here](http://biscuitlang.org/versions/1.0.0/examples.html).
