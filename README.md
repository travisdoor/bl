<div style="text-align:center"><img src="https://biscuitlang.org/versions/0.9.0/_static/header.png" /></div>

# About
![Windows](https://github.com/travisdoor/bl/workflows/Windows/badge.svg?branch=master)
![Linux](https://github.com/travisdoor/bl/workflows/Linux/badge.svg?branch=master)
![macOS](https://github.com/travisdoor/bl/workflows/macOS/badge.svg?branch=master)

[![Twitter URL](https://img.shields.io/twitter/url/https/twitter.com/MTravisDoor.svg?style=social&label=Follow%20%40MTravisDoor)](https://twitter.com/MTravisDoor)

The Biscuit Language (BL) is simple imperative programming language using LLVM backend implemented in C. 
Language syntax and all it's features are still in development. Biscuit is designed to be simple, fast and explicit.

Project homepage: [biscuitlang.org](https://biscuitlang.org) 
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
See installation guide [here](https://biscuitlang.org/#installation).

# Example
```rust
HelloWorld :: struct {
    hello: s32;
    world: s32;
};

main :: fn () s32 {
    info :: cast(*TypeInfoStruct) typeinfo(HelloWorld);

    loop i := 0; i < info.members.len; i += 1 {
        print("% ", info.members[i].name);
    }
    print("!!!\n");
    
    return 0;
}
```

See more examples [here](https://biscuitlang.org/#how-to/).
