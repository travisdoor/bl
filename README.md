# ![alt text](docs/biscuit_logo.png "logo") The Biscuit Language
[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=BRSWZ2U7A2TXG&source=url)

# About

The Biscuit Language (BL) is simple imperative programming language using LLVM backend implemented in C. Language syntax and all it's features are still in development and not ready for "real" use yet. Biscuit is designed to be simple, fast and explicit.

Contact email: [biscuitlang@gmail.com](mailto:biscuitlang@gmail.com)


<a id="orgf16669c"></a>

## Goals

-   manual memory management
-   pointers
-   no exceptions
-   fast compilation
-   full compile-time execution (integrated interpreter)
-   no OOP
-   types as values in compile-time
-   use of the LLVM backend
-   multiplatform


<a id="org4be7318"></a>

## Example

    main :: fn () s32 {
      return fib(10);
    };
    
    fib :: fn (n: s32) s32 {
      if n == 0 || n == 1 {
        return n;
      } else {
        return fib(n-1) + fib(n-2);
      }
    
      return -1;
    };


<a id="org82de004"></a>

## Made with Biscuit

-   [Sky Shooter](https://github.com/travisdoor/skyshooter) - Simple SDL game.
-   [PoissonDisk](https://github.com/travisdoor/PoissonDisk) - Unity C# poisson disk generator tool.


<a id="org31ff431"></a>

# Building compiler from source code


<a id="org0d76eca"></a>

## Requirements

-   git
-   CMake
-   LLVM
-   [bobject](https://github.com/travisdoor/bobject)
-   [dyncall](http://www.dyncall.org)
-   GCC/CLANG/Visual Studio compiler


<a id="orgf480187"></a>

## Linux

-   Compile and install bobject.
    
        git clone https://github.com/travisdoor/bobject.git
        cd bobject
        mkdir build
        cd build
        cmake ..
        sudo make install

-   Compile and install dyncall.
-   Install LLVM dev packages with your favourite package manager.
    
        sudo apt install llvm-dev

-   Download and compile bl.
    
        git clone https://github.com/travisdoor/bl.git
        cd bl
        mkdir build
        cd build
        cmake ..
        make

-   Add 'bl/api' and 'bl/bin' into the system PATH


<a id="org0d8ff1c"></a>

## MacOS

-   Compile and install bobject.
    
        git clone https://github.com/travisdoor/bobject.git
        cd bobject
        mkdir build
        cd build
        cmake ..
        sudo make install

-   Compile and install dyncall.
-   Install LLVM dev packages with your favourite package manager.
-   Download and compile bl.
    
        git clone https://github.com/travisdoor/bl.git
        cd bl
        mkdir build
        cd build
        cmake ..
        make

-   Add 'bl/api' and 'bl/bin' into the system PATH


<a id="org769a048"></a>

## Windows

You will need Visual Studio 17 installed on your machine. Everything needs to be compiled with the same Visual Studio version.

-   Compile and install bobject.
    
        git clone https://github.com/travisdoor/bobject.git
        cd bobject
        mkdir build
        cd build
        cmake .. -G "Visual Studio 15 Win64"
        cmake --build . --config Release --target Install

-   Compile and install dyncall.
-   Compile and install LLVM tool set. [guide](https://llvm.org/docs/GettingStartedVS.html)
-   Download and compile bl.
    
        git clone https://github.com/travisdoor/bl.git
        cd bl
        mkdir build
        cd build
        cmake .. -G "Visual Studio 15 Win64"
    
    You can specify path to DynCall by setting `DYNCALL_PATH` variable. Solution generated from cmake configuration can be found in `build` folder. 
    
    There is unresolved issue with generated VS solution, use of `llvm_map_components_to_libnames` in cmake config file cause generation of some invalid linker input files, to fix that you need to open solution in Visual Studio and right click on 'bl' in Solution Explorer -> Properties -> Linker -> Input -> Additional Dependencies and remove \*-NOTFOUND entries. 
    
    Now you should be able to compile the `bl` target from the Visual Studio or from the terminal with following command.
    
        cmake --build . --config Release
    
    Built executable can be found in `bin/Release` directory. Blc depends on `bobject.dll`, please copy-paste this file from the `bobject/bin` directory next to the `blc.exe`.

-   Biscuit compiler on Windows use Visual Studio linker `link.exe`. Location of linker should be added into system PATH.
-   Add 'bl/api' and 'bl/bin/Release' into the system PATH

Note: Compilation on Windows is quite complicated, we need to create some automated process for this&#x2026;


<a id="org44feb1f"></a>

# Releases


<a id="orga4f9cb8"></a>

## 0.4.1 pre-alpha

-   [source](https://github.com/travisdoor/bl)
-   [documentation](documentation.html)
-   [MIR - documentation](MIR.html)

