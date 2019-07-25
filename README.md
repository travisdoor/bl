<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=BRSWZ2U7A2TXG&source=url"><img src="https://img.shields.io/badge/Donate-PayPal-green.svg"></a>

# About

The Biscuit Language (BL) is simple imperative programming language using LLVM backend implemented in C. Language syntax and all it's features are still in development and not ready for "real" use yet. Biscuit is designed to be simple, fast and explicit.

Contact email: [biscuitlang@gmail.com](mailto:biscuitlang@gmail.com)


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


## Made with Biscuit

-   [Sky Shooter](https://github.com/travisdoor/skyshooter) - Simple SDL game.
-   [PoissonDisk](https://github.com/travisdoor/PoissonDisk) - Unity C# poisson disk generator tool.


## YouTube videos

1.  [Introduction](https://www.youtube.com/watch?v=4UNTkqYakgI&t=22s) - Introduction to the language.


# Building compiler from source code <a id="org81a19ea"></a>


## Requirements

-   git
-   CMake
-   LLVM
-   GCC/CLANG/Visual Studio compiler


## Linux

-   Install LLVM dev packages with your favourite package manager.
    
		sudo apt install llvm-dev

-   Download and compile bl.
    
        git clone https://github.com/travisdoor/bl.git
        cd bl
        mkdir build
        cd build
        cmake ..
        make

- For instalation use: 

		[sudo] make install


## MacOS

-   Install LLVM dev packages with your favourite package manager.
-   Download and compile bl.
    
        git clone https://github.com/travisdoor/bl.git
        cd bl
        mkdir build
        cd build
        cmake ..
        make

- For instalation use: 

		[sudo] make install


## Windows

You will need Visual Studio 2019 installed on your machine. Everything needs to be compiled with the same Visual Studio version.

- Compile and install LLVM tool set. [guide](https://llvm.org/docs/GettingStartedVS.html)
- Run 'cmd' as an administrator.
- Download and compile bl.

		git clone https://github.com/travisdoor/bl.git
		cd bl
		mkdir build
		cd build
		cmake .. -G "Visual Studio 16 2019" -Thost=x64

Now you should be able to compile the 'bl' target from the Visual Studio or from the terminal with following command.

		cmake --build . --config Release
     
Use this for installation into 'Program Files'

		cmake --build . --config Release --target Install

- Biscuit compiler on Windows use Visual Studio linker =link.exe=. 

## Configuration 
   Compiler config file `bl.conf` can be found in `etc` directory.

# Releases


## 0.4.1 pre-alpha

-   [source](https://github.com/travisdoor/bl)
-   [documentation](https://travisdoor.github.io/bl/documentation.html)
-   [MIR - documentation](https://travisdoor.github.io/bl/MIR.html)

