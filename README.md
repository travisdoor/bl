<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=BRSWZ2U7A2TXG&source=url"><img src="https://img.shields.io/badge/Donate-PayPal-green.svg"></a>


# Table of Contents

-   [About](#org7fd9e9a)
    -   [Goals](#org5b420a3)
    -   [Example](#orga80def3)
    -   [Made with Biscuit](#org3f1eec7)
    -   [YouTube videos](#org9cd8e97)
-   [Building compiler from source code ](#org9fc23e7)
    -   [Requirements](#orge834d13)
    -   [Linux](#org40fd2aa)
    -   [MacOS](#org553484e)
    -   [Windows](#org9e6eedd)
-   [Contribution](#org7db05af)
    -   [Download and compile](#orgc50941b)
    -   [Find your good first issue](#orgb2ff05c)
    -   [Pick your favourite editor and start hacking](#org2fe26c4)
    -   [Keep same style](#org54801e2)
-   [Releases](#orgf2f2348)
    -   [0.4.1 pre-alpha](#org17130b2)


<a id="org7fd9e9a"></a>

# About

The Biscuit Language (BL) is simple imperative programming language using LLVM backend implemented in C. Language syntax and all it's features are still in development and not ready for "real" use yet. Biscuit is designed to be simple, fast and explicit.

Contact email: [biscuitlang@gmail.com](mailto:biscuitlang@gmail.com)


<a id="org5b420a3"></a>

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


<a id="orga80def3"></a>

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


<a id="org3f1eec7"></a>

## Made with Biscuit

-   [Sky Shooter](https://github.com/travisdoor/skyshooter) - Simple SDL game.
-   [PoissonDisk](https://github.com/travisdoor/PoissonDisk) - Unity C# poisson disk generator tool.


<a id="org9cd8e97"></a>

## YouTube videos

1.  [Introduction](https://www.youtube.com/watch?v=4UNTkqYakgI&t=22s) - Introduction to the language.


<a id="org9fc23e7"></a>

# Building compiler from source code <a id="org81a19ea"></a>


<a id="orge834d13"></a>

## Requirements

-   git
-   CMake
-   LLVM
-   GCC/CLANG/Visual Studio compiler


<a id="org40fd2aa"></a>

## Linux

-   Install LLVM dev packages with your favourite package manager.
    
	```
        sudo apt install llvm-dev
	```

-   Download and compile bl.
    
	```
        git clone https://github.com/travisdoor/bl.git
        cd bl
        mkdir build
        cd build
        cmake ..
        make
	```

- For instalation use: 

	```
	[sudo] make install
	```


<a id="org553484e"></a>

## MacOS

-   Install LLVM dev packages with your favourite package manager.
-   Download and compile bl.
    
	```
        git clone https://github.com/travisdoor/bl.git
        cd bl
        mkdir build
        cd build
        cmake ..
        make
	```

- For instalation use: 

	```
	[sudo] make install
	```


<a id="org9e6eedd"></a>

## Windows

You will need Visual Studio 2019 installed on your machine. Everything needs to be compiled with the same Visual Studio version.

- Compile and install LLVM tool set. [guide](https://llvm.org/docs/GettingStartedVS.html)
- Run 'cmd' as an administrator.
- Download and compile bl.

	```
	git clone https://github.com/travisdoor/bl.git
	cd bl
	mkdir build
	cd build
	cmake .. -G "Visual Studio 16 2019" -Thost=x64
	```

Now you should be able to compile the 'bl' target from the Visual Studio or from the terminal with following command.

	```
	cmake --build . --config Release
	```
     
Use this for installation into 'Program Files'

	```
	cmake --build . --config Release --target Install
	```

- Biscuit compiler on Windows use Visual Studio linker =link.exe=. Location of linker should be added into system PATH.


<a id="org7db05af"></a>

# Contribution


<a id="orgc50941b"></a>

## Download and compile

Everything needed to do so is in section [Build compiler from source](#org81a19ea).


<a id="orgb2ff05c"></a>

## Find your good first issue

All issues and enhancements can be found [here](https://github.com/travisdoor/bl/issues), issues marked as `good first issue` are the best for your initial contribution to the compiler.


<a id="org2fe26c4"></a>

## Pick your favourite editor and start hacking

You can choose any IDE or text editor you want.


<a id="org54801e2"></a>

## Keep same style

Clang-format tool is used to keep consistent code style all across the source base, the configuration file `.clang-format` is located in the repositary root. Please keep in mind that any other styles will not be accepted.


<a id="orgf2f2348"></a>

# Releases


<a id="org17130b2"></a>

## 0.4.1 pre-alpha

-   [source](https://github.com/travisdoor/bl)
-   [documentation](documentation.html)
-   [MIR - documentation](MIR.html)

