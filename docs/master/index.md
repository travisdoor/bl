# Installation
## Windows
* Download and install latest [MS Build Tools](https://visualstudio.microsoft.com/visual-cpp-build-tools) or Visual Studio with C/C++ support.
* Download and unpack ``blc`` release zip package.
* Add ``path\to\your\blc\bin`` to the system ``PATH``.
* Run configuration.

```
blc --configure
```

## Linux (Ubuntu)
* Install Build Essential package if you don't have it. 
* **Compiler infrastructure and BL runtime depends on `glibc` and cannot be used with `musl` for now.**

```
apt-get update && apt-get install build-essential
```

* Download and unpack ``blc`` release zip package.
* Add ``path/to/your/blc/bin`` to the system ``PATH``.
* Run configuration.

```
blc --configure
```
  
## macOS
* Install Command Line Tools.
* **M1 support is experimental.**

```
xcode-select --install
```
    
* Download and unpack ``blc`` release zip package.
* Add ``path/to/your/blc/bin`` to the system ``PATH``.
* Run configuration.

```
blc --configure
```
   
# Installation from source code

## Requirements
* git
* CMake
* LLVM dev packages (only on Unix)
* gcc/clang/msbuild

## Windows
* Install Visual Studio 2019 with C/C++ support.
* Download and compile ``blc``, LLVM dev package and LLD linker binary will be downloaded as part of cmake project generation.

```
git clone https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake .. -G "Visual Studio 16 2019" -Thost=x64 -DCMAKE_BUILD_TYPE=Release
cmake --build . --config Release
```

* Add ``path/to/your/blc/bin`` to the system ``PATH``.
* Run first-use configuration with path to Visual Studio specified. (``bl-config.exe`` is located in ``bin`` directory after build)
  This will generate new ``etc\bl.conf`` file.

```
bl-config.exe --build-tools-path "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community"
```

## Linux (Ubuntu)
* Install Build Essential package if you don't have it ``apt-get update && apt-get install build-essential``.
* **Compiler infrastructure and BL runtime depends on `glibc` and cannot be used with `musl` for now.**
* Install LLVM dev package ``apt install llvm-11-dev``
* Download and compile ``bl``

```
git clone https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake ..
make
```
    
* Add ``path/to/your/blc/bin`` to the system ``PATH``.
* Run first-use configuration.

```
blc --configure
```

## macOS
* Install command line tools ``xcode-select --install``.
* Install LLVM package via your favourite package manager ``brew install llvm``.
* **M1 support is experimental.**
* Download and compile ``bl``

```
git clone https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake ..
make
```
    
* Add ``path/to/your/blc/bin`` to the system ``PATH``.
* Run first-use configuration.

```
blc --configure
```

**note**: ``blc`` use by default system linker ``ld`` because of some issues with ``lld`` on Mac. You can force use of ``LLD`` by ``bl-config --use-lld``.


## Build Utils
You can compile also ``Utils`` target to get some useful stuff. All utilities are located in ``utils`` folder. Compilation output
can be found in ``utils/bin`` directory.

```
cmake --build . --config Release --target Utils
```
