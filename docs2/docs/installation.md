# Installation

**Supported targets:**

* `x86_64-pc-windows-msvc`
* `x86_64-pc-linux-gnu`
* `x86_64-apple-darwin`
* `arm64-apple-darwin` (experimental)

## Windows

* Install [git](https://git-scm.com)
* Install [CMake](https://cmake.org)
* Install Visual Studio 2019 (2022) or [MS Build Tools](https://visualstudio.microsoft.com/visual-cpp-build-tools) with C/C++ support
* Download and compile

```bash
git clone https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake .. -G "Visual Studio 16 2019" -Thost=x64 -DCMAKE_BUILD_TYPE=Release
cmake --build . --config Release
```

* Add `bin` directory to the system `PATH`. 

**In Powershell:**
```
[Environment]::SetEnvironmentVariable(
   "Path",
   [Environment]::GetEnvironmentVariable("Path", "User") + ";path\to\bl\bin",
   "User"
)
```

## Linux (Ubuntu)
* Install needed tools
```bash
apt-get install git cmake build-essential llvm-11-dev
```  
* Download and compile

```bash
git clone https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake ..
make
```

* Add `bin` directory to the system `PATH`. 

```bash
export PATH=$PATH:/path/to/your/bl/bin
```

## macOS
* Install command line tools ``xcode-select --install``.
* Install other needed tools using [brew](https://brew.sh) `brew install git cmake llvm`.
* Download and compile

```bash
git clone https://github.com/travisdoor/bl.git
cd bl
mkdir build
cd build
cmake ..
make
```
    
* Add `bin` directory to the system `PATH`. 

```bash
export PATH=$PATH:/path/to/your/bl/bin
```

**note:** M1 support is experimental.

