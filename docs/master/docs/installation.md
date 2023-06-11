# Installation

Prebuilt packages are currently available only for Windows, you can download it [here](https://github.com/travisdoor/bl/releases/download/0.10.0/blc-0.10.0-win64.zip).

Just unzip the downloaded package, the compiler executable `blc.exe` can be found in the `bin` folder, use following snippet to add the `bin` directory to the system PATH.

**In Powershell:**
```
[Environment]::SetEnvironmentVariable(
   "Path",
   [Environment]::GetEnvironmentVariable("Path", "User") + ";path\to\bl\bin",
   "User"
)
```

# Installation From Source Code

Download the source from [here](https://github.com/travisdoor/bl/archive/refs/tags/0.10.0.zip).

**Supported targets:**

* `x86_64-pc-windows-msvc`
* `x86_64-pc-linux-gnu`
* `x86_64-apple-darwin`
* `arm64-apple-darwin` (experimental)

## Windows

* Install [CMake](https://cmake.org)
* Install Visual Studio 2022 or [MS Build Tools](https://visualstudio.microsoft.com/visual-cpp-build-tools) with C/C++ support
* Download and compile

```bash
cd path\to\unzipped\directory
mkdir build
cd build
cmake .. -G "Visual Studio 17 2022" -DCMAKE_BUILD_TYPE=Release
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
apt-get install cmake build-essential llvm-16-dev
```  
* Download and compile

```bash
cd path/to/unzipped/directory
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
* Install other needed tools using [brew](https://brew.sh) `brew install cmake llvm@16`.
* Download and compile

```bash
cd path/to/unzipped/directory
mkdir build
cd build
cmake ..
make
```
    
* Add `bin` directory to the system `PATH`. 

```bash
export PATH=$PATH:/path/to/your/bl/bin
```

!!! warning 
    ARM support is experimental. 

