# Modules

A module in BL is a chunk of reusable multi-platform code bundled with a configuration file in a single module *root* directory. See the following example of the *thread* module.

```text
thread/
  module.yaml       - module config
  _thread.win32.bl  - windows implementation
  _thread.posix.bl  - posix implementation
  thread.bl         - interface
  thread.test.bl    - unit tests
```

Modules can be imported into other projects using the `#import` directive followed by the module root directory path. The configuration file *module.yaml* is mandatory for each module and must be located in the module *root* directory. In general, the configuration file tells the compiler which source files should be loaded, which libraries are needed and what is the module version. See the module configuration example.

```yaml
version: 20221021

x86_64-pc-windows-msvc:
  src: "_thread.win32.bl"

x86_64-pc-linux-gnu:
  src: "_thread.posix.bl"

x86_64-apple-darwin:
  src: "_thread.posix.bl"

arm64-apple-darwin:
  src: "_thread.posix.bl"
```

The first in the file is the `version` of the module (usually in the format YYYYMMDD) followed by the list of all compilation targets supported by the module.  In this example, we use a different implementation for Windows. You can get a list of all supported platforms using the `blc --target-supported` command.

To import the `thread` module use the *root* directory path:

```c
#import "path/to/module/thread"
```

See also [Module Import Policy](/modules/build/#moduleimportpolicy).

!!! note
    Modules will be redesigned in the future to support the full set of features required by the build system. The long-term plan is to have the module configuration written directly in BL.

## List of module config entries

The configuration file entries may be *global* or *platform-specific*, see the following sections.

### Global Options

The *global* options are applied to the module on all target platforms.

- `version: <N>` - Module version number used during import to distinguish various versions of the same module, see also `ModuleImportPolicy` for more information.

### Global or Platform-Specific Options

All the following options may be applied globally or just for a specific target platform.

- `src: "<FILE1[;FILE2;...]>"` - List of source file paths relative to the module *root* directory separated by **platform-specific** separator (`:` on Windows and `;` on Unix).
- `linker_opt: "<OPTIONS>"` - Additional runtime linker options.
- `linker_lib_path: "<DIR1;[DIR2;...]>"` - Additional linker lookup directories relative to the module *root* directory.
- `link: "<LIB1[;LIB2;...]>` - Libraries to link. Note that libraries listed here are dynamically loaded during compilation (may be executed in compile-time).

```yaml
# The version is required to be global.
version: 20221021

# Load this file on all platforms. 
src: "my_file_imported_everytime.bl"

# Section specific for Windows.
x86_64-pc-windows-msvc:
  src: "my_file_only_for_windows.bl"

# Section specific for Linux.
x86_64-pc-linux-gnu:
  src: "my_file_only_for_linux.bl"
  linker_opt: "-lc -lm" # link these only on linux
```


