# c2bl

Converter from C library headers to BL wrappers.

## Build from source code

This tool is currently available only for Windows. 

```bash
blc -release-fast -b
```

## Usage

```bash
Usage:
  c2bl [options] <input files>
Options:
  -h, --help              Print this help.
  -I, --include-directory Specify include directory.
  -sc, --syntax-check     Check syntax of output files. BL compiler is required in PATH.
  -tm, --translate-macros Translate C macros into BL, this feature is experimental.
  -o, --output            Specify name of the output file.
```

## Known issues
* External variables are not supported since those are not so common in C library APIs.
* Expansion of macros is very tricky in clang, only basic stringification is supported
  and can be enabled by `--translate-macros` flag.
