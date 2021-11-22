# Compiler usage

Biscuit language compiler is standalone terminal application called blc.  It can be compiled from
source code found on GitHub repository or downloaded from home page as binary executable. All three
major operating systems (Windows, macOS and Linux) are supported, but current active development is
done on Windows and it usually takes some time to port latest changes to the other platforms.
Compiler executable can be found in bin directory it's usually good idea to add executable location
to system PATH to be accessible from other locations.

There are several options which can be passed to the compiler.

## Compiler options:

```text
blc --help
Usage:
  blc [options] [source-files]

Alternative usage:
  blc [options] <-build> [build-arguments]
  blc [options] <-run> <source-file> [arguments] [forwarded-arguments]

Options:
  -build                   Invoke project build pipeline. All following arguments are
                           forwarded into the build script and ignored by compiler itself.
                           Use as '-build [arguments]'.
  -doc                     Generate documentation and exit.
  -opt=<debug|release-fast|release-small>
                           Specify binary optimization mode (use 'debug' by default).
  -release                 Specify binary optimization mode to release. (same as
                           '-opt=release-fast')
  -run                     Execute BL program using interpreter and exit. The compiler
                           expects <source-file> after '-run' flag, the file name and
                           all following command line arguments are passed into the
                           executed program and ignored by compiler itself. Use as
                           '-run <source-file> [arguments]'.
  -shared                  Compile shared library.
  --about                  Print compiler info and exit
  --assert=<default|on|off>
                           Set assert mode ('default' option sets assert 'on' in debug
                           and 'off' in release mode).
  --ast-dump               Print AST.
  --configure              Generate configuration file and exit.
  --di=<dwarf|codeview>    Set debug info format.
  --doc-out-dir=<STRING>   Set documentation output directory. (Use 'out' in current
                           working directory by default.)
  --emit-asm               Write assembly to file.
  --emit-llvm              Write LLVM-IR to file.
  --emit-mir               Write MIR to file.
  --error-limit=<N>        Set maximum reported error count.
  --full-path              Report full file paths.
  --help                   Print usage information and exit.
  --lex-dump               Print tokens.
  --no-analyze             Disable analyze pass, only parse and exit.
  --no-api                 Don't load internal API.
  --no-bin                 Don't write binary to disk.
  --no-color               Disable colored output.
  --no-jobs                Enable single-thread mode.
  --no-llvm                Disable LLVM back-end.
  --no-usage-check         Disable checking of unused symbols.
  --no-warning             Ignore all warnings.
  --reg-split=<off|on>     Enable/disable splitting of structures passed into the
                           function by value into registers.
  --run-tests              Execute all unit tests in compile time.
  --silent                 Disable compiler console logging.
  --syntax-only            Check syntax and exit.
  --time-report            Print compilation time report.
  --verbose                Enable verbose mode.
  --verify-llvm            Verify LLVM IR after generation.
  --vmdbg-attach           Attach compile-time execution debugger.
  --vmdbg-break-on=<N>     Attach compile-time execution debugger and sets break
                           point to the MIR instruction with <N> id.
  --where-is-api           Return path to API folder and exit.
```

## Configuration

Use `bl-config` tool to change compiler configuration. This tool generates configuration file
`etc/bl.conf` containing all required information needed by compiler during compilation.

## Execution status

- After regular compilation process `blc` return 0 on success or numeric maximum error code on fail.
- When `-run` flag is specified `blc` return status returned by executed `main` function on success
or numeric maximum error code on fail (compilation error or compile time execution error).
- When `--run-tests` flag is specified `blc` return count of failed tests on success or numeric
maximum error code on fail.

