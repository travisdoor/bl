# Compiler Usage

Biscuit language compiler is a standalone terminal application called *blc*.  It can be compiled from source code found on [GitHub](https://github.com/travisdoor/bl) repository or downloaded from the home page as a binary executable (since the compiler is still under development, the binary versions may be outdated, currently [compilation](/installation) from the source code is preferred). All three major operating systems (Windows, macOS and Linux) are supported, but current active development is done on Windows and it usually takes some time to port the latest changes to the other platforms. The compiler executable can be found in *bin* directory it's usually a good idea to add the executable location to the system *PATH* to be accessible from other locations.

There are several options that can be passed to the compiler.

## Compiler Options

```text
blc --help

Usage:
  blc [options] [source-files]

Alternative usage:
  blc [options] <-build> [build-arguments]
  blc [options] <-run> <source-file> [arguments] [forwarded-arguments]

Options:
  -build                   Invoke project build pipeline. All following arguments are forwarded into the build script and ignored by compiler itself. Use as '-build [arguments]'.
  -doc                     Generate documentation and exit.
  -opt=<debug|release-fast|release-small|release-with-debug-info>
                           Specify binary optimization mode (use 'debug' by default).
  -release                 Specify binary optimization mode to release. (same as '-opt=release-fast')
  -run                     Execute BL program using interpreter and exit. The compiler expects <source-file> after '-run' flag, the file name and all following command line arguments are passed into the executed program and ignored by compiler itself. Use as '-run <source-file> [arguments]'.
  -shared                  Compile shared library.
  -silent-run              Execute BL program using interpreter and exit. The compiler expects <source-file> after '-silent-run' flag, the file name and all following command line arguments are passed into the executed program and ignored by compiler itself. Use as '-silent-run <source-file> [arguments]'. This flag also suppress all compiler console outputs. Basically it combines '-run' and '--silent' into a single flag. This can be useful in case the compiler is called implicitly from UNIX shebang.
  --about                  Print compiler info and exit
  --assert=<default|on|off>
                           Set assert mode ('default' option sets assert 'on' in debug and 'off' in release mode).
  --ast-dump               Print AST.
  --configure              Generate configuration file and exit.
  --di=<dwarf|codeview>    Set debug info format.
  --doc-out-dir=<STRING>   Set documentation output directory. (Use 'out' in current working directory by default.)
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
  --override-config=<STRING>
                           Set custom path to the 'bl.yaml' configuration file.
  --reg-split=<off|on>     Enable/disable splitting of structures passed into the function by value into registers.
  --run-tests              Execute all unit tests in compile time.
  --silent                 Disable compiler console logging.
  --stats                  Print compilation statistics.
  --syntax-only            Check syntax and exit.
  --target-experimental    Enable experimental compilation targets.
  --target-host            Print current host target triple and exit.
  --target-supported       Print all supported targets and exit. (Cross compilation is not allowed yet!)
  --tests-minimal-output   Reduce compile-time tests (--run-tests) output (remove results section).
  --verbose                Enable verbose mode.
  --verify-llvm            Verify LLVM IR after generation.
  --version                Print compiler version and exit.
  --vmdbg-attach           Attach compile-time execution debugger.
  --vmdbg-break-on=<N>     Attach compile-time execution debugger and sets break point to the MIR instruction with <N> id.
  --where-is-api           Print path to API folder and exit.
  --work-dir=<STRING>      Set current working directory. Compiler use by default the current working directory to output all files.
```

## Configuration

Use `blc --config` to change compiler configuration; this command generates a new `etc/bl.yaml` configuration file containing all the needed information about your system. This configuration runs automatically in case the config file was not found.

## Execution Status

- After a regular compilation process `blc` return 0 on success or a numeric maximum error code on the fail.
- When `-run` flag is specified `blc` return status returned by executed `main` function on success or numeric maximum error code on fail (compilation error or compile time execution error).
- When `--run-tests` flag is specified `blc` returns a count of failed tests on success or a numeric maximum error code on a fail.


