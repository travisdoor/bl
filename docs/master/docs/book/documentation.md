# Documentation

Integrated self-documentation tool can be used to generate [Markdown](https://en.wikipedia.org/wiki/Markdown) files from Biscuit source files automatically. Documentation text can be attached to a file by `//!` comment prefix or to declaration by `///` comment prefix.  Such comments will be recognized by the compiler and attached to the proper declaration or file compilation unit. Use `-doc` compiler flag followed by a list of files you want to generate documentation for. Documentation output will be written to the `out` directory into the current working directory if the `--doc-out-dir` is not specified.

Use marker `@Incomplete` in the documentation comment to mark it as incomplete. The compiler will warn you about symbols with incomplete documentation during generation.

** Documentation rules: **

- Only files listed in compiler input are used as generation input (no loaded or imported files are included).
- Documentation is generated from AST; only a parsing pass is performed.
- When the out directory already exists, the compiler will only append new files and override old ones in case of collision.
- Only global and non-private declarations can be documented.
- A declaration name and declaration itself are included automatically.
- A single `md` file is produced for every input source file.

Example of documented `print` function:

```rust
/// Write string to the standard output (stdout). Format string can include format specifiers `%`
/// which are replaced by corresponding argument value passed in `args`. Value-string conversion is
/// done automatically, we can pass values of any type as an arguments, even structures or arrays.
///
/// The `print` function accepts C-like escape sequences as `\n`, `\t`, `\r`, etc.
///
/// Pointers to `Error` are dereferenced automatically; so the `print` function can print out errors
/// directly.
///
/// Count of printed bytes is returned.
print :: fn (format: string, args: ...) s32 {
    buf: [PRINT_MAX_LENGTH]u8 #noinit;
    w := _print_impl(buf, format, args);
    __os_write(OS_STDOUT, buf.ptr, auto w);
    return w;
}
```

Execution of `blc -doc print.bl` will produce following output:

````md
## print

```
print :: fn (format: string, args: ...) s32
```

Write string to the standard output (stdout). Format string can include format specifiers `%`
which are replaced by corresponding argument value passed in `args`. Value-string conversion is
done automatically, we can pass values of any type as an arguments, even structures or arrays.

The `print` function accepts C-like escape sequences as `\n`, `\t`, `\r`, etc.

Pointers to `Error` are dereferenced automatically; so the `print` function can print out errors
directly.

Count of printed bytes is returned.
````



