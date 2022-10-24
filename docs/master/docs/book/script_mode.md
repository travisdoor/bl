# Script mode

Programs written in BL can easily act like shell scripts on UNIX systems due to the support of [shebang](https://en.wikipedia.org/wiki/Shebang_(Unix)) character sequence specified at the first line of the entry file. The `-silent-run` option passed to the compiler reduces all compiler diagnostic output and executes the `main` function using the integrated interpreter. No output binary is produced in such a case. The following example can be directly executed in *bash*.

```rust
#!/usr/local/bin/blc -silent-run

main :: fn () s32 {
    print("Hello!!!\n");
    return 0;
}
```

```bash
$ chmod +x main.bl
$ ./main.bl
```

All command line arguments are forwarded into the executed script and can be accessed via `command_line_arguments` builtin variable. The first argument contains the script name every time.


