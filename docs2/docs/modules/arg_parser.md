# Command Line Argument Parser

`#import "std/arg_parser"`

Generic command line argument parsing tool.

### Example

```
#import "std/arg_parser"

// Command line arguments context.
Args :: struct #base std.ArgParserCtx {
    my_flag: bool;
    my_name: string_view;
}

g_args: Args;

main :: fn () s32 {
    parser :: std.arg_parser_new();
    defer std.arg_parser_delete(parser);
    // After this all positional arguments stored inside the argument parser
    // context are invalid.
    defer std.arg_parser_ctx_terminate(&g_args);

    // Add new argument.
    std.arg_parser_add(
        parser, 
        "-f", // Short name (must start with -)
        "--flag", // Long name (must start with -)
        "Specify my flag.", // Help text.
        &fn (parser: std.ArgParser, args: []string_view, ctx: *std.ArgParserCtx) (s32, Error) {
            a: *Args = auto ctx;
            a.my_flag = true;
            return 1, ok(); // Return number of parsed arguments and state.
        });

    std.arg_parser_add(
        parser,
        "-n",
        "--name",
        "Specify my name.",
        &fn (parser: std.ArgParser, args: []string_view, ctx: *std.ArgParserCtx) (s32, Error) {
            a: *Args = auto ctx;
            if (args.len < 2) {
                return 0, error("Expected name!");
            }
            a.my_name = args[1];
            return 2, ok();
        });

    // Start parsing.
    state :: std.arg_parser_run(parser, command_line_arguments, &g_args, 1);
    if !is_ok(state) {
        print_err("%", state);
        std.arg_parser_print_help(parser);
        return 1;
    }

    print("%\n", g_args);
    return 0;
}
```

## std.ArgParserCtx

```c
ArgParserCtx :: struct {
    positional: [..]string_view;
    help: bool;
}
```

Arguments context base type.


### Members
* `positional` - Contains other positional arguments not considered as options (i.e. input files).
* `help` - True when help was invoked implicitly.


*File: arg_parser.bl*


## std.ArgParser

```c
ArgParser :: *u8
```

Argument parser opaque handle.



*File: arg_parser.bl*


## std.ArgParserHandlerFn

```c
ArgParserHandlerFn :: *fn (parser: ArgParser, args: []string_view, ctx: *ArgParserCtx) (_0: s32, _1: Error
)
```

Argument parser callback function type. In context of [add](#argparseradd) this callback 
function provides current `parser`, rest of the input argument slice starting from currently 
captured argument name and custom argument context `ctx`. Expected return of such function is 
count of parsed arguments from `args` input (at least one for successfully parsed argument with 
no additional input) and status.

See also: [add](#argparseradd)

**todo**: Is reference to parent parser needed here?




*File: arg_parser.bl*


## std.arg_parser_new

```c
arg_parser_new :: fn (help_text :: ) ArgParser
```

Create new instance of parser with possibility to set custom help text. Use 
[delete](#argparserdelete) to release parser instance.




*File: arg_parser.bl*


## std.arg_parser_delete

```c
arg_parser_delete :: fn (_parser: ArgParser) 
```

Delete parser instance.



*File: arg_parser.bl*


## std.arg_parser_ctx_terminate

```c
arg_parser_ctx_terminate :: fn (ctx: *ArgParserCtx) 
```

Release 'positional' arguments array inside the context data. 



*File: arg_parser.bl*


## std.arg_parser_run

```c
arg_parser_run :: fn (_parser: ArgParser, args: []string_view, ctx: *ArgParserCtx, start :: 0) Error
```

Perform parsing on input `args` slice and provide `ctx` as context to the argument parsing
callbacks. `start` specify first element of `args` slice considered to be an argument.
See also: (command_line_arguments)[#argparsercommand_line_arguments] 
(**note**: first element contains name of the executable)




*File: arg_parser.bl*


## std.arg_parser_add

```c
arg_parser_add :: fn (_parser: ArgParser, short: string_view, long: string_view, help: string_view, handler: ArgParserHandlerFn) 
```

Add new argument specified by `short` and `long` name to be recognised by parser. The argument 
description can be specified as `help`. `handler` function is called every time parser hits 
this argument (`short` or `long` name match). Argument name must be unique name starting with 
`-`. Help command (as `-h` or `--help`) is added by default.

### Example

```
std.arg_parser_add(parser,
    "-d", // short name.
    "--debug", // long name.
    "Enable debug mode.", // Help text.
    &fn (parser: std.ArgParser, args: []string_view, ctx: *std.ArgParserCtx) (s32, Error) {
        a: *MyArgs = auto ctx;
        a.is_debug = true;
        return 1, ok(); // One argument parsed ('-d' or '--debug') 
    });
```




*File: arg_parser.bl*


## std.arg_parser_print_help

```c
arg_parser_print_help :: fn (_parser: ArgParser) 
```

Print help for all registered commands.



*File: arg_parser.bl*

