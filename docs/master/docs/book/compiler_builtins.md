# Compiler Builtins

## Variables

List of builtin variables set by compiler:

- `IS_DEBUG` Is bool immutable variable set to true when assembly is running in debug mode.
- `IS_COMPTIME_RUN` Is bool immutable variable set to true when assembly is executed in compile time.
- `BLC_VER_MAJOR` Compiler major version number.
- `BLC_VER_MINOR` Compiler minor version number.
- `BLC_VER_PATCH` Compiler patch version number.

## Functions

### `sizeof`

```
sizeof(<expr>)
```

Returns size of any expression or type in bytes.

### `alignof`

```
alignof(<expr>) #comptime
```

Returns alignment of any expression or type.

### `typeinfo`

```
typeinfo(<expr>) #comptime
```

Returns pointer to type information structure allocated on a stack.

### `typeof`

```
typeof(<expr>) #comptime
```

Returns type of any expression.

### `compiler_error`

```
compiler_error(message: string_view) #comptime
```

Report error in compile-time.

### `compiler_warning`

```
compiler_warning(message: string_view) #comptime
```

Report warning in compile-time.
