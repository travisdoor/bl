# Type Info

The full *runtime type introspection* is supported in BL. You can use `typeinfo(<TYPE>)` builtin function to get the runtime [information](/modules/a/#typekind) about the *TYPE*.  The `typeinfo` returns a pointer to the type metadata included in the final binary, so there is no additional unexpected overhead involved (except the binary might be slightly bigger). The type metadata information is added lazily into the binary only for required types. 

This feature may be handy for automatic serialization of any kind, the good example is the [print](/modules/print/#print) which gives you automatically pretty print of any passed value or type.

```rust
main :: fn () s32 {
    // yields pointer to TypeInfo constant structure
    info := typeinfo(s32);

    if info.kind == TypeKind.INT {
        // safe cast to *TypeInfoInt
        info_int := cast(*TypeInfoInt) info;

        print("bit_count = %\n", info_int.bit_count);

        if info_int.is_signed {
            print("signed\n");
        } else {
            print("unsigned\n");
        }
    }

    return 0;
};
```


