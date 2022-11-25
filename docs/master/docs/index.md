# Biscuit Language

The Biscuit Language (BL) is simple imperative programming language using LLVM backend implemented
in C. Language syntax and all it's features are still in development and not ready for 'real' use
yet. Biscuit is designed to be simple, fast and explicit.

- Simple small language.
- Manual memory management.
- ABI compatibility with C libraries.
- Game development oriented.
- Compilation to native binary.
- Integrated interpreter.
- Offer testing tools out of the box.
- Rich type info in runtime.
- Debugging in gdb, lldb and Visual Studio.

## Current master (0.10.0)

The latest unstable version of the compiler.

- [Documentation](book/introduction/)
- [Source Code](https://github.com/travisdoor/bl)

### Changelog
```text
Add lazy IR generation to reduce LLVM generated stuff.
Add thread local global storage support.
Add compilation time report.
Add polymorphic function.
Add temporary allocator.
Add missing operators and change deref operator to @.
Add evaluation of compile time known conditional breaks.
Add static assert.
Add static if.
Add experimental #comptime directive for the functions evaluated in compile time.
Add typeof builtin.
Add enumcount helper function.
Add hash table.
Add buffer allocator.
Add using statement for scopes and enums.
Add implicit conversion from string to slice []u8.
Documentation format changed to markdown.
Fix crash on unnamed function argument RTTI generation.
Fix crash when invlid type of default argument value is provided by user.
Fix invalid conversion of enum variants to Any.
Fix DI locations using Code View.
Fix generation of unused LLVM IR code paths.
Fix missing usage check for symbols in named scope.
Fix DWARF generation for local anonymous functions.
Fix DI Generation of global variables using CodeView.
Test cases are no longer reported as unused during compilation.
Update dyncall to version 1.2 (due to experimental support of M1 Macs).
Redesign allocator structure.
Change array API.
Change enum naming conventions to uppercase.
Redesign libc importing into modules.
Redesign of bl-config + using YAML as main config format.
Move file read/write into io module.
Tags are now simple unsigned numbers.
Fix evaluation of multi-return and struct comptime functions.
Fix conversion of constants into slice.
Change constant string representation from 'string' to 'string_view' aka []u8.
Change syntax of compound initializers.
Add naming of compound members for structs.
Fix 0 size reported from sizeof for incomplete types.
Fix implicit conversion of alignof to Any.
```
