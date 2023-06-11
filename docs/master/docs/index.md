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

## 0.10.0

- [Documentation](book/introduction/)
- [Source Code](https://github.com/travisdoor/bl/archive/refs/tags/0.10.0.zip)
- [Github Page](https://github.com/travisdoor/bl)

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
Add naming of compound members for structs.
Add release-with-debug-info target.
Add realloc_slice.
Add type alignment into the type info.
Add #obsolete directive.
Add silent-run compiler argument.
Add call-stack getter for Windows and improve assert and panic reports.
Add function name into code location.
Add glwindow and draw modules + some example projects.
Add #maybe_unused for function arguments.
Add mixed function signature.
Add Static Array and Bucket Array
Add target triple API to the build system pipeline settings.
Add semaphores into synchronization API.
Add API to get ID of current thread.
Add #enable_if function directive.
Fix crash on unnamed function argument RTTI generation.
Fix crash when invalid type of default argument value is provided by user.
Fix invalid conversion of enum variants to Any.
Fix DI locations using Code View.
Fix generation of unused LLVM IR code paths.
Fix missing usage check for symbols in named scope.
Fix DWARF generation for local anonymous functions.
Fix DI Generation of global variables using CodeView.
Fix evaluation of multi-return and struct comptime functions.
Fix conversion of constants into slice.
Fix 0 size reported from sizeof for incomplete types.
Fix implicit conversion of alignof to Any.
Fix array type decomposition.
Fix bug related to unresolved types waiting for self.
Fix invalid binary operation volatile type borrowing for explicitly not matching types.
Fix argparse issues in doctor.
Fix emitting of string literals produced by compile time functions.
Fix invalid resolving of named-scope nested symbols via explicit operator.
Fix resume of postponed stack based call interpretations in compile-time.
Fix missing completeness check for value types converted to generic vargs (vargs of Any).
Documentation format changed to markdown.
Test cases are no longer reported as unused during compilation.
Update dyncall to version 1.2 (due to experimental support of M1 Macs).
Redesign allocator structure.
Change array API.
Change enum naming conventions to uppercase.
Redesign libc importing into modules.
Redesign of bl-config + using YAML as main config format.
Move file read/write into io module.
Tags are now simple unsigned numbers.
Change constant string representation from 'string' to 'string_view' aka []u8.
Change syntax of compound initializers.
Redesign memory allocators.
Simplify error handling API.
Redesign hash table implementation.
Simplified memory allocation API.
Rename slice_init and slice_terminate to alloc_slice and free_slice.
Improve compile time execution restore after postpone.
Function arguments are now immutable.
Improved error handling in synchronization API.
Improved error messages reporting unknown symbols.
Allow bunch of binary operations for enum flags types.
Update to LLVM 16.
```
