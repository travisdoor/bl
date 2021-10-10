#ifdef HD_GEN
    HD_GEN(HD_NONE, "", 0)
    HD_GEN(HD_LOAD, "load", 1 << 1)
    HD_GEN(HD_LINK, "link", 1 << 2)
    HD_GEN(HD_CALL_LOC, "call_location", 1 << 3)
    HD_GEN(HD_EXTERN, "extern", 1 << 4)
    HD_GEN(HD_COMPILER, "compiler", 1 << 5)
    HD_GEN(HD_PRIVATE, "private", 1 << 6)
    HD_GEN(HD_INLINE, "inline", 1 << 7)
    HD_GEN(HD_NO_INLINE, "noinline", 1 << 8)
    HD_GEN(HD_FILE, "file", 1 << 9)
    HD_GEN(HD_LINE, "line", 1 << 10)
    HD_GEN(HD_BASE, "base", 1 << 11)
    HD_GEN(HD_ENTRY, "entry", 1 << 12)
    HD_GEN(HD_BUILD_ENTRY, "build_entry", 1 << 13)
    HD_GEN(HD_STATIC_IF, "if", 1 << 14)
    HD_GEN(HD_TAGS, "tags", 1 << 15)
    HD_GEN(HD_NO_INIT, "noinit", 1 << 16)
    HD_GEN(HD_INTRINSIC, "intrinsic", 1 << 17)
    HD_GEN(HD_TEST_FN, "test", 1 << 18)
    HD_GEN(HD_IMPORT, "import", 1 << 19)
    HD_GEN(HD_EXPORT, "export", 1 << 20)
    HD_GEN(HD_SCOPE, "scope", 1 << 21)
    HD_GEN(HD_THREAD_LOCAL, "thread_local", 1 << 22)
    HD_GEN(HD_FLAGS, "flags", 1 << 23)
    HD_GEN(HD_ASSERT, "assert", 1 << 24)
    HD_GEN(HD_COMPTIME, "comptime", 1 << 25)
    HD_GEN(HD_ERROR, "error", 1 << 26)
    HD_GEN(HD_WARNING, "warning", 1 << 27)
#endif