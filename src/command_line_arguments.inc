#if defined(GEN_ARGS_DEFINITIONS)
typedef struct {
    const char *s;
    const char *l;
    const char *help;
} Arg;
#define _ARGS                                                                                      \
    _ARG(HELP, "", "-help", "Print usage information and exit.")                                   \
    _ARG(ABOUT, "", "-about", "Print compiler info.")                                              \
    _ARG(BUILD, "b", "-build", "Use BL build pipeline.")                                           \
    _ARG(RELEASE_FAST,                                                                             \
         "",                                                                                       \
         "-release-fast",                                                                          \
         "Enable release mode. (when not specified, compiler use debug mode by default)")          \
    _ARG(RELEASE_SMALL,                                                                            \
         "",                                                                                       \
         "-release-small",                                                                         \
         "Enable release mode. (when not specified, compiler use debug mode by default)")          \
    _ARG(RUN,                                                                                      \
         "r",                                                                                      \
         "-run",                                                                                   \
         "Execute 'main' method in compile time. This option change argument parser behaviour.")   \
    _ARG(RUN_SCRIPT,                                                                               \
         "rs",                                                                                     \
         "-run-script",                                                                            \
         "Run as script (same as '-r --silent --no-llvm'). This option change argument parser "    \
         "behaviour.")                                                                             \
    _ARG(RUN_TESTS, "rt", "-run-tests", "Execute all unit tests in compile time.")                 \
    _ARG(SILENT, "", "-silent", "Silent mode.")                                                    \
    _ARG(NO_BIN, "", "-no-bin", "Don't write binary to disk.")                                     \
    _ARG(NO_WARNING, "", "-no-warning", "Ignore all warnings.")                                    \
    _ARG(NO_API, "", "-no-api", "Don't load internal api.")                                        \
    _ARG(NO_LLVM, "", "-no-llvm", "Disable LLVM backend.")                                         \
    _ARG(NO_ANALYZE, "", "-no-analyze", "Disable analyze pass, only parse and exit.")              \
    _ARG(NO_COLOR, "", "-no-color", "Disable colored output.")                                     \
    _ARG(ASSERT_ON, "", "-assert=on", "Enable assert.")                                            \
    _ARG(ASSERT_OFF, "", "-assert=off", "Disable assert.")                                         \
    _ARG(SYNTAX_ONLY, "", "-syntax-only", "Check syntax and exit.")                                \
    _ARG(EMIT_LLVM, "", "-emit-llvm", "Write LLVM-IR to file.")                                    \
    _ARG(EMIT_ASM, "", "-emit-asm", "Write assembly to file.")                                     \
    _ARG(EMIT_MIR, "", "-emit-mir", "Write MIR to file.")                                          \
    _ARG(AST_DUMP, "", "-ast-dump", "Print AST.")                                                  \
    _ARG(LEX_DUMP, "", "-lex-dump", "Print output of lexer.")                                      \
    _ARG(VERBOSE, "", "-verbose", "Verbose mode.")                                                 \
    _ARG(CONFIGURE, "", "-configure", "Generate config file.")                                     \
    _ARG(REG_SPLIT_ON,                                                                             \
         "",                                                                                       \
         "-reg-split=on",                                                                          \
         "Enable splitting structures passed into the function by value into "                     \
         "registers.")                                                                             \
    _ARG(REG_SPLIT_OFF,                                                                            \
         "",                                                                                       \
         "-reg-split=off",                                                                         \
         "Disable splitting structures passed into the function by value into "                    \
         "registers.")                                                                             \
    _ARG(VERIFY_LLVM, "", "-verify-llvm", "Verify LLVM IR after generation.")                      \
    _ARG(DOCS, "", "-docs", "Generate documentation and exit.")                                    \
    _ARG(WHERE_IS_API, "", "-where-is-api", "Return path to API folder and exit.")                 \
    _ARG(NO_JOBS, "", "-no-jobs", "Compile on single thread.")                                     \
    _ARG(SHARED, "", "-shared", "Compile shared library.")                                         \
    _ARG(FULL_PATH, "", "-full-path", "Report full file paths.")                                   \
    _ARG(NO_USAGE_CHECK, "", "-no-usage-check", "Disable checking of unused symbols.")             \
    _ARG(TIME_REPORT, "", "-time-report", "Print compilation time report.")                        \
    _ARG(DI_DWARF, "", "-di-dwarf", "Set debug info to use DWARF format.")                         \
    _ARG(DI_CODEVIEW, "", "-di-codeview", "Set debug info to use Codeview format.")                \
    _ARG(VMDBG_ATTACH, "", "-vmdbg-attach", "Attach compile-time execution debugger.")

#define _ARG(k, s, l, h) ARG_##k,
typedef enum { _ARGS _ARG_COUNT } ArgKind;
#undef _ARG
#define _ARG(k, s, l, h) {s, l, h},
static const Arg ARGS[_ARG_COUNT] = {_ARGS};
#undef _ARG
#endif
