# Clang frontend compatibility

`clang2cpg` is packaged under the stock `io.joern.c2cpg` entry point. The
launcher and language selection remain unchanged. The parser backend is Clang;
`--parser clang` remains accepted, and requesting CDT fails explicitly.

## Command-line contract

The frontend exposes the stock c2cpg configuration surface:

| Options | Required behavior |
| --- | --- |
| `--include`, `--define` | Apply additional include directories and definitions to each translation unit. |
| `--compilation-database` | Select translation units and their build commands from compile_commands.json. |
| `--with-preprocessed-files` | Include `.i` files, preferring them over the corresponding unpreprocessed source. |
| `--with-include-auto-discovery`, `--no-include-auto-discovery` | Enable or disable reporting of the compiler’s actual configured include-search paths in `clang.includeSearch`. |
| `--include-comments` | Emit compiler-recorded comments. |
| `--log-problems` | Expose compiler diagnostics; retain diagnostics on failure. |
| `--log-preprocessor` | Log compiler-recorded preprocessing events. |
| `--print-ifdef-only` | Print conditional directives without creating a CPG. |
| `--skip-function-bodies` | Retain declarations while omitting source function bodies. |
| `--no-image-locations` | Deprecated stock compatibility option; warn that source locations remain enabled. |

The generic X2Cpg input/output, exclusion, file-content, schema-checking and help
options remain supplied by X2CpgMain. Include and definition options are
repeatable. The default for include auto-discovery matches stock c2cpg (off).

Clang-specific explicit inputs remain available: `--clang-command` (JSON argv),
`--clang-compiler`, `--clang-plugin`, and `--clang-evidence-dir`. Missing,
ambiguous or inconsistent compiler inputs must fail; a failed Clang invocation
must not switch to CDT or consume stale evidence.

## Validation boundary

CLI parsing alone does not establish frontend feature parity. The parser tests
cover the stock configuration flags, backend selection and rejection of the
removed completed-facts interface. Integration validation must additionally
exercise repository discovery, compilation databases, exclusions, all stock
C/C++ source and header extensions, preprocessed-source preference, comments,
preprocessor output, skipped bodies, generic file content and schema checking.

Repository graphs also need translation-unit-safe declaration identities,
header/static ownership and cross-file call/type links. C++ AST constructs,
preprocessor alternatives and malformed-source diagnostics require independent
coverage; a passing FalkorDB C model does not certify these capabilities.

The release gate is executable: run the affected frontend tests, then compare
all protected generated DSL/TLA bytes and record the complete 414-file census
with timings and explicit blockers. Do not infer a success count from an older
run or from the number of accepted CLI options.

## Compiler-native include search

`--with-include-auto-discovery` records Clang’s ordered quoted, angled and system
search entries, their directory/framework/header-map kind, compiler path, and
compiler working directory for interpreting relative paths.
It performs no external GCC probe and does not change the selected compile argv.
`--no-include-auto-discovery` disables this optional report. Clang’s intrinsic
header lookup remains governed by its compile command, including `-nostdinc`.
Search entries describe configured lookup paths; per-inclusion source-directory
lookup is handled by the compiler and is not an additional configured entry.
