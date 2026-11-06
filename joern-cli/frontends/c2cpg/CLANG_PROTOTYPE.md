# Explicit Clang frontend prototype

This path replaces CDT parsing **and** CDT-dependent AST creators for a selected
C translation unit. It does not convert Clang nodes to synthetic CDT objects.
The existing CDT frontend remains the default; a Clang failure never selects it.

```text
Explicit compiler argv + .c file
    -> one native Clang invocation with the matching exporter plugin
    -> JSON AST, canonical declaration/type evidence, macros, lexical tokens,
       loaded-source/header hashes
    -> ClangAstCreationPass
    -> standard CPG schema and Joern overlays
```

## Build the native exporter

The exporter must match the selected compiler's Clang/LLVM ABI. For the tested
Linux amd64 Clang 20.1.8 environment:

```sh
clang++ -std=c++17 -O1 -fPIC -shared \
  -I/usr/lib/llvm20/include \
  joern-cli/frontends/c2cpg/src/main/scala/io/joern/c2cpg/parser/clang/ClangExporter.cpp \
  -L/usr/lib/llvm20/lib -lclang-cpp -lLLVM-20 \
  -o /absolute/artifact/clang-cpg.so
```

Build the Scala frontend with `c2cpg/packageBin`. The native library is an explicit
external artifact; it is not downloaded, generated or substituted automatically.

## Select Clang

Create a JSON argv array containing the exact compiler executable, ordered build
flags and absolute source path, for example:

```json
["/usr/bin/clang-20", "-std=gnu11", "-I/absolute/project/include", "-DPROJECT_BUILD=1", "/absolute/project/source.c"]
```

Do not include output, plugin or AST-dump options; the parser owns those.
Includes and definitions belong in this argv, not the CDT frontend flags.

```sh
joern-parse /absolute/project/source.c --language C --output /new/source.cpg.bin \
  --frontend-args --parser clang \
  --clang-command /absolute/command.json \
  --clang-plugin /absolute/artifact/clang-cpg.so \
  --clang-evidence-dir /new/evidence
```

The prototype accepts one explicit `.c` file. Required configuration, unsupported
flags, compiler errors, missing native identities, stale source/header evidence
and unsupported executable AST shapes cause failure. Evidence directories cannot
be reused. Compiler diagnostics and intermediate evidence remain at the selected
path. A failed graph is not a successful partial result.

## Representation

- Canonical record/enum identities come from Clang declaration USRs, including
  anonymous types, rather than comparing source type spellings.
- Native types project pointers, arrays, functions, qualifiers and nominal types
  structurally. Typed graph nodes retain compiler source spellings, with canonical
  type evidence in tags and record TYPE links bound to declaration identities.
  Indirect calls use c2cpg's `pointerCall` convention: callable at argument -1,
  source arguments at indices 1 onwards.
- Compiler implicit conversions annotate their source operand instead of becoming
  written cast operations. Their conversion
  kinds, AST kinds and compiler literal values are retained as tags.
- Record fields, declarations, source methods, variables and REF links use native
  compiler identities. File scope comes directly from Clang; header locals are
  not imported as global state.
- Imported object and enumerator bindings carry stable compiler USRs; missing
  required USRs fail instead of exposing process-local AST pointers.
- Declaration-prefix macro provenance stays on typed declarations as metadata.
  It does not introduce a call or change the declaration's behavior.
- Branch and goto roles are explicit schema edges consumed by Joern overlays.
- Macro expansion records, token spelling/origins, the full compiler JSON AST,
  input closure and tool hashes remain in the evidence bundle. This is not yet
  an Invari evidence-adapter schema.

## Validation and limits

Tests require explicit matching tools; they do not silently skip missing tools:

```sh
CLANG_CPG_COMPILER=/absolute/clang \
CLANG_CPG_PLUGIN=/absolute/clang-cpg.so \
CLANG_CPG_TEST_ROOT=/existing/test-artifacts \
  sbt 'c2cpg/testOnly *ClangParserTests *ClangBackendTests'
```

Coverage includes field/index types, tag/typedef collisions, logical-not, enum
conditionals, implicit conversions, literal values, indirect calls, macros,
compiler-created typeof parameters, header redeclarations/local scope, branch and
goto roles, invalid compiler input, missing configuration, missing identities,
stale evidence and rejected unsupported syntax.

This is not a complete production replacement. Multi-TU orchestration, full C
syntax/initializer coverage and replacement of Invari's separate compiler-evidence
collectors remain separate work. Unsupported array fillers and inline
assembly are explicitly rejected. Header declarations are imported, while
executable bodies are emitted only for the selected source file. No all-414-file
coverage or byte-identical regenerated TLA is claimed by the frontend tests.

The integration has lowered pinned FalkorDB v4.22.0 `query_ctx.c` through the
Clang CPG and `invari init`, reproducing the protected TLA file byte for byte
(including macro comments). Intermediate source DSL bytes differ. The saved CDT
CPG still reproduces both protected DSL and TLA bytes with the same Invari parser.
These are single-file integration results, not all-414-file coverage.
