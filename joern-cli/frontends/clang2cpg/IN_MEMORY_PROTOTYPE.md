# Direct in-memory Clang-to-CPG prototype

The explicit `io.joern.clang2cpg.parser.memory.NativeBenchmark` entrypoint calls
Clang's C++ tooling inside the JVM through JNI. Clang returns structured Java
maps/lists, and the JVM emits CPG nodes and edges with the existing Clang mapping.
The ordinary Joern overlays run once after all selected translation units.

The path does not invoke a compiler subprocess, CDT, an AST dump, a JSON AST
reader, or evidence compression/archival. Existing semantic CPG tag values retain
their established string encoding so the Invari consumer contract stays stable.
Compilation databases and benchmark-result JSON are configuration/diagnostics,
not AST transport.

This is an explicit C prototype, not the default frontend or a complete parity
claim. The C syntax adapter covers loops, switch/goto, compound literals, GNU
statement expressions, generic selection, variadic arguments, and atomic builtins.
Unsupported syntax, unsupported semantic types,
invalid C, and missing dependencies fail visibly. There is no alternate parser.

## Build and focused tests

Use the pinned Linux amd64 builder/runtime from the Invari frontend lock, with
Clang/LLVM 20.1.8, JDK 21, and stock Joern 4.0.662. Run these commands from this
standalone module inside that builder. All paths below must be explicit.

```sh
mkdir -p target/native target/native-tests
clang++ -std=c++17 -O1 -fPIC -shared \
  -I/usr/lib/llvm20/include \
  -I/usr/lib/jvm/java-21-openjdk/include \
  -I/usr/lib/jvm/java-21-openjdk/include/linux \
  src/main/cpp/ClangJni.cpp \
  -L/usr/lib/llvm20/lib -Wl,-rpath,/usr/lib/llvm20/lib \
  -lclang-cpp -lLLVM-20 -o target/native/clang-jni.so
export JOERN_RUNTIME=/opt/joern
export CLANG_CPG_COMPILER=/usr/bin/clang-20
export CLANG_CPG_TEST_ROOT="$PWD/target/native-tests"
export CLANG_CPG_JNI_LIBRARY="$PWD/target/native/clang-jni.so"
export CLANG_CPG_JNI="$CLANG_CPG_JNI_LIBRARY"
sbt compile 'testOnly *NativeMemoryTests *NativeCpgTests *NativeParityTests' packageBin
```

The tests cover structured JNI values, compiler-owned declaration ranges and
resource-header paths, typed graph generation, invalid C, missing libraries,
valid unsupported syntax, and absence of AST/evidence files. The unchanged stock
c2cpg JAR is a **test-only** dependency for the independent stock comparator.
The JNI path does not load CDT classes.

## One CPG for five files

Prepare a compilation database containing exactly the five selected build
commands, retaining each command's real working directory and compiler flags.
Put the prototype package JAR first in the classpath:

```sh
java -Xmx6144m -XX:ActiveProcessorCount=4 \
  -cp "$PROTOTYPE_JAR:/opt/joern/frontends/c2cpg/lib/*" \
  io.joern.clang2cpg.parser.memory.NativeBenchmark \
  "$SOURCE_ROOT" "$FIVE_FILE_COMPILATION_DATABASE" "$NEW_OUTPUT_DIRECTORY" \
  "$CLANG_CPG_JNI_LIBRARY" repository
```

The output directory must not exist. The command saves `source.cpg.bin` plus a
small timing report. One JVM and native library handle all five files; there is
one final CPG and one overlay application. For a single selected source, pass its
path as SOURCE_ROOT and use `single` instead of `repository`.

For the stock comparator, compile/package the tests outside the timed region,
then run `io.joern.clang2cpg.parser.memory.StockBenchmark` against the **stock**
c2cpg JAR, with arguments SOURCE_ROOT, FIVE_FILE_COMPILATION_DATABASE, and a fresh
output directory. Give both processes the same CPU limit, heap, runtime, inputs,
and overlay settings; compare whole-process time as well as graph-stage time.
Do not include Scala compilation in either benchmark.

## Experiment acceptance

Before broader use, require:

- Protected query_ctx.c DSL and TLA hashes match for both single-source and
  shared-repository CPGs through the actual Invari init/persistent-worker path.
- All five translation units complete in the same graph build.
- Class-load diagnostics for the JNI process contain no CDT classes.
- No AST JSON, serialized AST, token dump, or evidence archive is produced.
- Unsupported behavior remains a visible error; partial graphs are not success.

The five-file experiment does not certify the historical 83-model inventory,
all 414 FalkorDB files, complete C/C++ support, or complete dataflow precision.

## Relationship to stock c2cpg

The frontend retains Joern's CPG schema, pass infrastructure, reference links,
operator vocabulary, and existing overlays. Clang-specific syntax capture feeds
one shared `ClangAstEmitter`; the legacy transport and JNI transport do not have
separate copies of the graph implementation. The native path never enters the
legacy JSON facade.

Stock graph projections are executable test oracles: compound literals use
`<operator>.cast` with a type-reference argument, switch statements carry the
`TRUE_BODY` edge, local declarations are not executable `FOR_INIT` expressions,
and atomic builtins retain ordinary call names. Additional compiler type and
provenance facts remain metadata. This does not establish complete feature
parity: designated initializer graph shape, additional GNU syntax, and the full
C++ frontend surface still require independent coverage.

A repository experiment must report completed translation units separately
from successful DSL/TLA lowering. An error after some translation units leaves
a diagnostic partial graph, never an accepted repository CPG. Compare timings
only when compilation flags, selected sources, worker counts, overlays, and
runtime limits match. Plain directory stock timings and release-database native
timings are not an apples-to-apples performance comparison.

## Repository memory behavior

Source selection includes every declaration in the selected main files. Imported
header declarations are emitted through the compiler's declaration/type/reference
closure, rather than eagerly copying every included prototype and record into
every translation unit. Canonical declarations, definitions, record fields,
function signatures, referenced globals, typedef chains, and unevaluated type
syntax participate in that closure. A missing required declaration or value type
fails capture. Assertion diagnostic text remains non-value syntax; its condition
still contributes dependencies.

Macro occurrences retain their complete compiler records and ordering. Native
translation-unit contracts use `macro_sites_storage = shared-tags-sha256-v1`
and an ordered `macro_site_refs` array. Each reference resolves to one shared
`clang.macroSite:<sha256>` tag containing the original record. Namespace TAGGED_BY
edges identify the records used by each translation unit. Repeated occurrences
remain repeated references; header records shared by several translation units
have one stored value. Missing references and digest collisions are errors.
The explicitly selected legacy facade retains its existing inline representation.

The retention probe on the same 12 FalkorDB release sources measured:

| Measurement | Before | After |
| --- | ---: | ---: |
| Live JVM objects after GC (class histogram) | 133.6 MB | 34.2 MB |
| Method nodes | 23,348 | 889 |
| Translation-unit metadata characters | 35,819,721 | 2,638,629 |

These are retained-heap diagnostics, not process peak RAM or normal benchmark
timings: the probe requests GC. Source call, control, identifier, local, and return
counts remained unchanged. Stock c2cpg retained 16.6 MB of live objects on that cohort; its graph
has less compiler metadata. Full CPG node-count equality is not claimed.

For the full repository experiment, reserve room for Clang outside Java's heap:

```sh
java -Xmx8192m -XX:ActiveProcessorCount=4 \
  -cp "$PROTOTYPE_JAR:/opt/joern/frontends/c2cpg/lib/*" \
  io.joern.clang2cpg.parser.memory.NativeBenchmark \
  "$SOURCE_ROOT" "$RELEASE_COMPILATION_DATABASE" "$NEW_OUTPUT_DIRECTORY" \
  "$CLANG_CPG_JNI_LIBRARY" repository
```

The harness persists per-translation-unit progress and heap usage before final
repository success. An incomplete graph is never reported as ready. In the
validated 414-file experiment, one graph including default overlays completed in
165.54 seconds with a 6.95 GiB peak process RSS inside a 16 GiB container.
`query_ctx.c` DSL and TLA matched their historical protected hashes twice when
lowered from that completed repository graph. This is 414-file CPG coverage;
it is not certification of 414 generated TLA models or full C++ feature parity.

The matched release-configured stock comparator (same 414 sources, CPU budget,
heap, runtime version, and default overlays) completed in 487.06 seconds with an
8.55 GiB peak RSS. The native process took 165.54 seconds and 6.95 GiB in that
single paired comparison. Stock emitted four existing CFG order-fallback warnings;
the native run emitted none. These timings do not represent the earlier plain
directory command without the release compilation database.
