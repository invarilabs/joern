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
claim. Unsupported syntax (currently including loops), unsupported semantic types,
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
sbt compile 'testOnly *NativeMemoryTests *NativeCpgTests' packageBin
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
