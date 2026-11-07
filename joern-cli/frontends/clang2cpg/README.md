# clang2cpg: isolated Clang frontend with a stock c2cpg launcher interface

This directory is an independent sbt 2.0.11 / Scala 3.9.0 build. It is not
registered in Joern's root build and does not modify `c2cpg`, the console,
`joern-parse`, or the distribution launchers.

The implementation lives in `io.joern.clang2cpg`. The only compatibility class is
`io.joern.c2cpg.Main`, which delegates to our main. The output is named `c2cpg`
so it can replace the exact JAR expected by the pinned stock c2cpg launcher.
Compile against the pinned stock Joern library classpath, excluding its original
c2cpg JAR. No CDT classes are used by this module.

```sh
cd joern-cli/frontends/clang2cpg
export JOERN_RUNTIME=/opt/joern
sbt compile test packageBin
```

The native build is separate:

```sh
clang++ -std=c++17 -O1 -fPIC -shared \
  -I/usr/lib/llvm20/include src/main/cpp/ClangExporter.cpp \
  -L/usr/lib/llvm20/lib -lclang-cpp -lLLVM-20 -o clang-cpg.so
```

Use Invari's `tools/joern/build.py` for locked Docker builds, artifact checks,
packaging and graph-to-DSL/TLA performance validation. Scala tests require
`CLANG_CPG_COMPILER`, `CLANG_CPG_PLUGIN`, and a writable `CLANG_CPG_TEST_ROOT`.
There is no completed-facts producer or Python compiler-evidence dependency.

Injection leaves the stock launcher unchanged:

```sh
joern-parse file.c --language C --output source.cpg.bin \
  --frontend-args --parser clang --clang-command command.json \
  --clang-plugin /opt/joern-clang/clang-cpg.so \
  --clang-evidence-dir /output/native
```

Explicit `--parser clang` is required. Default/explicit CDT requests, unsupported
options, stale inputs and missing native evidence fail; this replacement never
routes back to CDT. Keep an unmodified stock image for CDT comparisons. Directory
mode is still unsupported: this refactor does not implement repository parsing.

Run layout isolation tests from the fork root:

```sh
python3 -m unittest discover -s joern-cli/frontends/clang2cpg/tests
```

The pinned upstream baseline for the untouched c2cpg tree is commit
`4cdc64e689c130c8c599ba7dd8705795f1a330c0` (the upstream snapshot already present on fork master).
