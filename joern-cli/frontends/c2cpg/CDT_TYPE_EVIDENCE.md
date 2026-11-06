# Experimental C type evidence

Select this prototype explicitly with the C frontend option:

```sh
joern-parse /path/to/source-root --output /path/to/new.cpg.bin \
  --frontend-args --with-c-type-evidence \
  --include /path/to/include --define SELECTED_BUILD=1
```

Use the project's actual include paths and preprocessor definitions. Header
files must be within the selected input inventory when their declaration nodes
are required in the saved graph. Parsing a single `.c` file does not by itself
add included headers to Joern's separate header AST pass.

With the option selected, the C frontend:

- Gets field-access and array-index expression types from the existing CDT AST.
- Projects records, unions, and enums through resolved CDT bindings rather than
  using a typedef/tag spelling as identity.
- Uses declaration source location and macro-expansion position for nominal
  identities. Source spelling stays on declaration name/code properties.
- Keeps typedef, pointer and array projections consistent with those identities.
- Resolves CDT's lazy forward definitions before assigning keys; retains forward
  declaration nodes as aliases rather than letting them replace the definition.
- Rejects unresolved required type evidence with a source/build-context error.
  It does not retry through the legacy type producer or silently keep `ANY` for
  a failed required field/index expression.

The option is C-specific. Existing behavior remains selected by default, and
C++ type generation is unchanged. Graphs created with this option have different
nominal `TYPE_FULL_NAME` values; consumers must follow graph identities instead
of assuming those values are source identifiers. Regenerate a graph explicitly
when changing this configuration; no existing graph is rewritten in place.

This prototype does not claim that all CPG nodes are fully typed. Other operator
families and external/unsupported type representations retain their existing
contracts. It does not implement pointer memory semantics or change a TLA
abstraction policy. It does not remove Invari's compiler-evidence stage by itself.

Focused validation:

```sh
sbt --client --color=false --supershell=false --batch \
  'c2cpg/testOnly *CdtTypeEvidenceTests *FullNameUniquenessPassTests *CallTests *MacroHandlingTests'
```

`CdtTypeEvidenceTests` includes tag/typedef collisions, typedef chains and
shadowing, anonymous unions, arrays of records, macro-generated declarations,
forward declarations, anonymous enum arrays, default-mode compatibility and
explicit failure for an unresolved field expression.
