# scala-psi-host

Runs the IntelliJ Scala plugin's PSI and type checker **without an IntelliJ application container** — the
semantic core of a future standalone Scala language server. The plugin source is not modified.

The host installs three fixed tables before any PSI exists, then only consumes the plugin's PSI:

- an `Application` object (`java.lang.reflect.Proxy`) with a fixed service table — no EDT, no write actions;
- a root extension area with extension-point *declarations* read from descriptor XML, plus a few explicit
  language extensions (parser definitions, view providers, AST factory, stub schema);
- a `Project` object with a fixed service table.

Anything the plugin or platform asks for outside those tables throws, and every adapter call is recorded in
`CallLog`. Plugin XML is read as data only: extension-point declarations and registry-key defaults.

## Build

Needs JDK 25 for sbt itself (the plugin and platform 263.x are class file 69):

```sh
# 1. The Scala plugin, from an intellij-scala checkout next to this repo (~10 min, ~6 GB RSS)
(cd ../intellij-scala && sbt --java-home <jdk25> packageArtifactZip)

# 2. The host and its tests
sbt --java-home <jdk25> test
```

`SCALA_PLUGIN_LIB` points the build at another plugin build (default `../intellij-scala/target/plugin/Scala/lib`).
Platform jars come from the IntelliJ snapshots repository at the version pinned in `build.sbt`.

## Layout

- `ScalaPsiHost` — bootstrap: the three tables, service registrations, startup of synthetic classes.
- `LibraryFileSystem` — library class roots (jars via zipfs, JDK modules via `jrt:`) as read-only VirtualFiles.
- `SymbolIndex` — the stub index: library class files (through the plugin's decompiler) and project source files
  → the plugin's stub builder and indexers → an in-memory map answering `StubIndex`, kept per file.
- `LibraryClassFinder` — classes and packages of the library roots for the platform's `JavaPsiFacadeImpl`,
  next to the plugin's own element finders.
- `Descriptors` — extension-point declarations and registry keys read from descriptor XML.
- `Adapters` — `CallLog`, proxy and dispatch tables, `ServiceTable`.
- `src/main/java` — host subclasses of abstract platform classes. `Host_*` files are generated:
  `java -cp <compile classpath> tools/Gen.java src/main/java/scalapsihost <fqcn>...`. `Host_StubIndex` also has
  two hand-written overrides (concrete `StubIndex` overloads that delegate to each other); keep them when
  regenerating it.

## Tests

- `HostSemanticsTest` — parse, local resolve and types for a small scenario. Expectations are the correct
  Scala answers; the ones the host cannot give yet are marked `.fail` and name the phase that fixes them.
- `LibraryIndexTest` — scala-library binaries indexed through the plugin's own indexers, found by its lookups.
- `AdapterSurfaceTest` — no call leaves the tables, and the set of adapter entries equals the reviewed list in
  `src/test/resources/adapter-surface.txt`. A platform or plugin upgrade that changes the surface fails here; on
  failure the actual list is written to `target/host-state/adapter-surface.actual.txt` for review.

## Type-inference oracle

`sbt --java-home <jdk25> "Test/runMain scalapsihost.oracle.TypeInferenceOracle <intellij-scala> <report.md>"` runs
the plugin's own type-inference test data (the generated test classes that use the fixture's default setup: Scala
2.12, scala-library only) through the host and compares with the expected types in those files. It runs the host
in unit-test mode, as the fixture does, unless given `--production-mode`; `--only=SCL1234,...` runs single tests.

At commit time: **395 of 400 agree** (about 2 minutes). The 5 that do not:

- `SCL11568` — SAM conversion is enabled from the module's compiler settings; the host has no module model (F2).
- `SCL6601`, `SCL7805` — same type, rendered `SCL6601.::[A, B]` instead of infix `A :: B`.
- `SCL5475`, `SCL8178` — open; not order- or mode-dependent, and resolution in isolation is right.

## Plugin extensions

The host loads the plugin's extensions only for the extension points listed in
`ScalaPsiHost.SemanticExtensionPoints`, reading the implementation classes from the plugin's descriptors. Today
that is `syntheticMemberInjector`: case-class and companion members, context bounds, enums, `derives` and the
members library macros generate. Every other plugin extension point is declared with no extensions.

## Known limitations

- No library sources are attached: navigation into libraries lands on decompiled code.
- One scope for everything, no module model, single-threaded use. Phases F2 and F3. Settings the plugin reads from
  the module (compiler options such as SAM) take their defaults.
- Indexed library PSI is AST-backed (the IDE's is stub-backed), so the whole scala-library takes more than 768 MB
  of heap; the tests run with 2 GB.
- Java class files are read by path on demand, not indexed: Java short-name lookups (completion, auto-import)
  do not see them yet.
- Depends on platform `@Internal` APIs (`Extensions.setRootArea`, `ExtensionsAreaImpl`) and constructs a few
  platform and plugin classes with non-public constructors, as the IDE container does.
