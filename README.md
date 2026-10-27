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
- `LibraryIndex` — the symbol index: library binaries → the plugin's decompiler → PSI → the plugin's stub
  builder and indexers → an in-memory map answering `StubIndex`.
- `Descriptors` — extension-point declarations and registry keys read from descriptor XML.
- `Adapters` — `CallLog`, proxy and dispatch tables, `ServiceTable`.
- `src/main/java` — host subclasses of abstract platform classes. `Host_*` files are generated:
  `java -cp <compile classpath> tools/Gen.java src/main/java/scalapsihost <fqcn>...`

## Tests

- `HostSemanticsTest` — parse, local resolve and types for a small scenario. Expectations are the correct
  Scala answers; the ones the host cannot give yet are marked `.fail` and name the phase that fixes them.
- `LibraryIndexTest` — scala-library binaries indexed through the plugin's own indexers, found by its lookups.
- `AdapterSurfaceTest` — no call leaves the tables, and the set of adapter entries equals the reviewed list in
  `src/test/resources/adapter-surface.txt`. A platform or plugin upgrade that changes the surface fails here; on
  failure the actual list is written to `target/host-state/adapter-surface.actual.txt` for review.

## Known limitations

- Only the root `scala` package of scala-library is indexed, and there are no JDK symbols. Types that depend on
  them come out wrong rather than failing (`Box[Nothing]` instead of `Box[Int]`). Phase F1.
- `scala/Array.class` is not indexed until the host has a library directory model (see `LibraryIndex`).
- One scope for everything, no module model, single-threaded use. Phases F2 and F3.
- Depends on platform `@Internal` APIs (`Extensions.setRootArea`, `ExtensionsAreaImpl`) and constructs a few
  platform and plugin classes with non-public constructors, as the IDE container does.
