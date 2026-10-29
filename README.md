# scala-psi-host

> **Alpha.** Diagnostics, hover, go-to-definition, completion, signature help, inlay hints and formatting work;
> references, rename and code actions do not exist yet. See [ROADMAP.md](ROADMAP.md) and
> [Known limitations](#known-limitations).

A Scala language server built on the IntelliJ Scala plugin's PSI and type checker, running **without an IntelliJ
application container**. The plugin source is not modified: the host builds the plugin from intellij-scala and
answers, through fixed tables, what the plugin and the IntelliJ platform's PSI libraries ask of their environment.

It serves LSP diagnostics, hover, go to definition and type definition, document highlights, document symbols,
folding ranges, semantic tokens, completion, signature help, inlay hints and formatting for sbt (or any BSP) builds,
Scala 2 and 3, with Java sources in mixed projects.

Completion lists the suggestions of the plugin's completion contributors, ordered by its weighers; a suggestion's
insert handler (parentheses, imports, generated code) runs once the user has chosen it, as in the IDE: the item's
command asks the server, which applies the handler's edits (`workspace/applyEdit`) and moves the caret
(`window/showDocument`). Formatting is the IntelliJ formatter; a change runs on a copy of the file and comes back as
minimal edits.

Inlay hints come from the plugin's `ImplicitHintsPass`: inferred definition types, parameter names, implicit and
`using` arguments, conversions, method chains and type arguments. The plugin's settings and defaults apply (parameter
names and local/member variable types are off by default); implicit hints are enabled. Only the two range-hint
options are disabled, since their presentation requires the IDE's concrete editor.

Symbols and folding are the plugin's structure view and folding builder; highlights and type definition, its access
detector and type-declaration provider; semantic tokens, the colors its highlighting visitor and color-scheme
annotator give identifiers (the editor's grammar colors keywords, literals and comments).

## How it works

The host installs three fixed tables before any PSI exists, then only consumes the plugin's PSI:

- an `Application` object (`java.lang.reflect.Proxy`, `ApplicationEx`) with a fixed service table;
- a root extension area with extension-point *declarations* read from descriptor XML, plus explicit language
  extensions (parser definitions, view providers, AST factories, stub schema) and the plugin's semantic extensions;
- a `Project` object with a fixed service table.

Anything the plugin or platform asks for outside those tables throws, and every adapter call is recorded in
`CallLog`; a platform or plugin upgrade that changes the surface fails `AdapterSurfaceTest`. Plugin XML is read as
data only. Logged errors (`LOG.error`) are reported and recorded instead of thrown, as in the IDE.

What the host provides instead of the IDE:

- **Project model** from the build, over BSP: one module per Scala build target, with its Scala version (a Scala SDK
  library), compiler options (a compiler-settings profile), classpath jars and module dependencies, and a resolve
  scope per module.
- **Build changes**: the client watches the build's definition (`*.sbt`, `project/`) and created or deleted source
  files; on a change the server imports the build again (reloading its definition first when it changed) and
  applies the difference: new sources are opened, removed ones closed, and a module whose definition changed is
  added again with the modules that depend on it, its files reopened with the editor's text. Requests wait while
  the build reloads.
- **Files**: library jars and the JDK as read-only virtual files (`LibraryFileSystem`); project sources as physical
  files (`SourceFile`) whose text is the editor's. Library PSI is stub-based, as in the IDE (Scala 3 libraries from
  TASTy); source syntax trees can be dropped and rebuilt under memory pressure.
- **Library sources**: the build's source jars (BSP `dependencySources`; sbt downloads them on import) and the JDK's
  `src.zip`, attached to their libraries as in the IDE; source files are parsed when navigated to, never indexed. Go
  to definition into a library opens its source, found as the IDE finds it (the plugin's navigation elements), or its
  decompiled class file when it has no sources: either is written, read-only, under
  `$SCALA_PSI_HOST_STATE/library-files`, since editors open files, not jar entries.
- **Index**: the plugin's and the Java stub indexers over library and source stubs, in memory (`SymbolIndex`).
- **Editing**: document changes are committed synchronously in the host's write action; the platform reparses the
  changed range and the plugin's own PSI listener invalidates its caches.
- **Threads**: a fair read/write lock as the IDE's read and write actions (`HostLock`): queries read concurrently,
  an edit cancels them and writes alone. Work the platform schedules for the UI thread runs on a dispatch thread
  without UI, declared to the platform as its EDT.
- **Errors**: the plugin's highlighting (`ScalaAnnotator`), and for files whose errors come from the compiler
  (Scala 3 by default, as in the IDE) the compiler's: the module's own Scala 3 compiler on the document's current
  text (`DocumentCompiler`), or the build's compile on save.
- Usage statistics are turned off: building an event would write a device id into the user's Java preferences.

## Build

Needs JDK 25 for sbt itself (the plugin and platform 263.x are class file 69):

The host is built against one revision of intellij-scala, pinned in `build.sbt` (`scalaPluginRevision`, currently
`491569803c` on branch `idea263.x`); the build checks that `../intellij-scala` is at it.

```sh
# 1. The Scala plugin, from an intellij-scala checkout next to this repo (~10 min, ~6 GB RSS)
git clone https://github.com/JetBrains/intellij-scala ../intellij-scala
git -C ../intellij-scala checkout 491569803c6c3c2f93a0bda51490425fda237fc0
(cd ../intellij-scala && sbt --java-home <jdk25> packageArtifactZip)

# 2. The host, its tests and the distribution
sbt --java-home <jdk25> test dist
```

`SCALA_PLUGIN_LIB` points the build at another build of the plugin's jars (default
`../intellij-scala/target/plugin/Scala/lib`); its revision is then not checked.
Platform jars come from the IntelliJ snapshots repository at the version pinned in `build.sbt`. The build resolves
`ide-impl` only as input to `inlayPlatformJar`: it extracts the hints-filtering package, `TooltipGroup`, `HintRenderer`
and its companion into a small jar. The full `ide-impl` jar is kept off the host's compile/runtime classpath and out
of the distribution.

The BSP tests use `fixtures/bsp-project`, an sbt build; create its connection file once with `sbt bspConfig` in that
directory. sbt keeps a build server running per build directory after its clients leave (`sbt shutdown` there
stops it).

## Use

`sbt dist` writes the distribution to `target/dist`: `bin/scala-psi-host` and `lib/`. The script speaks LSP on stdin
and stdout; an editor starts it in a project that has a BSP connection file (`.bsp/`, e.g. from `sbt bspConfig`).

- JDK 25: `JAVA_HOME`, or `java` on the `PATH`.
- `SCALA_PSI_HOST_STATE`: the platform's state directory (default `$XDG_CACHE_HOME/scala-psi-host`).
- `SCALA_PSI_HOST_HEAP`: maximum heap (default `2g`; a 500-file Scala 3 project fits in 1.5 GB).
- `SCALA_PSI_HOST_LOG=debug`: log every request, with how long it ran and waited.

### Is it working?

- **Log**: what the server does — startup, the build server it starts (and where that server's own output goes),
  each module imported with its time, library sources, "ready" with the total time and heap, slow requests (over
  1 s), and the platform's and the plugin's warnings and errors. It is sent to the editor (`window/logMessage`: the
  VS Code extension's output channel) and written to `$SCALA_PSI_HOST_STATE/logs/scala-psi-host-<start>.log` (the
  latest ten are kept).
- **Progress**: opening and importing the project are reported as LSP work-done progress.
- **Failure**: when the project cannot be opened (no `.bsp/`, the build server fails), the editor shows an error
  with the reason and the log file.
- **State**: the notification `scalaPsiHost/state` (`starting`, `importing`, `ready`, `failed`, with a message) as
  it changes; the VS Code extension shows it in the status bar.
- **Status**: the request `scalaPsiHost/status` answers the state, the project (modules, source files), the heap,
  request latencies by kind (count, p50, p95, max), how many warnings and errors were logged and the latest ones,
  and adapter calls outside the host's tables. The extension's command *scala-psi-host: Show Status* prints it;
  *Show Log File* opens the log.

`dist/smoke-test.py <bin/scala-psi-host> <project> <file> <line> <column>` talks to a built distribution as an
editor would (initialize, open, diagnostics, hover, definition, shutdown).

## Layout

- `ScalaPsiHost` — bootstrap (the tables, service registrations, startup of synthetic classes) and the host's API:
  modules, libraries, source files, edits, reads, diagnostics.
- `ModuleModel` — modules, which module each source belongs to, and their resolve scopes.
- `LibraryFileSystem`, `SourceFile` — virtual files of libraries and of project sources.
- `SymbolIndex` — the stub index; `LibraryClassFinder` — classes and packages of the library roots.
- `BspImport` — the build connection: modules, sources, compile specs, compile with diagnostics.
- `DocumentCompiler` — a Scala 3 module's compiler on one document.
- `HostDocumentCommitProcessor` — the IDE's synchronous document commit.
- `Adapters` — `CallLog`, proxy and dispatch tables, `ServiceTable`, `HostLock`, the dispatch thread, the logger.
- `lsp/` — `ProjectSession` (an open workspace), `ScalaLanguageServer` (LSP), `Main` (stdio).
- `src/main/java` — host subclasses of abstract platform classes. `Host_*` files are generated:
  `java -cp <compile classpath> tools/Gen.java src/main/java/scalapsihost <fqcn>...`. `Host_StubIndex` also has
  two hand-written overrides (concrete `StubIndex` overloads that delegate to each other); keep them when
  regenerating it. The LSP services extend Java bases (`LspServices`): implemented in Scala directly, their
  interfaces' default methods would be registered twice by lsp4j.

## Tests

- `HostSemanticsTest`, `LibraryIndexTest`, `DiagnosticsTest` — parse, resolve, types, library index, highlighting.
- `ModuleTest` — per-module classpath, compiler options and dependencies.
- `EditingTest` — edits reparsed in place and seen by other files; Scala 3 language by module; queries cancelled by
  an edit.
- `BspImportTest` — the fixture build imported over BSP, compiled (with diagnostics), highlighted, and a Scala 3
  document compiled.
- `lsp.LanguageServerTest` — the server end to end through JSON-RPC (runs in a JVM of its own).
- `AdapterSurfaceTest` — no call leaves the tables, and the adapter entries equal the reviewed list in
  `src/test/resources/adapter-surface.txt` (on failure the actual list is in `target/host-state/`).

## Oracles

Tools in the test scope, each comparing the host with the plugin's own expectations:

- `oracle.TypeInferenceOracle <intellij-scala> <report.md>` — the plugin's type-inference test data (Scala 2.12, in
  unit-test mode as the fixture runs it). **395 of 400 agree.** The rest: `SCL11568` (SAM is enabled from module
  settings; the oracle's files have no module), `SCL6601` and `SCL7805` (the same type, rendered `X.::[A, B]`
  instead of `A :: B`), `SCL5475` and `SCL8178` (open).
- `oracle.LibraryHighlightingOracle <intellij-scala> <report.md>` — highlights the scala-library 2.13 sources and
  compares with `ScalaLibraryHighlightingTest_2_13`'s known errors. 469 of 546 files agree; most of the rest are
  references to private members of a source's own companion object, which resolve to the compiled companion.
- `oracle.ProjectHighlighting <project> <report.md> [--plugin-errors]` — highlights a whole BSP project and reports
  errors, timings, heap and a class histogram; a file whose highlighting logged errors (a failed annotator) is not
  counted as clean. On a 522-file Scala 3 project: no errors with compiler errors (the
  default), 81 with the plugin's type-aware checks (the same the IDE shows in its built-in mode). On korolev v1.5.0
  (Scala 2.13, 88 modules, 176 files, two versions of zio and cats-effect; its sbt 1.6.2 needs JDK 17) with
  `--plugin-errors`: the 12 errors of the plugin's `KorolevProjectHighlightingTest`, at the same ranges, and none other.

- `oracle.CompletionOracle <intellij-scala> <report.md>` — the plugin's completion tests: file fixtures, keyword
  completion, auto-import and the `completion3` test methods (parsed with the host's PSI; the resulting text after
  the chosen suggestion's handler). completion3: 118 agree, 11 disagree; keywords 70 of 70; Scala 3 fixtures are
  not replayed yet.
- `oracle.FormatterOracle <intellij-scala> <report.md>` — the plugin's file-based formatter tests: 187 of 187 (its
  inline Scala test methods are not replayed).
- `oracle.ParameterInfoOracle <intellij-scala> <report.md>` — the plugin's parameter info tests: 104 agree,
  3 disagree (library versions), 35 skip (Scala 3 fixtures).
- `oracle.InlayHintsOracle <intellij-scala> <report.md> [--only=<name>,...]` — literal cases from the plugin's
  definition-type, parameter-name, type-argument, implicit-argument and conversion hint tests, with their file wrapper
  and settings: **152 agree, 0 disagree, 9 skip**. Types: 25 agree; parameters: 13; type arguments: 62; implicit
  arguments: 38 (Scala 2 and 3); conversions: 14 (Scala 2 and 3). Eight source tests need Java setup, navigation or
  tooltip assertions; the additional range probe is skipped because range hints require `EditorImpl`.
- `oracle.SessionBenchmark <project> <report.md>` — latency and memory of a session as the language server runs it:
  requests at identifiers sampled over the project's files, then rounds of edits with heap samples.

Run them with `sbt "Test/runMain scalapsihost.oracle.<Name> ..."`.

## Measurements

`SessionBenchmark` on a 522-file Scala 3 project (2 GB heap, one thread, 2026-10-28):

| Request | p50 ms | p95 ms | p99 ms |
|---|---|---|---|
| hover | 0 | 28 | 83 |
| definition | 0 | 2 | 8 |
| document highlights | 1 | 66 | 267 |
| document symbols, folding ranges (per file) | 0 | 1 | 2 |
| semantic tokens (per file, first time) | 41 | 775 | 1432 |
| problems (per file, first time; plugin and Scala 3 compiler) | 261 | 1638 | 4248 |
| problems after an edit | 142 | 328 | 589 |
| semantic tokens after an edit | 29 | 64 | 71 |

Opening the project takes 18–24 s (BSP import, library sources, modules, parsing and indexing the sources); the heap
holds 650 MB after opening and 1.4 GB once every file was highlighted. Editing one file over and over, the heap
after a full GC grows by about 0.3 MB per edit (see [Known limitations](#known-limitations)).

## Known limitations

- Memory held per edit: about 0.3 MB stays reachable after each edit and the requests that follow it (a long day of
  typing adds a few hundred MB). Not yet located.
- Requests are answered one at a time, in order: while a file's problems or semantic tokens are computed (seconds
  for the slowest files) or the build reloads, other requests wait.

- Completion: the override contributor (member bodies come from the IDE's file templates) and the unresolved-name
  contributor (it reads the IDE daemon's highlights) do not run; handlers that start live templates insert their
  code without tab stops; a weigher failing on a suggestion whose PSI has no containing file leaves it out (cause
  open). Choosing a suggestion with a handler is two edits (undo takes two steps).
- Inlay hints: range hints are not shown (`RangeInlayHintsPass` builds drawn presentations over `EditorImpl`). Java
  parameter-name exclusions are unavailable: the real Java provider requires the absent `JavaBundle`, so Java calls
  can show names the IDE excludes. String labels have no tooltips, navigation, expansion controls or kind; method
  chains use inline labels without the IDE's column alignment. The plugin suppresses definition-type and method-chain
  hints in Scala 3 compiler-highlighting mode unless X-Ray mode is enabled, as in the IDE. The oracle uses unit-test
  mode with compiler highlighting disabled, and reports inherited Scala 2 cases once.
- Formatting uses the IntelliJ formatter with Scala's default code style: the project's code style and
  `.scalafmt.conf` are not read.
- Scala 2 has no document compilation (its compiler has no stable interface to call); Scala 2 errors come from the
  plugin, or from the build's compile when compiler errors are enabled for Scala 2.
- Java sources are parsed at the highest language level, not the build's.
- Java class files are read by path on demand, not indexed: Java short-name lookups (completion, auto-import) do
  not see them.
- The JDK is the one the server runs on.
- Depends on platform `@Internal` APIs (`Extensions.setRootArea`, `ExtensionsAreaImpl`), constructs a few platform
  and plugin classes with non-public constructors, as the IDE container does, and sets one private field of the
  platform's statistics holder (no event providers). Inlay hints also read the pass's private collected buffer and
  write `ImplicitHints._enabled`: its public setter requires the IDE's keymap API to rebind shortcuts.

## License

Apache License 2.0: see [LICENSE](LICENSE) and [NOTICE](NOTICE). scala-psi-host builds on the Scala plugin for
IntelliJ IDEA and the IntelliJ Platform, both Apache 2.0 by JetBrains. It is not affiliated with or endorsed by
JetBrains; IntelliJ is a trademark of JetBrains s.r.o.
