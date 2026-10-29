# Changelog

## Unreleased

- Go to definition and type definition into libraries: their attached sources (the build's source jars, the JDK's
  `src.zip`), or their decompiled class files.
- Go to type definition, as the IDE's "go to type declaration".
- Document highlights: a symbol's occurrences in the file, as reads or writes.
- Document symbols, from the plugin's structure view.
- Folding ranges, from the plugin's folding builder (with `//region` and `<editor-fold>` regions).
- Semantic tokens: identifiers and soft keywords colored as the IDE colors them.
- Build changes: the build is imported again after a change to its definition, or when source files are created or
  deleted.
- Observability: a log (sent to the editor and written to a file) of startup, import, slow requests and the
  platform's warnings and errors; progress while the project opens; an error message when it cannot; the server's
  state (`scalaPsiHost/state`) and a status report (`scalaPsiHost/status`). The VS Code extension shows the state in
  the status bar and has *Show Status* and *Show Log File* commands.
- Memory: the platform's reparse copies of edited files are no longer kept.
- Lifecycle: the server ends with its client (its input ending, or its process exiting when the server can see it)
  and leaves the build's sbt server running for its other clients.
- Formatting and completion's insert handlers no longer stop the other requests: they change a copy of the file
  under a read, as the IDE's intention preview does.

## 0.1.0-a1 — 2026-10-28

First public alpha: a language server for sbt (or any BSP) builds, Scala 2 and 3, with Java sources in mixed
projects, built on the Scala plugin for IntelliJ IDEA (intellij-scala `491569803c`, platform 263.7307.44).

- Diagnostics: the plugin's highlighting; for Scala 3 by default, the module's own compiler on the document's
  current text.
- Hover: the signature of the symbol, or of the declaration, under the cursor.
- Go to definition, for declarations in the project's sources.
- A minimal VS Code extension (`editors/vscode`), and a distribution with a launcher script (`sbt dist`).
