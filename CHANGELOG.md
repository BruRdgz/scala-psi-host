# Changelog

## Unreleased

- Go to type definition, as the IDE's "go to type declaration" (project sources).
- Document highlights: a symbol's occurrences in the file, as reads or writes.
- Document symbols, from the plugin's structure view.
- Folding ranges, from the plugin's folding builder (with `//region` and `<editor-fold>` regions).
- Semantic tokens: identifiers and soft keywords colored as the IDE colors them.

## 0.1.0-a1 — 2026-10-28

First public alpha: a language server for sbt (or any BSP) builds, Scala 2 and 3, with Java sources in mixed
projects, built on the Scala plugin for IntelliJ IDEA (intellij-scala `491569803c`, platform 263.7307.44).

- Diagnostics: the plugin's highlighting; for Scala 3 by default, the module's own compiler on the document's
  current text.
- Hover: the signature of the symbol, or of the declaration, under the cursor.
- Go to definition, for declarations in the project's sources.
- A minimal VS Code extension (`editors/vscode`), and a distribution with a launcher script (`sbt dist`).
