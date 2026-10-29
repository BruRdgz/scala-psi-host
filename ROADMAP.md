# Roadmap

Parity with the Scala plugin in IntelliJ IDEA is the goal. Nearly every editor feature of the plugin rests on one of
a few pieces of platform infrastructure, so the milestones are ordered by those pieces: each one, once the host
provides it, unlocks a group of features.

A feature is done when an oracle over the plugin's own test data (`scala/scala-impl/testdata` in intellij-scala)
agrees with the host, and the cases that do not agree are explained in the README, as for type inference today.

## Infrastructure

- **A. Read-only, one file.** Nothing new: walk the PSI, or capture what the plugin already produces.
- **B. Project-wide search.** A word index (`IdIndex`) over the sources and a host `PsiSearchHelper`, behind the
  plugin's `referencesSearch` and `definitionsScopedSearch` extensions.
- **C. Change engine.** A change runs on a non-physical copy of the file, as the platform's intention preview does;
  the difference with the original becomes a `WorkspaceEdit`. Needs `CodeStyleManager` and the formatter, which
  almost every modification of the plugin calls.
- **D. Caret-bound features.** An `Editor` object in the fixed tables (document, caret, selection, nothing more;
  `AdapterSurfaceTest` shows what the plugin uses of it), a completion context (`CompletionParameters`, the copy with
  a dummy identifier), and a parameter-info UI context that records what `updateUI` renders.
- **E. Inspections.** The plugin's `LocalInspectionTool`s run without UI against a fixed profile, the IDE's default.

## Done ahead of order (2026-10-29)

The change engine and the formatter (beta 1), and completion, signature help and the request-local `Editor`
(beta 2) exist; their gaps are in the README's known limitations. Next, from the review of that work:

- Inlay hints (parameter names, types, implicit and `using` arguments): the plugin's hints pass needs a platform
  module (hints filtering) that is not among the host's dependencies.
- Scala 3 fixtures in the completion and parameter info oracles; file templates (override completion); live
  templates as snippets.

## Alpha 2 — read-only features, and the gaps of alpha 1

- The memory held per edit (`SessionBenchmark --edit-file`, about 0.3 MB per edit): locate it and let it go.
- A real Scala 2 project as an acceptance test.

## Alpha 3 — project-wide search

- Word index over the sources, `PsiSearchHelper`.
- Find references, implementations, workspace symbols. Oracle: `findUsages`.
- Java class files in the short-name index (auto-import and completion need them).

## Beta 1 — changes

- `CodeStyleManager` and the formatter; formatting, range formatting. Oracle: `formatter`. Decide how scalafmt
  configurations are honoured (the plugin can delegate to scalafmt).
- The change engine.
- Code actions from the plugin's intentions and the annotators' quick fixes. Oracles: `actions`, `surroundWith`.
- Organize imports (oracle: `optimize`), auto-import (oracle: `autoImport`), rename (with B; oracle: `refactoring`).

## Beta 2 — completion and inspections

- The `Editor` object.
- Selection ranges: the plugin's word-selection handlers, which read the editor's settings and lexer.
- Completion (oracles: `completion`, `completion3`, `keywordCompletion`) and signature help (oracle:
  `parameterInfo`), with a latency budget (p95 under 150 ms on the acceptance projects).
- Inspections as diagnostics, with their quick fixes as code actions. Oracle: `inspections`.

## 0.1 — release

- Distribution and CI on tags.
- A platform upgrade process, with the adapter surface test as its gate.
- Third-party license listing for the binary distribution.
- Editor extensions published.

## Later

- A persistent index, for fast startup.
- Inlay hints: inferred types, implicit arguments and conversions.
- `.sbt` files (sbt's meta-build is a build target over BSP).
- Call and type hierarchy.
- Test discovery and code lenses over BSP.
- Refactorings with dialogs in the IDE: extract, inline, change signature.
- Worksheets; debugging (DAP).
