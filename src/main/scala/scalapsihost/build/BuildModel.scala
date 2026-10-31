package scalapsihost.build

import com.intellij.lang.annotation.HighlightSeverity

import scalapsihost.{LineColumn, ModuleSpec}

import java.nio.file.Path

/** What compiling a file of a module needs, as the build describes it. */
final case class CompileSpec(
  scalaVersion: String,
  compilerJars: Seq[Path],
  classpath: Seq[Path],
  classDirectory: Path,
  scalacOptions: Seq[String],
)

/** A Scala build target, its project sources, and the settings needed to compile them. */
final case class ImportedModule(spec: ModuleSpec, sources: Seq[Path], compile: CompileSpec)

/** A problem the build's compiler reported for a source file. */
final case class CompilerDiagnostic(severity: HighlightSeverity, start: LineColumn, end: LineColumn, message: String)

/** The outcome of the build server's most recent compile request. */
final case class CompileResult(succeeded: Boolean, diagnostics: Map[Path, Seq[CompilerDiagnostic]])
