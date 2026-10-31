package scalapsihost.platform

import com.intellij.openapi.vfs.{DeprecatedVirtualFileSystem, VirtualFile, VirtualFileWithId}

import java.io.{InputStream, OutputStream}
import java.net.URI
import java.nio.file.{FileSystem, FileSystems, Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/**
 * Library roots as read-only VirtualFiles: class roots (jars through the JDK's zip file system, JDK modules through
 * `jrt:`) and source roots (source jars, the JDK's `src.zip`), as the IDE attaches sources to libraries. Files have
 * parents and children, so the plugin's and the platform's library code can navigate from a class file to its
 * directory (inner classes, `.tasty` siblings) and to its sources.
 *
 * This is not the IDE's VFS: no persistence, no refresh, no events. Contents never change while mounted.
 * The instance owns the archive and alternate-JDK file systems it opens; the process host closes them with its root.
 */
final class LibraryFileSystem extends DeprecatedVirtualFileSystem with AutoCloseable {
  /** Root name (shown in paths) → root directory. Insertion order is classpath order. */
  private val roots = java.util.Collections.synchronizedMap(new java.util.LinkedHashMap[String, LibraryFile]())
  /** One VirtualFile per nio path, so PSI caches keyed by VirtualFile see stable identities. */
  private val files = new ConcurrentHashMap[Path, LibraryFile]()
  private val ids = new AtomicInteger()
  /** Zip and alternate JDK file systems opened by this instance; the current JDK's shared jrt is borrowed. */
  private val ownedFileSystems = scala.collection.mutable.LinkedHashSet.empty[FileSystem]
  private var closed = false

  override def getProtocol: String = LibraryFileSystem.Protocol

  /** Mounts a jar; returns its root directory. */
  def mountJar(jar: Path): VirtualFile = synchronized {
    ensureOpen()
    val zip = FileSystems.newFileSystem(jar)
    ownedFileSystems += zip
    mount(jar.toAbsolutePath.toString, zip.getPath("/"), sources = false)
  }

  /**
   * Mounts a source archive (a source jar, or a directory of one such as a JDK module's in `src.zip`) as a source
   * root; returns its root directory. Mounting an archive twice mounts it once.
   */
  def mountSources(archive: Path, directory: String = ""): VirtualFile = synchronized {
    ensureOpen()
    val name = if (directory.isEmpty) archive.toAbsolutePath.toString else s"${archive.toAbsolutePath}!/$directory"
    Option(roots.get(name)).getOrElse {
      val zip = sourceArchives.computeIfAbsent(archive.toAbsolutePath, a => {
        val opened = FileSystems.newFileSystem(a)
        ownedFileSystems += opened
        opened
      })
      mount(name, zip.getPath("/" + directory), sources = true)
    }
  }

  // Each archive's zip file system, opened once however many of its directories are mounted.
  private val sourceArchives = new ConcurrentHashMap[Path, FileSystem]()

  /** Mounts every module of the JDK at `javaHome` as a class root (as the IDE does); returns the module roots. */
  def mountJdk(javaHome: Path): Seq[VirtualFile] = synchronized {
    ensureOpen()
    val (jrt, owned) =
      if (Files.isSameFile(javaHome, Path.of(System.getProperty("java.home"))))
        (FileSystems.getFileSystem(URI.create("jrt:/")), false)
      else (FileSystems.newFileSystem(URI.create("jrt:/"), Map("java.home" -> javaHome.toString).asJava), true)
    if (owned) ownedFileSystems += jrt
    val modules = Using.resource(Files.list(jrt.getPath("/modules")))(_.iterator().asScala.toVector.sortBy(_.toString))
    modules.map(module => mount(s"jrt:$javaHome!/${module.getFileName}", module, sources = false))
  }

  /** Closes archives and alternate JDKs opened by this instance, leaving the JVM's shared jrt file system alone. */
  override def close(): Unit = synchronized {
    if (!closed) {
      closed = true
      var failure: Option[Throwable] = None
      ownedFileSystems.toVector.reverse.foreach { fileSystem =>
        try fileSystem.close()
        catch {
          case NonFatal(error) =>
            failure match {
              case Some(first) => first.addSuppressed(error)
              case None => failure = Some(error)
            }
        }
      }
      ownedFileSystems.clear()
      sourceArchives.clear()
      files.clear()
      roots.clear()
      byName = null
      failure.foreach(error => throw error)
    }
  }

  private def ensureOpen(): Unit =
    if (closed) throw new IllegalStateException("library file system is closed")

  /** Mounted class roots, in mount order; source archives are excluded. */
  def classRoots: Seq[VirtualFile] = roots.synchronized(roots.values().asScala.toVector.filterNot(_.isLibrarySource))

  /** Mounted source roots, including attached library sources and JDK source directories. */
  def sourceRoots: Seq[VirtualFile] = roots.synchronized(roots.values().asScala.toVector.filter(_.isLibrarySource))

  /**
   * File name → files, over every mounted root (classes and sources); rebuilt after a mount. What the IDE's
   * FilenameIndex holds for libraries.
   */
  @volatile private var byName: Map[String, Seq[VirtualFile]] = null

  /** All mounted class or source files named `name`; the filename index is rebuilt lazily after a new mount. */
  def filesNamed(name: String): Seq[VirtualFile] = {
    var index = byName
    if (index == null) {
      val all = Vector.newBuilder[VirtualFile]
      def visit(f: VirtualFile): Unit = { all += f; if (f.isDirectory) f.getChildren.foreach(visit) }
      roots.synchronized(roots.values().asScala.toVector).foreach(r => r.getChildren.foreach(visit))
      index = all.result().groupBy(_.getName)
      byName = index
    }
    index.getOrElse(name, Seq.empty)
  }

  /** Adds a stable root identity and invalidates the cached filename index. */
  private def mount(name: String, rootPath: Path, sources: Boolean): VirtualFile = {
    val root = new LibraryFile(this, ids.incrementAndGet(), rootPath, parent = null, rootName = name, sourcesRoot = sources)
    files.put(rootPath, root)
    roots.put(name, root)
    byName = null
    root
  }

  /** Reuses the virtual-file identity for a path already reached through another lookup. */
  private[scalapsihost] def child(parent: LibraryFile, path: Path): LibraryFile =
    files.computeIfAbsent(path, p => new LibraryFile(this, ids.incrementAndGet(), p, parent, rootName = null, sourcesRoot = false))

  /** Paths look like `<root name>!/<entry>`, as for the IDE's jar file system. */
  override def findFileByPath(path: String): VirtualFile = {
    val separator = path.indexOf("!/")
    val (rootName, entry) = if (separator < 0) (path, "") else (path.substring(0, separator), path.substring(separator + 2))
    roots.get(rootName) match {
      case null => null
      case root => if (entry.isEmpty) root else root.findFileByRelativePath(entry)
    }
  }

  override def refresh(asynchronous: Boolean): Unit = ()
  override def refreshAndFindFileByPath(path: String): VirtualFile = findFileByPath(path)
  override def isReadOnly: Boolean = true
}

/** Identifiers shared by the host-owned library virtual file system. */
object LibraryFileSystem {
  /** The protocol used to identify this host-owned virtual file system. */
  val Protocol = "scala-psi-host-library"
}

/**
 * A library file or directory. It carries an id (VirtualFileWithId) because the plugin decompiles only files of
 * an id-carrying file system; the id is unique within this process and never persisted.
 */
final class LibraryFile private[scalapsihost] (
  fs: LibraryFileSystem,
  id: Int,
  val nioPath: Path,
  parent: LibraryFile,
  rootName: String,
  sourcesRoot: Boolean,
) extends VirtualFile with VirtualFileWithId {
  override def getId: Int = id

  /** Under a source root (library sources), not a class root. */
  val isLibrarySource: Boolean = if (parent == null) sourcesRoot else parent.isLibrarySource

  /** The root this file is under. */
  def root: LibraryFile = if (parent == null) this else parent.root

  /** The path from the root, `/`-separated (empty for the root). */
  def entry: String = if (parent == null) "" else root.nioPath.relativize(nioPath).toString.stripSuffix("/")

  private lazy val directory = Files.isDirectory(nioPath)

  override def getName: String = if (parent == null) rootName else nioPath.getFileName.toString.stripSuffix("/")
  override def getFileSystem: LibraryFileSystem = fs
  override def getPath: String =
    if (parent == null) rootName
    else {
      val parentPath = parent.getPath
      if (parent.getParent == null) s"$parentPath!/$getName" else s"$parentPath/$getName"
    }
  override def isWritable: Boolean = false
  override def isDirectory: Boolean = directory
  override def isValid: Boolean = true
  override def getParent: VirtualFile = parent

  /**
   * Cached once because mounted library contents do not change, and package lookup repeatedly asks roots for children.
   */
  private lazy val childrenByName: Map[String, LibraryFile] =
    if (!directory) Map.empty
    else Using.resource(Files.list(nioPath)) { entries =>
      entries.iterator().asScala.map(p => p.getFileName.toString.stripSuffix("/") -> fs.child(this, p)).toMap
    }

  override def getChildren: Array[VirtualFile] =
    childrenByName.toVector.sortBy(_._1).map(_._2: VirtualFile).toArray

  override def findChild(name: String): VirtualFile = childrenByName.getOrElse(name, null)

  override def contentsToByteArray(): Array[Byte] = Files.readAllBytes(nioPath)
  override def getInputStream: InputStream = Files.newInputStream(nioPath)
  override def getOutputStream(requestor: AnyRef, newModificationStamp: Long, newTimeStamp: Long): OutputStream =
    throw new UnsupportedOperationException(s"library files are read-only: $getPath")
  // Library contents do not change while mounted, so a constant stamp is accurate.
  override def getTimeStamp: Long = 0L
  override def getModificationStamp: Long = 0L
  override def getLength: Long = if (directory) 0L else Files.size(nioPath)
  override def refresh(asynchronous: Boolean, recursive: Boolean, postRunnable: Runnable): Unit =
    if (postRunnable != null) postRunnable.run()
  override def toString: String = s"LibraryFile($getPath)"
}
