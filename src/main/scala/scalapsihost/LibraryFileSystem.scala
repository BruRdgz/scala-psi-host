package scalapsihost

import com.intellij.openapi.vfs.{DeprecatedVirtualFileSystem, VirtualFile, VirtualFileWithId}

import java.io.{InputStream, OutputStream}
import java.net.URI
import java.nio.file.{FileSystem, FileSystems, Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * Library class roots as read-only VirtualFiles: jars through the JDK's zip file system, JDK modules through
 * `jrt:`. Files have parents and children, so the plugin's and the platform's library code can navigate from a
 * class file to its directory (inner classes, `.tasty` siblings, library source directories).
 *
 * This is not the IDE's VFS: no persistence, no refresh, no events. Contents never change while mounted.
 */
final class LibraryFileSystem extends DeprecatedVirtualFileSystem {
  /** Root name (shown in paths) → root directory. Insertion order is classpath order. */
  private val roots = java.util.Collections.synchronizedMap(new java.util.LinkedHashMap[String, LibraryFile]())
  /** One VirtualFile per nio path, so PSI caches keyed by VirtualFile see stable identities. */
  private val files = new ConcurrentHashMap[Path, LibraryFile]()
  private val ids = new AtomicInteger()

  override def getProtocol: String = LibraryFileSystem.Protocol

  /** Mounts a jar; returns its root directory. */
  def mountJar(jar: Path): VirtualFile = {
    val zip = FileSystems.newFileSystem(jar)
    mount(jar.toAbsolutePath.toString, zip.getPath("/"))
  }

  /** Mounts every module of the JDK at `javaHome` as a class root (as the IDE does); returns the module roots. */
  def mountJdk(javaHome: Path): Seq[VirtualFile] = {
    val jrt: FileSystem =
      if (Files.isSameFile(javaHome, Path.of(System.getProperty("java.home")))) FileSystems.getFileSystem(URI.create("jrt:/"))
      else FileSystems.newFileSystem(URI.create("jrt:/"), Map("java.home" -> javaHome.toString).asJava)
    val modules = Using.resource(Files.list(jrt.getPath("/modules")))(_.iterator().asScala.toVector.sortBy(_.toString))
    modules.map(module => mount(s"jrt:$javaHome!/${module.getFileName}", module))
  }

  def classRoots: Seq[VirtualFile] = roots.synchronized(roots.values().asScala.toVector)

  /** File name → files, over every mounted root; rebuilt after a mount. What the IDE's FilenameIndex holds for jars. */
  @volatile private var byName: Map[String, Seq[VirtualFile]] = null

  def filesNamed(name: String): Seq[VirtualFile] = {
    var index = byName
    if (index == null) {
      val all = Vector.newBuilder[VirtualFile]
      def visit(f: VirtualFile): Unit = { all += f; if (f.isDirectory) f.getChildren.foreach(visit) }
      classRoots.foreach(r => r.getChildren.foreach(visit))
      index = all.result().groupBy(_.getName)
      byName = index
    }
    index.getOrElse(name, Seq.empty)
  }

  private def mount(name: String, rootPath: Path): VirtualFile = {
    val root = new LibraryFile(this, ids.incrementAndGet(), rootPath, parent = null, rootName = name)
    files.put(rootPath, root)
    roots.put(name, root)
    byName = null
    root
  }

  private[scalapsihost] def child(parent: LibraryFile, path: Path): LibraryFile =
    files.computeIfAbsent(path, p => new LibraryFile(this, ids.incrementAndGet(), p, parent, rootName = null))

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

object LibraryFileSystem {
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
) extends VirtualFile with VirtualFileWithId {
  override def getId: Int = id

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

  override def getChildren: Array[VirtualFile] =
    if (!directory) VirtualFile.EMPTY_ARRAY
    else Using.resource(Files.list(nioPath)) { entries =>
      entries.iterator().asScala.toVector.sortBy(_.getFileName.toString).map(p => fs.child(this, p): VirtualFile).toArray
    }

  // Direct lookup: large packages (java.util, scala.collection) would make the default child scan quadratic.
  override def findChild(name: String): VirtualFile =
    if (!directory) null
    else {
      val p = nioPath.resolve(name)
      if (Files.exists(p)) fs.child(this, p) else null
    }

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
