package scalapsihost

import com.intellij.openapi.extensions.ExtensionPoint
import com.intellij.openapi.util.registry.RegistryKeyDescriptor

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.zip.ZipFile
import javax.xml.parsers.DocumentBuilderFactory
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** An extension-point *declaration* (name → interface or bean class) read from descriptor XML. */
final case class EpDecl(name: String, className: String, kind: ExtensionPoint.Kind, dynamic: Boolean, fromScalaPlugin: Boolean)

/**
 * Reads plugin and platform descriptors as data only: extension-point declarations and registry-key defaults.
 * Nothing here loads a plugin, instantiates an extension or registers a service.
 */
object Descriptors {

  /**
   * Declarations from the Scala plugin's jars and from the other classpath jars. Classpath entries are told apart
   * from the plugin's by file name, because launchers may run from copies of the jars (sbt's `run` does).
   */
  def extensionPoints(classpathJars: Seq[Path], scalaPluginJars: Seq[Path]): Map[String, EpDecl] = {
    val pluginJarNames = scalaPluginJars.map(_.getFileName.toString).toSet
    val platformJars = classpathJars.filterNot(jar => pluginJarNames.contains(jar.getFileName.toString))
    (declarations(platformJars, fromScala = false) ++ declarations(scalaPluginJars, fromScala = true)).toMap
  }

  private def declarations(jars: Seq[Path], fromScala: Boolean): Seq[(String, EpDecl)] = {
    val factory = DocumentBuilderFactory.newInstance()
    val prefix = if (fromScala) "org.intellij.scala." else "com.intellij."
    for {
      jar <- jars
      (_, bytes) <- xmlEntries(jar) if new String(bytes, StandardCharsets.UTF_8).contains("<extensionPoint ")
      // Some jars ship XML that is not a descriptor (or not well-formed on its own); those declare no extension points.
      doc <- scala.util.Try(factory.newDocumentBuilder().parse(new java.io.ByteArrayInputStream(bytes))).toOption.toSeq
      nodes = doc.getElementsByTagName("extensionPoint")
      i <- 0 until nodes.getLength
      e = nodes.item(i).asInstanceOf[org.w3c.dom.Element]
      name = Option(e.getAttribute("qualifiedName")).filter(_.nonEmpty).getOrElse(prefix + e.getAttribute("name"))
      (cls, kind) = Option(e.getAttribute("interface")).filter(_.nonEmpty)
        .map(_ -> ExtensionPoint.Kind.INTERFACE).getOrElse(e.getAttribute("beanClass") -> ExtensionPoint.Kind.BEAN_CLASS)
      if cls.nonEmpty
    } yield name -> EpDecl(name, cls, kind, e.getAttribute("dynamic") == "true", fromScala)
  }

  /** `<registryKey>` defaults declared by the Scala plugin's descriptors. */
  def scalaRegistryKeys(loader: ClassLoader): Map[String, RegistryKeyDescriptor] = {
    val descriptors = Seq("META-INF/scala-plugin-common.xml", "META-INF/sbt.xml", "META-INF/codeInsight.xml", "META-INF/scala-meta-impl.xml")
    descriptors.flatMap { path =>
      val in = loader.getResourceAsStream(path)
      if (in == null) throw new IllegalStateException(s"Scala plugin descriptor not on the classpath: $path")
      val doc = Using.resource(in)(DocumentBuilderFactory.newInstance().newDocumentBuilder().parse)
      val nodes = doc.getElementsByTagName("registryKey")
      (0 until nodes.getLength).map(nodes.item(_).asInstanceOf[org.w3c.dom.Element]).map { e =>
        e.getAttribute("key") -> new RegistryKeyDescriptor(
          e.getAttribute("key"), e.getAttribute("description"), e.getAttribute("defaultValue"), false, false, "org.intellij.scala", null)
      }
    }.toMap
  }

  private def xmlEntries(jar: Path): Seq[(String, Array[Byte])] =
    Using.resource(new ZipFile(jar.toFile)) { zip =>
      zip.entries().asScala.filter(_.getName.endsWith(".xml")).map(e => e.getName -> zip.getInputStream(e).readAllBytes()).toVector
    }
}
