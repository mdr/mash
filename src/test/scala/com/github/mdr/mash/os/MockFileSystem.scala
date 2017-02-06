package com.github.mdr.mash.os

import java.nio.file.{ Path, Paths }
import java.time.Instant
import java.util.NoSuchElementException

import com.github.mdr.mash.ns.os.FileTypeClass

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object MockFileSystem {

  def of(path: String): MockFileSystem = {
    val segments = path.split("/").toSeq.tail
    val (init, last) = (segments.init, segments.last)
    val root = segments.foldRight[MockFileObject](MockFileObject.File()) {
      case (name, child) ⇒ MockFileObject.Directory(name -> child)
    }.asInstanceOf[MockFileObject.Directory]
    new MockFileSystem(root)
  }

}

class MockFileSystem(
    root: MockFileObject.Directory = MockFileObject.Directory(),
    var pwd: Path = Paths.get("/")) extends FileSystem {

  private def fetch(path: Path): MockFileObject = {
    val absPath = pwd.resolve(path)
    val segments = absPath.iterator.asScala.toSeq

    val sb = new StringBuilder("")
    var current: MockFileObject = root
    for (segment ← segments.map(_.toString)) {
      current = current match {
        case d: MockFileObject.Directory ⇒ d.children.getOrElse(segment,
          throw new NoSuchElementException(s"Could not find file '$segment' at '$sb'"))
        case f: MockFileObject.File ⇒ throw new NoSuchElementException(s"Could not find directory '$segment' at '$sb'")
      }
      sb.append("/" + segment)
    }
    current
  }

  def getChildren(parentDir: Path, ignoreDotFiles: Boolean, recursive: Boolean): Seq[PathSummary] = {
    val parent =
      try fetch(parentDir)
      catch { case _: NoSuchElementException ⇒ return Seq() }
    val immediateChildren =
      fetch(parentDir) match {
        case d: MockFileObject.Directory ⇒ d.children.toSeq.map {
          case (name, child) ⇒
            asPathSummary(parentDir.resolve(name), child)
        }
        case f: MockFileObject.File ⇒ throw new IllegalArgumentException("Not a directory")
      }
    if (ignoreDotFiles)
      immediateChildren.filterNot(_.path.getFileName.toString.startsWith("."))
    else
      immediateChildren
  }

  private val filePermissions = Permissions(
    owner = PermissionsSection(canRead = true, canWrite = true),
    group = PermissionsSection(canRead = true),
    others = PermissionsSection(canRead = true))

  private val dirPermissions = Permissions(
    owner = PermissionsSection(canRead = true, canWrite = true, canExecute = true),
    group = PermissionsSection(canRead = true, canExecute = true),
    others = PermissionsSection(canRead = true, canExecute = true))

  def asPathSummary(path: Path, fileObject: MockFileObject): PathSummary = fileObject match {
    case d: MockFileObject.Directory ⇒
      PathSummary(
        path,
        fileType = FileTypeClass.Values.Dir,
        size = 4096,
        owner = "mash",
        group = "mash",
        permissions = dirPermissions,
        lastModified = Instant.now(),
        lastAccessed = Instant.now())
    case f: MockFileObject.File ⇒
      PathSummary(
        path,
        fileType = FileTypeClass.Values.File,
        size = f.size,
        owner = "mash",
        group = "mash",
        permissions = dirPermissions,
        lastModified = Instant.now(),
        lastAccessed = Instant.now())
  }

  override def getPathSummary(path: Path): PathSummary = asPathSummary(path, fetch(path))

  override def glob(pattern: String): Seq[PathSummary] =
    ???

  override def chdir(path: Path) {
    this.pwd = path
  }

  override def readLines(path: Path): Seq[String] = Seq()

  override def read(path: Path): String = ???

  override def exists(path: Path): Boolean = ???

  override def isDirectory(path: Path): Boolean = ???

  override def createDirectory(path: Path, createIntermediates: Boolean): Path = ???

}

sealed abstract class MockFileObject

object MockFileObject {

  case class File(size: Long = 200) extends MockFileObject
  object Directory {
    def apply(children: (String, MockFileObject)*): Directory = Directory(ListMap(children: _*))
  }
  case class Directory(children: ListMap[String, MockFileObject]) extends MockFileObject

}
