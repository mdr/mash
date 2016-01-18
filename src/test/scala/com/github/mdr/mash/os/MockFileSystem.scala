package com.github.mdr.mash.os

import java.nio.file.Path
import scala.collection.JavaConverters._
import java.time.Instant
import com.github.mdr.mash.ns.os.FileTypeClass
import scala.collection.immutable.ListMap

object MockFileSystem {

}

class MockFileSystem(root: MockFileObject.Directory, var pwd: Path) extends FileSystem {

  private def fetch(path: Path): MockFileObject = {
    val absPath = pwd.resolve(path)
    val segments = absPath.iterator.asScala.toSeq

    val sb = new StringBuilder("")
    var current: MockFileObject = root
    for (segment ← segments.map(_.toString)) {
      current = current match {
        case d: MockFileObject.Directory ⇒ d.children.get(segment).getOrElse(
          throw new NoSuchElementException(s"Could not find file '$segment' at '$sb'"))
        case f: MockFileObject.File ⇒ throw new NoSuchElementException(s"Could not find directory '$segment' at '$sb'")
      }
      sb.append("/" + segment)
    }
    current
  }

  def getChildren(parentDir: Path, ignoreDotFiles: Boolean, recursive: Boolean): Seq[PathSummary] =
    fetch(parentDir) match {
      case d: MockFileObject.Directory ⇒ d.children.toSeq.map {
        case (name, child) ⇒
          asPathSummary(parentDir.resolve(name), child)
      }
      case f: MockFileObject.File ⇒ throw new IllegalArgumentException("Not a directory")
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

  def readLines(path: Path): Seq[String] = Seq()

}

sealed abstract class MockFileObject

object MockFileObject {

  case class File(size: Long = 200) extends MockFileObject
  case class Directory(children: ListMap[String, MockFileObject]) extends MockFileObject

}
