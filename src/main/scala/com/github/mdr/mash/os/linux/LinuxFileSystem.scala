package com.github.mdr.mash.os.linux

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.nio.file.attribute._
import java.util.EnumSet
import java.util.stream.Collectors

import com.github.mdr.mash.Posix
import com.github.mdr.mash.evaluator.ExecutionContext
import com.github.mdr.mash.ns.os.FileTypeClass
import com.github.mdr.mash.os.{ FileSystem, PathSummary, Permissions, PermissionsSection }
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

object LinuxFileSystem extends FileSystem {

  override def getChildren(parentDir: Path, ignoreDotFiles: Boolean, recursive: Boolean): Seq[PathSummary] = {
    var files: Seq[Path] =
      if (recursive) {
        val foundPaths = ArrayBuffer[Path]()
        object Visitor extends SimpleFileVisitor[Path]() {

          override def preVisitDirectory(dir: Path, attributes: BasicFileAttributes): FileVisitResult = {
            ExecutionContext.checkInterrupted()
            if (dir != parentDir)
              foundPaths += dir
            if (ignoreDotFiles && dir.getFileName.toString.startsWith(".") && dir != parentDir)
              FileVisitResult.SKIP_SUBTREE
            else
              FileVisitResult.CONTINUE
          }

          override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
            ExecutionContext.checkInterrupted()
            foundPaths += file
            FileVisitResult.CONTINUE
          }

          override def visitFileFailed(file: Path, e: IOException): FileVisitResult = {
            ExecutionContext.checkInterrupted()
            FileVisitResult.CONTINUE
          }

        }
        Files.walkFileTree(parentDir, EnumSet.of(FileVisitOption.FOLLOW_LINKS), Integer.MAX_VALUE, Visitor)
        foundPaths
      } else
        Files.list(parentDir).collect(Collectors.toList()).asScala.sortBy(_.getFileName)
    if (ignoreDotFiles)
      files = files.filterNot(_.getFileName.toString startsWith ".")
    files.map(getPathSummary)
  }

  override def getPathSummary(path: Path): PathSummary = {
    ExecutionContext.checkInterrupted()
    val owner = Files.getOwner(path, LinkOption.NOFOLLOW_LINKS).getName
    val attrs = Files.getFileAttributeView(path, classOf[PosixFileAttributeView], LinkOption.NOFOLLOW_LINKS).readAttributes()
    val group = attrs.group.getName
    val lastModified = attrs.lastModifiedTime.toInstant
    val lastAccessed = attrs.lastAccessTime.toInstant
    val perms = attrs.permissions
    val fileType =
      if (attrs.isSymbolicLink) FileTypeClass.Values.Link
      else if (attrs.isRegularFile) FileTypeClass.Values.File
      else if (attrs.isDirectory) FileTypeClass.Values.Dir
      else if (attrs.isOther) FileTypeClass.Values.Other
      else throw new AssertionError(s"Unexpected attributes: $attrs")

    PathSummary(
      path = path,
      fileType = fileType,
      size = attrs.size,
      owner = owner,
      group = group,
      permissions = permissionsObject(perms.asScala.toSet),
      lastModified = lastModified,
      lastAccessed = lastAccessed)
  }

  private def permissionsObject(perms: Set[PosixFilePermission]): Permissions = {
    import PosixFilePermission._
    val ownerPerms = PermissionsSection(
      canRead = perms contains OWNER_READ,
      canWrite = perms contains OWNER_WRITE,
      canExecute = perms contains OWNER_EXECUTE)
    val groupPerms = PermissionsSection(
      canRead = perms contains GROUP_READ,
      canWrite = perms contains GROUP_WRITE,
      canExecute = perms contains GROUP_EXECUTE)
    val othersPerms = PermissionsSection(
      canRead = perms contains OTHERS_READ,
      canWrite = perms contains OTHERS_WRITE,
      canExecute = perms contains OTHERS_EXECUTE)
    Permissions(
      owner = ownerPerms,
      group = groupPerms,
      others = othersPerms)
  }

  override def glob(pattern: String): Seq[PathSummary] = {
    val startDir = GlobHelper.globStart(pattern)
    val recurse = pattern.contains("**")
    val matcher = FileSystems.getDefault.getPathMatcher("glob:" + pattern)
    val foundPaths = ArrayBuffer[Path]()
    val visitor = new SimpleFileVisitor[Path]() {

      override def preVisitDirectory(dir: Path, attributes: BasicFileAttributes): FileVisitResult = {
        if (matcher.matches(dir))
          foundPaths += dir
        FileVisitResult.CONTINUE
      }

      override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
        if (matcher.matches(file))
          foundPaths += file
        FileVisitResult.CONTINUE
      }

      override def visitFileFailed(file: Path, e: IOException): FileVisitResult =
        FileVisitResult.CONTINUE

    }
    Files.walkFileTree(startDir, EnumSet.of(FileVisitOption.FOLLOW_LINKS), if (recurse) Integer.MAX_VALUE else 1, visitor)
    foundPaths.map(getPathSummary)
  }

  override def pwd: Path = Paths.get(Posix.posix.getcwd)

  override def chdir(path: Path) {
    Posix.posix.chdir(path.toString)
    System.setProperty("user.dir", pwd.toString)
  }

  override def readLines(path: Path): Seq[String] = FileUtils.readLines(path.toFile, StandardCharsets.UTF_8).asScala

  override def read(path: Path): String = FileUtils.readFileToString(path.toFile, StandardCharsets.UTF_8)

  override def exists(path: Path): Boolean = Files.exists(path)

  override def isDirectory(path: Path): Boolean = Files.isDirectory(path)

  override def createDirectory(path: Path, createIntermediates: Boolean): Path =
    if (createIntermediates)
      Files.createDirectories(path)
    else
      Files.createDirectory(path)
}