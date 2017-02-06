package com.github.mdr.mash.os

import java.time.Instant
import java.nio.file.Path
import java.io.IOException
import java.nio.charset.StandardCharsets

import org.apache.commons.io.FileUtils

case class PathSummary(
  path: Path,
  fileType: String,
  size: Long,
  owner: String,
  group: String,
  permissions: Permissions,
  lastModified: Instant,
  lastAccessed: Instant)

case class Permissions(owner: PermissionsSection, group: PermissionsSection, others: PermissionsSection)

case class PermissionsSection(canRead: Boolean = false, canWrite: Boolean = false, canExecute: Boolean = false)

trait FileSystem {

  def getPathSummary(path: Path): PathSummary

  @throws[IOException]
  def getChildren(parentDir: Path, ignoreDotFiles: Boolean, recursive: Boolean): Seq[PathSummary]

  def glob(pattern: String): Seq[PathSummary]

  def pwd: Path

  def chdir(path: Path)

  def readLines(path: Path): Seq[String]

  def read(path: Path): String

  def exists(path: Path): Boolean

  def isDirectory(path: Path): Boolean

  def createDirectory(path: Path, createIntermediates: Boolean): Path

}