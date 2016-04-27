package com.github.mdr.mash.completions

import java.io.IOException
import java.nio.file.Paths
import com.github.mdr.mash.ns.os.FileTypeClass
import com.github.mdr.mash.os.FileSystem
import java.nio.file.Path

class FilePathCompleter(fileSystem: FileSystem) {

  def getCompletions(s: String, directoriesOnly: Boolean = false): Seq[String] = {
    val (searchPath, prefix) = getSearchPathAndPrefix(s)
    val regularChildren = getRegularChildren(searchPath, prefix, directoriesOnly)
    val specialDirs: Seq[String] = getSpecialDotDirs(searchPath, prefix)
    (specialDirs ++ regularChildren).sortBy(f ⇒ if (f endsWith "/") f.init else f)
  }

  private def getRegularChildren(searchPath: Path, prefix: String, directoriesOnly: Boolean): Seq[String] = {
    val ignoreDotFiles = !prefix.startsWith(".")
    val children =
      try
        fileSystem.getChildren(searchPath, ignoreDotFiles = ignoreDotFiles, recursive = false)
      catch {
        case _: IOException ⇒ Seq()
      }
    children
      .filter(_.fileType == FileTypeClass.Values.Dir || !directoriesOnly)
      .filter(_.path.getFileName.toString startsWith prefix)
      .map { f ⇒
        val suffix = if (f.fileType == FileTypeClass.Values.Dir) "/" else ""
        searchPath.resolve(f.path.getFileName).toString + suffix
      }
  }

  private def getSearchPathAndPrefix(s: String): (Path, String) = {
    val path = Paths.get(s)
    val searchPath =
      if (s endsWith "/")
        path
      else
        Option(path.getParent).getOrElse(Paths.get(""))
    val prefix =
      if (s endsWith "/")
        ""
      else
        path.getFileName.toString
    (searchPath, prefix)
  }

  /**
   * Get the '.' and '..' directories, if required
   */
  private def getSpecialDotDirs(searchPath: Path, prefix: String): Seq[String] = {
    val dotDirs = if (prefix == ".") Seq(".", "..") else if (prefix == "..") Seq("..") else Seq()
    dotDirs.map(dir ⇒ searchPath.resolve(dir).toString + "/")
  }

}