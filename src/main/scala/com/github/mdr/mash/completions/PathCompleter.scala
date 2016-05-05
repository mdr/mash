package com.github.mdr.mash.completions

import java.io.IOException
import java.nio.file.Path
import java.nio.file.Paths

import scala.PartialFunction.condOpt
import scala.util.Try

import com.github.mdr.mash.ns.os.FileTypeClass
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.os.PathSummary

case class PathCompletion(path: String, typeOpt: Option[CompletionType]) {

  def sortKey = if (path endsWith "/") path.init else path

}

/**
 * Provide completions within the file system
 */
class PathCompleter(fileSystem: FileSystem) {

  def getCompletions(searchString: String, directoriesOnly: Boolean = false): Seq[PathCompletion] = {
    val (searchPath, prefix) = getSearchPathAndPrefix(searchString)
    val childPaths = getMatchingChildren(searchPath, prefix, directoriesOnly)
    val specialPaths = getSpecialDotDirs(searchPath, prefix)
    (specialPaths ++ childPaths).sortBy(_.sortKey)
  }

  private def getChildren(searchPath: Path, ignoreDotFiles: Boolean): Seq[PathSummary] =
    Try {
      fileSystem.getChildren(searchPath, ignoreDotFiles = ignoreDotFiles, recursive = false)
    }.getOrElse(Seq())

  /**
   * Get children in the given search path matching the given prefix
   */
  private def getMatchingChildren(searchPath: Path, prefix: String, directoriesOnly: Boolean): Seq[PathCompletion] = {
    val ignoreDotFiles = !prefix.startsWith(".")
    getChildren(searchPath, ignoreDotFiles)
      .filter(_.fileType == FileTypeClass.Values.Dir || !directoriesOnly)
      .filter(_.path.getFileName.toString startsWith prefix)
      .map(makeCompletion(searchPath))
  }

  private def makeCompletion(searchPath: Path)(pathSummary: PathSummary): PathCompletion = {
    val isDirectory = pathSummary.fileType == FileTypeClass.Values.Dir
    val suffix = if (isDirectory) "/" else ""
    val path = searchPath.resolve(pathSummary.path.getFileName).toString + suffix
    val typeOpt = condOpt(pathSummary.fileType) {
      case FileTypeClass.Values.Dir  ⇒ CompletionType.Directory
      case FileTypeClass.Values.File ⇒ CompletionType.File
    }
    PathCompletion(path, typeOpt)
  }

  private def getSearchPathAndPrefix(searchString: String): (Path, String) = {
    val path = Paths.get(searchString)
    val searchPath =
      if (searchString endsWith "/")
        path
      else
        Option(path.getParent).getOrElse(Paths.get(""))
    val prefix =
      if (searchString endsWith "/")
        ""
      else
        path.getFileName.toString
    (searchPath, prefix)
  }

  /**
   * Get the '.' and '..' directories, if required
   */
  private def getSpecialDotDirs(searchPath: Path, prefix: String): Seq[PathCompletion] =
    getSpecialDotDirs(prefix).map { dir ⇒
      val path = searchPath.resolve(dir).toString + "/"
      PathCompletion(path, Some(CompletionType.Directory))
    }

  private def getSpecialDotDirs(prefix: String): Seq[String] =
    prefix match {
      case "."  ⇒ Seq(".", "..")
      case ".." ⇒ Seq("..")
      case _    ⇒ Seq()
    }

}