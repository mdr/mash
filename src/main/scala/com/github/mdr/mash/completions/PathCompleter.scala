package com.github.mdr.mash.completions

import java.io.IOException
import java.nio.file.Path
import java.nio.file.Paths
import scala.PartialFunction.condOpt
import scala.util.Try
import com.github.mdr.mash.ns.os.FileTypeClass
import com.github.mdr.mash.os.FileSystem
import com.github.mdr.mash.os.PathSummary
import com.github.mdr.mash.evaluator.TildeExpander
import com.github.mdr.mash.os.EnvironmentInteractions
import com.github.mdr.mash.parser.StringEscapes
import com.github.mdr.mash.utils.Region
import com.github.mdr.mash.utils.Utils
import com.github.mdr.mash.parser.StringEscapeResult

case class PathCompletion(path: String, typeOpt: Option[CompletionType], pos: Int) {

  def sortKey = if (path endsWith "/") path.init else path

}

/**
 * Provide completions within the file system
 */
class PathCompleter(fileSystem: FileSystem, envInteractions: EnvironmentInteractions) {

  private val tildeExpander = new TildeExpander(envInteractions)

  /**
   * @param substring -- if true, match a substring of the path, else a prefix
   */
  def completePaths(s: String, region: Region, directoriesOnly: Boolean = false, substring: Boolean): Option[CompletionResult] = {
    val tildeExpandedOpt = tildeExpander.expandOpt(s)
    val searchString = StringEscapes.unescape(tildeExpandedOpt.getOrElse(s))
    val completions =
      for {
        PathCompletion(path, typeOpt, displayPos) ← getCompletions(searchString, directoriesOnly = directoriesOnly, substring = substring)
        retilded = if (tildeExpandedOpt.isDefined) tildeExpander.retilde(path) else path
        StringEscapeResult(escaped, escapeMap) = StringEscapes.escapeCharsFull(retilded)
        location = CompletionLocation(displayPos, escapeMap(displayPos))
      } yield Completion(
        displayText = retilded,
        insertTextOpt = Some(escaped),
        isQuoted = true,
        typeOpt = typeOpt,
        descriptionOpt = Some(path),
        location = location)
    CompletionResult.of(completions, region)
  }

  def getCompletions(searchString: String, directoriesOnly: Boolean = false, substring: Boolean): Seq[PathCompletion] = {
    val (searchPath, fragment) = getSearchPathAndFragment(searchString)
    val childPaths = getMatchingChildren(searchPath, fragment, directoriesOnly = directoriesOnly, substring = substring)
    val specialPaths = getSpecialDotDirs(searchPath, fragment)
    (specialPaths ++ childPaths).sortBy(_.sortKey)
  }

  /**
   * Get children in the given search path matching the given prefix
   */
  private def getMatchingChildren(searchPath: Path, fragment: String, directoriesOnly: Boolean, substring: Boolean): Seq[PathCompletion] = {
    val ignoreDotFiles = !fragment.startsWith(".")
    for {
      path ← getChildren(searchPath, ignoreDotFiles)
      if path.fileType == FileTypeClass.Values.Dir || !directoriesOnly
      (hit, pos) ← pathMatches(fragment, substring)(path)
    } yield makeCompletion(searchPath, hit, pos)
  }

  private def getChildren(searchPath: Path, ignoreDotFiles: Boolean): Seq[PathSummary] =
    Try {
      fileSystem.getChildren(searchPath, ignoreDotFiles = ignoreDotFiles, recursive = false)
    }.getOrElse(Seq())

  private def pathMatches(fragment: String, substring: Boolean)(path: PathSummary): Option[(PathSummary, Int)] = {
    val name = path.path.getFileName.toString
    val matches =
      if (substring)
        name contains fragment
      else
        name startsWith fragment
    if (matches)
      Some((path, name.indexOf(fragment)))
    else
      None
  }

  private def makeCompletion(searchPath: Path, pathSummary: PathSummary, pos: Int): PathCompletion = {
    val isDirectory = pathSummary.fileType == FileTypeClass.Values.Dir
    val suffix = if (isDirectory) "/" else ""
    val path = searchPath.resolve(pathSummary.path.getFileName).toString + suffix
    val typeOpt = condOpt(pathSummary.fileType) {
      case FileTypeClass.Values.Dir  ⇒ CompletionType.Directory
      case FileTypeClass.Values.File ⇒ CompletionType.File
    }
    PathCompletion(path, typeOpt, pos)
  }

  private def getSearchPathAndFragment(searchString: String): (Path, String) = {
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
      PathCompletion(path, Some(CompletionType.Directory), pos = 0)
    }

  private def getSpecialDotDirs(prefix: String): Seq[String] =
    prefix match {
      case "."  ⇒ Seq(".", "..")
      case ".." ⇒ Seq("..")
      case _    ⇒ Seq()
    }

}