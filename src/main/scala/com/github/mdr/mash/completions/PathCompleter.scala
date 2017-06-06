package com.github.mdr.mash.completions

import java.nio.file.{ Files, Path, Paths }

import com.github.mdr.mash.evaluator.{ RetildeResult, TildeExpander }
import com.github.mdr.mash.ns.os.FileTypeClass
import com.github.mdr.mash.os.{ EnvironmentInteractions, FileSystem, PathSummary }
import com.github.mdr.mash.parser.{ StringEscapeResult, StringEscapes }
import com.github.mdr.mash.utils.Region

import scala.PartialFunction.condOpt
import scala.util.Try

/**
 * @param pos -- position within path that corresponds to the completion search string
 */
case class PathCompletion(path: String, typeOpt: Option[CompletionType], prefixLength: Int, pos: Int) {

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
    val pathCompletions = getCompletions(searchString, directoriesOnly = directoriesOnly, substring = substring)
    val completions =
      for {
        PathCompletion(path, typeOpt, prefixLength, completionPos) ← pathCompletions
        RetildeResult(retilded, charsLost) = if (tildeExpandedOpt.isDefined) tildeExpander.retildeFull(path) else RetildeResult(path, 0)
        displayPos = math.max(0, completionPos - charsLost)
        adjustedPrefixLength = prefixLength - charsLost
        StringEscapeResult(escaped, escapeMap) = StringEscapes.escapeCharsFull(retilded)
        location = CompletionLocation(displayPos, escapeMap(displayPos), adjustedPrefixLength, adjustedPrefixLength)
      } yield Completion(
        displayText = retilded,
        insertTextOpt = Some(escaped),
        isQuoted = true,
        typeOpt = typeOpt,
        descriptionOpt = Some(path),
        location = location)
    CompletionResult.of(completions, region)
  }

  private[completions] def getCompletions(searchString: String, directoriesOnly: Boolean = false, substring: Boolean): Seq[PathCompletion] = {
    val (searchPath, fragment) = getSearchPathAndFragment(searchString)
    val childPaths = getMatchingChildren(searchPath, fragment, directoriesOnly = directoriesOnly, substring = substring)
    val specialPaths = getSpecialDotDirs(searchPath, fragment)
    (specialPaths ++ childPaths).sortBy(_.sortKey)
  }

  /**
   * Get children in the given search path matching the given prefix
   */
  private def getMatchingChildren(searchPath: Path, fragment: String, directoriesOnly: Boolean, substring: Boolean): Seq[PathCompletion] = {
    val ignoreDotFiles = substring || !fragment.startsWith(".")
    for {
      path ← getChildren(searchPath, ignoreDotFiles)
      if path.fileType == FileTypeClass.Values.Dir || !directoriesOnly
      (hit, pos) ← pathMatches(fragment, substring, path)
    } yield makeCompletion(searchPath, hit, pos)
  }

  private def getChildren(searchPath: Path, ignoreDotFiles: Boolean): Seq[PathSummary] =
    Try {
      fileSystem.getChildren(searchPath, ignoreDotFiles = ignoreDotFiles, recursive = false)
    }.getOrElse(Seq())

  private def pathMatches(fragment: String, substring: Boolean, path: PathSummary): Option[(PathSummary, Int)] = {
    val name = path.path.getFileName.toString
    val matches =
      if (substring)
        name contains fragment
      else
        name startsWith fragment
    if (matches)
      Some((path, name indexOf fragment))
    else
      None
  }

  private def makeCompletion(searchPath: Path, pathSummary: PathSummary, pos: Int): PathCompletion = {
    val pathSummaryWithLinksDereferenced =
      if (pathSummary.fileType == FileTypeClass.Values.Link)
        Try(fileSystem.getPathSummary(Files.readSymbolicLink(pathSummary.path))).getOrElse(pathSummary)
      else
        pathSummary
    val isDirectory = pathSummaryWithLinksDereferenced.fileType == FileTypeClass.Values.Dir
    val suffix = if (isDirectory) "/" else ""
    val path = searchPath.resolve(pathSummary.path.getFileName).toString + suffix
    val typeOpt = condOpt(pathSummary.fileType) {
      case FileTypeClass.Values.Dir  ⇒ CompletionType.Directory
      case FileTypeClass.Values.File ⇒ CompletionType.File
    }
    val prefixLength = searchPath.toString match {
      case "" ⇒ 0
      case s  ⇒ s.length + 1
    }
    PathCompletion(path, typeOpt, prefixLength, prefixLength + pos)
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
      val pos = if (searchPath.toString.isEmpty) 0 else searchPath.toString.length + 1
      PathCompletion(path, Some(CompletionType.Directory), pos = pos, prefixLength = pos)
    }

  private def getSpecialDotDirs(prefix: String): Seq[String] =
    prefix match {
      case "."  ⇒ Seq(".", "..")
      case ".." ⇒ Seq("..")
      case _    ⇒ Seq()
    }

}