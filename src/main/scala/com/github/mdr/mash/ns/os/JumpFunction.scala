package com.github.mdr.mash.ns.os

import java.nio.file.{ Files, Path, Paths }

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.repl.history.{ History, HistoryEntry }
import com.github.mdr.mash.runtime.{ MashString, MashUnit }
import com.github.mdr.mash.utils.Utils._
import org.apache.commons.text.similarity.LongestCommonSubsequence

object JumpFunction extends MashFunction("os.jump") {

  private def history: History = Singletons.history

  object Params {
    val Query = Parameter(
      nameOpt = Some("query"),
      summaryOpt = Some("Pattern to match a path"))
  }

  import Params._

  val params = ParameterModel(Query)

  def call(boundParams: BoundParams): MashUnit = {
    val query = boundParams.validateString(Query).s
    val queryAsPath = Paths.get(query)
    val finalPath =
      if (Files.isDirectory(queryAsPath))
        queryAsPath
      else
        findMatchingPath(query) getOrElse boundParams.throwInvalidArgument(Query, s"No matching path")
    ChangeDirectoryFunction.changeDirectory(finalPath)

    MashUnit
  }

  private def findMatchingPath(query: String): Option[Path] = {
    val scoredPaths = getScoredPaths
    findHighestScoringMatchingPath(scoredPaths, matches(query)) orElse
      findHighestScoringMatchingPath(scoredPaths, matches(query, caseInsensitive = true)) orElse
      findHighestScoringMatchingPath(scoredPaths, matches(query, caseInsensitive = true, subsequence = true)) map
      (_.path)
  }

  private def getScoredPaths: Seq[ScoredPath] =
    history.getHistory
      .groupBy(_.workingDirectory)
      .map((score _).tupled)
      .toSeq

  private def findHighestScoringMatchingPath(scoredPaths: Seq[ScoredPath], matcher: Path ⇒ Boolean): Option[ScoredPath] =
    getMatchingPaths(scoredPaths, matcher)
      .sortBy(_.score)
      .lastOption

  private def findMatchingPaths(scoredPaths: Seq[ScoredPath], matcher: Path ⇒ Boolean): Option[Seq[String]] =
    getMatchingPaths(scoredPaths, matcher) match {
      case Seq() ⇒ None
      case paths ⇒ Some(paths.map(_.path.toString))
    }

  private def getMatchingPaths(scoredPaths: Seq[ScoredPath], matcher: Path ⇒ Boolean): Seq[ScoredPath] =
    scoredPaths.filter(scoredPath ⇒ matcher(scoredPath.path))

  private def matches(query: String, caseInsensitive: Boolean = false, subsequence: Boolean = false)(path: Path): Boolean = {
    val lastQuery = query.when(caseInsensitive, _.toLowerCase)
    Option(path.getFileName)
      .map(_.toString.when(caseInsensitive, _.toLowerCase))
      .exists(s ⇒ if (subsequence) containsSubsequence(s, lastQuery) else s contains lastQuery)
  }

  private def containsSubsequence(s1: String, s2: String): Boolean =
    (new LongestCommonSubsequence) (s1, s2) == s2.length

  private def score(path: Path, entries: Seq[HistoryEntry]): ScoredPath = ScoredPath(path, entries.size)

  private case class ScoredPath(path: Path, score: Double)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments): Seq[CompletionSpec] =
    for {
      valueInfo ← params.bindTypes(arguments).getArgument(Query).toSeq
      query ← valueInfo.valueOpt.collect { case s: MashString ⇒ s.s }
      scoredPaths = getScoredPaths
      paths ← findMatchingPaths(scoredPaths, matches(query)) orElse
        findMatchingPaths(scoredPaths, matches(query, caseInsensitive = true)) orElse
        findMatchingPaths(scoredPaths, matches(query, caseInsensitive = true, subsequence = true))
    } yield CompletionSpec.Items(paths)

  override def summaryOpt: Option[String] = Some("Change directory to a working directory from the history that matches the argument")

}
