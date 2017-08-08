package com.github.mdr.mash.ns.os

import java.nio.file.Path

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.repl.history.{ History, HistoryEntry }
import com.github.mdr.mash.runtime.MashUnit
import com.github.mdr.mash.utils.Utils._
import org.apache.commons.text.similarity.LongestCommonSubsequence

object JumpFunction extends MashFunction("os.jump") {

  private def history: History = Singletons.history

  object Params {
    val Queries = Parameter(
      nameOpt = Some("queries"),
      summaryOpt = Some("Patterns to match a path"),
      isVariadic = true,
      variadicAtLeastOne = true)
  }

  import Params._

  val params = ParameterModel(Queries)

  def call(boundParams: BoundParams): MashUnit = {
    val queries = boundParams.validateSequence(Queries).map(ToStringifier.stringify)
    val scoredPaths = getScoredPaths
    val dir =
      findHighestScoringMatchingPath(scoredPaths, matches(queries)) orElse
        findHighestScoringMatchingPath(scoredPaths, matches(queries, caseInsensitive = true)) orElse
        findHighestScoringMatchingPath(scoredPaths, matches(queries, caseInsensitive = true, subsequence = true)) getOrElse
        boundParams.throwInvalidArgument(Queries, s"No matching path")

    ChangeDirectoryFunction.changeDirectory(dir.path)

    MashUnit
  }

  private def getScoredPaths: Seq[ScoredPath] = {
    history.getHistory
      .groupBy(_.workingDirectory)
      .map((score _).tupled)
      .toSeq
  }

  private def findHighestScoringMatchingPath(scoredPaths: Seq[ScoredPath], matcher: Path ⇒ Boolean): Option[ScoredPath] =
    scoredPaths
      .filter(scoredPath ⇒ matcher(scoredPath.path))
      .sortBy(_.score)
      .lastOption

  private def matches(queries: Seq[String], caseInsensitive: Boolean = false, subsequence: Boolean = false)(path: Path): Boolean = {
    val initialQueries = queries.init // safe, queries is non-empty
    val lastQuery = queries.last.when(caseInsensitive, _.toLowerCase)
    Option(path.getFileName)
      .map(_.toString.when(caseInsensitive, _.toLowerCase))
      .exists(s ⇒ if (subsequence) containsSubsequence(s, lastQuery) else s contains lastQuery)
  }

  private def containsSubsequence(s1: String, s2: String): Boolean = {
    val lcs = new LongestCommonSubsequence
    lcs(s1, s2) == s2.length
  }

  private def score(path: Path, entries: Seq[HistoryEntry]): ScoredPath =
    ScoredPath(path, entries.size)

  private case class ScoredPath(path: Path, score: Double)

  override def summaryOpt: Option[String] = Some("Jump to a historical working directory matching the given argument")

}
