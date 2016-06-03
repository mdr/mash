package com.github.mdr.mash.ns.git

import java.time.Instant
import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import org.eclipse.jgit.lib.PersonIdent
import org.eclipse.jgit.revwalk.RevCommit
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type.seqToType
import com.github.mdr.mash.ns.git.CommitClass.Fields
import com.github.mdr.mash.ns.git.IdentityClass.Fields
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.ns.git.branch.SwitchFunction
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.ns.git.branch.CreateFunction

object LogFunction extends MashFunction("git.log") {

  object Params {
    val Commit = Parameter(
      name = "commit",
      summary = "Name of a commit to merge")
  }
  import Params._

  val params = ParameterModel(Seq(Commit))

  def apply(arguments: Arguments): MashList = {
    val boundParams = params.validate(arguments)
    val commit = MergeFunction.validateCommit(boundParams, Commit)
    GitHelper.withGit { git ⇒
      val commits = git.log.add(commit).call().asScala.toSeq.reverse
      MashList(commits.map(asCommitObject))
    }
  }

  private def commitHash(commit: RevCommit) = MashString(commit.getName, CommitHashClass)

  def asCommitObject(commit: RevCommit): MashObject = {
    import CommitClass.Fields._
    val commitTime = Instant.ofEpochSecond(commit.getCommitTime)
    val author = MashString(commit.getAuthorIdent.getName)
    val parents = MashList(commit.getParents.toSeq.map(commitHash))
    MashObject(
      ListMap(
        Hash -> commitHash(commit),
        CommitTime -> commitTime,
        Author -> asIdentityObject(commit.getAuthorIdent),
        Committer -> asIdentityObject(commit.getCommitterIdent),
        Summary -> MashString(commit.getShortMessage),
        Message -> MashString(commit.getFullMessage),
        Parents -> parents),
      CommitClass)
  }

  private def asIdentityObject(ident: PersonIdent): MashObject = {
    import IdentityClass.Fields._
    MashObject(
      ListMap(
        Name -> MashString(ident.getName),
        Email -> MashString(ident.getEmailAddress)),
      IdentityClass)
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Seq(CommitClass))

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Commit ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches ++ CreateFunction.getRemoteBranches)
    }

  override def summary = "Return a sequence of commit objects from the current Git repository"

}