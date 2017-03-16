package com.github.mdr.mash.ns.git

import java.time.Instant

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.ns.git.branch.{ CreateFunction, SwitchFunction }
import com.github.mdr.mash.runtime._
import org.eclipse.jgit.lib.PersonIdent
import org.eclipse.jgit.revwalk.RevCommit

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

object LogFunction extends MashFunction("git.log") {

  object Params {
    val Commit = Parameter(
      nameOpt = Some("commit"),
      summaryOpt = Some("Commit to find the log of"),
      defaultValueGeneratorOpt = Some(MashNull))
  }
  import Params._

  val params = ParameterModel(Seq(Commit))

  def apply(boundParams: BoundParams): MashList = {
    GitHelper.withGit { git ⇒
      val cmd = git.log
      if (boundParams(Commit) != MashNull) {
        val commit = MergeFunction.validateCommit(boundParams, Commit)
        cmd.add(commit)
      }
      val commits = cmd.call().asScala.toSeq.reverse
      MashList(commits.map(asCommitObject))
    }
  }

  private def commitHash(commit: RevCommit) = MashString(commit.getName, CommitHashClass)

  def asCommitObject(commit: RevCommit): MashObject = {
    import CommitClass.Fields._
    val commitTime = Instant.ofEpochSecond(commit.getCommitTime)
    val author = MashString(commit.getAuthorIdent.getName)
    val parents = MashList(commit.getParents.toSeq.map(commitHash))
    MashObject.of(
      ListMap(
        Hash -> commitHash(commit),
        CommitTime -> MashWrapped(commitTime),
        Author -> asIdentityObject(commit.getAuthorIdent),
        Committer -> asIdentityObject(commit.getCommitterIdent),
        Summary -> MashString(commit.getShortMessage),
        Message -> MashString(commit.getFullMessage),
        Parents -> parents),
      CommitClass)
  }

  private def asIdentityObject(ident: PersonIdent): MashObject = {
    import IdentityClass.Fields._
    MashObject.of(
      ListMap(
        Name -> MashString(ident.getName),
        Email -> MashString(ident.getEmailAddress)),
      IdentityClass)
  }

  override def typeInferenceStrategy = Seq(CommitClass)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Commit ⇒ CompletionSpec.Items(SwitchFunction.getLocalBranches ++ CreateFunction.getRemoteBranches)
    }

  override def summaryOpt = Some("Return a sequence of commit objects from the current Git repository")

}