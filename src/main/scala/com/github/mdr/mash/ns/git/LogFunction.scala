package com.github.mdr.mash.ns.git

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import java.time.Instant
import com.github.mdr.mash.os.linux.LinuxFileSystem
import org.eclipse.jgit.lib.Repository
import com.github.mdr.mash.evaluator.MashList
import org.eclipse.jgit.lib.PersonIdent

object LogFunction extends MashFunction("git.log") {

  val params = ParameterModel()

  def apply(arguments: Arguments): MashList = {
    params.validate(arguments)
    GitHelper.withGit { git ⇒
      val commits = git.log.call().asScala.toSeq.reverse
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

  private def asIdentityObject(ident: PersonIdent): MashObject =  {
    import IdentityClass.Fields._
    MashObject(
      ListMap(
        Name -> MashString(ident.getName),
        Email -> MashString(ident.getEmailAddress)),
      IdentityClass)
  }
  
  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Seq(CommitClass))

  override def summary = "Return a sequence of commit objects from the current Git repository"

}