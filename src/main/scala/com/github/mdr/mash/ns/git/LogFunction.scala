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

object LogFunction extends MashFunction("git.log") {

  private val filesystem = LinuxFileSystem

  val params = ParameterModel()

  def apply(arguments: Arguments): Seq[MashObject] = {
    params.validate(arguments)
    val repo = getRepository
    try {
      val git = new Git(repo)
      git.log().call().asScala.toSeq.map(asCommitObject)
    } finally
      repo.close
  }

  private def asCommitObject(commit: RevCommit): MashObject = {
    import CommitClass.Fields._
    val commitTime = Instant.ofEpochSecond(commit.getCommitTime)
    val author = MashString(commit.getAuthorIdent.getName)
    MashObject(
      ListMap(
        Hash -> MashString(commit.getName, Some(CommitHashClass)),
        CommitTime -> commitTime,
        Author -> author,
        Summary -> MashString(commit.getShortMessage)),
      CommitClass)
  }

  private def getRepository() =
    new FileRepositoryBuilder()
      .readEnvironment()
      .findGitDir(filesystem.pwd.toFile)
      .setMustExist(true)
      .build()

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Seq(Type.Instance(CommitClass)))

  override def summary = "Return a sequence of commit objects from the current Git repository"

}