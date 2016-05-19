package com.github.mdr.mash.ns.git

import org.eclipse.jgit.revwalk.RevWalk

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type

object CommitHashClass extends MashClass("git.CommitHash") {

  val lifter = new MemberLifter(hash ⇒ getCommit(hash.s))

  override lazy val methods = Seq(
    lifter.liftField(CommitClass.Fields.CommitTime),
    lifter.liftField(CommitClass.Fields.Author),
    lifter.liftField(CommitClass.Fields.Committer),
    lifter.liftField(CommitClass.Fields.Summary),
    lifter.liftField(CommitClass.Fields.Parents),
    lifter.liftMethod(CommitClass.ParentMethod),
    InfoMethod)

  def summary = "A git commit SHA-1 hash"

  object InfoMethod extends MashMethod("info") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashObject = {
      params.validate(arguments)
      val hash = target.asInstanceOf[MashString].s
      getCommit(hash)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(CommitClass))

    override def summary = "Get information about the commit with this hash"

  }

  private def getCommit(s: String): MashObject =
    GitHelper.withRepository { repo ⇒
      val objectId = repo.resolve(s)
      val walk = new RevWalk(repo)
      try {
        val commit = walk.parseCommit(objectId)
        LogFunction.asCommitObject(commit)
      } finally
        walk.dispose()
    }

}

