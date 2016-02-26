package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.ns.os.PermissionsSectionClass
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.StringClass
import org.eclipse.jgit.revwalk.RevWalk
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy

object CommitHashClass extends MashClass("git.CommitHash") {

  override lazy val methods = Seq(
    liftCommitField(CommitClass.Fields.CommitTime),
    liftCommitField(CommitClass.Fields.Author),
    liftCommitField(CommitClass.Fields.Summary),
    liftCommitField(CommitClass.Fields.Parents),
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

  private def getCommit(s: String): MashObject = {
    LogFunction.withRepository { repo ⇒
      val objectId = repo.resolve(s)
      val walk = new RevWalk(repo)
      try {
        val commit = walk.parseCommit(objectId)
        LogFunction.asCommitObject(commit)
      } finally {
        walk.dispose()
      }
    }
  }

  private def liftCommitField(field: Field) = new MashMethod(field.name) {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): Any = {
      params.validate(arguments)
      val hash = target.asInstanceOf[MashString].s
      val commitObject = getCommit(hash)
      commitObject.fields(field.name)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(CommitClass.fieldsMap(field.name).fieldType)

    override def summary = field.summary

    override def descriptionOpt = field.descriptionOpt

  }

}