package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.MashList
import com.github.mdr.mash.evaluator.MashObject
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.ns.core.ObjectClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.time.DateTimeClass

object CommitClass extends MashClass("git.Commit") {

  object Fields {
    val Hash = Field("hash", "Commit hash", Type.Tagged(StringClass, CommitHashClass))
    val CommitTime = Field("commitTime", "Commit time", DateTimeClass)
    val Author = Field("author", "Author of the commit", IdentityClass)
    val Committer = Field("committer", "Committer of the commit", IdentityClass)
    val Summary = Field("summary", "Summary message of the commit", StringClass)
    val Message = Field("message", "Commit message", StringClass)
    val Parents = Field("parents", "Parents of this commit", Seq(CommitHashClass))
  }
  import Fields._

  override lazy val fields = Seq(Hash, CommitTime, Author, Committer, Summary, Message, Parents)

  override lazy val methods = Seq(
    ParentMethod,
    ToStringMethod)

  def summary = "A git commit object"

  case class Wrapper(target: Any) {
    def hash: MashString = target.asInstanceOf[MashObject].field(Hash).asInstanceOf[MashString]
    def parents: Seq[MashString] =
      target.asInstanceOf[MashObject].field(Parents).asInstanceOf[MashList].items.map(_.asInstanceOf[MashString])
  }

  object ToStringMethod extends MashMethod("toString") {

    val params = ObjectClass.ToStringMethod.params

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).hash
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, CommitHashClass))

    override def summary = ObjectClass.ToStringMethod.summary

  }

  object ParentMethod extends MashMethod("parent") {

    val params = ParameterModel()

    def apply(target: Any, arguments: Arguments): MashString = {
      params.validate(arguments)
      Wrapper(target).parents.headOption.orNull
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Tagged(StringClass, CommitHashClass))

    override def summary = "Return the first parent, if there is one, else null"

  }

}