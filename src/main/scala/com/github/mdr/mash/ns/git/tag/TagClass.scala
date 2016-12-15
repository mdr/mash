package com.github.mdr.mash.ns.git.tag

import com.github.mdr.mash.evaluator.{ Arguments, Field, MashClass }
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type }
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.ns.git.{ CommitHashClass, GitHelper }
import com.github.mdr.mash.runtime.{ MashObject, MashString, MashUnit, MashValue }

object TagClass extends MashClass("git.tag.Tag") {

  object Fields {
    val Name = Field("name", "Name of the tag", Type.Tagged(StringClass, TagNameClass))
    val Commit = Field("commit", "The commit the tag is pointing to", Type.Tagged(StringClass, CommitHashClass))
  }

  import Fields._

  override lazy val fields = Seq(Name, Commit)

  case class Wrapper(target: MashValue) {

    def name = target.asInstanceOf[MashObject](Name).asInstanceOf[MashString]

  }

  override lazy val methods = Seq(
    DeleteMethod)

  object DeleteMethod extends MashMethod("delete") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      params.validate(arguments)
      val tagName = Wrapper(target).name.s
      GitHelper.withGit { git ⇒
        git.tagDelete.setTags(tagName).call()
      }
      MashUnit
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Unit)

    override def summary = "Delete this tag"

  }

  override def summary: String = "A Git tag"
}