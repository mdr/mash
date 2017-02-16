package com.github.mdr.mash.ns.git.tag

import com.github.mdr.mash.classes.{ AbstractObjectWrapper, Field, MashClass, NewStaticMethod }
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.ns.core.{ StringClass, UnitClass }
import com.github.mdr.mash.ns.git.{ CommitHashClass, GitHelper }
import com.github.mdr.mash.runtime.{ MashUnit, MashValue }

object TagClass extends MashClass("git.tag.Tag") {

  object Fields {
    val Name = Field("name", Some("Name of the tag"), StringClass taggedWith TagNameClass)
    val Commit = Field("commit", Some("The commit the tag is pointing to"), StringClass taggedWith CommitHashClass)
  }

  import Fields._

  override lazy val fields = Seq(Name, Commit)

  override val staticMethods = Seq(NewStaticMethod(this))

  case class Wrapper(value: MashValue) extends AbstractObjectWrapper(value) {

    def name = getStringField(Name)

  }

  override lazy val methods = Seq(
    DeleteMethod)

  object DeleteMethod extends MashMethod("delete") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashUnit = {
      params.validate(arguments)
      val tagName = Wrapper(target).name
      GitHelper.withGit { git ⇒
        git.tagDelete.setTags(tagName).call()
      }
      MashUnit
    }

    override def typeInferenceStrategy = UnitClass

    override def summaryOpt = Some("Delete this tag")

  }

  override def summaryOpt = Some("A Git tag")
}