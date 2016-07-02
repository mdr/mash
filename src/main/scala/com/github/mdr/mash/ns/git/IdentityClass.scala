package com.github.mdr.mash.ns.git

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.core.AnyClass
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime.MashObject
import com.github.mdr.mash.runtime.MashString
import com.github.mdr.mash.runtime.MashValue

object IdentityClass extends MashClass("git.Identity") {

  object Fields {
    val Name = Field("author", "Name", StringClass)
    val Email = Field("email", "Email address", StringClass)
  }
  import Fields._

  override lazy val fields = Seq(Name, Email)

  override lazy val methods = Seq(
    ToStringMethod)

  def summary = "A combination of a person identity and time in Git"

  case class Wrapper(target: MashValue) {
    def name: MashString = target.asInstanceOf[MashObject](Name).asInstanceOf[MashString]
    def email: MashString = target.asInstanceOf[MashObject](Email).asInstanceOf[MashString]
  }

  object ToStringMethod extends MashMethod("toString") {

    val params = AnyClass.ToStringMethod.params

    def apply(target: MashValue, arguments: Arguments): MashString = {
      params.validate(arguments)
      val identity = Wrapper(target)
      MashString(s"${identity.name} <${identity.email}>")
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(StringClass)

    override def summary = AnyClass.ToStringMethod.summary

  }

  override def parentOpt = Some(AnyClass)

}