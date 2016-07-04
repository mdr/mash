package com.github.mdr.mash.ns.core

import scala.collection.immutable.ListMap

import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.functions.Parameter
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.runtime._

object RegexClass extends MashClass("core.Regex") {

  override val methods = Seq(
    MatchMethod)

  object MatchMethod extends MashMethod("match") {

    object Params {
      val String = Parameter(
        "string",
        "String to search within for matches")
    }

    private val String = "string"

    val params = ParameterModel(Seq(Params.String))

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      val boundParams = params.validate(arguments)
      val regex = target.asInstanceOf[MashString].s.r
      val s = boundParams.validateString(Params.String).s
      val matchOption = regex.findFirstMatchIn(s)
      matchOption.map { m ⇒
        import MatchClass.Fields
        MashObject.of(
          ListMap(
            Fields.Matched -> MashString(m.matched),
            Fields.Groups -> MashList(m.subgroups.map(MashString(_)))),
          klass = MatchClass)
      }.getOrElse(MashNull)
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(MatchClass)

    override def summary = "Find the first match of the regex in the given string, or else null"
  }

  override def summary = "A regular expression"

  override def parentOpt = Some(AnyClass)

}

object MatchClass extends MashClass("core.Match") {

  object Fields {
    val Matched = "matched"
    val Groups = "groups"
  }

  override val fields = Seq(
    Field(Fields.Matched, "The matched region of text", Type.Instance(StringClass)),
    Field(Fields.Groups, "Type (file, dir, link)", Type.Seq(Type.Instance(StringClass))))

  override def summary = "A match from a regular expression search"

}
