package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.{ Arguments, Field, MashClass }
import com.github.mdr.mash.functions.{ MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantMethodTypeInferenceStrategy, Type }
import com.github.mdr.mash.runtime._

import scala.collection.immutable.ListMap

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
        val groups = MashList(m.subgroups.map {
          case null ⇒ MashNull
          case s ⇒ MashString(s)
        })
        MashObject.of(
          ListMap(
            Fields.Matched -> MashString(m.matched),
            Fields.Groups -> groups),
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
