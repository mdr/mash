package com.github.mdr.mash.ns.core

import com.github.mdr.mash.evaluator.BoundMethod
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.evaluator.MashString
import com.github.mdr.mash.functions.MashFunction
import com.github.mdr.mash.functions.ParameterModel
import com.github.mdr.mash.functions.MashMethod
import com.github.mdr.mash.evaluator.MashClass
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.inference.Type
import com.github.mdr.mash.functions.Parameter
import java.util.regex.Pattern
import com.github.mdr.mash.evaluator.Field
import com.github.mdr.mash.evaluator.MashObject
import scala.collection.immutable.ListMap

object RegexClass extends MashClass("core.Regex") {

  override val methods = Seq(
    MatchMethod)

  object MatchMethod extends MashMethod("match") {

    private val String = "string"

    val params = ParameterModel(Seq(
      Parameter(
        String,
        "String to search within for matches")))

    def apply(target: Any, arguments: Arguments): MashObject = {
      val boundParams = params.validate(arguments)
      val regex = target.asInstanceOf[MashString].s.r
      val s = boundParams(String).asInstanceOf[MashString].s
      val matchOption = regex.findFirstMatchIn(s)
      matchOption.map { m ⇒
        m.matched
        m.subgroups
        import MatchClass.Fields
        MashObject(
          ListMap(
            Fields.Matched -> MashString(m.matched),
            Fields.Groups -> m.subgroups.map(MashString(_))),
          classOpt = Some(MatchClass))
      }.orNull
    }

    override def typeInferenceStrategy = ConstantMethodTypeInferenceStrategy(Type.Instance(MatchClass))

    override def summary = "Find the first match of the regex in the given string, or else null"
  }

  override def summary = "A regular expression"

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
