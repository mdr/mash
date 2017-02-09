package com.github.mdr.mash.ns.net

import java.net.URI

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashMethod, ParameterModel }
import com.github.mdr.mash.inference.ConstantMethodTypeInferenceStrategy
import com.github.mdr.mash.ns.core.{ AnyClass, StringClass }
import com.github.mdr.mash.runtime.{ MashNull, MashString, MashValue }

object UrlClass extends MashClass("net.Url") {

  override val methods = Seq(HostMethod)

  override def summaryOpt = Some("Tag class for a URL")

  object HostMethod extends MashMethod("host") {

    val params = ParameterModel()

    def apply(target: MashValue, arguments: Arguments): MashValue = {
      params.validate(arguments)
      val uri = new URI(target.asInstanceOf[MashString].s)
      Option(uri.getHost).map(MashString(_, HostClass)) getOrElse MashNull
    }

    override def typeInferenceStrategy = StringClass taggedWith HostClass

    override def summaryOpt = Some("Return the host component of this URL (may be null)")
  }

  override def parentOpt = Some(AnyClass)

}
