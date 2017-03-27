package com.github.mdr.mash.ns.net

import java.net.URI

import com.github.mdr.mash.classes.MashClass
import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashMethod, Parameter, ParameterModel }
import com.github.mdr.mash.ns.core.{ AnyClass, StringClass }
import com.github.mdr.mash.runtime.{ MashNull, MashObject, MashString, MashValue }
import org.apache.http.client.utils.URIBuilder

object UrlClass extends MashClass("net.Url") {

  override val methods = Seq(
    HostMethod,
    WithQueryParamsMethod)

  override def summaryOpt = Some("Tag class for a URL")

  object HostMethod extends MashMethod("host") {

    val params = ParameterModel()

    def apply(target: MashValue, boundParams: BoundParams): MashValue = {
      val uri = targetUri(target)

      Option(uri.getHost).map(MashString(_, HostClass)) getOrElse MashNull
    }

    override def typeInferenceStrategy = StringClass taggedWith HostClass

    override def summaryOpt = Some("Return the host component of this URL (may be null)")
  }

  private def targetUri(target: MashValue): URI = new URI(target.asInstanceOf[MashString].s)

  object WithQueryParamsMethod extends MashMethod("withQueryParams") {

    object Params {

      val QueryParams = Parameter(
        nameOpt = Some("queryParams"),
        summaryOpt = Some("Query parameters to set on URL"),
        defaultValueGeneratorOpt = Some(MashNull))

      val NamedQueryParams = Parameter(
        nameOpt = Some("namedQueryParams"),
        summaryOpt = Some("Query parameters to set on URL"),
        isNamedArgsParam = true)

    }

    import Params._

    val params = ParameterModel(Seq(QueryParams, NamedQueryParams))

    def apply(target: MashValue, boundParams: BoundParams): MashValue = {
      val uri = targetUri(target)
      val builder = new URIBuilder(uri)
      def addObject(obj: MashObject) =
        for ((field, value) ← obj.immutableFields)
          builder.addParameter(field, ToStringifier.stringify(value))
      boundParams.validateObjectOpt(QueryParams).foreach(addObject)
      addObject(boundParams.validateObject(NamedQueryParams))
      MashString(builder.build.toString, UrlClass)
    }

    override def typeInferenceStrategy = StringClass taggedWith UrlClass

    override def summaryOpt = Some("Add query parameters to this URL")

    override def descriptionOpt = Some(
      """Examples:
  net.url "http://example.com" | .withQueryParams { param: 42 } # http://example.com?param=42
  net.url "http://example.com" | .withQueryParams --param=42    # http://example.com?param=42""")

  }

  override def parentOpt = Some(AnyClass)

}
