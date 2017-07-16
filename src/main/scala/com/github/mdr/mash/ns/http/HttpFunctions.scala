package com.github.mdr.mash.ns.http

import com.github.mdr.mash.functions.{ BoundParams, Parameter }
import com.github.mdr.mash.ns.core.NoArgFunction._
import com.github.mdr.mash.ns.http.HttpFunctions.Params._
import com.github.mdr.mash.runtime.{ MashBoolean, MashObject, MashString }

object HttpFunctions {

  object Params {

    val Url = Parameter(
      nameOpt = Some("url"),
      summaryOpt = Some("URL to send request to"))

    val Body = Parameter(
      nameOpt = Some("body"),
      summaryOpt = Some("Body of request"),
      defaultValueGeneratorOpt = Some(MashString("")))

    val File = Parameter(
      nameOpt = Some("file"),
      summaryOpt = Some("Whether the body should be fetched from a file (default false)"),
      descriptionOpt = Some(
        """If true, the body parameter will be interpreted as a path, and the contents of the file at that location used as the body.
          |If false, the body will be used directly.""".stripMargin),
      shortFlagOpt = Some('f'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)

    val BasicAuth = Parameter(
      nameOpt = Some("basicAuth"),
      summaryOpt = Some("Basic authentication"),
      descriptionOpt = Some("Must either be a String of the form 'username:password', or an object of the form { username: 'username', password: 'password' }"),
      defaultValueGeneratorOpt = Some(NoArgValue),
      isFlag = true)

    val Headers = Parameter(
      nameOpt = Some("headers"),
      summaryOpt = Some("Headers to add to request"),
      descriptionOpt = Some(
        """Headers can be provided either as an object or a list. Examples:
          |  --headers={ header1: value }
          |  --headers=["header1:value", "header2:value"]
          |  --headers=[{ name: "header1", value: "value"}]""".stripMargin),
      isFlag = true,
      defaultValueGeneratorOpt = Some(NoArgValue))

    val Cookies = Parameter(
      nameOpt = Some("cookies"),
      summaryOpt = Some("Cookies to add to the request"),
      descriptionOpt = Some(
        """Cookies can be provided as an object. Examples:
          |  --cookies={ sessionId: 'xyz'}""".stripMargin),
      isFlag = true,
      defaultValueGeneratorOpt = Some(NoArgValue))

    val QueryParams = Parameter(
      nameOpt = Some("queryParams"),
      summaryOpt = Some("Query parameters to add to the URL"),
      descriptionOpt = Some(
        """Examples:
          |  --queryParams={ name: "value" }""".stripMargin),
      isFlag = true,
      defaultValueGeneratorOpt = Some(() ⇒ MashObject.empty))

    val Json = Parameter(
      nameOpt = Some("json"),
      summaryOpt = Some("Whether to send the body as JSON (default false)"),
      shortFlagOpt = Some('j'),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)

    val Form = Parameter(
      nameOpt = Some("form"),
      summaryOpt = Some("Whether to send the body as a URL-encoded form (default false)"),
      isFlag = true,
      defaultValueGeneratorOpt = Some(MashBoolean.False),
      isBooleanFlag = true)
  }

  def getBodySource(boundParams: BoundParams): BodySource =
    if (boundParams(File).isTruthy)
      BodySource.File(boundParams.validatePath(Body))
    else
      BodySource.Value(boundParams(Body))

}
