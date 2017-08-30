package com.github.mdr.mash.ns.os

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.ns.core.NoArgFunction._
import com.github.mdr.mash.ns.core.{ NoArgFunction, StringClass }
import com.github.mdr.mash.runtime._
import org.apache.commons.io.IOUtils
import org.apache.commons.lang3.SystemUtils

object ClipboardFunction extends MashFunction("os.clipboard") {

  object Params {
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("Item to place on the clipboard. If not provided, the contents of the clipboard are returned."),
      defaultValueGeneratorOpt = Some(NoArgValue))
  }

  import Params._

  val params = ParameterModel(Item)

  override def call(boundParams: BoundParams): MashValue =
    NoArgFunction.option(boundParams(Item)) match {
      case Some(xs: MashList) ⇒ setClipboard(xs.elements.map(ToStringifier.stringify).mkString("\n"))
      case Some(item)         ⇒ setClipboard(ToStringifier.stringify(item))
      case None               ⇒ MashString(getClipboard)
    }

  private def setClipboard(contents: String): MashUnit = {
    val builder = if (SystemUtils.IS_OS_MAC_OSX) new ProcessBuilder("pbcopy") else new ProcessBuilder("xclip", "-selection", "clipboard")
    val process = builder.redirectInput(ProcessBuilder.Redirect.PIPE).start()
    IOUtils.write(contents, process.getOutputStream, StandardCharsets.UTF_8)
    process.getOutputStream.close()
    process.waitFor()
    MashUnit
  }

  private def getClipboard: String = {
    val builder = if (SystemUtils.IS_OS_MAC_OSX) new ProcessBuilder("pbpaste") else new ProcessBuilder("xclip", "-selection", "clipboard", "-out")
    val process = builder.redirectOutput(ProcessBuilder.Redirect.PIPE).start()
    val output = IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
    process.waitFor()
    output
  }

  override def summaryOpt: Option[String] = Some("Interact with the copy/paste clipboard")

  override def typeInferenceStrategy: TypeInferenceStrategy = StringClass

}
