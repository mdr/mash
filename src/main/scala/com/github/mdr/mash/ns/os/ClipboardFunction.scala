package com.github.mdr.mash.ns.os

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.evaluator.ToStringifier
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypeInferenceStrategy
import com.github.mdr.mash.ns.core.StringClass
import com.github.mdr.mash.runtime._
import org.apache.commons.io.IOUtils

object ClipboardFunction extends MashFunction("os.clipboard") {

  object Params {
    val Item = Parameter(
      nameOpt = Some("item"),
      summaryOpt = Some("Item to place on the clipboard. If not provided, the contents of the clipboard are returned."),
      defaultValueGeneratorOpt = Some(() ⇒ MashNull))
  }

  import Params._

  val params = ParameterModel(Seq(Item))

  override def apply(boundParams: BoundParams): MashValue = {
    MashNull.option(boundParams(Item)) match {
      case Some(xs: MashList) ⇒ setClipboard(xs.elements.map(ToStringifier.stringify).mkString("\n"))
      case Some(item)         ⇒ setClipboard(ToStringifier.stringify(item))
      case None               ⇒ MashString(getClipboard)
    }
  }

  private def setClipboard(contents: String): MashUnit = {
    val process = new ProcessBuilder("pbcopy", contents).redirectInput(ProcessBuilder.Redirect.PIPE).start()
    IOUtils.write(contents, process.getOutputStream, StandardCharsets.UTF_8)
    process.getOutputStream.close()
    process.waitFor()
    MashUnit
  }

  private def getClipboard: String = {
    val process = new ProcessBuilder("pbpaste").redirectOutput(ProcessBuilder.Redirect.PIPE).start()
    val output = IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
    process.waitFor()
    output
  }

  override def summaryOpt: Option[String] = Some("Interact with the copy/paste clipboard")

  override def typeInferenceStrategy: TypeInferenceStrategy = StringClass

}
