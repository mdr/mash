package com.github.mdr.mash.ns.mash

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.functions.{ BoundParams, MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.TypedArguments
import com.github.mdr.mash.runtime._
import org.apache.commons.io.FileUtils

object SourceFunction extends MashFunction("mash.source") {

  private lazy val scriptExecutor = Singletons.scriptExecutor

  object Params {
    val File = Parameter(
      nameOpt = Some("file"),
      summaryOpt = Some("Mash source file to read commands from"))
  }
  import Params._

  val params = ParameterModel(Seq(File))

  def apply(boundParams: BoundParams): MashValue = {
    val path = boundParams.validatePath(File)
    val s = FileUtils.readFileToString(path.toFile, StandardCharsets.UTF_8)
    scriptExecutor.runUnit(CompilationUnit(s, name = path.toString, mish = false))
  }

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summaryOpt = Some("Read the given mash source file and execute its contents")

}
