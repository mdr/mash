package com.github.mdr.mash.ns.mash

import java.nio.charset.StandardCharsets

import com.github.mdr.mash.Singletons
import com.github.mdr.mash.compiler.CompilationUnit
import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.Arguments
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ ConstantTypeInferenceStrategy, Type, TypedArguments }
import com.github.mdr.mash.runtime._
import org.apache.commons.io.FileUtils

object SourceFunction extends MashFunction("mash.source") {

  private lazy val scriptExecutor = Singletons.scriptExecutor

  object Params {
    val File = Parameter(
      name = "file",
      summary = "Mash source file to read commands from")
  }
  import Params._

  val params = ParameterModel(Seq(File))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val path = boundParams.validatePath(File)
    val s = FileUtils.readFileToString(path.toFile, StandardCharsets.UTF_8)
    scriptExecutor.runUnit(CompilationUnit(s, name = path.toString, mish = false))
  }

  override def typeInferenceStrategy = ConstantTypeInferenceStrategy(Type.Any)

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) = Seq(CompletionSpec.File)

  override def summary = "Read the given mash source file and execute its contents"

}
