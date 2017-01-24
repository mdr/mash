package com.github.mdr.mash.ns.os

import com.github.mdr.mash.completions.CompletionSpec
import com.github.mdr.mash.evaluator.{ Arguments, EvaluatorException }
import com.github.mdr.mash.functions.{ MashFunction, Parameter, ParameterModel }
import com.github.mdr.mash.inference.{ Inferencer, Type, TypeInferenceStrategy, TypedArguments }
import com.github.mdr.mash.os.linux.LinuxFileSystem
import com.github.mdr.mash.runtime.MashValue

object WithinFunction extends MashFunction("os.within") {

  private val fileSystem = LinuxFileSystem

  object Params {
    val Directory = Parameter(
      nameOpt = Some("directory"),
      summary = "Directory to change into")
    val Block = Parameter(
      nameOpt = Some("block"),
      summary = "Code to execute",
      isLazy = true)
  }
  import Params._

  val params = ParameterModel(Seq(Directory, Block))

  def apply(arguments: Arguments): MashValue = {
    val boundParams = params.validate(arguments)
    val path = boundParams.validatePath(Directory)
    val f = boundParams(Block).asInstanceOf[MashFunction]
    val oldDir = fileSystem.pwd
    if (fileSystem.isDirectory(path))
      fileSystem.chdir(path)
    else
      throw new EvaluatorException(s"Could not change directory to '$path', not a directory")
    try
      f.apply(Arguments(Seq()))
    finally
      fileSystem.chdir(oldDir)
  }

  override def typeInferenceStrategy = WithinFunctionTypeInferenceStrategy

  override def getCompletionSpecs(argPos: Int, arguments: TypedArguments) =
    params.bindTypes(arguments).paramAt(argPos).toSeq.collect {
      case Directory ⇒ CompletionSpec.Directory
    }

  override def summaryOpt = Some("Temporarily change the current working directory to execute a task, then change back")

  override def descriptionOpt = Some("""Examples:
   within "~/project" git.log""")

}

object WithinFunctionTypeInferenceStrategy extends TypeInferenceStrategy {

  import WithinFunction.Params._

  def inferTypes(inferencer: Inferencer, arguments: TypedArguments): Option[Type] =
    WithinFunction.params.bindTypes(arguments).getType(Block)

}